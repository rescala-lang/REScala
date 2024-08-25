#!/usr/bin/env python3
import pathlib
import re
import time

from core.api.grpc import client
from core.api.grpc.core_pb2 import Node, NodeType, Position, SessionState, Interface, LinkOptions
from core.utils import random_mac


this_filepath = pathlib.Path(__file__).parent.resolve()

dgs_filepath = this_filepath / "data" / "dgs" / "exp1.dgs"

cutoff_after_x_steps = 120
wait_time_per_step_seconds = 5.0
janitor_interval_milliseconds = 2500
discovery_interval_milliseconds = 500

rdt_variant = "addwins"  # options: "addwins", "observeremove", "lastwriterwins"
clients = {
  "n1": "listen",
  "n2": "active",
  "n3": "listen",
  "n4": "listen",
  "n5": "listen",
  "n6": "listen",
  "n7": "listen",
  "n8": "listen",
  "n9": "listen",
  "n10": "listen",
  "n11": "listen",
  "n12": "listen",
  "n13": "listen",
  "n17": "listen",
  "n18": "listen",
  "n19": "listen",
  "n26": "listen"
} # 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 17, 18, 19, 26

router_variant = "rdt"  # options: "flooding", "epidemic", "spray", "binary", "rdt", "rdt2"

rdt_client_operation_mode = "pushall"  # options: "pushall", "requestlater"

dtnd_cla = "udp"

# special configs
addwins_rdt_number_of_additions = 2000
addwins_rdt_sleep_time_milliseconds = int((500 * 1000) / addwins_rdt_number_of_additions)  # we want to simulate about 500 seconds of changes

router_rdt_n_total_nodes = 6
router_rdt_top_n_neighbours = 3

# WARNING
# this script is custom tailored for my simulation use case and other simulations might use parts of this script because it works, 
# but, they are probably better of writing their own script


# this script can play rudimentary dgs files (info taken from https://graphstream-project.org/doc/Advanced-Concepts/The-DGS-File-Format/)
#
# supported operations:
#  st <number>
#  an <nuode-id>
#  ae <edge-id> <node-id1> <node-id2>
#  de <edge-id>
#
# this script is built to be compatible with "contacts-to-dgs.py"
# so look at that script for more information regarding the dgs-file
#
# special case: underlying graph network
#  each unique link connection must be in its own subnet
#  we therefore add each link beforehand, with the two unqiue addresses 10.0.<subnet>.20 and 10.0.<subnet>.21
#  (we assume there will be no more than 255 links and an error will be thrown otherwise)
#
# special case: control network
#  this script automatically adds the control network: 172.16.0.0/24
#
# special case: control node
#  this script automatically adds a control node of type "MONITORING" outside the grid
#
# special case: step 0
#  step 0 must exist and we accept only 'an' add-node instructions in that step
#  supplied y and y coordinates are ignored
#  nodes are placed in a 10 by x grid evenly spaced apart solely for better visibility
#  the node model can be specified
#
# special case: step x
#  steps must not be continuous
#  step x will be executed after approximately x seconds
#  (step execution does not count into our wait-time)
#  another wait-time may be specified
#
# special case: DTN node
#  the node model we add here is "DTN"
#  there is also a custom mechanism to set the dtnd service configuration file dtnd.toml


class ID_Counter:

  def __init__(self, init=0) -> None:
    self.counter = init-1
  
  def next(self) -> int:
    self.counter += 1
    return self.counter


class Interface_Creator:

  def __init__(self) -> None:
    self.net_id_counter = ID_Counter(0)
    self.interface_counters = {}
  
  def get_interfaces(self, node1_id, node2_id):
    if node1_id not in self.interface_counters:
      self.interface_counters[node1_id] = ID_Counter(0)
    if node2_id not in self.interface_counters:
      self.interface_counters[node2_id] = ID_Counter(0)
    
    net_id = self.net_id_counter.next()

    if net_id > 255:
      raise Exception("cannot assign more than 255 links in total")

    return (
      Interface(id=self.interface_counters[node1_id].next(), mac=random_mac(), ip4=f"10.0.{net_id}.20", ip4_mask=24),
      Interface(id=self.interface_counters[node2_id].next(), mac=random_mac(), ip4=f"10.0.{net_id}.21", ip4_mask=24)
    )


class Dtnd_Configfile_Helper:

  def __init__(self) -> None:
    self.configs = {}
    self.discovery_addresses = {}

  def add_node(self, node_id, file_contents):
    self.configs[node_id] = file_contents
  
  def add_discovery_address(self, node_id, address):
    if node_id not in self.discovery_addresses:
      self.discovery_addresses[node_id] = []
    self.discovery_addresses[node_id].append(address)
  
  def _substitute_config_file(self, node_id):
    file_contents = self.configs[node_id]

    #file_contents = re.sub(r'debug = false', 'debug = true', file_contents, 1)
    file_contents = re.sub(r'cla.0.id = "mtcp"', f'cla.0.id = "{dtnd_cla}"', file_contents, 1)
    file_contents = re.sub(r'strategy = "epidemic"', 'strategy = "external"', file_contents, 1)
    file_contents = re.sub(r'interval = "2s"', f'interval = "{discovery_interval_milliseconds}ms"', file_contents, 1)
    file_contents = re.sub(r'janitor = "10s"', f'janitor = "{janitor_interval_milliseconds}ms"', file_contents, 1)

    discovery_string = "[discovery_destinations]\n"
    for idx, address in enumerate(self.discovery_addresses[node_id]):
      discovery_string += f'target.{idx}.destination = "{address}"\n'
    file_contents = re.sub(r'# \[discovery_destinations\]\n', discovery_string, file_contents)

    self.configs[node_id] = file_contents
  
  def get_substituted_config_files(self):
    for node_id in self.configs:
      self._substitute_config_file(node_id)
    return self.configs


grid_node_counter = ID_Counter(0)
global_node_counter = ID_Counter(1)
interface_creator = Interface_Creator()
dtnd_configfile_helper = Dtnd_Configfile_Helper()


with open(dgs_filepath, "rt", encoding="utf8") as f:
  dgs_lines = f.read().split("\n")


file_version = dgs_lines.pop(0)
if file_version != "DGS004":
  raise Exception(f"file version is '{file_version}' but only 'DGS004' is supported")


file_header = dgs_lines.pop(0)  # ignore, because we dont set a session name and do not care about step-numbers or event-numbers
print("dgs file opened")


core = client.CoreGrpcClient()
core.connect()

session_id = core.create_session().session_id

core.set_session_state(session_id, SessionState.CONFIGURATION)
print("connected to core")


service_defaults = {}
for existing_default in core.get_service_defaults(session_id).defaults:
  service_defaults[existing_default.node_type] = existing_default.services
service_defaults["DTN"] = ["DefaultMulticastRoute", "dtnd", "rdtrouter"]
service_defaults["MONITORING"] = ["rdtmonitoring"]

core.set_service_defaults(session_id, service_defaults)
print("set service defaults for nodes")


core.set_session_options(session_id, {'controlnet': '172.16.0.0/24'})
print("added control network 172.16.0.0/24")

monitoring_node_id = global_node_counter.next()
core.add_node(session_id, Node(id=monitoring_node_id, name="control", type=NodeType.DEFAULT, model="MONITORING", position=Position(x=50, y=50)))
core.set_node_service(session_id, monitoring_node_id, "rdtmonitoring", startup=(f"bash -c '/root/.coregui/scripts/rdt_tool -m monitoring &> monitoring.log'",))
print ("added control node")


print("running setup")

# gathering all nodes that participate in the simulation, e.g. all nodes that to something up until step "cutoff_after_x_steps"
def get_all_participating_nodes():
  participating_nodes_until_cutoff = set()
  step = -1

  for line in dgs_lines:
    action, *others = line.split(" ")

    if action == "st":
      step += 1
    
    if step > 0 and action == "ae":
      participating_nodes_until_cutoff.add(others[1])
      participating_nodes_until_cutoff.add(others[2])

    if step >= cutoff_after_x_steps + 1:
      break
  
  return participating_nodes_until_cutoff

participating_nodes_until_cutoff = get_all_participating_nodes()


line = dgs_lines.pop(0)
if line != "st 0":
  raise Exception(f"'step 0' is missing, where each 'an ...' should be, aborting")

# adding all nodes
node_map = {}

line = dgs_lines.pop(0)
while not line.startswith("st"):
  action, node_name, *others = line.split(" ")
  line = dgs_lines.pop(0)

  if not action == "an":
    raise Exception(f"unexpected line in setup step 0, expected 'an ..' got '{line}'")
  
  if node_name not in participating_nodes_until_cutoff:
    continue
  
  grid_node_id = grid_node_counter.next()
  node_map[node_name] = global_node_counter.next()
  position = Position(x=100+(grid_node_id%10)*50, y=100+int(grid_node_id/10)*50)

  if node_name in clients:
    service_defaults["DTN"].append("rdtclient")
    core.set_service_defaults(session_id, service_defaults)

  core.add_node(session_id, Node(
    id=node_map[node_name], 
    name=node_name,
    type=NodeType.DEFAULT,
    model="DTN",
    position=position
  ))

  if node_name in clients:
    additional_config = " "

    if rdt_variant == "addwins":
      additional_config = f"-awa {addwins_rdt_number_of_additions} -awt {addwins_rdt_sleep_time_milliseconds} "
    
    config_str = f"bash -c '/root/.coregui/scripts/rdt_tool -m client -cr {rdt_variant}.{clients[node_name]} -cm {rdt_client_operation_mode} {additional_config}-ma 172.16.0.1 &> client.log'"

    print(f"adding rdt client to node {node_name}, config: {config_str}")
    core.set_node_service(session_id, node_map[node_name], "rdtclient", startup=(config_str,))
    
    service_defaults["DTN"].remove("rdtclient")
    core.set_service_defaults(session_id, service_defaults)
  
  if router_variant == "rdt":
    core.set_node_service(session_id, node_map[node_name], "rdtrouter", startup=(f"bash -c '/root/.coregui/scripts/rdt_tool -m routing -rs {router_variant} -rrn {router_rdt_n_total_nodes} -rrt {router_rdt_top_n_neighbours} -ma 172.16.0.1 &> routing.log'",))
  else:
    core.set_node_service(session_id, node_map[node_name], "rdtrouter", startup=(f"bash -c '/root/.coregui/scripts/rdt_tool -m routing -rs {router_variant} -ma 172.16.0.1 &> routing.log'",))

  dtnd_toml_contents = core.get_node_service_file(session_id, node_map[node_name], "dtnd", "dtnd.toml").data
  dtnd_configfile_helper.add_node(node_map[node_name], dtnd_toml_contents)
  print(f"added node '{node_name}'")

# adding all links (with 100% loss, links are reused)
def add_all_links():
  link_iface_map = {}
  step = -1

  for line in dgs_lines:
    if line.startswith("st"):
      step += 1
    
    if step >= cutoff_after_x_steps + 1:
      break

    if line.startswith("ae"):
      action, link_name, node1_name, node2_name, *other = line.split(" ")

      if node1_name not in participating_nodes_until_cutoff or node2_name not in participating_nodes_until_cutoff:
        break

      node_min_id = min(node_map[node1_name], node_map[node2_name])
      node_max_id = max(node_map[node1_name], node_map[node2_name])

      if (node_min_id, node_max_id) not in link_iface_map:
        node1_iface, node2_iface = interface_creator.get_interfaces(node_min_id, node_max_id)

        core.add_link(
          session_id=session_id, 
          node1_id=node_min_id, 
          node2_id=node_max_id, 
          iface1=node1_iface, 
          iface2=node2_iface,
          options=LinkOptions(loss=100)
        )

        link_iface_map[(node_min_id, node_max_id)] = (node1_iface.id, node2_iface.id)
        dtnd_configfile_helper.add_discovery_address(node_min_id, node2_iface.ip4)
        dtnd_configfile_helper.add_discovery_address(node_max_id, node1_iface.ip4)
        print(f"added link betweeen '{node1_name}' and '{node2_name}'")
  
  return link_iface_map

link_iface_map = add_all_links()


# editing dtnd.toml
for node_id, file_contents in dtnd_configfile_helper.get_substituted_config_files().items():
  core.set_node_service_file(session_id, node_id, "dtnd", "dtnd.toml", file_contents)
print(f"updated dtnd.toml for each node")

print("setup complete")


input("press enter to start the simulation")
core.set_session_state(session_id, SessionState.INSTANTIATION)

num_steps_ran = -1
last_step = 0
link_name_map = {}

while True:
  num_steps_ran += 1

  if num_steps_ran >= cutoff_after_x_steps:
    print(f"ran {num_steps_ran} steps. reached cutoff max. break here.")
    break

  action, step, *others = line.split(" ")
  step = int(step)

  if not action == "st":
    raise Exception(f"unexpected line, expected 'st ..' got '{line}'")
  
  #print(f"waiting {step-last_step} steps, resulting wait-time seconds: {(step - last_step)*wait_time_per_step_seconds}")
  #time.sleep((step - last_step)*wait_time_per_step_seconds)
  print(f"waiting predefined seconds: {wait_time_per_step_seconds}")
  time.sleep(wait_time_per_step_seconds)
  last_step = step
  #input(f"press enter to run step {step}")

  line = dgs_lines.pop(0)

  while not line.startswith("st"):
    action, link_name, *others = line.split(" ")

    if action == "ae":
      node1_name, node2_name = others

      node_min_id = min(node_map[node1_name], node_map[node2_name])
      node_max_id = max(node_map[node1_name], node_map[node2_name])

      node1_iface_id, node2_iface_id = link_iface_map[(node_min_id, node_max_id)]

      core.edit_link(
        session_id=session_id,
        node1_id=node_min_id,
        node2_id=node_max_id,
        iface1_id=node1_iface_id,
        iface2_id=node2_iface_id,
        options=LinkOptions(loss=0)
      )

      link_name_map[link_name] = (node_min_id, node_max_id)
    
      print(f"activated link {link_name} between {node_min_id} and {node_max_id}")
    elif action == "de":
      node_min_id, node_max_id = link_name_map[link_name]

      node1_iface_id, node2_iface_id = link_iface_map[(node_min_id, node_max_id)]

      core.edit_link(
        session_id=session_id,
        node1_id=node_min_id,
        node2_id=node_max_id,
        iface1_id=node1_iface_id,
        iface2_id=node2_iface_id,
        options=LinkOptions(loss=100)
      )

      print(f"deactivated link {link_name} between {node_min_id} and {node_max_id}")
    else:
      raise Exception(f"unknown action '{line}'")

    if len(dgs_lines) > 0:
      line = dgs_lines.pop(0)
    else:
      break

  if len(dgs_lines) <= 0:
    break

print("reached end of simulation, shutting down")
core.set_session_state(session_id, SessionState.SHUTDOWN)
print("done")
