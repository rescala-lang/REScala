#!/usr/bin/env python3
import pathlib
import re
import time

from core.api.grpc import client
from core.api.grpc.core_pb2 import Node, NodeType, Position, SessionState, Interface, LinkOptions
from core.utils import random_mac


this_filepath = pathlib.Path(__file__).parent.resolve()

dgs_filepath = this_filepath / "data" / "dgs" / "exp1.dgs"


node_model = "DTN"

control_node_model = "CHECKER"

wait_time_per_step_seconds = 1.0


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
#  this script automatically adds a first node outside the grid
#  the node model can be specified
#
# special case: step 0
#  if step 0 exists, we accept only 'an' add-node instructions in that step
#  supplied y and y coordinates are ignored
#  nodes are placed in a 10 by x grid evenly spaced apart solely for better visibility
#  the node model can be specified
#
# special case: step x
#  steps must not be continuous
#  step x will be executed after approximately x seconds
#  (step execution does not count into our wait-time)
#  another wait-time may be specified


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

    file_contents = re.sub('strategy = "epidemic"', 'strategy = "external"', file_contents, 1)
    file_contents = re.sub('interval = "2s"', 'interval = "1s"', file_contents, 1)

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
service_defaults["CHECKER"] = ["rdtchecker"]

core.set_service_defaults(session_id, service_defaults)
print("set service defaults for nodes")


core.add_node(session_id, Node(id=global_node_counter.next(), name="control", type=NodeType.DEFAULT, model=control_node_model, position=Position(x=50, y=50)))
print ("added control node")


node_map = {}


print("running setup")

line = dgs_lines.pop(0)
if line != "st 0":
  raise Exception(f"'step 0' is missing, where each 'an ...' should be, aborting")

# adding all nodes
line = dgs_lines.pop(0)
while not line.startswith("st"):
  action, node_name, *others = line.split(" ")

  if not action == "an":
    raise Exception(f"unexpected line in setup step 0, expected 'an ..' got '{line}'")
  
  grid_node_id = grid_node_counter.next()
  node_map[node_name] = global_node_counter.next()
  position = Position(x=100+(grid_node_id%10)*50, y=100+int(grid_node_id/10)*50)

  core.add_node(session_id, Node(
    id=node_map[node_name], 
    name=node_name,
    type=NodeType.DEFAULT,
    model=node_model,
    position=position
  ))

  dtnd_toml_contents = core.get_node_service_file(session_id, node_map[node_name], "dtnd", "dtnd.toml").data
  dtnd_configfile_helper.add_node(node_map[node_name], dtnd_toml_contents)
  print(f"added node '{node_name}'")

  line = dgs_lines.pop(0)

# adding all links (with 100% loss, links are reused)
link_iface_map = {}

for lline in dgs_lines:
  if lline.startswith("ae"):
    action, link_name, node1_name, node2_name, *other = lline.split(" ")

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
    
      #todo: edit config file

# editing dtnd.toml
for node_id, file_contents in dtnd_configfile_helper.get_substituted_config_files().items():
  core.set_node_service_file(session_id, node_id, "dtnd", "dtnd.toml", file_contents)
  print(f"edited dtnd.toml for node-id {node_id}")

print("setup complete")


input("press any key to start the simulation")
core.set_session_state(session_id, SessionState.INSTANTIATION)

last_step = 0
link_name_map = {}

while True:
  action, step, *others = line.split(" ")
  step = int(step)

  if not action == "st":
    raise Exception(f"unexpected line, expected 'st ..' got '{line}'")
  
  #print(f"waiting {step-last_step} steps, resulting wait-time seconds: {(step - last_step)*wait_time_per_step_seconds}")
  #time.sleep((step - last_step)*wait_time_per_step_seconds)
  last_step = step
  input(f"press any key to run step {step}")

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
    
      print(f"activated link {link_name}")
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

      print(f"deactivated link {link_name}")
    else:
      raise Exception(f"unknown action '{line}'")

    if len(dgs_lines) > 0:
      line = dgs_lines.pop(0)
    else:
      break

  if len(dgs_lines) <= 0:
    break
