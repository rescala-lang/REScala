#!/usr/bin/env python3
import pathlib
import pandas as pd
from pyvis.network import Network
import networkx as nx



graph = nx.Graph()


this_filepath = pathlib.Path(__file__).parent.resolve()

dgs_filepath = this_filepath / "data" / "dgs" / "exp1.dgs"


with open(dgs_filepath, "rt", encoding="utf8") as f:
  dgs_lines = f.read().split("\n")

file_version = dgs_lines.pop(0)
if file_version != "DGS004":
  raise Exception(f"file version is '{file_version}' but only 'DGS004' is supported")

file_header = dgs_lines.pop(0)  # ignore, because we dont set a session name and do not care about step-numbers or event-numbers
print("dgs file opened")


line = dgs_lines.pop(0)

if line == "st 0":
  print("running setup")

  line = dgs_lines.pop(0)
  while not line.startswith("st"):
    action, node_name, *others = line.split(" ")

    if not action == "an":
      raise Exception(f"unexpected line in setup step 0, expected 'an ..' got '{line}'")
    
    graph.add_node(node_name)
    print(f"added node '{node_name}'")

    line = dgs_lines.pop(0)

  print("setup complete")


link_map = {}

while True:
  action, step, *others = line.split(" ")
  step = int(step)

  if not action == "st":
    raise Exception(f"unexpected line, expected 'st ..' got '{line}'")
  
  input(f"press any key to run step {step}")

  line = dgs_lines.pop(0)

  while not line.startswith("st"):
    action, link_name, *others = line.split(" ")

    if action == "ae":
      node1_name, node2_name = others

      link_info = (node1_name, node2_name)

      graph.add_edge(*link_info)

      link_map[link_name] = link_info
      print(f"added link {link_name} from {node1_name} to {node2_name}")
    elif action == "de":
      link_info = link_map[link_name]

      graph.remove_edge(*link_info)

      print(f"deleted link {link_name}")
    else:
      raise Exception(f"unknown action '{line}'")

    if len(dgs_lines) > 0:
      line = dgs_lines.pop(0)
    else:
      break
  
  net = Network()
  net.toggle_physics(True)
  net.show_buttons(filter_=['physics'])
  net.from_nx(graph)
  net.show('graph.html', notebook=False)

  if len(dgs_lines) <= 0:
    break
