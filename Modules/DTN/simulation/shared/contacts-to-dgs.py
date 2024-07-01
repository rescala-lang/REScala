#!/usr/bin/env python3
import pathlib
import os


this_filepath = pathlib.Path(__file__).parent.resolve()

contacts_filepath = this_filepath / "data" / "Exp1" / "contacts.Exp1.dat"
dgs_filepath = this_filepath / "data" / "dgs" / "exp1.dgs"

if not os.path.exists(dgs_filepath.parent):
  os.makedirs(dgs_filepath.parent)


dgs_file = open(dgs_filepath, "wt", encoding="utf8")


# input file format (taken from https://ieee-dataport.org/open-access/crawdad-cambridgehaggle-v-2009-05-29)
# (items e.g. numbers are separated by tabs)
#  column 1: device-id that recorded the sighting
#  column 2: device-id that was seen
#  column 3: start time of the sighting
#  column 4: end time of the sighting
#  column 5: occurence-number of this column1+column2 combination (not used by us)
#  column 6: time difference between this occurence and the previous occurence of this column1+column2 combination

# output file format (taken from https://graphstream-project.org/doc/Advanced-Concepts/The-DGS-File-Format/)
# (we only use a subset of the available events)
# header lines:
dgs_file.write("DGS004\n")      # the file specification version
dgs_file.write("contacts 0 0\n")  # unique-graph-name number-of-steps number-of-events (both numbers may be zero to indicate unknown length)
# body:
#  "st 0"           adds step zero
#  "an n1"          adds node n1 (position may be added with x=<double> y=<double> but we dont need that)
#  "dn n1"          deletes node n1
#  "ae e1 n0 n1"    adds bidirectional edge e1 between n0 and n1
#  "ae e1 n0 > n1"  adds directional edge e1 from n0 to n1
#  "de e1"          deletes edge e1
# notes:
#  The input file gives us only directional edges, as the discovery is unsynchronized and happens every 2 minutes for 20 seconds.
#  A continuous connection is assumed if a device is also discovered in the next period.
#  We assume a bidirectional edge in every case in the output file, because we assume that both are in range of each other.
#  For a data transfer the discovering node would contact the discovered node.

# temporary data structure
#  each second is a step
#  structure: step -> list(actions)
#  for each contact we put an "ae" and "de" at the respective timestamp-steps
#  many contact have the same begin-time and end-time, for these, we add a random number between 1 and 20 to the end-time
#  for each "ae" check if there is a previous "ae" without a following "de", then don't add it
#  for each "de" check if there is a previous "de" without a following "ae", then remove the "de" and at this "de"
steps = {}
nodes = set()

print("reading input file")

with open(contacts_filepath, "rt", encoding="utf8") as f:
  input_data = f.read()


def should_append_add_edge(add_edge, delete_edge, start_time):
  for i in reversed(range(0, start_time)):
    for event in steps.get(i, []):
      if event == delete_edge:  # early return if we found a previous delete-edge we can safely append a new add-edge
        return True
      if event == add_edge:  # if we found a previous add-edge without a trailing delete-edge, then do not append this add-edge
        return False
  return True  # nothing found, then we can safely append the add-adge


def remove_inbetween_delete_edge(add_edge, delete_edge, end_time):
  for i in reversed(range(0, end_time)):
    for ei, event in enumerate(steps.get(i, [])):
      if event == delete_edge:  # if we found a previous delete-edge, remove it and the new end will be this delete-edge
        del steps[i][ei]
        return
      if event == add_edge:  # early return, if we found a previous add-edge, we can safely append this delete-edge
        return
  raise Exception(f"there should be no delete-edge {delete_edge} without a previous add-edge {add_edge}")


edge_counter = 0
def get_unique_edge_name():
  global edge_counter
  unique_edge_name = f"e{edge_counter}"
  edge_counter += 1
  return unique_edge_name


print("adding edges to temp structure")

for line in input_data.splitlines():
  device_id, seen_id, start_time, end_time, *others = line.split(u"\u0009")

  device_id, seen_id, start_time, end_time = int(device_id), int(seen_id), int(start_time), int(end_time)

  nodes.add(device_id)
  nodes.add(seen_id)

  if (start_time == end_time):
    end_time += 10

  add_edge = ("ae", min(device_id, seen_id), max(device_id, seen_id))
  delete_edge = ("de", min(device_id, seen_id), max(device_id, seen_id))

  if should_append_add_edge(add_edge, delete_edge, start_time):
    lst: list = steps.get(start_time, [])
    lst.append(add_edge)
    steps[start_time] = lst
  
  remove_inbetween_delete_edge(add_edge, delete_edge, end_time)

  lst: list = steps.get(end_time, [])
  lst.append(delete_edge)
  steps[end_time] = lst


print("writing nodes to dgs file")

dgs_file.write("st 0\n")
for i in nodes:
  dgs_file.write(f"an n{i}\n")


print("writing data to dgs file")

latest_unique_edge_names = {}

for i in sorted(steps.keys()):
  dgs_file.write(f"st {i}\n")

  for event in steps[i]:
    if event[0] == "ae":
      unique_edge_name = get_unique_edge_name()
      latest_unique_edge_names[event] = unique_edge_name
      dgs_file.write(f"{event[0]} {unique_edge_name} n{event[1]} n{event[2]}\n")
    elif event[0] == "de":
      unique_edge_name = latest_unique_edge_names[("ae", event[1], event[2])]
      dgs_file.write(f"{event[0]} {unique_edge_name}\n")

dgs_file.close()

