import json

from datetime import datetime, timedelta
from pathlib import Path



import matplotlib.pyplot as plt
import matplotlib.dates as mdates


monitoring_path = (Path(__file__).parent / 'monitoring').resolve()
forwarded_fp = monitoring_path / 'forwarded.data'
received_fp = monitoring_path / 'received.data'
created_and_delivered_fp = monitoring_path / 'created_and_delivered.data'
ratios_fp = monitoring_path / 'ratios.data'


def util_get_time_from_stamp(timestamp):
  try:
    return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S.%fZ[%Z]')  # example timestamp: "2024-08-12T16:41:44.797120Z[UTC]"
  except:
    return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ[%Z]')  # example timestamp: "2024-08-12T16:41:44Z[UTC]"




"""
first plot - bundles forwarded per node over time
"""

### accumulating number of bundles per second ###

begin_time = None

data = {}  # structure: {node-id: {second: num-bundles}}

with open(forwarded_fp, 'r', encoding='utf8') as f:
  for line in f.readlines():
    message = json.loads(line)

    message_time = util_get_time_from_stamp(message['time'])

    if begin_time == None:
      begin_time = message_time
    
    node_name = message['nodeId'].split('/')[-2]

    total_second = int((message_time - begin_time).total_seconds())

    if node_name not in data:
      data[node_name] = {}
    
    if total_second not in data[node_name]:
      data[node_name][total_second] = 1
    else:
      data[node_name][total_second] += 1


### converting structure ####

plot_data = {}

for node_id, d in data.items():
  plot_data[node_id] = []

  for second, number_of_bundles in sorted(d.items(), key=lambda item: item[0]):
    while len(plot_data[node_id]) < second:
      plot_data[node_id].append(0)
    
    plot_data[node_id].append(number_of_bundles)


### plot data ###

for node_id, l in plot_data.items():
  plt.plot(l, label=node_id)

plt.title('bundles forwarded per second')
plt.ylabel('number of bundles')
plt.xlabel('second')
plt.legend()
plt.show()



"""
second plot - bundles received per node over time:
"""

### accumulating number of bundles per second ###

begin_time = None

data = {}  # structure: {node-id: {second: num-bundles}}

with open(received_fp, 'r', encoding='utf8') as f:
  for line in f.readlines():
    message = json.loads(line)

    message_time = util_get_time_from_stamp(message['time'])

    if begin_time == None:
      begin_time = message_time
    
    node_name = message['nodeId'].split('/')[-2]

    total_second = int((message_time - begin_time).total_seconds())

    if node_name not in data:
      data[node_name] = {}
    
    if total_second not in data[node_name]:
      data[node_name][total_second] = 1
    else:
      data[node_name][total_second] += 1


### converting structure ####

plot_data = {}

for node_id, d in data.items():
  plot_data[node_id] = []

  for second, number_of_bundles in sorted(d.items(), key=lambda item: item[0]):
    while len(plot_data[node_id]) < second:
      plot_data[node_id].append(0)
    
    plot_data[node_id].append(number_of_bundles)


### plot data ###

for node_id, l in plot_data.items():
  plt.plot(l, label=node_id)

plt.title('bundles received per second')
plt.ylabel('number of bundles')
plt.xlabel('second')
plt.legend()
plt.show()



"""
third plot - state convergence of clients over time
"""

### import ratio converted data ###

plot_data = {}

with open(ratios_fp, 'r', encoding='utf8') as f:
  for line in f.readlines():
    message = json.loads(line)

    converted_timestamps = [util_get_time_from_stamp(timestamp) for timestamp in message['timestamps']]

    plot_data[message['clientId']] = (converted_timestamps, message['ratios'])

### plot data ###

locator = mdates.AutoDateLocator()
formatter = mdates.ConciseDateFormatter(locator)
ax = plt.gca()
ax.xaxis.set_major_locator(locator)
ax.xaxis.set_major_formatter(formatter)

for node_id, (l1, l2) in plot_data.items():
  plt.plot(l1, l2, label=node_id)

plt.title('state convergence over time')
plt.ylabel('convergence ratio')
plt.xlabel('time')
plt.legend()
plt.show()


