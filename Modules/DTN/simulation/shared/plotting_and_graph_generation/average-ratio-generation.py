import json

from datetime import datetime
from pathlib import Path



def util_get_time_from_stamp(timestamp):
  try:
    return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S.%fZ[%Z]')  # example timestamp: "2024-08-12T16:41:44.797120Z[UTC]"
  except:
    return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ[%Z]')  # example timestamp: "2024-08-12T16:41:44Z[UTC]"

def util_get_seconds_from_time(timestamp, start_time):
  return (timestamp - start_time).total_seconds()

def util_get_nodename_from_clientid(clientid):
  return clientid.split("/")[2]


comparison_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/udp-cla-arguing')


for monitoring_path in comparison_dir.glob('*'):
  ratios_fp = monitoring_path / 'ratios.data'
  average_ratios_fp = monitoring_path / 'average_ratios.data'

  ### read input ###
  data = {}
  start_time = datetime.now()

  with open(ratios_fp, 'r', encoding='utf8') as f:
    for line in f.readlines():
      message = json.loads(line)

      converted_timestamps = [util_get_time_from_stamp(timestamp) for timestamp in message['timestamps']]

      start_time = min(start_time, converted_timestamps[0])

      data[util_get_nodename_from_clientid(message['clientId'])] = (converted_timestamps, message['ratios'])
  
  ### convert timestamps to seconds ###
  for nodename in data:
    seconds = [util_get_seconds_from_time(timestamp, start_time) for timestamp in data[nodename][0]]
    data[nodename] = (seconds, data[nodename][1])
  
  ### generate average ratios ###
  latest_ratios = {nodename: 0 for nodename in data}

  all_ratios = []
  for nodename in data:
    all_ratios += [(nodename, second, ratio) for (second, ratio) in zip(*data[nodename])]

  all_ratios.sort(key=lambda x: x[1])

  average_ratios = []
  for nodename, second, ratio in all_ratios:
    latest_ratios[nodename] = ratio

    average_ratios.append((second, sum(latest_ratios.values()) / len(latest_ratios)))

  with open(average_ratios_fp, 'w', encoding='utf8') as f:
    for second, average_ratio in average_ratios:
      f.write(f"{second} {average_ratio}\n")

