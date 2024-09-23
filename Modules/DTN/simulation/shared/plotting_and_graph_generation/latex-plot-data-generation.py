import json
import shutil

from datetime import datetime
from pathlib import Path
from random import random



### node constellation plot n2 ###

def util_get_time_from_stamp(timestamp):
  try:
    return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S.%fZ[%Z]')  # example timestamp: "2024-08-12T16:41:44.797120Z[UTC]"
  except:
    return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ[%Z]')  # example timestamp: "2024-08-12T16:41:44Z[UTC]"

def util_get_seconds_from_time(timestamp, start_time):
  return (timestamp - start_time).total_seconds()


def util_get_nodename_from_clientid(clientid):
  return clientid.split("/")[2]


def ratios_to_latex(ratios_fp, output_dir):
  data = {}
  start_time = datetime(year=2100, month=1, day=1)

  with open(ratios_fp, 'r', encoding='utf8') as f_in:
    for line_in in f_in.readlines():
      message = json.loads(line_in)

      converted_timestamps = [util_get_time_from_stamp(timestamp) for timestamp in message['timestamps']]

      data[util_get_nodename_from_clientid(message['clientId'])] = (converted_timestamps, message['ratios'])

      start_time = min(start_time, converted_timestamps[0])
  
  for nodename, (converted_timestamps, ratios) in data.items():

      output_data_before = [f"{util_get_seconds_from_time(timestamp, start_time)} {ratio}\n" for timestamp, ratio in zip(converted_timestamps, ratios)]
      p = 1200.0 / len(output_data_before)
      output_data_after = [line for line in output_data_before if random() <= p]

      print(f"lines before: {len(output_data_before)}, lines after: {len(output_data_after)}")

      with open(output_dir / f"{nodename}.log", 'w', encoding='utf8') as f_out:
        f_out.write("header1 header2\n")

        for line_out in output_data_after:
          f_out.write(line_out)


### rdt likelihood m, n evaluation plots ####
def get_m_n_name(monitoring_path):
  parts = monitoring_path.name.split("-")
  m = parts[-1][0:-1]
  n = parts[-2][0:-1]
  return f"{m}m-{n}n"

def likelihood_evaluation_to_latex(rdt_router_selection_arguing_dir, output_dir):
  average_last_ratios = {}

  for monitoring_path in rdt_router_selection_arguing_dir.glob('*'):
    ratios_fp = monitoring_path / 'ratios.data'

    latest_ratios = []

    with open(ratios_fp, 'r', encoding='utf8') as f:
      for line in f.readlines():
        message = json.loads(line)

        latest_ratios.append(message['ratios'][-1])
    
    average_last_ratios[monitoring_path] = sum(latest_ratios) / len(latest_ratios)


  total_forwarded = {}

  for monitoring_path in rdt_router_selection_arguing_dir.glob('*'):
    forwarded_fp = monitoring_path / 'forwarded.data'

    with open(forwarded_fp, 'r', encoding='utf8') as f:    
      total_forwarded[monitoring_path] = len(f.readlines())

  
  scores = {}

  ratio_comparison_fp = output_dir / 'ratios.log'
  with open(ratio_comparison_fp, 'w', encoding='utf8') as f:
    f.write("header1 header2\n")

    coords = []

    for idx, (monitoring_path, ratio) in enumerate(sorted(average_last_ratios.items(), key=lambda x: x[1], reverse=True)):
      scores[monitoring_path] = idx

      name = get_m_n_name(monitoring_path)

      coords.append(name)

      f.write(f"{name} {ratio}\n")
    
    print(f"ratio coords: \n{', '.join(coords)}\n")

  total_forwarded_comparison_fp = output_dir / 'forwarded.log'
  with open(total_forwarded_comparison_fp, 'w', encoding='utf8') as f:
    f.write("header1 header2\n")

    coords = []

    for idx, (monitoring_path, total) in enumerate(sorted(total_forwarded.items(), key=lambda x: x[1])):
      scores[monitoring_path] = (scores[monitoring_path] + idx) / (2*len(average_last_ratios))

      name = get_m_n_name(monitoring_path)

      coords.append(name)

      f.write(f"{name} {total}\n")
    
    print(f"total_forwarded coords: \n{', '.join(coords)}\n")

  
  aggregated_scores = {}

  for monitoring_path, combined in sorted(scores.items(), key=lambda x: x[1]):
    name = get_m_n_name(monitoring_path)

    if combined not in aggregated_scores:
      aggregated_scores[combined] = [name]
    else:
      aggregated_scores[combined].append(name)
    
  for combined, names in sorted(aggregated_scores.items(), key=lambda x: x[0]):
    print(f"\item {', '.join(names)}")


### rdt likelihood vs random plots ###
def get_random_evaluation_name(monitoring_path):
  parts = monitoring_path.name.split("-")
  m = parts[-1][0:-1]
  n = parts[-2][0:-1]
  if parts[0] == "randomv":
    return f"random-spread-{m}m-{n}n"
  elif parts[0] == "rdtv2":
    return f"rdt-likelihood-{m}m-{n}n"
  elif parts[0] == "epidemicv":
    return "epidemic"
  elif parts[0] == "rdt2":
    return "rdt-flooding"
  else:
    raise Exception(f"unknown router: {parts[0]}")

def random_evaluation(monitoring_paths, output_dir):
  average_last_ratios = {}

  for monitoring_path in monitoring_paths:
    ratios_fp = monitoring_path / 'ratios.data'

    latest_ratios = []

    with open(ratios_fp, 'r', encoding='utf8') as f:
      for line in f.readlines():
        message = json.loads(line)

        latest_ratios.append(message['ratios'][-1])
    
    average_last_ratios[monitoring_path] = sum(latest_ratios) / len(latest_ratios)


  total_forwarded = {}

  for monitoring_path in monitoring_paths:
    forwarded_fp = monitoring_path / 'forwarded.data'

    with open(forwarded_fp, 'r', encoding='utf8') as f:    
      total_forwarded[monitoring_path] = len(f.readlines())

  
  scores = {}

  ratio_comparison_fp = output_dir / 'ratios.log'
  with open(ratio_comparison_fp, 'w', encoding='utf8') as f:
    f.write("header1 header2\n")

    coords = []

    for idx, (monitoring_path, ratio) in enumerate(sorted(average_last_ratios.items(), key=lambda x: x[1], reverse=True)):
      scores[monitoring_path] = idx

      name = get_random_evaluation_name(monitoring_path)

      coords.append(name)

      f.write(f"{name} {ratio}\n")
    
    print(f"ratio coords: \n{', '.join(coords)}\n")

  total_forwarded_comparison_fp = output_dir / 'forwarded.log'
  with open(total_forwarded_comparison_fp, 'w', encoding='utf8') as f:
    f.write("header1 header2\n")

    coords = []

    for idx, (monitoring_path, total) in enumerate(sorted(total_forwarded.items(), key=lambda x: x[1])):
      scores[monitoring_path] = (scores[monitoring_path] + idx) / (2*len(average_last_ratios))

      name = get_random_evaluation_name(monitoring_path)

      coords.append(name)

      f.write(f"{name} {total}\n")
    
    print(f"total_forwarded coords: \n{', '.join(coords)}\n")

  
  aggregated_scores = {}

  for monitoring_path, combined in sorted(scores.items(), key=lambda x: x[1]):
    name = get_random_evaluation_name(monitoring_path)

    if combined not in aggregated_scores:
      aggregated_scores[combined] = [name]
    else:
      aggregated_scores[combined].append(name)
    
  for combined, names in sorted(aggregated_scores.items(), key=lambda x: x[0]):
    print(f"\item {', '.join(names)}")


### copy average ratios into output dir ###
def copy_average_ratios_into_output_dir(monitoring_paths, output_dir):
  for monitoring_path in monitoring_paths:
    input_fp = monitoring_path / 'average_ratios.data'
    output_fp = output_dir / f'{monitoring_path.name}.log'

    with open(input_fp, 'r', encoding='utf8') as f:
      lines_before = f.readlines()

    p = 1200.0 / len(lines_before)
    lines_after = [line for line in lines_before if random() <= p]
    
    print(f"lines before: {len(lines_before)}, lines after: {len(lines_after)}")

    with open(output_fp, 'w', encoding='utf8') as f:
      f.write("header1 header2\n")

      for line in lines_after:
        f.write(line)


### convert forwarded.data to a bundles-forwarded-over-time graph ###
def convert_forwarded(monitoring_path, output_dir):
  forwarded_fp = monitoring_path / 'forwarded.data'

  ### accumulating number of bundles per second ###

  begin_time = None

  data = {}  # structure: {node-name: {second: num-bundles}}

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

  plot_data = {}  # structure: {node-name: [num-bundles-per-step]}

  for node_id, d in data.items():
    plot_data[node_id] = []

    for second, number_of_bundles in sorted(d.items(), key=lambda item: item[0]):
      while len(plot_data[node_id]) < second:
        plot_data[node_id].append(0)
      
      plot_data[node_id].append(number_of_bundles)
  
  ### writing output files ###
  
  for node_name in plot_data:
    output_fp = output_dir / f'{node_name}-forwarded.log'

    with open(output_fp, 'w', encoding='utf8') as f:
      counter = 0
      for num_bundles in plot_data[node_name]:
        f.write(f"{counter} {num_bundles}\n")
        counter += 1



ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/2024-08-23-1851-flooding-n2-addwins-2000') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n2')

# ratios_to_latex(ratios_fp, output_dir)

ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/2024-08-23-1811-flooding-n12-addwins-2000') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n12')

# ratios_to_latex(ratios_fp, output_dir)

ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/2024-08-23-1825-flooding-n13-addwins-2000') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n13')

# ratios_to_latex(ratios_fp, output_dir)

ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/2024-08-23-1838-flooding-n17-addwins-2000') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n17')

# ratios_to_latex(ratios_fp, output_dir)

ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/flooding-n18-addwins-2000-new') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n18')

# ratios_to_latex(ratios_fp, output_dir)

ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/flooding-n19-addwins-2000-new') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n19')

# ratios_to_latex(ratios_fp, output_dir)

ratios_fp = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/node-constellation-arguing/2024-08-24-1955-flooding-n26-addwins-2000') / 'ratios.data'
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/node_constellation_n26')

# ratios_to_latex(ratios_fp, output_dir)


rdt_router_selection_arguing_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/rdt-router-selection-arguing')
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/likelihood_m_n_evaluation')

#likelihood_evaluation_to_latex(rdt_router_selection_arguing_dir, output_dir)


monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-5n-10t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-10t')
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/random_evaluation_addwins')

# random_evaluation(monitoring_paths, output_dir)


monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-5n-10t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-10t')
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/random_evaluation_observeremove')

# random_evaluation(monitoring_paths, output_dir)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/epidemicv-n2-addwins-2000'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdt2-n2-addwins-2000'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-10t'),
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/rdt_evaluation_addwins')

# random_evaluation(monitoring_paths, output_dir)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/epidemicv-n2-observeremove-2000-remove-10'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdt2-n2-observeremove-2000-clear-100'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-10t'),
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/rdt_evaluation_observeremove')

# random_evaluation(monitoring_paths, output_dir)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/epidemicv-n2-addwins-2000'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdt2-n2-addwins-2000'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-10t'),
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_rdt_evaluation_addwins')

# copy_average_ratios_into_output_dir(monitoring_paths, output_dir)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/epidemicv-n2-observeremove-2000-remove-10'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdt2-n2-observeremove-2000-clear-100'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-10t'),
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_rdt_evaluation_observeremove')

# copy_average_ratios_into_output_dir(monitoring_paths, output_dir)




### extra graphs random evaluation ###
monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-5n-10t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-10t')
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_random_evaluation_addwins')

# copy_average_ratios_into_output_dir(monitoring_paths, output_dir)


monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-5n-10t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-10t')
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_random_evaluation_observeremove')

# copy_average_ratios_into_output_dir(monitoring_paths, output_dir)



#### cla arguing averages plot appendix ####

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/udp-cla-arguing/2024-08-23-1616-flooding-n2-n1345678-addwins-2000-udp'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/udp-cla-arguing/2024-08-23-1630-flooding-n2-n1345678-addwins-2000-tcp'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/udp-cla-arguing/2024-08-23-1642-flooding-n2-n1345678-addwins-2000-http'),
)
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_cla_arguing')

# copy_average_ratios_into_output_dir(monitoring_paths, output_dir)


monitoring_path = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/flooding-arguing/2024-08-23-0133-flooding-n2-n1345678-addwins-2000')
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_rate_of_change_4')

# convert_forwarded(monitoring_path, output_dir)


monitoring_path = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/flooding-arguing/2024-08-23-0154-flooding-n2-n1345678-addwins-4000')
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_rate_of_change_8')

convert_forwarded(monitoring_path, output_dir)


monitoring_path = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/flooding-arguing/2024-08-23-0215-flooding-n2-n1345678-addwins-8000')
output_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/latex/appendix_rate_of_change_16')

convert_forwarded(monitoring_path, output_dir)
