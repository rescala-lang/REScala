import json

from pathlib import Path


def get_filepaths(monitoring_path):
  forwarded_fp = monitoring_path / 'forwarded.data'
  received_fp = monitoring_path / 'received.data'
  created_and_delivered_fp = monitoring_path / 'created_and_delivered.data'
  ratios_fp = monitoring_path / 'ratios.data'

  return forwarded_fp, received_fp, created_and_delivered_fp, ratios_fp


script_path = Path(__file__).parent.resolve()
monitoring_paths = (script_path / 'archive' / 'rdt-router-selection-arguing').glob('*')


monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-5n-10t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-10t')
)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-5n-10t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-10t')
)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/epidemicv-n2-addwins-2000'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdt2-n2-addwins-2000'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-addwins-2000-5n-10t'),
)

monitoring_paths = (
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/epidemicv-n2-observeremove-2000-remove-10'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/randomv-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdt2-n2-observeremove-2000-clear-100'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-3n-3t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-5t'),
  Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/comparison/rdtv2-n2-observeremove-2000-5n-10t'),
)


average_last_ratios = {}

for monitoring_path in monitoring_paths:
  forwarded_fp, received_fp, created_and_delivered_fp, ratios_fp = get_filepaths(monitoring_path)

  latest_ratios = []

  with open(ratios_fp, 'r', encoding='utf8') as f:
    for line in f.readlines():
      message = json.loads(line)

      latest_ratios.append(message['ratios'][-1])
  
  average_last_ratios[monitoring_path] = sum(latest_ratios) / len(latest_ratios)


total_forwarded = {}

for monitoring_path in monitoring_paths:
  forwarded_fp, received_fp, created_and_delivered_fp, ratios_fp = get_filepaths(monitoring_path)

  with open(forwarded_fp, 'r', encoding='utf8') as f:    
    total_forwarded[monitoring_path] = len(f.readlines())

print("\n\n\n")

scores = {}

for idx, (monitoring_path, ratio) in enumerate(sorted(average_last_ratios.items(), key=lambda x: x[1], reverse=True)):
  scores[monitoring_path] = idx

  print(f"{idx}. {monitoring_path}: {ratio}")

print("\n\n\n")

for idx, (monitoring_path, total) in enumerate(sorted(total_forwarded.items(), key=lambda x: x[1])):
  scores[monitoring_path] = (scores[monitoring_path] + idx) / (2*len(average_last_ratios))

  print(f"{idx}. {monitoring_path}: {total}")

print("\n\n\n")

for idx, (monitoring_path, combined) in enumerate(sorted(scores.items(), key=lambda x: x[1])):
  print(f"{idx}. {monitoring_path}: {combined}")



