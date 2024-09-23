import matplotlib.pyplot as plt

from pathlib import Path



def add_averages_plot(monitoring_path):
  average_ratios_fp = monitoring_path / 'average_ratios.data'  

  ### import data ###

  average_ratios_seconds = []
  average_ratios_ratios = []

  with open(average_ratios_fp, 'r', encoding='utf8') as f:
    for line in f.readlines():
      second, ratio = line.split()

      average_ratios_seconds.append(float(second))
      average_ratios_ratios.append(float(ratio))

  ### plot data ###
  plt.plot(average_ratios_seconds, average_ratios_ratios, label=monitoring_path.name)



comparison_dir = Path('/home/kali/REScala/Modules/DTN/simulation/shared/archive/rdt-router-selection-arguing')

monitoring_paths = comparison_dir.glob('*')


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


for monitoring_path in monitoring_paths:
  add_averages_plot(monitoring_path)

plt.title('average state convergence over time')
plt.ylabel('convergence ratio')
plt.xlabel('time')
plt.legend()
plt.show()