""" DWD Temperature Extraction

This script loads weather data from the DWD and extracts a list of temperature values.
The data written into a JSON array file, which is used as input data for the "iot sensor node".

The script can be adjusted to fetch and parse other weather stations and/or other time-spans.

A description of the historical station IDs can be found here:
https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/historical/zehn_min_tu_Beschreibung_Stationen.txt

A description of the historical table data can be found here:
https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/historical/DESCRIPTION_obsgermany_climate_10min_tu_historical_en.pdf
"""
import os
import csv
import json
import urllib.request
from typing import List

from zipfile import ZipFile
from io import BytesIO, StringIO


# insert here a list of continuous air-temperature data from the same station
resources_to_parse = [
    '10minutenwerte_TU_00044_20070209_20091231_hist.zip',
    '10minutenwerte_TU_00044_20100101_20191231_hist.zip'
]

# url to the folder containing the resources
open_dwd_url = 'https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/historical/'

# file to store the JSON array of temperature values in
out_filepath = '/app/data/temps.json'


def fetch_csvs_from_url(url: str) -> List[str]:
    # although the zip file should only contain a single file we can also handle multiple files

    with urllib.request.urlopen(url) as f:
        zip_file = ZipFile(BytesIO(f.read()))

    # sorted by int('yyyymmdd'), example: "produkt_zehn_min_tu_20070209_20091231_00044.txt" -> 20070209
    # NOTE: key == 4
    f_names = zip_file.namelist()[:]
    f_names.sort(key=lambda x: int(x.split('_')[4]))

    return [zip_file.read(f_name).decode() for f_name in f_names]


def extract_temperature_list(csv_contents: str) -> List[float]:
    reader = csv.reader(StringIO(csv_contents), delimiter=';', lineterminator='\r\n')

    # skip first line
    reader.__next__()

    # extract index 4 -> TT_10 -> air temperature at 2m height
    temps = (float(line[4]) for line in reader)

    # filter invalid measurements
    temps = filter(lambda x: x != -999, temps)

    return list(temps)


if __name__ == '__main__':
    temperatures: List[str] = []

    # sorted by int('yyyymmdd'), example: "10minutenwerte_TU_00044_20070209_20091231_hist.zip" -> 20070209
    # NOTE: key == 3
    resources_to_parse.sort(key=lambda x: int(x.split('_')[3]))

    for filename in resources_to_parse:
        csvs = fetch_csvs_from_url(f'{open_dwd_url}{filename}')

        for csv_content in csvs:
            temperatures += extract_temperature_list(csv_content)

    os.makedirs(os.path.dirname(out_filepath))

    with open(out_filepath, 'w') as f:
        json.dump(temperatures, f)
