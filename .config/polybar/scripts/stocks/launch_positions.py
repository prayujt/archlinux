#!/usr/bin/python3

import json
import os
from dotenv import dotenv_values

config = {
    **dotenv_values(),
    **os.environ
}

data_file = open('{0}/polybar/scripts/stocks/data.json'.format(config['XDG_CONFIG_HOME']), 'r')

data = json.loads(data_file.read())

for key, value in data.items():
    print(key, value)

os.system('cp {0}/polybar/scripts/stocks/data.json {0}/polybar/scripts/stocks/data2.json'.format(config['XDG_CONFIG_HOME']))
data_file.close()
