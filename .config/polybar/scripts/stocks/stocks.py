#!/usr/bin/python3

from tda.auth import client_from_token_file
from tda.client import Client

import json
import os
from dotenv import dotenv_values

config = {
    **dotenv_values(),
    **os.environ
}

API_KEY = config['TDA_CLIENT_KEY']
ACCOUNT_ID = config['TDA_ACCOUNT_ID']
TOKEN_PATH = config['TOKEN_PATH']

tda_client = client_from_token_file(
    api_key=API_KEY,
    token_path=TOKEN_PATH
)

positions = tda_client.get_account(ACCOUNT_ID, fields=Client.Account.Fields.POSITIONS).json()
value = positions['securitiesAccount']['currentBalances']['liquidationValue']

total_change = 0
data_file = open('{0}/eww/stock_data.json'.format(config['XDG_CONFIG_HOME']), 'w')
data = []
for position in positions['securitiesAccount']['positions']:
    data.append({
        'symbol': position['instrument']['symbol'],
        'change': round(position['currentDayProfitLoss'], 2),
        'percent_change': round(position['currentDayProfitLossPercentage'], 2),
        'num_shares': position['longQuantity'],
        'value': round(position['marketValue'], 2)
    })
    total_change += position['currentDayProfitLoss']

data_file.write(json.dumps(data))
data_file.close()

color = 'DA3B3B' if total_change < 0 else '2FCB56'
icon = '' if total_change < 0 else ''
sign = '-' if total_change < 0 else ''
print('%{{F#C19025}}${0}   %{{F#{1}}}{2} {3}${4}'.format('{:,.2f}'.format(value), color, icon, sign, '{:,.2f}'.format(round(abs(total_change), 2), ',d')), flush=True)
