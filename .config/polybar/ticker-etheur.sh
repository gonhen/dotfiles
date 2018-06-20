#!/usr/bin/env python3                                                                                        

#rate=$(curl -sf https://api.kraken.com/0/public/Ticker?pair=ETHEUR | jq -r ".result.XETHZEUR.c[0]")
#rate=$(LANG=C printf "%.2f\n" "$rate")

#echo "#1 $rate €"

import requests
 
r = requests.get('https://api.kraken.com/0/public/Ticker?pair=ETHEUR');
ticker = r.json()
 
print('%.2f€' % float(ticker['result']['XETHZEUR']['c'][0]))

