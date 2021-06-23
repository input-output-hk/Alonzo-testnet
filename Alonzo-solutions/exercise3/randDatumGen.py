#!/usr/bin/env python3
from numpy import random
randDatum = ''
for i in range(4):
    randDatum += str(random.randint(1e3,1e4-1))

with open("./txs/randomDatum.txt", "w") as f:
    f.write(randDatum)
print(randDatum)
