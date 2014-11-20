#!/usr/bin/env python
import struct 

data = open('ndata.dat','r')

a = []
byte = data.read(4)
intbyte=  struct.unpack("<L", byte)[0]
a.append(intbyte)

i=0
while byte:
  byte= data.read(4)
  intbyte=  struct.unpack("<L", byte)[0]
  a.append(intbyte)
  i = i+1

data.close()

a.sort()
x=-1
for e in a:
  if (e != x and e>=0):
    print(e)
    x = e
