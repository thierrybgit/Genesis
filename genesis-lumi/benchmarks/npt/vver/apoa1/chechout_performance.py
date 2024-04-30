import os 
fid=open(input("file : "),'r')
perf=[]
for line in fid.readlines():
   if ("total time" in line): perf.append(1728./float(line.split('=')[1]))
fid.close()
mean=0.
kk=0
print("======================")
print("  run\t perf(ns/day)")
print("======================")
for i in range(1,len(perf)):
    mean+=perf[i]
    kk+=1
    print("  ",kk,'\t  {0:7.3f}'.format(perf[i]))
print("======================")
print(" mean\t  {0:7.3f}".format(mean/(len(perf)-1)))
print("======================")
