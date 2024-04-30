import os 
fid=open(input("file : "),'r')
perf=[]
for line in fid.readlines():
   if ("total time" in line): perf.append(1728./float(line.split('=')[1]))
fid.close()
mean=0.
for run in perf: mean+=run
for run in perf: print(run)
print(mean/len(perf))
