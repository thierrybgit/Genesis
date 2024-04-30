import os 
current=os.getcwd()
dirs=["genesis-lumi/benchmarks/npt/vver/apoa1","genesis-lumi/benchmarks/npt/vres/apoa1"]

print("Performance in ns/day")
for dir in dirs:
    os.chdir(dir)
    for item in os.listdir():
        if ("slurm" in item): 
            fid=open(item,'r')
            x=fid.readlines()
            fid.close()
            kk=0
            for k in range(len(x)):
                line=x[k]
                if ("total time" in line): 
                    t=float(line.split('=')[1])
                    d=dir.split("/")[3]
                    if (d=='vver'): factor=1728.
                    if (d=='vres'): factor=1814.4
                    print(d," : id00"+"%s"%kk,"  = ",factor/t)
                    kk+=1
            print("\n")
    os.chdir(current)

