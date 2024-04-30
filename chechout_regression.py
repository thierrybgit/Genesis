import os 
current=os.getcwd()
os.chdir('genesis-lumi/tests/regression_test')
for item in os.listdir():
    if ("slurm" in item): 
        fid=open(item,'r')
        x=fid.readlines()
        fid.close()
        for k in range(len(x)):
            line = x[k]
            if ("Passed" in line and "/" in line): 
                kk = k
                succ=int(line.split()[1])
                fail=int(x[kk+1].split()[1])
                abor=int(x[kk+2].split()[1])
                break
        passed = (succ==56) and (fail==0) and (abor==0)
        if (passed) : print ("Ok regression test")
        else: print("success = ",succ," failed = ",fail," aborded = ",abor)
        break

