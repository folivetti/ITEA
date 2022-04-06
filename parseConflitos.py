f = open("results/conflito2/stats.csv")
lastResult = f.readlines()[-1]
f.close()
data = lastResult.split(",")
header = "name,time,length,Log-Loss_test,Log-Loss_train,Precision_test,Precision_train,Recall_test,Recall_train,F1_test,F1_train,Accuracy_test,Accuracy_train".split(",")

ddata = {k:v for k,v in zip(header,data)}

for k in header:
    if "train" in k and "Log" not in k:
        print(k, ddata[k])
print()        
for k in header:
    if "test" in k and "Log" not in k:
        print(k, ddata[k])
