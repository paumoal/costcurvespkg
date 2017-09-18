####################################################################
#Funcion to calculate TP and FP rates
#Inputs: predictions or scores, classes
#classes: class boolean vector
#predictions: Scores values
####################################################################
TP_FP.rates=function(predictions,classes){
V<-matrix(c(predictions,classes),ncol=2)
V1<-V[order(V[,1],V[,2]),]
Z<-sum(V1[,2] == 0)
O<-sum(V1[,2] == 1)
N<-seq(0,1,length=(Z)+1)
P<-seq(0,1,length=(O)+1)
rates=matrix(0, nrow = length(predictions)+1, ncol = 2)
IZ=which(V1[,2] %in% c(0,2))
IO=which(V1[,2] %in% c(1,2))
for (i in seq(IO)){rates[(IO[i]+1),1]=P[i+1]}
for (j in seq(IZ)){rates[(IZ[j]+1),2]=N[j+1]}
for (k in 2:(length(predictions)+1)){
  if (rates[k,1]>rates[k-1,1]){
    if (rates[k,2]<=rates[k-1,2]){rates[k,2]=rates[k-1,2]}}
  else{rates[k,1]=rates[k-1,1]}}
R<-duplicated(V1[,1])
if(sum(R)>0){ d<-which(R %in% 1)
rates<-rates[-d,]}
colnames(rates) <- c("FP","TP")
return (rates)
}
