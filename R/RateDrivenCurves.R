####################################################################
#Rate Driven
#Inputs: predictions, classes ,loss2skew, ...
#predictions: list of Scores array values
#classes: list of class boolean array
#uniquec: option to use the same array classes for each predictions
#array in a list.
#loss2skew: TRUE, FALSE, NULL. It's TRUE ploting loss by Skew otherwise
#ploting loss by cost.
#... : plot options (hold, gridOFF, legendOFF, main, xlab, ylab, lwd,
#      lty, col, namesClassifiers)
####################################################################
RateDrivenCurves=function(predictions, classes, uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
                          plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
                          main, xlab, ylab, namesClassifiers, col, lwd, lty, pch, cex,
                          xPosLegend, yPosLegend, cexL){
  if ((typeof(predictions)!="list")||(typeof(classes)!="list")){
    stop("Predictions type and classes type must be a list")
  }
  Np =length(predictions)
  if(!exists("TP_FP.rates", mode="function")) source("TP_FP.rates.R")
  if(missing(col)){col=c(topo.colors(Np))}
  if(missing(pch)){pch=sample(c(0,1,2,5,6,c(15:25)),Np, replace=F)}
  if(missing(lwd)){lwd=2}
  if(missing(lty)){lty=rep(1, Np)}
  if(missing(cex)){cex=1.2}
  if (length(lty)==1){lty=rep(lty[1], Np)}
  if (length(pch)==1){pch=rep(pch[1], Np)}
  if (length(col)==1){col=rep(col[1], Np)
  if (Np>1){
  warning(
    "You have more than one curve to plot,
    you should define other colors to visualize curves better")}}
  if(missing(ylab)){ylab="Loss"}
  if(missing(xPosLegend)){xPosLegend=0.7}
  if(missing(yPosLegend)){yPosLegend=0.97}
  if(missing(cexL)){cexL=0.75}
  if(plotOFF==FALSE){
    if(hold==FALSE){plot.new()
      plot.window(xlim=c(0,1),ylim=c(0,1),xaxs="i", yaxs="i");
      axis(1, at=seq(from = 0, to = 1, by = 0.1));
      axis(2,at=seq(from = 0, to = 1, by = 0.1));
      box();
      if(gridOFF==FALSE){
        grid(nx = 10, ny =10,col = "lightgray", lty = "dotted",
             lwd = par("lwd"), equilogs = TRUE)}}}
  if(missing(namesClassifiers)){
    namesClassifiers=NULL
    for (i in seq(Np)){namesClassifiers=c(namesClassifiers,
                                          paste("C", i, sep=""))}}
  ####################################################################
  result=c(NULL)
  nameslegend <- c(NULL)
  namesResult <- c(NULL)
  for (pred in seq(predictions)){
    S<-unlist(predictions[pred])
    if (uniquec==TRUE) {
      c<-unlist(classes)}
    else{
      if (length(predictions)!=length(classes)){
        stop ("prediction list and classes list may have the same length")}
      else{c<-unlist(classes[pred])}
    }
    rates<-TP_FP.rates(S,c)

    tP<-rates[,2]# F0 o valores de Y de Curva ROC
    fP<-rates[,1]# F1 o valores de X de curva ROC
    pd=seq(0,1, length.out = length(S)+1)
    V<-matrix(c(S,c),ncol=2); V<-V[order(V[,1],V[,2]),];
    if (sum(duplicated(S))==0){
      TP<-tP; FP<-fP;
    }else{
      index<-which(!duplicated(V))
      V<-V[index,]
      TP<-rep(0, length(V[,1]))
      FP<-rep(0, length(V[,1]))
      dup<-duplicated(V[,1])
      k1=0
    for (i in seq(V[,1])){
      if (dup[i]==FALSE){
        TP[i]=tP[i-k1]
        FP[i]=fP[i-k1]
      }else{
        TP[i]=TP[i-1-k1]
        FP[i]=FP[i-1-k1]
        k1=k1+1
      }}
    TP<-c(TP,1)
    FP<-c(FP,1)
    pd=c(pd[index],1)
    }
    classOrder<-c(V[,2])
    AUC<-0
    pi0=sum(c==0)/length(c);
    pi1=1-pi0;
####################################################################
#Loss by Cost Curve
#loss2skew=NULL or FALSE
####################################################################
if(loss2skew==FALSE){
  if (plotOFF==FALSE){if(missing(main)){main="Loss by Cost"}
    if(missing(xlab)){xlab="Cost"}
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14)}
  if(length(unique(S))<=15)  {
  for (i in 1:(length(pd)-1)){
    cost=seq(pd[i],pd[i+1], length.out =10)
    if (classOrder[i]==0)
    {y=2*(cost*(pi0-cost)+pi1*FP[i+1])}
    else{y=2*((1-cost)*(cost-pi0)+pi0*(1-TP[i+1]))}
    if(plotOFF==FALSE){
     if(pointsOFF==FALSE)
    {points(c(cost[1], cost[11]),c(y[1], y[11]),col=col[pred],pch =pch[pred],cex=cex)}
    lines(cost,y,col=col[pred], lwd=lwd, lty=lty[pred])}
      AUC<-round(sum(AUC+auc(cost,y, dens=1000)),3)}}

  else{
    pd=seq(0,1, length.out = length(unique(S))+1)
    TP<-tP
    FP<-fP
    X=seq(0,1, length.out = length(unique(S))+1)
    for (i in 1:(length(pd)-1)){
      y1=2*(pd[i]*pi0*(1-TP[i])+(1-pd[i])*pi1*FP[i])
      y2=2*(pd[i+1]*pi0*(1-TP[i+1])+(1-pd[i+1])*pi1*FP[i+1])
      y=c(NULL)
      for (j in seq(X)){
        y=c(y, y1*(1-X[j])+y2*X[j])}
      x=seq(pd[i], pd[i+1],length.out = length(unique(S))+1)
      AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
      if(plotOFF==FALSE){
         if(pointsOFF==FALSE)
        {points(c(x[1], x[11]),c(y[1], y[11]),col=col[pred],pch =pch[pred],cex=cex)}
      lines(x,y, col=col[pred], lwd=lwd, lty=lty[pred])}}


  }
  }
####################################################################
#Loss by Skew Curve
#loss2skew=TRUE
####################################################################
else{
  if (plotOFF==FALSE){if(missing(main)){main="Loss by Skew"}
    if(missing(xlab)){xlab="Skew"}
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14)}

    if ((pi0==0.5) && (length(unique(S))<=15 )){
      for (i in 1:(length(pd)-1)){
        cost=seq(pd[i],pd[i+1], length.out =10)
        if (classOrder[i]==0)
        {y=2*(cost*(pi0-cost)+pi1*FP[i+1])}
        else{y=2*((1-cost)*(cost-pi0)+pi0*(1-TP[i+1]))}
        if(plotOFF==FALSE){
          if(pointsOFF==FALSE)
          {points(c(cost[1], cost[11]),c(y[1], y[11]),col=col[pred],pch =pch[pred],cex=cex)}
          lines(cost,y,col=col[pred], lwd=lwd, lty=lty[pred])}
          AUC<-round(sum(AUC+auc(cost,y, dens=1000)),3)}

    }else{
    TP=tP
    FP=fP
    pd=seq(0,1, length.out = length(unique(S))+1)
    X=seq(0,1, length.out = length(unique(S))+1)
    for (i in 1:(length(pd)-1)){
       y1=((1-TP[i])-FP[i])*pd[i]+FP[i]
       y2=((1-TP[i+1])-FP[i+1])*pd[i+1]+FP[i+1]
       y=c(NULL)
       for (j in seq(X)){
          y=c(y, y1*(1-X[j])+y2*X[j])}
       x=seq(pd[i], pd[i+1],length.out = length(unique(S))+1)
       AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
       if(plotOFF==FALSE){
         if(pointsOFF==FALSE)
         {points(c(x[1], x[11]),c(y[1], y[11]),col=col[pred],pch =pch[pred],cex=cex)}
         lines(x,y,col=col[pred], lwd=lwd, lty=lty[pred])}}}
}
  nameslegend = c(nameslegend, paste(namesClassifiers[pred], "AURDC:",
                                     AUC, sep=" "))
  namesResult = c(namesResult, paste(namesClassifiers[pred], "AURDC:", sep=" "))
  result=c(result, AUC)
  }
  #Legend
  if(plotOFF==FALSE){
    if(legendOFF == FALSE){
      legend(xPosLegend, yPosLegend, nameslegend, lty = lty, col = col,cex=cexL,
             y.intersp=0.7, x.intersp=0.3, bty="n"); box()}}

  names(result)<-namesResult
  return(result)
}
