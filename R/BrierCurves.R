####################################################################
#Brier Curves (score driven threshold)
#Inputs: predictions, classes ,loss2skew, ...
#predictions: list of Scores array values
#classes: list of class boolean array
#uniquec: option to use the same array classes for each predictions
#array in a list.
#loss2skew: TRUE, FALSE, NULL. It's TRUE ploting loss by Skew otherwise
#ploting loss by cost.
#... : plot options (hold, gridOFF, pointsOFF, legendOFF,
#      main, xlab, ylab, nameClassifiers lwd, lty, col, pch, cex...)
####################################################################
BrierCurves=function(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
                     plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
                     main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
                     xPosLegend,yPosLegend, cexL){
  if ((typeof(predictions)!="list")||(typeof(classes)!="list")){
    stop("Predictions type and classes type must be a list")
  }
  Np =length(predictions)
  if(!exists("TP_FP.rates", mode="function")) source("TP_FP.rates.R")
  if(missing(col)){col=c("magenta", cm.colors(Np-1))}
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

  rates=TP_FP.rates(S,c);
  TP=rates[,2]; FP=rates[,1];
  S=sort(unique(S));pd=c(0,S,1);k=0;
  yconex=c(0);
  AUC=0

  ####################################################################
  #Loss by Cost Curve
  #loss2skew=NULL or FALSE
  ####################################################################
  if(loss2skew==FALSE){
    if (plotOFF==FALSE){if(missing(main)){main="Loss by Cost"}
    if(missing(xlab)){xlab="Cost"}
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14)}
    pi0=sum(c==0)/length(c);
    pi1=1-pi0;
    for (i in 1:(length(pd)-1)){
      x=c(pd[i],pd[i+1])
      y=2*(x*(pi0*(1-TP[i])-pi1*FP[i])+pi1*FP[i])
      AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
      yconex=c(yconex,y)
      if(plotOFF==FALSE){
      if(pointsOFF==FALSE)
      {points(x,y,col=col[pred],pch =pch[pred],cex=cex)}
      lines(x,y,col=col[pred], lwd=lwd, lty=lty[pred])}}
    }else{
    ####################################################################
    #Loss by Skew Curve
    #loss2skew=TRUE
    ####################################################################
    if (plotOFF==FALSE){
      if(missing(main)){main="Loss by Skew"}
    if(missing(xlab)){xlab="Skew"}
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14)}
    for (i in 1:(length(pd)-1)){
      x=c(pd[i],pd[i+1])
      y=x*(1-TP[i]-FP[i])+FP[i]
      AUC=round(sum(AUC+auc(x,y, dens=1000)), 3)
      yconex=c(yconex,y)
      if(plotOFF==FALSE){
        if(pointsOFF==FALSE)
      {points(x,y,col=col[pred],pch =pch[pred],cex=cex)}
      lines(x,y,col=col[pred], lwd=lwd, lty=lty[pred])}}

    }
  yconex=yconex[-c(1,2,length(yconex))] #union entre segmentos
  if(plotOFF==FALSE){
    for (i in seq(S)){
      x=c(S[i],S[i])
      y=c(yconex[i+k],yconex[i+k+1])
      lines(x,y,col=col[pred], lwd=lwd, lty=lty[pred])
      k=k+1}}
  nameslegend = c(nameslegend, paste(namesClassifiers[pred], "AUBC:",
                                     AUC, sep=" "))
  namesResult = c(namesResult, paste(namesClassifiers[pred], "AUBC:", sep=" "))
  result=c(result, AUC)
  }
  #Legend
  if(plotOFF==FALSE){
    if(legendOFF == FALSE){
      legend(xPosLegend, yPosLegend, nameslegend, lty = lty, col = col,cex=cexL,
             y.intersp=0.7, x.intersp=0.3, bty="n"); box()
    }}
  names(result)<-namesResult
  return(result)
}
