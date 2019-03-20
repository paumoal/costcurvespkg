####################################################################
#Kendall Curves (score driven threshold)
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
KendallCurves=function(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
                       plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
                       main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
                       xPosLegend,yPosLegend,cexL){
####################################################################
  Np =length(predictions)
  if ((typeof(predictions)!="list")||(typeof(classes)!="list")){
    stop("Predictions type and classes type must be a list")
  }
  if(!exists("TP_FP.rates", mode="function")) source("TP_FP.rates.R")
  if(missing(col)){col=c("orange",  cm.colors((Np-1)*2)[-seq((Np-1))])}
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
    TP<-rates[,2]# F0 o valores de Y de Curva ROC
    FP<-rates[,1]# F1 o valores de X de curva ROC
    pd=seq(0,1, length.out = length(S)+1)
    pd=c(pd[which(!duplicated(sort(S)))],1)
    y<-c(NULL)
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
  for (i in 1:(length(pd))){
    if (signif(pd[i],digits = 6)<=signif(pi0,digits=6)){
      y=c(y,2*pi1*FP[i])
    }else{
      y=c(y,2*(pi0*(1-TP[i])))
    }
  } }
  ####################################################################
  #Loss by Skew Curve
  #loss2skew=TRUE
  ####################################################################
  else{
    if (plotOFF==FALSE){
      if(missing(main)){main="Loss by Skew"}
      if(missing(xlab)){xlab="Skew"}
      title(main=main,xlab=xlab,ylab=ylab,font.main= 14)}
    for (i in 1:(length(pd))){
      if (signif(pd[i],digits = 6)<=signif(0.5,digits=6)){
        y=c(y,FP[i])
      }else{
        y=c(y,(1-TP[i]))
      }
    }}
    AUC=round(sum(AUC+auc(pd,y, dens=1000)), 3)
    if(plotOFF==FALSE){
      if(pointsOFF==FALSE)
      {points(pd,y,col=col[pred],pch =pch[pred],cex=cex)}
      lines(pd, y,col=col[pred], lwd=lwd, lty=lty[pred])}

    nameslegend = c(nameslegend, paste(namesClassifiers[pred], "AUKC:",
                                       AUC, sep=" "))
    namesResult = c(namesResult, paste(namesClassifiers[pred], "AUKC:", sep=" "))
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
