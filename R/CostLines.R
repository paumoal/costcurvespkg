####################################################################
#CostCurves Function
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
CostLines=function(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
                    gridOFF=TRUE, legendOFF=FALSE, main, xlab, ylab, namesClassifiers,
                    lwd, lty, col, xPosLegend,yPosLegend, cexL)
  {
  if ((typeof(predictions)!="list")||(typeof(classes)!="list")){
    stop("Predictions type and classes type must be a list")
  }
  if(!exists("TP_FP.rates", mode="function")) source("TP_FP.rates.R")
  if(missing(lwd)){lwd=2}
  if(missing(ylab)){ylab="Loss"}
  if(missing(xPosLegend)){xPosLegend=0.74}
  if(missing(yPosLegend)){yPosLegend=0.97}
  if(missing(cexL)){cexL=0.75}
  Np =length(predictions)
  if(missing(lty)){lty=rep(5, Np)}
  if(missing(col)){cl<-terrain.colors(Np+3)
    col=c("gray73", cl[-c(1:3)])}
  if (length(lty)==1){lty=rep(lty[1], Np)}
  if (length(col)==1){col=rep(col[1], Np)
  if (Np>1){
  warning(
    "You have more than one curve to plot,
    you should define other colors to visualize curves better")}}
  if(hold==FALSE){
  plot.new()
  plot.window(xlim=c(0,1),ylim = c(0,1),xaxs="i", yaxs="i");
  axis(1, at=seq(from = 0, to = 1, by = 0.1));
  axis(2,at=seq(from = 0, to = 1, by = 0.1));
  box();}
  if(gridOFF==FALSE){
    grid(nx = 10, ny =10, col = "lightgray",
         lty = "dotted",lwd = par("lwd"), equilogs = TRUE)}

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
  ####################################################################
  #Loss by Cost Curve
  #loss2skew=NULL or FALSE
  ####################################################################
  if(loss2skew==FALSE){
    pi0=sum(c==0)/length(c);
    pi1=1-pi0;
    if (hold==FALSE){
      if(missing(main)){main="Loss by Cost"}
      if(missing(xlab)){xlab="Cost"}
      title(main=main,xlab=xlab,ylab=ylab,font.main= 14);}
    # Lazo para grafica multiples rectas a=intercepto, b=pendiente
    for (i in seq(FP)){abline(a=2*FP[i]*pi1, b=2*(pi0*(1-TP[i])-pi1*FP[i]),
                       col = col[pred], lty=lty[pred], lwd=lwd)};
    }else{
    ####################################################################
    #Loss by Skew Curve
    #loss2skew=TRUE
    ####################################################################
      if (hold==FALSE){
        if(missing(main)){main="Loss by Skew"}
        if(missing(xlab)){xlab="Skew"}
        title(main=main,xlab=xlab,ylab=ylab,font.main= 14);}

    for (i in seq(FP)){
      abline(a=FP[i], b=(1-(TP[i]+FP[i])), col = col[pred], lty=lty[pred], lwd=lwd);

      }
    }
  }
  if(legendOFF == FALSE){
    if(missing(namesClassifiers)){
      namesClassifiers=NULL
      for (i in seq(Np)){namesClassifiers=c(namesClassifiers,
                                            paste("classifier", i, sep=" "))}}
    legend(xPosLegend, yPosLegend, namesClassifiers, lty = lty, col = col,cex=cexL,
           y.intersp=0.7, x.intersp=0.3, bty="n")
    box();
  }
}
