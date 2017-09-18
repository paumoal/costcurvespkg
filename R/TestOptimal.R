####################################################################
#Test Optimal treshold choice
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
TestOptimal=function(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
                     plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
                     main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
                     xPosLegend,yPosLegend, cexL)
{ if ((typeof(predictions)!="list")||(typeof(classes)!="list")){
    stop("Predictions type and classes type must be a list")
  }
  if(!exists("TP_FP.rates", mode="function")) source("TP_FP.rates.R")
  Np =length(predictions)
  if(missing(lty)){lty=rep(1, Np)}
  if(missing(col)){col=c("blue", heat.colors(Np-1))}
  if(missing(pch)){pch=sample(c(0,1,2,5,6,c(15:25)),Np, replace=F)}
  if (length(lty)==1){lty=rep(lty[1], Np)}
  if (length(pch)==1){pch=rep(pch[1], Np)}
  if (length(col)==1){col=rep(col[1], Np)
  if (Np>1){
  warning(
    "You have more than one curve to plot,
    you should define other colors to visualize curves better")}}
  if(missing(lwd)){lwd=2}
  if(missing(cex)){cex=1.2}
  if(missing(ylab)){ylab="Loss"}
  if(missing(xPosLegend)){xPosLegend=0.7}
  if(missing(yPosLegend)){yPosLegend=0.97}
  if(missing(cexL)){cexL=0.75}
  if(plotOFF==FALSE){
  if(hold==FALSE){plot.new();
      plot.window(xlim=c(0,1),ylim=c(0,1),xaxs="i", yaxs="i");
      axis(1, at=seq(from = 0, to = 1, by = 0.1));
      axis(2,at=seq(from = 0, to = 1, by = 0.1));
      box();
      if(gridOFF==FALSE)
      {grid(nx = 10, ny =10, col = "lightgray", lty = "dotted",
            lwd = par("lwd"), equilogs = TRUE)}}}
  if(missing(namesClassifiers)){
    namesClassifiers=NULL
    for (i in seq(Np)){namesClassifiers=c(namesClassifiers,
                                          paste("C", i, sep=""))}}
  ####################################################################
  Result <- list()
  nameslegend <- c(NULL)

  for (pred in seq(predictions)){
      S<-unlist(predictions[pred])
    if (uniquec==TRUE) {
      c<-unlist(classes)}
  else{
      if (length(predictions)!=length(classes)){
        stop ("prediction list and classes list may have the same length")}
      else{c<-unlist(classes[pred])}
  }
  ####################################################################
  rates<-TP_FP.rates(S,c)
  TP<-rates[,2]
  FP<-rates[,1]
  AUC<- 0
  pd<-c(0, sort(unique(S)),1)
  if (length(S)<=100){x=seq(from = 0, to = 1, by = 0.001);
  }else{x=seq(from = 0, to = 1, by = 0.01)}
  y <- c(NULL);
  ymin_t<-c(0);
  index_TPmin=c(NULL);
  ####################################################################
  #Loss by Cost Curve
  #loss2skew=NULL or FALSE
  ####################################################################
  if(loss2skew==FALSE){
    pi0=sum(c==0)/length(c);
    pi1=1-pi0;
    for (i in seq(FP)){y <- rbind(y,2*((pi0*(1-TP[i])-pi1*FP[i])*x+pi1*FP[i]))};
    for (i in seq(x)[-1]){ymin_t<-c(ymin_t,min(y[,i]))
    index_TPmin<-c(index_TPmin, which(y[,i]==min(y[,i]))[1])}
    ####################################################################
    index_break_points=c(which(diff(index_TPmin)!=0))
    if (length(index_break_points)==0){
      xmin=c(0,1);ymin=c(0,0);
      treshold=pd[index_TPmin[1]+1]
    }else{
      xmin=x[index_break_points+1];xmin=c(0,xmin, 1);
      ymin=ymin_t[index_break_points+1];ymin=c(0,ymin, 0);
      treshold=c(rbind(pd[index_TPmin[index_break_points]+1],
                       pd[index_TPmin[index_break_points+1]+1]));}
    if(plotOFF==FALSE){
      if(missing(main)){main="Loss by Cost"}
      if(missing(xlab)){xlab="Cost"}
      title(main=main,xlab=xlab,ylab=ylab,font.main= 14);
      lines(xmin,ymin, lty = lty[pred], pch=pch[pred] ,col=col[pred],lwd=lwd);
      if(pointsOFF==FALSE){
        points(xmin, ymin,col=col[pred],pch =pch[pred],cex = cex)}}


  }else{
    ####################################################################
    #Loss by Skew Curve
    #loss2skew=TRUE
    ####################################################################
    for (i in seq(FP)){y<-rbind(y,(1-(TP[i]+FP[i]))*x+FP[i]);}
    for (i in seq(x)[-1]){ymin_t<-c(ymin_t,min(y[,i]))
    index_TPmin<-c(index_TPmin, which(y[,i]==min(y[,i]))[1])}
    ####################################################################
    index_break_points=c(which(diff(index_TPmin)!=0))
    xmin=x[index_break_points+1];xmin=c(0,xmin, 1);
    ymin=ymin_t[index_break_points+1];ymin=c(0,ymin, 0);
    if (length(index_break_points)==0){
      xmin=c(0,1);ymin=c(0,0);
      treshold=pd[index_TPmin[1]+1]
    }else{treshold=c(rbind(pd[index_TPmin[index_break_points]+1],
                           pd[index_TPmin[index_break_points+1]+1]));}
    if(plotOFF==FALSE){
      if(missing(main)){main="Loss by Skew"}
      if(missing(xlab)){xlab="Skew"}
      title(main=main,xlab=xlab,ylab=ylab,font.main= 14);
      lines(xmin,ymin, lty = lty[pred], pch=pch[pred] ,col=col[pred],lwd=lwd);
      if(pointsOFF==FALSE){
        points(xmin, ymin,col=col[pred],pch =pch[pred],cex = cex)}}
  }
  AUC=round(sum(AUC+auc(xmin,ymin, dens=1000)),3)
  nameslegend = c(nameslegend, paste(namesClassifiers[pred],
                                     " AUCC_tO: ", AUC, sep=""))

  result=list(xmin, treshold[!duplicated(treshold, fromLast=TRUE)], AUC)
  names(result)<-c(paste("break_points", namesClassifiers[pred]),
                   paste("treshold", namesClassifiers[pred]),
                   paste("AUC", namesClassifiers[pred]))
  Result <- append(Result, result)
    }
  #Legend
  if(plotOFF==FALSE){
  if(legendOFF == FALSE){
    legend(xPosLegend, yPosLegend, nameslegend, lty = lty, col = col,cex=cexL,
           y.intersp=0.7, x.intersp=0.3, bty="n"); box()
    }}
  return (Result)
}
