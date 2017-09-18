####################################################################
#Train Optimal treshold choice
#Inputs: predictions_train, classes_train , predicitions_test, ...
#predictions_train: list of Scores array values
#train classifier
#classes_train: list of class boolean array of training classifier
#predictions_test: list of Scores array values of test
#classes_test: list of class boolean array of test
#uniquecT: option to use the same array classes for each predictions
#training array
#uniquect: option to use the same array classes for each predictions
#test array
#refuseT: option to use the test optimal of only one training classifier
#different training classifier
#loss2skew: TRUE, FALSE, NULL. It's TRUE ploting loss by Skew otherwise
#ploting loss by cost.
#... : plot options (hold, gridOFF, pointsOFF, legendOFF,
#      main, xlab, ylab, nameClassifiers, nameTests lwd, lty, col, pch, cex)
####################################################################
TrainOptimal = function(predictions_train, classes_train,
                           predictions_test, classes_test,
                           uniquecT=FALSE, uniquect=FALSE, refuseT=FALSE,
                           loss2skew=FALSE, hold=FALSE, plotOFF=FALSE,
                           pointsOFF=TRUE, gridOFF=TRUE,legendOFF=FALSE,
                           main, xlab, ylab, namesClassifiers,namesTests,
                           lwd, lty, col,pch, cex, xPosLegend,yPosLegend, cexL){
  if ((typeof(predictions_train)!="list")||(typeof(classes_train)!="list")){
    stop("Predictions type and classes type must be a list")}
  if ((typeof(predictions_test)!="list")||(typeof(classes_test)!="list")){
    stop("Predictions type and classes type must be a list")}
  if(!exists("TestOptimal", mode="function")) source("TestOptimal.R")
  Np =length(predictions_test)
  if(missing(lty)){lty=rep(1, Np)}
  if(missing(col)){col=c("chartreuse3", heat.colors(Np-1))}
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
    for (i in seq(predictions_train)){namesClassifiers=c(namesClassifiers,
                                          paste("C", i, sep=""))}}
  if(missing(namesTests)){
    namesTests=NULL
    for (i in seq(Np)){namesTests=c(namesTests, paste("t", i, sep=""))}}
  ####################################################################
  optimalT <-TestOptimal(predictions_train, classes_train,
                         loss2skew = loss2skew, uniquec=uniquecT,plotOFF=TRUE);
  j=0
  result=c(NULL)
  nameslegend <- c(NULL)
  namesResult <- c(NULL)
  #refuse Training
  for (pred in seq(predictions_test)){
    AUC<- 0
    optimal=c(NULL)
    if (refuseT==TRUE){
      optimal[1]=optimalT[1]
      optimal[2]=optimalT[2]
      namesClassifiers<-rep(namesClassifiers, Np)
    }else{
      optimal[1]=optimalT[pred+j]
      optimal[2]=optimalT[pred+j+1]
    }
    j=j+2;
    S_test<-unlist(predictions_test[pred])
    if (uniquect==TRUE) {
      c_test<-unlist(classes_test)}
    else{
      if (length(predictions_test)!=length(classes_test)){
        stop ("prediction list and classes list may have the same length")}
      else{c_test<-unlist(classes_test[pred])}
  }
  ####################################################################
  V<-matrix(c(S_test,c_test),ncol=2);
  V1<-V[order(V[,1],V[,2]),];
  S_test=V1[,1];c_test=V1[,2];
  FP=c(NULL); TP=c(NULL);
  y<-c(NULL); yconex<-c(NULL);  k<-0;
  break_points<-unlist(optimal[1])
  treshold<-unlist(optimal[2])
  for (i in seq(treshold)){
    Ps=(treshold[i]>S_test)*1
    TP=c(TP,sum((Ps==1)*(c_test==0))/sum(c_test==0))
    FP=c(FP,sum((Ps==1)*(c_test==1))/sum(c_test==1))}
  ####################################################################
  #Loss by Cost Curve
  #loss2skew=NULL or FALSE
  ####################################################################
  if(loss2skew==FALSE){
    if(plotOFF==FALSE){
    if(missing(main)){main="Loss by Cost"}
    if(missing(xlab)){xlab="Cost"}
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14);}
    pi0=sum(c_test==0)/length(c_test); pi1=1-pi0;
    for (i in 1:(length(break_points)-1)){
      x=c(break_points[i],break_points[i+1]);
      y=2*(x*(pi0*(1-TP[i])-pi1*FP[i])+pi1*FP[i]);

      yconex=c(yconex,y);
      AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
    #Plot
    if(plotOFF==FALSE){
      if(pointsOFF==FALSE)
        {points(x,y,col=col[pred],pch =pch[pred],cex=cex)}
      lines(x,y,col=col[pred], lwd=lwd, lty=lty[pred])}}

  }else{
    ####################################################################
    #Loss by Skew Curve
    #loss2skew=TRUE
    ####################################################################
    if(plotOFF==FALSE){if(missing(main)){main="Loss by Skew"}
    if(missing(xlab)){xlab="Skew"}
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14)}
    for (i in 1:(length(break_points)-1)){
      x=c(break_points[i],break_points[i+1])
      y=x*(1-TP[i]-FP[i])+FP[i]
      yconex=c(yconex,y)
      AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
      #plot
      if(plotOFF==FALSE){
        if(pointsOFF==FALSE)
          {points(x,y,col=col[pred],pch =pch[pred],cex=cex)}
        lines(x,y,col=col[pred], lwd=lwd, lty=lty[pred])}}

      }
  if(plotOFF==FALSE){
    yconex=yconex[-c(1,length(yconex))] #union entre segmentos
    xconex=break_points[-c(1,length(break_points))]
    for (i in seq(xconex))
    {xfoo=c(xconex[i],xconex[i]);
    yfoo=c(yconex[i+k],yconex[i+k+1]);
    lines(xfoo,yfoo,col=col[pred], lwd=lwd, lty=lty[pred]);
    k=k+1;}}
  nameslegend = c(nameslegend, paste(namesClassifiers[pred],
                                     #"/", namesTests[pred],
                                     "AUCC_TO:",
                                     AUC, sep=" "))
  namesResult = c(namesResult, paste(namesClassifiers[pred],"/",
                                     namesTests[pred],"AUCC:", sep=" "))

  result<-c(result, AUC)
  }
  names(result)<-namesResult
  #Legend
  if (plotOFF==FALSE){
    if(legendOFF == FALSE){
      legend(xPosLegend, yPosLegend, nameslegend, lty = lty,
             col = col,cex=cexL, y.intersp=0.7, x.intersp=0.3, bty="n")
  box()}}

  return(result)
}

