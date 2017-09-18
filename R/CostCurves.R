###################################################################
#Cost Cuves function
#Inputs: predictions, classes ,loss2skew, ...
#predictions: list of Scores array values
#classes: list of class boolean array
#uniquec: option to use the same array classes for each predictions
#array in a list.
#uniquecT, uniquetrain
#train_optimal, predictionT, classesT,
#score_driven,
#rate_driven,
#kendall_curves,
#optimal_rate_driven,
#loss2skew: TRUE, FALSE, NULL
#plot argument (gridOFF, pointsOFF,main, ...)
####################################################################
CostCurves=function(predictions, classes, cost_lines = TRUE,
                          test_optimal = TRUE, train_optimal = FALSE,
                          predictionsT = NULL, classesT = NULL,
                          uniquec=FALSE, uniqueTrain=FALSE, uniquecT=FALSE,
                          score_driven = FALSE, rate_driven = FALSE,
                          kendall_curves = FALSE, loss2skew = FALSE,
                          hold = FALSE, gridOFF = TRUE, pointsOFF = TRUE,
                          legendOFF = FALSE,
                          main, xlab, ylab, namesClassifiers, col, lwd,
                          lty, pch, cex,xPosLegend,yPosLegend, cexL) {
  ####################################################################
  #               Initialization Parameters                          #
  ####################################################################
  if(!exists("TP_FP.rates", mode="function")) source("TP_FP.rates.R")
  if ((typeof(predictions)!="list")||(typeof(classes)!="list")){
    stop("Predictions type and classes type must be a list")
  }

  Np =length(predictions)
  col_plot=rep(0, 6); lty_plot=rep(0, 6); lwd_plot=rep(0, 6);
  pch_plot=rep(0, 6); cex_plot=rep(0, 6);
  parameters <- c(cost_lines == TRUE, test_optimal == TRUE,
                  train_optimal == TRUE, score_driven == TRUE,
                  rate_driven == TRUE, kendall_curves == TRUE);
  index <- which (parameters == TRUE)
  Ni = length(index)
  if (Ni==0){
    stop ("You must choice an option to plot:
          At least one of the following arguments must be TRUE
          1. cost_lines
          2. test_optimal
          3. train_optimal
          4. score_driven
          5. rate_driven
          6. kendall_curves")}
  if ((uniquecT == TRUE)&& (uniqueTrain == TRUE)){
    stop ("You must choice only one option. uniquecT or uniqueTrain")}

  if(missing(col)){
    if(Ni >1){
      color_cost_lines = c("gray73", terrain.colors(Np+3)[-c(1:3)])
      color_test_optimal = heat.colors(Np)
      color_train_optimal = topo.colors(3*Np)[-seq(2*Np)]
      color_score_driven = c("magenta", cm.colors(Np-1))
      color_rate_driven = topo.colors(Np)
      color_kendall_curves = c("orange", cm.colors((Np-1)*2)[-seq((Np-1))])
    }else{
      if(index[1]==1){color = terrain.colors(Np)}
      else{color = heat.colors(Np)}
    }
    col_ ="missing col"}
  else{
    if (!((Np==1)&&(Ni==1))){
      if(typeof(col)=="list"){
        if(length(col)==1){
          if((length(unlist(col))==1)&&((Np>1)||(Ni>1))){
          warning(
            "You have more than one curve to plot,
you should define other colors to visualize curves better")} else{
          if(((length(unlist(col))==Np)||(length(unlist(col))==Ni))&&(
            (Ni>1)&&(Np>1))){
          warning(
            "You have more than one curve to plot,
you should define other colors to visualize curves better.")}}}else{
          if((length(unlist(col[1]))==1)&&((length(col)==Np)||(
          length(col)==Ni))&&((Ni>1)&&(Np>1))){
          warning(
            "You have more than one curve to plot,
you should define other colors to visualize curves better")}}}else{
          if ((length(col)==1)&&(Np>1||Ni>1)){
            warning(
              "You have more than one curve to plot,
you should define other colors to visualize curves better")}
        if (((Ni>1)&&(Np>1))&&((length(col)==Np)||(length(col)==Ni))){
            warning(
              "You have more than one curve to plot,
you should define other colors to visualize curves better")}
          }}
        col_ =col}

  if(missing(lty)){lty=c(1, rep(5,5));
  lty_plot[index] = lty[index]} else {
    lty_plot[index]=lty}

  if(missing(lwd)){lwd=c(1.5, rep(2,5));
  lwd_plot[index] = lwd[index]} else {
    lwd_plot[index] = lwd}

  if(missing(pch)){pch_plot[index] = 16}
  else {pch_plot[index] = pch}

  if(missing(cex)){cex_plot[index] = 0.7}
  else {cex_plot[index] = cex}

  if(missing(xPosLegend)){xPosLegend=0.7}
  if(missing(yPosLegend)){yPosLegend=0.97}
  if(missing(cexL)){cexL=0.75}

  if(missing(hold)||hold==FALSE){ plot.new();
    plot.window(xlim=c(0,1),ylim=c(0,1),xaxs="i", yaxs="i");
    axis(1, at=seq(from = 0, to = 1, by = 0.1));
    axis(2,at=seq(from = 0, to = 1, by = 0.1)); box();
    if(missing(ylab)){ylab="Loss"}
    if(missing(loss2skew)||loss2skew==FALSE){
      if(missing(main)){main="Loss by Cost"}
      if(missing(xlab)){xlab="Cost"}
    } else{
      if(missing(main)){main="Loss by Skew"}
      if(missing(xlab)){xlab="Skew"}
    }
    title(main=main,xlab=xlab,ylab=ylab,font.main= 14);
    if(missing(gridOFF)||gridOFF==FALSE){
      grid(nx = 10, ny =10, col = "lightgray",
           lty = "dotted",lwd = par("lwd"), equilogs = TRUE)}}

  if(missing(namesClassifiers)){
    namesClassifiers=NULL
    if (Np==1){namesClassifiers<-""}else{
      for (i in seq(Np)){namesClassifiers=c(namesClassifiers,
                                            paste("C", i, sep=""))}}}

  ####################################################################
  #                          FUNCTIONS                               #
  ####################################################################
  CostLines = function(TP,FP, loss2skew=FALSE, lwd, lty, col){
    ####################################################################
    #Loss by Cost Curve
    if(loss2skew==FALSE){
      pi0=sum(c==0)/length(c);
      pi1=1-pi0;
      # Lazo para grafica multiples rectas a=intercepto, b=pendiente
      for (i in seq(FP)){abline(a=2*FP[i]*pi1, b=2*(pi0*(1-TP[i])-pi1*FP[i]),
                                col = col, lty=lty, lwd=lwd)};
    }else{
      #Loss by Skew Curve
      for (i in seq(FP)){
        abline(a=FP[i], b=(1-(TP[i]+FP[i])), col = col, lty=lty, lwd=lwd);}}
  }
  ####################################################################
  TestOptimal = function(TP, FP, S, loss2skew=FALSE, pointsOFF=TRUE,
                         lwd, lty, col, pch, cex, plotOFF=FALSE){
    ####################################################################

    pd<-c(0, sort(unique(S)),1)
    if (length(S)<=100){x=seq(from = 0, to = 1, by = 0.001);
    }else{x=seq(from = 0, to = 1, by = 0.01)}
    y <- c(NULL);
    ymin_t<-c(0);
    index_TPmin=c(NULL);
    #Loss by Cost Curve
    if(loss2skew==FALSE){
      pi0=sum(c==0)/length(c);
      pi1=1-pi0;
      for (i in seq(FP)){y <- rbind(y,2*((pi0*(1-TP[i])-pi1*FP[i])*x+pi1*FP[i]))};
      for (i in seq(x)[-1]){ymin_t<-c(ymin_t,min(y[,i]))
      index_TPmin<-c(index_TPmin, which(y[,i]==min(y[,i]))[1])}
      index_break_points=c(which(diff(index_TPmin)!=0))
      if (length(index_break_points)==0){
        xmin=c(0,1);ymin=c(0,0);
        treshold=pd[index_TPmin[1]+1]
      }else{
        xmin=x[index_break_points+1];xmin=c(0,xmin, 1);
        ymin=ymin_t[index_break_points+1];ymin=c(0,ymin, 0);
        treshold=c(rbind(pd[index_TPmin[index_break_points]+1],
                         pd[index_TPmin[index_break_points+1]+1]));}

    }else{
      #Loss by Skew Curve
      for (i in seq(FP)){y<-rbind(y,(1-(TP[i]+FP[i]))*x+FP[i]);}
      for (i in seq(x)[-1]){ymin_t<-c(ymin_t,min(y[,i]))
      index_TPmin<-c(index_TPmin, which(y[,i]==min(y[,i]))[1])}
      index_break_points=c(which(diff(index_TPmin)!=0))
      xmin=x[index_break_points+1];xmin=c(0,xmin, 1);
      ymin=ymin_t[index_break_points+1];ymin=c(0,ymin, 0);
      if (length(index_break_points)==0){
        xmin=c(0,1);ymin=c(0,0);
        treshold=pd[index_TPmin[1]+1]
      }else{treshold=c(rbind(pd[index_TPmin[index_break_points]+1],
                             pd[index_TPmin[index_break_points+1]+1]));}
    }
    if (plotOFF==FALSE){
      lines(xmin,ymin, lty = lty, pch=pch ,col=col,lwd=lwd);
      if(pointsOFF==FALSE){
        points(xmin, ymin,col=col,pch =pch,cex = cex)}}
    AUC<-round(auc(xmin,ymin, dens=1000),3)
    Result<-list(xmin, treshold[!duplicated(treshold, fromLast=TRUE)], AUC)
    return (Result)
  }
  ####################################################################
  TrainOptimal = function(S, c, break_points, treshold, loss2skew = FALSE,
                             lwd, lty, col,pch, cex, pointsOFF = TRUE){
    ####################################################################
    AUC<- 0;  optimal=c(NULL)
    S_test<-S;  c_test<-c;
    V<-matrix(c(S_test,c_test),ncol=2);
    V1<-V[order(V[,1],V[,2]),];
    S_test=V1[,1];c_test=V1[,2];
    FP=c(NULL); TP=c(NULL);
    y<-c(NULL); yconex<-c(NULL);  k<-0;
    for (i in seq(treshold)){
      Ps=(treshold[i]>S_test)*1
      TP=c(TP,sum((Ps==1)*(c_test==0))/sum(c_test==0))
      FP=c(FP,sum((Ps==1)*(c_test==1))/sum(c_test==1))}
    #Loss by Cost Curve
    if(loss2skew==FALSE){
      pi0=sum(c_test==0)/length(c_test); pi1=1-pi0;
      for (i in 1:(length(break_points)-1)){
        x=c(break_points[i],break_points[i+1]);
        y=2*(x*(pi0*(1-TP[i])-pi1*FP[i])+pi1*FP[i]);
        yconex=c(yconex,y);
        AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
        if(pointsOFF==FALSE)
        {points(x,y,col=col,pch =pch,cex=cex)}
        lines(x,y,col=col, lwd=lwd, lty=lty)}
    }else{
      #Loss by Skew Curve
      for (i in 1:(length(break_points)-1)){
        x=c(break_points[i],break_points[i+1])
        y=x*(1-TP[i]-FP[i])+FP[i]
        yconex=c(yconex,y)
        AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
        if(pointsOFF==FALSE)
        {points(x,y,col=col,pch =pch,cex=cex)}
        lines(x,y,col=col, lwd=lwd, lty=lty)}}

    yconex=yconex[-c(1,length(yconex))]
    xconex=break_points[-c(1,length(break_points))]

    for (i in seq(xconex))
    {xfoo=c(xconex[i],xconex[i]);
    yfoo=c(yconex[i+k],yconex[i+k+1]);
    lines(xfoo,yfoo,col=col, lwd=lwd, lty=lty);
    k=k+1;}
    return(AUC)
  }
  ####################################################################
  BrierCurves=function(TP, FP, S, loss2skew=FALSE, pointsOFF=TRUE, lwd,
                       lty, col, pch, cex){
    ####################################################################
    S=sort(unique(S));pd=c(0,S,1);k=0;
    yconex=c(0);
    AUC=0
    if(loss2skew==FALSE){
      #Loss by Cost Curve
      pi0=sum(c==0)/length(c);
      pi1=1-pi0;
      for (i in 1:(length(pd)-1)){
        x=c(pd[i],pd[i+1])
        y=2*(x*(pi0*(1-TP[i])-pi1*FP[i])+pi1*FP[i])
        AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
        yconex=c(yconex,y)
        if(pointsOFF==FALSE)
        {points(x,y,col=col,pch =pch,cex=cex)}
        lines(x,y,col=col, lwd=lwd, lty=lty)}
    }else{
      #Loss by Skew Curve
      for (i in 1:(length(pd)-1)){
        x=c(pd[i],pd[i+1])
        y=x*(1-TP[i]-FP[i])+FP[i]
        AUC=round(sum(AUC+auc(x,y, dens=1000)), 3)
        yconex=c(yconex,y)
        if(pointsOFF==FALSE)
        {points(x,y,col=col,pch =pch,cex=cex)}
        lines(x,y,col=col, lwd=lwd, lty=lty)}}
    yconex=yconex[-c(1,2,length(yconex))] #union entre segmentos
    for (i in seq(S)){
      x=c(S[i],S[i])
      y=c(yconex[i+k],yconex[i+k+1])
      lines(x,y,col=col, lwd=lwd, lty=lty)
      k=k+1}
    return(AUC)
  }
  ####################################################################
  RateDrivenCurves=function(tP, fP, S, c, loss2skew=FALSE,
                            pointsOFF=TRUE, lwd, lty, col, pch, cex){
    ####################################################################
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
    #Loss by Cost Curve
    if(loss2skew==FALSE){
      if(length(unique(S))<=15){
      for (i in 1:(length(pd)-1)){
        cost=seq(pd[i],pd[i+1], length.out =10)
        if (classOrder[i]==0)
        {y=2*(cost*(pi0-cost)+pi1*FP[i+1])}
        else{y=2*((1-cost)*(cost-pi0)+pi0*(1-TP[i+1]))}
        if(pointsOFF==FALSE)
        {points(c(cost[1], cost[11]),c(y[1], y[11]),col=col,pch =pch,cex=cex)}
        lines(cost,y,col=col, lwd=lwd, lty=lty)
        AUC<-round(sum(AUC+auc(cost,y, dens=1000)),3)}}
      else{
        TP<-tP
        FP<-fP
        pd=seq(0,1, length.out = length(unique(S))+1)
        X=seq(0,1, length.out = length(unique(S))+1)
        for (i in 1:(length(pd)-1)){
          y1=2*(pd[i]*pi0*(1-TP[i])+(1-pd[i])*pi1*FP[i])
          y2=2*(pd[i+1]*pi0*(1-TP[i+1])+(1-pd[i+1])*pi1*FP[i+1])
          y=c(NULL)
          for (j in seq(X)){
            y=c(y, y1*(1-X[j])+y2*X[j])}
          x=seq(pd[i], pd[i+1],length.out = length(unique(S))+1)
          AUC=round(sum(AUC+auc(x,y, dens=1000)),3)
          if(pointsOFF==FALSE)
            {points(c(x[1], x[11]),c(y[1], y[11]),col=col,pch =pch,cex=cex)}
            lines(x,y, col=col, lwd=lwd, lty=lty)}
      }
    }else{
      #Loss by Skew Curve
      if (pi0==0.5){
        for (i in 1:(length(pd)-1)){
          cost=seq(pd[i],pd[i+1], length.out =10)
          if (classOrder[i]==0)
          {y=2*(cost*(pi0-cost)+pi1*FP[i+1])}
          else{y=2*((1-cost)*(cost-pi0)+pi0*(1-TP[i+1]))}
          if(pointsOFF==FALSE)
          {points(c(cost[1], cost[11]),c(y[1], y[11]),col=col,pch =pch,cex=cex)}
          lines(cost,y,col=col, lwd=lwd, lty=lty)
          AUC<-round(sum(AUC+auc(cost,y, dens=1000)),3)}
      }else{
        TP<-tP
        FP<-fP
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
          if(pointsOFF==FALSE)
          {points(c(x[1], x[11]),c(y[1], y[11]),col=col,pch =pch,cex=cex)}
          lines(x,y,col=col, lwd=lwd, lty=lty)}}
    }
    return(AUC)
  }
  ####################################################################
  KendallCurves=function(TP, FP, S, loss2skew=FALSE,
                         pointsOFF=TRUE, lwd, lty, col, pch, cex){
    ####################################################################
    pd=seq(0,1, length.out = length(S)+1)
    pd=c(pd[which(!duplicated(sort(S)))],1)
    y<-c(NULL)
    #loss2skew=NULL or FALSE
    if(loss2skew==FALSE){
      pi0=sum(c==0)/length(c);
      pi1=1-pi0;
      for (i in 1:(length(pd))){
        if (signif(pd[i],digits = 6)<=signif(pi0,digits=6)){
          y=c(y,2*pi1*FP[i])
        }else{
          y=c(y,2*(pi0*(1-TP[i])))
        }
      }}else{
        #Loss by Skew Curve
        for (i in 1:(length(pd))){
          if (signif(pd[i],digits = 6)<=signif(0.5,digits=6)){
            y=c(y,FP[i])
          }else{
            y=c(y,(1-TP[i]))
          }
        }}
    AUC=round(auc(pd,y, dens=1000), 3)
    if(pointsOFF==FALSE)
    {points(pd,y,col=col,pch =pch,cex=cex)}
    lines(pd, y,col=col, lwd=lwd, lty=lty)
    return(AUC)
  }

  ####################################################################
  #                        INIT PROCESS                              #
  ####################################################################
  COL<-c(NULL);  namesLegend<-c(NULL); Result<-list();
  AUC_tO<-c(NULL); AUC_TO<-c(NULL); AUBC<-c(NULL); AURDC<-c(NULL)
  AUKC<-c(NULL); AUC_names<-c("AUC_tO","AUC_TO","AUBC","AURDC","AUKC")

  for (pred in seq(predictions)){
    result<-c(NULL)
    if(col_[1] =="missing col"){
      if(Ni>1){
        col_plot = c(color_cost_lines[pred],
                     color_test_optimal[pred],
                     color_train_optimal[pred],
                     color_score_driven[pred],
                     color_rate_driven[pred],
                     color_kendall_curves[pred])
      }
      else{
        col_plot[index]=color[pred]}}
    else{
      col=col_
      if(Ni>1){
        if (typeof(col)=="list"){
          if(length(col)==1){
            col=unlist(col);
            col_plot[index] = col}
          else{
            col= unlist(col[pred])
            col_plot[index] = col}}
        else{
          col_plot[index] = col}}
      else{
        if (typeof(col)=="list"){
          if(length(col)==1){
            if(length(unlist(col))==1){
              col=unlist(col);
              col_plot[index] = col}
            else{
              col=unlist(col);
              col_plot[index] = col[pred]}}
          else{
            col=unlist(col[pred])
            col_plot[index] = col}}
        else{
          if(length(col)==1){
            col_plot[index]=col}
          else{
            col_plot[index] = col[pred]}}}}

    S<-unlist(predictions[pred])
    if (uniquec==TRUE) {
      c<-unlist(classes)}
    else{
      if (length(predictions)!=length(classes)){
        stop ("prediction list and classes list may have the same length")}
      else{c<-unlist(classes[pred])}}

    ####################################################################
    #                 Initialization Variables                         #
    ####################################################################
    rates<-TP_FP.rates(S,c)
    TP<-rates[,2]# F0 o valores de Y de Curva ROC
    FP<-rates[,1]# F1 o valores de X de curva ROC

    ####################################################################
    #                         cALL FUNCTIONS                           #
    ####################################################################
    if (parameters[1]==TRUE){
      CostLines(TP,FP,loss2skew = loss2skew,
                 col = col_plot[1], lwd = lwd_plot[1], lty = lty_plot[1])
      namesLegend<-c(namesLegend, paste(namesClassifiers[pred], " Cost_Curve", sep=" "))
      COL<-c(COL, col_plot[1])
    }

    if (parameters[2]==TRUE){
      AUC_tO<- TestOptimal(TP,FP, S, loss2skew = loss2skew, pointsOFF = pointsOFF,
                           col = col_plot[2], pch = pch_plot[2], lwd = lwd_plot[2],
                           lty = lty_plot[2], cex = cex_plot[2])
      namesLegend<-c(namesLegend, paste(namesClassifiers[pred], " AUC_tO: ", unlist(AUC_tO[3]), sep=""))
      COL<-c(COL, col_plot[2])
      result<-c(result, unlist(AUC_tO[3]))

    }

    if (parameters[3]==TRUE){
      if (uniqueTrain ==TRUE){
        S_train<-unlist(predictionsT)
        c_train<-unlist(classesT)
      }else{
        S_train<-unlist(predictionsT[pred])
        if (uniquecT==TRUE){
          c_train<-unlist(classesT)
        }else{
          c_train<-unlist(classesT[pred])}}

      ratest<-TP_FP.rates(S_train,c_train)
      TPt<-ratest[,2]
      FPt<-ratest[,1]
      test_O<- TestOptimal(TPt,FPt, S_train, loss2skew = loss2skew,
                           col = NULL, pch = NULL, lwd = NULL,
                           lty = NULL, cex = NULL, plotOFF = FALSE)
      breakpoints<-unlist(test_O[1])
      treshold<-unlist(test_O[2])
      AUC_TO<-TrainOptimal(S, c, breakpoints, treshold, loss2skew = loss2skew, pointsOFF = pointsOFF,
                              col = col_plot[3], pch = pch_plot[3], lwd = lwd_plot[3],
                              lty = lty_plot[3], cex = cex_plot[3])
      namesLegend<-c(namesLegend, paste(namesClassifiers[pred], " AUC_TO: ", AUC_TO, sep=""))
      COL<-c(COL, col_plot[3])
      result<-c(result, AUC_TO)
    }

    if (parameters[4]==TRUE){
      AUBC<- BrierCurves(TP,FP,S, loss2skew = loss2skew, pointsOFF = pointsOFF,
                         col = col_plot[4], pch = pch_plot[4], lwd = lwd_plot[4],
                         lty = lty_plot[4], cex = cex_plot[4])
      namesLegend<-c(namesLegend, paste(namesClassifiers[pred], " AUBC: ", AUBC, sep=""))
      COL<-c(COL, col_plot[4])
      result<-c(result, AUBC)
    }

    if (parameters[5]==TRUE){
      AURDC<- RateDrivenCurves(TP,FP, S, c, loss2skew = loss2skew, pointsOFF = pointsOFF,
                               col = col_plot[5], pch = pch_plot[5], lwd = lwd_plot[5],
                               lty = lty_plot[5], cex = cex_plot[5])
      namesLegend<-c(namesLegend, paste(namesClassifiers[pred], " AURDC: ", AURDC, sep=""))
      COL<-c(COL, col_plot[5])
      result<-c(result, AURDC)
    }

    if (parameters[6]==TRUE){
      AUKC<- KendallCurves(TP,FP, S,loss2skew = loss2skew, pointsOFF = pointsOFF,
                           col = col_plot[6], pch = pch_plot[6], lwd = lwd_plot[6],
                           lty = lty_plot[6], cex = cex_plot[6])
      namesLegend<-c(namesLegend, paste(namesClassifiers[pred], " AUKC: ", AUKC, sep=""))
      COL<-c(COL, col_plot[6])
      result<-c(result, AUKC)
    }
    if (!((Ni==1)&&(index==1))){
      names(result)<-AUC_names[which(parameters[-1]==TRUE)]
      Result<-append(Result, list(result))}else{
        Result<-NULL}}

  if(legendOFF == FALSE){
    legend(xPosLegend, yPosLegend, namesLegend, lty = lty, col = COL, cex = cexL,
           y.intersp = 0.7, x.intersp = 0.3, bty="n"); box()}
  return(Result)
}
