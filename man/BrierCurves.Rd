\name{BrierCurves}
\alias{BrierCurves}
\title{Plotting Brier Curves}
\description{Function to plot loss against operating condition using the probabilistic choice method (Brier Curves)}
\usage{
BrierCurves(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
            plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
            main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
            xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list of predictions arrays, each array contains scores or predictions of a specific classifier.}
	\item{classes}{A list of classes arrays, each array contains binary classes.}
	\item{uniquec}{If it is TRUE, the same array classes is used for each array in a list predictions.}
	\item{loss2skew}{If it is TRUE, loss by Skew is plotted otherwise loss by cost.}
	\item{hold}{If it is TRUE, the view is maintained to plot a new curve above the current curve.}
	\item{plotOFF}{Disable/enable plot visualization, only return AUC values.}
	\item{gridOFF}{Disable/enable grid visualization.}
	\item{pointsOFF}{Disable/enable point marks visualization.}
	\item{legendOFF}{Disable/enable legend visualization.}
	\item{main}{title.}
	\item{xlab}{x label.}
	\item{ylab}{y label.}
	\item{namesClassifiers}{An array with names of each classifier.}
	\item{lwd}{Line width. } 
	\item{lty}{Line type. }
	\item{col}{Line color. }
	\item{pch}{Point type. }
	\item{cex}{Size point. }
	\item{xPosLegend}{x coordinate to be used to position the legend.}
	\item{yPosLegend}{y coordinate to be used to position the legend.}
	\item{cexL}{size of box legend.}
	}
\details{
\strong{Definition:}


Function that plots the expected cost/skew against loss. For a given probabilistic classifier and operating condition defined by cost proportion, the probabilistic treshold choice method sets the treshold as fallows:


\deqn{T(c)=c}


If the operating condition is defined by skew, the treshold is set as fallows:


\deqn{T(z)=z-T(c)z/c}


The Brier curve for a given classifier is defined as a plot of loss against operating condition using the probabilistic treshold choice method. 


In particular, if the operating conditin is determined by cost proportion the Brier curve is defined by:


\deqn{BC(c)=2(c*pi0(1-F0(t))+(1-c)pi1*F1(t))}


A Brier curve for skew is defined by


\deqn{BC(z)=z(1-F0(t))+(1-z)F1(t))} 


Where:
\tabular{rlll}{
\tab c:   \tab cost values of x_axis between [0, 1]. \cr
\tab z:   \tab skew values of x_axis between [0, 1]. \cr
\tab t:   \tab treshold t=T(c) or t=T(z) as appropriate. \cr
\tab pi0: \tab negative class proportion (Y==0)/length(Y). \cr
\tab pi1: \tab positive class proportion (Y==1)/length(Y). \cr
\tab F1(t):   \tab false positive rate of specific treshold. \cr
\tab 1-F0(t): \tab true positive rate of specific treshold. \cr}
}


\value{
An array with AUBC (Area Under Brier Curve) for each test.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{CostCurves}, \link{CostLines}, \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves}, \link{RocCurves}, \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
\references{
Ferri, C., Hernandez-orallo, J., & Flach, P. A. (2011). Brier curves: a new cost-based visualisation of classifier performance. In Proceedings of the 28th International Conference on Machine Learning (ICML-11) (pp. 585-592).
}
\examples{
#Load data
data(predictions)

#Loss by cost
R<-BrierCurves(list(predictions$A, predictions$B), list(predictions$classes),
	uniquec = TRUE, loss2skew = FALSE)


#Loss by skew
R<-BrierCurves(list(predictions$A, predictions$B), list((1-predictions$classes),
	predictions$classes), loss2skew = TRUE, gridOFF = FALSE, main=NULL)
}
