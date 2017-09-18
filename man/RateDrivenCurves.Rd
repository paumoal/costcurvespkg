\name{RateDrivenCurves}
\alias{RateDrivenCurves}
\title{Plotting of lower envelope of Cost Curves}
\description{Function to plot  Cost Curves using Rate Driven Treshold Choice}
\usage{
RateDrivenCurves(predictions, classes, uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
                 plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
                 main, xlab, ylab, namesClassifiers, col, lwd, lty, pch, cex,
                 xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list of predictions arrays, each array contains scores or predictions of a specific classifier.}
	\item{classes}{A list of classes arrays, each array contains binary classes.}
	\item{uniquec}{If it is TRUE, the same array classes is used for each array in a list predictions.}
	\item{loss2skew}{If it is TRUE, loss by Skew is plotted otherwise loss by cost is plotted.}
	\item{hold}{If it is TRUE, the view is maintained to plot a new curve above the current curve.}
	\item{plotOFF}{Disable/enable plot visualization, only return AUC values.}
	\item{gridOFF}{Disable/enable grid visualization.}
	\item{pointsOFF}{Disable/enable point marks visualization.}
	\item{legendOFF}{Disable/enable legend visualization.}
	\item{main}{title.}
	\item{xlab}{x label.}
	\item{ylab}{y label.}
	\item{namesClassifiers}{An array with names of each classifier}
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
The rate-driven treshold choice method is a natural way of choosing the tresholds, especially when we only have a ranking or a poorly calibrated probabilistic caddifier.


The rate driven cost curves by cost is defined as a plot of:

\deqn{2(c*pi0(1-F0((R^-1(c))))+(1-c)pi1*F1((R^-1(c))))}

on the \emph{axis y} against cost \emph{c}


The rate driven cost curves by skew is defined as a plot of:

\deqn{z(1-F0(R^-1(c)))+(1-z)F1(R^-1(c)))} 

on the \emph{axis y} against skew \emph{z}


and

\deqn{R(t)=pi0*F0(t)+pi1*F1(t), by   c} 
\deqn{R(t)=(F0(t)+F1(t))/2, by   z } 


Where:


\tabular{rlll}{
\tab c:   \tab cost values of x_axis between [0, 1]. \cr
\tab z:   \tab skew values of x_axis between [0, 1]. \cr
\tab t:   \tab treshold t=R^-1(c) or  t=R^-1(c) as appropriate, and c=R(t) \cr
\tab pi0: \tab negative class proportion (Y==0)/length(Y). \cr
\tab pi1: \tab positive class proportion (Y==1)/length(Y). \cr
\tab F1(t):   \tab false positive rate of specific treshold. \cr
\tab 1-F0(t): \tab true positive rate of specific treshold. \cr
\tab R(c):   \tab recall that the predicted positive rate. \cr}

}
\value{
An array with AUKC (Area Under Kendall Curve) for each test.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves}, \link{CostCurves}, \link{CostLines}, \link{KendallCurves}, \link{predictions}, \link{RocCurves}, \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
\references{
Hernandez-Orallo, J., Flach, P., & Ferri, C. (2013). ROC curves in cost space. Machine learning, 93(1), 71-91.
}
\examples{
#Load data
data(predictions)

#Loss by cost
R<-RateDrivenCurves(list(predictions$A, predictions$B),list(predictions$classes), uniquec=TRUE)

#Loss by skew
R<-RateDrivenCurves(list(predictions$A, predictions$B), list(predictions$classes), uniquec=TRUE,
	loss2skew = TRUE)
}
