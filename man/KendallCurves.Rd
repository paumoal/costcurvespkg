\name{KendallCurves}
\alias{KendallCurves}
\title{Plotting Kendall Curves }
\description{Function to plot  the expected loss of the model, using the rate driven threshold choice method 
	     discounting the loss of a perfect ranker}
\usage{
KendallCurves(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
               plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
               main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
               xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list with  predicted scores.}
	\item{classes}{A list with labels, (only binary classes).}
	\item{uniquec}{If it is TRUE, the same array classes is used for each array in a list predictions.}
	\item{loss2skew}{If it is TRUE, loss by Skew is plotted otherwise loss by cost is plotted.}
	\item{hold}{If it is TRUE, the plot is maintained open allowing to insert new curves on the same plot..}
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
The Kendall curve is defined as fallows:


\deqn{Q(c)=2pi1*F1(R^-1(c)),  if c<=pi0} 
\deqn{Q(c)=2pi0(1-F0(R^-1(c))),  if c>=pi0} 

and


\deqn{R(t)=pi0*F0(t)+pi1*F1(t), by   c} 
\deqn{R(t)=(F0(t)+F1(t))/2, by   z } 


Where:


\tabular{rlll}{
\tab c:   \tab cost values of x_axis between [0, 1]. \cr
\tab z:   \tab skew values of x_axis between [0, 1]. \cr
\tab t:   \tab threshold t=R^-1(c) or  t=R^-1(c) as appropriate, and c=R(t) \cr
\tab pi0: \tab negative class proportion (Y==0)/length(Y). \cr
\tab pi1: \tab positive class proportion (Y==1)/length(Y). \cr
\tab F1(t):   \tab false positive rate of specific threshold. \cr
\tab 1-F0(t): \tab true positive rate of specific threshold. \cr
\tab R(c):   \tab recall that the predicted positive rate. \cr}


It shows for each cost proportion \emph{c}, the expected loss of the model, once the loss of a perfect ranker is discounted.

}
\value{
An array with AUKC (Area Under Kendall Curve) for each test.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves}, \link{CostCurves},  \link{CostLines},\link{predictions}, \link{RateDrivenCurves},  \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
\references{
Hernandez-Orallo, J., Flach, P., & Ferri, C. (2013). ROC curves in cost space. Machine learning, 93(1), 71-91.
}
\examples{
#Load data
data(predictions)

#Loss by cost
R<-KendallCurves(list(predictions$A, predictions$B), list(predictions$classes), uniquec=TRUE, 
	main="Kendall Curves")

#Loss by skew
R<-KendallCurves(list(predictions$A, predictions$B), list(predictions$classes), uniquec=TRUE,
	loss2skew = TRUE, pointsOFF=FALSE)
}
