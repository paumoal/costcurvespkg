\name{TestOptimal}
\alias{TestOptimal}
\title{Plotting lower envelope of Cost Curves}
\description{Function to plot  of lower envelope of Cost Curves}
\usage{
TestOptimal(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
            plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
            main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
            xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list with  predicted scores.}
	\item{classes}{A list with labels, (only binary classes).}
	\item{uniquec}{If it is TRUE, the same array classes is used for each array in a list predictions.}
	\item{loss2skew}{If it is TRUE, loss by Skew is plotted otherwise loss by cost is plotted.}
	\item{hold}{If it is TRUE, the plot is maintained open allowing to insert new curves on the same plot.}
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
The lower envelope at any given \emph{c} (cost) or \emph{z} (skew) value is defined as the lowest loss value on any of the given cost curves at that \emph{c} or \emph{z}. Each segment of lower envelope or test optimal correspond to points on a ROC covex hull.
}
\value{
A list of lists, for each classifier is returned: extreme points of each segment of lower envelope, thresholds and AUCC (area under curve cost).
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves},  \link{CostCurves}, \link{CostLines}, \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves},  \link{TP_FP.rates}, \link{TrainOptimal}}
\references{
Drummond, C., & Holte, R. C. (2006). Cost curves: An improved method for visualizing classifier performance.


Hernandez-Orallo, J., Flach, P., & Ferri, C. (2013). ROC curves in cost space. Machine learning, 93(1), 71-91.
}
\examples{
#Load data
data(predictions)

#Loss by cost
R<-TestOptimal(list(predictions$A, predictions$B), list(predictions$classes), uniquec=TRUE)

#Loss by skew
R<-TestOptimal(list(predictions$A, predictions$B), list(predictions$classes), uniquec=TRUE,
	loss2skew = TRUE)

#names legend
R<-TestOptimal(list(predictions$A, predictions$B), list(predictions$classes, predictions$classes),
	plotOFF=TRUE)

#LegendOFF
R<-TestOptimal(list(predictions$A, predictions$B), list(predictions$classes), uniquec=TRUE,
	loss2skew = TRUE, pointsOFF=FALSE, legendOFF=TRUE, gridOFF=FALSE)
}
