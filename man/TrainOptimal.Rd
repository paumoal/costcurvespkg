\name{TrainOptimal}
\alias{TrainOptimal}
\title{Plotting Cost Curves using the Optimal Training threshold choice}
\description{Function to plot  the optimal training curve}
\usage{
TrainOptimal(predictions_train, classes_train, predictions_test, classes_test,
                uniquecT=FALSE, uniquect=FALSE, refuseT=FALSE, loss2skew=FALSE,
                hold=FALSE, plotOFF=FALSE, pointsOFF=TRUE, gridOFF=TRUE,legendOFF=FALSE,
                main, xlab, ylab, namesClassifiers,namesTests, lwd, lty, col,pch, 
                cex, xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions_train}{A list with  predicted scores. Train Dataset}
	\item{classes_train}{A list with labels, (only binary classes). Train Dataset}
	\item{predictions_test}{A list with  predicted scores. Train Dataset}
	\item{classes_test}{A list with labels, (only binary classes). Test Dataset}
	\item{uniquecT}{If it is TRUE, the same array classes is used for each array in a list predictions Training.}
	\item{uniquect}{If it is TRUE, the same array classes is used for each array in a list predictions test.}
	\item{refuseT}{It is possible to use the same training classifier for every test.}
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
	\item{namesTests}{An array with names of each classifier.}
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
This function plot the cost curves  on a test considering the optimal threshold on a train dataset.
}
\value{
An array with AUCC (Area Under Cost Curve) for each test.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves}, \link{CostCurves}, \link{CostLines},  \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves}, \link{TestOptimal}, \link{TP_FP.rates}}
\references{
#It reference is not exact.
Hernandez-Orallo, J., Flach, P., & Ferri, C. (2013). ROC curves in cost space. Machine learning, 93(1), 71-91.
}
\examples{
#Load data
data(predictions)

#Loss by Skew
R<-TrainOptimal(list(predictions$A), list(predictions$classes), 
	list(predictions$A, predictions$B), list(predictions$classes), 
	uniquect = TRUE, uniquecT = TRUE, loss2skew = TRUE, refuseT = TRUE)

#Loss by Cost
R<-TrainOptimal(list(predictions$A, predictions$B), list(predictions$classes),
	list(predictions$B, predictions$A), list(predictions$classes), uniquect = TRUE,
	namesClassifiers=c("A", "B"), namesTests=c("B","A"), uniquecT = TRUE)
	}


