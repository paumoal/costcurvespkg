\name{BrierCurves}
\alias{BrierCurves}
\title{Plotting Brier Curves}
\description{Function to plot loss against operating condition using the score-driven threshold choice method (Brier Curves)}
\usage{
BrierCurves(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
            plotOFF=FALSE, gridOFF=TRUE, pointsOFF=TRUE, legendOFF=FALSE,
            main, xlab, ylab, namesClassifiers, lwd, lty, col, pch, cex,
            xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list of predictions arrays, each array contains predicted scores of a specific classifier.}
	\item{classes}{A list of classes arrays, each array contains binary classes.}
	\item{uniquec}{If it is TRUE, the same array of classes is used for each array in a list predictions.}
	\item{loss2skew}{If it is TRUE, loss is computed with respect to Skew otherwise loss is used.}
	\item{hold}{If it is TRUE, the plot is not closed. This is useful to include  new curves above the current curve.}
	\item{plotOFF}{Disable/enable plot visualization, only return AUC values.}
	\item{gridOFF}{Disable/enable grid visualization.}
	\item{pointsOFF}{Disable/enable point marks visualization.}
	\item{legendOFF}{Disable/enable legend visualization.}
	\item{main}{Title of the plot.}
	\item{xlab}{x label.}
	\item{ylab}{y label.}
	\item{namesClassifiers}{An array with names of each classifier.}
	\item{lwd}{Line width. } 
	\item{lty}{Line type. }
	\item{col}{Line color. }
	\item{pch}{Point type. }
	\item{cex}{Size point. }
	\item{xPosLegend}{x coordinate to be used in the position of the legend.}
	\item{yPosLegend}{y coordinate to be used in the position of the legend.}
	\item{cexL}{size of box legend.}
	}
\details{
\strong{Definition:}


Function that plots the expected cost/skew against loss. For a given probabilistic classifier and operating condition defined by cost proportion,   the Score-Driven treshold choice method 
	sets the threshold equal to the operating condition (cost proportion or skew).


The Brier curve for a given classifier is defined as a plot of loss against operating condition using 
	 score-driven threshold choice method.


Assuming the score-driven threshold choice method, expected loss under a uniform distribution of cost proportions is equal to
the model’s Brier score. Using skews, we arrive at the prior-independent version of the Brier score.

}


\value{
An array with AUBC (Area Under Brier Curve) for each test.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{CostCurves}, \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves}, \link{CostLines}, \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
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
