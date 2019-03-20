\name{CostLines}
\alias{CostLines}
\title{Plotting Cost Curves based on Cost or Skew}
\description{Function to plot  cost lines based on \emph{cost} or \emph{skew}.
	  }
\usage{
CostLines(predictions,classes,uniquec=FALSE, loss2skew=FALSE, hold=FALSE,
          gridOFF=TRUE, legendOFF=FALSE, main, xlab, ylab, namesClassifiers,
           lwd, lty, col, xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list with  predicted scores.}
	\item{classes}{A list with labels, (only binary classes).}
	\item{uniquec}{If TRUE, the same array classes is used for each array in a list predictions.}
	\item{loss2skew}{If  TRUE, loss by Skew is plotted otherwise loss by cost is plotted.}
	\item{hold}{If it is TRUE, the plot is maintained open allowing to insert new curves on the same plot.}
	\item{gridOFF}{Disable/enable grid visualization.}
	\item{legendOFF}{Disable/enable legend visualization.}
	\item{main}{title.}
	\item{xlab}{x label.}
	\item{ylab}{y label.}
	\item{namesClassifiers}{An array with names of each classifier.}
	\item{lwd}{Line width. } 
	\item{lty}{Line type.}
	\item{col}{Line color.}
	\item{xPosLegend}{x coordinate to be used to position the legend.}
	\item{yPosLegend}{y coordinate to be used to position the legend.}
	\item{cexL}{size of box legend.}
	}
\details{
The loss which is produced at a decision threshold \emph{t} and a cost proportion \emph{c} is given by formula:


\strong{Loss by Cost:}


\deqn{2(c*pi0(1-F0(t))+(1-c)pi1*F1(t))}


The loss which is produced at a decision threshold \emph{t} and a skew \emph{z} is given by formula:


\strong{Loss by Skew:}


\deqn{z(1-F0(t))+(1-z)F1(t))} 


Where:
\tabular{rlll}{
\tab c:   \tab cost values of x_axis between [0, 1]. \cr
\tab z:   \tab skew values of x_axis between [0, 1]. \cr
\tab t:   \tab threshold. \cr
\tab pi0: \tab negative class proportion (Y==0)/length(Y). \cr
\tab pi1: \tab positive class proportion (Y==1)/length(Y). \cr
\tab F1(t):   \tab false positive rate of specific threshold. \cr
\tab 1-F0(t): \tab true positive rate of specific threshold. \cr}
}
\value{
No return value.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves}, \link{CostCurves}, \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves}, \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
\references{Hernandez-Orallo, J., Flach, P., & Ferri, C. (2013). ROC curves in cost space. Machine learning, 93(1), 71-91.}
\examples{
data(predictions)
#Loss by cost
CostLines(list(predictions$A, predictions$B), list(predictions$classes),
			uniquec=TRUE)

#Loss by skew
CostLines(list(predictions$A, predictions$B), list(predictions$classes),
			uniquec=TRUE, loss2skew = TRUE)

#names legend
CostLines(list(predictions$A, predictions$B), list(predictions$classes,
			predictions$classes), loss2skew = TRUE, col=c("blue", "red"), lty=c(1, 2),
            namesClassifiers = c("A","B"))

#LegendOFF
CostLines(list(predictions$A, predictions$B), list(predictions$classes),
			uniquec=TRUE, loss2skew = TRUE, legendOFF=TRUE, lty=5)
}
