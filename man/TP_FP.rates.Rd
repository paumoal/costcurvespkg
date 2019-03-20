\name{TP_FP.rates}
\alias{TP_FP.rates}
\title{Calculates TP and FP rates}
\description{Calculates TP and FP rates, given a list of scores and a  list of classes}
\usage{
TP_FP.rates(predictions, classes)
}
\arguments{
	\item{predictions}{An array where each element contains a predicted score of each instance}
	\item{classes}{A vector with class for each instance}
	}
\details{
We calculate a \emph{TPR} (True positive rate)  and \emph{FPR}  (False positive rate) rates for each threshold between each pair of scores or predictions values.


We consider "1" for positives class (P), and "0" for negatives class (N)

\tabular{rlll}{
	\tab TP:   \tab Number of true positives.\cr
	\tab FP:   \tab Number of false positives. \cr
	\tab TPR:  \tab Estimate as: TP/P. \cr
	\tab FPR:  \tab Estimate as: FP/N. \cr
}
}
\value{
An array with two columns, first one corresponding to TPR and second one corresponding to FPR.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves}, \link{CostCurves}, \link{CostLines}, \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves},  \link{TestOptimal}, \link{TrainOptimal}}
\references{
Drummond, C., & Holte, R. C. (2006). Cost curves: An improved method for visualizing classifier performance.}
\examples{
predictions <- round(runif(10), 0)
classes <- round(runif(10), 1)
TP_FP.rates(predictions, classes)
}

