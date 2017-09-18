\name{predictions}
\alias{predictions}
\title{class and predictions data}
\description{
	This data frame contains the predictions given by 4 
	classifiers (A, B , C , D ) to 10 instances . 
	The first column contains the classes.
}
\usage{
predictions
}
\format{A data.frame containing 10 observations of 5 variables.}
\keyword{datasets}
\docType{data}
\source{
This predictions values had been obtained from examples of:
Ferri, C., Hernandez-orallo, J., & Flach, P. A. (2011). 
Brier curves: a new cost-based visualisation of classifier performance. 
In Proceedings of the 28th International Conference on Machine Learning (ICML-11)
(pp. 585-592).
}
\seealso{\link{BrierCurves}, \link{CostCurves}, \link{CostLines}, \link{KendallCurves}, \link{RateDrivenCurves}, \link{RocCurves}, \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
\examples{
#load the dataset
data(predictions)
#table of classes
table(predictions$classes)
#order of predictions
order(predictions$A)
}

