\name{CostCurves}
\alias{CostCurves}
\title{CostCurves}
\description{Function to plot  Cost Curves with different option of treshold choice method}
\usage{
CostCurves(predictions, classes, cost_lines = TRUE,
                 test_optimal = TRUE, train_optimal = FALSE,
                 predictionsT = NULL, classesT = NULL,
                 uniquec=FALSE, uniqueTrain=FALSE, uniquecT=FALSE,
                 score_driven = FALSE, rate_driven = FALSE,
                 kendall_curves = FALSE, loss2skew = FALSE,
                 hold = FALSE, gridOFF = TRUE, pointsOFF = TRUE,
                 legendOFF = FALSE,
                 main, xlab, ylab, namesClassifiers, col, lwd,
                 lty, pch, cex,xPosLegend,yPosLegend, cexL)
}
\arguments{
	\item{predictions}{A list of predictions or scores arrays.}
	\item{classes}{A list of classes arrays, each array contains binary classes.}
	\item{cost_lines}{Lines cost are displayed if it is TRUE.}
	\item{test_optimal}{Option to plot cost curves using test optimal treshold
	choice method.}
	\item{train_optimal}{Option to plot cost curves using train optimal
	treshold choice method. }
	\item{predictionsT}{A list of classes arrays, each array contains predictions
	or scores of training classifier. It's necessary only if option train optimal is TRUE.}
	\item{classesT}{A list of classes arrays, each array contains binary train classes.
	It's necessary only if option train_optimal is TRUE.}
	\item{uniquec}{If it is TRUE, the same array classes is used for each array in a list predictions.}
	\item{uniquecT}{If it is TRUE, the same array classes is used for each array in a list
	train predictions. It's ecessary only if the option train_optimal is TRUE.}
	\item{uniqueTrain}{If it is TRUE, the same array of predictionsT and classesT is used
	for each array in a list predictions. It's necessary only if the option train_optimal is TRUE.}
	\item{score_driven}{If it is TRUE, plot cost curves using score driven treshold choice method.}
	\item{rate_driven}{If it is TRUE, plot cost curves using rate driven treshold choice method.}
	\item{kendall_curves}{If it is TRUE, plot cost curves using kendall driven treshold choice method.}
	\item{loss2skew}{If it is TRUE, loss by Skew is plotted otherwise loss by cost is plotted.}
	\item{hold}{If it is TRUE, the view is maintained to plot a new curve above the current curve.}
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
	\item{cexL}{Size point. It is posible select any valid option to the graphics parameters of R.}
	}
\details{
Ploting Cost curves with different treshold choice method.


Cost Lines: \link{CostLines}


Test Optimal treshold choice method: \link{TestOptimal}


Train Optimal treshold choice method: \link{TrainOptimal}


Score Driven treshold choice method: \link{BrierCurves}


Rate Driven treshold choice method: \link{RateDrivenCurves}


Kendall Curves: \link{KendallCurves}
}
\value{
A list of arrays with AUCCs of different cost curves selected.
}
\author{Paulina Morillo: \email{paumoal@inf.upv.es}}
\seealso{\link{BrierCurves}, \link{CostLines}, \link{KendallCurves}, \link{predictions}, \link{RateDrivenCurves}, \link{RocCurves}, \link{TestOptimal}, \link{TP_FP.rates}, \link{TrainOptimal}}
\references{
Drummond, C., & Holte, R. C. (2006). Cost curves: An improved method for visualizing
classifier performance.


Fawcett, T. (2006). An introduction to ROC analysis. Pattern recognition letters, 27(8), 861-874.


Ferri, C., Hernandez-orallo, J., & Flach, P. A. (2011). Brier curves: a new cost-based
visualisation of classifier performance. In Proceedings of the 28th International Conference
on Machine Learning (ICML-11) (pp. 585-592).


Hernandez-Orallo, J., Flach, P., & Ferri, C. (2013). 
ROC curves in cost space. Machine learning, 93(1), 71-91.}
\examples{
#Load data
data(predictions)

#Loss by Skew
R<-CostCurves(list(predictions$A, predictions$B), 
	list(predictions$classes), uniquec = TRUE, train_optimal = TRUE,
	predictionsT =  list(predictions$B, predictions$A),
	classesT = list(predictions$clases, predictions$classes),
	loss2skew = TRUE, test_optimal = FALSE,
	rate_driven = FALSE, col=list(c("cyan", "red"), c("gray", "blue")),
	pointsOFF = FALSE, cex=1)

R<-CostCurves(list(predictions$B), list(predictions$classes), 
	rate_driven = TRUE, kendall_curves = TRUE, col=c("gray", "red","green"))

#Loss by Cost
R<-CostCurves(list(predictions$A, predictions$B), list(predictions$classes), 
	uniquec = TRUE,	train_optimal = TRUE,
	predictionsT =  list(predictions$B, predictions$A),
	classesT = list(predictions$classes), uniquecT = TRUE)

R<-CostCurves(list(predictions$A, predictions$B), list(predictions$classes),
	uniquec = TRUE, train_optimal = TRUE, predictionsT =  list(predictions$B),
	classesT = list(predictions$classes), uniqueTrain = TRUE,
	kendall_curves = TRUE)

}
