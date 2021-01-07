% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiency.R
\name{efficiencyEAT}
\alias{efficiencyEAT}
\title{EAT Efficiency Scores}
\usage{
efficiencyEAT(
  data,
  x,
  y,
  object,
  scores_model,
  r = 4,
  FDH = TRUE,
  na.rm = TRUE
)
}
\arguments{
\item{data}{Dataframe for which the efficiency score is calculated.}

\item{x}{Vector. Column input indexes in data.}

\item{y}{Vector. Column output indexes in data.}

\item{object}{An EAT object.}

\item{scores_model}{Mathematic programming model to calculate scores. 
\itemize{
\item{\code{EAT_BCC_out}} BBC model. Output orientation.
\item{\code{EAT_BCC_in}}  BBC model. Input orientation.
\item{\code{EAT_DDF}}     Directional distance model.
\item{\code{EAT_RSL_out}} Rusell model. Output orientation
\item{\code{EAT_RSL_in}}  Rusell model. Input orientation.
\item{\code{EAT_WAM}}     Weighted Additive model.
}}

\item{r}{Integer. Decimal units for scores.}

\item{na.rm}{Logical. If \code{TRUE}, \code{NA} rows are omitted.}

\item{FDH.}{Logical. If \code{TRUE}, FDH scores are calculated with the programming model selected in \code{scores_model}}
}
\value{
Dataframe with input variables and efficiency scores by an EAT model.
}
\description{
This function calculates the efficiency scores for each DMU by an EAT model.
}
\examples{

X1 <- runif(50, 1, 10)
X2 <- runif(50, 2, 10)
Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
Y2 <- log(X1) + 2 - abs(rnorm(50, mean = 0, sd = 0.7))

simulated <- data.frame(x1 = X1, x2 = X2, y1 = Y1, y2 = Y2)

n <- nrow(simulated)
t_index <- sample(1:n, n * 0.8)
training <- simulated[t_index, ]
test <- simulated[-t_index, ]

EAT_model <- EAT(data = training, x = c(1,2), y = c(3, 4), numStop = 3, fold = 7)

efficiencyEAT(data = training, x = c(1, 2), y = c(3, 4), object = EAT_model, 
              scores_model = "EAT_BCC_out", r = 2, na.rm = TRUE)

efficiencyEAT(data = test, x = c(1, 2), y = c(3, 4), object = EAT_model, 
              scores_model = "EAT_BCC_out", r = 2, na.rm = TRUE)

}