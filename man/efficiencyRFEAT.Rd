% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiencyRFEAT.R
\name{efficiencyRFEAT}
\alias{efficiencyRFEAT}
\title{Efficiency Scores computed through a Random Forest + Efficiency Analysis Trees model.}
\usage{
efficiencyRFEAT(
  data,
  x,
  y,
  object,
  digits = 3,
  FDH = TRUE,
  print.table = FALSE,
  na.rm = TRUE
)
}
\arguments{
\item{data}{\code{data.frame} or \code{matrix} containing the variables in the model.}

\item{x}{Column input indexes in \code{data}.}

\item{y}{Column output indexes in \code{data}.}

\item{object}{A \code{RFEAT} object.}

\item{digits}{Decimal units for scores.}

\item{FDH}{\code{logical}. If \code{TRUE}, FDH scores are computed.}

\item{print.table}{\code{logical}. If \code{TRUE}, a summary descriptive table of the efficiency scores is displayed.}

\item{na.rm}{\code{logical}. If \code{TRUE}, \code{NA} rows are omitted.}
}
\value{
A \code{data.frame} with the efficiency scores computed through a Random Forest + Efficiency Analysis Trees model. Optionally, a summary descriptive table of the efficiency scores can be displayed.
}
\description{
This function computes the efficiency scores for each DMU through a Random Forest + Efficiency Analysis Trees model and the Banker Charnes and Cooper mathematical programming model with output orientation. Efficiency level at 1.
}
\examples{
\donttest{
simulated <- X2Y2.sim(N = 50, border = 0.2)
RFEAT_model <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4))

efficiencyRFEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = RFEAT_model, 
                digits = 2, FDH = TRUE, na.rm = TRUE)
}

}
