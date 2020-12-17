#' @title EAT: Efficiency Analysis Trees
#' 
#' @name eat-package
#'
#' @description This package provides functions to determine production frontiers and technical efficiency measures through non-parametric techniques based upon regression trees. The package includes code for estimating radial input, output, directional and additive measures, plotting graphical representations of the scores and the production frontiers by means of trees, and determining rankings of importance of input variables in the analysis. Additionally, an adaptation of Random Forest by a set of individual Efficiency Analysis Trees for estimating technical efficiency is also included.
#' 
#' @author 
#' \strong{Mantainer}: \itemize{
#' \item{Víctor Javier España Roch \email{email@@rstudio.com}}
#' \item{Míriam Esteve Campello}
#' } \cr
#' Authors:
#' \itemize{
#' \item{Míriam Esteve Campello}
#' \item{Víctor Javier España Roch}
#' \item{Juan Aparicio Baeza}
#' \item{Jesús Javier Rodríguez Sala}
#' \item{Xavier Barber Vallés}
#' \item{Alejandro Rabasa Dolado} 
#' }
#' 
#' @importFrom utils globalVariables
#' @importFrom Rdpack reprompt
#' 
#' @keywords internal  
#' 
#' @seealso 
#' Repositorio en Github: \url{https://github.com/MiriamEsteve/EAT} \cr
#' \cr
#' \insertRef{esteve2020}{eat}
#' 
"_PACKAGE"

utils::globalVariables(c("x1", "x_values",
                         "index",
                         "SL", "a", "R", "N", "id", "Prop", "MSE", "index",
                         "splitvar", "Id", "R", "nodesize", "Y", "n1", "n2",
                         "V1",
                         "id", "index", "Group", "Score",
                         "xi", "desc", "Importance", "id",
                         "Importance",
                         "id", "X1", "X2", "X3", "X4", "X5", "X6", "X7",
                         "RF", 
                         "Model", "score")) 

# frontier - print_results - EAT_object - EAT_plot - efficiency_scores - efficiency_jitter - M_Breiman - barplot_importance - imp_var_Breiman - efficiency_RFEAT - efficiency_density

