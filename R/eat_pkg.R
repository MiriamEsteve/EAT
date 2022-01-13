#' @title EAT: Efficiency Analysis Trees
#' 
#' @name eat-package
#'
#' @description This package provides functions to determine production frontiers and technical efficiency measures through non-parametric techniques based upon regression trees. The package includes code for estimating radial input, output, directional and additive measures, plotting graphical representations of the scores and the production frontiers by means of trees, and determining rankings of importance of input variables in the analysis. Additionally, an adaptation of Random Forest by a set of individual Efficiency Analysis Trees for estimating technical efficiency is also included.
#' 
#' @author 
#' \strong{Mantainers}: \itemize{
#' \item{Miriam Esteve Campello \email{miriam.estevec@@umh.es}}
#' \item{Víctor Javier España Roch}
#' } \cr
#' Authors:
#' \itemize{
#' \item{Miriam Esteve Campello}
#' \item{Víctor Javier España Roch}
#' \item{Juan Aparicio Baeza}
#' \item{Xavier Barber Vallés}
#' }
#' 
#' @importFrom utils globalVariables
#' @importFrom Rdpack reprompt
#' 
#' @keywords internal  
#' 
#' @seealso 
#' Github: \url{https://github.com/MiriamEsteve/EAT} \cr
#' \cr
#' \insertRef{esteve2020}{eat}
#' 
"_PACKAGE"
utils::globalVariables(c("SL", "a", "R", "N", "id", "Proportion", "index", # EAT_object
                         "xi", "desc", "Importance", # M_Breiman + id
                         "value", "Model", # efficiencyDensity
                         "Group", "Score", # efficiencyJitter + SL, id, index
                         "RF", # efficiencyRFEAT 
                         "x1", "Frontier", # frontier + Model
                         "X1", "X2", "X3", "X4", "X5", "X6", "X7", # imp_var_EAT + id
                         "splitvar", "Id", "n1", # plotEAT + R
                         "RMSE", # plotRFEAT
                         "is"
                         ))

# RFEAT_object : id
# baplot_importance: Importance
# imp_var_RFEAT: desc + Importance
# summary.EAT: SL + index

