#' @title Print tree
#'
#' @description This function is used to simulate the data in a multioutput scenario.
#'
#' @param tree List structure with tree nodes.
#'
#' @export
#'
#' @return Data frame with the values of a scenario with nX inputs and nY outputs.
show_tree <- function(tree) {
  for (t in 1:length(tree)) {
    cat(
      "[", t, "]", "\n", "[id: ", tree[[t]][["id"]],
      "  idF: ", tree[[t]][["F"]],
      "  xi: ", round(tree[[t]][["xi"]], 5),
      "  s: ", round(tree[[t]][["s"]], 5),
      "  R: ", round(tree[[t]][["R"]], 5),
      "  index:", length(tree[[t]][["index"]]),
      "  y: ["
    )

    for (j in 1:length(tree[[t]][["y"]])) {
      cat(round(tree[[t]][["y"]][[j]], 5), " ")
    }

    cat("] ]", rep("\n", 2),
      sep = ""
    )
  }
}
