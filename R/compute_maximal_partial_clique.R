#' Compute Maximal Partial Clique
#'
#' This function attempts to find the maximal partial clique with an edge density
#' at least as great as the specified alpha value in the given adjacency matrix.
#' The function searches for all possible combinations of nodes, evaluating each
#' set for the specified edge density.
#'
#' @param adj_mat A symmetric adjacency matrix where each element is either 0 or 1.
#'        This matrix represents the graph in which we are looking for the clique.
#'        The matrix should be square, with the diagonal elements set to 1.
#' @param alpha A numeric value between 0 and 1 inclusive, specifying the minimum
#'        edge density required for the partial clique. This is a threshold to decide
#'        which subsets of nodes form a valid clique based on their internal connectivity.
#'
#' @return A list containing two elements: `clique_idx`, a vector of node indices
#'         forming the maximal partial clique with the required density, and `edge_density`,
#'         the actual edge density of the found clique. If no clique meets the criteria,
#'         `clique_idx` will be an empty integer vector and `edge_density` will be 0.
#'
#' @export
library(parallel)

compute_maximal_partial_clique <- function(adj_mat, alpha = 0.5) {
  n <- nrow(adj_mat)

  # Check input conditions
  if (!is.matrix(adj_mat) || !all(adj_mat == t(adj_mat)) || !all(adj_mat %in% c(0, 1))) {
    stop("adj_mat must be a symmetric matrix with elements 0 or 1.")
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
    stop("alpha must be a single numeric value between 0 and 1.")
  }

  # Function to calculate edge density
  calculate_density <- function(nodes) {
    subgraph <- adj_mat[nodes, nodes]
    # Remove diagonal elements for the edge count
    actual_edges <- (sum(subgraph) - length(nodes)) / 2
    possible_edges <- length(nodes) * (length(nodes) - 1) / 2
    return(actual_edges / possible_edges)
  }

  # Determine number of cores, leaving one free
  numCores <- detectCores() - 1

  # Try to find the maximal clique with the density above alpha
  results <- NULL
  for (size in n:1) {
    combinations <- combn(n, size, simplify = FALSE)
    # Parallel processing for each size of combination
    temp_results <- mclapply(combinations, function(nodes) {
      current_density <- calculate_density(nodes)
      if (current_density >= alpha) {
        list(clique_idx = nodes, edge_density = current_density)
      } else {
        NULL
      }
    }, mc.cores = numCores)

    # Check if any valid cliques were found
    valid_results <- Filter(Negate(is.null), temp_results)
    if (length(valid_results) > 0) {
      results <- valid_results[[1]]
      break
    }
  }

  # Return result if found, else return empty
  if (is.null(results)) {
    return(list(clique_idx = integer(0), edge_density = 0))
  } else {
    return(results)
  }
}
