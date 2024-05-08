#' Generate a Random Graph with a Partial Clique
#'
#'This function creates a random graph containing a specified number of nodes and embeds a partial clique with defined edge density. The graph is represented as an adjacency matrix.
#'
#' @param n Integer, the number of nodes in the graph. Defaults to 10.
#' @param clique_fraction Numeric, the fraction of nodes that are part of the partial clique. Must be between 0 and 1. Defaults to 0.3.
#' @param clique_edge_density Numeric, the density of edges within the clique. Must be between 0 and 1. Defaults to 0.5.
#'
#' @return A list containing a single element `adj_mat`, which is an n x n adjacency matrix of the graph. The matrix includes a partial clique according to the specified edge density.
#'
#' @export
generate_partial_clique <- function(n = 10 , clique_fraction = 0.3, clique_edge_density = 0.5) {
  # Validate input parameters
  stopifnot(is.numeric(n) && n > 0 && n %% 1 == 0)  # n must be a positive integer
  stopifnot(is.numeric(clique_fraction) && clique_fraction >= 0 && clique_fraction <= 1)
  stopifnot(is.numeric(clique_edge_density) && clique_edge_density >= 0 && clique_edge_density <= 1)

  # Initialize adjacency matrix
  adj_mat <- matrix(0, nrow = n, ncol = n)
  diag(adj_mat) <- 1  # Set diagonal to 1

  # Calculate the number of nodes in the partial clique
  m <- round(n * clique_fraction)

  # Create a partial clique
  if (m > 1) {
    # Select m nodes randomly
    nodes <- sample(n, m)
    # Set edges for the partial clique based on clique_edge_density
    for (i in nodes) {
      for (j in nodes) {
        if (i != j && runif(1) < clique_edge_density) {
          adj_mat[i, j] <- 1
          adj_mat[j, i] <- 1  # Ensure the matrix is symmetric
        }
      }
    }
  }

  # Return the adjacency matrix as a list
  return(list(adj_mat = adj_mat))
}
