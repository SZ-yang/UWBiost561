test_that("returns empty for an empty clique", {
  res <- generate_partial_clique(n = 5, clique_fraction = 0, clique_edge_density = 0.5)
  result <- compute_maximal_partial_clique(res$adj_mat, alpha = 0.5)
  expect_equal(result$clique_idx, integer(0))
  expect_equal(result$edge_density, 0)
})

test_that("identifies a full clique", {
  res <- generate_partial_clique(n = 5, clique_fraction = 1, clique_edge_density = 1)
  result <- compute_maximal_partial_clique(res$adj_mat, alpha = 0.5)
  expect_equal(sort(result$clique_idx), 1:5)
  expect_equal(result$edge_density, 1)
})

test_that("finds clique at exact threshold", {
  res <- generate_partial_clique(n = 5, clique_fraction = 0.5, clique_edge_density = 1)
  result <- compute_maximal_partial_clique(res$adj_mat, alpha = 0.5)
  expect_true(length(result$clique_idx) >= 2)  # Assuming clique of at least 2 nodes
  expect_true(result$edge_density >= 0.5)
})

test_that("identifies the correct clique among multiple possibilities", {
  n <- 10
  adj_mat <- matrix(0, nrow = n, ncol = n)
  diag(adj_mat) <- 1  # Diagonal elements should be 1

  # First clique: indices 1 to 5, intended lower density (won't meet alpha = 0.7)
  for (i in 1:5) {
    for (j in i:5) {
      if (i != j && runif(1) < 0.4) {  # Ensure i != j to skip diagonal, set lower density
        adj_mat[i, j] <- 1
        adj_mat[j, i] <- 1
      }
    }
  }

  # Second clique: indices 6 to 10, higher density (meets alpha = 0.7)
  for (i in 6:10) {
    for (j in i:10) {
      if (i != j && runif(1) < 0.8) {  # Ensure i != j to skip diagonal, set higher density
        adj_mat[i, j] <- 1
        adj_mat[j, i] <- 1
      }
    }
  }

  result <- compute_maximal_partial_clique(adj_mat, alpha = 0.7)
  # Expect that the second clique is identified
  expect_true(all(result$clique_idx %in% 6:10))
  expect_true(result$edge_density >= 0.7)
})



test_that("performance on larger graphs", {
  res <- generate_partial_clique(n = 20, clique_fraction = 0.5, clique_edge_density = 0.5)
  system.time({
    result <- compute_maximal_partial_clique(res$adj_mat, alpha = 0.5)
  }) -> time_taken
  expect_true(time_taken["elapsed"] < 10)  # Test should complete in less than 10 seconds
})

library(testthat)

test_that("compute_maximal_partial_clique recovers known partial clique", {
  set.seed(123)  # For reproducibility
  n <- 10  # Total number of nodes
  clique_fraction <- 0.5  # Fraction of nodes in the partial clique
  clique_edge_density <- 0.8  # Density of edges within the clique

  # Generate a graph with a partial clique
  res <- generate_partial_clique(n = n, clique_fraction = clique_fraction, clique_edge_density = clique_edge_density)
  adj_mat <- res$adj_mat

  # Compute maximal partial clique from the generated adjacency matrix
  result <- compute_maximal_partial_clique(adj_mat, alpha = 0.75)

  # Calculate the expected number of nodes in the clique
  expected_clique_size <- round(n * clique_fraction)

  # Check if the recovered clique has the expected size
  expect_equal(length(result$clique_idx), expected_clique_size)

  # Check if the density of the recovered clique is at least the threshold
  expect_true(result$edge_density >= 0.75)
})

