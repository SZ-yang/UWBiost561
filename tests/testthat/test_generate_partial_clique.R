test_that("adjacency matrix contains only 0s and 1s", {
  set.seed(42)
  res <- generate_partial_clique(n = 10, clique_fraction = 0.4, clique_edge_density = 0.8)
  expect_true(all(res$adj_mat %in% c(0, 1)))
})

test_that("clique edge density is approximately correct", {
  set.seed(42)
  n <- 10
  clique_fraction <- 0.4
  clique_edge_density <- 0.8
  res <- generate_partial_clique(n = n, clique_fraction = clique_fraction, clique_edge_density = clique_edge_density)
  m <- round(n * clique_fraction)
  expected_edges <- m * (m - 1) / 2 * clique_edge_density
  actual_edges <- sum(res$adj_mat[lower.tri(res$adj_mat)])
  expect_true(abs(expected_edges - actual_edges) < 1)
})
