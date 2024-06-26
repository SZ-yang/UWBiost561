% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_maximal_partial_clique.R
\name{compute_maximal_partial_clique}
\alias{compute_maximal_partial_clique}
\title{Compute Maximal Partial Clique}
\usage{
compute_maximal_partial_clique(adj_mat, alpha = 0.5)
}
\arguments{
\item{adj_mat}{A symmetric adjacency matrix where each element is either 0 or 1.
This matrix represents the graph in which we are looking for the clique.
The matrix should be square, with the diagonal elements set to 1.}

\item{alpha}{A numeric value between 0 and 1 inclusive, specifying the minimum
edge density required for the partial clique. This is a threshold to decide
which subsets of nodes form a valid clique based on their internal connectivity.}
}
\value{
A list containing two elements: \code{clique_idx}, a vector of node indices
forming the maximal partial clique with the required density, and \code{edge_density},
the actual edge density of the found clique. If no clique meets the criteria,
\code{clique_idx} will be an empty integer vector and \code{edge_density} will be 0.
}
\description{
This function attempts to find the maximal partial clique with an edge density
at least as great as the specified alpha value in the given adjacency matrix.
The function searches for all possible combinations of nodes, evaluating each
set for the specified edge density.
}
