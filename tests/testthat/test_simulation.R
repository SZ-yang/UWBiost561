library(testthat)
library(UWBiost561)

test_that("Demo simulation script runs without error", {
  set.seed(10)

  imp_numbers <- 1:25
  trials <- 5
  alpha_vec <- c(0.5, 0.95)

  # Run the main script code
  level_trial_list <- lapply(alpha_vec, function(alpha) {
    trial_list <- lapply(1:trials, function(trial) {
      set.seed(trial)
      data <- UWBiost561::generate_partial_clique(n = 10)
      adj_mat <- data$adj_mat

      result_list <- lapply(imp_numbers, function(imp_number) {
        set.seed(trial)
        result <- UWBiost561::compute_maximal_partial_clique_master(
          adj_mat = adj_mat,
          alpha = alpha,
          number = imp_number,
          time_limit = 30
        )
        return(result)
      })
      names(result_list) <- paste("Implementation:", imp_numbers)
      return(result_list)
    })
    names(trial_list) <- paste("Trial:", 1:trials)
    return(trial_list)
  })
  names(level_trial_list) <- paste0("alpha:", alpha_vec)

  # Save the date and session info
  date_of_run <- Sys.time()
  session_info <- devtools::session_info()

  # Save results
  save(level_trial_list, alpha_vec, date_of_run, session_info, file = "~/demo_simulation.RData")

  # Tests to ensure the structures are correct
  expect_equal(length(level_trial_list), length(alpha_vec))
  expect_equal(names(level_trial_list), paste0("alpha:", alpha_vec))

  for (alpha in alpha_vec) {
    trial_list <- level_trial_list[[paste0("alpha:", alpha)]]
    expect_equal(length(trial_list), trials)
    expect_equal(names(trial_list), paste("Trial:", 1:trials))

    for (trial in 1:trials) {
      result_list <- trial_list[[paste("Trial:", trial)]]
      expect_equal(length(result_list), length(imp_numbers))
      expect_equal(names(result_list), paste("Implementation:", imp_numbers))
    }
  }
})
