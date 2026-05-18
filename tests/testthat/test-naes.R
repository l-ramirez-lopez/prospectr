context("test-naes")
test_that("naes works", {
  data(NIRsoil)
  skip_on_cran()
  
  # --- method = 0: closest to cluster centre (default) ---
  set.seed(140920)
  X_naes <- naes(NIRsoil$spc, k = 30, iter.max = 100, pc = 50)
  
  # output structure
  expect_is(X_naes, "list")
  expect_true(all(c("model", "test", "pc", "cluster", "centers") %in% names(X_naes)))
  
  # pc scores matrix dimensions
  expect_equal(nrow(X_naes$pc), nrow(NIRsoil$spc))
  expect_equal(ncol(X_naes$pc), 50)
  
  # correct number of selections and no duplicates
  expect_equal(length(X_naes$model), 30)
  expect_equal(length(unique(X_naes$model)), 30)
  
  # valid row indices
  expect_true(all(X_naes$model >= 1))
  expect_true(all(X_naes$model <= nrow(NIRsoil$spc)))
  
  # model and test are complementary
  expect_equal(length(X_naes$test), nrow(NIRsoil$spc) - 30)
  expect_equal(sort(c(X_naes$model, X_naes$test)), seq_len(nrow(NIRsoil$spc)))
  
  # cluster assignments cover all samples and have correct number of clusters
  expect_equal(length(X_naes$cluster), nrow(NIRsoil$spc))
  expect_equal(length(unique(X_naes$cluster)), 30)
  
  # cluster centres have correct dimensions
  expect_equal(nrow(X_naes$centers), 30)
  expect_equal(ncol(X_naes$centers), 50)
  
  # cross-platform reproducible exact indices
  sel_samples <- c(
    398, 578, 277, 595, 692, 402, 68, 166, 501, 568, 213, 732, 273, 38,
    370, 763, 730, 359, 672, 383, 740, 138, 241, 434, 225, 606, 360, 179,
    310, 825
  )
  expect_true(!any(!sel_samples == X_naes$model))
  
  # --- method = 1: farthest from data centre ---
  set.seed(140920)
  X_naes_m1 <- naes(NIRsoil$spc, k = 10, iter.max = 100, pc = 50, method = 1)
  expect_equal(length(X_naes_m1$model), 10)
  expect_equal(length(unique(X_naes_m1$model)), 10)
  expect_true(all(X_naes_m1$model >= 1))
  expect_true(all(X_naes_m1$model <= nrow(NIRsoil$spc)))
  expect_equal(sort(c(X_naes_m1$model, X_naes_m1$test)), seq_len(nrow(NIRsoil$spc)))
  
  # --- method = 2: random selection within clusters ---
  set.seed(140920)
  X_naes_m2 <- naes(NIRsoil$spc, k = 10, iter.max = 100, pc = 50, method = 2)
  expect_equal(length(X_naes_m2$model), 10)
  expect_equal(length(unique(X_naes_m2$model)), 10)
  expect_true(all(X_naes_m2$model >= 1))
  expect_true(all(X_naes_m2$model <= nrow(NIRsoil$spc)))
  expect_equal(as.vector(sort(c(X_naes_m2$model, X_naes_m2$test))), seq_len(nrow(NIRsoil$spc)))
  
  sel_samples_m2 <- c(236, 578, 789, 167, 721, 286, 162, 577, 519, 765)
  expect_true(all(sel_samples_m2 %in% X_naes_m2$model))
  
  # --- pc < 1: variance-based component selection ---
  set.seed(140920)
  X_naes_var <- naes(NIRsoil$spc, k = 10, iter.max = 100, pc = 0.99)
  expect_equal(length(X_naes_var$model), 10)
  expect_true("pc" %in% names(X_naes_var))
  sel_samples_var <- c(93, 616, 225, 609, 464, 10, 100, 43, 561, 728)
  expect_true(!any(!sel_samples_var == X_naes_var$model))
  
  # --- pre-defined centres as input to k ---
  set.seed(140920)
  X_naes_centers <- naes(NIRsoil$spc, k = X_naes$centers, iter.max = 100, pc = 50)
  expect_equal(length(X_naes_centers$model), 30)
  expect_equal(length(unique(X_naes_centers$model)), 30)
  sel_samples_centers <- c(
    398, 578, 277, 595, 692, 402, 68, 166, 501, 568, 213, 732, 273, 38,
    370, 763, 730, 359, 672, 383, 740, 138, 241, 434, 225, 606, 360, 179,
    310, 825
  )
  expect_true(!any(!sel_samples_centers == X_naes_centers$model))
  
  # --- data frame input is accepted ---
  set.seed(140920)
  X_naes_df <- naes(as.data.frame(NIRsoil$spc), k = 10, iter.max = 100, pc = 50)
  expect_equal(length(X_naes_df$model), 10)
  
  # --- no pc argument: k-means on raw variable space ---
  set.seed(140920)
  X_naes_nopc <- naes(NIRsoil$spc, k = 10, iter.max = 100)
  expect_is(X_naes_nopc, "list")
  expect_false("pc" %in% names(X_naes_nopc))
  expect_equal(length(X_naes_nopc$model), 10)
  expect_equal(sort(c(X_naes_nopc$model, X_naes_nopc$test)), seq_len(nrow(NIRsoil$spc)))
  sel_samples_nopc <- c(529, 715, 486, 342, 362, 473, 409, 632, 19, 769)
  expect_true(!any(!sel_samples_nopc == X_naes_nopc$model))
  
  # --- input validation ---
  expect_error(naes(NIRsoil$spc, k = 1))
  expect_error(naes(NIRsoil$spc, k = nrow(NIRsoil$spc)))
  expect_error(naes(NIRsoil$spc[, 1, drop = FALSE], k = 5))
  expect_error(naes(NIRsoil$spc))
  expect_error(naes(NIRsoil$spc, k = 10, method = 5))
})
