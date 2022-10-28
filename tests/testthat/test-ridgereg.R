test_that("Coefficent values matches", {
  ridgeobj <- ridgereg$new(formula = Petal.Length~Species, 
                           data = iris, lamda = 4)
  lm_test <- MASS::lm.ridge(formula = Petal.Length~Species, 
                            data = iris, lambda = 4)
  expect_equal(round(ridgeobj$B_ridge[2],1), round(lm_test$coef[1],1))
  expect_equal(round(ridgeobj$B_ridge[3],1), round(lm_test$coef[2],1))
})
test_that("Intercept values matches", {
  ridgeobj <- ridgereg$new(formula = Petal.Length~Species, 
                           data = iris, lamda = 4)
  lm_test <- MASS::lm.ridge(formula = Petal.Length~Species, 
                            data = iris, lambda = 4)
  expect_equal(as.integer(ridgeobj$B_ridge[1]), as.integer(lm_test$ym))
})