
source("backward_stepwise.R")

#testing to see if make_matrix_X returns a matrix
test_that("makes a mattrix",{
  expect_true(is.matrix(make_matrix_X(fuel2001)),TRUE)
})

#testing to see if the AIC calculated is accurate
test_that("Comparing AIC",{
  expect_equal(step_func(fuel2001$FuelC, make_matrix_X(subset(fuel2001, select=-FuelC)))$AIC, 
               AIC(lm(FuelC ~ ., data = fuel2001))+(2*8^2+2*8)/(51-8-1))
})

#testing to see if output of step_func is the same as the summary lm output
testa = lm(FuelC ~ Drivers + Income + Miles + MPC + Pop + Tax, data = fuel2001)
testb = subset(fuel2001,select = -FuelC)
testc = make_matrix_X(testb)
step_func(fuel2001$FuelC,testc)
summary(testa)

#testing to see if output of bws_selection is the same as the output of the step function
strg = "FuelC ~ Drivers + Income + Miles + MPC + Pop + Tax"
bws_selection(strg,fuel2001)
step(testa,direction = "backward",trace = 0)
