testthat::context('Testing with all valid inputs')
testthat::test_that (
	'initialize nppCART, with all possible inputs, does not output an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), NA)
	}
)
testthat::test_that (
	'initialize nppCART, with all required inputs, does not output an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		weight <- "weight"

		testthat::expect_error(nppCART(np.data = np.data, p.data = p.data, weight = weight), NA)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the np.data parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL np.data, outputs an error',
	{
		np.data <- NULL
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(np.data) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input numeric np.data, outputs an error',
	{
		np.data <- 10
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels),
			regexp = 'base::is.data.frame(np.data) | base::is.matrix(np.data) | tibble::is_tibble(np.data) is not TRUE',
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input empty data frame np.data, outputs an error',
	{
		np.data <- base::data.frame()
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels),
			regexp = 'base::nrow(np.data) > 0 is not TRUE',
			fixed  = TRUE
			)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the p.data parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL p.data, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- NULL
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(p.data) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input numeric p.data, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- 10
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels),
			regexp = 'base::is.data.frame(p.data) | base::is.matrix(p.data) | tibble::is_tibble(p.data) is not TRUE',
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input empty data frame p.data, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame()
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels),
			regexp = 'base::nrow(p.data) > 0 is not TRUE',
			fixed  = TRUE
			)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the predictors parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL predictors, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- NULL
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(predictors) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input numeric predictors, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- 10
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.character(predictors) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input empty string predictors, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- ""
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::length(base::setdiff(predictors, base::colnames(np.data))) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input predictors not in colnames(np.data), outputs an error',
	{
		np.data <- base::data.frame(oof = 1:4, rab = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::c("foo", "bar")
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::length(base::setdiff(predictors, base::colnames(np.data))) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input predictors not in colnames(p.data), outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(oof = 1:4, rab = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::c("foo", "bar")
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::length(base::setdiff(predictors, base::colnames(p.data))) ==  .... is not TRUE", 
			fixed  = TRUE	)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the weight parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL weight, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- NULL
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(weight) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input numeric weight, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- 10
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.character(weight) & (base::length(weight) == 1) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input vector weight, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- base::c("weight1", "weight2")
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.character(weight) & (base::length(weight) == 1) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input weight not in colnames(p.data), outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "test"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::length(base::setdiff(weight, base::colnames(p.data))) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input vector of strings p.data$weight, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = "test")
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(p.data$weight) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input vector of zeros p.data$weight, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 0)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::all(p.data$weight > 0) is not TRUE", 
			fixed = TRUE
			)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the min.cell.size parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL min.cell.size, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- NULL
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(min.cell.size) is not TRUE", 
			fixed = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input string min.cell.size, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- "test"
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(min.cell.size) & (base::length(min.cell.size) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input vector min.cell.size, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- base::c(1, 2, 3)
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(min.cell.size) & (base::length(min.cell.size) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input decimal min.cell.size, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 3.14
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "(min.cell.size%%1 == 0) & (min.cell.size > 0) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input zero min.cell.size, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 0
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "(min.cell.size%%1 == 0) & (min.cell.size > 0) is not TRUE", 
			fixed  = TRUE)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the min.impurity parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL min.impurity, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- NULL
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(min.impurity) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input string min.impurity, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- "test"
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(min.impurity) & (base::length(min.impurity) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input vector min.impurity, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- base::c(1, 2, 3)
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(min.impurity) & (base::length(min.impurity) ==  .... is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input zero min.impurity, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "min.impurity > 0 is not TRUE", 
			fixed = TRUE
			)
	}
)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

testthat::context('Testing the max.levels parameter for errors')
testthat::test_that (
	'initialize nppCART, with input NULL max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- NULL

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "!base::is.null(max.levels) is not TRUE", 
			fixed = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input string max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- "test"

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(max.levels) & (base::length(max.levels) == 1) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input vector max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- base::c(1, 2, 3)

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::is.numeric(max.levels) & (base::length(max.levels) == 1) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input decimal max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 3.14

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "(max.levels%%1 == 0) & (max.levels >= 0) is not TRUE", 
			fixed  = TRUE
			)
	}
)
testthat::test_that (
	'initialize nppCART, with input negative max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		p.data <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- -1

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "(max.levels%%1 == 0) & (max.levels >= 0) is not TRUE", 
			fixed  = TRUE)
	}
)
testthat::test_that (
	'initialize nppCART, with input np.data containing a factor exceeding max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = base::factor(base::c("A","B","C","D","A","B","C","D","A","B","C","D","A","B","C")), bar = base::factor(base::c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")), weight = 1)
		p.data <- base::data.frame(foo = base::factor(base::c("A","B","C","D","A","B","C","D","A","B","C","D","A","B","C")), bar = base::factor(base::c("A","B","C","D","A","B","C","D","A","B","C","D","A","B","C")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::max(base::unlist(base::lapply(X = self$np.data[, self$predictors_factor],  .... is not TRUE", 
			fixed  = TRUE)
	}
)
testthat::test_that (
	'initialize nppCART, with input p.data containing a factor exceeding max.levels, outputs an error',
	{
		np.data <- base::data.frame(foo = base::factor(base::c("A","B","C","D","A","B","C","D","A","B","C","D","A","B","C")), bar = base::factor(base::c("A","B","C","D","A","B","C","D","A","B","C","D","A","B","C")), weight = 1)
		p.data <- base::data.frame(foo = base::factor(base::c("A","B","C","D","A","B","C","D","A","B","C","D","A","B","C")), bar = base::factor(base::c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")), weight = 1)
		predictors <- base::colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.levels <- 10

		testthat::expect_error(
			object = nppCART(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.levels), 
			regexp = "base::max(base::unlist(base::lapply(X = self$p.data[, self$predictors_factor],  .... is not TRUE", 
			fixed  = TRUE)
	}
)