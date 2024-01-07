# PUZZLE 13A

# Reading the input and formatting it as a list of matrices
x <- readLines("puzzle13.txt")
x <- paste0(x, collapse = "\n")
x <- strsplit(x, "\n\n")[[1]]
x <- strsplit(x, "\n")
x <- lapply(x, function(x) do.call("rbind", strsplit(x, "")))

# A function that finds a vertical axis of symmetry in a matrix
find_symmetry <- function(x) {
	# Looping over the columns
	for(i in 1:(ncol(x) - 1)) {
		if(is_symmetric(x[, 1:i, drop = FALSE], x[, (i + 1):ncol(x), drop = FALSE])) return(i)
	}

	return(0)
}

# A function that tests the symmetry of two matrices
is_symmetric <- function(x, y) {
	# Determining the number of columns that we need to test based on the smaller matrix
	n <- min(ncol(x), ncol(y))

	# Looping over the columns
	for(i in 1:n) {
		x_index <- (ncol(x) - i + 1)
		y_index <- i

		if(!all(x[, x_index] == y[, y_index])) return(FALSE)
	}

	# We return TRUE if all columns did match
	return(TRUE)
}

# Computing the solution by adding the column symmetries to 100 times the row symmetries
column_symmetries <- sapply(x, find_symmetry)
row_symmetries <- sapply(x, function(x) find_symmetry(t(x)))

cat("Puzzle 13A: ", sum(column_symmetries + 100 * row_symmetries), "\n")

# PUZZLE 13B

# We simply need to adjust the is_symmetric function to return TRUE when there is just ONE difference
is_symmetric <- function(x, y) {

	# Initializing the number of differences to 0
	n_differences <- 0

	# Determining the number of columns that we need to test based on the smaller matrix
	n <- min(ncol(x), ncol(y))

	# Looping over the columns
	for(i in 1:n) {
		x_index <- (ncol(x) - i + 1)
		y_index <- i

		n_differences <- n_differences + sum(x[, x_index] != y[, y_index])
	}

	# We return TRUE if there is only ONE difference (exactly)
	n_differences == 1
}

column_symmetries <- sapply(x, find_symmetry)
row_symmetries <- sapply(x, function(x) find_symmetry(t(x)))

cat("Puzzle 13A: ", sum(column_symmetries + 100 * row_symmetries), "\n")

