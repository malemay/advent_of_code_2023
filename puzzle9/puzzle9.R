# PUZZLE 9A

# Read the input
x <- readLines("puzzle9.txt")
x <- lapply(strsplit(x, " "), as.numeric)

# A function that returns the sum requested by the challenge
next_num <- function(x) {
	# Initializing a numeric vector that will be filled with the last
	# number of each difference sequence
	numbers <- numeric(length(x))

	# We compute the differences until all numbers are 0
	i <- 0

	while(!all(x == 0)) {
		# Increment the counter and add the last number to the series
		i <- i + 1
		numbers[i] <- x[length(x)]
		x <- diff(x)
	}

	# At the end we compute the sum of the numbers vector
	return(sum(numbers))
}

cat("Puzzle 9A:", sum(sapply(x, next_num)), "\n")

## PUZZLE 9B

# Because of the substractions, we just need to invert the sign for each
# number before computing the sum
prev_num <- function(x) {
	# Initializing a numeric vector that will be filled with the first
	# number of each difference sequence
	numbers <- numeric(length(x))

	# We compute the differences until all numbers are 0
	i <- 0

	while(!all(x == 0)) {
		# Increment the counter and add the first number to the series
		i <- i + 1
		numbers[i] <- x[1]
		x <- diff(x)
	}

	if(i == 0) return(x[1])

	# We need to invert the signs, starting from + for the first number
	# because it will be substracted from
	numbers <- suppressWarnings(numbers[1:i] * c(1, -1))

	# At the end we compute the sum of the numbers vector
	return(sum(numbers))
}

cat("Puzzle 9B:", sum(sapply(x, prev_num)), "\n")

