# PUZZLE 12A

# Read the puzzle input
x <- readLines("puzzle12.txt")
x <- strsplit(x, " ")
x <- lapply(x, function(x) list(strsplit(x[1], "")[[1]], as.integer(strsplit(x[2], ",")[[1]])))

# Looking at how many unknowns are in each record
table(sapply(x, function(x) sum(x[[1]] == "?")))
# Even the maximum of 2^19 (524288) is still doable

# A function that takes a given record and returns the number of possible arrangements
n_possible <- function(x) {
	# Creating a data.frame with all possible combinations
	comb_list <- list()
	for(i in 1:sum(x[[1]] == "?")) comb_list[[i]] <- c(0, 1)
	combinations <- as.matrix(expand.grid(comb_list))

	# We also get the indices of the unknown values
	indices <- which(x[[1]] == "?")

	# Initializing the output value to 0
	output <- 0

	# Then we loop over all combinations
	for(i in 1:nrow(combinations)) {
		x_copy <- x[[1]]
		# The ones are converted to #
		x_copy[indices[combinations[i, ] == 1]] <- "#"
		# The remaining values are converted to "."
		x_copy[x_copy == "?"] <- "."
		# Getting the run length encoding of the resulting values
		x_rle <- rle(x_copy)
		# We check if it matches the pattern
		output <- output + identical(x_rle$lengths[x_rle$values == "#"], x[[2]])
	}

	output
}

n_possibilities <- parallel::mclapply(1:length(x), function(i, x) {
			   message("Processing record ", i, " out of ", length(x))
			   output <- n_possible(x[[i]])
			   output
}, x = x, mc.cores = 8)

cat("Puzzle 12A:", sum(unlist(n_possibilities)), "\n")

# PUZZLE 12B

# A function that computes the number of combinations to test
# given a number of '#' symbols to reach, the number already there,
# and the number of '?' symbols

# A recursive function that tests a target series of '#' symbols against a sequence
test_sequence <- function(sequence, series) {

	# Initialize the return value to 0
	return_value <- 0

	# If this is the last element of the sequence then we can increment the return value
	# by the number of times that the series sites into the remaining sequence
	# But we must make sure that the admissible sequence is just one stretch
	#if(length(series) == 1) {
	if(length(series) == 1) {
		# We test if the sequence is compatible with the provided series of numbers
		# We must make sure that we have consumed all the '#' symbols in the sequence
		# If the sequence does not work, we cut the first symbol so we can try the next one
		# We can move it along to the end and count the number of matches
		# However we must make sure to consume all # symbols along the way

		while(series <= length(sequence)) {
			if(all(sequence[1:series] == "#" | sequence[1:series] == "?") &&
			   (length(sequence) == series || ! '#' %in% sequence[(series + 1):(length(sequence))])) {
				# We meet all conditions, so we increment the counter
				return_value <- return_value + 1
			}

			# Then we test the next iteration, unless we would skip a '#' symbol
			if(sequence[1] == "#") return(return_value)
			sequence <- sequence[-1]
		}

		return(return_value)
	} else {
		# We increment the starting position as long as we do not leave a '#' behind or
		# the series is longer than the remaining sequence
		series_length <- sum(series) + length(series) - 1
		while(series_length <= length(sequence)) {
			# The series number eats up as many elements as the number + 1 (for the dot)
			if(sequence[series[1] + 1] != "#" && all(sequence[1:series[1]] == "#" | sequence[1:series[1]] == "?")) {
				return_value <- return_value + test_sequence(sequence[-(1:(series[1] + 1))], series[-1])
			}

			# Then we try the next position
			# We cannot eat up a fixed '#' symbol
			if(sequence[1] == "#") return(return_value)
			sequence <- sequence[-1]
		}
	}

	return_value
}

sum(sapply(x, function(x) test_sequence(x[[1]], x[[2]])))
library(microbenchmark)
microbenchmark(sum(sapply(x, function(x) test_sequence(x[[1]], x[[2]]))))

# Applying our new function to the modified input
n_possibilities <- parallel::mclapply(1:length(x), function(i, x) {
			   message("Processing record ", i, " out of ", length(x))
			   output <- test_sequence(x[[i]][[1]], x[[i]][[2]])
			   output
}, x = x, mc.cores = 8)

# Applying our new function to the modified input
n_possibilities <- parallel::mclapply(1:length(x), function(i, x) {
			   message("Processing record ", i, " out of ", length(x))
			   output <- test_sequence(x[[i]][[1]], x[[i]][[2]])
			   output
}, x = x, mc.cores = 8)

# Producing the duplicated records
x <- lapply(x, function(x) {
	       x[[1]] <- rep(c(x[[1]], "?"), 5)
	       x[[1]] <- x[[1]][-length(x[[1]])]
	       x[[2]] <- rep(x[[2]], 5)
	       x
})


cat("Puzzle 12B:", sum(unlist(n_possibilities)), "\n")
