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

# A function that preprocesses a sequence to determine the maximum number of hashes
# that can fit for each index
preprocess_sequence <- function(x) {
	output <- numeric(length(x))

	for(i in 1:length(x)) {
		j <- i
		while(x[j] != "." && j <= length(x)) {
			output[i] <- output[i] + 1
			j <- j + 1
		}
	}

	output
}

# A function that returns a logical vector of positions of the hashes in a sequence
# The length is 1 more than the length of the sequence because we need to be able
# to tell whether there is a hash after a sequence
hash_positions <- function(x) {
	c(x == "#", FALSE)
}

# A function that computes the number of combinations to test
# given a number of '#' symbols to reach, the number already there,
# and the number of '?' symbols

# A recursive function that tests a target series of '#' symbols against a sequence
test_sequence <- function(sequence, seqlength, pp_vect, hpos, which_h, index, series) {

	# Initialize the return value to 0
	return_value <- 0

	# If this is the last element of the sequence then we can increment the return value
	# by the number of times that the series sites into the remaining sequence
	# But we must make sure that the admissible sequence is just one stretch
	if(length(series) == 1 && !any(hpos[(index + series):length(hpos)])) {
		# We meet all conditions, so we have found a possible match
		return(1)
	} else {
		# The series number eats up as many elements as the number + 1 (for the dot)
		if(!hpos[index + series[1]]) {
			# We need to find the next potential indices based on the series
			# size and the preprocessed sequence
			next_indices <- which(pp_vect >= series[2])
			next_indices <- next_indices[next_indices >= (index + series[1] + 1)]
			remaining_hashes <- which_h[which_h >= (index + series[1] + 1)]
			next_indices <- next_indices[next_indices <= suppressWarnings(min(remaining_hashes))]

			for(i in next_indices) {
				return_value <- return_value + test_sequence(sequence, seqlength, pp_vect, hpos,
									     which_h,
									     index = i,
									     series[-1])
			}
		}
	}

	return_value
}

prepare_indices <- function() {

}

# A function that determines the indices to test and adds them all to get the result
test_sequence_wrapper <- function(sequence, seqlength, pp_vect, hpos, index, series) {
	result <- 0

	which_h <- which(hpos)
	indices_to_test <- which(pp_vect >= series[1])
	indices_to_test <- indices_to_test[indices_to_test <= min(which_h)]

	for(i in indices_to_test) {
		result <- result + test_sequence(sequence, seqlength, pp_vect, hpos, which_h, i, series)
	}

	result
}

sum(sapply(x, function(x) test_sequence_wrapper(sequence = x[[1]],
						seqlength = length(x[[1]]),
						pp_vect = preprocess_sequence(x[[1]]),
						hpos = hash_positions(x[[1]]),
						index = 1,
						series = x[[2]])))

library(microbenchmark)
microbenchmark(sum(sapply(x, function(x) test_sequence_wrapper(sequence = x[[1]],
							       seqlength = length(x[[1]]),
							       pp_vect = preprocess_sequence(x[[1]]),
							       hpos = hash_positions(x[[1]]),
							       index = 1,
							       series = x[[2]]))),
	       times = 50)

test_sequence_wrapper(sequence = x[[1]][[1]],
		      seqlength = length(x[[1]][[1]]),
		      pp_vect = preprocess_sequence(x[[1]][[1]]),
		      hpos = hash_positions(x[[1]][[1]]),
		      index = 1,
		      series = x[[1]][[2]])

# Applying our new function to the modified input
n_possibilities <- parallel::mclapply(1:length(x), function(i, x) {
			   message("Processing record ", i, " out of ", length(x))
			   output <- test_sequence_wrapper(sequence = x[[i]][[1]],
							   seqlength = length(x[[i]][[1]]),
							   pp_vect = preprocess_sequence(x[[i]][[1]]),
							   hpos = hash_positions(x[[i]][[1]]),
							   index = 1,
							   series = x[[i]][[2]])
			   output
	       }, x = x, mc.cores = 8)

# Producing the duplicated records
x <- lapply(x, function(x) {
	       x[[1]] <- rep(c(x[[1]], "?"), 5)
	       x[[1]] <- x[[1]][-length(x[[1]])]
	       x[[2]] <- rep(x[[2]], 5)
	       x
})

n_possibilities <- parallel::mclapply(1:length(x), function(i, x) {
			   message("Processing record ", i, " out of ", length(x))
			   output <- test_sequence(sequence = x[[i]][[1]],
						   seqlength = length(x[[i]][[1]]),
						   pp_vect = preprocess_sequence(x[[i]][[1]]),
						   hpos = hash_positions(x[[i]][[1]]),
						   index = 1,
						   series = x[[i]][[2]])
			   output
	       }, x = x, mc.cores = 8)

cat("Puzzle 12B:", sum(unlist(n_possibilities)), "\n")
