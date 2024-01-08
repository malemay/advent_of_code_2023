# PUZZLE 14A

# Reading the input and formatting as a matrix
x <- readLines("puzzle14.txt")
x <- do.call("rbind", strsplit(x, ""))

# A function that tilts the rocks towards north
# Takes one column at a time and returns the tilted column
tilt_north <- function(x) {
	# Finding the indices of all the (non-moving) cube-shaped rocks
	cubes <- which(x == "#")

	# Adding boundaries at the beginning and end of the column
	cubes <- c(0, cubes, length(x) + 1)

	# For each interval in-between cube-shaped rocks, we bring all rounded rocks to the beginning of the range
	for(i in 1:(length(cubes) - 1)) {
		cube1 <- cubes[i]
		cube2 <- cubes[i + 1]

		if((cube2 - cube1) == 1) next

		pos_range <- (cube1 + 1):(cube2 - 1)

		n_rounded_cubes <- sum(x[pos_range] == "O")

		# Assigning the new values based on the number of rounded rocks
		if(n_rounded_cubes == 0) next

		x[pos_range] <- "."
		x[(cube1 + 1):(cube1 + n_rounded_cubes)] <- "O"
	}

	# Returning the modified vector
	x
}

# Tilting all the rocks in the matrix
tilted_x <- apply(x, 2, tilt_north)

# A function that computes the load of a given column
compute_load <- function(x) {
	sum((length(x):1)[x == "O"])
}

solution <- sum(apply(tilted_x, 2, compute_load))

cat("Puzzle 14A:", solution, "\n")

# PUZZLE 14B

# We need to rewrite the tilt_north function to allow tilting in various directions
# Tilting north works the same as tilting west, the difference is in how the function is called
# We need to adjust for south/east tilting
tilt <- function(x, direction) {

	# Finding the indices of all the (non-moving) cube-shaped rocks
	cubes <- which(x == "#")

	# Adding boundaries at the beginning and end of the column
	cubes <- c(0, cubes, length(x) + 1)

	# For each interval in-between cube-shaped rocks, we bring all rounded rocks to the beginning of the range
	for(i in 1:(length(cubes) - 1)) {
		cube1 <- cubes[i]
		cube2 <- cubes[i + 1]

		if((cube2 - cube1) == 1) next

		pos_range <- (cube1 + 1):(cube2 - 1)

		n_rounded_cubes <- sum(x[pos_range] == "O")

		# Assigning the new values based on the number of rounded rocks
		if(n_rounded_cubes == 0) next

		x[pos_range] <- "."

		if(direction %in% c("north", "west")) {
			x[(cube1 + 1):(cube1 + n_rounded_cubes)] <- "O"
		} else if(direction %in% c("south", "east")) {
			x[(cube2 - 1):(cube2 - n_rounded_cubes)] <- "O"
		} else {
			stop("Unsupported tilting direction")
		}
	}

	# Returning the modified vector
	x
}

# This function applied a complete north-west-south-east cycle to the matrix
apply_cycle <- function(x) {
	x <- apply(x, 2, tilt, direction = "north")
	x <- t(apply(x, 1, tilt, direction = "west"))
	x <- apply(x, 2, tilt, direction = "south")
	x <- t(apply(x, 1, tilt, direction = "east"))

	x
}

# Applying the function one billion times is too time-demanding
# Perhaps there is some period after which the matrix returns
# to the same state, in which case we do not need to do all
# one billion iterations to compute the solution

# Let us compute 100,000 cycles and see whether there is a periodic
# return to the same state. To do that, we will compute the load after
# each cycle
i <- 0
tilted <- x
loads <- numeric(500)

while(i < 500) {
	if(i %% 100 == 0) message("Iteration ", i)
	tilted <- apply_cycle(tilted)
	loads[i + 1] <- sum(apply(tilted, 2, compute_load))
	i <- i + 1
}

# Let us see if some numbers occur frequently
sort(table(loads))

# We can find the length of the period by looking at the return times for the less
# frequently visited loads
table(diff(which(loads == 95021)))
# The return time is 27

# Let us find an index that could periodically lead to one billion
solution_index <- 10^9 %% 27 + 27 * 10

cat("Puzzle 14A:", loads[solution_index], "\n")
