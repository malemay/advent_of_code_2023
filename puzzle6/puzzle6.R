# PUZZLE 6A

# Reading the input
x <- readLines("puzzle6.txt")

# Formatting the times and distances
times <- sub("^Time:[ ]+", "", x[1])
times <- as.numeric(strsplit(times, "[ ]+")[[1]])

distances <- sub("^Distance:[ ]+", "", x[2])
distances <- as.numeric(strsplit(distances, "[ ]+")[[1]])

# A function that computes the number of winning combinations
# for each time/distance
n_winning <- function(time, distance) {
	sum <- 0

	for (i in 0:time) {
		sum <- sum + (i * (time - i) > distance)
	}

	sum
}

combs <- numeric(length(times))

for(i in 1:length(combs)) {
	combs[i] <- n_winning(times[i], distances[i])
}

cat("Puzzle 6A:", prod(combs), "\n")

# Puzzle 6B

# We need to reprocess the input
times <- sub("^Time:[ ]+", "", x[1])
times <- as.numeric(gsub(" ", "", times))

distances <- sub("^Distance:[ ]+", "", x[2])
distances <- as.numeric(gsub(" ", "", distances))

# The product is maximized when the time is divided in half.
# It should be sufficient to start from there and go down
# until we no longer win the race. Since this is symmetrical
# we could then multiply by two and add the value for the mid
num <- times / 2 - 1
n_times <- 0

while(num * (times - num) > distances) {
	n_times <- n_times + 1
	num <- num - 1
}

cat("Puzzle 6B:", n_times * 2 + 1, "\n")
