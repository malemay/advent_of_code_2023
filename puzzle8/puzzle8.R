## PUZZLE 8A

# Read the input
x <- readLines("puzzle8.txt")

# The instructions are the first line
rl <- strsplit(x[1], "")[[1]]

# Then I will format the graph as an associative array with "R" and "L" elements
nodes <- x[-(1:2)]

# A function to parse a node
parse_node <- function(x) {
	l <- substring(x, 8, 10)
	r <- substring(x, 13, 15)

	# Returning a named vector
	c(R = r, L = l)
}

# Parsing all nodes into a list and naming them
node_list <- lapply(nodes, parse_node)
names(node_list) <- substring(nodes, 1, 3)

# Finding the number of steps it takes to reach ZZZ
i_node <- "AAA"
n_steps <- 0
index <- 1

while(i_node != "ZZZ") {
	i_node <- node_list[[i_node]][rl[index]]
	n_steps <- n_steps + 1
	index <- index + 1
	if(index > length(rl)) index <- 1
}

cat("Puzzle 8A:", n_steps, "\n")

## PUZZLE 8B

# Let us take a naive approach and hope it will work out
# in a reasonable amount of time

# Naive approach did not work but it seems that each series
# periodically returns to a node that ends in "Z"
# Let us try to identify those periods for each input node

i_nodes <- names(node_list)[substr(names(node_list), 3, 3) == "A"]
n_steps <- 0
index <- 1
periods <- rep(0, length(i_nodes))

while(any(periods == 0)) {
	i_nodes <- sapply(node_list[i_nodes], function(x) x[rl[index]])
	n_steps <- n_steps + 1

	for(i in 1:length(i_nodes)) {
		if(substr(i_nodes[i], 3, 3) == "Z" && periods[i] == 0) periods[i] <- n_steps
	}

	index <- index + 1
	if(index > length(rl)) index <- 1
}

# We need to find the least common multiple of all these numbers
# To do that we need to identify their prime factors
# I found out (somewhat be chance) that they all share 281 as a prime factor
solution <- prod((periods / 281)) * 281

cat("Puzzle 8B:", solution, "\n")

