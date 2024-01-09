# PUZZLE 15A

# Reading the input
x <- readLines("puzzle15.txt")
x <- strsplit(x, ",")[[1]]

# A function that runs the hash algorithm on a string
hash <- function(x) {
	x <- utf8ToInt(x)
	output <- 0

	for(i in x) {
		output <- ((output + i) * 17) %% 256
	}

	output
}

cat("Puzzle 15A:", sum(sapply(x, hash)), "\n")

# PUZZLE 15B

# We can use a list (boxes) of lists (lenses) to very conveniently
# solve this problem in R
boxes <- list()

# Initializing the list with a place-holder so the list elements are never deleted
for(i in 1:256) boxes[[i]] <- list(placeholder1 = 0)

# Looping over the instructions to move the lenses around
for(i in x) {
	if(grepl("-$", i)) {
		string <- substring(i, 1, nchar(i) - 1)
		hash_value <- hash(string)
		boxes[[hash_value + 1]][[string]] <- NULL
	} else if(grepl("=", i)) {
		num <- as.numeric(substring(i, nchar(i), nchar(i)))
		string <- substring(i, 1, nchar(i) - 2)
		hash_value <- hash(string)
		boxes[[hash_value + 1]][[string]] <- num
	}
}

# Computing the total focusing power
solution <- 0

# We need to take into account the fact that the "0" lenses are not actually
# taking up a position
for(i in 1:length(boxes)) {
	# If only the placeholder is left then it is not worth computing
	if(length(boxes[[i]]) == 1) next

	for(j in 2:length(boxes[[i]])) {
		solution <- solution + i * (j - 1) * boxes[[i]][[j]]
	}
}

cat("Puzzle 15B:", solution, "\n")

