## PUZZLE 2A

# Read the input
x <- readLines("puzzle2.txt")

# A function that parses a line into a list with lookup
# tables containing the information for each trial
parse_line <- function(x) {
	# Removing the prefix and splitting along semicolons
	x <- sub("Game [0-9]+: ", "", x)
	x <- strsplit(x, ";")[[1]]

	# Setting the number of cubes per color for each trial
	lapply(x, function(x) {
		       # Initializing each color to 0
		       output <- c("blue" = 0, "green" = 0, "red" = 0)

		       # Splitting the input using commas
		       counts <- strsplit(x, ",")[[1]]

		       # Looping over each color and using regular expression to extract the counts
		       for(i in counts) {
			       color <- regmatches(i, regexpr("(red|green|blue)", i))
			       output[color] <- as.numeric(sub(paste0("[ ]?([0-9]+) ", color), "\\1", i))
		       }

		       output
	})
}

# Parsing all games
all_games <- lapply(x, parse_line)

# A function that tests whether a given game matches input criteria
is_possible <- function(x, max_red, max_blue, max_green) {

	for(i in 1:length(x)) {
		if(x[[i]]["red"] > max_red) return(FALSE)
		if(x[[i]]["blue"] > max_blue) return(FALSE)
		if(x[[i]]["green"] > max_green) return(FALSE)
	}

	return(TRUE)
}

# A logical vector indicating which games are possible
possible_games <- sapply(all_games, is_possible, max_red = 12, max_blue = 14, max_green = 13)

cat("Puzzle 1B:", sum(which(possible_games)), "\n")


## PUZZLE 2B

# We can use the same all_games list as above, but we apply a different function

# A function that finds the maximum value for each color in a game
set_power <- function(x) {
	max_blue <- max(sapply(x, function(x) x["blue"]))
	max_red <- max(sapply(x, function(x) x["red"]))
	max_green <- max(sapply(x, function(x) x["green"]))

	# Multiplying the values together (requested output)
	max_blue * max_red * max_green
}

cat("Puzzle 1B:", sum(sapply(all_games, set_power)), "\n")

