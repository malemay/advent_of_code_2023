# PUZZLE 4A

# Reading the input
x <- readLines("puzzle4.txt")

# A function to parse a single card
parse_card <- function(x) {
	x <- sub("^Card.*:", "", x)
	x <- strsplit(x, "\\|")[[1]]
	winning <- strsplit(trimws(x[1]), "[ ]+")[[1]]
	numbers <- strsplit(trimws(x[2]), "[ ]+")[[1]]
	list(winning = winning, numbers = numbers)
}

# A function to calculate the score associated with a given card
get_score <- function(card) {
	n_matches <- sum(card$numbers %in% card$winning)

	if(n_matches == 0) return(0) else return(2^(n_matches - 1))
}

solution <- sum(sapply(lapply(x, parse_card), get_score))

cat("Puzzle 4A:", solution, "\n")

# PUZZLE 4A

# We need a new function to get the score of each card
get_score2 <- function(card) sum(card$numbers %in% card$winning)

# We pre-compute the number of matches
matches <- sapply(lapply(x, parse_card), get_score2)

# We start with one of each card
n_cards <- rep(1, length(x))

# We loop over each card and add to the number of cards as we go
for(i in 1:length(matches)) {
	if(matches[i] > 0) n_cards[(i + 1):(i + matches[i])] <- n_cards[(i + 1):(i + matches[i])] + n_cards[i]
}

cat("Puzzle 4B:", sum(n_cards), "\n")
