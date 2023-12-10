# PUZZLE 7A

# Reading the input
x <- read.table("puzzle7.txt")

# Functions that determine whether a hand matches a given type
get_table <- function(x) as.numeric(table(strsplit(x, "")[[1]]))

is_five <- function(x) {
	xtab <- get_table(x)
	5 %in% xtab
}

is_four <- function(x) {
	xtab <- get_table(x)
	4 %in% xtab
}

is_fullhouse <- function(x) {
	xtab <- get_table(x)
	identical(c(2, 3), sort(xtab))
}

is_three <- function(x) {
	xtab <- get_table(x)
	3 %in% xtab
}

is_twopair <- function(x) {
	xtab <- get_table(x)
	identical(c(2, 2), sort(xtab, decreasing = TRUE)[1:2])
}

is_pair <- function(x) {
	xtab <- get_table(x)
	2 %in% xtab
}

# A function that assigns a type to a hand
hand_type <- function(x) {
	if(is_five(x)) return(7)
	if(is_four(x)) return(6)
	if(is_fullhouse(x)) return(5)
	if(is_three(x)) return(4)
	if(is_twopair(x)) return(3)
	if(is_pair(x)) return(2)
	return(1)
}

# Then we need another function for converting hands to numerical values
# Because there are more hands than digits, we will use a base-100 system
# for conversion
card_lookup <- c("A" = 13,
		 "K" = 12,
		 "Q" = 11,
		 "J" = 10,
		 "T" = 9,
		 "9" = 8,
		 "8" = 7,
		 "7" = 6,
		 "6" = 5,
		 "5" = 4,
		 "4" = 3,
		 "3" = 2,
		 "2" = 1)

hand_to_num <- function(x, card_lookup) {
	cards <- strsplit(x, "")[[1]]
	sum(card_lookup[cards] * 10^(4:0 * 2))
}

hand_types <- sapply(x[[1]], hand_type)
hand_sums <- sapply(x[[1]], hand_to_num, card_lookup = card_lookup)

x <- x[order(hand_types, hand_sums), ]

cat("Puzzle 7A:", sum(x[[2]] * 1:nrow(x)), "\n")

## PUZZLE 7B

# We first redefine the card_lookup vector
card_lookup_jokers <- c("A" = 13,
			"K" = 12,
			"Q" = 11,
			"T" = 10,
			"9" = 9,
			"8" = 8,
			"7" = 7,
			"6" = 6,
			"5" = 5,
			"4" = 4,
			"3" = 3,
			"2" = 2,
			"J" = 1)

# We need to redefine the hand_type function to take jokers into account
hand_type_jokers <- function(x, lookup) {
	# If there are no jokers, this is the same as before
	if(!grepl("J", x)) return(hand_type(x))

	# Otherwise we go on with considering very possible value for jokers
	# and keep the value that yields the strongest type
	max_value <- 0

	for(i in names(lookup)) {
		x2 <- gsub("J", i, x)
		i_value <- hand_type(x2)
		max_value <- max(max_value, i_value)
	}

	return(max_value)
}

# Then we just need to perform the same operations as before
hand_types <- sapply(x[[1]], hand_type_jokers, lookup = card_lookup_jokers)
hand_sums <- sapply(x[[1]], hand_to_num, card_lookup = card_lookup_jokers)

x <- x[order(hand_types, hand_sums), ]

cat("Puzzle 7B:", sum(x[[2]] * 1:nrow(x)), "\n")

