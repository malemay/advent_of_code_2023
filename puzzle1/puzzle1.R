## PUZZLE 1A
# Reading the input and removing letters
x <- readLines("puzzle1.txt")
x <- gsub("[^0-9]+", "", x)

# Extracting the first and last digit
x <- sapply(strsplit(x, ""), function(x) paste0(x[1], x[length(x)]))

# Computing and printing the solution
cat("Puzzle 1a:", sum(as.numeric(x)), "\n")

## PUZZLE 1B
# Reading the input
x <- readLines("puzzle1.txt")

# A lookup table of spelled-out numbers to digits
lookup <- c("one" = "1",
	    "two" = "2",
	    "three" = "3",
	    "four" = "4",
	    "five" = "5",
	    "six" = "6",
	    "seven" = "7",
	    "eight" = "8",
	    "nine" = "9")

# This pattern allows to find spelled-out numbers at the beginning of a (sub)string
# The pattern replacement approach did not work for this puzzle because
# some letters are used to spell more than one digit
pattern <- paste0("^(", paste0(names(lookup), collapse = "|"), ")")

# Filling up a list with the digits in each entry
digits <- vector("list", length(x))

# Looping over each character of every input line
# The code could probably be re-written more efficiently
for(i in 1:length(x)) {
	for(j in 1:nchar(x[i])) {
		# First we check if the character is a digit and append it if it is
		if((char <- substring(x[i], j, j)) %in% lookup) {
			digits[[i]] <- c(digits[[i]], char)
		} else {
			# Otherwise we check if the substring at this location
			# starts with a spelled-out digit, and append it if so
			sstring <- substring(x[i], j)

			if(length(num <- regmatches(sstring, regexpr(pattern, sstring)))) {
				digits[[i]] <- c(digits[[i]], lookup[num])
			}
		}
	}
}

# The remainder of the puzzle is the same as in part A
digits <- sapply(digits, function(i) paste0(i[1], i[length(i)]))
cat("Puzzle 1b:", sum(as.numeric(digits)), "\n")

