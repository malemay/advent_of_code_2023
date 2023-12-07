## PUZZLE 3A
# Reading the input
x <- readLines("puzzle3.txt")

# A function that computes the numbers and their range for a single row
get_coords <- function(x, pattern) {

	# Get the positions of matches to numbers
	num_positions <- gregexpr(pattern, x)[[1]]
	numbers <- regmatches(x, list(num_positions))[[1]]

	lapply(1:length(num_positions), function(i){
		       c(numbers[i], num_positions[i], num_positions[i] + attr(num_positions, "match.length")[i] - 1)
})
}

# Getting the numbers and their positions in the whole dataset, along with their row position
coords <- lapply(1:length(x), function(i){
			 output <- get_coords(x[i], pattern = "[0-9]+")
			 for(j in 1:length(output)) {
				 output[[j]] <- c(output[[j]][1], i, output[[j]][2:3])
			 }
			 do.call("rbind", output)
})

# Binding everything into a matrix with all numbers and their coordinates
coords <- do.call("rbind", coords)
coords <- apply(coords, 2, as.numeric)

# Now finding the coordinates of all symbols
symbol_coords <- lapply(1:length(x), function(i){
			 output <- get_coords(x[i], pattern = "[^.0-9]")
			 for(j in 1:length(output)) {
				 output[[j]] <- c(output[[j]][1], i, output[[j]][2:3])
			 }
			 do.call("rbind", output)
})

symbol_coords <- do.call("rbind", symbol_coords)

# Removing the rows with NA (those without any symbols)
symbol_coords <- symbol_coords[complete.cases(symbol_coords), ]

# Also removing the columns that we do not need (we only need row and column position)
stopifnot(identical(symbol_coords[, 3], symbol_coords[, 4]))
symbol_coords <- symbol_coords[, c(2, 3)]
symbol_coords <- apply(symbol_coords, 2, as.numeric)

# Will be even easier if we generate a logical matrix of the positions where symbols occur
stopifnot(all(nchar(x) == nchar(x[1])))
symbol_matrix <- matrix(FALSE, nrow = length(x), ncol = nchar(x[1]))
symbol_matrix[symbol_coords] <- TRUE

# A function that determines whether a given number is adjacent to a symbol
is_adjacent <- function(num_row, symbols) {
	positions <- expand.grid(i = (num_row[2] - 1):(num_row[2] + 1), j = (num_row[3] - 1):(num_row[4] + 1))

	# Removing positions that are outside the range of the symbols matrix
	positions <- positions[positions$i >= 1 & positions$i <= nrow(symbols) & positions$j >= 1 & positions$j <= ncol(symbols), ]
	
	# Extracting the corresponding elements from the symbols matrix and returning TRUE if any of them is TRUE
	any(symbols[as.matrix(positions)])
}

# Getting a vector of valid numbers (those that have an adjacent symbol)
valid_numbers <- apply(coords, 1, is_adjacent, symbols = symbol_matrix)

cat("Puzzle 3A:", sum(coords[valid_numbers, 1]), "\n")


## PUZZLE 3B

# We first need to identify the potential gears by finding all asterisks
gear_coords <- lapply(1:length(x), function(i){
			 output <- get_coords(x[i], pattern = "\\*")
			 for(j in 1:length(output)) {
				 output[[j]] <- c(output[[j]][1], i, output[[j]][2:3])
			 }
			 do.call("rbind", output)
})

# Removing the rows with NA (those without any gears)
gear_coords <- do.call("rbind", gear_coords)
gear_coords <- gear_coords[complete.cases(gear_coords), ]

# Also removing the columns that we do not need (we only need row and column position)
stopifnot(identical(gear_coords[, 3], gear_coords[, 4]))
gear_coords <- gear_coords[, c(2, 3)]
gear_coords <- apply(gear_coords, 2, as.numeric)

# A function that sums the numbers adjacent to a gear if there are *exactly* two such numbers
gear_ratio <- function(gear_pos, num_coords) {
	gear_adj <- expand.grid(i = (gear_pos[1] - 1):(gear_pos[1] + 1), j = (gear_pos[2] - 1):(gear_pos[2] + 1))

	adjacent_numbers <- logical(nrow(num_coords))

	for(i in 1:nrow(num_coords)) {
		num_adj <- expand.grid(i = num_coords[i, 2], j = (num_coords[i, 3]):(num_coords[i, 4]))

		# Looping over the gear_adj to see if any of them matches the numbers
		for(k in 1:nrow(gear_adj)) {
			if(any(gear_adj[k, "i"] == num_adj$i & gear_adj[k, "j"] == num_adj$j)) {
				adjacent_numbers[i] <- TRUE
				break
			}
		}
	}

	if(sum(adjacent_numbers) == 2) return(prod(coords[adjacent_numbers, 1])) else return(0)
}

cat("Puzzle 3B:", sum(apply(gear_coords, 1, gear_ratio, num_coords = coords)), "\n")

