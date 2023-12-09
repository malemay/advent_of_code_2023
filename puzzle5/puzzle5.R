# PUZZLE 5A

# Read the input
x <- readLines("puzzle5.txt")

# Splitting each map as an element of a vector
x <- strsplit(paste0(x, collapse = "\n"), "\n\n")[[1]]
x <- strsplit(x, "\n")

# Setting the seeds vector apart
seeds <- as.numeric(strsplit(sub("seeds: ", "", x[[1]]), " ")[[1]])

# Setting the map name as element name
maps <- x[-1]
names(maps) <- sapply(maps, function(i) sub(" map:", "", i[1]))

# Making the maps into matrices with the appropriate ranges
maps <- lapply(maps, function(x) x[-1])
maps <- lapply(maps, function(x) do.call("rbind", lapply(strsplit(x, " "), as.numeric)))

# A function that returns the destination number from a map and a source number
mapnum <- function(source_num, map) {
	# Looping over the ranges in the map to see if the source number matches any
	for(i in 1:nrow(map)) {
		if(source_num >= map[i, 2] && source_num < map[i, 2] + map[i, 3]) {
			return(map[i, 1] + (source_num - map[i, 2]))
		}
	}

	return(source_num)
}

get_location <- function(source_num, maps) {
	for(i in 1:length(maps)) {
		source_num <- mapnum(source_num, maps[[i]])
	}

	source_num
}

locations <- sapply(seeds, get_location, maps = maps)

cat("Puzzle 5A: ", min(locations), "\n")

## PUZZLE 5B

# We now need to work with ranges instead of individual positions
# if we are to ever hope solving the problem

# Transforming the seeds vector to a matrix with ranges
seeds <- matrix(c(seeds[c(TRUE, FALSE)], seeds[c(FALSE, TRUE)]), ncol = 2)
seeds[, 2] <- seeds[, 1] + seeds[, 2] - 1

# Also transforming the last column of the maps to an end of range
# instead of the length of the range
# We also order them to help with processing later
maps <- lapply(maps, function(x) {x[, 3] <- x[, 2] + x[, 3] - 1 ; x <- x[order(x[, 2]), ]; x})

# A function that compares a set of ranges against another one
# and returns a new set of ranges
compare_ranges <- function(input_range, map) {
	# First we keep only the set of ranges in the map that the input range interacts with
	map <- map[input_range[1] <= map[, 3] & input_range[2] >= map[, 2], , drop = FALSE]

	if(nrow(map) == 0) return(matrix(nrow = 0, ncol = 2))

	# We convert the input_range as a row matrix because it will be modified
	# by adding subsequent rows
	input_range <- matrix(input_range, nrow = 1)

	# Then we loop over the ranges that do overlap
	for(i in 1:nrow(map)) {
		# First we check if part of the input range is to the left of the map
		if(input_range[nrow(input_range), 1] < map[i, 2]) {
			# Then that part of the range is mapped to itself
			# And the remainder is kept for later processing
			new_rows <- rbind(c(input_range[nrow(input_range), 1], map[i, 2] - 1),
					  c(map[i, 2], input_range[nrow(input_range), 2]))

			input_range <- rbind(input_range[-nrow(input_range), ], new_rows)
		}

		# Then we process the part of the range that does overlap the map
		if(input_range[nrow(input_range), 1] >= map[i, 2]) {
			# We need to know how much to add relative to the start of the mapped range
			add_factor <- input_range[nrow(input_range), 1] - map[i, 2]
			input_width <- input_range[nrow(input_range), 2] - input_range[nrow(input_range), 1]
			
			# The input width must be constrained by the width of the range it is mapped to
			input_width <- min(input_width, map[i, 3] - input_range[nrow(input_range), 1])
			new_range <- c(map[i, 1] + add_factor, map[i, 1] + add_factor + input_width - 1)

			# The case when the input range extends beyond the map
			# then we need to keep that part
			if(input_range[nrow(input_range), 2] > map[i, 3]) {
				extending_range <- input_range[nrow(input_range), ]
				extending_range[1] <- map[i, 3] + 1
			} else {
				extending_range <- matrix(nrow = 0, ncol = 2)
			}

			input_range <- rbind(input_range[-nrow(input_range), ], new_range, extending_range)
		}
	}

	# We do not need to process the part of the range that extends beyond the map,
	# because it is mapped to itself anyway
	input_range
}

ranges <- seeds

for(i in 1:length(maps)) {
	ranges <- do.call("rbind", lapply(1:nrow(ranges), function(x) compare_ranges(ranges[x, ], maps[[i]])))
}

min(as.numeric(ranges))

cat("Puzzle 5B: ", min(as.numeric(ranges)), "\n")
