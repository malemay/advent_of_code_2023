# PUZZLE 16A

# Reading the input
x <- readLines("puzzle16.txt")
x <- do.call("rbind", strsplit(x, ""))

# A function that takes a direction and tile and returns the next direction
# The direction is a length-2 vector with the (row, column) direction
move_beam <- function(direction, tile) {
	if(tile == ".") {
		return(list(direction))
	} else if(tile == "/") {
		#if(direction[1] == 1) return(c(0, -1))
		#if(direction[1] == -1) return(c(0, 1))
		#if(direction[2] == 1) return(c(-1, 0))
		#if(direction[2] == -1) return(c(1, 0))
		return(list(-rev(direction)))
	} else if(tile == "\\") {
		#if(direction[1] == 1) return(c(0, 1))
		#if(direction[1] == -1) return(c(0, -1))
		#if(direction[2] == 1) return(c(1, 0))
		#if(direction[2] == -1) return(c(-1, 0))
		return(list(rev(direction)))
	} else if(tile == "|") {
		if(direction[1] != 0) return(list(direction))

		return(list(c(1, 0), c(-1, 0)))
	} else if(tile == "-") {
		if(direction[2] != 0) return(list(direction))

		return(list(c(0, 1), c(0, -1)))
	} else {
		stop("Unknown tile")
	}
}

# Writing a function that computes the number of energized tiles given 
# a set of tiles and an initial beam
# x: the matrix of tiles
# initial_dir: the initial direction of the beam
# initial_pos: the position of the tile that the beam starts from
energize_tiles <- function(x, initial_dir, initial_pos) {

	# Initializing a list with all the active beams
	# The first beam is heading right in the top-left position
	beams <- list(list(dir = initial_dir, pos = initial_pos))

	# Looping over the beams, always processing the last one in the list
	# until there are no longer any beams
	nrows <- nrow(x)
	ncols <- ncol(x)

	energized <- matrix(FALSE, nrow = nrows, ncol = ncols)
	energized[1, 1] <- TRUE

	# Creating a list of arrays that tell whether a given tile has already
	# been visited in a given direction. These beams to not need to be processed
	# because they will yield energized tiles that are already known from a previous
	# beam
	visited <- list()

	for(i in c("right", "left", "up", "down")) {
		visited[[i]] <- matrix(FALSE, nrow = nrows, ncol = ncols)
	}

	while(length(beams)) {
		# Getting the position and direction of the current beam
		i <- length(beams)

		current_dir <- beams[[i]]$dir
		current_pos <- beams[[i]]$pos

		# Getting the next direction for this beam
		next_dir <- move_beam(current_dir, x[current_pos[1], current_pos[2]])

		# Updating the current beam accordingly
		beams[[i]]$dir <- next_dir[[1]]
		beams[[i]]$pos <- beams[[i]]$pos + next_dir[[1]]

		# We remove the beam if it went outside the matrix
		if(beams[[i]]$pos[1] < 1 || beams[[i]]$pos[1] > nrow(x) || beams[[i]]$pos[2] < 1 || beams[[i]]$pos[2] > ncol(x)) {
			beams[[i]] <- NULL
		} else {
			# Otherwise we add it to the matrix of energized tiles
			energized[beams[[i]]$pos[1], beams[[i]]$pos[2]] <-  TRUE

			# We can remove the beam if it goes in a direction that has already been visited
			if(beams[[i]]$dir[1] == 1) string_dir <- "down"
			if(beams[[i]]$dir[1] == -1) string_dir <- "up"
			if(beams[[i]]$dir[2] == 1) string_dir <- "right"
			if(beams[[i]]$dir[2] == -1) string_dir <- "left"

			if(visited[[string_dir]][beams[[i]]$pos[1], beams[[i]]$pos[2]]) {
				beams[[i]] <- NULL
			} else {
				visited[[string_dir]][beams[[i]]$pos[1], beams[[i]]$pos[2]] <- TRUE
			}
		}

		# Updating the i index
		i <- length(beams)

		# If the beam was split then we need to add it to the list
		if(length(next_dir) == 2) {
			# The current beam becomes one of the two beams
			beams[[i + 1]] <- list()
			beams[[i + 1]]$dir <- next_dir[[2]]
			beams[[i + 1]]$pos <- current_pos + next_dir[[2]]

			# We remove the beam if it went outside the matrix
			if(beams[[i + 1]]$pos[1] < 1 || beams[[i + 1]]$pos[1] > nrow(x) || beams[[i + 1]]$pos[2] < 1 || beams[[i + 1]]$pos[2] > ncol(x)) {
				beams[[i + 1]] <- NULL
			} else {
				# Otherwise we add it to the matrix of energized tiles
				energized[beams[[i + 1]]$pos[1], beams[[i + 1]]$pos[2]] <-  TRUE

				# We can remove the beam if it goes in a direction that has already been visited
				if(beams[[i + 1]]$dir[1] == 1) string_dir <- "down"
				if(beams[[i + 1]]$dir[1] == -1) string_dir <- "up"
				if(beams[[i + 1]]$dir[2] == 1) string_dir <- "right"
				if(beams[[i + 1]]$dir[2] == -1) string_dir <- "left"

				if(visited[[string_dir]][beams[[i + 1]]$pos[1], beams[[i + 1]]$pos[2]]) {
					beams[[i + 1]] <- NULL
				} else {
					visited[[string_dir]][beams[[i + 1]]$pos[1], beams[[i + 1]]$pos[2]] <- TRUE
				}
			}
		}
	}

	sum(energized)
}

solution <- energize_tiles(x, initial_dir = c(0, 1), initial_pos = c(1, 1))
cat("Puzzle 16A:", solution, "\n")

# PUZZLE 16B

# We simply need to loop over all possible entry points
solution <- 0

# Loop over all entries from the left side
for(i in 1:nrow(x)) {
	message("Processing iteration ", i, " out of ", nrow(x))
	solution <- max(solution, energize_tiles(x, initial_dir = c(0, 1), initial_pos = c(i, 1)))
}

# Loop over all entries from the right side
for(i in 1:nrow(x)) {
	message("Processing iteration ", i, " out of ", nrow(x))
	solution <- max(solution, energize_tiles(x, initial_dir = c(0, -1), initial_pos = c(i, ncol(x))))
}

# Loop over all entries from the top
for(i in 1:ncol(x)) {
	message("Processing iteration ", i, " out of ", ncol(x))
	solution <- max(solution, energize_tiles(x, initial_dir = c(1, 0), initial_pos = c(1, i)))
}

# Loop over all entries from the bottom
for(i in 1:ncol(x)) {
	message("Processing iteration ", i, " out of ", ncol(x))
	solution <- max(solution, energize_tiles(x, initial_dir = c(-1, 0), initial_pos = c(nrow(x), i)))
}

cat("Puzzle 16B:", solution, "\n")

