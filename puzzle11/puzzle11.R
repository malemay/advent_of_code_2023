# PUZZLE 11A

# Reading the puzzle input
x <- readLines("puzzle11.txt")
x <- do.call("rbind", strsplit(x, ""))

# Expanding the galaxies by finding the rows and columns that have no galaxies
expanded_rows <- which(apply(x, 1, function(x) sum(x == "#") == 0))
expanded_cols <- which(apply(x, 2, function(x) sum(x == "#") == 0))
expanded_rows <- rep(1:nrow(x), ifelse(1:nrow(x) %in% expanded_rows, 2, 1))
expanded_cols <- rep(1:ncol(x), ifelse(1:ncol(x) %in% expanded_cols, 2, 1))

# Getting the final matrix of expanded galaxies
galaxies <- x[expanded_rows, expanded_cols]

# Finding the coordinates of the galaxies
gcoords <- cbind(row(galaxies)[galaxies == "#"], col(galaxies)[galaxies == "#"])

# We create a distance matrix for all galaxies
dist_matrix <- matrix(0, nrow = nrow(gcoords), ncol = nrow(gcoords))

# Computing the distance between every matrix pair
for(i in 1:(nrow(gcoords) - 1)) {
	for(j in (i + 1):nrow(gcoords)) {
		dist_matrix[i, j] <- abs(gcoords[i, 1] - gcoords[j, 1]) + abs(gcoords[i, 2] - gcoords[j, 2])
	}
}

cat("Puzzle 11A:", sum(dist_matrix), "\n")

# PUZZLE 11B

# Let us work from the initial unexpanded galaxies
expanded_rows <- which(apply(x, 1, function(x) sum(x == "#") == 0))
expanded_cols <- which(apply(x, 2, function(x) sum(x == "#") == 0))

# We recreate the matrix of galaxy coordinates and the distance matrix
gcoords <- cbind(row(x)[x == "#"], col(x)[x == "#"])
dist_matrix <- matrix(0, nrow = nrow(gcoords), ncol = nrow(gcoords))

# Computing the distance between every matrix pair
for(i in 1:(nrow(gcoords) - 1)) {
	for(j in (i + 1):nrow(gcoords)) {
		dist_matrix[i, j] <- abs(gcoords[i, 1] - gcoords[j, 1]) + abs(gcoords[i, 2] - gcoords[j, 2])
		# But now we add 999999 (one million minus the one already computed) for each expanded part of the universe
		dist_matrix[i, j] = dist_matrix[i, j] + 999999 * sum(gcoords[i, 1]:gcoords[j, 1] %in% expanded_rows) +
			                                999999 * sum(gcoords[i, 2]:gcoords[j, 2] %in% expanded_cols)
	}
}

options(scipen = 12)
cat("Puzzle 11B:", sum(dist_matrix), "\n")

