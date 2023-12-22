# PUZZLE 10A

# Read the input and format as a matrix
x <- readLines("puzzle10.txt")
x <- do.call("rbind", strsplit(x, ""))

# Find the position of the "S"
s_index <- which(x == "S")
s_row <- row(x)[s_index]
s_col <- col(x)[s_index]
x[(s_row - 1):(s_row + 1), (s_col - 1):(s_col + 1)]

# The S connects to the left and right

# A list that contains the connections enabled by each symbol
# Each row contains one possible connection *from* the symbol,
# with the first number indicating the x-direction and the
# second number indicating the y-direction
symlist <- list(
		"-" = matrix(c(0, 1, 0, -1), nrow = 2, ncol = 2, byrow = TRUE),
		"|" = matrix(c(1, 0, -1, 0), nrow = 2, ncol = 2, byrow = TRUE),
		"J" = matrix(c(0, -1, -1, 0), nrow = 2, ncol = 2, byrow = TRUE),
		"L" = matrix(c(0, 1, -1, 0), nrow = 2, ncol = 2, byrow = TRUE),
		"F" = matrix(c(0, 1, 1, 0), nrow = 2, ncol = 2, byrow = TRUE),
		"7" = matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2, byrow = TRUE),
		"." = matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
)

# A function that returns the next movement based on the last
# one and the pipe shape at a location
next_move <- function(move, pipe_matrix) {
	# First, we check if the move connects to the pipe
	# For this, the incoming move must be the negative
	# of an outcoming move of the pipe
	if(all(move == -pipe_matrix[1, ])) return(pipe_matrix[2, ])
	if(all(move == -pipe_matrix[2, ])) return(pipe_matrix[1, ])
	
	# Otherwise we return a move of 0, 0 because the pipes do not connect
	return(c(0, 0))
}

# Now, starting from the S location, we move along the pipe
# until we come back to S
current_pos <- c(s_row, s_col)
current_move <- c(0, 1)
current_pos <- current_pos + current_move
n_moves <- 1

while(!(current_pos[1] == s_row && current_pos[2] == s_col)) {
	current_move <- next_move(current_move, symlist[[x[current_pos[1], current_pos[2]]]])
	current_pos <- current_pos + current_move
	n_moves <- n_moves + 1
}

cat("PUZZLE 10A:", n_moves / 2, "\n")

# PUZZLE 10B

# The problem is equivalent to considering the loop as a polygon
# and verifying which points fall *within* the polygon

# First we need to identify the coordinates of all the points
# that the loop goes through (the coordinates of the polygon)
# For that, we re-run the loop above

# We know that our polygon has 14024 vertices
# so we need a matrix with one more row
vertices <- matrix(nrow = 14025, ncol = 2)
vertices[1, ] <- c(s_row, s_col)

current_pos <- c(s_row, s_col)
current_move <- c(0, 1)
current_pos <- current_pos + current_move
n_moves <- 1
vertices[n_moves + 1, ] <- current_pos

while(!(current_pos[1] == s_row && current_pos[2] == s_col)) {
	current_move <- next_move(current_move, symlist[[x[current_pos[1], current_pos[2]]]])
	current_pos <- current_pos + current_move
	n_moves <- n_moves + 1
	vertices[n_moves + 1, ] <- current_pos
}

# To define the coordinates of the points of the polygons,
# we only need to consider the corners. Therefore, we
# can remove any vertex that corresponds to a - or a |
# We must also substitute the S character with the pipe
# type it truly corresponds to (-)

# Also creating a matrix that indicates whether a given position is in the loop
in_loop <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
in_loop[vertices] <- TRUE

x[s_row, s_col] <- "-"
vertices <- vertices[!x[vertices] %in% c("-", "|"), ]

# Creating a matrix for all sides of the polygon
sides <- cbind(vertices[1:(nrow(vertices) - 1),], vertices[2:nrow(vertices), ])
sides <- sides[, c(2, 1, 4, 3)]
colnames(sides) <- c("x1", "y1", "x2", "y2")

# Trying solution involving the winding number algorithm
# It involves finding the oriented angle between points and each side of
# the polygon, and summing those angles. The angle should be 360 degrees
# if the point is within the polygon, and 0 otherwise

# A function that computes the angle between two vectors
angle <- function(x1, y1, x2, y2) {
	atan2(x1 * y2 - y1 * x2, x1 * x2 + y1 * y2)
}

# A function that computes the angle between a point and all sides of the polygon
angle_sum <- function(point, sides) {
	x1 <- point[1] - sides[, 1]
	y1 <- point[2] - sides[, 2]
	x2 <- point[1] - sides[, 3]
	y2 <- point[2] - sides[, 4]
	sum(angle(x1, y1, x2, y2))
}

# This matrix holds the values for all points
angle_sums <- matrix(0, nrow = nrow(x), ncol = ncol(x))

for(i in 1:nrow(x)) {
	for(j in 1:ncol(x)) {
		# We do not need to test the points that are part of the loop
		if(in_loop[i, j]) next
		angle_sums[i, j] <- angle_sum(c(j, i), sides)
	}
}

# The ones within are the ones with a sum of angles near 2 * pi
cat("Puzzle 10B:", sum(round(angle_sums) > 0), "\n")

