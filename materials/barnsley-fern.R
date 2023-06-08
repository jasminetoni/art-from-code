
library(ggplot2)
library(tibble)
library(purrr)
library(dplyr)

# making a tree instead of a fern
tree_transform <- function(coord, ind) {
  # coefficients for the trunk function f_trunk
  if (ind == 1) {
    mat <- matrix(c(0, 0, 0, 0.16), 2, 2)
    off <- c(0, 0)
  }
  
  # coefficients for the left branch function f_left_branch
  if (ind == 2) {
    mat <- matrix(c(-0.15, 0.28, 0.26, 0.24), 2, 2)
    off <- c(0, 0.44)
  }
  
  # coefficients for the right branch function f_right_branch
  if (ind == 3) {
    mat <- matrix(c(0.15, 0.28, -0.26, 0.24), 2, 2)
    off <- c(0, 0.44)
  }
  
  # coefficients for the leaf function f_leaf
  if (ind == 4) {
    mat <- matrix(c(0.85, 0.04, -0.04, 0.85), 2, 2)
    off <- c(0, 1.6)
  }
  
  # return the affine transformed coordinates
  coord <- mat %*% coord + off
  return(coord)
}

tree_chaos <- function(iterations = 10000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # which transformation to apply at each iteration
  transform_index <- sample(
    x = 1:4, 
    size = iterations, 
    replace = TRUE, 
    prob = c(0.02, 0.45, 0.45, 0.08)
  )
  
  # initialise chaos game at the origin
  start <- matrix(c(0, 0))
  
  # helper function to collapse accumulated output
  bind_to_column_matrix <- function(lst) {
    do.call(cbind, lst)
  }
  
  # iterate until done
  coord_matrix <- transform_index |>
    accumulate(tree_transform, .init = start) |>
    bind_to_column_matrix() 
  
  # tidy the output, add extra columns, and return
  coord_df <- t(coord_matrix) |> 
    as.data.frame() 
  names(coord_df) <- c("x", "y")
  coord_df <- coord_df |>
    as_tibble() |>
    mutate(
      transform = c(0, transform_index),
      iteration = row_number() - 1
    )
  return(coord_df)
}

tree_dat <- tree_chaos(seed = 1)

# pulling it all together and adding transformation key on the side
pic <- ggplot(tree_dat, aes(x, y, colour = factor(transform))) +
  geom_point(size = 2, stroke = 0.1, alpha = 0.5) +
  coord_equal() +
  theme_void() +
  guides(colour = guide_legend(
    title = "Transformation",
    override.aes = list(size = 2)
  ))

plot(pic)


