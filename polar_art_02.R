library(ggplot2)
library(tibble)

polar_art <- function(seed, n, palette, opacity = 1, size_range = c(0, 10), color_scheme = "gradient") {
  
  # set the state of the random number generator
  set.seed(seed)
  
  # data frame containing random values for 
  # aesthetics we might want to use in the art
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = as.factor(runif(n)),  # Convert shade to factor
    size = runif(n)
  )
  
  # Create the base ggplot object
  p <- ggplot(data = dat, aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = shade,
    size = size
  ))
  
  # Add geom_segment layer
  p <- p + geom_segment(show.legend = FALSE, alpha = opacity)
  
  # Set coordinate system to polar
  p <- p + coord_polar()
  
  # Add scale, size range, and color scheme
  p <- p + scale_y_continuous(expand = c(0, 0))
  p <- p + scale_x_continuous(expand = c(0, 0))
  
  if (color_scheme == "gradient") {
    p <- p + scale_colour_gradientn(colours = palette)
  } else {
    unique_shades <- levels(dat$shade)
    num_shades <- length(unique_shades)
    colors <- palette[seq_len(num_shades)]
    p <- p + scale_colour_manual(values = colors)
  }
  
  p <- p + scale_size(range = size_range)
  
  # Apply theme
  p <- p + theme_void()
  
  # Return final ggplot object
  return(p)
}

polar_art(
  seed = 10, 
  n = 100, 
  palette = c("dodgerblue", "magenta", "brown"),
  opacity = 0.8,
  size_range = c(5, 15),
  color_scheme = "manual"
)
