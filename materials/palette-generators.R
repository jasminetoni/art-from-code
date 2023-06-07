library(ggthemes)

# the original function from the first session
sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

# exercise 2 pt 1
palette <- sample_canva()
show_col(palette)

# exercise 2 pt 2
sample_named_colours <- function(n) {
  possible_colours <- colours(distinct = TRUE)
  sample(possible_colours, n)
}

n <- 100  # number of segments in polar art
colors <- sample_named_colours(n)  # generating sample of named colors

polar_art(seed = 123, n = n, palette = colors)

# exercise 2 pt 3
sample_canva_custom <- function(n) {
  colors <- unlist(ggthemes::canva_palettes)
  palette <- sample(colors, n)
  polar_art(seed = 123, n = n, palette = palette)
}
sample_canva_custom(100)









# the extended function used in later sessions
sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}


