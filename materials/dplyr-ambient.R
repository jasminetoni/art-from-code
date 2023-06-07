# spatial exercise 3 pt 1

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

blank_canvas <- long_grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
) 

plot_painted_canvas <- function(canvas, palette = NULL) {
  if(is.null(palette)) {
    palette <- c("#e5ddc8","#01949a","#004369","#db1f48")
  }
  canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

# your code here! add to the blank canvas :)
blank_canvas

# spatial exercise 3 pt 2

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)

sample_canva <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

blank_canvas <- long_grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
) 

plot_painted_canvas <- function(canvas, palette = NULL) {
  if (is.null(palette)) {
    palette <- c("#e5ddc8", "#01949a", "#004369", "#db1f48")
  }
  canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

# Your code here! Add to the blank canvas :)

# gen_worley generator for spatial pattern
canvas_with_pattern <- blank_canvas %>%
  mutate(
    paint = gen_worley(x, y, seed = 12345)
  )

# define custom palette
custom_palette <- c("#eecc33", "#ff33ff", "#0535ff", "#09dd99")

# plot painted canvas with added pattern using custom palette
plot_painted_canvas(canvas_with_pattern, palette = custom_palette)

# save plot to a file
ggsave(
  filename = here("output", "spatial_pattern.png"), 
  plot = last_plot(),
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 200
)

