# spatial exercise 2 pt 1
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

make_noise_art <- function(
    generator = gen_perlin, 
    frequency = 100, 
    seed = 2000,
    pixels = 20000,
    palette = c("#e5ddc2", "#01949b", "#004362", "#db1f41"), 
    ...
) {
  
  # define the grid
  canvas <- expand.grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) 
  
  # use the generator to add paint
  canvas <- canvas %>%
    mutate(
      paint = generator(
        x, y, 
        frequency = frequency, 
        seed = seed, 
        ...
      )
    )
  
  # use ggplot2 to draw the picture
  art <- ggplot(canvas, aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
  
  return(art)
}

# call make_noise_art with idiosyncratic parameters
art <- make_noise_art(
  generator = gen_worley,
  seed = 2000, 
  palette = sample_canva(200),
  value = "distance",
  pixels = 2000
)

# print the plot
print(art)

# save the plot to file with a generic file name
ggsave(
  filename = here("output", "noise-art.png"), 
  plot = art,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 200
)


# spatial exercise 2 pt 2

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)
#install.packages("lubridate")
# timestamps package
library(lubridate)

# generate unique filename
generate_filename <- function(prefix) {
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  filename <- paste0(prefix, "_", timestamp, ".png")
  return(filename)
}
# rest of code goes here

# save the plot to a file with a unique filename
output_dir <- here("output")
filename <- generate_filename("noise-art")
ggsave(
  filename = file.path(output_dir, filename), 
  plot = art,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 200
)

