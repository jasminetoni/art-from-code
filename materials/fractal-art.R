# spatial exercise 4

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

fractal_art <- function(fractal, generator, palette = NULL, ...) {
  blank_canvas |>
    mutate(
      paint = fracture(
        noise = generator,
        fractal = fractal,
        x = x, 
        y = y, 
        ...
      )
    ) |>
    plot_painted_canvas(palette = palette)
}

# idea 1 (modifying basic arguements)

# change  generator, fractal, freq_init, octaves, seed, and palette

# load in libraries
library(ggplot2)
library(ggthemes)

# define sample_canva
sample_canva <- function(seed = NULL) {
  if (!is.null(seed))
    set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

# define blank_canvas
blank_canvas <- expand.grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
)

# define plot_painted_canvas
plot_painted_canvas <- function(canvas, palette = NULL) {
  if (is.null(palette)) {
    palette <- c("#e5ddc8", "#01949a", "#004369", "#db1f48")
  }
  ggplot(data = canvas, aes(x, y, fill = paint)) +
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

# define fractal_art
fractal_art <- function(generator, palette = NULL, ...) {
  canvas <- blank_canvas %>%
    dplyr::mutate(paint = generator(x, y, ...))
  
  plot_painted_canvas(canvas, palette)
}

# define custom generator function
my_generator <- function(x, y, ...) {
  noise <- runif(length(x))
  return(noise)
}

# frequency value for first octave
freq_init <- 100

# number of octaves
octaves <- 6

# seed value
seed <- 98765

# palette colors
palette <- c("#ff0000", "#00ff00", "#0000ff")

# generate and plot fractal art
fractal_art(
  generator = my_generator,
  palette = palette,
  freq_init = freq_init,
  octaves = octaves,
  seed = seed
)

# idea 2 (fun inversion)

# define sample_canva 
sample_canva <- function(seed = NULL) {
  if (!is.null(seed))
    set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

# define blank_canvas
blank_canvas <- expand.grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
)

# define plot_painted_canvas
plot_painted_canvas <- function(canvas, palette = NULL) {
  if (is.null(palette)) {
    palette <- c("#e5ddc8", "#01949a", "#004369", "#db1f48")
  }
  ggplot(data = canvas, aes(x, y, fill = paint)) +
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

# Define the fractal_art function
fractal_art <- function(generator, palette = NULL, ...) {
  canvas <- blank_canvas %>%
    dplyr::mutate(paint = generator(x, y, ...))
  
  plot_painted_canvas(canvas, palette)
}

# define custom generator function for ugly fractal
my_ugly_generator <- function(x, y, freq_init, octaves, seed) {
  # generate random noise with high frequency
  noise <- rnorm(length(x), mean = 0.5, sd = 0.2)
  
  # add multiple octaves with random noise
  for (i in 1:octaves) {
    freq <- freq_init * 2^(i-1)
    # diff seeds for diff octaves
    set.seed(seed + i) 
    noise <- noise + rnorm(length(x), mean = 0, sd = 0.2/freq) * freq
  }
  
  return(noise)
}

# frequency value for first octave
freq_init <- 100

# the number of octaves
octaves <- 6

# the seed value
seed <- 98765

# palette for ugly fractal
palette <- c("#ff00ff", "#00ffff", "#ff0000")

# plot ugly fractal art
fractal_art(
  generator = my_ugly_generator,
  palette = palette,
  freq_init = freq_init,
  octaves = octaves,
  seed = seed
)


# idea 3 (fractal art)

# load in library
library(ggplot2)
library(ggthemes)
library(dplyr)
library(fracture)

# define sample_canva
sample_canva <- function(seed = NULL) {
  if (!is.null(seed))
    set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

# define blank_canvas
blank_canvas <- expand.grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
)

# define plot_painted_canvas
plot_painted_canvas <- function(canvas, palette = NULL) {
  if (is.null(palette)) {
    palette <- c("#e5ddc8", "#01949a", "#004369", "#db1f48")
  }
  ggplot(data = canvas, aes(x, y)) +
    geom_raster(aes(fill = paint), show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

# define fractal_art function with customizable gain and frequency functions
fractal_art <- function(fractal, generator, palette = NULL, ...) {
  canvas <- blank_canvas %>%
    dplyr::mutate(
      paint = fracture::fracture(
        noise = !!generator,
        fractal = !!fractal,
        x = rep(seq(from = 0, to = 1, length.out = 2000), 2000),
        y = rep(seq(from = 0, to = 1, length.out = 2000), each = 2000),
        ...
      )
    )
  
  plot_painted_canvas(canvas, palette)
}

# define my custom generator function
my_generator <- function(x, y, gain, frequency, ...) {
}

# define custom gain function
my_gain <- function(x, y) {
}

# define custom frequency function
my_frequency <- function(x, y) {                                                                      
}

# specify color palette
palette <- c("#ff0000", "#00ff00", "#0000ff")

# plot the fractal art with customized gain and frequency functions
fractal_art(
  fractal = list(gain = my_gain, frequency = my_frequency),
  generator = my_generator,
  palette = palette
)


# 4 gen_scope and gen_gate

gen_sentence <- function() {
  # make my own gen function
  # make a function what will generate sentences
  greetings <- c("Hey", "Howdy", "Sup", "Hi")
  # need a subject
  subjects <- c("earth", "everyone", "there", "people")
  # need a verb
  verbs <- c("is", "are", "seems", "appears")
  # need an adjective
  adjectives <- c("awesome", "crazy", "insane", "amazing")
  # piecing it all together 
  greeting <- sample(greetings, 1)
  subject <- sample(subjects, 1)
  verb <- sample(verbs, 1)
  adjective <- sample(adjectives, 1)
  
  sentence <- paste(greeting, subject, verb, adjective, "!", sep = " ")
  return(sentence)
}
# run the sentence
sentence <- gen_sentence()
cat(sentence)


