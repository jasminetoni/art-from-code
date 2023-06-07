library(ggplot2)
library(tibble)
library(dplyr)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

sample_data <- function(seed = NULL, n = 100){
  if(!is.null(seed)) set.seed(seed)
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n),
    shape = factor(sample(0:22, size = n, replace = TRUE))
  )
}

polar_styled_plot <- function(data = NULL, palette) {
  ggplot(
    data = data,
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) + 
    coord_polar(clip = "off") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) + 
    scale_colour_gradientn(colours = palette) + 
    scale_size(range = c(0, 10)) + 
    theme_void() + 
    guides(
      colour = guide_none(),
      size = guide_none(),
      fill = guide_none(),
      shape = guide_none()
    )
}

# exercise 3 pt 1

# (1) generate data
set.seed(100)
data <- sample_data(n = 50)

data_avg <- data %>%
  group_by(shade, size) %>%
  summarise(avg_size = mean(size),
            x0 = mean(x0),
            y0 = mean(y0),
            x1 = mean(x1),
            y1 = mean(y1))

# (2) initialize the plot
palette <- sample_canva(seed = 500)
plot <- polar_styled_plot(data = data_avg, palette = palette)

# (3) add ggplot2 geoms
plot <- plot +
  geom_tile(aes(x = x0, y = avg_size, fill = shade), width = 0.1) +
  labs(title = "Polar Styled Plot") +
  theme(plot.title = element_text(hjust = 0.5))

print(plot)

# exercise 3 pt 2

my_styled_plot <- function(data = NULL, palette) {
  ggplot(
    data = data,
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) + 
    coord_polar(clip = "off") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) + 
    scale_colour_gradientn(colours = palette) + 
    scale_size(range = c(0, 10)) + 
    # theme to dark
    theme_dark() +
    theme(
      # giving the plot grid lines
      panel.grid.major = element_line(color = "brown", linetype = "dashed"),
      # no minor grid lines though
      panel.grid.minor = element_blank(),
      # giving the plot a custom title
      plot.title = element_text(color = "dodgerblue3", size = 18, face = "bold")
    ) +
    guides(
      colour = guide_none(),
      size = guide_none(),
      fill = guide_none(),
      shape = guide_none()
    )
}

# generate data
set.seed(100)
data <- sample_data(n = 50)

data_avg <- data %>%
  group_by(shade, size) %>%
  summarise(avg_size = mean(size),
            x0 = mean(x0),
            y0 = mean(y0),
            x1 = mean(x1),
            y1 = mean(y1))

# initialize the plot
palette <- sample_canva(seed = 500)
plot <- my_styled_plot(data = data_avg, palette = palette)

# add ggplot2 geoms
plot <- plot +
  geom_tile(aes(x = x0, y = avg_size, fill = shade), width = 0.1) +
  labs(title = "My Styled Plot") +
  theme(plot.title = element_text(hjust = 0.5))

print(plot)


