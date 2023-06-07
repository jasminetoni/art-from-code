library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)
library(tictoc)

curl_data <- function(
    data, 
    iterations = 50,
    step_size = .001,
    ...
) {
  
  update <- function(current_state, iteration, ...) {
    curl <- curl_noise(
      x = current_state$x, 
      y = current_state$y,
      generator = fracture,
      ...
    )
    next_state <- current_state |>
      mutate(
        x = x + curl$x * step_size,
        y = y + curl$y * step_size,
        time = time + 1
      )
    return(next_state)
  }
  
  data |> 
    mutate(id = row_number(), time = 1) |>
    accumulate(1:iterations, update, .init = _, ...) |>
    bind_rows()
}

curl_art <- function(...) {
  curl_data(...) |> 
    ggplot(aes(x, y, group = id)) + 
    geom_path() +
    theme_void() + 
    coord_equal() 
}

custom_curl_data <- function(data) {
  curl_data(
    data = data,
    iterations = 80, 
    octaves = 10,
    fractal = ridged,
    noise = gen_cubic,
    freq_init = 1,
    frequency = ~ . * 1.2,
    gain_init = 1,
    gain = ~ . * .9,
    seed = 1
  )
}

circle <- function(n = 100) {
  tibble(
    theta = 2 * pi * (1:n) / n, 
    x = cos(theta),
    y = sin(theta)
  )
}

dat1 <- circle(5000) |> 
  custom_curl_data()

dat2 <- circle(5000) |>
  mutate(x = x * .99, y = y * .99) |>
  custom_curl_data()

pic <- ggplot(mapping = aes(x, y, group = time)) +
  geom_polygon(data = dat1, fill = "#ffffff10") +
  geom_polygon(data = dat2, fill = "#22222205") +
  theme_void() +
  coord_equal()

tic()
ggsave(
  filename = here("output", "curl-art-2.png"), 
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "white"
)
toc()

# for part one of spatial exercise 5:
# curl-art-1.R took 1.802 seconds to render and curl-art-2.R took 0.421 seconds to render. 
# smoother code below

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)
library(tictoc)
library(magrittr)

# Define the curl_data function
curl_data <- function(
    data,
    iterations = 50,
    step_size = 0.01,
    noise = gen_simplex,
    fractal = fbm,
    octaves = 1,
    freq_init = 1,
    ...
) {
  update <- function(current_state, iteration, ...) {
    curl <- curl_noise(
      x = current_state$x,
      y = current_state$y,
      generator = noise,
      ...
    )
    next_state <- mutate(
      current_state,
      x = x + curl$x * step_size,
      y = y + curl$y * step_size,
      time = time + 1
    )
    return(next_state)
  }
  
  data <- mutate(data, id = row_number(), time = 1)
  accum <- data
  for (i in 1:iterations) {
    accum <- update(accum, i)
    data <- bind_rows(data, accum)
  }
  return(data)
}

# Define the number of iterations
iterations <- 5  

# Apply the update function iteratively using a loop
result <- data %>%
  mutate(id = row_number(), time = 1) %>%
  curl_data(iterations = iterations)

# Define the curl_art function
curl_art <- function(...) {
  curl_data(...) %>%
    ggplot() +
    geom_path(aes(x = x, y = y, group = id)) +
    theme_void() +
    coord_equal()
}

# Create a small grid
smol_grid <- long_grid(x = seq(1, 20, by = 0.1), y = seq(1, 20, by = 0.1))

# Generate the curl art
pic <- smol_grid %>%
  mutate(x = normalise(x), y = normalise(y)) %>%
  curl_art(noise = gen_simplex, fractal = fbm)

# Save the output image
tic()
ggsave(
  filename = here("output", "curl-art-1.png"),
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "white"
)
toc()

# exercise 5 part 3

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)
library(tictoc)
library(magrittr)

# Define the curl_data function
curl_data <- function(
    data,
    iterations = 50,
    step_size = 0.01,
    noise = gen_simplex,
    fractal = fbm,
    octaves = 1,
    freq_init = 1
) {
  update <- function(current_state, iteration) {
    curl <- curl_noise(
      x = current_state$x,
      y = current_state$y,
      generator = noise
    )
    next_state <- mutate(
      current_state,
      x = x + curl$x * step_size,
      y = y + curl$y * step_size,
      time = time + 1
    )
    return(next_state)
  }
  
  data <- mutate(data, id = row_number(), time = 1)
  accum <- data
  for (i in 1:iterations) {
    accum <- update(accum, i)
    data <- bind_rows(data, accum)
  }
  return(data)
}

# Define the number of iterations
iterations <- 100  # Increase number of iterations for more complex patterns

# Define custom parameters
custom_parameters <- list(
  noise = gen_simplex,  # Use simplex noise generator
  fractal = fbm,        # Use fbm fractal (replace with the desired fractal)
  octaves = 3,          # Increase the number of octaves for more detail
  freq_init = 0.5       # Adjust the initial frequency
)

# Apply the update function iteratively using a loop
result <- data %>%
  mutate(id = row_number(), time = 1) %>%
  curl_data(iterations = iterations, noise = custom_parameters$noise,
            fractal = custom_parameters$fractal, octaves = custom_parameters$octaves,
            freq_init = custom_parameters$freq_init)

# Define a custom palette
custom_palette <- c("#FF0000", "#00FF00", "#0000FF")  # Use red, green, and blue colors

# Define the curl_art function with custom palette
curl_art <- function(data) {
  unique_ids <- unique(data$id)
  num_ids <- length(unique_ids)
  palette_size <- max(num_ids, length(custom_palette))
  custom_palette <- custom_palette[1:palette_size]
  
  ggplot(data) +
    geom_path(aes(x = x, y = y, group = factor(id), color = factor(id)), size = 1) +  # Convert id to a factor
    theme_void() +
    coord_equal() +
    scale_color_manual(values = custom_palette)  # Set custom color palette
}

# Generate the customized curl art
pic <- curl_art(data = result)

# Save the output image
tic()
ggsave(
  filename = here("output", "custom-curl-art.png"),  # Set a custom filename
  plot = pic,
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  bg = "white"
)
toc()


