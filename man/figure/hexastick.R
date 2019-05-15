library(ggplot2)
library(png)
library(grid)
library(hexSticker)

n_steps <- 60
y_min <- 0.95
y_max <- 1.35
x_min <- -0.07
x_max <- 1.5

img <- readPNG("max.png")
g_img <- rasterGrob(img, width = 1, x = 0.48, interpolate = TRUE)
ys <- seq(y_min, y_max, length.out = n_steps + 1)
alpha_steps <- seq(from = 0, to = 0.5, length.out = n_steps)
trans_df <- data.frame(xmin = x_min, xmax = x_max, ymin = ys[-length(ys)],
                       ymax = ys[-1], alpha = alpha_steps)
gg <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1.5, ymin = 0, ymax = 1.5), fill = NA) +
  annotation_custom(g_img, xmin = -0.1) + coord_fixed() +
  theme_void() + guides(alpha = FALSE)

col_bg <- "#ffffff"
col_border <- "#000000"
col_text <- "#000000"
fac <- 1.3
sticker(gg, package="EstimationTools", p_size = 15, s_x = 1.03, s_y = 1.08, s_width = 2.6/fac,
        s_height = 2.2/fac, p_color = col_text, h_fill = col_bg, p_family = ,
        spotlight = F, p_x = 1, p_y = 0.6, l_x = 1, l_y = 1.5, l_width = 3, l_height = 3,
        h_color = col_border, filename="ETLogo.png", u_color = col_border)
