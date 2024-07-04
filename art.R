library(tidyverse)
library(plotly)
library(svglite)

# From FAOSTAT food balance sheets. Data for year 2021.
# Animal and vegetal protein g/capita/day and population for all countries.
art_data <- read_csv("data_raw/world.csv")

art_data_wide <- art_data %>%
  pivot_wider(id_cols = c(Area, Year), values_from = Value, names_from = Item)

home_art <- art_data_wide %>%
  ggplot(
    aes(`Animal Products`, `Vegetal Products`, color = Area, size = Population)
  ) +
  geom_point(show.legend = F, alpha = 0.5, stroke = 0) +
  scale_x_reverse() +
  scale_y_reverse() +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #panel.border = element_blank(),
    #panel.grid = element_blank(),
    #axis.ticks = element_blank(),
    #axis.ticks.length = unit(0, "cm"),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank()
  )

ggsave("home_art.svg", plot = home_art, device = "svg", bg = "transparent")

art_data2 < art_data %>%
  filter

home_art2 <- art_data_wide %>%
  ggplot(
    aes(`Vegetal Products`)
  ) +
  geom_freqpoly(show.legend = F, alpha = 0.5) +
  #scale_x_reverse() +
  #scale_y_reverse() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    #panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

home_art2

  
