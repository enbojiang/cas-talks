library(tidyverse)
library(locktonr)
library(ggthemes)
setwd("C:/Users/Enbo.Jiang.LOCKTON/OneDrive - Lockton/_PERSONAL/Talks/CAS-Spring-2024")

colorspace::hcl_palettes(type = "Sequential", plot = TRUE)
colorspace::hcl_palettes(type = "Diverging", plot = TRUE)
colorspace::hcl_palettes(type = "Qualitative", plot = TRUE)
glimpse(mpg)

# ---- original ----
plt1 <- ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Highway Miles Per Gallon vs. Engine Displacement")
plt1
ggsave("plots/plt1.jpg", plt1, width = 30, height = 15, unit = "cm")

# ---- intentional aesthetics ----
## ---- transparency ----
plt2 <- ggplot(mpg, aes(displ, hwy)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth() + 
  ggtitle("Highway Miles Per Gallon vs. Engine Displacement")
plt2
ggsave("plots/plt2.jpg", plt2, width = 30, height = 15, unit = "cm")

## ---- position ----
set.seed(0)
plt3 <- ggplot(mpg, aes(displ, hwy)) + 
  geom_jitter(alpha = 0.3) + 
  geom_smooth() + 
  ggtitle("Highway Miles Per Gallon vs. Engine Displacement")
plt3
ggsave("plots/plt3.jpg", plt3, width = 30, height = 15, unit = "cm")

# ---- descriptive labels and title ----
set.seed(0)
plt4 <- ggplot(mpg, aes(displ, hwy)) + 
  geom_jitter(alpha = 0.3) + 
  geom_smooth() +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind()

plt4
ggsave("plots/plt4.jpg", plt4, width = 30, height = 15, unit = "cm")

# ---- remove distractions ----
set.seed(0)
plt5 <- ggplot(mpg, aes(displ, hwy)) + 
  geom_jitter(alpha = 0.3) + 
  geom_smooth() +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(plot.background = element_blank())
plt5
ggsave("plots/plt5.jpg", plt5, width = 30, height = 15, unit = "cm")

# ---- simpler trend line ----
set.seed(0)
plt6 <- ggplot(mpg, aes(displ, hwy)) + 
  geom_jitter(alpha = 0.3) + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(plot.background = element_blank())
plt6
ggsave("plots/plt6.jpg", plt6, width = 30, height = 15, unit = "cm")

# ---- increase data density ----
## ---- distinguish drive using color and shape ----
set.seed(0)
plt7 <- mpg |>
  mutate(
    "Drive" = case_when(
      drv == "4" ~ "4WD",
      drv == "f" ~ "Front",
      TRUE ~ "Rear"
      )
    ) |>
  ggplot(aes(displ, hwy)) + 
  geom_jitter(aes(color = Drive, shape = Drive)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(
    aes(x, y, label = text, color = text), 
    data = tibble::tibble(x = c(2.5, 3.5, 6), y = c(35, 15, 30), text = c("Front", "4WD", "Rear"))
    ) +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(
    plot.background = element_blank(),
    legend.position = "none"
    )
plt7
ggsave("plots/plt7.jpg", plt7, width = 30, height = 15, unit = "cm")

## ---- distinguish class using facets ----
set.seed(0)
plt8 <- mpg |>
  mutate(
    "Drive" = case_when(
      drv == "4" ~ "4WD",
      drv == "f" ~ "Front",
      TRUE ~ "Rear"
    )
  ) |>
  ggplot(aes(displ, hwy)) + 
  geom_jitter(aes(color = Drive, shape = Drive)) + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(plot.background = element_blank()) +
  facet_wrap(vars(class))
plt8
ggsave("plots/plt8.jpg", plt8, width = 30, height = 15, unit = "cm")

## ---- order by median hwy mpg ----
set.seed(0)
plt9 <- mpg |>
  mutate(
    "Drive" = case_when(
      drv == "4" ~ "4WD",
      drv == "f" ~ "Front",
      TRUE ~ "Rear"
    ),
    "class" = fct_reorder(class, hwy, median)
  ) |>
  ggplot(aes(displ, hwy)) + 
  geom_jitter(aes(color = Drive, shape = Drive)) + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(plot.background = element_blank()) +
  facet_wrap(vars(class))
plt9
ggsave("plots/plt9.jpg", plt9, width = 30, height = 15, unit = "cm")

# ---- labels ----
## ---- add static labels for some ----
set.seed(0)
plt10 <- mpg |>
  mutate(
    "Drive" = case_when(
      drv == "4" ~ "4WD",
      drv == "f" ~ "Front",
      TRUE ~ "Rear"
    ),
    "class" = fct_reorder(class, hwy, median),
    "car" = ifelse(lhs::randomLHS(n(), 1) > 0.1, "", model) #paste(model, year, trans))
  ) |>
  ggplot(aes(displ, hwy)) + 
  geom_jitter(aes(color = Drive, shape = Drive)) + 
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(
    aes(label = car),
    max.overlaps = Inf,
    box.padding = 1.0,
    color = "#CC79A7"
  ) + 
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(plot.background = element_blank()) +
  facet_wrap(vars(class))
plt10
ggsave("plots/plt10.jpg", plt10, width = 30, height = 15, unit = "cm")

## ---- make the plot interactive with dynamic labels for all ----
set.seed(0)
plt11 <- mpg |>
  mutate(
    "Drive" = case_when(
      drv == "4" ~ "4WD",
      drv == "f" ~ "Front",
      TRUE ~ "Rear"
    ),
    "class" = fct_reorder(class, hwy, median),
    "car" = paste(manufacturer, model, year, trans, fl, sep = "\n")
  ) |>
  ggplot(aes(displ, hwy)) + 
  ggiraph::geom_jitter_interactive(aes(color = Drive, shape = Drive, tooltip = car)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Engine Displacement") +
  ylab("Highway Miles Per Gallon") + 
  ggtitle("Fuel Efficiency Generally Decreases as Engine Displacement Increases") + 
  scale_color_colorblind() + 
  theme_clean() + 
  theme(plot.background = element_blank()) +
  facet_wrap(vars(class))
plt11
ggiraph::girafe(ggobj = plt11, width_svg = 16, height_svg = 9) |>
  htmltools::save_html("plots/plt11.html")


# ---- communicating heavy-tailed distribution ----
one <- tibble::tibble(
  loss = rlnorm(1000, 12.77578979, 0.832554611)
  )
two <- tibble::tibble(
  loss = rlnorm(1000, 13.0107916, 0.472380727)
)

one_ecdf_fn <- ecdf(one$loss)
two_ecdf_fn <- ecdf(two$loss)

one <- one |> mutate(cdf = map_dbl(loss, one_ecdf_fn), "class" = "one")
two <- two |> mutate(cdf = map_dbl(loss, two_ecdf_fn), "class" = "two")

plt12 <- bind_rows(one, two) |>
  ggplot(aes(loss, color = class)) + 
  geom_density() +
  scale_y_continuous(
    labels = scales::scientific_format()
  ) + 
  scale_x_continuous(
    labels = function(l) {paste0("$", round(l/1e6, 1), "m")}
  ) + 
  theme_clean() + theme(plot.background = element_blank()) +
  scale_color_colorblind() + 
  guides(color = "none") + 
  ggtitle("A")
plt12
ggsave("plots/plt12.jpg", plt12, width = 12, height = 16, unit = "cm")

plt13 <- bind_rows(one, two) |>
  ggplot(aes(loss, color = class)) + 
  geom_density() +
  scale_y_continuous(
    labels = scales::scientific_format(),
    transform = "log"
    ) + 
  scale_x_continuous(
    labels = function(l) {paste0("$", round(l/1e6, 1), "m")}
  ) + 
  ylab("log density") +
  theme_clean() + theme(plot.background = element_blank()) +
  scale_color_colorblind() + 
  guides(color = "none") +
  ggtitle("B")
plt13
ggsave("plots/plt13.jpg", plt13, width = 12, height = 16, unit = "cm")

# bind_rows(one, two) |>
#   ggplot(aes(loss, class)) + 
#   ggridges::geom_density_ridges(fill = "blue", color = "white", alpha = 0.5) + 
#   scale_x_continuous(
#     labels = function(l) {paste0("$", round(l/1e6, 1), "m")}
#   ) +
#   theme_clean() + theme(plot.background = element_blank())

plt14 <- bind_rows(one, two) |>
  ggplot(aes(loss, cdf, color = class)) + geom_line() + 
  theme_clean() + theme(plot.background = element_blank()) +
  scale_color_colorblind() +
  scale_x_continuous(
    labels = function(l) {paste0("$", round(l/1e6, 1), "m")}
  ) + 
  guides(color = "none") +
  ggtitle("C")
plt14
ggsave("plots/plt14.jpg", plt14, width = 12, height = 16, unit = "cm")

plt15 <- bind_rows(one, two) |>
  ggplot(aes(cdf, loss, color = class)) + geom_line() + 
  theme_clean() + theme(plot.background = element_blank()) + 
  scale_color_colorblind() + 
  scale_y_continuous(
    labels = function(l) {paste0("$", round(l/1e6, 1), "m")}
  ) + 
  guides(color = "none") +
  ggtitle("D")
plt15
ggsave("plots/plt15.jpg", plt15, width =12, height = 16, unit = "cm")

