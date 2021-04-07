total_scores <- read_rds("data/05-total-scores.rds")

breaks <- seq(0, 1.00, length.out = 5) # lenght.out = 5 gives a 4-by-4 grid
xlabels <- scales::number
ylabels <- scales::number

shapes <- c(
  "Banking" = 1,
  "Consumer electronics" = 13,
  "Energy provider" = 2,
  "Energy storage" = 6,
  "Finance" = 0,
  "Healthcare" = 8,
  "Housing" = 5,
  "Manufacturing" = 3,
  "Tech" = 7
)

# remove points if they have no assessment (NA) in one of the axes

p_shape <-
  total_scores %>%
  filter(!(is.na(Agility) | is.na(Intelligence))) %>%
  ggplot() +
  aes(x = Agility, y = Intelligence, shape = sector) +
  geom_point(size = 5) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(limits = c(0,1), breaks = breaks, labels = xlabels) +
  scale_y_continuous(limits = c(0,1), breaks = breaks, labels = ylabels) +
  # scale_color_brewer(palette = "BrBG") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave("images/landscape_shape.pdf", p_shape, width = 9, height = 6)

p_color <-
  total_scores %>%
  filter(!(is.na(Agility) | is.na(Intelligence))) %>%
  ggplot() +
  aes(x = Agility, y = Intelligence, colour = sector, size = size) +
  geom_point() +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(limits = c(0,1), breaks = breaks, labels = xlabels) +
  scale_y_continuous(limits = c(0,1), breaks = breaks, labels = ylabels) +
  scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave("images/landscape_color.pdf", p_color, width = 9, height = 6, dpi = 300, scale = 1)
ggsave("images/landscape_color.png", p_color, width = 16, height = 9, dpi = 300, scale = .55)
ggsave("images/landscape_color_square.png",
       p_color + theme(legend.position = "top", legend.title = element_blank()
                       legend.key.size = unit(2, "point")) +
                 guides(colour = guide_legend(nrow = 3), size = "none"),
       width = 5, height = 5, dpi = 300, scale = 1)

# p_color <-
total_scores %>%
  filter(!(is.na(Agility) | is.na(Intelligence))) %>%
  # mutate(xydistance = ) +
  ggplot() +
  aes(x = Agility, y = Intelligence, colour = sector, size = size) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = .25) +
  geom_point(size = 5) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(limits = c(0,1), breaks = breaks, labels = xlabels) +
  scale_y_continuous(limits = c(0,1), breaks = breaks, labels = ylabels) +
  scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
