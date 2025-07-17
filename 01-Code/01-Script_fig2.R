library(tidyverse)
library(lubridate)

data <- read_csv("00-Data/figure2_data.csv", show_col_types = FALSE)

data_clean <- data %>%
  separate(date, into = c("year", "month", "day"), sep = "-", convert = TRUE) %>%
  mutate(edate = make_date(year, month, day))

data_long <- data_clean %>%
  pivot_longer(
    cols = c(p_cosp, p_comp),
    names_to = "belief_type",
    values_to = "proportion"
  )

figure2 <- ggplot(data_long, aes(x = edate, y = proportion, color = belief_type, linetype = belief_type)) +
  geom_smooth(method = "loess", span = 0.6, se = FALSE, linewidth = 1) +
  scale_color_manual(
    values = c("p_cosp" = "darkgrey", "p_comp" = "black"),
    labels = c("p_cosp" = "Derogatory", "p_comp" = "Non-derogatory")
  ) +
  
  scale_linetype_manual(
    values = c("p_cosp" = "solid", "p_comp" = "dashed"),
    labels = c("p_cosp" = "Derogatory", "p_comp" = "Non-derogatory"),
  ) +
  
  labs(
    x = "Date",
    y = "Proportion",
    color = "Legend",
    linetype = "Legend",
  ) +
  theme_minimal()


figure2

ggsave("02-Plots/Figure 2.png", plot = figure2, width = 10, height = 6)
