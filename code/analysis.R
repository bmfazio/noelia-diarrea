table(data$week)

data %>%
  ggplot(
    aes(x = week,
        y = vload,
        group = paste(id, episode),
        color = diarrhea)) +
  geom_line(size = 0.8) +
  facet_grid(rows = vars(group)) +
   theme(legend.position="bottom")

library(brms)
b