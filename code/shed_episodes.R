ndata %>%
  filter(genotip != 99) %>%
  select(genotip) %>%
  table %>%
  sort

nshed %>%
  select(Genotipo) %>%
  table %>%
  sort