library(readxl)
library(tidyverse)
d1 <- read_xlsx("D:/gitrepos/noelia-diarrea/input/Libro1.xlsx",
                col_types = "text")

d1 <-
  d1 %>% mutate_at(-(1:6), as.numeric)

apply(d1, 2, function(x)sum(duplicated(x)))
apply(d1, 2, function(x)length(unique(x)))

# ID: muestra unica
# Nombre: diferentes cepas?
# Genotipo: variedades de una misma cepa?
# Tipo: medidas tomadas en diferentes momentos?

# mes - dia = misma informacion
d1 %>% transmute(duration_month, duration_days, aerps = duration_days/30)

# los limites estan bien
d1 %>%
  mutate(conflict_limit = upper_limit <= lower_limit) %>%
  filter(conflict_limit)


# Exploracion
  # Index vs duracion
d1 %>%
  ggplot(aes(x = Index, y = duration_days)) +
  geom_point()

  # Index vs diarrea
d1 %>%
  group_by(`dia+diarrea`) %>%
  summarise(mIndex = mean(Index))

lm(duration_days ~ Index, data = d1) %>% summary

glm(`dia+diarrea` ~ Index, data = d1, family = binomial) %>% summary