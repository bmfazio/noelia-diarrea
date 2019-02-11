library(here)
library(naniar)
library(readxl)
library(tidyverse)

ndata <- read_xlsx(
  here("in", "NC BASE DE DATOS_CALEND_EPISODES_FOR_SHEDDING_LEYENDA.xlsx"),
  sheet = "calend") %>%
  replace_with_na_all(condition = ~.x == 99) %>%
  select(-month, -year, -cod_nv)

nshed <- read_xlsx(
  here("in", "NC BASE DE DATOS_CALEND_EPISODES_FOR_SHEDDING_LEYENDA.xlsx"),
  sheet = "episodes_for_shedding")
