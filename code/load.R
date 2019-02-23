data <- read_xlsx(
  here("in", "datos para curso de carga viral.xlsx")) %>%
  replace_with_na_all(condition = ~.x == 99) %>%
  select(-month, -year, -cod_nv, -`Categoria de edad`,
         -genotip, -dia_diarrea2, -Cq) %>%
  select(id = codigo,
         week = `Número de semanas`,
         vload = `log(copias/ul)`,
         type = `número genotipo`,
         group = `número genogrupo`,
         date = fec_vig,
         episode = `#Episodio durante 24 meses`,
         age_mo = edadmeses,
         diarrhea = `Diarrea para curso de la infección`,
         norov1 = nov_g1, norov2 = nov_g2, rotav = rota) %>%
### Corregir error
  mutate(
    date =
      case_when(
        id == "PX087" & week == "1" ~ date - days(30),
        TRUE ~ date)
    ) %>%
### ---
  mutate(diarrhea = as.logical(diarrhea),
         group = as.character(group),
         type = as.character(type),
         time_study = as.integer(date - min(date))/86400)

# Crear nuevas variables
data <- data %>%
  left_join(
    data %>%
      filter(week == 1) %>%
      transmute(
        time1 = time_study,
        age1 = age_mo,
        id = id, episode = episode),
    by = c("id", "episode")
  ) %>%
  mutate(
    time_obs = time_study - time1,
    vload_lag = ifelse(week != 1, lag(vload), 0),
    initialt = as.numeric(vload_lag == 0)
  )

### Genotable
genodata <- read_xlsx(
  here("in", "datos para curso de carga viral.xlsx")) %>%
  replace_with_na_all(condition = ~.x == 99) %>%
  select_at(6:8) %>%
  group_by_at(1:3) %>%
  mutate(nobs = n()) %>%
  unique %>%
  arrange_at(3:2) %>%
  filter(!is.na(genotip))