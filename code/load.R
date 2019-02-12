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
  mutate(diarrhea = as.logical(diarrhea))