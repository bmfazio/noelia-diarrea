source("in/load.R")
library(lubridate)
row_window <- function(data, var, size){
  row_match <- which(!is.na(data[,var]))
  data[unique(as.vector(sapply(row_match, `+`, -size:size))),]
}

# Deberíamos poder reproducir los intervalos mostrados en episodes_for_shedding

  # Ver los episodios mas largos:
nshed %>% arrange(-duration_days)
  # Examinar PX205
ndata %>%
  filter(codigo == "PX205") %>%
  row_window("genotip", 2)
  # tomando fecha exacta de muestra no es:
ymd("2009-05-25") - ymd("2009-06-23")
  # tomando fechas de medidas anterior y posterior si es:
ymd("2009-05-18") - ymd("2009-0628")
# (pero de donde sale el decimal?)

ndata %>%
  transmute(infec = genotip)