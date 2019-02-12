options(encoding = "utf8")
source(here::here("code", "setup.R"))
source(here("code","load.R"))
source(here("code","analysis.R"))

# evaluar nesteado: genotipo dentro de genogrupo
# la carga esta en log10, usar una dist normal para predecir seria "absurdo" si intervalos cruzan 1, pero ya we, luego vemos eso