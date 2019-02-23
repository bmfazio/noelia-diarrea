# Cosas que me importan examinar:
# - vload: carga viral y su relacion con el resto de variables
# - time_obs: evolucion de la carga viral a traves del tiempo

# Evolucion carga viral
table(data %>% filter(week == 1) %>% pull(group))
data %>%
  ggplot(aes(x = time_obs, y = vload, group = paste(id, episode), color = group)) +
  geom_line() + geom_point()
  # Obs: Hay una alta variabilidad en el nivel inicial de carga viral en todos los grupos evaluados
  # Parece que hay una tendencia a disminuir en magnitud, aunque en realidad los que mas disminuyen
  # son los que han comenzado con una CV elevada y los que comienzan bajo parecen mantenerse o subir
  # un poco ademas de tener una duracion un poco mas corta

  # Idea: ver si la primera observacion tiene informacion suficiente para predecir duracion del
  # episodio

# Duraciones de episodio
data %>% group_by(id, episode) %>% filter(week == max(week)) %>% pull(time_obs) -> dur_ep
# Cargas virales iniciales
data %>% group_by(id, episode) %>% filter(week == 1) %>% pull(vload) -> cvi_in
plot(cvi_in, dur_ep)
  # Obs: no parece habre relacion aparente, pero quizas haya algo luego de ajustar otras variables...

# Carga viral inicial
# Tiempo del año vs carga inicial
data %>%
  filter(week == 1) %>%
  ggplot(aes(x = time1, y = vload)) +
  geom_density2d() + geom_point() + geom_smooth()
data %>% filter(vload > 3) %>% pull(date) %>% sort %>% month %>% table %>% barplot
  # Obs: Parece haber una estacionalidad! Los casos con carga viral mas alta
  # ocurren de marzo a abril
data %>% filter(week == 1) %>% pull(date) %>% sort %>% month %>% table %>% barplot
  # Obs: La incidencia tambien coincide con ese periodo

# Edad vs carga inicial
data %>%
  filter(week == 1) %>%
  ggplot(aes(x = age_mo, y = vload)) +
  geom_density2d() + geom_point() + geom_smooth()
# Obs: Parece haber un incremento alrededor de los 7 meses
# (cambio en practicas de alimentacion?)
# pero un grupo siempre se mantiene bajo
data %>%
  ggplot(aes(x = age_mo, y = vload)) +
  geom_density2d() + geom_point() + geom_smooth()
# Obs: sin restringir a inicial, igual se ve que hay un incremento a la mitad del periodo
data %>%
  filter(week == 1) %>%
  ggplot(aes(x = age_mo, y = vload)) +
  geom_point() + geom_smooth() + facet_grid(rows =  vars(month(date)))
# Obs: hay cierta relacion entre edad y mes de observacion asi que de todas habria que ajustar ahi

# Genogrupo/tipo vs carga inicial
data %>%
  filter(week == 1) %>%
  ggplot(aes(x = group, y = vload)) +
  geom_point()
data %>%
  filter(week == 1) %>%
  ggplot(aes(x = type, y = vload)) +
  geom_point()
  # Obs: la variacion es masiva, no parece haber grandes diferencias

data %>%
  filter(week == 1) %>%
  ggplot(aes(x = date, y = group)) +
  geom_jitter(width = 0, height = 0.05)
  # Obs: no todos los genogrupos aparecen de forma igual a traves del tiempo
  # Esto haria que no sea correcto ajustar por epoca del anio...

# Relacion entre carga viral y diarrea
data %>%
  group_by(id, episode) %>%
  filter(vload == max(vload)) %>%
  ggplot(aes(y = vload, x = diarrhea)) +
  geom_jitter(width = 0.01, height = 0)
  # Obs: los casos con diarrea tienen una carga viral maxima mayor
data %>%
  ggplot(aes(y = vload, x = time_obs, group = paste(id, episode), color = diarrhea)) +
  geom_line()
  # Obs: pareciera que los casos de diarrea comienzan en su pico
data %>%
  filter(time_obs < 20) %>%
  ggplot(aes(y = vload, x = time_obs, group = paste(id, episode), color = group)) +
  geom_line() + geom_point() +
  facet_grid(rows = vars(diarrhea))

# Conclusiones
# >Definitivamente NO ajustar por estacionalidad, porque la data de por si ya restringe esa variable,
# si se incorpora estaria restando efecto de los genogrupos propiamente
# >Ajustar por diarrea 
# >El ajuste por edad si se ve justificado!
# >En los genogrupos no se ve ninguna diferencia grande, pero igual va por ser
# la principal relacion de interes