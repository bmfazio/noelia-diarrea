# Funca!
delta <- 0.99
treed <- 20
ctrl_stan <- list(adapt_delta = delta, max_treedepth = treed)
fitfun <- function(x)brm(x, data = data, control = ctrl_stan)

m2_2<- fitfun(vload ~ s(age1) + time_obs + group + (1|id))

# # table(data$week)
# # 
# # data %>%
# #   ggplot(
# #     aes(x = week,
# #         y = vload,
# #         group = paste(id, episode),
# #         color = diarrhea)) +
# #   geom_line(size = 0.8) +
# #   facet_grid(rows = vars(group)) +
# #    theme(legend.position="bottom")
# 
# 
# # delta <- 0.95
# # ctrl_stan <- list(adapt_delta = delta)
# # fitfun <- function(x)brm(x, data = data, control = ctrl_stan)
# # 
# # # Modelo 1: modelo normal "tradicional"
# # # carga ~ edad + week + (1|paciente) + (1|genotipo)
# # fitfun(vload ~ age_mo + week) -> m1.1
# # fitfun(vload ~ age_mo + week + group) -> m1.2
# # fitfun(vload ~ age_mo + week + (1|id)) -> m1.3
# # fitfun(vload ~ age_mo + week + (1|group)) -> m1.4
# # fitfun(vload ~ age_mo + week + group + (1|id)) -> m1.5
# # fitfun(vload ~ age_mo + week + (1|group) + (1|id)) -> m1.6
# # fitfun(vload ~ age_mo + week + group + diarrhea + (1|id)) -> m1.7
# # 
# # m1 %>% parcoord(., stanpars(.)[1:10])
# # m1 %>% parcoord(., stanpars(.)[1:10], stdize = T)
# # m1 %>% parcoord(., stanpars(.)[-c(1:10,45)], stdize = T)
# # 
# # #P check
# # m1.7 %>% mcmc_areas(as.array(.), stanpars(.)[1:10])
# # #PP checks
# # y <- data$vload
# # yrep <- posterior_predict(m1.7, draws = 500)
# # ppc_violin_grouped(y, yrep, group = data$week)
# 
# # Observaciones: reproduce correctamente la media de los grupos, pero estos
# # parecen mostrar una bimodalidad que el modelo no captura correctamente
# #
# # Considerando que grupo y diarrea (las dos variables categoricas que pense
# # permitirian discernir posibles grupos) no corrigen esa diferencia, creo que
# # vale la pena recurrir a formas no lineales para edad y tiempo de infeccion
# #
# # AUNQUE! Ahora que lo pienso, tambien puede ser importante la fecha real debido
# # al efecto del clima o algun otro asunto estacional
# #
# # Otro asunto: usar la medida previa como "serie de tiempo" tal vez sea relevante
# # pero necesitaria agregar una dummy adicional en el primer dia para distinguir
# # que es una fecha "especial"
# 
# # Modelo 1b: modelo normal "tradicional" + fecha
# # > Vamos a pasar semanas a dias exactos
# # > Colocare fecha de inicio del episodio
# # > Si ademas agrego edad esto va a causar problemas...?
# # ... recuerdo que por ahi lei como lidiar con el tema de tiempos:
# # calendario vs lapso vs edad (efecto "cohorte" y cosas asi)
# # > Pero para efectos de este periodo tan corto, creo que seria practicamente igual
# # tratar la edad como fija segun el inicio de la infeccion
# 
# # Para cada episodio, fijar:
# # - Edad en la primera semana de observacion
# # - Fecha en la primera semana de observacion
# # Y transformar:
# # - Semana -> tiempo transcurrido desde la primera observacion
# # (el lag de vload ya lo examine con la funcion acf() y no pasa nada...
# # ... porque la data no esta ordenada LOL, pero igual ahora que lo pienso
# # seria muy dificil observar algo consistente con tan pocas observaciones,
# # lo metere en el modelo y ya)
# 
# 
# 
# # m0.1 <- fitfun(vload ~ diarrhea + (1|id))
# # m0.2 <- fitfun(vload ~ group + diarrhea + (1|id))
# # m1.3 <- fitfun(vload ~ age_mo + week + (1|id))
# # m1.5 <- fitfun(vload ~ age_mo + week + group + (1|id))
# # m1.7 <- fitfun(vload ~ age_mo + week + group + diarrhea + (1|id))
# # m1b.3 <- fitfun(vload ~ age1 + group + diarrhea + (1|id))
# # 
# # save(m0.1, m0.2, m1.3, m1.5, m1.7, m1b.1, m1b.3, file = "modelitos.RDS")
# # load("modelitos.RDS")
# 
# #PP checks
# # y <- data$vload
# # yrep <- posterior_predict(m1b.1, draws = 500)
# # ppc_violin_grouped(y, yrep, group = data$week)
# # yrep <- posterior_predict(m1b.3, draws = 500)
# # ppc_violin_grouped(y, yrep, group = data$week)
# # 
# # mcmc_areas(as.array(m0.1), stanpars(m0.1)[2:10])
# # mcmc_areas(as.array(m0.2), stanpars(m0.2)[2:10])
# # mcmc_areas(as.array(m1b.1), stanpars(m1b.1)[2:10])
# # mcmc_areas(as.array(m1b.1), stanpars(m1b.1)[c(2,4)])
# # 
# # loo(m0.1, m0.2, m1.3, m1.5, m1.7, m1b.1, m1b.3)
# 
# # Conclusion: despues de ver el LOO, todos los modelos son indiscernibles
# # No parece haber un "efecto de cohorte" con el tiempo simple (time1)
# # Pendiente incorporar estacion del año... aunque eso no deberia influir directamente en VL
# # OK, lo ignorare. (igual el estimado era el cero mas exacto ever)
# # ---
# # Lo que puedo decir con cierta confianza:
# # - La edad no parece importar mucho
# # - La diarrea esta asociada a un incremento, aunque no se si deberia incluirlo
# # (diarrea no "causa" carga viral... haz tu DAG!)
# # - Lo mas consistente es time_obs, o sea que con el tiempo baj la carga pero duhhh
# 
# # Modelo 2: incorporar splines
# # Con los ajustes anteriores, darle splenitud a time_obs a ver si ajuste mas boni
# 
# m1b.1 <- fitfun(vload ~ age1 + time1 + time_obs + group + diarrhea + (1|id))
# # Saca diarrea por causalidad:
# m1b.2 <- fitfun(vload ~ age1 + time1 + time_obs + group + (1|id))
# mcmc_areas(as.array(m1b.1), stanpars(m1b.1)[c(2,4:7,9:10)]) + coord_cartesian(xlim = c(-3, 3))
# mcmc_areas(as.array(m1b.2), stanpars(m1b.2)[c(2,4:9)]) + coord_cartesian(xlim = c(-3, 3))
#   # OK, hay un cambio masomenos importante para grupo2 cuando tomamos en cuenta diarrea
#   # Esencialmente, la decision viene en terminos de pensar si queremos describir
#   # "carga viral" como comportamiento global del grupo o enfasis en diarrea vs no diarrea
# # Aplica spline
# m2.1 <- fitfun(vload ~ s(age1) + s(time_obs) + group + (1|id))
# posterior_interval(m2.1)[1:10,]
# mcmc_areas(as.array(m2.1), stanpars(m2.1)[1:15])
# marginal_effects(m2.1)
# marginal_smooths(m2.1)
# 
# # Parece que no es necesario hacer smoothing con time_obs, quizas si retiro eso
# # deje de tener problemas de divergencias?
# m2.2 <- fitfun(vload ~ s(age1) + time_obs + group + (1|id))
# marginal_effects(m2.2)
# # Nop, igual da problema
# 
# # Cuadradito?
# m2.3 <- fitfun(vload ~ poly(age1,2) + time_obs + group + (1|id))
# marginal_effects(m2.3)
# 
# # MAS PODER!
# m2.4 <- fitfun(vload ~ poly(age1,3) + poly(time_obs,2) + group + (1|id))
# marginal_effects(m2.4)
# 
# # Ok, comparando con el smooth, da resultados bastante similares y sin divergencias.
# 
# # Predictivamente?
# loo(m2.1, m2.2, m2.3, m2.4)
# y <- data$vload
# yrep1 <- posterior_predict(m2.2, draws = 500)
# yrep2 <- posterior_predict(m2.3, draws = 500)
# yrep3 <- posterior_predict(m2.4, draws = 500)
# ppc_violin_grouped(y, yrep1, group = data$week)
# ppc_violin_grouped(y, yrep2, group = data$week)
# ppc_violin_grouped(y, yrep3, group = data$week)
# 
# # Todos se comportan de forma practicamente identica, y todos fracasan similarmente
# # al momento de reproducir la bimodalidad de las cargas virales!
# 
# # Un izi para revisar el codigo stan
# m2b <- fitfun(vload ~ s(age1))
# marginal_effects(m2b)
# stancode(m2b)
# 
# # Otra idea que se me acaba de ocurrir:
# # > Modificar la "ruta" del spline mediante interaccion con... diarrea?
# m2c <- fitfun(vload ~ s(age1) + s(time_obs, by = diarrhea) + group + diarrhea + (1|id))
# # m2d <- fitfun(vload ~ s(age1) + s(time1) + time_obs + group + (1|id))
# # m2e <- fitfun(vload ~ s(age1) + s(time1, by = diarrhea) + time_obs + group + diarrhea + (1|id))
# 
# # Modelo 3: Proceso gaussiano
# # 3.1: solo por curioso, ver si gp funciona mejor que smooth para el efecto de edad
# m3.1 <- fitfun(vload ~ gp(age1) + time_obs + group + (1|id))
# # > OK, demora mucho mas en hacer el fit asi que mejor no :d
# 
# # 3.2: aqui si me parece mas relevante, permitir que cada tiempo sea un poco diferente para
# # recuperar "la curva"
# # NO CORRE! :c
# #m3.2 <- fitfun(vload ~ s(age1) + gp(time_obs) + group + (1|id))
# 
# # 3.3: agrego diarrea y permito que el curso sea diferente porque eso le fue bien al m2c
# # m3.3 <- fitfun(vload ~ s(age1) + gp(time_obs, by = diarrhea) + group + diarrhea + (1|id))
# # Full divergencia D:
# 
# ###
# # 4: extiendo el m2c que funciono ien con un smooth para tiempo1 porque de repeeente ahora si
# # se ve el "efecto cohorte"
# m4 <- fitfun(vload ~ s(age1) + s(time_obs, by = diarrhea) + s(time1) + group + diarrhea + (1|id))
# 
# # OH! La curva para time1 resulta verse identica a la de edad... y la de edad ahora esta plana?!
# # Mi teoria es que time1 representa el "pasar del tiempo" tan bien como age1 cuando se ajustan ambos
# # Si quiero que time1 represente algo diferente entonces deberia codificarlo como categoria...
# 
# # y <- data$vload
# # yrep1 <- posterior_predict(m2.2, draws = 500)
# # yrep2 <- posterior_predict(m2.3, draws = 500)
# # yrep3 <- posterior_predict(m2.4, draws = 500)
# # yrep4 <- posterior_predict(m4, draws = 500)
# # yrep5 <- posterior_predict(m2c, draws = 500)
# # ppc_violin_grouped(y, yrep1, group = data$week)
# # ppc_violin_grouped(y, yrep2, group = data$week)
# # ppc_violin_grouped(y, yrep3, group = data$week)
# # ppc_violin_grouped(y, yrep4, group = data$week)
# # ppc_violin_grouped(y, yrep5, group = data$week)
# 
# ###
# # Conclusion: los GP me estan dando problemas, mucha divergencia
# 
# # Se me ocurre que hay demasiada variabilidad en el genogrupo por temas del
# # mismo sujeto asi que decidi
# m5 <- fitfun(vload ~ s(age1) + s(time_obs) + group + (1 + group|id))
#   # Al "relajar" el efecto de group he dejado de tener problemas de divergencias!
#   # Deberia analizar (por self-learning) que cosas de los diagnosticos
#   # para ver si habia algo que sugieriese que este era el camino correcto
# 
# m6 <- fitfun(vload ~ s(age1) + time_obs + group + (1 + group|id))
#   # Si retiro el spline para time_obs, su efecto se mantiene casi identico
#   # excepto hacia el final, que dejar de irse hacia arriba
#   # FALSOOooo
# 
# m7 <- fitfun(vload ~ s(age1) + time_obs + group + (1 + time_obs + group|id))
#   # Agregar la variabilidad por grupo no genera gran aporte y genera problemas
#   # de convergencia
# 
# # Ahora probare nested groups con tipo y grupo... aunque viendo conteos,
# # dudo que salga algo muy util de ahi
# m8 <- fitfun(vload ~ s(age1) + time_obs + (1 + time_obs|id) + (1|group/type))
# 
# # Accidentalmente corri esto y se fue el problema de m7?!
# # Definitivamente quiero entender como es que estas vaias lokaz permiten
# # mas estabilidad
# ###m8 <- fitfun(vload ~ s(age1) + time_obs + (1 + time_obs|id) + (1|group/type))
# # ... nah, aun diverge ocasionalmente, solo fue azarts
# 
# # Fitting issues still:
# #m2c<- fitfun(vload ~ s(age1) + s(time_obs, by = diarrhea) + group + diarrhea + (1|id))
# #m5 <- fitfun(vload ~ s(age1) + s(time_obs) + group + (1 + group|id))
# #m8 <- fitfun(vload ~ s(age1) + time_obs + (1 + time_obs|id) + (1|group/type))
# 
