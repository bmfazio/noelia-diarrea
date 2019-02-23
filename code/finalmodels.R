# Modelos finalistas
f1 <- fitfun(vload ~ s(age1) + time_obs + group + (1 + group|id))