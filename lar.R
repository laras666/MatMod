eddypro=read.csv("eddypro.csv", skip = 1)[-1, c(1:69, 71:77)] 
eddypro = eddypro[ , c(-32, -33, -37, -38, -63)] 
eddypro$daytime = as.double(eddypro$daytime) 
for(i in c(4, 6:length(eddypro))){ 
  eddypro[, i] = as.double(as.vector(eddypro[, i])) 
} 
eddypro[eddypro == -9999] = NA 
eddypro[884:2307, ] -> eddypro 
eddypro[eddypro$daytime == "4", ] -> eddypro 
eddypro2=na.exclude(eddypro) 
cor.matrix = round(cor(eddypro2[, c(-1, -2, -3, -4)]), 2)
model1=lm(RH~(rand_err_Tau+H+rand_err_H+LE+qc_LE+rand_err_LE+h2o_flux+co2_molar_density+co2_mole_fraction)^9,data=eddypro)
