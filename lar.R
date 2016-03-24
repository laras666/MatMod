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
model1
summary(model1)
model2=lm(RH~(LE+rand_err_LE+h2o_flux+rand_err_h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume+es)^8,data=eddypro)
summary(model2)
model3=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume+es)^6,data=eddypro)
summary(model3)
model4=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume)^5,data=eddypro)
summary(model4)
anova(model4)
anova
model5=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume)^5 -LE:h2o_flux:co2_mole_fraction:air_temperature:air_molar_volume-LE:h2o_flux:co2_mole_fraction:air_molar_volume-LE:co2_mole_fraction:air_molar_volume-h2o_flux:air_molar_volume,data=eddypro)
summary(model5)
anova(model5)
model6=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume)^5 -LE:h2o_flux:co2_mole_fraction:air_temperature:air_molar_volume-LE:h2o_flux:co2_mole_fraction:air_molar_volume-LE:co2_mole_fraction:air_molar_volume-h2o_flux:air_molar_volume-LE:co2_mole_fraction:air_temperature:air_molar_volume-h2o_flux:co2_mole_fraction:air_molar_volume-LE:h2o_flux:co2_mole_fraction-LE:co2_mole_fraction,data=eddypro)
anova(model6)
model7=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume)^5 -LE:h2o_flux:co2_mole_fraction:air_temperature:air_molar_volume-LE:h2o_flux:co2_mole_fraction:air_molar_volume-LE:co2_mole_fraction:air_molar_volume-h2o_flux:air_molar_volume-LE:co2_mole_fraction:air_temperature:air_molar_volume-h2o_flux:co2_mole_fraction:air_molar_volume-LE:h2o_flux:co2_mole_fraction-LE:co2_mole_fraction-h2o_flux:co2_mole_fraction-LE:air_temperature:air_molar_volume,data=eddypro)
anova(model7)
model8=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume)^5 -LE:h2o_flux:co2_mole_fraction:air_temperature:air_molar_volume-LE:h2o_flux:co2_mole_fraction:air_molar_volume-LE:co2_mole_fraction:air_molar_volume-h2o_flux:air_molar_volume-LE:co2_mole_fraction:air_temperature:air_molar_volume-h2o_flux:co2_mole_fraction:air_molar_volume-LE:h2o_flux:co2_mole_fraction-LE:co2_mole_fraction-h2o_flux:co2_mole_fraction-LE:air_temperature:air_molar_volume-h2o_flux:air_temperature:air_molar_volume-LE:co2_mole_fraction:air_temperature,data=eddypro)
anova(model8)
model9=lm(RH~(LE+h2o_flux+co2_mole_fraction+air_temperature+air_molar_volume)^5 -LE:h2o_flux:co2_mole_fraction:air_temperature:air_molar_volume-LE:h2o_flux:co2_mole_fraction:air_molar_volume-LE:co2_mole_fraction:air_molar_volume-h2o_flux:air_molar_volume-LE:co2_mole_fraction:air_temperature:air_molar_volume-h2o_flux:co2_mole_fraction:air_molar_volume-LE:h2o_flux:co2_mole_fraction-LE:co2_mole_fraction-h2o_flux:co2_mole_fraction-LE:air_temperature:air_molar_volume-h2o_flux:air_temperature:air_molar_volume-LE:co2_mole_fraction:air_temperature-h2o_flux:co2_mole_fraction:air_temperature-LE:h2o_flux:co2_mole_fraction:air_temperature-h2o_flux:co2_mole_fraction:air_temperature:air_molar_volume,data=eddypro)
anova(model9)