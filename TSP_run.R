library(dplyr)

GA_50iter=read.csv("file_ga_50iter_new_500pop.csv")
GA_50iter=select(GA_50iter, -generation.1 ,-generation.2 ,-generation.3 ,-generation.4 ,-generation.5 ,-generation.6 ,-generation.7 ,-generation.8 ,-generation.9 ,-generation.10 ,-generation.11 ,-generation.12 ,-generation.13 ,-generation.14 ,-generation.15 ,-generation.16 ,-generation.17 ,-generation.18 ,-generation.19 ,-generation.20 ,-generation.21 ,-generation.22 ,-generation.23 ,-generation.24 ,-generation.25 ,-generation.26 ,-generation.27 ,-generation.28 ,-generation.29 )
GA_50iter_par=parseData(GA_50iter,2,30)

pso_50iter=read.csv("file_pso_50iter_new_500pop.csv")
pso_50iter=select(pso_50iter, -generation.1 ,-generation.2 ,-generation.3 ,-generation.4 ,-generation.5 ,-generation.6 ,-generation.7 ,-generation.8 ,-generation.9 ,-generation.10 ,-generation.11 ,-generation.12 ,-generation.13 ,-generation.14 ,-generation.15 ,-generation.16 ,-generation.17 ,-generation.18 ,-generation.19 ,-generation.20 ,-generation.21 ,-generation.22 ,-generation.23 ,-generation.24 ,-generation.25 ,-generation.26 ,-generation.27 ,-generation.28 ,-generation.29 )
pso_50iter_par=parseData(pso_50iter,2,30)


aco_50iter=read.csv("file_aco_50iter_new_500pop.csv")
aco_50iter=select(aco_50iter, -generation.1 ,-generation.2 ,-generation.3 ,-generation.4 ,-generation.5 ,-generation.6 ,-generation.7 ,-generation.8 ,-generation.9 ,-generation.10 ,-generation.11 ,-generation.12 ,-generation.13 ,-generation.14 ,-generation.15 ,-generation.16 ,-generation.17 ,-generation.18 ,-generation.19 ,-generation.20 ,-generation.21 ,-generation.22 ,-generation.23 ,-generation.24 ,-generation.25 ,-generation.26 ,-generation.27 ,-generation.28 ,-generation.29 )
aco_50iter_par=parseData(aco_50iter,2,30)

plotbars(GA_50iter_par,aco_50iter_par,pso_50iter_par,"GA","ACO","PSO")
##############ANOVA TEST#######################
data <- data.frame(
  Algorithm = c(rep("GA", 50), rep("PSO", 50), rep("ACO", 50)),
  Result = c(GA_50iter_par[,2], pso_50iter_par[,2], aco_50iter_par[,2])
)
model <- aov(Result ~ Algorithm, data = data)
summary(model)
##############TukeyHSD TEST#######################
tukey <- TukeyHSD(model)
tukey
