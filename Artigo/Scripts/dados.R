## -- Pedro Milreu Cunha -- ##
## -- Mestrando em Economia Aplicada pelo PPGE/UFPB -- ##

#### Bibliotecas ####

library(dplyr)
library(magrittr)

#---- 1) Lendo os dados #----

temp <- read.csv("Dados iniciais/dados.csv")
dados <- temp[,c(1,8,13:20,26,27)]

#---- 2) Criando as dummies #----

dados$Murder <- ifelse(dados$typeOfDeath == "Murder", 1, 0)
dados$Brazil <- ifelse(dados$country == "Brazil", 1, 0)
dados$Colombia <- ifelse(dados$country == "Colombia", 1, 0)
dados$France <- ifelse(dados$country == "France", 1, 0)
dados$Iraq <- ifelse(dados$country == "Iraq", 1, 0)
dados$Syria <- ifelse(dados$country == "Syria", 1, 0)
dados$Government <- ifelse(grepl("Government Officials", dados$sourcesOfFire), 1, 0)
dados$Military <- ifelse(grepl("Military Officials", dados$sourcesOfFire), 1, 0)
dados$Political <- ifelse(grepl("Political Group", dados$sourcesOfFire), 1, 0)
dados$Male <- ifelse(dados$gender == "Male", 1, 0)
dados$Local <- ifelse(grepl("Local", dados$localOrForeign), 1, 0)
dados$Freelance <- ifelse(grepl("Staff",dados$employedAs), 0, 1)
dados$War <- ifelse(grepl("War",dados$coverage), 1 , 0)

#dados %<>% mutate_if(is.character,as.factor) Comando para transformar strings em fatores

#---- 3) Remoção das observações com dados faltantes  #----

dados <- na_if(dados,"")
dados <- na.omit(dados)

#---- 4) Base final #----

df <- dados[, c(1,13:25)]

#---- 5) Exportando a base #----

write.csv(df, "Dados trabalhados/dados_finais.csv")
