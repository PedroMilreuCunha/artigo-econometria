## -- Pedro Milreu Cunha -- ##
## -- Mestrando em Economia Aplicada pelo PPGE/UFPB -- ##

#### Bibliotecas ####

library(stargazer)
library(mfx)
library(caret)
library(glmx)
library(hetprobit)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(RStata)

#---- 1) Importando os dados #----

dados <- read.csv("Dados trabalhados/dados_finais.csv")
dados <- dados[, -c(1,2,5:8)]

#---- 1.1) Estatísticas descritivas #----

stargazer(dados, type = "text")

#---- 2) Estimação via probit #----

mpl <- lm(data = dados,
          Murder ~ Brazil+Government+Military+Political+Male+Local+Freelance+War)

probit_1 <- glm(data = dados,
                Murder ~ Brazil+Government+Military+Political+Male+Local+Freelance+War,
                family = binomial(link = "probit"))
stargazer(mpl, probit_1, type = "text")

probit_2 <- hetprobit(data = dados,
                      Murder ~ Brazil+Government+Military+Political+Male+Local+Freelance+War |Freelance+War)
summary(probit_2)


  #LR_1 <- 2*(logLik(probit_2) - logLik(probit_1))
  #1-pchisq(LR_1, df = 3)
  #LR_2 <- 2*(logLik(probit_3) - logLik(probit_1))
  #1-pchisq(LR_2, df = 8)
  #LR_3 <- 2*(logLik(probit_3) - logLik(probit_2))
  #1-pchisq(LR_3, df = 0)

AIC(probit_1, probit_2)
BIC(probit_1, probit_2)
lrtest(probit_1, probit_2)

#---- 3) Média dos efeitos marginais #----

em <- probitmfx(Murder ~ Brazil+Government+Military+Political+Male+Local+Freelance+War,
                data = dados, robust = TRUE,
                atmean = FALSE)

margins <- data.frame(V1 = c("Brasil", "Governo", "Militares", "Política", "Homem", "Não-estrangeiro", "Freelancer", "Guerra"),
                      me = c(0.2500408, 0.0405206, -0.4513651, -0.1178965, -0.0839517, 0.0859645, -0.1034486, -0.128683),
                      se = c(0.1392582, 0.0423573, 0.0328466, 0.0340714, 0.0451171, 0.0343014, 0.0388752, 0.0267374),
                      pvalue = c("< 0.1", "> 0.1", "< 0.01", "< 0.01", "< 0.1", "< 0.05", "< 0.01", "< 0.01")) #Calculado no Stata

#Os efeitos marginais do modelo heterocedástico foram calculados no Stata, utilizando o comando 
#margins, dy/dx(*) após a estimação via hetprob Murder . , het(Freelancer Guerra) vce(robust)

 #---- 3.1) Características pessoais e local de atuação #----

mfx_1 <- data.frame(V1 = c("Brasil", "Homem", "Não-estrangeiro", "Freelancer"), 
                  me = em$mfxest[c(1,5,6,7),1],
                  se = em$mfxest[c(1,5,6,7),2],
                  pvalue = ifelse(em$mfxest[c(1,5,6,7),4] < 0.01, "< 0.01",
                                  ifelse(em$mfxest[c(1,5,6,7),4] > 0.01 & em$mfxest[c(1,5,6,7),4] < 0.05, "< 0.05",
                                  ifelse(em$mfxest[c(1,5,6,7),4] > 0.05 & em$mfxest[c(1,5,6,7),4] < 0.1, "< 0.1", "> 0.1"))))

margins_1 <- subset(margins, V1 %in% mfx_1$V1)

labels_1 <- round(100*mfx_1$me,2)
labels_1 <- paste(labels_1, "%")

labels_s_1 <- round(100*margins_1$me, 2)
labels_s_1 <- paste(labels_s_1, "%")

g1 <- ggplot(mfx_1, aes(V1, me ,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete("") +
  scale_y_continuous('Efeito marginal',limits=c(-1,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),         panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_errorbar(aes(x = V1, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = me), show.legend = FALSE) +
  geom_hline(yintercept=0, linetype = "dashed", alpha = 0.15) + 
  coord_flip()+
  geom_text(aes(label = labels_1, color = pvalue), hjust=0.1, vjust=-1.2, show.legend = TRUE, size = 3,
            key_glyph = "rect")+
  labs(tag = "a)", subtitle = "Características pessoais e local de atuação", caption = "Os dados dizem respeito apenas aos jornalistas mortos em atuação e estão disponíveis em (Committee to Protect Journalists, 2020).")+
  scale_colour_discrete("P-valor")

g1_s <- ggplot(margins_1, aes(V1, me ,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete("") +
  scale_y_continuous('Efeito marginal',limits=c(-1,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),         panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_errorbar(aes(x = V1, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = me), show.legend = FALSE) +
  geom_hline(yintercept=0, linetype = "dashed", alpha = 0.15) + 
  coord_flip()+
  geom_text(aes(label = labels_s_1, color = pvalue), hjust=0.1, vjust=-1.2, show.legend = TRUE, size = 3,
            key_glyph = "rect")+
  labs(tag = "a)", subtitle = "Características pessoais e local de atuação", caption = "Os dados dizem respeito apenas aos jornalistas mortos em atuação e estão disponíveis em (Committee to Protect Journalists, 2020).")+
  scale_colour_discrete("P-valor")


 #---- 3.2) Responsável pelo assassinato #----
  
  mfx_2 <- data.frame(V2 = c("Governo", "Militares"), 
                      me = em$mfxest[c(2,3),1],
                      se = em$mfxest[c(2,3),2],
                      pvalue = ifelse(em$mfxest[c(2,3),4] < 0.01, "< 0.01",
                                      ifelse(em$mfxest[c(2,3),4] > 0.01 & em$mfxest[c(2,3),4] < 0.05, "< 0.05",
                                             ifelse(em$mfxest[c(2,3),4] > 0.05 & em$mfxest[c(2,3),4] < 0.1, "< 0.1", "> 0.1"))))
margins_2 <- subset(margins, V1 %in% mfx_2$V2)

  labels_2 <- round(100*mfx_2$me,2)
  labels_2 <- paste(labels_2, "%")

  labels_s_2 <- round(100*margins_2$me, 2)
  labels_s_2 <- paste(labels_s_2, "%")
  
    
g2 <- ggplot(mfx_2, aes(V2, me ,ymin = me - 2*se,ymax= me + 2*se)) +
    scale_x_discrete("") +
    scale_y_continuous('Efeito marginal',limits=c(-1,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),         panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    geom_errorbar(aes(x = V2, y = me),size=.3,width=.2) + 
    geom_point(aes(x = V2, y = me), show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed", alpha = 0.15) + 
    coord_flip()+
    geom_text(aes(label = labels_2, color = pvalue), hjust=0.1, vjust=-1.2, show.legend = TRUE, size = 3,
              key_glyph = "rect")+
    labs(tag = "b)", subtitle = "Suspeito de ser responsável pela morte", caption = "Os dados dizem respeito apenas aos jornalistas mortos em atuação e estão disponíveis em (Committee to Protect Journalists, 2020).")+
  scale_colour_discrete("P-valor")

g2_s <- ggplot(margins_2, aes(V1, me ,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete("") +
  scale_y_continuous('Efeito marginal',limits=c(-1,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),         panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_errorbar(aes(x = V1, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = me), show.legend = FALSE) +
  geom_hline(yintercept=0, linetype = "dashed", alpha = 0.15) + 
  coord_flip()+
  geom_text(aes(label = labels_s_2, color = pvalue), hjust=0.1, vjust=-1.2, show.legend = TRUE, size = 3,
            key_glyph = "rect")+
  labs(tag = "b)", subtitle = "Suspeito de ser responsável pela morte", caption = "Os dados dizem respeito apenas aos jornalistas mortos em atuação e estão disponíveis em (Committee to Protect Journalists, 2020).")+
  scale_colour_discrete("P-valor")

  
  #---- 3.3) Tópico de cobertura #----
  
  mfx_3 <- data.frame(V3 = c("Política", "Guerra"), 
                      me = em$mfxest[c(4,8),1],
                      se = em$mfxest[c(4,8),2],
                      pvalue = ifelse(em$mfxest[c(4,8),4] < 0.01, "< 0.01",
                                      ifelse(em$mfxest[c(4,8),4] > 0.01 & em$mfxest[c(4,8),4] < 0.05, "< 0.05",
                                             ifelse(em$mfxest[c(4,8),4] > 0.05 & em$mfxest[c(4,8),4] < 0.1, "< 0.1", "> 0.1"))))
margins_3 <- subset(margins, V1 %in% mfx_3$V3)

labels_3 <- round(100*mfx_3$me,2)
labels_3 <- paste(labels_3, "%")

labels_s_3 <- round(100*margins_3$me, 2)
labels_s_3 <- paste(labels_s_3, "%")


g3 <- ggplot(mfx_3, aes(V3, me ,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete("") +
  scale_y_continuous('Efeito marginal',limits=c(-1,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),         panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_errorbar(aes(x = V3, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V3, y = me), show.legend = FALSE) +
  geom_hline(yintercept=0, linetype = "dashed", alpha = 0.15) + 
  coord_flip()+
  geom_text(aes(label = labels_3, color = pvalue), hjust=0.1, vjust=-1.2, show.legend = TRUE, size = 3,
            key_glyph = "rect")+
  labs(tag = "c)", subtitle = "Tópico de cobertura", caption = "Os dados dizem respeito apenas aos jornalistas mortos em atuação e estão disponíveis em (Committee to Protect Journalists, 2020).")+
  scale_colour_discrete("P-valor")

g3_s <- ggplot(margins_3, aes(V1, me ,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete("") +
  scale_y_continuous('Efeito marginal',limits=c(-1,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),         panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_errorbar(aes(x = V1, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = me), show.legend = FALSE) +
  geom_hline(yintercept=0, linetype = "dashed", alpha = 0.15) + 
  coord_flip()+
  geom_text(aes(label = labels_s_3, color = pvalue), hjust=0.1, vjust=-1.2, show.legend = TRUE, size = 3,
            key_glyph = "rect")+
  labs(tag = "c)", subtitle = "Tópico de cobertura", caption = "Os dados dizem respeito apenas aos jornalistas mortos em atuação e estão disponíveis em (Committee to Protect Journalists, 2020).")+
  scale_colour_discrete("P-valor")

grid.arrange(g1,g2,g3)
grid.arrange(g1_s,g2_s,g3_s)

#---- 4) Tabela de confusão #----

temp <- predict(probit_1, type = "response")
previstos <- ifelse(temp > 0.5, 1, 0)
previstos <- as.factor(previstos)

confusionMatrix(data = previstos, reference = as.factor(dados$Murder))

temp_het <- predict(probit_2, type = "response")
previstos_het <- ifelse(temp_het > 0.5, 1, 0)
previstos_het <- as.factor(previstos_het)

confusionMatrix(data = previstos_het, reference = as.factor(dados$Murder))

