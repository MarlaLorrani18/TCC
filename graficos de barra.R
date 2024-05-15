rm(list=ls(all=TRUE))

# Pacotes 

library(dplyr)
library(ggplot2)

# Lendo a base final

VIOLBRFEM <- read.csv("C:\\Users\\marla\\Desktop\\TCC I\\Bases\\VIOLBRFEM.csv",
                      sep = ";")
VIOLBRFEM <- VIOLBRFEM[,-1]

# Gráficos de barras
#####
# CS_RACA, CS_ESCOL_N, SIT_CONJUG, LOCAL_OCOR, OUT_VEZES, VIOL_MOTIV, 
# NUM_ENVOLV, AUTOR_SEXO, AUTOR_ALCO, CICL_VID

# CS_RACA

VIOLBRFEM %>% filter(is.na(CS_RACA) != T & CS_RACA != 9) %>% 
  group_by(CS_RACA) %>%
  ggplot() + geom_bar(aes(x = as.factor(CS_RACA), fill = "red"),
                      show.legend = F) + 
  scale_y_continuous(breaks = c(150000, 300000, 450000)) +
  scale_x_discrete(labels = c("Branca", "Preta", "Amarela", "Parda", 
                              "Indígena")) +
  labs(y = "Número de notificações", x = "Cor/Raça") + theme_minimal()

# ORIENT_SEX

VIOLBRFEM %>% filter(is.na(ORIENT_SEX) != T & ORIENT_SEX != 8 & ORIENT_SEX != 9) %>% 
  group_by(ORIENT_SEX) %>% 
  ggplot() + geom_bar(aes(x = as.factor(ORIENT_SEX), fill = "red"), 
                      show.legend = F) + 
  scale_x_discrete(labels = c("Heterossexual", "Homossexual", "Bissexual"))  +
  scale_y_continuous(breaks = c(150000, 300000, 450000)) +
  labs(x = "Orientação Sexual", y = "Notificações")  + theme_minimal()

# IDENT_GEN

VIOLBRFEM %>% filter(is.na(IDENT_GEN) != T & IDENT_GEN != 1 & IDENT_GEN != 8 & IDENT_GEN != 9) %>% 
  group_by(IDENT_GEN) %>% 
  ggplot() + geom_bar(aes(x = as.factor(IDENT_GEN), fill = "red"), 
                      show.legend = F) + 
  scale_x_discrete(labels = c("Transexual Mulher", 
                              "Transexual Homem")) +
  labs(x = "Identidade de Gênero", y = "Notificações")  + theme_minimal()

# CS_ESCOL_N

VIOLBRFEM %>% filter(is.na(CS_ESCOL_N) != T & CS_ESCOL_N != 9 & CS_ESCOL_N != 10) %>% 
  group_by(CS_ESCOL_N) %>% 
  ggplot() + geom_bar(aes(x = as.factor(CS_ESCOL_N), fill = as.factor(CS_ESCOL_N))) + 
  scale_fill_discrete(name = "Escolaridade",
                      labels = c("Analfabeto",
                                 "1ª a 4ª série incompleta do EF",
                                 "4ª série completa do EF",
                                 "5ª à 8ª série incompleta do EF",
                                 "Ensino fundamental completo",
                                 "Ensino médio incompleto",
                                 "Ensino médio completo",
                                 "Educação superior incompleta",
                                 "Educação superior completa")) +
  labs(x = "Escolaridade", y = "Notificações") +
  theme(axis.text.x = element_blank())  + theme_minimal()

# SIT_CONJUG

VIOLBRFEM %>% filter(is.na(SIT_CONJUG) != T & SIT_CONJUG != 8 & SIT_CONJUG != 9) %>% 
  group_by(SIT_CONJUG) %>% 
  ggplot() + geom_bar(aes(x = as.factor(SIT_CONJUG), fill = "red"),
                      show.legend = F) + 
  scale_x_discrete(labels = c("Solteiro", "Casado", "Viúvo", 
                              "Separado", "Não se Aplica")) +
  scale_y_continuous(breaks = c(150000, 300000, 450000)) +
  labs(x = "Situação Conjugal", y = "Notificações")  + theme_minimal()

# LOCAL_OCOR

VIOLBRFEM %>% filter(is.na(LOCAL_OCOR) != T & LOCAL_OCOR != 99) %>% 
  group_by(LOCAL_OCOR) %>% 
  ggplot() + geom_bar(aes(x = as.factor(LOCAL_OCOR), fill = as.factor(LOCAL_OCOR))) + 
  scale_fill_discrete(name = "Local de ocorrência",
                      labels = c("Residência",
                                 "Habitação Coletiva",
                                 "Escola",
                                 "Local de prática esportiva",
                                 "Bar ou similar",
                                 "Via pública",
                                 "Comércio/Serviços",
                                 "Industrias/Construção",
                                 "Outro")) +
  labs(x = "Local de ocorrência", y = "Notificações") +
  theme(axis.text.x = element_blank()) + 
  theme_minimal()

# OUT_VEZES

VIOLBRFEM %>% filter(is.na(OUT_VEZES) != T & OUT_VEZES != "*" & OUT_VEZES != 0 & OUT_VEZES != 9) %>% 
  group_by(OUT_VEZES) %>% 
  ggplot() + geom_bar(aes(x = as.factor(OUT_VEZES), fill = "red"),
                      show.legend = F) + 
  scale_x_discrete(labels = c("Sim", "Não")) +
  scale_y_continuous(breaks = c(150000, 300000, 450000)) +
  labs(x = "A violência ocorreu outras vezes", y = "Notificações")  + 
  theme_minimal()

# VIOL_MOTIV

VIOLBRFEM %>% filter(is.na(VIOL_MOTIV) != T & VIOL_MOTIV != 99 & VIOL_MOTIV != 88) %>% 
  group_by(VIOL_MOTIV) %>% 
  ggplot() + geom_bar(aes(x = as.factor(VIOL_MOTIV), fill = as.factor(VIOL_MOTIV))) + 
  scale_fill_discrete(name = "Local de ocorrência",
                      labels = c("Sexismo",
                                 "Homofobia/Lesbofobia
Bifobia/Transfobia",
                                 "Racismo",
                                 "Intolerância religiosa",
                                 "Xenofobia",
                                 "Conflito geracional",
                                 "Situação de rua",
                                 "Deficiência",
                                 "Outros")) +
  labs(x = "Motivo da Violência", y = "Notificações") +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000))+
  theme(axis.text.x = element_blank()) + 
  theme_minimal()

# NUM_ENVOLV
VIOLBRFEM %>% filter(is.na(NUM_ENVOLV) != T & NUM_ENVOLV != "*" & NUM_ENVOLV != 9) %>% 
  group_by(NUM_ENVOLV) %>% 
  ggplot() + geom_bar(aes(x = as.factor(NUM_ENVOLV), fill = "red"),
                      show.legend = F) + 
  scale_x_discrete(labels = c("Um", "Dois ou mais")) +
  labs(x = "Número de envolvidos", y = "Notificações")  + theme_minimal()

# AUTOR_SEXO

VIOLBRFEM %>% filter(is.na(AUTOR_SEXO) != T & AUTOR_SEXO != 9) %>% 
  group_by(AUTOR_SEXO) %>% 
  ggplot() + geom_bar(aes(x = as.factor(AUTOR_SEXO), fill = "red"),
                      show.legend = F) + 
  scale_x_discrete(labels = c("Masculino", "Feminino", "Ambos")) +
  labs(x = "Sexo do provável autor da agressão", y = "Notificações")  + 
  theme_minimal()

# AUTOR_ALCO

VIOLBRFEM %>% filter(is.na(AUTOR_ALCO) != T & AUTOR_ALCO != 9) %>% 
  group_by(AUTOR_ALCO) %>% 
  ggplot() + geom_bar(aes(x = as.factor(AUTOR_ALCO), fill = "red"),
                      show.legend = F) + 
  scale_x_discrete(labels = c("Sim", "Não")) +
  scale_y_continuous(breaks = c(200000, 350000, 500000))+
  labs(x = "O provável autor da agressão tinha suspeita de uso de álcool", 
       y = "Notificações")  + theme_minimal()

# CICL_VID

VIOLBRFEM %>% filter(is.na(CICL_VID) != T & CICL_VID != 9) %>% 
  group_by(CICL_VID) %>% 
  ggplot() + geom_bar(aes(x = as.factor(CICL_VID), fill = as.factor(CICL_VID))) + 
  scale_fill_discrete(name = "Ciclo de vida",
                      labels = c("Criança", "Adolescente", "Jovem",
                                 "Adulto", "Idoso")) +
  scale_y_continuous(breaks = c(200000, 350000, 500000)) +
  labs(x = "Ciclo de vida do provável autor", y = "Notificações")+
  theme(axis.text.x = element_blank()) + 
  theme_minimal()

#####
