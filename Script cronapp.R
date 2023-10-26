#### ANALISIS CRONAPP
rm(list=ls())
setwd("C:/Users/Jose/Desktop/cronapp")

# LIBRERIAS
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
librerias <- c("openxlsx","ggplot2","readxl","dplyr","epiDisplay","stringr","tidyverse","ggpubr","rstatix")
ipak(librerias)

# CARGAR DATOS
data_pacientes <- read.csv("pacientes.csv")

data_diabetes <- read.csv("registros_diabetes.csv")

data_presion <- read.csv("registros_presion.csv")


# HOUSECLEAN
data_pacientes <- data_pacientes %>%
  dplyr::select(rut,genero,centro,diabetes,presion,fechaRegistro) %>%
  mutate(genero = tolower(genero),
         genero = case_when(genero == "hombre"~"masculino",
                            genero == "mujer"~"femenino",
                            genero == "femenino"~"femenino",
                            genero == "masculino"~"masculino"))
data_diabetes <- data_diabetes %>%
  dplyr::select(rut = Rut, glicemia = Glicemia, fecha_diabetes = Fecha)

data_presion <- data_presion %>%
  dplyr::select(rut = Rut, sistolica = Sistolica,diastolica = Diastolica, fecha_presion = Fecha) %>%
  mutate(rut = as.character(rut))
 # NO HAY VALORES REPETIDOS

# JOIN
data <-left_join(data_diabetes,data_presion, by = "rut")
data <- left_join(data,data_pacientes, by = "rut")
data <- data %>% arrange(rut)

# TIME DEPURATION
library(lubridate)
data <- data %>%
  mutate(fecha_diabetes = as.POSIXct(fecha_diabetes, format = "%Y-%m-%d %H:%M"),
         fecha_presion = as.POSIXct(fecha_presion, "%Y-%m-%d %H:%M"),
         month_dm2 = month(fecha_diabetes),
         month_pres = month(fecha_presion))

data <- data %>%
  group_by(rut,month_dm2) %>%
  mutate(n_dm2_mes = n_distinct(fecha_diabetes)) %>% 
  ungroup(month_dm2) %>%
  mutate(n_dm2_total = n_distinct(fecha_diabetes)) %>%
  ungroup()

data <- data %>%
  group_by(rut,month_pres) %>%
  mutate(n_pres_mes = n_distinct(fecha_presion)) %>% 
  ungroup(month_pres) %>%
  mutate(n_pres_total = n_distinct(fecha_presion)) %>%
  ungroup() 

data <- data %>%
  group_by(rut) %>%
  mutate(dias_seg_dm2 = as.numeric(round(difftime(max(fecha_diabetes),min(fecha_diabetes), units = "days"))),
         dias_seg_pres = as.numeric(round(difftime(max(fecha_presion),min(fecha_presion), units = "days"))),
         ratio_seg_dm2 = round(n_dm2_total/dias_seg_dm2,2),
         ratio_seg_pres = round(n_pres_total/dias_seg_pres,2)) %>%
  ungroup() 

openxlsx::write.xlsx(data, file = "data_piloto.xlsx")
# CHEQUEO DE COSAS RARAS
summary(data$ratio_seg_dm2) # EL PROMEDIO DA INFINITO
# ESTO ES PORQUE HAY PERSONAS QUE SOLO ENTRARON 1 VEZ
data <-data %>%
  mutate(dias_seg_dm2 = ifelse(dias_seg_dm2 == 0,1,dias_seg_dm2),
         dias_seg_pres = ifelse(dias_seg_pres == 0,1,dias_seg_pres),
         ratio_seg_dm2 = round(n_dm2_total/dias_seg_dm2,2),
         ratio_seg_pres = round(n_pres_total/dias_seg_pres,2))
summary(data$ratio_seg_dm2) 
sd(data$ratio_seg_dm2)
# AHORA SI
summary(data$ratio_seg_pres) # EL PROMEDIO DA INFINITO
# AL PARECER, EL PROMEDIO DE RATIO DE MEDICIONES EN LAS PERSONAS CON DM2 ES MAYOR AL DE HTA
data <- data %>%
  mutate(grupo = case_when(diabetes == 1 & presion == 0 ~ "DM2",
                           presion == 1 & diabetes == 0 ~ "HTA",
                           diabetes == 1 & presion == 1 ~ "HTA y DM2",
                           diabetes == 0 & presion == 0 ~ "Ninguna")) %>%
  dplyr::select(-diabetes,-presion)
         
tab1(data$grupo)

########################
# ANALISIS DESCRIPTIVO #
########################

# LA VARIABLE PRINCIPAL SERA EL RATIO ENTRE EL NUMERO DE MEDICIONES Y EL NUMERO DE DIAS DE SEGUIMIENTO
# EL DENOMINADOR REPRESENTA EL NUMERO DE DIAS TRANSCURRIDOS ENTRE LA PRIMERA Y LA ULTIMA MEDICION (SEGUIMIENTO)
# EL NUMERADOR ES UN CONTEO DE TODAS LAS VECES EN QUE UNA PERSONA SE MIDIO
# CUENTA TODOS LOS REGISTROS, INCLUYENDO LAS PERSONAS QUE SE MIDIERON MAS DE UNA VEZ 

# NUMERO TOTAL DE PARTICIPANTES
n_distinct(data$rut)
# HAY 36 RUT DISTINTOS

# PARA DESCRIPTIVOS
descriptivo <- data %>%
  group_by(rut) %>%
  slice(1) %>%
  ungroup()

# GENERO
tab1(descriptivo$genero)
# 47.2% MUJERES
# 48.5% HOMBRES
# 3 MISSING
min(data$fecha_diabetes)
min(data$fecha_presion, na.rm = T)
# CENTRO 
tab1(descriptivo$centro)
# 19.4% CECOF COÑIMO
# 22.2% CECOF JUAN ARAVENA
# 44.4% CESFAM SOR TERESA

# HTA
tab1(descriptivo$grupo)
# RATIO MEDICIONES/SEGUIMIENTO DE DIABETES
summary(descriptivo$ratio_seg_dm2)
sd(descriptivo$ratio_seg_dm2)

# RATIO MEDICIONES/SEGUIMIENTO DE HTA
summary(data$ratio_seg_pres)

# LA INTERPRETACION ES LA SIGUIENTE:
# SI EL VALOR ES CERCANO A 1 SIGNIFICA QUE LA PERSONA SE MIDE EN PROMEDIO 1 VEZ AL DIA
# VALORES MAYORES A 1 INDICAN QUE LA PERSONA SE MIDE EN PROMEDIO MAS DE UNA VEZ AL DIA
# VALORES MENORES A 1 INDICAN QUE LA PERSONA SE MIDE EN PROMEDIO MENOS DE UNA VEZ AL DIA

# EN ESTE CASO, LAS PERSONAS QUE USARON CRONAPP SE MIDIERON LA GLICEMIA 1.65 VECES AL DIA EN PROMEDIO
# LAS PERSONAS QUE USARON CRONAPP SE MIDIERON LA PRESION 0.68 VECES AL DIA. 
# ¿POR QUE HAY TANTA DIFERENCIA?
# ¿QUÉ FACTORES PODRÍAN EXPLICAR ESO?

# HAY 0 PERSONAS QUE TIENEN SOLAMENTE HTA

data %>%
  group_by(grupo) %>%
  summarise(mean_ratio_hta = mean(ratio_seg_pres, na.rm = T),
            mean_ratio_dm2 = mean(ratio_seg_dm2))

data %>%
  filter(grupo != "DM2") %>%
ggplot(., aes(x = ratio_seg_pres, y = grupo)) +
  geom_boxplot() +
  labs(x = "Mediciones/Seguimiento de presión arterial", y = "Grupo de riesgo")


ggplot(data, aes(x = ratio_seg_dm2, y = grupo)) +
  geom_boxplot() +
  labs(x = "Mediciones/Seguimiento de presión arterial", y = "Grupo de riesgo")

# ES DE VITAL IMPORTANCIA NO INCORPORAR GENTE SIN PERFIL DE RIESGO

data <- data %>%
  group_by(rut) %>%
  mutate(time = row_number(),
         grupo = as.factor(grupo))
class(data$grupo)
data$grupo
summary(descriptivo$ratio_seg_dm2)
summary(descriptivo$ratio_seg_pres)

# ANOVA ENTRE RATIO HTA Y GRUPO
shapiro.test(descriptivo$ratio_seg_pres)
anova_hta <- aov(descriptivo$ratio_seg_pres~descriptivo$grupo)
summary(anova_hta)
TukeyHSD(anova_hta)

# KRUSKAL WALLIS ENTRE RATIO DM2 Y GRUPO
shapiro.test(descriptivo$ratio_seg_dm2)
kruskal.test(descriptivo$ratio_seg_dm2~descriptivo$grupo)
pairwise.wilcox.test(descriptivo$ratio_seg_dm2, descriptivo$grupo,
                     p.adjust.method = "BH")

rdf<-data %>%
  arrange(rut) %>%
  group_by(rut, fecha_diabetes) %>%
  mutate(dummy = 1) %>%
  mutate(times = sum(dummy)) %>%
  dplyr::select(-dummy)

min(data$fecha_diabetes)
# 2020-12-17
max(data$fecha_diabetes)
# 2021-12-04

min(data$fecha_presion,na.rm = T)
# 2021-10-09
max(data$fecha_presion, na.rm = T)
# 2022-11-15
table(data$centro)
table(data$diabetes)
table(data$presion)
table(data$genero)

# RATIO ENTRE EL SEGUIMIENTO Y LA FRECUENCIA DE MEDICIONES
data %>%
  summarise(media_ratio = mean(ratio_seg_dm2))
summary(data$n_dias_seg_dm2)
table(data$ratio_seg_glicemia)
chek <- data %>%
  filter(ratio_seg_dm2 == Inf)
