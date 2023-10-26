setwd("C:/Users/Jose/Desktop/cronapp")
library(blockrand)
library(tidyverse)
library(xlsx)
data$id <- 1:nrow(data)
n_participantes <- nrow(data)

# Definir el tamaño de bloque
tamano_bloque <- 4

# Definir los grupos de tratamiento
grupos <- c("Tratamiento", "Control")

# Realizar la randomización por bloques
asignacion <- blockrand(n_participantes, num.levels = 2, levels = grupos)
# Ver la asignación
print(asignacion)

data_full <- data %>% 
  inner_join(asignacion, by = "id") %>% 
  mutate(EDAD = as.numeric(EDAD))

t.test(data_full$EDAD~data_full$treatment)

data_full %>% 
  group_by(treatment) %>% 
  summarise(mean(EDAD))

tratamiento <- data_full %>% 
  filter(treatment == "Tratamiento")

control <- data_full %>% 
  filter(treatment == "Control")

write.xlsx(tratamiento, "BBDD grupo tratamiento.xlsx")
write.xlsx(control, "BBDD grupo control.xlsx")

quantile(1:25, c(0.33,0.66))
