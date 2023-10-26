require(gender)
require(readxl)
require(ggplot2)
require(nortest)
if (!require(coin)) {
  install.packages("coin")
}
library(coin)

data <- read_excel("Github/Research/patients.xlsx")

data <- data %>% 
  mutate(treatment = as.factor(treatment))

# Descriptive statistics by group
data_summary <- data %>%
  group_by(treatment) %>%
  summarise(
    count = n(),
    avg_age = mean(age, na.rm = TRUE),
    avg_tech_literacy = mean(tech_literacy, na.rm = TRUE),
    proportion_male = mean(gender == 'M', na.rm = TRUE), # 'M' for males
    proportion_female = mean(gender == 'F', na.rm = TRUE) # 'F' for females
  )

print(data_summary)


# Bar chart for gender distribution
ggplot(data, aes(x = treatment, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution by Treatment Group", x = "Treatment Group", fill = "Gender")

# Chi-squared test for 'gender'
table_gender <- table(data$gender, data$treatment, margin(1))
print(table_gender)
chisq_result <- chisq.test(table_gender)


# Print the chi-squared test result
print(chisq_result)



# NORMALITY ASSUMPTION
ggplot(data, aes(x = tech_literacy)) +
  geom_density() +
  facet_wrap(~treatment)
lillie.test(data[data$treatment == "Control",]$tech_literacy)
lillie.test(data[data$treatment == "Tratamiento",]$tech_literacy)
 # not normal

wilcox_test(tech_literacy ~ treatment, data = data, conf.int = TRUE)


ggplot(data, aes(x = age)) +
  geom_density() +
  facet_wrap(~treatment)

# Preparamos los datos para el grupo "Control"
data_control <- data[data$treatment == "Control", ]$age

# Preparamos los datos para el grupo "Tratamiento"
data_treatment <- data[data$treatment == "Tratamiento", ]$age

# Hacemos el QQ-plot para el grupo "Control"
qqplot <- ggplot(data.frame(sample = data_control), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  ggtitle("QQ-plot for Age among control group") +
  theme_minimal() +
  xlab("Cuantiles teóricos") +
  ylab("Cuantiles de los datos")

print(qqplot)

# Hacemos el QQ-plot para el grupo "Tratamiento"
qqplot <- ggplot(data.frame(sample = data_treatment), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(colour = "red") + 
  ggtitle("QQ-plot for Age among treatment group") +
  theme_minimal() +
  xlab("Theoric Quantiles") +
  ylab("Empiric Quantiles")

print(qqplot)

data_control <- data.frame(age = data[data$treatment == "Control", ]$age, 
                           group = "Control")
data_treatment <- data.frame(age = data[data$treatment == "Tratamiento", ]$age, 
                             group = "Tratamiento")

# Unimos ambos conjuntos de datos
data_combined <- rbind(data_control, data_treatment)

# Ahora creamos el QQ-plot usando ggplot2, especificando 'group' como la variable de facetas.
ggplot(data_combined, aes(sample = age)) +
  stat_qq() +
  stat_qq_line(colour = "red") +  # Esto añade la línea de referencia teórica
  facet_wrap(~group, ncol = 2, scales = "free") +  # Esto crea los paneles separados por grupo
  ggtitle("QQ-plots para 'age' en grupos de tratamiento") +
  theme_minimal() +
  xlab("Cuantiles teóricos") +
  ylab("Cuantiles de los datos")

lillie.test(data[data$treatment == "Control",]$age)
lillie.test(data[data$treatment == "Tratamiento",]$age)

# Wilcoxon Mann Whitney
wilcox_test(age ~ treatment, data = data, conf.int = TRUE)




t.test(tech_literacy ~ treatment, data = data)
