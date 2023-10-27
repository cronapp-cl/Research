list_of_packages <- c("httr", "readxl", "dplyr", "ggplot2", "nortest", "coin")

# Function to automatically install and load multiple packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the packages
invisible(lapply(list_of_packages, library, character.only = TRUE))

# URL of the Excel file
url <- "https://github.com/cronapp-cl/Research/raw/main/patients.xlsx"

# Read the data directly from the URL
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file))
data <- read_excel(temp_file)

# Data pre-processing
data <- data %>% 
  mutate(treatment = as.factor(treatment))

# DESCRIPTIVE STATISTICS
data_summary <- data %>%
  group_by(treatment) %>%
  summarise(
    count = n(),
    avg_age = mean(age, na.rm = TRUE),
    avg_tech_literacy = mean(tech_literacy, na.rm = TRUE),
    proportion_male = mean(gender == 'M', na.rm = TRUE),
    proportion_female = mean(gender == 'F', na.rm = TRUE)
  )
print(data_summary)

#### Boxplot for tech_literacy
ggplot(data, aes(x = treatment, y = tech_literacy, color = treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Treatment Group", y = "Technological literacy")

#### Boxplot for age
ggplot(data, aes(x = treatment, y = age, color = treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Treatment Group", y = "Age")

#### Gender distribution by treatment group
ggplot(data, aes(x = treatment, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution by Treatment Group", x = "Treatment Group", fill = "Gender")

### VISUALIZATIONS AND TESTS


#### Chi-square test for 'gender'
chisq_result <- chisq.test(table(data$gender, data$treatment))
print(chisq_result)

#### Density plot for tech_literacy
ggplot(data, aes(x = tech_literacy)) +
  geom_density() +
  facet_wrap(~treatment)

#### QQ-plot for tech_literacy
ggplot(data, aes(sample = tech_literacy)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(~treatment, ncol = 2, scales = "free") +
  ggtitle("QQ-plots for 'tech_literacy' in treatment groups") +
  theme_minimal()

#### Normality tests
lillie.test(data$tech_literacy[data$treatment == "Control"])
lillie.test(data$tech_literacy[data$treatment == "Tratamiento"])

#### Density plot for 'age' 
ggplot(data, aes(x = age)) +
  geom_density() +
  facet_wrap(~treatment)

#### QQ-Plot for 'age'
ggplot(data, aes(sample = age)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(~treatment, ncol = 2, scales = "free") +
  ggtitle("QQ-plots for 'age' in treatment groups") +
  theme_minimal()

#### Normality tests for 'age'
lillie.test(data$age[data$treatment == "Control"])
lillie.test(data$age[data$treatment == "Tratamiento"])

### Wilcoxon test
#### Technological literacy ~ Treatment
wilcox_test_result <- wilcox_test(tech_literacy ~ treatment, data = data, conf.int = TRUE)
print(wilcox_test_result)

#### Age ~ Treatment
wilcox_test_result <- wilcox_test(age ~ treatment, data = data, conf.int = TRUE)
print(wilcox_test_result)

# Clean up the environment from temporary files
unlink(temp_file)

