rm(list=ls())

Input_file <- "OSA_DB_UPM.xlsx"

Output_file <- "OSA_extreme_male.xlsx"
Data_Directory <- "/Users/mariabrullmartinez/RSeminar-master/OSA_CaseStudy/DATA/"

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

#MALE
df_OSA_male=subset(df_OSA, Gender=="hombre")

###
# We define column to tag three clases:
#     Healthy (IAH <= 10)
#     Mild (10<IAH<30)
#     Severe (IAH >=30)

library(dplyr)

df_OSA_male <- df_OSA_male %>%
  mutate(OSA = ifelse(IAH <= 10, "Healthy",
                      ifelse(IAH>=30, "Severe", "Mild")))
df_OSA_male
#remove mild
df_OSA_male <- df_OSA_male %>% filter(OSA != "Mild")
df_OSA_male

# Define OSA as a factor

df_OSA_male$OSA = factor(df_OSA_male$OSA)

# Add BMI column
df_OSA_male$BMI <-
  with(df_OSA_male, Weight / (Height/100.0)^2)

summary(df_OSA_male)
describe(df_OSA_male)

Output_file <- "OSA_extreme_male.xlsx"


library(writexl)

write_xlsx(df_OSA_male,
           paste(Data_Directory, Output_file, sep = ""))

#EDA 
cor(df_OSA_male[,3:7])
# for examle a visualization
library(corrplot)
correlations = cor(df_OSA_male[,3:7])
corrplot(correlations, method="number")


#LATTICE
library(lattice)

# Each group in a separate mini plot
xyplot(BMI~ Weight | OSA, data = df_OSA_male)

xyplot(BMI ~ Cervical , 
       groups =  OSA, data = df_OSA_male,
       auto.key = list(corner = c(1, 1), cex = 0.7))

#HISTOGRAM
library(ggplot2)

ggplot(df_OSA_male, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

