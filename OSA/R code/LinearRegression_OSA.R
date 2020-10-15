Input_file <- "OSA_DB_UPM.xlsx"
Data_Directory <- "/Users/mariabrullmartinez/RSeminar-master/OSA_CaseStudy/DATA/"

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)

names(df_OSA)
dim(df_OSA)

# First define Gender as a factor!
df_OSA$Gender = factor(df_OSA$Gender)
summary(df_OSA)

# See relations between variables
attach(df_OSA)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age)

library(corrplot)
# back to as.numeric for including it..

df_OSA_C=df_OSA

df_OSA_C$Gender = as.numeric(df_OSA_C$Gender)
M <- cor(subset(df_OSA_C, select = - Patient))
M

corrplot(M, method="number")
corrplot(M, method="circle")

lm.fit=lm(IAH~Weight+Height+Cervical+Age+Gender)
#bear in mind that gender is 1 or 0
summary(lm.fit)

### Male population
df_OSA_male=subset(df_OSA_C, Gender==1)

names(df_OSA_male)
attach(df_OSA_male)

#try with SVM or radom_forest
lm_male.fit=lm(IAH~Height+Cervical+Age+Weight)


summary(lm_male.fit)

###Female population
df_OSA_female=subset(df_OSA_C, Gender==2)
names(df_OSA_female)
attach(df_OSA_female)

lm_female.fit=lm(IAH~Height+Cervical+Age+Weight)


summary(lm_female.fit)

#add BMI column
df_OSA_C$BMI <- with(df_OSA_C, Weight / (Height/100.0)^2)

attach(df_OSA_C)

lm.fit=lm(IAH~BMI+Cervical+Age)

summary(lm.fit)


