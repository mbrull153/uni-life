#clear data
rm(list=ls())

#define input and output
Input_file <- "Info_BDApnea_QuironMalaga.xlsx"

Output_file <- "OSA_DB_UPM.xlsx"
#define where are we going to save it 
Data_Directory <- "/Users/mariabrullmartinez/RSeminar-master/OSA_CaseStudy/DATA/"

library(readxl)
#reading the excell and coping in the directory
df_tmp <- read_excel(paste(Data_Directory, Input_file, sep = ""))

typeof(df_tmp)
is.data.frame(df_tmp)  

class(df_tmp)
#assign this as a dataframe
df_tmp = as.data.frame(df_tmp)
df_tmp
class(df_tmp)  

library(dplyr)

df_tmp1 <- select(df_tmp, Patient, Gender, IAH, Peso, Talla, Edad, PerCervical)

library(visdat)
vis_dat(df_tmp1)

library(naniar)

# Now change -1 values for NA in all columns (i.e. variables)
df_tmp2 <- replace_with_na_all(df_tmp1,condition = ~.x == -1)
df_tmp3<-transform(df_tmp2, Weight = as.numeric(Peso))
df_tmp3 <- df_tmp3[ ,-c(4)]
df_tmp3<- df_tmp3[,c(1,2,3,7,4,5,6)]
vis_dat(df_tmp3)
library(tidyr)

df_final <- df_tmp3 %>% drop_na()

df_final 
sum(df_final$Gender=='hombre')
sum(df_final$Gender=='mujer')
summary(df_final)
df_final$Gender <- factor(df_final$Gender)
df_final$Gender

library(writexl)

# You can change the names of the columns

df_final <- df_final %>% rename(Weight = Weight,
                                Height = Talla,
                                Age = Edad,
                                Cervical = PerCervical) 

vis_dat(df_final)

write_xlsx(df_final,
           paste(Data_Directory, Output_file, sep = ""))

names(df_final)

