
##Análisis Básico -> PREPROCESAMIENTO DE DATOS y Visualización##

##Instalación de Tidyverse
install.packages("tidyverse")
library(tidyverse)

heart <- read.csv('heart.csv')
View(heart)

##Deteccion de outliers
str(heart) #Contenido de heart.csv
head(heart)



#######Explicación de las variables #########

(heart$Age)# Edad

(heart$Sex) #Género

(heart$RestingBP) #Resting BloodPressure en mmHg. Pienso que es la sistolica por los niveles (media de 120 frente a los 80 de la diastolica)

(heart$ChestPainType) #Dolores en el pecho: 
#-- Value 1: typical angina
#-- Value 2: atypical angina
#-- Value 3: non-anginal pain
#-- Value 4: asymptomatic

(heart$Cholesterol) #nivel de colesterol. Se podría clasificar normal(<200), alto(200-240), muy alto(>240)

(heart$FastingBS) #fasting blood sugar (glucemia en ayunas). Si es mayor a 120mg/dl es un 1 (si) si es menor es un 0 (no)

(heart$RestingECG) #resting electrocardiographic results
#-- Value 0: normal
#-- Value 1: having ST-T wave abnormality (T wave inversions and/or ST 
#                                          elevation or depression of > 0.05 mV). Segmento ST representa la despolarizacion y repolarizacion
#                                         del ventriculo izquierdo.
#-- Value 2: showing probable or definite Left Ventricular Hypertrophy (LVH)
#by Estes' criteria

(heart$MaxHR) #Frecuencia Cardíaca Máxima

(heart$ExerciseAngina) #Anginas inducidas por el ejercicio

(heart$Oldpeak) #ST depression induced by exercise relative to rest

(heart$ST_Slope) #the slope of the peak exercise ST segment
#-- Value 1: upsloping
#-- Value 2: flat
#-- Value 3: downsloping

(heart$HeartDisease) #clasificación, 1 (sI ha tenido problemas de corazón) y 0 (NO ha tenido problemas de corazon)




########BOXPLOT (DIAGRAMA DE CAJAS Y BIGOTES) para la detección de outliers########

boxplot(heart2$Age, horizontal = TRUE)


###Atributo RestingBP

RestingBP <- heart$RestingBP

boxplot(RestingBP, horizontal = TRUE) #Hay outliers por debajo de 92 y por encima de 170
boxplot.stats(RestingBP)
heart_Sin_Out_RestingBP <- RestingBP[RestingBP>92 & RestingBP<170] #Sin outliers
boxplot(heart_Sin_Out_RestingBP, horizontal = TRUE)
boxplot.stats(heart_Sin_Out_RestingBP)

#FIltrado del dataframe mediante paquete dplyr para eliminar outliers de RestingBP
heart1 <- filter(heart, RestingBP > 92 & RestingBP<170) #Nuevo dataframe con 875 observaciones

str(heart1)
head(heart1)


###Atributo Cholesterol
Cholesterol <- heart1$Cholesterol

boxplot(Cholesterol, horizontal=TRUE)
boxplot.stats(Cholesterol)
heart1_Sin_Out_Cholesterol <- Cholesterol[Cholesterol >0 & Cholesterol < 404]
boxplot(heart1_Sin_Out_Cholesterol, horizontal=TRUE)
boxplot.stats(heart1_Sin_Out_Cholesterol)

heart2_Sin_out_Cholesterol2 <- heart1_Sin_Out_Cholesterol[heart1_Sin_Out_Cholesterol > 110 & heart1_Sin_Out_Cholesterol< 369]
boxplot(heart2_Sin_out_Cholesterol2, horizontal=TRUE)

#FIltrado del dataframe mediante paquete dplyr para eliminar outliers de CHholesterol
heart2 <- filter(heart1, Cholesterol > 110 & Cholesterol < 369)
str(heart2)
head(heart2)

###Atributo MaxHR
MaxHR <- heart2$MaxHR
boxplot(MaxHR, horizontal=TRUE)
boxplot.stats(MaxHR)

##No es necesario el filtrado puesto que el actual dataframe(heart2) no los contiene para este atributo
str(heart2)

###Atributo OldPeak
OldPeak <- heart2$Oldpeak
boxplot(OldPeak, horizontal=TRUE)
boxplot.stats(OldPeak)
heart2_Sin_out_OldPeak <- OldPeak[OldPeak < 3.6]
boxplot(heart2_Sin_out_OldPeak, horizontal=TRUE)
boxplot.stats(heart2_Sin_out_OldPeak)

#Filtrado del dataframe mediante paquete dplyr para eliminar outliers de Oldpeak
heart3<- filter(heart2, OldPeak < 3.6)

##Dataset reducido sin outliers
str(heart3)
View(heart3)

boxplot(heart3$RestingBP, horizontal = TRUE)
boxplot(heart3$Cholesterol, horizontal = TRUE)
boxplot(heart3$MaxHR, horizontal = TRUE)
boxplot(heart3$Oldpeak, horizontal = TRUE)



