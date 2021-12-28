
##Análisis Básico -> PREPROCESAMIENTO DE DATOS y Visualización##

##Instalación de Tidyverse
install.packages("tidyverse")

heart <- read.csv('heart.csv')
View(heart)

##Deteccion de outliers
str(heart) #Contenido de heart.csv



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

boxplot(heart$Age, horizontal = TRUE)
boxplot(heart$RestingBP, horizontal = TRUE) #Hay outliers.
boxplot(heart$Cholesterol,  horizontal = TRUE) #hay outliers. 
boxplot(heart$MaxHR, horizontal=TRUE)#hay 2 outliers
boxplot(heart$Oldpeak, horizontal=TRUE)#hay outliers

