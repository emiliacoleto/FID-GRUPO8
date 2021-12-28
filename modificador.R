# Leemos los datos del CSV
data <- read.csv('heart.csv')

# Creamos los datos de forma aleatoria
weigth <- sample(c(6000:15000), 918, replace = TRUE)/100
heigth <- sample(c(140:200), 918, replace = TRUE)/100
countries <- c('Spain', 'France', 'Germany', 'England', 'Portugal', 'Cuba', 'China', 'South Africa', 'Italy')
country <- sample(countries, 918, replace = TRUE)

# Los agregamos al dataframe
data$weigth <- weigth
data$heigth <- heigth
data$country <- country

# Visualizamos los datos
View(data)

# Exportamos los nuevos datos a un CSV
write.csv(data,"heart3.csv", row.names = FALSE)
