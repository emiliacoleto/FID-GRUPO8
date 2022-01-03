#------------------------------------------------------------------
# Ejercicio 1. Puntos 1,2,3
#------------------------------------------------------------------
# Preparamos los conjuntos de train y test.
train<-read.delim("data/train.csv", sep = "\t", head = TRUE)
dim(train)
colnames(train)
rownames(train)[1:5]
train[1:5, 1:5]

rownames(train) <- train$id
train$id <- NULL
head(train)
colnames(train)
rownames(train)[1:5]

# Set random seed. 
set.seed(1)
  
# Carga de rpart, rattle, rpart.plot y RColorBrewer 
install.packages("rattle")

library(rpart)
library(rattle)
library(RColorBrewer)


tree <- rpart(Survived~., train, method = "class")

# Plot del árbol
fancyRpartPlot(tree)

#------------------------------------------------------------------
# Ejercicio 1. Punto 4
#------------------------------------------------------------------
test<-read.delim("data/test.csv", sep = "\t", head = TRUE)
dim(test)
colnames(test)
rownames(test)[1:5]

rownames(test) <- test$id
test$id <- NULL
dim(test)
colnames(test)
rownames(test)[1:5]

# Predice los valores del conjunto de test
pred <- predict(tree, test, type = "class")

# Construye la matriz de confusion
conf <- table(test$Survived, pred)

# Calcula accuracy
acc <- sum(diag(conf)) / sum(conf)
print(acc)

#------------------------------------------------------------------
# Ejercicio 1. Puntos 5
#------------------------------------------------------------------
# Calcula un árbol complejo
set.seed(1)
tree2 <- rpart(Survived ~ ., train, method = "class", 
               control = rpart.control(cp=0.00001))

# Dibuja árbol
fancyRpartPlot(tree2)

# Poda el árbol
pruned <- prune(tree2, cp = 0.01)

# Dibuja árbol
fancyRpartPlot(pruned)

#------------------------------------------------------------------
# Ejercicio 1. Puntos 6
#------------------------------------------------------------------

# Cambia la llamada a rpart para utilizar como heurística la ganancia de información
tree_i <- rpart(Survived ~ ., train, method = "class", 
                parms = list(split = "information"))

pred_i <- predict(tree_i, test, type = "class")
conf_i <- table(test$Survived, pred_i)
acc_i <- sum(diag(conf_i)) / sum(conf_i)
acc_i
print("La accuracy anterior con el criterio de división Gini es: ")
acc

#------------------------------------------------------------------
# Ejercicio 2. 
#------------------------------------------------------------------

# Predecimos valores de probabilidad utilizando el modelo anterior (índice gini CART)
all_probs <- predict(tree, test, type="prob")

# Print out all_probs
summary(all_probs)

# Select second column of all_probs: probs
dim(all_probs)
all_probs[1:10,2]
probs <- all_probs[,2]

#### Esto me sirve para el punto 3
# TREE_I es ID_3 porque se creo con ganancia de información como heurísticas
# Predict probability values using the model: all_probs
all_probs_i <- predict(tree_i, test, type="prob")
probs_i <- all_probs_i[,2]


#-------------------------
# Ejercicio 2. Punto 1
#-------------------------

# Carga librería ROCR 
install.packages("ROCR")
library(ROCR)

# Make a prediction object: pred
pred <- prediction(probs, test$Survived)

# Make a performance object: perf
perf <- performance(pred, "tpr", "fpr")

# Plot this curve
plot(perf)

#-------------------------
# Ejercicio 2. Punto 2
#-------------------------

# Make a performance object: perf
perf_auc <- performance(pred, "auc")

# Print out the AUC
print(perf_auc@y.values[[1]])


#-------------------------
# Ejercicio 2. Realizamos lo mismo con el ID3 (punto 3 enunciado)
#-------------------------
pred_i <- prediction(probs_i, test$Survived)

perf_i <- performance(pred_i, "tpr", "fpr")
perf_auc_i <- performance(pred_i, "auc")
print(perf_auc_i@y.values[[1]])

#draw_roc_lines(perf, perf_i)
plot(perf)
plot(perf_i)



#------------------------------------------------------------------
# Ejercicio 3. Puntos 1,2
#------------------------------------------------------------------

test<-read.delim("data/test.csv", sep = "\t", head = TRUE)
train<-read.delim("data/train.csv", sep = "\t", head = TRUE)

# Store the Survived column of train and test in train_labels and test_labels
train_labels <- train$Survived
test_labels <- test$Survived

# Copy train and test to knn_train and knn_test
knn_train <- train
knn_test <- test

# Drop Survived and id column for knn_train and knn_test
knn_train$Survived <- NULL
knn_train$id <- NULL
knn_test$Survived <- NULL
knn_test$id <- NULL

#------------------------------------------------------------------
# Ejercicio 3. Punto 2
#------------------------------------------------------------------


# Normalize Pclass
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

# Normalize Age
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age) / (max_age - min_age)

#------------------------------------------------------------------
# Ejercicio 3. Punto 3
#------------------------------------------------------------------

set.seed(1)

# Load the class package
library(class)

knn_train$Sex <- as.numeric(knn_train$Sex)
knn_test$Sex <- as.numeric(knn_test$Sex)

pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 5)

# Construct the confusion matrix: conf
tab <- table(test_labels, pred)

# Print out the confusion matrix
tab
acc_knn <- sum(diag(tab)) / sum(tab)
print(acc_knn)

#------------------------------------------------------------------
# Ejercicio 3. Puntos 4
#------------------------------------------------------------------
set.seed(1)

# Load the class package, define range and accs
library(class)
range <- 1:round(0.2 * nrow(knn_train))
accs <- rep(0, length(range))

for (k in range) {
  
  pred <- knn(knn_train, knn_test, train_labels, k = k)
  
  conf <- table(test_labels, pred)
  
  accs[k] <- sum(diag(conf)) / sum(conf)
}

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab = "k")

# Calculate the best k
which.max(accs)
accs[which.max(accs)]
