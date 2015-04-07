library(ggplot2)
library(nortest)
library(modeest)
library(dplyr)
library(caret)
library(multilevelPSA)

########################################
##########  CARGA DE DATOS   ###########
########################################

# Cargamos los datos de Kaggle
train.data <- read.csv(file = './data/cs-training.csv', header = TRUE, sep = ',')

########################################################
##########  MODELO REGRESION MONTHLYINCOME   ###########
########################################################

# Vamos a usar el modelo Stochastic Gradient Boosting 'gbm'
# para predecir el MonthlyIncome en base a las demas variables,
# obviando en este caso la variable 'NumberOfDependents' 
# para evitar biases ya que habría que completar los NAs previamente
train.data <- train.data[,3:length(train.data)]
train.data <- train.data[, !names(train.data) %in% c('NumberOfDependents')]

#Vemos que solo MonthlyIncome tiene NAs
str(lapply(train.data, function(col) any(is.na(col))))

#Para crear el conjunto de train y test:
# 1) Para el conjunto de train, filtraremos el dataset con aquellas
#    filas que no tengan NAs en MonthlyIncome para despues hacer
#    CrossValidation con una proporción 80/20
# 2) Utilizamos el modelo entrenado para predecir aquellos valores que tienen NAs en MonthlyIncome 
training.data <- train.data[!is.na(train.data$MonthlyIncome),]
data.to.predict <- train.data[is.na(train.data$MonthlyIncome),]
  
#Creamos ambos conjuntos de training y test
inTrain <- createDataPartition(training.data$MonthlyIncome, p=0.8, list=FALSE)
training.regression <- training.data[inTrain,]
testing.regression <- training.data[-inTrain,]

################ MODELOS CON TODOS LOS ALGORITMOS

#Configuramos el TrainControl
BootControl <- trainControl(method = 'cv', number = 10, repeats = 3)

#Training del modelo gbm
gbm.regression <- train(MonthlyIncome ~ ., training.regression, 
                        method='gbm', trControl = BootControl, tuneLength = 10)

plot(gbm.regression)

#Pintamos los residuales para ver el error
#La mayoria de los valores son próximos a 0
plot(resid(gbm.regression))

#Predecimos con testing.regression para valorar el modelo entrenado
pred.test <- predict(gbm.regression$finalModel, testing.regression[, -5], 
                     n.trees=gbm.regression$bestTune$n.trees)

#RMSE y el Error cuadrático medio con los datos de testing
modelvalues<-data.frame(obs = testing.regression$MonthlyIncome, pred=pred.test)
defaultSummary(modelvalues)

#Ateniendonos al valor de Rsquared de 0.247,
#no parece una gran aproximación 

################ PREDICCION VALORES DE MONTHLY INCOME

#Ahora predecimos el data.to.predict para rellenar la variable MonthlyIncome
pred.validation <- predict(gbm.regression$finalModel, data.to.predict[, -5], 
                           n.trees=gbm.regression$bestTune$n.trees)
