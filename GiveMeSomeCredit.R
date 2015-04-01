# PRIMERA VERSION - HACER REFACTOR CODIGO DESPUES

library(ggplot2)
library(nortest)
library(dplyr)

########################################
#####   DECLARACION  FUNCIONES    ######
########################################

calculate.max.whisker <- function(variable){
  # Devuelve el max.whisker de la variable pasada como parametro
  quantiles <- summary(variable)
  RIC <- quantiles[['3rd Qu.']] - quantiles[['1st Qu.']]
  return(quantiles[['3rd Qu.']] + 1.5*RIC)
}

calculate.min.whisker <- function(variable){
  # Devuelve el min.whisker de la variable pasada como parametro
  quantiles <- summary(variable)
  RIC <- quantiles[['3rd Qu.']] - quantiles[['1st Qu.']]
  return(quantiles[['1st Qu.']] - 1.5*RIC)
}

# Cargamos los datos de Kaggle
train.data <- read.csv(file = './data/cs-training.csv', header = TRUE, sep = ',')
testing.data <- read.csv(file = './data/cs-test.csv', header = TRUE, sep = ',')

#Cambiamos los tipos de las columnas del dataset acorde a la especificacion
train.data$SeriousDlqin2yrs <- as.factor(train.data$SeriousDlqin2yrs)

#Cambiamos el nombre de la columna que tiene el numero de fila
colnames(train.data)[1] <- c('Fila')

##################################################################
#########   PREPARACION DATOS ###################################
##################################################################

###### TRATAMIENTO NAS

#Identificamos aquellas columnas que tienen algun NA
str(lapply(train.data, function(col) any(is.na(col))))
#Vemos que las unicas features que tienen NA son 'MonthlyIncome' y 'NumberofDependents'

#Para ver como rellenar estos NAs, vamos a realizar un PCA del dataset inicial 
#eliminando las filas con NAs para ver si hay alguna feature
#que tenga alta correlacion con una componente principal y utilizar estas features para rellenar los NAs
#Quitamos tambien la variable a predecir
data.no.nas <- train.data[,-1:-2]
data.no.nas <- data.no.nas[apply(data.no.nas, 1, function(row){all(!is.na(row))}),]
pr.out = prcomp(data.no.nas, scale=TRUE)

#Vemos el porcentaje de variabilidad explicada y vemos que las 4 primeras PC cubren el 67.47%
pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var)
cumsum(pve) * 100
plot(pve, type = 'b')

#Analizamos las correlaciones de las features con las 4 PCs
pr.out$rotation[,1:4]

#Vemos que las correlaciones significativas son:
# NumberOfOpenCreditLinesAndLoans con PC2 (64.6%)
# NumberRealEstateLoansOrLines con PC2 (63.7%)
# age con PC3 (65.5%)

# Utilizaremos estas 3 variables como modelo para rellenar los NAs
# tanto de 'MonthlyIncome' como de 'NumberofDependents'

#########################################################################

#MonthlyIncome
#Filtramos dataset quitando los NA de income en esta feature
income.no.nas <- train.data[!is.na(train.data$MonthlyIncome),]
income.nas <- train.data[is.na(train.data$MonthlyIncome),]


#Aplicar para todas las filas
apply(income.nas, 1, )


#Para la 1º fila de income.nas
test <- income.no.nas[income.no.nas$age == eval(income.nas[1,]$age) &
                      income.no.nas$NumberOfOpenCreditLinesAndLoans == eval(income.nas[1,]$NumberOfOpenCreditLinesAndLoans) &
                      income.no.nas$NumberRealEstateLoansOrLines == eval(income.nas[1,]$NumberRealEstateLoansOrLines),]

  
#Pasamos test de normalidad (Confianza de un 0.5)
#   SI --> Rellenamos con la media
#   NO --> Rellenamos con la mediana
  
rellenar <- round(ifelse(lillie.test(test$MonthlyIncome)$p.value > 0.05, 
                           mean(test$MonthlyIncome), median(test$MonthlyIncome)))
  
  
  

  
  
  
  
  
  
  fill.column <- function(df, df.to.fill, column.to.fill, ...){
    #Rellena la columna pasada como parametro en base a los valores a filtrar
    #con los mismos parametros que el dataframe original
    column.filter <- list(...)
    
    #Comprobamos que se cumplen todas las condiciones de column.filter
    #para la fila que se esta evaluando
    lapply(column.filter, df$feature == df.to.fill$feature)
    
    #Filtramos con dplyr con filter con las condiciones del column.to.fill
    #Si tenemos datos --> devolvemos el monthlyIncome
    #No tenemos datos --> devolvemos NA para tratar despues
    
    filter(studentdata, Drink == "water")
    
    
    
    row.to.fill <- df[df$column.filter[1] == ,
                      df$column.filter[1]]
    
  }  
  
  
  
  
  
  
  
  
  

#Rellenamos todas las features para aquellos registros que tengan la combinacion
#de los 3 valores en sus datos

#En el caso de que no se de esto, seguiremos la escala de porcentajes para rellenar
#es decir, sobreponderaremos age, luego NumberOfOpenCreditLinesAndLoans y finalmente
#NumberRealEstateLoansOrLines



all(sort(unique(income.nas$age)) %in% sort(unique(income.no.nas$age)))
all(unique(income.nas$NumberOfOpenCreditLinesAndLoans) %in% unique(income.no.nas$NumberOfOpenCreditLinesAndLoans))
all(unique(income.nas$NumberRealEstateLoansOrLines) %in% unique(income.no.nas$NumberRealEstateLoansOrLines))






