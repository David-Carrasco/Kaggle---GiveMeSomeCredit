#Cargamos los modelos entrenados del .RData

load('clean.RData')

#El mejor modelo ha sido la neural network
#Veamos cuales han sido las variables mas destacadas en el modelo
#para poder enfocarnos en que nuevas features podemos crear
plot(varImp(nn.fit))




# Resolviendo el Class Imbalance
prop.table(table(train.data$SeriousDlqin2yrs))

# MIRAR https://topepo.github.io/caret/custom_models.html