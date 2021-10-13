

# Librerías
library(tidyverse)
library(GGally) # Gráficos de correlación
library(caret)
library(ggplot2)
library(ROCR)
library(MLTools)
# install.dependencies()
diabetes <- read.csv("Diabetes.csv", sep = ";")

#### DATOS #### 
head(diabetes)
dim(diabetes) # 768 x 9
any(is.na(diabetes)) # No hay datos missing

####  EDA #### 

####  Resumen numérico #### 

summary(diabetes)
# Desviación de cada variable
signif(apply(diabetes, 2, sd),4)

# Convertimos Y en factor
diabetes$DIABETES <- factor(diabetes$DIABETES, labels = c("No", "Si"))

####  Resumen gráfico #### 

#### BOXPLOTS ####
plots <-   imap(select( diabetes,PREGNANT:AGE), ~ {
  ggplot(diabetes, aes(x = DIABETES,
                       y = .x, fill = DIABETES)) + 
    geom_boxplot() +
    xlab("DIABETES")+ 
    theme(legend.position = "top", legend.direction = "horizontal")+ 
    labs (fill = ' DIABETES ') +
    ylab(.y)
})

# Muestra los gráficos
lapply(plots, function(x) print(x))


########################### Todo esto se puede omitir ##########################################################
#### HISOGRAMAS ####
HIST <- imap(select( diabetes,PREGNANT:AGE), ~ {
  ggplot(diabetes, aes(
    x = .x, fill = DIABETES)) + 
    geom_histogram(aes(y=stat(density))) +
    geom_density(color= c("red"), size=1.5, alpha = 0.2) +
    xlab("DIABETES")+ 
    theme(legend.position = "top", legend.direction = "horizontal")+ 
    labs (fill = ' DIABETES ') +
    xlab(.y)
})



# Muestra los gráficos
lapply(HIST, function(x) print(x))

# Vemos si está balanceada la variable respuesta
ggplot(data = diabetes) + 
  geom_bar(mapping = aes(x = DIABETES,
                         fill = factor(DIABETES, labels = c("No", "Si"))))+ 
  labs (fill = ' DIABETES ')+ 
  xlab("DIABETES")+ theme_bw()+ 
  theme(legend.position = "top", legend.direction = "horizontal")

#########################################################################################################

# Esta funcion hace lo que yo hice arriba
#Function for plotting multiple plots between the output of a data frame
#and the predictors of this output.
PlotDataframe(fdata = diabetes, 
              output.name = "DIABETES")


#### BIVARIATE ANALYSIS ####
ggpairs(diabetes, aes(color = DIABETES, alpha = 0.2))

# Correlación entre variables explicativas para evitar multicolinealidad
ggcorr(diabetes[, -ncol(diabetes)], method = c("everything", "pearson")) 


#### ALGORITMOS DE MACHINE LEARNING ####

#Creación del train y test set y sus conjuntos de evaluación
set.seed(150) 
trainIndex <- createDataPartition(diabetes$DIABETES, p = 0.8, list = FALSE, times = 1) 
train_set <- diabetes[trainIndex,]
test_set <- diabetes[-trainIndex,]
train_set_eval <- train_set
test_set_eval <- test_set
str(train_set)

#Control del train 
ctrl <- trainControl(method = "cv", number = 10,summaryFunction = defaultSummary, classProbs = TRUE) 

#Regresión logística

# No hace falta esto ya
# train_set <- train_set %>%
#   mutate(DIABETES = factor(DIABETES == 1, levels = c(TRUE, FALSE), labels = c("Havediabetes", "Donthavediabetes")))

LogReg.fit <- train(form = DIABETES ~ ., 
                    data = train_set,              
                    method = "glm",                   
                    preProcess = c("center","scale"), 
                    trControl = ctrl,                 
                    metric = "Accuracy") 
LogReg.fit
summary(LogReg.fit) 
str(LogReg.fit) 
boxplot(LogReg.fit$resample$Accuracy, xlab = "Accuracy")

#Evaluacuón del modelo
train_set_eval$LRprob <- predict(LogReg.fit, type="prob", newdata = train_set) # predict probabilities
train_set_eval$LRpred <- predict(LogReg.fit, type="raw", newdata = train_set) # predict classes 
#test
test_set_eval$LRprob <- predict(LogReg.fit, type="prob", newdata = test_set) # predict probabilities
test_set_eval$LRpred <- predict(LogReg.fit, type="raw", newdata = test_set) # predict classes 

head(train_set)

# En los modelos, se suele partir del más complejo al más simple
# o viceversa (hacer glm univariados para ver rqué variables entran a mi modelo)
# En el modelo anterior, la variable SKINTHICKNESS era la que tenía unp-valor más alto
# por tanto, puede ser que esté metiendo ruido al modelo.

# Planteamos un modelo de reg log excluyendo dicha variable:

LogReg.fit2 <- train(form = DIABETES ~ PREGNANT + GLUCOSE + BLOODPRESS + INSULIN
                     + BODYMASSINDEX + PEDIGREEFUNC +AGE, 
                    data = train_set,              
                    method = "glm",                   
                    preProcess = c("center","scale"), 
                    trControl = ctrl,                 
                    metric = "Accuracy")
summary(LogReg.fit2) 
 



LogReg.fit3 <- train(form = DIABETES ~ PREGNANT + GLUCOSE + BLOODPRESS + INSULIN
                     + BODYMASSINDEX + PEDIGREEFUNC, 
                     data = train_set,              
                     method = "glm",                   
                     preProcess = c("center","scale"), 
                     trControl = ctrl,                 
                     metric = "Accuracy")
summary(LogReg.fit3) 

LogReg.fit4 <- train(form = DIABETES ~ PREGNANT + GLUCOSE + BLOODPRESS 
                     + BODYMASSINDEX + PEDIGREEFUNC, 
                     data = train_set,              
                     method = "glm",                   
                     preProcess = c("center","scale"), 
                     trControl = ctrl,                 
                     metric = "Accuracy")
summary(LogReg.fit4)

# Se coge el de menor AIC (pero hay que tener cuidado porque cuando no hay mucha
# diferencia, se tiende a coger el más sencillo o según las variables que recomiende
# el experto. En este caso, por ejemplo, la insulina no sé yo si estaría bien quitarla)

# PREGUNTAR EN CLASE SI PODEMOS HACER ESTO QUE HE HECHO DE SELECCIÓN DE VARIABLES
AIC(LogReg.fit4$finalModel, LogReg.fit3$finalModel, LogReg.fit2$finalModel, LogReg.fit$finalModel)

# Nos quedaríamos con el modelo más simple y evaluaríamos en el test