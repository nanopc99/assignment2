

# Librerías
library(tidyverse)
library(GGally) # Gráficos de correlación
library(caret)
library(ggplot2)
library(ROCR)
library(MLTools)
install.dependencies()
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

####  Resumen gráfico #### 

#### BOXPLOTS ####
plots <-   imap(select( diabetes,PREGNANT:AGE), ~ {
  ggplot(diabetes, aes(x = factor(DIABETES, labels = c("No", "Si")),
                       y = .x, fill = factor(DIABETES, labels = c("No", "Si")))) + 
    geom_boxplot() +
    xlab("DIABETES")+ 
    theme(legend.position = "top", legend.direction = "horizontal")+ 
    labs (fill = ' DIABETES ') +
    ylab(.y)
})

# Muestra los gráficos
lapply(plots, function(x) print(x))


#### HISOGRAMAS ####
HIST <- imap(select( diabetes,PREGNANT:AGE), ~ {
  ggplot(diabetes, aes(
    x = .x, fill = factor(DIABETES, labels = c("No", "Si")))) + 
    geom_histogram(aes(y=stat(density))) +
    geom_density(color="red", size=1.5, alpha = 0.2) +
    xlab("DIABETES")+ 
    theme(legend.position = "top", legend.direction = "horizontal")+ 
    labs (fill = ' DIABETES ') +
    xlab(.y)
})



# Muestra los gráficos
lapply(HIST, function(x) print(x))

# Vemos si está balanceada la variable respuesta
ggplot(data = diabetes) + 
  geom_bar(mapping = aes(x = factor(DIABETES, labels = c("No", "Si")),
                         fill = factor(DIABETES, labels = c("No", "Si"))))+ 
  labs (fill = ' DIABETES ')+ 
  xlab("DIABETES")+ theme_bw()+ 
  theme(legend.position = "top", legend.direction = "horizontal")


#### BIVARIATE ANALYSIS ####
ggpairs(diabetes, aes(color = factor(DIABETES, labels = c("No", "Si")), alpha = 0.2))

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
train_set <- train_set %>%
  mutate(DIABETES = factor(DIABETES == 1, levels = c(TRUE, FALSE), labels = c("Havediabetes", "Donthavediabetes")))
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

