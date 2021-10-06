

# Librerías
library(tidyverse)
diabetes <- read.csv("Diabetes.csv", sep = ";")

head(diabetes)

# EDA

summary(diabetes)
any(is.na(diabetes)) # No hay datos missing

apply(diabetes[,-diabetes$DIABETES],2, function (x){
  ggplot(data = diabetes, mapping = aes(x = diabetes$DIABETES, y = x, fill = diabetes$DIABETES)) + xlab(c)+
    geom_boxplot()+
    facet_wrap(~DIABETES)
    # geom_jitter(width = 0.1, alpha = 0.3)
  
})

 

 
 