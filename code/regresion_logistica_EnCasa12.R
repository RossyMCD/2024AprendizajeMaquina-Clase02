
# rm(list=ls())

#########################################################################
### -- Maestria en Ciencia de Datos -- ## 
#########################################################################
### Autor: MSc. Jose Cardenas  ## 
### Tema: Regresion Logistica aplicada a negocios  ## 

#########################################################################

# setwd("C:/Users/josec/OneDrive/Escritorio/Maestria de Ciencia de Datos/Aprendizaje de maquina/Sesión 2/data")

########### 1) LIBRERIAS A UTILIZAR ################# 

library(dplyr)
library(MLmetrics)
library(party)
library(pROC)
library(lattice)
library(corrplot)
library(mlr)
library(sqldf)
library(ggplot2)
library(klaR)

########### 2) DATA A UTILIZAR ################# 

# train <- read.csv("german_credit_data_train.csv")
# test <- read.csv("german_credit_data_test.csv")
train <- read.csv("C:/Users/josec/OneDrive/Desktop/Maestria2023/2024_01/4AprendizajeMaquina/1.Clases/12Clase2024_04_07Domingo/DesarrolladoEnClase/sesion 2/data/german_credit_data_train.csv") %>% as_tibble()
test <- read.csv("C:/Users/josec/OneDrive/Desktop/Maestria2023/2024_01/4AprendizajeMaquina/1.Clases/12Clase2024_04_07Domingo/DesarrolladoEnClase/sesion 2/data/german_credit_data_test.csv") %>% as_tibble()

dim(train)
dim(test)



########### 3) TRATAMIENTO DE LA DATA ################# 

## en primer lugar ver el analisis descriptivo de la data

resumen <- data.frame(mlr::summarizeColumns(train))
# write.csv(resumen,"tabla_resumen.csv")


resumen
# View(train)
glimpse(train)

library(elucidate)
plot_var_all(train)





# NAs

# install.packages("mice")
library(mice)

md.pattern(train, rotate.names = TRUE)


########### 4) PRIMER ANALISIS  ################# 

## generamos una copia de la data original

data_train <- train

## imputar en primer lugar la data

data_train <- mlr::impute(train, classes = list(factor = imputeMode(), 
                                                  integer = imputeMode(),
                                                  numeric = imputeMedian(),
                                                   character = imputeMode()))
data_train <- data_train$data[,1:ncol(train)]


# Var la data después de imputar
#--------------------------------
md.pattern(data_train, rotate.names = TRUE)


## luego dar un orden a las variables

summary(data_train)

md.pattern(data_train) # Ya no hay NAs

resumen2 <- data.frame(mlr::summarizeColumns(data_train))

## Vamos a guardar las variables cuantitativas en "cuantis"
cuantis <- data_train %>% dplyr::select(Age,Credit_amount,Duration)
## Vamos a guardar las variables cualitativas en "cualis"
cualis <- data_train %>% dplyr::select(-Age,-Credit_amount,-Duration)


# El profesor ordena el tipo de datos
# los cuanti primero
# los cuali segundo
data_train <- cbind(cuantis,cualis)




# recodificacion manual
data_train$Sex <- ifelse(data_train$Sex=='male',2,1)
data_train$Housing <- ifelse(data_train$Housing == 'own',1,
                             ifelse(data_train$Housing =='rent', 2,3))
data_train$Saving_accounts <- ifelse(data_train$Saving_accounts == 'little',1,
                                     ifelse(data_train$Saving_accounts == 'moderate',2,
                                            ifelse(data_train$Saving_accounts == 'rich',3,4)))
data_train$Checking_account <- ifelse(data_train$Checking_account == 'little',1,
                                      ifelse(data_train$Checking_account == 'moderate',2,3))

data_train$Purpose <- ifelse(data_train$Purpose == 'car',1,
                             ifelse(data_train$Purpose == 'furniture/equipment',2,
                                    ifelse(data_train$Purpose == 'radio/TV',3,
                                           ifelse(data_train$Purpose == 'domestic appliances',4,
                                                  ifelse(data_train$Purpose == 'repairs',5,
                                                         ifelse(data_train$Purpose == 'education',6,
                                                                ifelse(data_train$Purpose == 'business',7,8)))))))

data_train$Risk <- ifelse(data_train$Risk=='bad',1,0)
summary(data_train)

glimpse(data_train)
plot_var_all(data_train)




# Antes de convertir a factor las variables
# se calcula las correlaciones

## ver correlacion antes de categorizar

# source("C:/Users/Alumno/Downloads/AprendizajeMaquina/sesion 2/codigo/funciones.R")
source("C:/Users/josec/OneDrive/Desktop/Maestria2023/2024_01/4AprendizajeMaquina/1.Clases/12Clase2024_04_07Domingo/DesarrolladoEnClase/sesion 2/codigo/funciones.R")


corre <- cor(data_train,method = c("spearman"))
corre

library(corrplot)
corrplot(corre)

corrplot(corre
         , addCoef.col = TRUE
         , number.cex = .6
         , diag = FALSE
         , type = "lower"
         )



## colocamos la primera funcion de correlacion
corre <- correlacionS(corre)
corre$filtro <- ifelse(abs(corre$cor)>0.6,1,0)

# write.csv(corre,"correlacion_variables.csv",row.names = F)





# categorizando a factor

data_train[,(ncol(cuantis)+1):ncol(data_train)] <- lapply(data_train[,(ncol(cuantis)+1):ncol(data_train)],as.factor)
summary(data_train)
glimpse(data_train)









## primer modelo logistica ##

# retiramos la variable correlacionada

data.frame(names(data_train))


formula <-    Risk ~
              Age+
         Duration+
              Sex+
              Job+
          Housing+
  Saving_accounts+
 Checking_account+
          Purpose
              
modelo1 <- glm(formula,data=data_train,family = binomial() )

# ver la significancia del modelo
options(scipen = 666)
summary(modelo1)

# purpose2 aumenta
# entonces purpose3 aumenta

# Concepto: Monotonia!!!





# pesos del modelo importancia mediante la tercera funcion
# Aplica la metodologia Z-wald
pesos(modelo1)

# Duration: aporta 41% del modelo
# Es decir, si retiras "Duration" se cae el modelo



# Lo ideal es que haya una distribución equitativo de las variables que entran al modelo
# Si fueran 5 variables, se quisiera que cada una ellas aporte el 20%, pero en la realidad no sucede así


# Lo malo sería que solo una variable acumule el 90%



# indicadores mediante la segunda funcion
calcula_indicadores(modelo1)











# SEGUNDO MODELO, retirando variables

## reajustando

formula <-
  Risk ~
  Age+
  Duration+
  Sex+
  #Job+
  Housing+
  Saving_accounts+
  Checking_account+
  Purpose

## en su paper luego de escoger el modelo ver una propuesta de un indicador
## del PSI otra forma es de ver un CV

modelo2 <- glm(formula,data=data_train,family = binomial())

# indicadores mediante la segunda funcion
calcula_indicadores(modelo2)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo2)

# ver la significancia del modelo

summary(modelo2)

# Interpretación:
# Como variables te aporta, pero debes ordenar las categorias!!!










# GRAFICOS
## representacion de las variables univariadamente

modelovar <- glm(Risk~Age,data=data_train,family = binomial() )
summary(modelovar)

data_train$prob <- predict(modelovar,
                           newdata = data.frame(Age = data_train$Age),
                           type = "response")

ggplot(data_train, aes(x=Age, y=prob )) + 
  geom_line(colour="blue") 

# Interpretación.
# A mayor edad entonces mayor probabilidad de riesgo!!!!!





## representacion de las variables multivariadamente
datosF <- sqldf::sqldf("select     
Age,
Duration,
Sex,
Housing,
Saving_accounts,
Checking_account,
Purpose 
from data_train")
datosF$probabilidades_result1 <- predict(modelo2, newdata = datosF, type = "response")

tabla <- reshape2::melt(datosF, 
  id.vars = c(
    "Age",
    "Duration",
    "Sex",
    "Housing",
    "Saving_accounts",
    "Checking_account",
    "Purpose" 
  ), value.name = "probability")

# Grafico de las probabilidades

ggplot2::ggplot(tabla, aes(x = Duration, 
                           y = probability)) + geom_line() + 
  facet_grid(variable ~., scales = "free")

ggplot2::ggplot(tabla, aes(x = Duration, 
                           y = probability, colour = Sex)) + geom_line() + 
  facet_grid(variable ~., scales = "free")

ggplot2::ggplot(tabla, aes(x = Duration, 
                           y = probability, colour = Checking_account)) + geom_line() + 
  facet_grid(variable ~., scales = "free")

## Ejercicio

# Crear un modelo tratando de que todas su categorias esten con
# monotonia y significativas sus coeff



















########### 5) SEGUNDO ANALISIS  #################

data_train_2 <- train

glimpse(train)
summarizeColumns(train)
head(train)


cuantis <- data_train_2 %>% dplyr::select(Age,Credit_amount,Duration)
cualis <- data_train_2 %>% dplyr::select(-Age,-Credit_amount,-Duration)

data_train_2 <- cbind(cuantis,cualis)

# recodificacion manual
data_train_2$Sex <- ifelse(data_train_2$Sex=='male',2,1)
data_train_2$Housing <- ifelse(data_train_2$Housing == 'own',1,
                             ifelse(data_train_2$Housing =='rent', 2,3))
data_train_2$Saving_accounts <- ifelse(data_train_2$Saving_accounts == 'little',1,
                                     ifelse(data_train_2$Saving_accounts == 'moderate',2,
                                            ifelse(data_train_2$Saving_accounts == 'rich',3,4)))
data_train_2$Checking_account <- ifelse(data_train_2$Checking_account == 'little',1,
                                      ifelse(data_train_2$Checking_account == 'moderate',2,3))

data_train_2$Purpose <- ifelse(data_train_2$Purpose == 'car',1,
                             ifelse(data_train_2$Purpose == 'furniture/equipment',2,
                                    ifelse(data_train_2$Purpose == 'radio/TV',3,
                                           ifelse(data_train_2$Purpose == 'domestic appliances',4,
                                                  ifelse(data_train_2$Purpose == 'repairs',5,
                                                         ifelse(data_train_2$Purpose == 'education',6,
                                                                ifelse(data_train_2$Purpose == 'business',7,8)))))))

data_train_2$Risk <- ifelse(data_train_2$Risk=='bad',1,0)

data_train_2[,(ncol(cuantis)+1):ncol(data_train_2)] <- lapply(data_train_2[,(ncol(cuantis)+1):ncol(data_train_2)],as.factor)
summary(data_train_2)


glimpse(data_train)






# utilizar la el codigo de funcion2 para recodificar mediante arboles
# CREA MULTIPLES PDFs Y TE DA LA VARIABLE: tabla_ks_gini
# Cada uno de ellos representa cada variables
# "C:\Users\josec\OneDrive\Desktop\Maestria2023\2024_01\4AprendizajeMaquina\1.Clases\12Clase2024_04_07Domingo\DesarrolladoEnClase\sesion 2\codigo\funciones2.R"source("C:/Users/Alumno/Downloads/AprendizajeMaquina/sesion 2/codigo/funciones2.R")

# source("C:/Users/josec/OneDrive/Escritorio/Maestria de Ciencia de Datos/Aprendizaje de maquina/Sesión 2/codigo/funciones2.R")

source("C:/Users/josec/OneDrive/Desktop/Maestria2023/2024_01/4AprendizajeMaquina/1.Clases/12Clase2024_04_07Domingo/DesarrolladoEnClase/sesion 2/codigo/funciones2.R")


tabla_ks_gini




# Correr esta linea para que guarde un archivo csv
write.csv(file = "tabla_ks_gini.csv", tabla_ks_gini,row.names = T)
















## recodificacion mediante arboles Chaid
# Realizando la codificación que nos indica los arboles


data_train_2_f <- sqldf(" 
select
case 
when Duration is null then 999
when Duration <=11 then 1
when Duration <=33 then 2
else 3 end Duration_cat,
case
when Credit_amount is null then 999
when Credit_amount <=3913 then 1
when Credit_amount <=6742 then 2
else 3 end Credit_amount_cat,
case
when Checking_account is null then 999
when Checking_account =3 then 1
when Checking_account =2 then 2
else 3 end Checking_account_cat,
case
when Housing is null then 999
when Housing in (2,3) then 2
else 1 end Housing_cat,
case
when Age is null then 999
when Age <= 25 then 2
else 1 end Age_cat,
case
when Saving_accounts is null then 999
when Saving_accounts in (3,4) then 1
else 2 end Saving_accounts_cat,
Sex,
Risk
from data_train_2")

summary(data_train_2_f)

# realizamos el analisis univariado mediante riesgo

exp <- sqldf("select 
          Duration_cat,
     Credit_amount_cat,
  Checking_account_cat,
           Housing_cat,
               Age_cat,
   Saving_accounts_cat,
                   Sex,
                   sum(Risk) riesgo,
             count(*) casos
             from data_train_2_f
             group by
      Duration_cat,
     Credit_amount_cat,
  Checking_account_cat,
           Housing_cat,
               Age_cat,
   Saving_accounts_cat,
                   Sex")

write.csv(exp,"dataagrupada_segundo_analisis.csv",row.names = F)






# realizamos la recodificacion segun riesgo del excel

# sE REALIZA LA RECODIFICACIÓN EN BASE AL EXCEL

data_train_2_f <- sqldf(" 
select
case 
when Duration is null then 999
when Duration <=11 then 1
when Duration <=33 then 2
else 3 end Duration_cat,
case
when Credit_amount is null then 999
when Credit_amount <=3913 then 1
when Credit_amount <=6742 then 2
else 2 end Credit_amount_cat,
case
when Checking_account is null then 1
when Checking_account =3 then 1
when Checking_account =2 then 2
else 3 end Checking_account_cat,
case
when Housing is null then 999
when Housing in (2,3) then 2
else 1 end Housing_cat,
case
when Age is null then 999
when Age <= 25 then 2
else 1 end Age_cat,
case
when Saving_accounts is null then 1
when Saving_accounts in (3,4) then 1
else 2 end Saving_accounts_cat,
Sex,
Risk 
from data_train_2")

summary(data_train_2_f)

data_train_2_f$Sex <- as.numeric(as.character(data_train_2_f$Sex))
data_train_2_f$Risk <- as.numeric(as.character(data_train_2_f$Risk))




corre <- cor(data_train_2_f,method = c("spearman"))




## colocamos la primera funcion de correlacion


corre <- correlacionS(corre)
corre$filtro <- ifelse(abs(corre$cor)>0.6,1,0)



write.csv(corre,"correlacion_variables_2.csv",row.names = F)






# categorizamos la data total

data_train_2_f <- lapply(data_train_2_f,as.factor)

## realizamos el segundo modelo logistico

formula <-
                Risk~
        Duration_cat+
   Credit_amount_cat+
Checking_account_cat+
         Housing_cat+
             Age_cat+
 Saving_accounts_cat+
                 Sex
                

modelo3 <- glm(formula,data=data_train_2_f,family = binomial() )

# indicadores mediante la segunda funcion
calcula_indicadores(modelo3)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo3)

# ver la significancia del modelo
summary(modelo3)



# INTERPRETACION:
# - vemos que todos son significativos
# - hay una lógica de monotonia en las categoria de cada variable!!




# reajustando de acuerdo a los pesos
formula <-
  Risk~
  Duration_cat+
  Credit_amount_cat+
  Checking_account_cat+
  #Housing_cat+
  #Age_cat+
  Saving_accounts_cat+
  Sex

modelo4 <- glm(formula,data=data_train_2_f,family = binomial())

# indicadores mediante la segunda funcion
calcula_indicadores(modelo4)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo4)

# ver la significancia del modelo
summary(modelo4)




## Ahora Hagamos negocio
proba <- as.data.frame(modelo4$fitted.values)
colnames(proba)<-'probabilidades'
data_train_2_f$probabilidades <- proba$probabilidades

data_train_2_f<-as.data.frame(data_train_2_f)

# formando los 5 perfiles para estrategias
cortes <- quantile(data_train_2_f$probabilidades,probs=c(0.20,0.4,0.6,0.8))

data_train_2_f$perfiles <- ifelse(data_train_2_f$probabilidades<=as.numeric(cortes[1]),1,
                           ifelse(data_train_2_f$probabilidades<=as.numeric(cortes[2]),2,
                           ifelse(data_train_2_f$probabilidades<=as.numeric(cortes[3]),3,
                           ifelse(data_train_2_f$probabilidades<=as.numeric(cortes[4]),4,5))))

data_train_2_f$perfiles <- as.numeric(data_train_2_f$perfiles)













# ahora agrupamos y enviamos a un excel para su mejor vista

datafinal <- data_train_2_f %>% select( 
  Duration_cat,
  Credit_amount_cat,
  Checking_account_cat,
  Saving_accounts_cat ,
  probabilidades,
  perfiles,
Risk)

datafinal$operaciones <- 1

write.csv(datafinal,"pruebafinal.csv",row.names = F)

## colocando la matriz completa
 
data_ficticia <- expand.grid(Duration_cat = seq(1, 3, 1),
                               Credit_amount_cat = seq(1, 3, 1),
                               Checking_account_cat = seq(1, 3, 1),
                               Housing_cat = seq(1, 2, 1),
                               Saving_accounts_cat = seq(1, 2, 1)
                               )

data_ficticia<-lapply(data_ficticia,as.factor)

## Ahora Hagamos negocio
proba <- as.data.frame((predict.glm(modelo4,newdata = data_ficticia,type="response")))
colnames(proba)<-'probabilidades'
data_ficticia$probabilidades <- proba$probabilidades

data_ficticia<-as.data.frame(data_ficticia)

# formando los 5 perfiles para estrategias
data_ficticia$perfiles <- ifelse(data_ficticia$probabilidades<=as.numeric(cortes[1]),1,
                                  ifelse(data_ficticia$probabilidades<=as.numeric(cortes[2]),2,
                                         ifelse(data_ficticia$probabilidades<=as.numeric(cortes[3]),3,
                                                ifelse(data_ficticia$probabilidades<=as.numeric(cortes[4]),4,5))))

data_ficticia$perfiles <- as.numeric(data_ficticia$perfiles)

write.csv(data_ficticia,"dataficticia.csv",row.names = F)

### --- Metodologia mediante woe --- ###

data.frame(names(data_train_2_f))

data_woe <- klaR::woe(Risk~
                     Duration_cat+
                Credit_amount_cat+
             Checking_account_cat+
                      Housing_cat+
                          Age_cat+
              Saving_accounts_cat+
                              Sex
            ,data_train_2_f,applyontrain=TRUE)

attributes(data_woe)

## para el IV de cada variable
data_woe$IV

## el woe para cada variable
data_woe$woe

## grafico de los woes por variable para ver monotonia
plot(data_woe,type='woes')

## data transformada mediante woe
data_woe$xnew

## data consolidada

data_woe_risk <- data.frame(data_woe$xnew,Risk=data_train_2_f$Risk)

## CREACION DEL MODELO LOGIT MEDIANTE WOES

modelo5 <- glm(Risk~.,data=data_woe_risk,family = binomial())

# indicadores mediante la segunda funcion
calcula_indicadores(modelo5)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo5)

# ver la significancia del modelo
summary(modelo5)

## Modelo Ajustado

formula  <- Risk ~
  woe_Duration_cat+
woe_Credit_amount_cat+
woe_Checking_account_cat+
woe_Saving_accounts_cat+
woe_Sex

modelo6 <- glm(formula,data=data_woe_risk,family = binomial())

# indicadores mediante la segunda funcion
calcula_indicadores(modelo6)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo6)

# ver la significancia del modelo
summary(modelo6)

proba6 <- predict(modelo6,data_woe_risk,type='response')
head(proba6)
