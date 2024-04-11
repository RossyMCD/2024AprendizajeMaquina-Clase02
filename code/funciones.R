
## GINI y KS

calcula_indicadores <- function(objeto_logit)
{
  objeto_logit.ks <- ks.test(x = objeto_logit$fitted.values[which(objeto_logit$y == 0)],
                             y = objeto_logit$fitted.values[which(objeto_logit$y == 1)])
    ks <- objeto_logit.ks$statistic
    objeto_logit.roc <- roc(response=objeto_logit$y,predictor=objeto_logit$fitted.values)
    objeto_logit.gini <- 2*objeto_logit.roc$auc-1
    out <- data.frame(variable="logistico",Gini=round(100*objeto_logit.gini,2),KS=round(100*ks,2))
    row.names(out) <- NULL
    return(out)
  }

## Correlaciones

correlacionS <- function(m) {
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut])
}


# Pesos

pesos <- function(objeto_logit)
{
coef_model<-data.frame(summary(objeto_logit)[["coefficients"]][, "z value"])
colnames(coef_model)<-c("t_value")
coef_model$t_value2<-coef_model$t_value^2
coef_model$variable<-rownames(coef_model)
coef_model<-coef_model[2:nrow(coef_model),]
coef_model$variable2<-substr(coef_model$variable,1,nchar(coef_model$variable)-1)
coef_model$total<-sum(coef_model$t_value2)
coef_model$part<-coef_model$t_value2/coef_model$total
datos1<-sqldf('select variable2, (sum(part))*100 as pesos 
              from coef_model 
              group by variable2 
              order by 2')
return(datos1)
return(datos1)
}
