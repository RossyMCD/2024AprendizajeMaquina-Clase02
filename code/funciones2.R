# RECODIFICACION

## recodificacion
data.train=data_train_2

#### -- OPERACION -- ####
n1=ncol(data.train)
tabla_ks_gini<-data.frame(matrix(0,nrow = (n1-1),ncol =7 )) #El valor de n a tomar en cuenta
names(tabla_ks_gini)<-paste(c("variable","KS","Gini","divergencia_modelo","LogLoss","%default","casos"))

for(i in 1:(ncol(data.train)-1))
  {
  #i=3
  datos = data.frame(data.train[,i],data.train[,n1]) ; colnames(datos)=c(names(data.train[i]),names(data.train[n1]))
  n = nrow(datos)
  cero.na=function(x){ifelse(is.na(x),0,x)}

  pdf(paste0("",i,"_",names(data.train[i]),".pdf","") , width=20, height=10)

  #Arboles ----
  datos.filt = datos[complete.cases(datos[,c(1)]),]
  n = nrow(datos.filt)
  datos.tree<-ctree( datos.filt[,c(2)] ~ datos.filt[,c(1)]
                     ,data=datos.filt,
                     controls=ctree_control(mincriterion=0.95, minbucket = 0.1*n))
  #Estimacion
  yprob<-sapply(predict(datos.tree,type="prob"),'[[',2)
  fit.roc1<-roc(datos.filt[,c(2)], yprob)

  #Gini
  datos.tree.gini = 2*c(auc(fit.roc1))-1
  datos.tree.ks = ks.test(yprob[datos.filt[,c(2)]==1],yprob[datos.filt[,c(2)]==0])

  ##arbol
  plot(datos.tree,     main=paste("(1) ",names(data.train[i])," (y / (T))  (GINI:", round(100*datos.tree.gini,2),"% | KS:", round(100*datos.tree.ks$statistic,2), "%) (", n,"casos)",  sep=" "), cex=0.5,type="simple")
    # Divergencia
  yresp<-predict(datos.tree,type="response")
  divergencia_modelo = cero.na((2*((cero.na(mean(yprob[datos.filt[,c(2)]==1]))-cero.na(mean(yprob[datos.filt[,c(2)]==0])))^2))/ ((cero.na(var(yprob[datos.filt[,c(2)]==1]))+cero.na(var(yprob[datos.filt[,c(2)]==0])))))
  por_default= (cero.na(sum(as.numeric(as.character(datos.filt[,c(2)][yresp==0])))))/length(as.numeric(as.character(datos.filt[,c(2)][yresp==0])))
  LogLoss=LogLoss(y_pred = yprob, y_true = as.numeric(as.character(datos.filt[,c(2)])))
  tabla_ks_gini[i,1] =paste0("",names(data.train[i]),"")
  tabla_ks_gini[i,2] =round(100*datos.tree.ks$statistic,2)
  tabla_ks_gini[i,3] = round(100*datos.tree.gini,2)
  tabla_ks_gini[i,4]= divergencia_modelo
  tabla_ks_gini[i,5] = LogLoss
  tabla_ks_gini[i,6]= por_default
  tabla_ks_gini[i,7] = n
  dev.off()
}

