# Librerías

library("readxl")
library(dplyr)
library(FactoMineR)
library(knitr)
library(summarytools)
library(ROCR)
library(AUC)
library(car)
library(dplyr)
library(factoextra)
library(fpc)


setwd("E:/Universidad/Stats2021/tfg/datos")

dc<- openxlsx::read.xlsx("clientes3.xlsx", detectDates = TRUE, skipEmptyRows=T)

dp<- openxlsx::read.xlsx("polizas2.xlsx", detectDates = TRUE, skipEmptyRows=T)

dr<- openxlsx::read.xlsx("recibos3.xlsx", detectDates = TRUE, skipEmptyRows=T)



dc<-read_xlsx("clientes3.xlsx", sheet="Hoja2")

dp<-read_excel("polizas2.xlsx")

dr<-read_excel("recibos3.xlsx")

# UNIR BASES DE DATOS

names(dc)[1]<- "NºCliente"

names(dp)[1]<- "Cód. Poliza"

db<-inner_join(dc,dr,by="NºCliente") 

db<-inner_join(db,dp,by="Cód. Poliza") 

db<- as.data.frame(db)

# ELIMINAR VARIABLES CON +90% missing

db.na<-(colSums(apply(db,2,is.na))/dim(db)[1])>0.90  ## cutoff 90%
which(db.na==TRUE) 

db.na[178]<-FALSE

db<- db[,-which(db.na==TRUE)]



# DEPURACIÓN VARIABLES NUMÉRICAS

# Resumen de las variables numéricas
dbn<-select_if(db, is.numeric)
summary(dbn)

# Observar % Error de las variables

## Liquid.Cía  

100*sum(db$Liquid.Cía<0,na.rm=T)/dim(db)[1] #1.4% Error

#Pr.Anterior
100*sum(db$Pr.Anterior<0,na.rm=T)/dim(db)[1] #0.2%Error

#Com Anterior

100*sum(db$Com.Anterior<0,na.rm=T)/dim(db)[1]  #0.2% Error

#Pr Tot

100*sum(db$Pr.Tot<0,na.rm=T)/dim(db)[1]  # 0.8% Error

# Comisiones

100*sum(db$Comisiones<0,na.rm=T)/dim(db)[1] #0%

# Comisiones Brutas

100*sum(db$Com.Bruta<0,na.rm=T)/dim(db)[1] # 0.81 son errores

# Comisiones Cedidas

100*sum(db$C.Ced<0,na.rm=T)/dim(db)[1]  #35% son errores

#Comissiones Líquidas

100*sum(db$C.liq<0,na.rm=T)/dim(db)[1] #4.13% errores

#Pr.Cartera  

100*sum(db$`PR. Media`<0,na.rm=T)/dim(db)[1] #0.6%

# Prima Anual

100*sum(db$`Prima anual`<0,na.rm=T)/dim(db)[1] #0.077

#PR.Neta

100*sum(db$Pr.Net<0,na.rm=T)/dim(db)[1] #0.003% Error

#Recargos

100*sum(db$Recargos<0,na.rm=T)/dim(db)[1] # 0.107%

##Desajuste 

100*sum(db$Desajuste!=0,na.rm=T)/dim(db)[1] # solo un 0.3% de las veces no es 0.



# corregir variables

db<-subset(db, `PR. Cartera`>=0 & `PR. Media`>=0,`Prima anual`>=0)

db<-subset(db,C.liq>=0 & Com.Bruta>=0)

db<-db[db$Comisiones>=0,]

db<- db[db$Pr.Tot>=0 & db$Pr.Net>=0,]

db<-subset(db,Recargos>=0)
db<-db[db$Pr.Anterior>=0 & db$Liquid.Cía>=0 & db$Com.Anterior>=0,]

## Condición Correduría: Cliq>20%comBruta

db<- db[db$C.liq>db$Com.Bruta*0.2,]



# Eliminar Variables no útiles

db$NºCía.x<-db$NºCía.y<-NULL 
db$Impuestos  <- NULL
db$C.Tal <- db$C.Adicional<-NULL
db$Var.Abs.<-db$`Var.Abs.(%Com)`<-db$`Var.Abs.(Com)`<-NULL

db$Var.Por.<-db$`Var.Por.(%Com)`<-db$`Var.Por.(Com)`<-NULL

db$`%Com.Actual`<- db$`%Com.Anterior`<-NULL

db[,"Uso Auto"]<-NULL

db$P.Cartera<-NULL

db$Código<-NULL

db[,"Cod. Tomador"]<-NULL 

db$Cod.Pro<-NULL

db[,which(names(db)=="Núm.Siniestros")]<-NULL

db$Pr.Tot<-NULL

db$Matrícula<-NULL
db[,"Primas Totales"]<- db[,"Primas Netas"]<-NULL

db$HSP<-db$`B.I. HSP`<-db$H.S.P.<-db$`HSP cedido`<-NULL

db$`Cuota IVA HSP`<-NULL

db[,which(names(db)=="Plazas auto")]<-NULL

db[,"Importe total"]<-NULL

db[,"C.Ced"]<-NULL

db[,"Desajuste"]<-NULL



# DEPURACIÓN VARIABLES CATEGÓRICAS

#Depuración Variables Categóricas

# niveles de las categóricas
dbc<- select_if(db, is.character)
unc<- apply(dbc,2,function(x){length(unique(x))})
sort(unc, decreasing = T)

# Condición datos sean correctos

db<-db[which(db$Of.Cobradora==db$Of.Cobradora.x & db$Of.Cobradora==db$Of.Cobradora.y),]

#Recategorización de las variables

## Tipo Cliente

###{AGROSEGURO,SEGUROS GENERALES Y AGROSEGURO y SEGUROS GENERALES Y ASESORIA} -> OTROS

dbTipo<-db[,which(names(db)=="Tipo Cliente")]

bol<-  dbTipo=="AGROSEGURO" |dbTipo=="SEGUROS GENERALES Y AGROSEGURO" | dbTipo=="SEGUROS GENERALES Y ASESORIA"

db[,which(names(db)=="Tipo Cliente")][bol]<-"OTROS"


## Motivo Baja

### {COMPETENCIA,DECISION TECNICA,DEFUNCION}->"OTROS"

MOTBAJA<-db[,which(names(db)=="Motivo Baja")]
bol<-  MOTBAJA=="COMPETENCIA" |MOTBAJA=="DECISION TECNICA" | MOTBAJA=="DEFUNCION"

db[,which(names(db)=="Motivo Baja")][bol]<-"OTROS"


## Cobro

### {COMPAñIA EFECTIVO,OFICINA EFECTIVO} -> EFECTIVO

### {BANCO,BCO.TRANSFERENCIA,BCO.VENTANILLA,COMPAñIA BANCO,TARJETA BANCARIA} -> NO EFECTIVO

bol<- db$Cobro=="COMPAÑIA EFECTIVO"|db$Cobro=="OFICINA EFECTIVO"

bol2<- db$Cobro=="BANCO"|db$Cobro=="BCO.TRANSFERENCIA"|db$Cobro=="BCO.VENTANILLA"|db$Cobro=="COMPAÑIA BANCO"|db$Cobro=="TARJETA BANCARIA"


db$Cobro[bol]<- "EFECTIVO"
db$Cobro[bol2]<- "NO EFECTIVO"


## Reg Laboral

### {Todos los niveles diferentes a REG.General} -> "OTROS"

db$`Reg. Laboral`[db$`Reg. Laboral`!="REG.GENERAL"]<-"OTROS"

## Forma Pago

### {Niveles diferente a ANUAL}-> NO ANUAL

db$`Forma Pago`[db$`Forma Pago`!="ANUAL"]<-"NO ANUAL"

## Estado.x

### {Todas las categorias diferentes a anulados} ->"VIGOR"


db$Estado.x[db$Estado.x!="ANULADO"]<- "VIGOR"


## Provincia

### {GERONA,BARCELONA,LERIDA Y TARRAGONA}
### {CASTELLON, VALENCIA Y ALICANTE}
### {HUESCA, ZARAGOZA Y TERUEL}
### {Resto de niveles} -> OTROS

bol3<-db$Provincia=="GERONA"| db$Provincia=="BARCELONA"  | db$Provincia=="LERIDA"| db$Provincia=="TARRAGONA" 

db$Provincia[bol3]<-"Cataluña"

bol4<- db$Provincia=="CASTELLON"| db$Provincia=="VALENCIA"| db$Provincia=="ALICANTE" 

db$Provincia[bol4]<-"Valencia"

bol5<- db$Provincia=="HUESCA"| db$Provincia=="ZARAGOZA"| db$Provincia=="TERUEL" 

db$Provincia[bol5]<-"Aragon"


bol6<-bol3+bol4+bol5
db$Provincia[bol6==0]<-"Otros"


## Abrev.Cía
### {Niveles diferentes a ZURICH,MAPFRE,ALLIANZ,HELVETIA,AXA,GENERALI Y PLUS ULTRA}-> OTRAS


bol7<- db$Abrev.Cía.x=="ZURICH" | db$Abrev.Cía.x=="MAPFRE" | db$Abrev.Cía.x=="ALLIANZ" | db$Abrev.Cía.x=="HELVETIA" | db$Abrev.Cía.x=="AXA" | db$Abrev.Cía.x=="GENERALI" |db$Abrev.Cía.x=="PLUS ULTRA"  # 87.57% 

db$Abrev.Cía.x[ bol7==0]<-"Otras"


##Segmento
### {ORO,VIP,PLATA}-> ORO-PLATA-VIP
db$Segmento[db$Segmento=="ORO" |db$Segmento=="VIP" | db$Segmento=="PLATA" ]<-"ORO-PLATA-VIP"


# Eliminación de algunos niveles

##Est.Civ

### Eliminar todo los niveles diferentes a C,S y O


db<-db[db$Est.Civ=="C"|db$Est.Civ=="O"| db$Est.Civ=="S",]

## Grupo cliente

### {Solo tener en cuenta a los niveles SIN GRUPO y SEGUROS}

db<-db[db$`Grupo Cliente.x`=="SEGUROS"|db$`Grupo Cliente.x`=="SIN GRUPO",]


# Elimiar Variable no útiles


db$Irpf <- NULL
db$Tipo.x<-NULL
db$Segmento.y<-NULL
db$Of.Cobradora.x<-db$Of.Cobradora.y<-db$Of.Gestora<-db$Of.Productora.y<-NULL
db$Of.Gestora.x<-db$Of.Gestora.y<-db$Of.Productora<-db$Of.Productora.x<-NULL
db$Pago<-NULL
db$Tipo.y<-NULL
db$Pago<-NULL
db[,which(names(db)=="Medio Cobro")]<-NULL
db[,which(names(db)=="Motivo Anulación")]<-NULL
db[,which(names(db)=="Nombre Correduría.x")]<-NULL
db[,which(names(db)=="Nombre Correduría.y")]<-NULL
db$Comentarios.x<- NULL
db$Tipo.x<-NULL
db$Segmento.y<-NULL
db$Pago<-NULL
db$Estado.x<- NULL
db[,which(names(db)=="Plan Com..y")]<-NULL
db$Duración.y<-NULL
db$Estado.y[db$Estado.y=="SUSPENSION DE GARANTIAS"]<- "ANULADA"
db[,which(names(db)=="Tipo Cliente")]<-NULL
db[,which(names(db)=="Riesgo Poliza")]<-NULL
db[,which(names(db)=="Situacion Riesgo")]<-NULL
db[,which(names(db)=="Ramo Niv.2.y")]<-NULL
db[,which(names(db)=="Ramo Niv.1.y")]<-NULL
db$Riesgo.y<-NULL 
db[,which(names(db)=="Grupo Cliente.y")]<-NULL
db$Compañía<-db$Nombre.Cía<-db$Abrev.Cía.y <-NULL
db[,which(names(db)=="Prov Postal")]<-NULL
db$Segmento.x<-NULL
db[,which(names(db)=="Nº Orden")]<-NULL
db$Producto.y<-NULL
db[,which(names(db)=="Municipio Principal")]<-NULL
db[,which(names(db)=="Situacion Riesgo")]<-NULL
db[,which(names(db)=="Población Principal")]<-NULL
db[,which(names(db)=="Pob Postal")]<-NULL
db[,which(names(db)=="Marca/Modelo")]<-NULL
db$Comentarios.y<-NULL
db$Nat.Riesgo<-NULL
db$Modelo<-NULL
db$Riesgo.x<-db$Riesgo.y<-NULL
db[,which(names(db)=="Riesgo Poliza")]<-NULL
db$`Sect. Laboral`<-NULL
db$CP<-NULL
db[,which(names(db)=="CP Principal")]<-NULL
db[,which(names(db)=="Cod. Tomador")]<-NULL
db[,which(names(db)=="Prima anual")]<-NULL

################################################################# k<-db

# CONSTRUCCIÓN DE VARIABLES RESPUESTA Y variables activas clustering

#yc - 1:Ha cancelado alguna poliza, 0 no ha cancelado ninguna.

#Yimp C.tal medio del asegurado

m<-aggregate(db$C.liq, list(db$NºCliente), mean)

db$Estado.y<- as.numeric(db[, "Estado.y"]=="ANULADA")

my<-aggregate(db$Estado.y, list(db$NºCliente), mean) 

my2<-aggregate(db$Pr.Actual, list(db$NºCliente), sum) 
my3<-aggregate(db$Pr.Net, list(db$NºCliente), sum) 
my4<-aggregate(db$Com.Bruta, list(db$NºCliente), sum) 

db.d<-distinct(db, NºCliente,.keep_all = TRUE)

db.d<- arrange(db.d,NºCliente)

db.d<-db.d[-nrow(db.d),]

db.d$C.liq<-m[,2]

db.d$Estado.y[my$x!=0]<-1

db.d$Tot.Pr.Actual<-my2[,2]

db.d$Tot.Pr.Net<-my3[,2]

db.d$Tot.Com.Bruta<-my4[,2]

db.d<- db.d[db.d$C.liq>0 , ]
db.d<- db.d[db.d$`PR. Media`>0, ]

db.d$yc<- factor(db.d$Estado.y) 

db.d$yimp<- db.d$C.liq


row.names(db.d)<- db.d$NºCliente

# CONSTRUCCIÓN VARIABLE ANTIGÜEDAD

db.d$F.Baja[is.na(db.d$F.Baja)]<-"2021-02-20 UTC"

db.d$F.Baja <- as.Date(db.d$F.Baja, format='%d/%m/%y')

db.d$anti<-as.numeric(difftime(db.d$F.Baja,db.d$F.Alta, units = "weeks")/52.25) # Fbaja-Falta

db.d<-db.d[db.d$anti>0, ] 
db.d<-db.d[db.d$anti<100, ] # solo un punto es 180 y es claramente un error 

# ANÁLISIS DESCRIPTIVO UNIVARIANTE PARA LOS MODELOS LINEALES

# NUMÉRICAS

##yimp

summary(db.d$yimp)

sqrt(var(db.d$yimp))

hist(db.d$yimp[db.d$yimp<200],col="royalblue",main="Histograma Yimp", xlab="Yimp")

##Pr.media

summary(db.d$`PR. Media`)

sqrt(var(db.d$`PR. Media`))

hist(db.d$`PR. Media`[db.d$`PR. Media`<1800],col="royalblue",main="Histograma Pr.Media", xlab="Pr.Media")

##Num Pólizas

plot(db.d$`Num Pólizas`)
summary(db.d$`Num Pólizas`)

# Antigüedad



summary(db.d$anti)
boxplot(db.d$anti, col="royalblue",main="Antigüedad")


# CUALITATIVAS
#yc
table(db.d$yc)

##Fis/Jur

table(db.d$`Fis/Jur`)/sum(table(db.d$`Fis/Jur`))

table(db.d$`Fis/Jur`)

# Est.Civ

table(db.d$Est.Civ)

kable(table(db.d$Est.Civ,db.d$yc)/rowSums(table(db.d$Est.Civ,db.d$yc))*100, align="c")

#Sexo

table(db.d$Sexo)

#Provincia

table(db.d$Provincia)

#GRUPO CLIENTE

table(db.d$`Grupo Cliente.x`)

#REG LABORAL
table(db.d$`Reg. Laboral`)


#PLAN COM
table(db.d$`Plan Com..x`)

#ABREV CIA
table(db.d$Abrev.Cía.x)

#COBRO

table(db.d$Cobro)

#FORMA PAGO

table(db.d$`Forma Pago`)

#OF COBRADORA

table(db.d$Of.Cobradora)

#CLASE

table(db.d$Clase)



# ANÁLISIS DESCRIPTIVO BIVARIANTE PARA LOS MODELOS LINEALES

# variables explicativas en función de Yc


names(db.d)[1]<-"ind"
dx1<- na.omit(db.d[, c("Est.Civ","Sexo","Grupo Cliente.x","Reg. Laboral","Segmento","Cobro","Provincia", "Plan Com..x", "Abrev.Cía.x","Of.Cobradora","Num Pólizas","PR. Media","yc","Fis/Jur","Forma Pago","Clase","ind","anti")])
avg<-function(x){
  
  barplot(table(dx1$yc,dx1[,x]),col=c("royalblue","darkblue"), xlab="",ylab="Frecuencia", main=paste("Yc vs",x))
  legend("topright", legend=c("Yc=0", "Yc=1"),
         col=c("royalblue","darkblue"), pch=16, cex=1)  
  
  
}

#Est.Civ
avg(x = "Est.Civ")
table(dx1$Est.Civ, dx1$yc)/rowSums(table(dx1$Est.Civ, dx1$yc))

#Sexo
avg(x = "Sexo")

table(dx1$Sexo, dx1$yc)/rowSums(table(dx1$Sexo, dx1$yc)) 

#GrupoCliente

avg(x = "Grupo Cliente.x")
table(dx1$`Grupo Cliente.x`, dx1$yc)/rowSums(table(dx1$`Grupo Cliente.x`, dx1$yc))  

#Reg Laboral

avg(x = "Reg. Laboral") 
table(dx1$`Reg. Laboral`, dx1$yc)/rowSums(table(dx1$`Reg. Laboral`, dx1$yc))

#Segmento
avg(x = "Segmento") 
table(dx1$Segmento, dx1$yc)/rowSums(table(dx1$Segmento, dx1$yc)) 

#Cobro
avg(x = "Cobro")
table(dx1$Cobro, dx1$yc)/rowSums(table(dx1$Cobro, dx1$yc))  

#Pr.MEDIA

boxplot(dx1$`PR. Media`~factor(dx1$yc), ylim=c(0,1800), col="royalblue",ylab="Pr Media",xlab="Yc",main="Yc vs Pr.Media")

#Plan Com

avg(x = "Plan Com..x")

#Abrev cia

avg(x = "Abrev.Cía.x")

table(dx1$Abrev.Cía.x, dx1$yc)/rowSums(table(dx1$Abrev.Cía.x, dx1$yc))
#Of.Cobradora

avg(x = "Of.Cobradora")

#Num Pólizas

boxplot(dx1$`Num Pólizas`~factor(dx1$yc), ylim=c(0,20) ,col="royalblue",ylab="Núm Polizas",xlab="Yc")

avg(x = "Num Pólizas")

#Provincia

avg(x = "Provincia")
table(dx1$Provincia, dx1$yc)/rowSums(table(dx1$Provincia, dx1$yc))  

#Forma Pago
avg(x = "Forma Pago")
table(dx1$`Forma Pago`, dx1$yc)/rowSums(table(dx1$`Forma Pago`, dx1$yc)) 

#Clase
avg("Clase")
table(dx1$Clase, dx1$yc)/rowSums(table(dx1$Clase, dx1$yc)) 

# Antigüedad

boxplot(dx1$anti~factor(dx1$yc), col="royalblue",ylab="Antigüedad",xlab="Yc",main="Yc vs Antigüedad")

# Racategorización de variables

## Variable Antigüedad por cuartiles

dx1$fanti<-cut(dx1$anti, c(0,2,5,10,32), include.lowest = F)

## Variable Abrev Cía

### {PLUSULTRA, GENERALI y ALLIANZ}->+ULTRA o GENERALI o ALLIANZ

###{AXA Y MAPFRE} -> AXA o MAPFRE

### {OTRAS Y ZURICH} -> OTRAS

dx1$Abrev.Cía.x[dx1$Abrev.Cía.x=="PLUS ULTRA"|dx1$Abrev.Cía.x=="GENERALI"|dx1$Abrev.Cía.x=="ALLIANZ"]<- "+ULTRA o GENERALI o ALLIANZ"

dx1$Abrev.Cía.x[dx1$Abrev.Cía.x=="AXA"|dx1$Abrev.Cía.x=="MAPFRE" ]<- "AXA o MAPFRE"

dx1$Abrev.Cía.x[dx1$Abrev.Cía.x=="Otras"|dx1$Abrev.Cía.x=="ZURICH" ]<- "OTRAS"

## Variable Número de Pólizas

### {2 y 3} -< "2-3"
### {4,5,.....70} -> "+4"

dx1$f.numpol<-dx1$`Num Pólizas`

dx1$f.numpol[dx1$f.numpol>=4]<-"+4"

dx1$f.numpol[dx1$f.numpol=="3" |dx1$f.numpol=="2"]<-"2-3"


## Variable Provincia

### {Aragon y cataluña} -> Cataluña o Aragon

dx1$Provincia[dx1$Provincia=="Aragon" | dx1$Provincia=="Cataluña"]<-"Cataluña o Aragon"



# Distribución con variables recategorizadas

# Antigüedad

avg(x = "fanti")

table(dx1$fanti, dx1$yc)/rowSums(table(dx1$fanti, dx1$yc)) 


#Abrev cia

avg(x = "Abrev.Cía.x")

table(dx1$Abrev.Cía.x, dx1$yc)/rowSums(table(dx1$Abrev.Cía.x, dx1$yc))


# Número de Pólizas como factor

avg(x = "f.numpol")

table(dx1$f.numpol, dx1$yc)/rowSums(table(dx1$f.numpol, dx1$yc))

#Provincia

avg(x = "Provincia")
table(dx1$Provincia, dx1$yc)/rowSums(table(dx1$Provincia, dx1$yc)) 




# variables explicativas en función de Yimp

dx2<- na.omit(db.d[, c("Fis/Jur","Provincia", "Grupo Cliente.x","Plan Com..x", "Abrev.Cía.x","Of.Cobradora","Num Pólizas","yimp","Segmento","Forma Pago","Clase","Est.Civ","Cobro","ind","anti")])



avg2<-function(x){
  
  boxplot(dx2$yimp~dx2[,x],ylim=c(0,200),col="royalblue",ylab="Yimp",xlab=x,main=paste("Yimp vs ",x))
  
}


#Fis/Jur

avg2(x = "Fis/Jur")  

#Provincia

avg2(x = "Provincia") 

#Plan Com

avg2(x = "Plan Com..x")

#Abrev Cia

avg2(x = "Abrev.Cía.x")

#Of.Cobradora

avg2(x = "Of.Cobradora")

#Num Pólizas

plot(dx2$yimp~dx2$`Num Pólizas`,ylim=c(0,200),col="royalblue") 

table(dx2$`Num Pólizas`)  

#Clase
avg2(x = "Clase")
#Forma de Pago
avg2(x = "Forma Pago")

#Est.Civ

boxplot(db.d$yimp~db.d$Est.Civ,ylim=c(0,200),col="royalblue", main="Yimp vs Est.Civil", ylab="Yimp",xlab = "Est.Civ")

# Antigüedad
plot(dx2$yimp, dx2$anti)

# Recategorización de las variables 

# Antigüedad por cuartiles
dx2$fanti<-cut(dx2$anti, c(0,2,5,10,32), include.lowest = F)

##Variable Abrev.Cía:

### {Otras y PLUS ULTRA} -> +ULTRA o OTRAS

###{AXA Y HELVETIA} ->AXA o HELVETIA

### {Mapfre y Zurich} -> MAPFRE o ZURICH

dx2$Abrev.Cía.x[dx2$Abrev.Cía.x=="PLUS ULTRA"|dx2$Abrev.Cía.x=="Otras" ]<- "+ULTRA o OTRAS"

dx2$Abrev.Cía.x[dx2$Abrev.Cía.x=="AXA"|dx2$Abrev.Cía.x=="HELVETIA"]<- "AXA o HELVETIA"

dx2$Abrev.Cía.x[dx2$Abrev.Cía.x=="MAPFRE"|dx2$Abrev.Cía.x=="ZURICH"]<-"MAPFRE o ZURICH"

## Variable Num Pólizas

dx2$f.numpol<-dx2$`Num Pólizas`

### {4,5,.....70} -> "+4"
###{2,3} -> "2-3" 

dx2$f.numpol[dx2$f.numpol>3]<-"+4"

dx2$f.numpol[dx2$f.numpol=="2" | dx2$f.numpol=="3"]<-"2-3"

# Distribución de las variables recategorizadas

## Abrev Cía

avg2(x = "Abrev.Cía.x")

# Núm de Pólizas como factor

avg2(x = "f.numpol")

boxplot(dx2$yimp~dx2[,"f.numpol"],ylim=c(0,200),col="royalblue",ylab="Yimp",xlab="Núm.Pólizas",main="Yimp vs Número de Pólizas")

# Antigüedad

avg2(x = "fanti")



# REGRESÓN LOGÍSTICA

# modelo con todas la variables 

lmodcT<-glm(yc~Est.Civ+Sexo+Provincia+`Grupo Cliente.x`+`Reg. Laboral`+`Num Pólizas`+`Abrev.Cía.x`+Cobro + 
              Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+`PR. Media`+anti, data=dx1, family=binomial) 

residualPlot(lmodcT,ylim=c(-20,20))
Anova(lmodcT,test.statistic="LR") 

# probar con  antigüedad factorizada

lmodc<-glm(yc~Est.Civ+Sexo+Provincia+`Reg. Laboral`+`Num Pólizas`+`Abrev.Cía.x`+Cobro + Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+ fanti, data=dx1, family=binomial) 

Anova(lmodc,test.statistic="LR") # significativa


# Cambios en los niveles de las variables:

##sexo femenino y masculino no se diferencian

dx1$Sexo[dx1$Sexo!="S"]<-"M o F"

##Est.civ S no se distingue de Casado

dx1$Est.Civ[dx1$Est.Civ!="O"]<-"C o S"



# Test anova entre modelo completo y simplificado

anova(lmodcT,lmodc,test="Chisq")

# Percentil 95 de la chi cuadrado para el modelo simplificado

qchisq(0.95,lmodc$df.residual)

# AIC Y BIC

AIC(lmodcT, lmodc)

BIC(lmodcT, lmodc)


# Modelo con la variable Núm de Pólizas como factor

lmodcf2<-glm(yc~Est.Civ+Sexo+Provincia+`Reg. Laboral`+f.numpol+`Abrev.Cía.x`+Cobro + Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+ fanti, data=dx1, family=binomial)


# Devianzas del los con Núm de Pólizas como numérica y factor - y valor límite de la diferencia

cbind(Devianza1= deviance(lmodc), Devianza2= deviance(lmodcf2), Limite=qchisq(0.95,lmodc$df.residual-lmodcf2$df.residual) )

# AIC Y BIC

AIC(lmodcf2, lmodc)

BIC(lmodcf2, lmodc)

# Se elige el modelo con Núm de Pólizas como factor

lmodc<-lmodcf2


### Más cambios en los niveles de las variables

## Abrev.Cía HELVETIA y OTRAS tienen coeficientes muy similares

dx1$Abrev.Cía.x[dx1$Abrev.Cía.x=="HELVETIA"]<- "OTRAS"

## Antigüedad factorizada (2,5] y (5,10] tienen coeficientes muy similares
dx1$fanti<-as.character(dx1$fanti)
dx1$fanti[ dx1$fanti=="(2,5]" | dx1$fanti=="(5,10]"] <- "(2,10]"


# modelo con los nuevos cambios
lmodct2<-glm(yc~Est.Civ+Sexo+Provincia+`Reg. Laboral`+f.numpol+`Abrev.Cía.x`+Cobro + Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+ fanti, data=dx1, family=binomial)

# Test de Devianza entre el modelo anterior y el nuevo

anova(lmodct2, lmodc, test="Chisq")

# Se elige el nuevo modelo

lmodc<-lmodct2


# Comprobación puntos influyentes y mulicolinealidad

plot(lmodc)  # Sin puntos influyente 

vif(lmodc)  # no multicolinealidad



## Evaluación capacidad predictiva del modelo (todos los datos)

db.d2<- na.omit(dx1)
dadesroc <- prediction(predict(lmodc,type="response"),db.d2$yc)
par(mfrow=c(1,2))
roc.perf <- performance(dadesroc,"auc",fpr.stop=0.05)
plot(performance(dadesroc,"err"),main="Tasa de error", col="royalblue", ylab="Tasa de error", xlab="Punto de corte")
plot(performance(dadesroc,"tpr","fpr"),main="Curva ROC",col="royalblue", ylab="Tasa de positivos reales",xlab="Tasa de falsos positivos")

pres.est <- ifelse(lmodc$fit<0.55,0,1)

# Matriz Confusión
(t <- table(pres.est,db.d2$yc))

#Área bajo la curva ROC

auc(roc(predict(lmodc,type="response"),factor(db.d2$yc)))  

# Cálculo de estadísticos interensantes

x<-c(Sensibilidad=t[2,2]/sum(t[,2]), Especificidad=t[1,1]/sum(t[,1]), ValorPredPositiu=t[2,2]/sum(t[2,])  , ValorPredNegatiu=t[1,1]/sum(t[1,]))

x

# Exponencial de los coeficientes

round(exp(lmodc$coefficients),4)




# REGRESIÓN GAMMA

#Modelo con todas las variables


lmodT<-glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Grupo Cliente.x`+`Abrev.Cía.x`+Of.Cobradora+`Num Pólizas`+`Forma Pago`+Clase+Cobro+anti,data=dx2, family=Gamma("log"), maxit=150)

plot(lmodT,ask=F)

Anova(lmodT,test="F") 

#Modelo sin variables no significativas (Grupo Cliente, Núm de Pólizas y Clase)

lmod<-glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cía.x`+Of.Cobradora+`Forma Pago`+Cobro+anti,data=dx2, family=Gamma("log"), maxit=150)

Anova(lmod,test="F")

# Modelo con Núm de Pólizas como factor

lmod<-glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cía.x`+Of.Cobradora+f.numpol+`Forma Pago`+Cobro+anti,data=dx2, family=Gamma("log"), maxit=150)
Anova(lmod,test="F") 

# Probar variable antigüedad categorizada

lmodf<-glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cía.x`+Of.Cobradora+f.numpol+`Forma Pago`+Cobro+fanti,data=dx2, family=Gamma("log"), maxit=150)

# Test de devianza

anova(lmod,lmodf, test="F") #mejor como numérica

# Observar p-valores de los coeficientes

summary(lmod)

# Cambios en los niveles de las variables:

##Axa y Helvetia y generali no se diferencia de +ULTRA o OTRAS 

dx2$Abrev.Cía.x[dx2$Abrev.Cía.x=="AXA o HELVETIA" | dx2$Abrev.Cía.x=="+ULTRA o OTRAS"]<-"OTRAS"


#modelo con la recategorización hecha
lmod<-glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cía.x`+Of.Cobradora+f.numpol+`Forma Pago`+anti,data=dx2, family=Gamma("log"), maxit=150)

summary(lmod)

# Test de devianza modelo nuevo y el aditivo completo

anova(lmodT,lmod,"F")

# AIC Y BIC

AIC(lmod,lmodT)
BIC(lmod,lmodT)

# Ajuste?

cbind(Devianza=deviance(lmod),Chi2=qchisq(0.95, df.residual(lmod)))

# Pseudo-R^2

1-deviance(lmod)/lmod$null.deviance 

# Exponencial coeficientes

round(exp(lmod$coefficients),4)


# COMPROBACIÓN CAPACIDAD PREDICTIVA DE LOS 2 MODELOS (Datos entrenamiento y test)


## Logit

# Modelización con los datos de entrenamiento

set.seed(1)
sel<- sample(1:nrow(dx1),size=round(0.8*nrow(dx1)))

lmodctest<-glm(yc~Est.Civ+Sexo+Provincia+`Reg. Laboral`+f.numpol+`Abrev.Cía.x`+Cobro + Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+ fanti, data=dx1[sel,], family=binomial)

# Predicciones

p.test<-predict(lmodctest,type="response",dx1[-sel,])


## Capacidad predictiva

dadesroc <- prediction(p.test,dx1[-sel,"yc"])
par(mfrow=c(1,2))
roc.perf <- performance(dadesroc,"auc",fpr.stop=0.05)
plot(performance(dadesroc,"err"),main="Tasa de error", col="royalblue", ylab="Tasa de error", xlab="Punto de corte")
plot(performance(dadesroc,"tpr","fpr"),main="Curva ROC",col="royalblue", ylab="Tasa de positivos reales",xlab="Tasa de falsos positivos")

pres.est <- ifelse(p.test<0.55,0,1)

# Matriz de confusión

(t <- table(pres.est,dx1[-sel,"yc"]))

#Área bajo la curva

auc(roc(p.test,factor(dx1[-sel,"yc"])))  


## Gamma

### 10-fold Cross validation

(EQM<-boot::cv.glm(dx2, lmod, K=10)$delta[1])
sqrt(EQM) 



# Modelización con los datos de entrenamiento

set.seed(1)
sel<- sample(1:nrow(dx2),size=round(0.8*nrow(dx2)))

lmodtest<-glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cía.x`+Of.Cobradora+f.numpol+`Forma Pago`+anti, family=Gamma("log"),data=dx2[sel,], maxit=150)

# Predicciones

p.test2<-predict(lmodtest,type="response",dx2[-sel,c("Fis/Jur","Provincia","Plan Com..x","Abrev.Cía.x","Of.Cobradora","f.numpol","Forma Pago","anti")])

# Comparación distribuciones empíricas

plot(ecdf(dx2[-sel,"yimp"]), col="blue",pch=16, main="Distribuciones de probabilidad acumulada")
lines(ecdf(p.test2), col="darkblue")

legend("topright", legend=c("Mostral", "Predicciones"),
       col=c("royalblue","darkblue"),pch=15)




# CONTRUCCIÓN MAPAS DE RIESGO 

# Predicciones a nivel respuesta

dx1$pc<-100*predict(lmodc, type="response")

dx2$pimp<- predict(lmod, type="response")

# Contrucción mapas de riesgo en función de los niveles de los factores

x<-inner_join(dx1,dx2,by="ind") 

sel_noB<-data.frame(ind=db.d[which(db.d$F.Baja=="2021-02-19"),1])

x<- inner_join(x,sel_noB,by="ind") 

xr<-x[, c("yimp","pimp","yc","pc","ind")]

xe<- x[,c(1:4,6,8,10,14,15,16,37,19,36,7,23,9,26,5,17)]

xe$fanti.y<-as.character(xe$fanti.y)


# Por variables

palet<-c("royalblue","darkblue","turquoise4","skyblue")

for (i in 1:(ncol(xe)-1)){ 
  
  v<- unique(xe[,i])
  col<- xe[,i]
  for(j in 1:length(v)){
    
    col[xe[,i]==v[j]]<- palet[j]
  }
  
  plot(xr$pc,xr$pimp, ylab="Impacto",xlab="Probabilidad de Cancelación ", col=col,pch=16,main=names(xe)[i])
  legend("topleft", legend=v,
         col=unique(col), pch=16, cex=0.8) 
}

# Por Cuadrantes

myimp<- 0.5*(max(x$pimp)+min(x$pimp))

xe$cuadrantes<- rep("C1", nrow(xe))

xe$cuadrantes[xr$pc>50 & xr$pimp>myimp]<- "C2"

xe$cuadrantes[xr$pc<50 & xr$pimp<myimp]<- "C3"

xe$cuadrantes[xr$pc>50 & xr$pimp<myimp]<- "C4"

v<- unique(xe$cuadrantes)

col<- xe$cuadrantes

for(j in 1:4){
  
  col[xe$cuadrantes==v[j]]<- palet[j]
}



plot(xr$pc,xr$pimp, ylab="Impacto",xlab="Probabilidad de Cancelación ", col=col,pch=16,main="Mapa de riesgo por cuadrantes")
legend("topleft", legend=v,col=unique(col), pch=16, cex=0.8) 


# CLUSTERING

# Datos Para Relizar el análisis

ve<-c("Tot.Pr.Actual","Tot.Com.Bruta","Tot.Pr.Net", "yimp","PR. Media")


dx3<- na.omit(db.d[,c("Est.Civ","Sexo","Grupo Cliente.x","Reg. Laboral","Segmento","Cobro",
                      "Provincia", "Plan Com..x", "Abrev.Cía.x","Of.Cobradora","yc","Fis/Jur",
                      "Forma Pago","Clase","anti","Num Pólizas",ve )])

## Análsis descriptivo variables activas

apply(dx3[,ve],2,sd)

apply(dx3[,ve],2,summary)

boxplot(dx3[,"Tot.Pr.Actual"], main="Tot.Pr.Actual", col="royalblue")

boxplot(dx3[,"Tot.Com.Bruta"], main="Tot.Com.Bruta", col="royalblue")

boxplot(dx3[,"Tot.Pr.Net"], main="Tot.Pr.Net", col="royalblue")

boxplot(dx3[,"yimp"], main="Yimp", col="royalblue")

boxplot(dx3[,"PR. Media"], main="PR. Media", col="royalblue")


# Calcular el  límite superior de las variables activas


l1<-quantile(dx3[,"Tot.Pr.Actual"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Actual"])


l2<-quantile(dx3[,"Tot.Com.Bruta"],probs=0.75)+3*IQR(dx3[,"Tot.Com.Bruta"])


l3<-quantile(dx3[,"Tot.Pr.Net"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Net"])


l4<-quantile(dx3[,"yimp"],probs=0.75)+3*IQR(dx3[,"yimp"])


l5<-quantile(dx3[,"PR. Media"],probs=0.75)+3*IQR(dx3[,"PR. Media"])


# Eliminar outliers


dx3<- dx3[dx3[,"Tot.Pr.Actual"]<round(l1), ]  ## 432 outliers

dx3<- dx3[dx3[,"Tot.Com.Bruta"]<round(l2), ]  #78 outliers

dx3<- dx3[dx3[,"Tot.Pr.Net"]<round(l3), ] #35 outliers

dx3<- dx3[dx3[,"yimp"]<round(l4), ]  ## #68 outliers

dx3<- dx3[dx3[,"PR. Media"]<round(l5), ]  ## 204 outliers

# Análsis descriptivo variables activas (después de eliminar outliers)

apply(dx3[,ve],2,sd)

apply(dx3[,ve],2,summary)

boxplot(dx3[,"Tot.Pr.Actual"], main="Tot.Pr.Actual", col="royalblue")

boxplot(dx3[,"Tot.Com.Bruta"], main="Tot.Com.Bruta", col="royalblue")

boxplot(dx3[,"Tot.Pr.Net"], main="Tot.Pr.Net", col="royalblue")

boxplot(dx3[,"yimp"], main="Yimp", col="royalblue")

boxplot(dx3[,"PR. Media"], main="PR. Media", col="royalblue")


# Yc numércia y cálculo de correlaciones de las variables activas

dx3[,"yc"]<- as.numeric(dx3[,"yc"])

cor(dx3[,ve])


# Seleccionar numéro de componentes ACP

dx3.2<-dx3

pc1 <- prcomp(dx3.2[,ve], scale=TRUE)

eigcr_val<-pc1$sdev^2

par(mfrow=c(1,1))

plot(cumsum(eigcr_val/sum(eigcr_val)),type="b",xlab="Nºcomponent",ylab="Varianza explicada acumulada",main="Criterio PCA",col="royalblue")
abline(h=0.80)

# Escalar variables activas

dx3.2[,ve]<-scale(dx3.2[,ve]) 


# PCA

res.pca <- PCA(dx3.2,quali.sup=c(1:10,12:16), quanti.sup = c(11,15,16), ncp = 2) 

Psi = res.pca$ind$coord

# K means con k=3
set.seed(123)
k3<-kmeans(Psi,3,nstart = 25,iter.max=150)


# Número de observaciones por clúster y gráfico

table(k3$cluster) 

fviz_cluster(k3, geom = c("point") , data = dx3.2[,ve])+ ggtitle("k = 3")


# Cálculo estadísticos 

dd <- dist(dx3.2[,ve], method ="euclidean")

km_stats <- cluster.stats(dd,  k3$cluster)

km_stats$within.cluster.ss 

km_stats$clus.avg.silwidths

km_stats$dunn


# comparación con variable segmento

cseg<-dx3$Segmento
cseg[cseg=="ANULADO"]<-"red"
cseg[cseg=="BRONCE"]<-"black"
cseg[cseg=="ORO-PLATA-VIP"]<-"green"

plot(Psi,type="n",main="Variable Segmento", xlim=c(-2.5,10))
text(Psi, col=cseg,labels=names((k3$cluster)))
abline(h=0,v=0,col="gray")
legend("bottomright",unique(dx3$Segmento),pch=20,col=unique(cseg))


# Descriptiva de los cluesters por numéricas y categóricas

des<-catdes(cbind(as.factor(k3$cluster),dx3.2),1)

des

# Seleccionar una mejor K.

# scree Plot

ratio_ss <- rep(0, 10)

for (k in 1:10) {
  set.seed(123)
  school_km <- kmeans(Psi, k, nstart = 20)
  
  ratio_ss[k] <- school_km$tot.withinss / school_km$totss
  
}


plot(ratio_ss, type = "b", xlab = "k", col="royalblue", main="Scree Plot")


# Plot de los diferentes K means
par(mfrow=c(2,2))

fviz_cluster(kmeans(Psi,4,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",4))  

fviz_cluster(kmeans(Psi,5,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",5))

fviz_cluster(kmeans(Psi,6,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",6))

fviz_cluster(kmeans(Psi,7,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",7))


