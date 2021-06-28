
library(shiny)
library(shinythemes)
library(knitr)
library(dplyr)
library(FactoMineR)
library(base)
library(factoextra)
library(DT)


ui <- fluidPage(
    theme=shinytheme("cerulean"),
    
    navbarPage(
        "TFG",
        tabPanel("Mapas de Riesgo",
                 
                 sidebarPanel(
                     
                     selectInput("fchosen","Discriminar por", choices = c("Posición"=0,"Est.Civ"=1,"Sexo"=2,
                                                                             "Grupo Cliente"=3,"Reg. Laboral"=4,
                                                                             "Cobro"=5,
                                                                             "Plan Com"=6,"Of.Cobradora"=7,
                                                                             "Forma Pago"=9,"Clase"=10,
                                                                             "Antigüedad"=11,"Segmento"=18,
                                                                             
                                                                             "Num.Polizas(Cancelar)"=12,"Num.Polizas(Impacto)"=13,
                                                                             "Provincia(Cancelar)"=14, "Provincia(Impacto)"=15,
                                                                             "Abrev.Cia(Cancelar)"=16, "Abrev.Cia(Impacto)"=17,
                                                                             "Clústeres"=20) 
                               ),
                    
                     
                     
                     numericInput(inputId = "idc", label="Escribe el ID del Cliente con póliza en vigor (Ejemplo: 11)",value=0),
                     
                     h4("¿Tienes dudas?"),
                     
                     h5("Discriminar por"),
                     
                     "Debes seleccionar de qué manera deseas discriminar a los asegurados. Si eliges Posición, se coloreará a los asegurados según estén en el primer (C1), segundo (C2), tercer(C3) o cuarto cuadrante (C4) - donde estos representas las zonas bajo riego - alto impacto, alto riesgo - alto impacto, bajo riesgo - bajo impacto, alto riesgo - bajo impacto; respectivamente.
                      A su vez, en la leyenda podrás observar el porcentaje de asegurados que se encuentra en cada cuadrante. En cambio, al seleccionar una variable podrás ver el efecto de cada uno de los niveles de la variable seleccionada sobre ambas variables respuesta; al igual que sucede con la opción de clústeres donde verás el efecto de los tres clústeres.
                      Las variables tienen el siguiente significado:",
                    div(""),
                     tags$b("Est.Civ:"),"El estado civil del Asegurado (C=casado, S=Soltero y O=Desconocido).",
                    div(""),
                    tags$b("Sexo:"),"Sexo del asegurado(F=Femenino, M=Masculino y S=Desconocido).",
                    div(""),
                    tags$b("Grupo Cliente:"),"Grupo al que pertenece el asegurado (Seguros y sin grupo).",
                    div(""),
                    tags$b("Reg.Laboral:"),"Regulación del asegurado (Regulación general y otros).",
                    div(""),
                    tags$b("Cobro:"),"Muestra cómo se realizó el cobro (Efectivo y No efectivo).",
                    div(""),
                    tags$b("Plan Com:"),"Muestra si fue adquirido con un Plan Comercial o no (Sí y No).",
                    div(""),
                    tags$b("Of.Cobradora:"),"Oficina Cobradora del seguro (Segresegur-Zaragoza, Segresegur Central y Unió de Llauradors).",
                    div(""),
                    tags$b("Forma Pago:"),"Periodicidad del pago (Anual y no anual).",
                    div(""),
                    tags$b("Clase:"),"Muestra si este es el primer año del asegurado o no (PRODUCCIÓN=Primer año, CARTERA=Renovación después del primer año).",
                    
                    div(""),
                    tags$b("Antigüedad:"),"Muestra los años que lleva el asegurado dado de alta. (Los niveles están en la forma (x,y] donde x e y son números naturales cuya interpretación es que el usuario tiene más de x años de antigüedad pero no más de y).",
                    div(""),
                    tags$b("Segmento:"),"Muestra la tres categoría a la que pertenece el asegurado según el eBroker (Bronce, Oro y Plata o VIP).",
                    
                    div(""),
                    tags$b("Num.Polizas(Cancelar)*:"),"Número de Pólizas Contratadas categorizada de acuerdo a la variable.",
                    div(""),
                    tags$b("Num.Polizas(Impacto)*:"),"Número de Pólizas Contratadas categorizada de acuerdo a la variable del impacto ecónomico.",
                    div(""),
                    tags$b("Provincia(Cancelar)*:"),"Comunidad de residencia el asegurado (Aragón, Valencia, Cataluña y Otros). Categorizada de acuerdo a la variable de cancelación.",
                    div(""),
                    tags$b("Provincia(Impacto)*:"),"Comunidad de residencia el asegurado (Aragón, Valencia, Cataluña y Otros). Categorizada de acuerdo a la variable del impacto ecónomico.",
                    div(""),
                    tags$b("Abrev.Cia(Cancelar)*:"),"Abreviatura  de la compañía aseguradora (Axa, Allianz, Adeslas...). Categorizada de acuerdo a la variable de cancelación.",
                    div(""),
                    tags$b("Abrev.Cia(Impacto)*:"),"Abreviatura  de la compañía aseguradora (Axa, Allianz, Adeslas...). Categorizada de acuerdo a la variable del impacto ecónomico.",
                    div(""),
                    tags$b("Clústeres:"),"Cluster o grupo, de los formados en el clustering, al que pertenecen los asegurados.(1=Cluster 1, 2=Cluster 2 y 3=Cluster 3).",
                    div(""),
                    h6("*La categorización de las variables acompañadas por un asterisco (*) es distinta en función de la variable de cancelación y la del impacto económico."),
                    
                    h5("Id del Cliente"),
                    "Debes escribir el Número del cliente que desee consultar y este se mostrará como un punto rojo en el mapa de riesgo (Si no se ha dado de baja). 
                    Además, podrás observar sus características en la tabla al final de la página." ),
                 
                 
        mainPanel(
            h2("¿Qué hace esta App?", align="center"),
            p("Esta App te permite observar cómo se distribuyen los asegurados en el mapa de riesgo formado por 2 ejes, uno que muestra el impacto económico (Eje Y) y el otro la probabilidad de cancelar alguna póliza (Eje X). 
            A su vez, utilizando las opciones del panel de la izquierda, te permite discriminar a los asegurados en función de su posición, un conjunto de variables categóricas o al clúster al que pertenece. Además, puedes observar la situación exacta de un determinado cliente en el mapa y sus características."),
            h4("Mapa de riesgo"),
            "La posición de cada cliente (punto) viene determinada por el riesgo de cancelar alguna póliza (Eje X) y el impacto económico (Eje Y). De esta manera, conforme mayor sea el valor de 
            cade uno de los ejes mayor es el riesgo y el impacto del usuario, respectivamente. Nota que solo los clientes con póliza(s) en vigor están en el mapa.",
           
            plotOutput("mapR",height="800px"),
           h4("Distribución de los niveles de la variable seleccionada en los cuatro cuadrantes"),
           
           "Aquí puedes observar el porcentaje de asegurados, para cada uno de los niveles de la variable seleccionada, que se encuentran en cada uno de los cuadrantes.
           En caso de no haber seleccionado ninguna variable, no aparecerá nada.",
           
           div(),
           
           tableOutput("tableporcen"),
           
           div(),
           
           h4("Gráfico de los efectos de las variables explicativas sobre las variables respuesta"),
           
           "Ambos gráficos muestran el efecto de cada uno de los niveles de las variables explicativas sobre la variable respuesta respecto a un cierto nivel de referencia; que se representa con la línea negra vertical.",
           
           "A la izquierda del gráfico se encuentra la información sobre las variables y el efecto que tiene cada uno de sus niveles sobre las ODDS (manera alternativa de expresar una probabilidad) escritas de la siguiente manera:",
          
           div(),
           
           em("Variable | Nivel | % de variación"),
          
           div(),
           
           "Nótese que todas las variables tienen un nivel que no aparece, por ejemplo la variable clase tiene los niveles Producción y cartera, pero no aparece Clase | Cartera | % variación - mientras que si aparece Clase | Producción | % variación. Esto se debe, a que el nivel omitido es el de referencia y el porcentaje muestra la variación entre el nivel que se esté observando y el de referencia.",
           
           div(),
           
           "Es menester mencionar que en el caso de la variable cancelación se muestra la variación de las ODDS mientras que en el de la variable del impacto económico se muestra la variación de su media.",
           
           div(),
           
           "Finalmente,  en caso de que la variable no sea categórica, como antigüedad en el gráfico de la variable impacto, se muestra el efecto por cada unidad que aumenta el valor de dicha variable numérica.",
           
           h5("Variable de Cancelación"),
           
           "El gráfico a continuación muestra el porcentaje de variación sobre las ODDS de cancelar alguna póliza al pasar del nivel de referencia, que se representa con la línea negra vertical, a otro nivel determinado.",
           
           div(""),
           
          "Para interpretar el gráfico es necesario entender el concepto de ODDS. Las ODDS son una manera alternativa de mostrar la probabilidad de ocurrencia de un evento (v.g cancelar una póliza) respecto a otro evento (v. g. No cancelar una póliza). En este caso, si un cliente tiene unas ODDS de 3, se debe interpretar como que el asegurado es tres veces más probable que cancele una póliza en contraposición a que no lo haga.",
         
           div(),
          
          "En el gráfico, no se muestran las ODDS sino la variación de las ODDS del nivel que está observado respecto al de referencia. Es decir, que un 50% se debe leer como que los asegurados de ese nivel tienen unas ODDS un 50% más altas de cancelar una póliza que los asegurados del nivel de referencia.",
           
           div(),
           
          "De esta manera, “Antigüedad | (2,10] | -47%” se debe leer como que los asegurados con una antigüedad superior a 2 y menor a 10 años tienen unas ODDS de cancelar una póliza un 47% menores respecto a las  del nivel de referencia, en este caso tener una antigüedad de menos de 2 años.",
          
          div(),
          
          "Asimismo, que el punto se encuentre a la derecha de la línea negra vertical es equivalente a que el porcentaje sea positivo y por lo tanto las ODDS de cancelar la póliza son mayores que en el nivel de referencia, mientras si está a la izquierda implica que las ODDS de cancelar la póliza son menores respecto a las del nivel de referencia.",
          
           
           plotOutput("clevlogit",height="700px"),
          
           h5("Variable del impacto"),
           
           "El gráfico a continuación muestra el porcentaje de variación sobre la esperanza de la variable impacto al pasar del nivel de referencia,cuyo porcentaje siempre es 0, a otro nivel determinado.
           Entonces, un valor positivo implica que el impacto económico es mayor y, uno  negativo implica que tiene un impacto inferior.",
          
           div(""),
          
           "A la izquierda de la gráfica se encuentra la información sobre la variable y el efecto que tiene cada uno de sus niveles sobre la esperanza de la variable escritos de la siguiente manera:",
           div(""),
          
           em("Variable | Nivel | % de variación"),
           
           div(""),
           
           "De esta manera, Forma Pago | No Anual | -36% se debe leer como que los asegurados con una forma de pago No Anual tiene una media de impacto un 36% menor que los del nivel de referencia, es decir los que han seleccionado la forma de pago Anual.
           ",
           "En caso de que la variable sea numérica, como sucede con la variable antigüedad, no tiene niveles y el porcentaje de variación se debe leer como la variación sobre la esperanza del impacto por cada unidad que aumenta la variable numérica.",
           
           plotOutput("clevgamma",height="700px"),
          
           h4("Características del cliente seleccionado"),
          
           "Aquí puedes ver las características del cliente seleccionado en el panel de la izquierda:",
          
           tableOutput("tablemr"),
        )
    ),
    
    tabPanel("Clustering", 
             
             
             sidebarPanel(
                 
                 sliderInput("rangox", "Selecciona el rango de Dim1:",
                             min = -5, max = 15,
                             value = c(-3,10)),
                 
                 sliderInput("rangoy", "Selecciona el rango de Dim2:",
                             min = -10, max = 10,
                             value = c(-7,5)),
                 selectInput("geomchosen","Elige cómo deseas representar las observaciones", choices = c("Puntos Y Nº de Cliente"=3,
                                                                                                         "Puntos"=1,
                                                                                                         "Nº de Cliente"=2) 
                 ),
                 numericInput(inputId = "idc2", label="Escribe el ID del Cliente",value=NA),
                 
                 h4("¿Tienes dudas?"),
                 h5("Sliders"),
                 "Debes variar los valores de los sliders para hacer zoom en la parte del mapa que desees observar con mayor nitidez.",
                 div(""),
                 h5("Representación de las observaciones"),
                 "Debes seleccionar si quieres que los asegurados se representen cómo puntos, con su número de cliente o ambos.",
                 h5("Id del Cliente"),
                 "Debes  escribir el Nº del cliente del que desees saber el cluster al que pertenece. La respuesta aparece justo debajo del gráfico."
             ),
             
             
             mainPanel(
                 h2("¿Qué hace esta App?", align="center"),
                 p("Esta App te permite observar los 3 clústeres, que se formaron a partir del análisis de conglomerados, sobre los ejes artificiales Dim1 y Dim 2 que están muy correlacionados con el dinero que pagan los asegurados y el número de pólizas que tienen, respectivamente.
                   Además, utilizando las opciones del panel de la izquierda, te permite  centrarte en la parte del gráfico que desees y también observar el clúster al que pertenece el cliente seleccionado."),
                 plotOutput("clust",height="800px"),
                 textOutput("txtc")
             )
             
             )

    
    
    
    
    ),
)




server <- function(input, output) {
    
    dx3<- reactive({
        
        load("dbd.Rdata")
        names(db.d)[22]<-"Abrev.Cia.x"
        names(db.d)[14]<-"Num Polizas"
        names(db.d)[1]<-"ind"
        
        db.d$F.Baja[is.na(db.d$F.Baja)]<-"2021-02-20"
        
        db.d$F.Baja <- as.Date(db.d$F.Baja, format='%d/%m/%y') 
        
        db.d$anti<-as.numeric(difftime(db.d$F.Baja,db.d$F.Alta, units = "weeks")/52.25)
        
        ve<-c("Tot.Pr.Actual","Tot.Com.Bruta","Tot.Pr.Net", "yimp","PR. Media")
        
        dx3<- na.omit(db.d[,c("Est.Civ","Sexo","Grupo Cliente.x","Reg. Laboral","Segmento","Cobro","Provincia", "Plan Com..x", "Abrev.Cia.x","Of.Cobradora","yc","Fis/Jur","Forma Pago","Clase","anti","Num Polizas",ve )])
        
       
         l1<-quantile(dx3[,"Tot.Pr.Actual"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Actual"])
        
        
        l2<-quantile(dx3[,"Tot.Com.Bruta"],probs=0.75)+3*IQR(dx3[,"Tot.Com.Bruta"])
        
        
        l3<-quantile(dx3[,"Tot.Pr.Net"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Net"])
        
        
        l4<-quantile(dx3[,"yimp"],probs=0.75)+3*IQR(dx3[,"yimp"])
        
        
        l5<-quantile(dx3[,"PR. Media"],probs=0.75)+3*IQR(dx3[,"PR. Media"])
        
        
        dx3<- dx3[dx3[,"Tot.Pr.Actual"]<round(l1), ]  ## 432 outliers
        
        dx3<- dx3[dx3[,"Tot.Com.Bruta"]<round(l2), ]  #78 outliers
       
        dx3<- dx3[dx3[,"Tot.Pr.Net"]<round(l3), ] #35 outliers
        
        dx3<- dx3[dx3[,"yimp"]<round(l4), ]  ## #68 outliers
        
        dx3<- dx3[dx3[,"PR. Media"]<round(l5), ]  ## 204 outliers
        
        dx3[,ve]<-scale(dx3[,ve])
        
        dx3
    })
    
    
    
    k3<- reactive({
        
        dx3<-dx3()
        
        dx3$yc<-as.numeric(dx3$yc)
        
        res.pca <- PCA(dx3,quali.sup=c(1:10,12:16), quanti.sup = c(11,15,16), ncp = 2)  
        
        Psi = res.pca$ind$coord[,1:2]
        
        set.seed(123)
        
        kmeans(Psi,3,nstart = 25)}) 

    
    
    lmod1<- reactive({
        load("dx1.Rdata")
       
        
        names(dx1)[9]<- "Abrev.Cia.x"
   
        glm(yc~Est.Civ+Sexo+Provincia+`Reg. Laboral`+f.numpol+`Abrev.Cia.x`+Cobro + Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+ fanti, data=dx1, family=binomial)
    })
    
    lmod2<- reactive({

        load("dx2.Rdata")
        names(dx2)[5]<- "Abrev.Cia.x"
        
        glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cia.x`+Of.Cobradora+f.numpol+`Forma Pago`+anti,data=dx2, family=Gamma("log"), maxit=150)
        
        
    })
        
    x<- reactive({
        load("dx1.Rdata")
        load("dx2.Rdata")
        names(dx2)[5]<- "Abrev.Cia.x"
        names(dx1)[9]<- "Abrev.Cia.x"
        
        dx1$pc<-100*predict(lmod1(), type="response")
        
        dx2$pimp<- predict(lmod2(), type="response")
        
        inner_join(dx1,dx2,by="ind") 
    
    })
    
    xsc<- reactive({
        load("dbd.Rdata")
        
        sel_noB<-data.frame(ind=db.d[which(is.na(db.d$F.Baja)),1])
        
        inner_join(x(),sel_noB,by="ind")  # Solo los clientes que no tienen ninguna póliza en vigor tienen una fecha, es decir  no un Na.
        
    })
output$mapR<- renderPlot({
    
    x<- xsc()
    
    xr<-x[, c("yimp","pimp","yc","pc","ind")]
    
    xe<- x[,c(1:4,6,8,10,14,15,16,37,19,36,7,23,9,26,5,17)]
    
    xe$fanti.y<-as.character(xe$fanti.y)
    
    palet<-c("royalblue","darkblue","skyblue","turquoise4")
    
    myimp<- 0.5*(max(xr$pimp)+min(xr$pimp))
    

    
    
    if(as.numeric(input$fchosen)!=0) {
    
    
        if(as.numeric(input$fchosen)==20){
        
        vr<- data.frame(ind=as.numeric(names(k3()$cluster)), clu=as.character(k3()$cluster))
        
        xe<-filter(xe, ind %in%  vr$ind)
        xr<-filter(xr, ind %in%  vr$ind)
        
        xe[,20]<-inner_join(xe,vr, by="ind")$clu
        
        
        
        } 
        
        v<- unique(xe[,as.numeric(input$fchosen)])
    
    col<- xe[,as.numeric(input$fchosen)]
    
    for(j in 1:length(v)){
        
        col[xe[,as.numeric(input$fchosen)]==v[j]]<- palet[j]
    }
    
    col2<- palet[1:length(unique(xe[,as.numeric(input$fchosen)]))]
    
    
    }else{
        
        
        
        bol1<- xr$pc<50 & xr$pimp>myimp
        bol2<- xr$pc<50 & xr$pimp<myimp
        bol3<-xr$pc>50 & xr$pimp>myimp
        bol4<-xr$pc>50 & xr$pimp<myimp     
        
        xr[bol1, "q"]<-palet[1]
        
        xr[bol2, "q"]<-palet[3]
        
        xr[bol3, "q"]<-palet[2]
        
        xr[bol4, "q"]<-palet[4]
        
        porcen<- c(sum(bol1),sum(bol2),sum(bol3),sum(bol4))*100/nrow(xr)
        
        v<- paste(c("C1", "C2", "C3", "C4"),paste(round(porcen,2), "%",sep=""),sep="-") 
    
        col<-xr$q
        
        col2<- palet
        }
    
        
    sel<-xr$ind==input$idc
    
        plot(xr$pc,xr$pimp, ylab="Impacto",xlab="Probabilidad de Cancelar", col=col,cex=1.6,pch=19)
        abline(v=50,lwd=2)
        abline(h=  myimp, lwd=2)
        points(xr$pc[sel],xr$pimp[sel],col="red", pch=19,cex=2)
        
        
    
        
    if(!is.na(input$idc) & sum(sel)>0){
        
        legend("topleft", legend=c(v,"Elegido"),col=c(col2,"red"), pch=19, cex=1.4,pt.cex=1.4) 
        
    } else{legend("topleft", legend=v,col=col2, pch=19, cex=1.4,pt.cex=1.4) }
    
    })




output$tableporcen<- renderTable({


    x<- xsc()
    
    myimp<- 0.5*(max(x$pimp)+min(x$pimp))
    
   if(as.numeric(input$fchosen)!=0){
     if(as.numeric(input$fchosen)==20) {
        
        vr<- data.frame(ind=as.numeric(names(k3()$cluster)), clu=as.character(k3()$cluster))
    
        x<-inner_join(x,vr, by="ind")
    }
    
    x$cuadrantes<- rep("C1", nrow(x))
    
    x$cuadrantes[x$pc>50 & x$pimp>myimp]<- "C2"
    
    x$cuadrantes[x$pc<50 & x$pimp<myimp]<- "C3"
    
    x$cuadrantes[x$pc>50 & x$pimp<myimp]<- "C4"
    
    
    xe<- x[,c(1:4,6,8,10,14,15,16,37,19,36,7,23,9,26,5,17)]
    
    if(as.numeric(input$fchosen)==20){
        
    
        varia<-x$clu
        df<-as.data.frame.matrix(table(varia, x$cuadrantes))
        100*df/rowSums(df)
        
        
    }else{
       
        varia<-xe[,as.numeric(input$fchosen)]
        df<-as.data.frame.matrix(table(varia, x$cuadrantes))
        100*df/rowSums(df)
    }
        
   }
},rownames = TRUE, align='c', bordered = T, hover = T, width="900px", digits=2)



output$tablemr<- renderTable({
    
    if(is.null(input$idc)| is.na(input$idc) ){
        
        
    } else {
        
        x1<- x()[,c(17,1:5,6,8,10,12,14,15,16,37,19,36,7,23,9,26)]
        
        names(x1)<-c("NºCliente","Est.Civ","Sexo", "Grupo Cliente","Reg.Laboral","Segmento","Cobro","Plan Com",
                     "Of Cobradora","PR. Media","Fis/Jur","Forma Pago","Clase","Antigüedad",
                     "Num Polizas(Cancelar)","Num Polizas(Impacto)","Provincia(Cancelar)","Provincia(Impacto)","Abrev.Cia (Cancelar)","Abrev.Cia(Impacto)")
        
        x1$Cluster <- rep(0, nrow(x1))
        
        if( sum(as.numeric(names(k3()$cluster))==input$idc)*sum(input$idc==x1[,1])>0){
        
        cl<- k3()$cluster[as.numeric(names(k3()$cluster))==input$idc]
        
            x1$Cluster[input$idc==x1[,1]]<-cl
        }else{
            x1$Cluster[input$idc==x1[,1]]<- NA                            
        }
        x1[input$idc==x()$ind,]
    }
})



output$clevlogit<- renderPlot({
    
    vlog<- c(1, 2, 
             14, 14,
             4, 12, 12, 
             16, 16, 
             5,7, 7, 
             6, 9,10, 11,11)
    
    
    dfb<-c("Est.Civ - O", "Sexo | S", 
           "Provincia | Otros", "Provincia | Valencia",
           "Reg.Laboral | Reg.General", "Núm Pólizas | 1", "Núm Pólizas | 2-3", 
           "Abrev.Cia | Axa o Mapfre", "Abrev.Cia | Otras", 
           "Cobro | No efectivo","Of.Cobradora | Segregur Central", "Of.Cobradora | Unió de Llauradors", 
           "Plan Com | Sí", "Forma Pago | No Anual","Clase | Producción","Antigüedad | (10,32]","Antigüedad | (2,10]")
    
    colores<-c("yellow3","orange", 
               "red","red",
               "violetred", "purple","purple",
               "green","green",
               "darkgreen","royalblue","royalblue",
               "blue","darkblue","brown","black","black")
    
    xyz<- data.frame(variable=vlog, valor=dfb ,colores=colores,xl=100*(exp(coef(lmod1())[-1])-1))

    lim<-c(-120,1250)
    bol<-xyz$variable==input$fchosen
    
    if(sum(bol)>0){
        
        xyz<- xyz[bol,]
    
    f<-max(xyz$xl)*min(xyz$xl)
    
    if(f>0){
     lim<-c(-5,max(xyz$xl)+5)
        if(max(xyz$xl)<0){lim<-c(min(xyz$xl)-5,5) }
    }
    
    }
    eti<-paste(xyz$valor, round(xyz$xl),sep=" | " )
    
    
    dotchart(xyz$xl,color=xyz$colores, cex=1.4, pch=16, main="Efecto de las variables" , xlab=expression(exp(beta)), labels=paste(eti,"%",sep=""), xlim=lim)
    abline(v=0, col=1)
    
})




output$clevgamma<- renderPlot({
    
    vg<- c(-4, 15, 15,15,
           6,17, 17, 17, 7,
           7,  13, 13,9,11)
    
    vv<- c("Fis/Jur | J", "Provincia | Cataluña", "Provincia | Otros","Provincia | Valencia",
           "Plan Com | Sí","Abrev.Cia | Generali ", "Abrev.Cia | Mapfre o Zurich", "Abrev.Cia | Otras", "Of.Cobradora | Segregur Central",
           "Of.Cobradora | Unió de Llauradors",  "Núm Pólizas | 1", "Núm Pólizas | 2-3","Forma Pago | No Anual","Antigüedad | Sin Niveles (Numérica) |")
    
    
    colores<-c("yellow3","orange", "orange","orange",
               "red","violetred","violetred", "violetred" ,"purple",
               "purple","green","green","darkgreen","royalblue")
    
    
    xyz<- data.frame(variable=vg, valor=vv ,colores=colores, xg=100*(exp(coef(lmod2())[-1])-1) )
    
    
    
    lim<-c(-65,140)
    bol<-xyz$variable==input$fchosen
    
    if(sum(bol)>0){
        
        xyz<- xyz[bol,]
        
        f<-max(xyz$xg)*min(xyz$xg)
        
        if(f>0){
            lim<-c(-5,max(xyz$xg)+5)
            if(max(xyz$xg)<0){lim<-c(min(xyz$xg)-5,5) }
        }
        
    }
    
    eti<-paste(xyz$valor, round(xyz$xg),sep=" | " )
    
    dotchart(xyz$xg,color=xyz$colores, cex=1.4, pch=16, main="Efecto de las variables" , xlab=expression(exp(beta)), labels=paste(eti,"%",sep=""), xlim=lim)
    abline(v=0, col=1)
    
})




output$clust<- renderPlot({
    
    geo<-c("point", "text")
    
    if(input$geomchosen!=3) {geo<-geo[as.numeric(input$geomchosen)]}
    
    ve<-c("Tot.Pr.Actual","Tot.Com.Bruta","Tot.Pr.Net", "yimp","PR. Media")
    
    fviz_cluster(k3(), geom =geo , data = dx3()[,ve], xlim=input$rangox,ylim=input$rangoy)+ theme(plot.title = element_text(face="bold", hjust=0.5,size=24))+ggtitle("Clustering")
})

output$txtc<- renderText({
    
    if(is.null(input$idc2)| is.na(input$idc2) ){
        "Por favor, seleccione el ID del cliente que desee consultar"
        
    } else {
        
        if(sum(as.numeric(names(k3()$cluster))==input$idc2)>0) {
            paste("El cliente pertenece al clúster",as.numeric(k3()$cluster)[as.numeric(names(k3()$cluster))==input$idc2],sep=" ")
        } else{"El Cliente no se encuentra en ningún clúster"} 
       
    }
})




}



shinyApp(ui = ui, server = server)
