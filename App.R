compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    Sys.sleep(0.25)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }
  
  dat
}


library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = shinytheme("lumen"),
  titlePanel("Segmento Microfinanzas"),
          sidebarLayout(
                   sidebarPanel(
                      h2("Calculadora Microfinanzas"),
                      p("Permite calcular la remuneracion variable a partir de la informacion de productividad estimada de cada colaborador"),
                      
                      br(),
                      br(),
                      br(),
                      br(),
                      img(src = "bancopichincha.png", height = 70, width = 150),
                      br(),
                      "Es una app de  ", 
                      span("Control Financiero", style = "color:blue")
                    ),
                    mainPanel(  
                     tabsetPanel(
                       tabPanel("Asesor en Formacion",
                            p(strong(h4("Ingresar la informacion requerida:"))),
                            fluidRow(
                              column(4,selectInput("select1", h5("Tiempo laborando"), 
                                       choices = list("MES 1 a 4" = 1, "MES 5" = 2,
                                          "MES 6" = 3, "MES 7" = 4, "MES 8 a Mas" = 5), selected = 1)),
                        
                               column(4, numericInput("text1", h5("Monto en Ventas"), 
                                          value = 20000,step = 500)),
      
      
                              column(4, numericInput("text2", h5("Clientes Nuevos"),
                                          value = 4))
                            )
                            ,
                            p(strong(h4("Calcular Comision :"))),
                            fluidRow(
                              column(7, actionButton("action", "Remuneracion Variable"), 
                              br()),
                              br(), 
                              br(),
                              textOutput("text5"),
                              textOutput("text7"),
                              textOutput("text6"),
                              textOutput("text8"),
                              textOutput("text9"),
                             
                              tableOutput("table")
                            ),
                            fluidRow( 
                              h4("Pago Final"),
                             strong(verbatimTextOutput("txtout")) ,
                             tags$head(tags$style(HTML("
                            #txtout {color:gray; font-size:18px; font-style:bold; 
overflow-y:scroll; max-height: 50px; background: ghostwhite;}
                            
                            ")))
                            )
                            
                        ),
                       tabPanel("Asesor Microfinanzas", 
                                p(h4(strong("Ingresar la informacion requerida:"))),
                                fluidRow(
                                  column(4,selectInput("selectpuesto", strong(h5("Puesto")), 
                                                       choices = list("Asesor Senior"=1,"Asesor Intermedio"=2,"Asesor junior"=3))),
                                  column(4,
                                   selectInput("selectstock", strong(h5("Stock Clientes")), 
                                          choices = list("0-60 clientes" = 1, "61-100 clientes" = 2,
                                                         "101-130 clientes" = 3,"131-150 clientes" = 4,"151-170 clientes" = 5
                                                         ,"171-180 clientes" = 6,"181-190 clientes" = 7,"191-199 clientes" = 8
                                                         ,"200 a mas clientes" = 9), selected = 1))
                                  
                      
                                 ),
                                fluidRow(
                                  column(width = 3,
                                         div(class = "option-group",
                                             h4(strong(div(class = "option-header", "Crecimiento clientes"))),
                                             sliderInput("text01", "Metas", min=0, max=25, value=0,
                                                         step=1)
                                         ),
                                         div(class = "option-group",
                                             sliderInput("text02", "Real", min=-20, max=30, value=0,
                                                         step=1)
                                             
                                         )
                                  ),
                                  column(width = 3,
                                         div(class = "option-group",
                                             h4(strong(div(class = "option-header", "Crecimiento Saldo"))),
                                             numericInput("text03", h5("Meta"), 
                                                                    value = 20000,step = 500)
                                         ),
                                         div(class = "option-group",
                                             numericInput("text04", h5("Real"), 
                                                           value = 20000,step = 500)
                                             
                                         )
     
                                  )
                                  ,
                                  column(width = 3,
                                         div(class = "option-group",
                                             h4(strong(div(class = "option-header", "Numero de operaciones"))),
                                             sliderInput("text05", "Metas", min=0, max=25, value=0,
                                                         step=1)
                                         ),
                                         div(class = "option-group",
                                             sliderInput("text06", "Real", min=0, max=50, value=0,
                                                         step=1)
                                             
                                         )
                                  )
                                  ,
                                  column(width = 3,
                                         h4(strong(div(class = "option-header", "Ventas"))),
                                         numericInput("text07", strong(h5("Real estimado")), 
                                                      value = 20000,step = 500),
                                         sliderInput("text08", "Tasa promedio estimada", min=0, max=100, value=0,
                                                     step=1)
                                         
                                  )
                                  #column(
                                    
                                  #)
                                  
                                ),
                                
                              fluidRow(
                              
                               
                              
                                actionButton("button", "Calcular Comision",class = "btn-primary"),
                                               br(),
                              #  radioButtons('style', 'Progress bar style', c('notification', 'old')),
                                
                                             # textOutput("textaa1"),
                                             # textOutput("textaa2"),
                                             # textOutput("textaa3"),
                                             # textOutput("textaa4"),
                                tableOutput("tableasesores1")
                              ),
                              fluidRow( 
                                h4("Pago Final"),
                                strong(verbatimTextOutput("asesorespago")) ,
                                tags$head(tags$style(HTML("
                            #txtout {color:gray; font-size:18px; font-style:bold; 
overflow-y:scroll; max-height: 50px; background: ghostwhite;}
                            
                            ")))
                              )
                             
                              
                              
                        ),
                        tabPanel("Asesor Pyme",
                                 p(h4(strong("Ingresar la informacion requerida:"))),
                                 fluidRow(
                                   column(4,numericInput("pymes1", strong(h5("Tasa de Desembolso")), 
                                                                          value = 15,step = 0.01) 
                                                        ),
                                   column(4,
                                          numericInput("pymes2", strong(h5("Tasa de cartera")), 
                                                       value = 15,step = 0.01 )),
                                   column(4,
                                          numericInput("pymes3", strong(h5("Crecimiento Cartera")), 
                                                       value = 10000,step = 1000 ))
                                   
                                   
                                 ),
                                 fluidRow(actionButton("buttonpymes", "Calcular Comision")
                                 ),
                                 fluidRow( 
                                   h4("Pago Final"),
                                   strong(verbatimTextOutput("pymespago")) ,
                                   tags$head(tags$style(HTML("
                                                             #txtout {color:gray; font-size:18px; font-style:bold; 
                                                             overflow-y:scroll; max-height: 50px; background: ghostwhite;}
                                                             
                                                             ")))
                                   )
                        )
                    )
                     
                    
            )
    )
)




server <- function(input, output) {
  values <- reactiveValues()
  observe({ values$metasopera<-(if(input$select1==1) 0
  
    else if(input$select1==2) {7}
      else  if(input$select1==3)  {9}
        else if(input$select1==4) {10}

           else
              {12}
        )
  })
  observe({values$opera<-input$text2})
  
  observe({values$venta<-input$text1})
  observe({ values$metasventa<-((if(input$select1==1){ 0}
                         else if(input$select1==2){24000}
                         else if(input$select1==3) {27000}
                         else if(input$select1==4){ 30000}
                         else 50000
                           ))
 
  })
  
  observe({values$cventa<-(if (input$select1==1) {0} 
                            else {values$venta/values$metasventa})})
  observe({values$copera<-(if (input$select1==1)
                           {0} else {values$opera/values$metasopera})})
  
  observe({values$comiventa<-(if (values$cventa<1) {0} 
                           else if (input$select1==2 ) {100}
                           else if (input$select1==3 ) {200}
                           else if (input$select1==4 ) {200}
                           else  {300})})
  
  observe({values$comiopera<-(if (values$copera<1) {0} 
                    
                           else  {300})})
  observe({values$comision<-(values$comiventa+values$comiopera)})
  
 # paste(round(100*m, 2), "%", sep="")
  
  observe({values$table<-data.frame(paste(round(values$cventa*100,2),"%",sep=" "),values$comiventa,paste(round(values$copera*100,2),"%",sep=" "),values$comiopera)})
  observe({names(values$table)[1]<-c("Cumplimiento venta")
  names(values$table)[2]<-c("Comision venta")
  names(values$table)[3]<-c("Cumplimiento Clientes")
  names(values$table)[4]<-c("Comision Clientes")
  })
  
  output$table <-renderTable(values$table,align="c")
  
  
  
  
  output$txtout <- renderText({
    paste(values$comision)
  })
  

    


  observe({
    values$operacioesserior<-     c(0,100,130,160,190,220,250,360,440,520,600,680,780,880,980,1080,1180,1560)
    values$operacionesintermedio<-c(0,80,100,120,140,160,180,200,350,400,450,500,550,650,700,750,800,900,1200)
    values$operacionesjunior<-    c(0,40,55,70,85,100,115,130,155,180,205,230,255,295,335,375,415,455,600)
    
    ########### Genero matriz crecimiento clientes###########
    values$c1s<-c(seq(0,440,by=40),640)
    values$c2s<-c(seq(0,660,by=60),960)
    values$c3s<-c(seq(0,880,by=80),1280)
    values$c4s<-c(seq(0,1100,by=100),1600)
    
    values$c5s<-c(seq(0,1100,by=110),1100,1700)
    values$c6s<-c(seq(0,1200,by=120),1200,1800)
    values$c7s<-c(seq(0,1300,by=130),1300,1900)
    values$c8s<-c(seq(0,1400,by=140),1400,2000)
    values$c9s<-c(seq(0,1500,by=150),1500,2100)
    
    values$clientess<-matrix(rbind(values$c1s,values$c2s,values$c3s,values$c4s,values$c5s,values$c6s,values$c7s,values$c8s,values$c9s),nrow=9)
    ########################################
    values$c1i<-c(seq(0,220,by=20),320)
    values$c2i<-c(seq(0,440,by=40),640)
    values$c3i<-c(seq(0,660,by=60),960)
    values$c4i<-c(seq(0,880,by=80),1280)
    
    values$c5i<-c(seq(0,900,by=90),900,1380)
    values$c6i<-c(seq(0,1000,by=100),1000,1480)
    values$c7i<-c(seq(0,1100,by=110),1100,1580)
    values$c8i<-c(seq(0,1200,by=120),1200,1680)
    values$c9i<-c(seq(0,1300,by=130),1300,1780)
    
    values$clientesi<-matrix(rbind(values$c1i,values$c2i,values$c3i,values$c4i,values$c5i,values$c6i,values$c7i,values$c8i,values$c9i),nrow=9)
    #########################################
    values$c1j<-c(seq(0,110,by=10),160)
    values$c2j<-c(seq(0,165,by=15),240)
    values$c3j<-c(seq(0,330,by=30),480)
    values$c4j<-c(seq(0,550,by=50),800)
    
    values$c5j<-c(seq(0,600,by=60),600,900)
    values$c6j<-c(seq(0,700,by=70),700,1000)
    values$c7j<-c(seq(0,800,by=80),800,1100)
    values$c8j<-c(seq(0,900,by=90),900,1200)
    values$c9j<-c(seq(0,1000,by=100),1000,1300)
    
    values$clientesj<-matrix(rbind(values$c1j,values$c2j,values$c3j,values$c4j,values$c5j,values$c6j,values$c7j,values$c8j,values$c9j),nrow=9)
    ##################################
    values$saldoss<-matrix(c(450,720,900,900,900,1440,1800,1800,900,1440,1800,2430),nrow=3,byrow=TRUE)
    values$saldosi<-matrix(c(375,550,750,750,750,1100,1500,1500,750,1100,1500,2025),nrow=3,byrow=TRUE)
    values$saldosj<-matrix(c(250,400,550,550,500,800,1100,1100,500,800,1100,1485),nrow=3,byrow=TRUE)
    
#########calculo para crecimiento#######
    values$cumplimientocrecimiento<-round(({if(input$text01>0 & input$text02==0){0}
      else if(input$text01==0 & input$text02==0) {100}
      else if (input$text01>=0 & input$text02<0){0}
      else if (input$text01<=0 & input$text02>0){130}
      else if (input$text01<0 & input$text02<0){input$text01/input$text02*100}
      else if (input$text01<0 & input$text02<0 & input$text01/input$text02>1.3){130}
      else if (input$text01<0 & input$text02<0 ){input$text01/input$text02*100}
      else if (input$text01<0 & input$text02>=0 ){130}
      else if (input$text01>0 & input$text02>0 & input$text02/input$text01>1.3){130}
      else {input$text02/input$text01*100}
    })
    ,2)
    values$cumplimientosaldo<-round(({if(input$text03>0 & input$text04==0){0}
      else if(input$text03==0 & input$text04==0) {100}
      else if (input$text03>=0 & input$text04<0){0}
      else if (input$text03<=0 & input$text04>0){130}
      else if (input$text03<0 & input$text04<0){input$text01/input$text04*100}
      else if (input$text03<0 & input$text04<0 & input$text01/input$text04>1.3){130}
      else if (input$text03<0 & input$text04<0 ){input$text01/input$text04*100}
      else if (input$text03<0 & input$text04>=0 ){130}
      else if (input$text03>0 & input$text04>0 & input$text04/input$text03>1.3){130}
      else {input$text04/input$text03*100}
    })
    ,2)
    
    values$cumplimientooperaciones<-({if(input$text05<=0){0}
                              else if (input$text06/input$text05>1.3){130}
                              else {input$text06/input$text05}

    })
    
    
################comsion clientes###########
    values$crecirow<-as.numeric(input$selectstock)
    values$crecicol<-as.numeric((if (input$text02<=0){1}
          else if (input$text02<11){input$text02+1}
          else if (input$text02<16){12}
         else {13}))

values$pagocrecimiento<-(if (input$selectpuesto==1){ values$clientess[values$crecirow,values$crecicol]*(if  (values$cumplimientocrecimiento>=100) {1.2} else {1} )  }
                         else if (input$selectpuesto==2){ values$clientesi[values$crecirow,values$crecicol]*(if  (values$cumplimientocrecimiento>=100) {1.2} else {1} )  }

                          else { values$clientesj[values$crecirow,values$crecicol]*(if  (values$cumplimientocrecimiento>=100) {1.2} else {1} )  })

################comision saldo############

values$saldocol<-(if(values$cumplimientosaldo<90.0001) {1}
                  else if (values$cumplimientosaldo<100.0001 & values$cumplimientosaldo>=90.00001) {2}
                  else if (values$cumplimientosaldo<129.9999 & values$cumplimientosaldo>=100.0001) {3}
                  else  {4})
values$saldorow<-(if (input$text06<=11){1}
                  else if (input$text06<=15){2}
                  else {3}
                   )
values$pagosaldo <-(if (input$selectpuesto==1){ values$saldoss[values$saldorow,values$saldocol]*(if  (input$text02<0) {0} else {1} )* (if  (input$text06<9) {0} else {1} )*(if  (values$cumplimientosaldo<79.999) {0} else {1} ) }
                    else if (input$selectpuesto==2){ values$saldosi[values$saldorow,values$saldocol]*(if  (input$text02<0) {0} else {1} )* (if  (input$text06<9) {0} else {1} )*(if  (values$cumplimientosaldo<79.999) {0} else {1} ) }
                    
                    else { values$saldosj[values$saldorow,values$saldocol]*(if  (input$text02<0) {0} else {1} )* (if  (input$text06<9) {0} else {1} )*(if  (values$cumplimientosaldo<79.999) {0} else {1} )  })        


values$operacol<-(if(input$text06<9) {1}
                  else if (input$text06<26) {input$text06-8}
                  else {18})

values$pagoopera<-((if (input$selectpuesto==1){values$operacioesserior[values$operacol]}
                    else if (input$selectpuesto==2){values$operacioesintermedio[values$operacol]}
                   else {values$operacioesjunior[values$operacol]})*(if (input$text01<0) {0.6} else {1}))

  })
################################################33
  observeEvent(input$button,{
    
    style <- isolate("notification")
    progress <- shiny::Progress$new(style = style)
    
    progress$set(message = "Computing data", value = 0)
  ###############modelo de predicion#############################################
  library(readxl)
  values$VENTAS_MICROFINANZAS_MODELO <- read_excel(/data/VENTAS MICROFINANZAS MODELOl.xlsx)
  library(lattice)
  library(foreach)
  library(Matrix)
  library(ggplot2)
  library(caret)
  library(glmnet)
  library(C50)
  library(caTools)
  set.seed(42)
  values$NUMERO<-round(nrow(values$VENTAS_MICROFINANZAS_MODELO)*0.6)
  #train<-VENTAS_MICROFINANZAS_MODELO[1:NUMERO,]
  values$train<-values$VENTAS_MICROFINANZAS_MODELO
  values$test<-values$VENTAS_MICROFINANZAS_MODELO[values$NUMERO:nrow(values$VENTAS_MICROFINANZAS_MODELO),]
  # Fit linear regression model
  values$myGrid <- data.frame(mtry = 1,splitrule="variance",min.node.size=1)
  progress$set(message = "Computing data", value = 0.2)
  values$modelranger <- train(comision ~ monto+tasa+operaciones, data=values$train,
                              method = "ranger",tuneGrid = values$myGrid,
                              trControl = trainControl(
                                method = "cv", number = 10,
                                verboseIter = TRUE
                              )
  )
  values$prediccion<-data.frame(monto=input$text07,tasa=input$text08,operaciones=input$text06)
  values $pagoventa<-predict(values$modelranger,values$prediccion)*input$text06
  ###############################################################################
  
  #output$textaa1 <- renderText({
  #  paste(values$pagocrecimiento)})
 # output$textaa2 <- renderText({
  #  paste(values$pagosaldo)})
  #output$textaa3 <- renderText({
  #  paste(values$pagoopera)})
  
 # output$textaa4 <- renderText({
  #  paste(values $pagoventa)})
  
  observe({values$pagosasesoresfinal<-values$pagoventa+values$pagocrecimiento+values$pagosaldo+values$pagoopera})

  observe({values$tableasesores<-data.frame(values$cumplimientocrecimiento,values$pagocrecimiento,values$cumplimientosaldo,values$pagosaldo,values$pagoopera,values$pagoventa)})
  observe({names(values$tableasesores)[1]<-c("Cumplimiento Crecimiento Cliente")
  names(values$tableasesores)[2]<-c("Comision Crecimiento Cliente")
  names(values$tableasesores)[3]<-c("Cumplimiento Saldo")
  names(values$tableasesores)[4]<-c("Comision Saldo") 
  names(values$tableasesores)[5]<-c("Comision Operaciones")
  names(values$tableasesores)[6]<-c("Comision ventas")})
  
  
  output$tableasesores1 <-renderTable(values$tableasesores)
  progress$set(message = "Computing data", value = 0.8)
  output$asesorespago <- renderText({
    paste(values$pagosasesoresfinal)})

  

  
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }
 # compute_data(updateProgress)

  })
  
  ########################PYMES


  
  # Compute the new data, and pass in the updateProgress function so
  # that it can update the progress indicator.

  
  
  observeEvent(input$buttonpymes,{

  
  values$comisionpumes<-matrix(c(0,	0	,0,	0	,0,	0,0	,0,	1020,	1700,	1870,	2040,170,	680,	1190,	2380,	2890,	3060,340,	850,	1360,	2720,	3050,	3230,
                               510,	1010,	1530,	2880,	3220,	3400,670	,1180	,1690,	3040,	3390,	4080),nrow=6,byrow=TRUE)

                              
  values$rowpymes<-if (input$pymes3<0){1}
                   else if(input$pymes3<70000){2}
                   else if(input$pymes3<150000){3}
                   else if (input$pymes3<300000){4}
                   else if (input$pymes3<450000){5}
                   else {6}
  values$difpyme<-input$pymes1-input$pymes2       
  
  values$colpymes<-if (values$difpyme<=-0.7){1}
                 else if (values$difpyme<=-0.2){2}
                 else if (values$difpyme<=0){3}
                 else if(values$difpyme<=1){4}
                 else if (values$difpyme<=2.5){5}
                else {6}
  values$pagospymesfinal<- values$comisionpumes[values$rowpymes,values$colpymes]
  output$pymespago <- renderText({
    paste(values$pagospymesfinal)})
  
                        
  
  })
 
  
  installed.packages()
  

  #############33
    


  
}
shinyApp(ui, server)
