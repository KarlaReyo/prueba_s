#install.packages(c("shiny","shinythemes","dplyr","readr","ggplot2","forcats"))

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(scales)
library(RColorBrewer)

#Notacion cientifica
options(scipen=999)

setwd("~/R/App-4")
ctes <- read.csv("Ctes.csv", header = T)
ctes[is.na(ctes)] <- 0

sdo <- read.csv("Importe.csv", header = T)
sdo[is.na(sdo)] <- 0

ratio <- read.csv("Ratios.csv", header = T)
ratio[is.na(ratio)] <- 0

casos <- read.csv("Casos.csv", header = T)
casos[is.na(casos)] <- 0

alertas <- read.csv("Alertas.csv", header = T)
alertas[is.na(alertas)] <- 0

bnet <- read.csv("Bnet.csv", header = T)
bnet[is.na(bnet)] <- 0

tarjeta <- read.csv("Tarjeta.csv", header = T)
tarjeta[is.na(tarjeta)] <- 0

# Definir UI
ui <- fluidPage( 
  
  #Links and css resources
  includeCSS("app1.css"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
  ),
  
  br(),
  
  tags$div(class="container-fluid", 
           br(), 
           img(src='logo.png', align = "left"),
           tags$div(class="cuadro", h1("Modelo de Aclaraciones"))
  ),
  
  # Layout ventana para definiciones de input y output ----
  sidebarLayout(
    
    # Panel para inputs ----
    sidebarPanel(
      
      radioButtons("prod", "Filtro de producto:",
                   c("Crédito" = "Credito",
                     "Débito" = "Debito")),
      
      br(),
      
      selectInput("seg", "Filtro de segmento:",
                  c('Citibanamex1' = 'Citibanamex1',
                    'Priority' = 'Priority',
                    'CitiGold 1' = 'CitiGold 1',
                    'CitiGold 2' = 'CitiGold 2',
                    'CPC' = 'CPC',
                    'Ingresos Bajos - Adultos' = 'Ingresos Bajos - Adultos',
                    'Ingresos Bajos - Jovenes' = 'Ingresos Bajos - Jovenes',
                    'Ingresos Marginales' = 'Ingresos Marginales',
                    'Ingresos Marginales Nomina' = 'Ingresos Marginales Nomina',
                    'Ingresos Medios - Adultos' = 'Ingresos Medios - Adultos',
                    'Ingresos Medios - Jovenes' = 'Ingresos Medios - Jovenes',
                    'Jovenes Profesionistas' = 'Jovenes Profesionistas',
                    'Jubilados No Nomina' = 'Jubilados No Nomina',
                    'Jubilados Nomina' = 'Jubilados Nomina',
                    'PyME PFAE' = 'PyME PFAE',
                    'Sin Segmento' = 'Sin Segmento'))
      
    ),
    
    # Panel para outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Clientes", plotOutput("Clientes")),
                  tabPanel("Importes", plotOutput("Importes")),
                  tabPanel("Ratio", plotOutput("Ratio")),
                  tabPanel("Llamada", plotOutput("Llamada")),
                  tabPanel("Alertas", plotOutput("Alertas")),
                  tabPanel("Canal", plotOutput("Canal")),
                  tabPanel("TP", plotOutput("TP"))
      )
      
    )
  )
)

# Definir servidor ----
server <- function(input, output) {
  
  imp <- reactive({
    prod <- switch(input$prod,
                   Credito = "Saldo",
                   Debito = "Balance")
  })
  tx <- reactive({
    prod <- switch(input$prod,
                   Credito = "Ratioc",
                   Debito = "Ratiod")
  })
 
  # Tabla de clientes ----
  output$Clientes <- renderPlot({
    
    new <- ddply(ctes, .(Segmento, Tipo, Mes, Edad,Anio), 
                 summarize, 
                 Ctes = sum(Ctes))
    
    filtro <- as.data.frame(new[new$Tipo == input$prod,])
    filtro <- as.data.frame(filtro[filtro$Segmento == input$seg,])
    
    ggplot(filtro, aes(x= Mes,y=Ctes, fill=Edad)) + 
      geom_bar(stat="identity") +
      facet_wrap(~ Anio) + 
      scale_y_continuous("Clientes", labels = comma) +   scale_x_discrete(name="Mes") +
      aes(x = fct_inorder(Mes)) +
      scale_fill_brewer()+
      theme_minimal()
    
  })
  # Tabla de importes ----
  output$Importes <- renderPlot({
    
    imp <- imp()
    
    filtro <- as.data.frame(sdo[sdo$Importe == imp,])
    filtro <- as.data.frame(filtro[filtro$Segmento == input$seg,])
    
    ggplot(filtro, aes(x= Mes,y=Mediana)) + 
      geom_bar(stat="identity", position="dodge", fill="#0066ff") +
      facet_wrap(~ Anio) + 
      scale_y_continuous("Importe", labels = comma) +   
      scale_x_discrete(name="Mes") + 
      aes(x = fct_inorder(Mes))
    
  })
  
  # Tabla de Ratio ----
  output$Ratio <- renderPlot({
    
    tx <- tx()
    
    filtro <- as.data.frame(ratio[ratio$Tipo == input$prod,])
    filtro <- as.data.frame(filtro[filtro$Segmento == input$seg,])
    new <- select(filtro,one_of(c("Mes",as.character(tx),"Anio", "Tipo","Segmento","Ctes")))
    
    ggplot(new, aes(x= Mes,y=new[,2],fill=Ctes)) + 
      geom_bar(stat="identity", position="dodge", fill="#0B2161") +
      facet_wrap(~ Anio) + 
      scale_y_continuous("Acl/Tx", labels = scales::percent) +   scale_x_discrete(name="Mes") +
      aes(x = fct_inorder(Mes))

  })
  # Tabla de Contacto ----
  output$Llamada <- renderPlot({
    
    filtro <- as.data.frame(casos[casos$Tipo == input$prod,])
    filtro <- as.data.frame(filtro[filtro$Segmento == input$seg,])
    
    ggplot(filtro, aes(x= Mes,y=Folios)) + 
      geom_bar(stat="identity", position="dodge", fill="#004cc0") +
      facet_wrap(~ Anio) + 
      scale_y_continuous("Contacto", labels = comma) +   scale_x_discrete(name="Mes") + 
      aes(x = fct_inorder(Mes))
    
  })
  # Tabla de Alertas ----
  output$Alertas <- renderPlot({
    
    filtro <- as.data.frame(alertas[alertas$Tipo == input$prod,])
    
    ggplot(filtro, aes(x= Segmento,y=Ctes,fill=Notificacion)) + 
      geom_bar(stat="identity") +
      scale_y_continuous("Segmento", labels = comma) +   
      scale_x_discrete(name="Alertas") +
      scale_fill_brewer()+
      theme_minimal()+
      coord_polar(theta = "y")+
      geom_text(data = filtro, hjust = 1, size = 3, aes(x = Segmento, y = 0, label = Segmento)) +
      theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
    
  })
  # Tabla de Canal ----
  output$Canal <- renderPlot({
    
    filtro <- as.data.frame(bnet[bnet$Tipo == input$prod,])
    cols <- c(Ambas="#0561FF",App="#1782FF", 
              Bancanet="#1962BF", Ninguna="#C1D5FF") 
    
    ggplot(filtro, aes(x= Canal,y=Ctes,fill=Canal)) + 
      geom_bar(stat="identity") +
      scale_y_continuous("Clientes", labels = comma) +   
      scale_x_discrete(name="Canal") +
      scale_fill_manual(values = cols)+
      coord_polar(theta = "y")+
      geom_text(data = filtro, hjust = 1, size = 3, aes(x = Canal, y = 0, label = Canal)) +
      theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
  })
  # Tabla de TP ----
  output$TP <- renderPlot({
    
    filtro <- filter(tarjeta, !Uso %in% c('Sin Descripcion','Wallet','N/I','Contactless','Internet'))
    filtro <- as.data.frame(filtro[filtro$Tipo == input$prod,])
    
    ggplot(filtro, aes(x= Uso,y=Ctes,fill=Tarjeta)) + 
      geom_bar(stat="sum", show.legend = FALSE) +
      scale_y_continuous("Tarjeta", labels = comma) +   
      scale_x_discrete(name="Uso") +
      scale_fill_brewer()+
      theme_minimal()
    
  })

}

shinyApp(ui = ui, server = server)
