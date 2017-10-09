#install.packages(c("shiny","shinythemes","dplyr","readr","ggplot2","forcats"))

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(scales)
library(RColorBrewer)
library(reshape2)
library(plotly)

#Notacion cientifica
options(scipen=999)

#Paleta
citi <- c( "#00b0b9", "#002d72","#0095f2", "#53565a", "#ed8b00", "#890c58", "#a05eb5","#84bd00","#007377")

key <- kpi_gen
key$kpi <- sample(100, size = nrow(key), replace = TRUE)

npv <- npv_y2

ans <- resp

ch <- data.frame(
  Map(function(x,y) if(all(is.numeric(x),is.numeric(y))) x * y else x, ans, npv)
)

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
           tags$div(class="cuadro", h1("Management Console"))
  ),
  
  # Panel para inputs ----
  column(2,
         
         radioButtons("eleg", "Filtro de elegibilidad:",
                      c("Elegible" = "TRUE",
                        "No Elegible" = "FALSE"))
         
         
         #selectInput("riesgo", "Filtro de riesgo:",
          #           c('TC' = 'TC','CP' = 'CP','CN' = 'CN','CLI' ='CLI',
           #            'DB' = 'DB','EFE' = 'EFE','AAC' = 'AAC'))
  ),
  
  column(2,
         checkboxGroupInput("riesgo", "Filtro de riesgo:",
                            c('AAA' = 'AAA',
                              'AA' = 'AA',
                              'A' = 'AA',
                              'BBB' = 'BBB',
                              'BB' = 'BB',
                              'B' = 'B'))
  ),
  
  # Panel para outputs ----
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("NCM", plotOutput("NCM")),
                tabPanel("ANR", plotOutput("ANR")),
                tabPanel("Colocacion", plotOutput("Colocacion"))
    ),
    br(),
    tabsetPanel(type = "tabs",
                tabPanel("Productos", plotOutput("Productos")),
                tabPanel("Tabla", tableOutput("Tabla"))
    )
    
  )
)

# Definir servidor ----
server <- function(input, output, session) {
  
  # Tabla de NCM ----
  output$NCM <- renderPlot({
    
    filtro <- as.data.frame(key[key$eleg == input$eleg,])
    #filtro <- as.data.frame(filtro[filtro$seg_riesgo == input$riesgo,])
    filtro <- filtro %>% filter(seg_riesgo %in% input$riesgo)

    ggplot(filtro, aes(x=t, y=kpi, color=seg_riesgo)) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 11), se = FALSE) +
      scale_y_continuous("NCM", labels = comma) +   
      scale_x_continuous(name="Mes")+
      scale_color_manual(values=citi)
    
    #ggplotly(p)
    
  })
  # Tabla de ANR ----
  output$ANR <- renderPlot({
    
    filtro <- as.data.frame(key[key$eleg == input$eleg,])
    filtro <- as.data.frame(filtro[filtro$seg_riesgo == input$riesgo,])
    #filtro <- filtro %>% filter(prod %in% input$prod)
    
    #p  <- 
    ggplot(filtro, aes(x=t, y=kpi, color=prod)) +
      geom_line()+
      scale_y_continuous("ANR", labels = comma) +   
      scale_x_continuous(name="Mes")+
      scale_color_manual(values=citi)
    
    #ggplotly(p)
    
  })
  # Tabla de Colocacion ----
  output$Colocacion <- renderPlot({
    
    filtro <- as.data.frame(key[key$eleg == input$eleg,])
    filtro <- as.data.frame(filtro[filtro$seg_riesgo == input$riesgo,])
    #filtro <- filtro %>% filter(prod %in% input$prod)
    
    #p  <- 
    ggplot(filtro, aes(x=t, y=kpi, color=prod)) +
      geom_line()+
      scale_y_continuous("Colocacion", labels = comma) +   
      scale_x_continuous(name="Mes")+
      scale_color_manual(values=citi)
    
    #ggplotly(p)
    
  })
  
  # Tabla de Productos ----
  output$Productos <- renderPlot({
    
    ans[is.na(ans)] <- 0
    ans[,2:8][ans[,2:8] > 0] <- 1
    #filtro <- as.data.frame(ans[ans$seg_riesgo == input$seg,])
    ans.m = melt(ans, id.var="seg_riesgo")
    ans.s <- ddply(ans.m, .(variable), 
                   summarize, 
                   value = sum(value))
    
    ch$max <- colnames(ch[,2:8])[apply(ch[,2:8],1,which.max)]
    t = as.data.frame(table(ch$max))
    names(t)[1] = 'variable'
    gr <- merge(x=ans.s,y=t,by="variable", all.x = TRUE)
    names(gr)[1] = 'producto'
    gr[is.na(gr)] <- 0
    gr$eleg <- gr$Freq/gr$value
    gr$total <- 1- gr$eleg
    gr[is.na(gr)] <- 0
    
    gr.m = melt(gr[,c(1,4:5)], id.var="producto")
    
    ggplot(gr.m, aes(x = producto, y = value, fill=variable)) +
      geom_bar(stat="identity") +
      labs(x="Producto", y="Ctes")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_manual(values=citi)
    
    # ggplotly(p)
    
  })
  
  # Generar vista de tabla ----
  output$Tabla <- renderTable({
    
    gr[,1:3]
    
  })
}

shinyApp(ui = ui, server = server)
