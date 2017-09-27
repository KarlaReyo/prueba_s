#install.packages(c("shiny","shinythemes","dplyr","readr","ggplot2","forcats"))

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(scales)

#Notacion cientifica
options(scipen=999)

acl <- read.csv("Data.csv", header = T)
acl[is.na(acl)] <- 0

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
      
      selectInput("pob", "Filtro de beneficio:",
                  c("Folios Total" = "F_Universo",
                    "Folios Sin ATM/Diversas" = "F_Sin", 
                    "Folios Modelo" = "F_SFav",
                    "Folios Otros Beneficios" = "F_OFav",
                    "Importe Total" = "I_Universo",
                    "Importe Sin ATM/Diversas" = "I_Sin", 
                    "Importe Modelo" = "I_SFav",
                    "Importe Otros Beneficios" = "I_OFav"))
      
      
    ),
    
    # Panel para outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Gráficas", plotOutput("distPlot")),
                  tabPanel("Tabla", tableOutput("table"))
      )
      
    )
  )
)

# Definir servidor ----
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    filtro <- as.data.frame(acl[acl$Tipo == input$prod,])
    new <- select(filtro,one_of(c("Mes",as.character(input$pob),"Anio", "Tipo")))
    
    ggplot(new, aes(x= Mes,y=new[,2])) + 
      geom_bar(stat="identity", position="dodge", fill="#004cc0") +
      facet_wrap(~ Anio) + 
      scale_y_continuous("Folios", labels = comma) +   scale_x_discrete(name="Mes") + 
      aes(x = fct_inorder(Mes))

    })
  # Generar vista de tabla ----
  output$table <- renderTable({
    
    filtro <- as.data.frame(acl[acl$Tipo == input$prod,])
    
    nr <- which( colnames(filtro)== as.character(input$pob ))
    
    filtro[,c(1,nr,nr+1,nr+2)]
    
  })
}

shinyApp(ui = ui, server = server)