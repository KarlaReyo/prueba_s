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

tab <- read.csv("Data.csv", header = T)
tab[is.na(tab)] <- 0

# Definir UI
ui <- fluidPage(
  
  #Links and css resources
  includeCSS("app1.css"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
  ),
  
  br(),
  
  tags$div(br(), 
           img(src='logo.png', align = "left"),
           tags$div(h1("Aclaraciones Top Negocio"), align = "center")
  ),
  
  # Layout ventana para definiciones de input y output ----
  sidebarLayout(
    
    # Panel para inputs ----
    sidebarPanel(
      
      selectInput("nombre", "Filtro de beneficio:",
                  c('ADO WALLET' = 'ADO WALLET',
                    'AEROMEX CALL CENT SE' = 'AEROMEX CALL CENT SE',
                    'AEROMEX CTRO T INT H2H' = 'AEROMEX CTRO T INT H2H',
                    'AEROMEXICO AV OL' = 'AEROMEXICO AV OL',
                    'APPLE ONLINE STORE' = 'APPLE ONLINE STORE',
                    'APPLE STORE R708' = 'APPLE STORE R708',
                    'AT&T SERV CAB CR' = 'AT&T SERV CAB CR',
                    'AUT DE ORIENTE ADO MU' = 'AUT DE ORIENTE ADO MU',
                    'B N M VICAM' = 'B N M VICAM',
                    'BP' = 'BP',
                    'CFE ENERGIA ELEC CIBCV' = 'CFE ENERGIA ELEC CIBCV',
                    'CLIP' = 'CLIP',
                    'DESPEGAR COM' = 'DESPEGAR COM',
                    'E TRAVEL RESERVATION' = 'E TRAVEL RESERVATION',
                    'EKT VTA EN LINEA 3' = 'EKT VTA EN LINEA 3',
                    'ETN TURISTAR INTER CIB' = 'ETN TURISTAR INTER CIB',
                    'EXPEDIA MEXICO' = 'EXPEDIA MEXICO',
                    'EXPEDIA MX018005630600' = 'EXPEDIA MX018005630600',
                    'FOREVER 21 MEXICO' = 'FOREVER 21 MEXICO',
                    'HIPODROMO AGUA CALIENT' = 'HIPODROMO AGUA CALIENT',
                    'HOME DEPOT' = 'HOME DEPOT',
                    'HOTELES COM' = 'HOTELES COM',
                    'INFONAVIT' = 'INFONAVIT',
                    'INTERJET' = 'INTERJET',
                    'IZZI METRO IVR' = 'IZZI METRO IVR',
                    'LINIO COM MX' = 'LINIO COM MX',
                    'LIVERPOOL POR INTERNET' = 'LIVERPOOL POR INTERNET',
                    'MEDICALL PLUS GN CR' = 'MEDICALL PLUS GN CR',
                    'MICROSOFT 7' = 'MICROSOFT 7',
                    'MICROSOFT XBOX OFFICE' = 'MICROSOFT XBOX OFFICE',
                    'NET PAY' = 'NET PAY',
                    'PAGO LUZ CFE MOVILES' = 'PAGO LUZ CFE MOVILES',
                    'PAGO MERCADOPAGO CIB' = 'PAGO MERCADOPAGO CIB',
                    'PAYPAL MEXICO CR' = 'PAYPAL MEXICO CR',
                    'POCKET*BPCKT' = 'POCKET*BPCKT',
                    'PRIVALIA' = 'PRIVALIA',
                    'SAM S CLUB TEPEYAC' = 'SAM S CLUB TEPEYAC',
                    'SAMS CANCUN' = 'SAMS CANCUN',
                    'Sin registro' = 'Sin registro',
                    'TELEVIA PEAJE' = 'TELEVIA PEAJE',
                    'TELMEX' = 'TELMEX',
                    'TICKETMASTER MEXICO' = 'TICKETMASTER MEXICO',
                    'VIVA AEROBUS CIB' = 'VIVA AEROBUS CIB',
                    'VIVRI' = 'VIVRI',
                    'WALMART VENTA EN LIN 1' = 'WALMART VENTA EN LIN 1'))
      
      
    ),
    
    # Panel para outputs ----
    mainPanel(
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Definir servidor ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    t1 <- as.data.frame(tab[tab$nombre == input$nombre,])
    
    ggplot(t1, aes(x= mes,y=importe)) + 
      geom_bar(stat="sum", position="dodge", fill="#002d72") +
      facet_wrap(~ anio) + 
      scale_y_continuous("Importe", labels = comma) +   scale_x_discrete(name="Mes")+
      #aes(x = fct_inorder(mes))+
      theme(legend.position="none")  
    
  })
  
}

shinyApp(ui = ui, server = server)