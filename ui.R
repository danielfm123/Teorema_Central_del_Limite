library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Teorema Central del Limite"),
  
  sidebarPanel(
    selectInput("distribucion","distribucion",
                c(Normal = "norm",
                  Exponencial = "exp",
                  Student = "t",
                  Cauchy = "cauchy",
                  Chi2 = "chisq",
                  Binomial = "binom",
                  Sample = "sample"
                  )
                ),
    h4("parametros distribucion"),
    uiOutput("parametros"),
    conditionalPanel("input.pestana == 'teorema central'",
                     actionButton("reejecutar","reejecutar"))
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("distribucion",
               plotOutput("grafico_dist"),
               sliderInput("rango","rango grafico",-100,100,c(-2,2),dragRange = T,width = "100%",step = .01)
      ),
      tabPanel("teorema central",
               fluidRow(column(6,sliderInput("tamano_muestra","tamano muestra",1,1000,10,1)),
                        column(6,sliderInput("experimentos","exerimentos",1,1000,100,1))
                        ),
               fluidRow(column(3,
                               h3("Indicadores Muestras"),
                               tableOutput("tabla_indicadores")),
                        column(9,
                               h3("Histograma Promedios"),
                               plotOutput("grafico_histograma"))
               )
      ),id = "pestana"
    )
  )
))

