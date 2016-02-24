shinyUI(fluidPage(

  # Application title
  titlePanel("SNAP Peru Simulator"),
  h5('This is a fishery simulater based on code by Cody Szuwalski'),

  # Sidebar with a slider input for number of bins
  fluidRow( column(width = 4,
                   wellPanel(
                     h6('Select number of years for simulation'),
                     sliderInput("Years",
                                 "Number of Years of Simulation:",
                                 min = 2,
                                 max = 50,
                                 value = 30),
                     br(),
                     h6('Select species to use'),
                     selectInput(inputId = 'species',label = 'Especia',
                                 choices = c('Chita', "Lorna Drum",'Sea Cucumber'), selected = 'Chita'))

  ),

  column(4,
         plotOutput("biomass_trend")),
  column(4,
         plotOutput("length_freq"))
  )
))