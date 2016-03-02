shinyUI(fluidPage(theme = 'new_theme.css',

  # Application title
  titlePanel(fluidRow(column(width =2,  h1("SimFish")), column(width = 2, offset = 8,
  img(src = "snap_logo.png", width = 85, height = 65)))),
  h5('This is a fishery simulater based on  the General MSE code by Cody Szuwalski'),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
#       h6('Select number of years for simulation'),
      downloadButton('downloadModel', 'Download Data', class="dlButton"),
      h6('Select species to use'),
      selectInput(inputId = 'species',label = 'Species',
                  choices = c('Fine Flounder', "Lorna Drum",'Chita'), selected = 'Chita'),
      selectInput(inputId = 'fleet_select',label = 'Fleet Model',
                  choices = c('None', "Open Access"), selected = 'Open Access'),
      selectInput(inputId = 'select_select',label = 'Selectivity Form',
                  choices = c('Logistic' =  'logistic', 'Slot' = 'slot'), selected = 'logistic'),
      sliderInput("Years",
                  "Number of Years of Simulation:",
                  min = 2,
                  max = 50,
                  value = 30),
      br(),
      sliderInput('sigmaR', 'Recruitment Variability',
                  min = 0, max = 2, value = 0.05, step = 0.05),
      br(),
      sliderInput('recruit_ac', 'Recruitment Autocorrelation',
                  min = 0, max = 1, value = 0, step = 0.025),
      br(),
      sliderInput('fish_select','Fishery Selectivity (% of Max Length)', min = 0, max = 100, value = c(39,40), step = 0.5, pre = '%'),
      br(),
      sliderInput('f_select','Fishing Mortality Rate', min = 0, max = 2, value = c(1), step = 0.025),
      br(),
      sliderInput('phi_select','Fleet Response', min = 0, max = 0.25, value = c(.1), step = 0.001),
      sliderInput('tech_select','Technology Rate', min = -10, max = 10, value = c(0), step = 0.5, pre = '%'),

      br(),
      sliderInput('cr_ratio_select','Cost', min = 0, max = 1, value = c(0.6), step = 0.025),
      br(),
      sliderInput('surv_select','Survey Selectivity (% of Max Length)', min = 0,
                  max = 100, value = c(10,10), step = 0.5, pre = '%')),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Reference Points", plotOutput('reference_plot')),
                  tabPanel("Life History", plotOutput('life_plot'), plotOutput('recruitment_plot')),
                  tabPanel("Froese Indicators",plotOutput("froese_trend")),
                  tabPanel("CPUE Trends",plotOutput("cpue_trend")),
                  tabPanel("Catch Trends",plotOutput("catch_trend")),
                  tabPanel("Biomass Trends",plotOutput("biomass_trend")),
                  tabPanel("Length Frequency",plotOutput("length_freq")),
                  tabPanel("Recruitment", plotOutput('recruits_plot')),
                  tabPanel("Catch Curve", plotOutput('catch_curve_plot'))#,
#                   tabPanel("Catch Curve Summary", tableOutput('catch_curve_plot'))



      ) #close tabsetPanel
    ) # close mainPanel
  ) #close sidebarPanel
) #close fluidpage
) #close ShinyUI

