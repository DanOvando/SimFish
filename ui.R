shinyUI(fluidPage(theme = 'new_theme.css',

                  # Application title
                  titlePanel(fluidRow(column(width =2,  h1("SimFish")), column(width = 2, offset = 8,
                                                                               img(src = "snap_logo.png", width = 85, height = 65)))),
                  h5('Basado en Codigo hecho por Cody Szuwalski'),
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                    sidebarPanel(
                      #       h6('Select number of years for simulation'),
                      downloadButton('downloadModel', 'Retirar Datos', class="dlButton"),
                      #       h6('Select species to use'),
                      sliderInput("Years",
                                  "Años de Simulacíon",
                                  min = 2,
                                  max = 50,
                                  value = 30),
                      selectInput(inputId = 'species',label = 'Especia',
                                  choices = c('Fine Flounder', "Lorna Drum",'Chita'), selected = 'Chita'),
                      selectInput(inputId = 'fleet_select',label = 'Model Pesquero',
                                  choices = c('Ninguno' = 'None', "Libre Accesso" = 'Open Access'), selected = 'Open Access'),
                      selectInput(inputId = 'select_select',label = 'Selectividad',
                                  choices = c('Logistico' =  'logistic', 'Espacio' = 'slot'), selected = 'logistic'),
                      sliderInput('fish_select','Selectividad Pesquero (% de Tamaño Maximo)', min = 0, max = 100, value = c(39,40), step = 0.5, pre = '%'),
                      sliderInput('f_select','Mortalidad Pesquero', min = 0, max = 2, value = c(1), step = 0.025),
                      sliderInput('phi_select','Flexibilidad de Flota', min = 0, max = 0.25, value = c(.1), step = 0.001),
                      sliderInput('tech_select','Rato de Technologia', min = -10, max = 10, value = c(0), step = 0.5, pre = '%'),
                      sliderInput('cr_ratio_select','Costos', min = 0, max = 1, value = c(0.6), step = 0.025),
                      sliderInput('sigmaR', 'Variabilidad de Reclutamiento',
                                  min = 0, max = 2, value = 0.05, step = 0.05),
                      sliderInput('recruit_ac', 'Autocorrelation de Reclutamiento',
                                  min = 0, max = 1, value = 0, step = 0.025),
                      sliderInput('fm_select', 'Ratio de F y M',
                                  min = 0.2, max = 1, value = 0.4, step = 0.025),
                      sliderInput('surv_select','Selectividad de Muestras (% de Tamaño Maximo)', min = 0,
                                  max = 100, value = c(10,10), step = 0.5, pre = '%'),
                      sliderInput('GrowthCV_select', 'Variación en Tamaño en Edad',
                                  min = 0.001, max = .2, value = 0.025, step = 0.025)
                      #                       selectInput(inputId = 'x_compare',label = 'Comparación 1',
                      #                                   choices = c('Captura' = 'total_catch','CPUE' = 'mean_cpue',
                      #                                               'Biomasa' = 'total_biomass','Froese' = 'p_obj'),selected = 'total_biomass'),
                      #                       selectInput(inputId = 'y_compare',label = 'Comparación 2',
                      #                                   choices = c('Captura' = 'total_catch','CPUE' = 'mean_cpue',
                      #                                               'Biomasa' = 'total_biomass','Froese' = 'p_obj'),selected = 'total_biomass')
                    ), #Close sidebar Panel
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Puntos de Referencia", plotOutput('reference_plot')),
                                  tabPanel("Historia de Vida", plotOutput('life_plot'), plotOutput('recruitment_plot')),
                                  tabPanel("Indicadores Froese",plotOutput("froese_trend")),
                                  tabPanel("CPUE",plotOutput("cpue_trend")),
                                  tabPanel("Captura",plotOutput("catch_trend")),
                                  tabPanel("Biomassa",plotOutput("biomass_trend")),
                                  tabPanel("Muestras de Tamaño",plotOutput("length_freq")),
                                  tabPanel("SPR", plotOutput('spr_plot')),
                                  tabPanel("Reclutamiento", plotOutput('recruits_plot')),
                                  tabPanel("CPUE v Biomasa", plotOutput('bvcatch_plot')),
                                  tabPanel("Catch v Biomasa", plotOutput('bvcpue_plot')),
                                  tabPanel("Froese v Biomasa", plotOutput('bvfroese_plot')),
                                  tabPanel("CPUE y Biomasa", plotOutput('b_v_cpue_plot')),
                                  tabPanel("Indicadores de Prince", plotOutput('prince_plot')),
                                  tabPanel("Curva de Captura", plotOutput('catch_curve_plot')),
                                  tabPanel("Curva de Captura Tendencia", plotOutput('catch_curve_trend_plot')),
                                  tabPanel("Curva de Captura Relativo Tendencia", plotOutput('catch_curve_fvm_trend_plot'))





                                  #                   condition("Comparación", plotOutput('comp_plot'))


                      ) #close tabsetPanel
                    ) # close mainPanel
                  ) #close sidebarPanel
) #close fluidpage
) #close ShinyUI

