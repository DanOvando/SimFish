# ui <- shinyUI(fluidPage(
#   titlePanel("My Shiny App"),
#   sidebarLayout(
#     sidebarPanel(
#       h2("Installation"),
#       p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
#       code('install.packages("shiny")'),
#       br(),
#       br(),
#       br(),
#       br(),
#       img(src = "bigorb.png", height = 72, width = 72),
#       "shiny is a product of ",
#       span("RStudio", style = "color:blue")
#     ),
#     mainPanel(
#       h1("Introducing Shiny"),
#       p("Shiny is a new package from RStudio that makes it ",
#         em("incredibly easy"),
#         " to build interactive web applications with R."),
#       br(),
#       p("For an introduction and live examples, visit the ",
#         a("Shiny homepage.",
#           href = "http://www.rstudio.com/shiny")),
#       br(),
#       h2("Features"),
#       p("* Build useful web applications with only a few lines of code—no JavaScript required."),
#       p("* Shiny applications are automatically “live” in the same way that ",
#         strong("spreadsheets"),
#         " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
#     )
#
#
# server <- shinyServer(function(input, output) {
#
#     # Expression that generates a histogram. The expression is
#     # wrapped in a call to renderPlot to indicate that:
#     #
#     #  1) It is "reactive" and therefore should
#     #     re-execute automatically when inputs change
#     #  2) Its output type is a plot
#
#     output$distPlot <- renderPlot({
#       x    <- faithful[, 2]  # Old Faithful Geyser data
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'skyblue', border = 'white')
#     })
#   })
# )))
#
# shinyApp(ui = ui, server = server)
