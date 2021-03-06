set.seed(123)
library(shiny)
# library(plyr)
library(tidyverse)
library(ggthemes)
library(roxygen2)
sapply(list.files(
  pattern = "[.]R$",
  path = "simfish/R/",
  full.names = TRUE
),
source)
# devtools::load_all('simfish')

shinyServer(function(input, output) {
  simmedpop <- reactive({
    if (input$species == 'Lorna Drum')
    {
      MaxAge <- 35

      vbk <- 0.195

      linf <- 550

      t0 <- -0.808

      mat50 <- 2

      mat95 <- 3

      alpha <- 0.00013

      beta <- 2.63

      Rzero <- 3e6

      MaxMove <- 2

      Move50 <- 3

      Move95 <- 4

      steepness <- 0.8

      sigmaR <- 0.1

      NatM <- 0.52


    }
    if (input$species == 'Fine Flounder')
    {
      MaxAge <- 35

      vbk <- 0.196

      linf <- 1011.7

      t0 <- -0.808

      mat50 <- 4

      mat95 <- 5

      alpha <- 0.00013

      beta <- 2.63

      Rzero <- 3e6

      MaxMove <- 5

      Move50 <- 4

      Move95 <- 5

      steepness <- 0.8

      sigmaR <- 0.1

      NatM <- 0.4


    }
    if (input$species == 'Chita')
    {
      MaxAge <- 35

      vbk <- 0.146

      linf <- 614.4

      t0 <- -0.894

      mat50 <- 2

      mat95 <- 3

      alpha <- 0.00013

      beta <- 3.14

      Rzero <- 3e6

      MaxMove <- 5

      Move50 <- 4

      Move95 <- 5

      steepness <- 0.7

      sigmaR <- 0.1

      NatM <- 0.4


    }

    out <- simfish(
      SimYear = input$Years,
      VonKn = vbk,
      VonKs = vbk,
      LinfN = linf,
      LinfS = linf,
      t0n = t0,
      t0s = t0,
      mat50n = mat50,
      mat50s = mat50,
      mat95n = mat95,
      mat95s = mat95,
      alphaN = alpha,
      betaN = beta,
      alphaS = alpha,
      betaS = beta,
      MaxMovingN = MaxMove,
      MaxMovingS = MaxMove,
      Move50n = Move50,
      Move50s = Move50,
      Move95n = Move95,
      Move95s = Move95,
      steepnessN = steepness,
      steepnessS = steepness,
      sigmaRn = input$sigmaR,
      sigmaRs = input$sigmaR,
      RzeroN = Rzero,
      RzeroS = Rzero,
      MaxAge = MaxAge,
      NatMn = NatM,
      NatMs = NatM,
      sel50n = input$fish_select[1] / 100 * linf,
      sel50s = input$fish_select[1] / 100 * linf,
      sel95n = input$fish_select[2] / 100 * linf,
      sel95s = input$fish_select[2] / 100 * linf,
      surv50n = input$surv_select[1] / 100 * linf,
      surv50s = input$surv_select[1] / 100 * linf,
      surv95n = input$surv_select[2] / 100 * linf,
      surv95s = input$surv_select[2] / 100 * linf,
      HistoricalF = input$f_select,
      recruit_ac = input$recruit_ac,
      CalcFMSY = 1,
      phi = input$phi_select,
      cr_ratio = input$cr_ratio_select,
      fleetmodel = input$fleet_select,
      select_model = input$select_select,
      tech_rate = input$tech_select / 100,
      fm_ratio = input$fm_select,
      GrowthCV = input$GrowthCV_select
    )


    #     out <- simfish(SimYear = input$Years)

    return(out)
  })


  output$biomass_trend <- renderPlot({
    #     out <- simfish(SimYear = input$Years)
    simmedpop()$plots$biomass_plot
    #     out$plots$biomass_plot
  })

  output$length_freq <- renderPlot({
    simmedpop()$plots$length_plot
  })

  output$life_plot <- renderPlot({
    simmedpop()$plots$life_plot
  })

  output$recruitment_plot <- renderPlot({
    simmedpop()$plots$recruitment_plot
  })

  output$froese_trend <- renderPlot({
    simmedpop()$plots$cope_punt_plot
  })

  output$cpue_trend <- renderPlot({
    simmedpop()$plots$cpue_plot
  })

  output$reference_plot <- renderPlot({
    simmedpop()$plots$reference_plot
  })


  output$catch_trend <- renderPlot({
    simmedpop()$plots$catch_plot
  })

  output$recruits_plot <- renderPlot({
    simmedpop()$plots$recruits_plot
  })

  output$catch_curve_plot <- renderPlot({
    simmedpop()$plots$catch_curve_plot
  })

  output$catch_curve_trend_plot <- renderPlot({
    simmedpop()$plots$catch_curve_trend_plot
  })

  output$spr_plot <- renderPlot({
    simmedpop()$plots$spr_plot
  })


  output$bvcatch_plot <- renderPlot({
    simmedpop()$plots$bvcatch_plot

    #     ggplot(dat,aes(input$x_compare, input$y_compare, fill = year)) +
    #       geom_point() +
    #       theme_economist() +
    #       theme(text = element_text(size = 18))
  })

  output$bvcpue_plot <- renderPlot({
    simmedpop()$plots$bvcpue_plot

    #     ggplot(dat,aes(input$x_compare, input$y_compare, fill = year)) +
    #       geom_point() +
    #       theme_economist() +
    #       theme(text = element_text(size = 18))
  })

  output$bvfroese_plot <- renderPlot({
    simmedpop()$plots$bvfroese_plot
  })

  output$b_v_cpue_plot <- renderPlot({
    simmedpop()$plots$b_v_cpue_v_time_plot

  })

  output$catch_curve_fvm_trend_plot <- renderPlot({
    simmedpop()$plots$catch_curve_fvm_trend_plot

  })

  output$prince_plot <- renderPlot({
    simmedpop()$plots$cpue_by_group_plot

  })



  #   output$bio_table <- renderTable({
  #     simmedpop()$biomass_data
  #   })

  #   output$catchcurve_table <- renderTable({
  #     simmedpop()$catch_curve_fits$Output
  #   })

  storesim <- reactiveValues()
  observe({
    if (!is.null(simmedpop()))
      isolate(storedsim <<- simmedpop())
  })

  # Download Random Forest Model
  output$downloadModel <- downloadHandler(
    filename <- function() {
      paste("SimFish.RData")
    },

    content = function(file) {
      save(storedsim, file = file)
    }
  )



})
