library(shiny)
library(leaflet)
library(wildlandhydRo)
library(evd)
library(extRemes)
library(lfstat)

usgs_sites <- dataRetrieval::whatNWISdata(stateCd = 'MT', parameterCd='00060')
usgs_sites <- usgs_sites %>% filter(count_nu >= 365*10) %>%
  group_by(station_nm) %>% slice_max(count_nu) %>% ungroup()

ui <- fluidPage(
  tabsetPanel(id='tabs',
              tabPanel('mapping',
  leafletOutput('leaf_map'),
  fluidRow(plotly::plotlyOutput('peak_plot'))),
  tabPanel('Flood Freq',
              fluidRow(plotly::plotlyOutput('floodfreq'))),
  tabPanel('GOF',
           plotOutput('dist_g'), fluidRow(plotOutput('test_g'),
           pickerInput('distType', 'Plot Type', choices = c('PP', 'Q-Q', "Density", 'CDF'), selected = 'Density',
                       options = list(`actions-box` = TRUE))))
  ))

server <- function(input, output, session) {


  output$leaf_map <- leaflet::renderLeaflet({
    labs <- as.list(usgs_sites$station_nm)
    leaflet() %>% addTiles() %>%
      addCircles(data = usgs_sites, lng = usgs_sites$dec_long_va,
                       lat = usgs_sites$dec_lat_va,weight = 10,
                       fillOpacity = 0.9,
                       label = lapply(labs, HTML),
                       layerId = ~usgs_sites$station_nm)

  })

  usgs_ggplot_data <- reactive({
    site <- input$leaf_map_shape_click$id
    usgs_sites %>% filter(station_nm %in% site)
    })
      output$peak_plot <- plotly::renderPlotly({
        req(input$leaf_map_shape_click$id)
    withProgress(message = 'loading data', value = 1/2,{
      ply <- peak_plots(usgs_ggplot_data()$site_no)
      print(plotly::ggplotly(ply))
      })
  })


      output$floodfreq = plotly::renderPlotly({
        shiny::validate(
          need(input$leaf_map_shape_click$id, 'Please select at least one site'))
        withProgress(message = 'loading data', value = 1/2,{
          freq <- reactive({
        peak <- usgs_ggplot_data()$site_no %>%
          dataRetrieval::readNWISpeak(.)

       peak <- peak %>% filter(!is.na(peak_va)) %>% wildlandhydRo::batch_frequency(peak_va)


        })

        print(plotly::ggplotly(ggplot(freq(), aes(x= ReturnInterval, y = Value, color = Distribution )) +
          geom_point(alpha = 0.8, size = 2.5) + geom_line() +
          #geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) +
          expand_limits(y = 0) +
          scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + scale_x_log10() + theme(text = element_text(size=20)) +
          labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
          theme_light()+theme(text = element_text(size=15))))
      })
      })



observeEvent(input$leaf_map_shape_click$id, {
 dist_react <- reactive({
                peak <- dataRetrieval::readNWISpeak(usgs_ggplot_data()$site_no)
                peak %>% filter(!is.na(peak_va)) %>% wildlandhydRo::batch_distribution(peak_va)
              })
 print(dist_react())
        output$dist_g <- renderPlot({

          if(input$distType == "Density") {

            wildlandhydRo::plot_densDist(dist_react()) + labs(title = paste('Density Plot ', usgs_ggplot_data()$station_nm))

          } else if (input$distType == "Q-Q"){

            wildlandhydRo::plot_qqDist(dist_react()) + labs(title = paste('Density Plot ', usgs_ggplot_data()$station_nm))
          } else if (input$distType == "PP"){

            wildlandhydRo::plot_ppDist(dist_react()) + labs(title = paste('Density Plot ', usgs_ggplot_data()$station_nm))

          }else if (input$distType == "CDF"){

            wildlandhydRo::plot_cdfDist(dist_react()) + labs(title = paste('Density Plot ', usgs_ggplot_data()$station_nm))
          }

        })

        output$test_g <- renderPlot({
          dist_rep <- reactive({wildlandhydRo::reportDist(dist_react())})
          wildlandhydRo::plot_reportDist(dist_rep())
        })
})

}


shinyApp(ui, server)
