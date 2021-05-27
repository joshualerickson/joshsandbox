library(shiny)
library(leaflet)
library(wildlandhydRo)
library(evd)
library(dataRetrieval)
library(smwrBase)
library(fitdistrplus)
library(extRemes)
library(lfstat)
library(ggtext)
library(tidyverse)
library(plotly)
library(sf)
library(knitr)
library(rmarkdown)

usgs_sites <- dataRetrieval::whatNWISdata(stateCd = 'MT', parameterCd='00060')

usgs_sites <- usgs_sites %>% filter(count_nu >= 365*10) %>%
  group_by(station_nm) %>% slice_max(count_nu) %>% ungroup()
peak_plots <- function(siteID){


  peak <- dataRetrieval::readNWISpeak(siteID)

  # graph data
  ggplot(peak, aes(peak_dt, peak_va)) +
    geom_point() +
    geom_line() +
    labs(title = paste0("Peak Flow: ",attr(peak, which = 'siteInfo')$station_nm),
         subtitle = paste0("Drainage Area: ", attr(peak, which = 'siteInfo')$drain_area_va, ' sq.miles'),
         x = 'Date',
         y = "Q (ft<sup>3</sup>/sec)") +
    theme_bw() + theme(axis.title.y = element_markdown())

}
ui <- fluidPage(
  tabsetPanel(id='tabs',
              tabPanel('mapping',
  leafletOutput('leaf_map'),
  fluidRow(plotly::plotlyOutput('peak_plot'))),
  tabPanel('Flood Freq',
              fluidRow(plotly::plotlyOutput('floodfreq'))),
  tabPanel('GOF',
           plotOutput('dist_g'), fluidRow(plotOutput('test_g'),
           selectInput('distType', 'Plot Type', choices = c('PP', 'Q-Q', "Density", 'CDF'), selected = 'Density'))),
  tabPanel("Report", style = "height:92vh",
           textInput("author", "Enter Author."),
           selectInput('user_dist', "Enter Distribution", choices = c("Gumbel","LogPearson","GEV","Normal","Lognormal","Pearson","Weibull"), multiple = TRUE),
           radioButtons('format', 'Document format', c('HTML', 'Word'),
                        inline = TRUE),
           downloadButton("report_floods", "Generate report"))
  ))

server <- function(input, output, session) {

  usgs_sites_reac <- reactiveValues()

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
      usgs_data <- reactive(dataRetrieval::readNWISsite(usgs_ggplot_data()$site_no))

      output$floodfreq = plotly::renderPlotly({
        freq <- reactive({
  peak <- usgs_ggplot_data()$site_no %>%
    dataRetrieval::readNWISpeak(.)

  peak <- peak %>% filter(!is.na(peak_va)) %>% wildlandhydRo::batch_frequency(peak_va)


})
        shiny::validate(
          need(input$leaf_map_shape_click$id, 'Please select at least one site'))
        withProgress(message = 'loading data', value = 1/2,{

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

            wildlandhydRo::plot_qqDist(dist_react()) + labs(title = paste('Q-Q Plot ', usgs_ggplot_data()$station_nm))
          } else if (input$distType == "PP"){

            wildlandhydRo::plot_ppDist(dist_react()) + labs(title = paste('PP Plot ', usgs_ggplot_data()$station_nm))

          }else if (input$distType == "CDF"){

            wildlandhydRo::plot_cdfDist(dist_react()) + labs(title = paste('CDF Plot ', usgs_ggplot_data()$station_nm))
          }

        })

        output$test_g <- renderPlot({
          dist_rep <- reactive({wildlandhydRo::reportDist(dist_react())})
          wildlandhydRo::plot_reportDist(dist_rep())
        })
})
freq2 <- reactive({
  peak <- usgs_ggplot_data()$site_no %>%
    dataRetrieval::readNWISpeak(.)

  peak <- peak %>% filter(!is.na(peak_va)) %>% wildlandhydRo::batch_frequency(peak_va)


})
dist_user <- reactive(input$user_dist)

output$report_floods <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, HTML = 'html', Word = 'docx'
    ))
  },

  content = function(file) {
    src <- normalizePath('flood_freq_rep.Rmd')

    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'flood_freq_rep.Rmd', overwrite = TRUE)

withProgress(message = 'rendering report', value = 1/2,{
    out <- render('flood_freq_rep.Rmd',
                  params = list(set_author = input$author),

                  switch(
                    input$format,
                    HTML = html_document(), Word = word_document()
                  ))
    })
    file.rename(out, file)
  }

)

}


shinyApp(ui, server)
