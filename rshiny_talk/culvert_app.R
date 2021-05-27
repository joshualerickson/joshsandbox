
library(shiny)
library(leaflet)
library(leafpop)
library(leafem)
library(mapview)
library(knitr)
library(rmarkdown)
library(DT)
library(kableExtra)
library(htmltools)
library(scales)
library(httr)
library(leaflet.extras)
library(streamstats)
library(geojsonsf)
library(jsonlite)
library(AOI)
library(sf)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(shinydashboard)

if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

ui <- fluidPage(
    tabsetPanel(id = "tabchart_ss",
      tabPanel("Map", style = "height:92vh;",tags$style(type = 'text/css',
                                                        '#ss_maps {cursor: crosshair !important;}'),
               leafletOutput("ss_maps"),
               dataTableOutput("ss_table")
      ),
      tabPanel("Peak Plot", style = "height:92vh;",
               plotOutput("ss_peak"),
               dataTableOutput("ss_peak_table"),
               box(textInput("wkID", "Workspace ID"), pickerInput("state", "States", choices = datasets::state.abb, multiple = F, selected = "MT"),
                   actionButton("peak", label = "Peak Flow"))),

      tabPanel("Culvert Size", style = "height:92vh",tags$style(type = 'text/css', '#culvert_plot {height: calc(100vh - 250px) !important;}'),
               column(4,box(selectInput("use_reg", "Do you want to use known bankfull widths?", choices = c("Yes", "No")),
                            numericInput("drain_area", "Enter Drainage Area (sq.mi)", 2),
                            numericInput("precip_drain", "Enter Precipitation (in)", 20),
                            numericInput("for_known", "Enter Percent Forested", 95),
                            numericInput("bf_known", "Enter Bankfull Width", 5),
                            numericInput("acw_known", "Enter Active Channel Width", 5),
                            numericInput("geo_known", "Enter Geographic Factor", 1),
                            actionButton("calculate_culvert", label = "Calculate"))),
               column(8,plotlyOutput("culvert_plot")), dataTableOutput("culvert_table")),
      tabPanel("Report", style = "height:92vh",
               textInput("author", "Enter Author."),
               textInput("drain_name", "Enter a Name or ID for the drain point location."),
               radioButtons('format', 'Document format', c('HTML', 'Word'),
                            inline = TRUE),
               downloadButton("report_culvert", "Generate report"))

    )
)

server <- function(input, output, session) {


  ss_list <- reactiveValues()

  # * 5 USGS stream stats ----



  output$ss_maps <- renderLeaflet({


    trexr:::base_map() %>%
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
      setView(lat = 48.91167, lng = -114.90246, zoom = 4)




  })



  observeEvent(input$ss_maps_click, {

    click <- input$ss_maps_click
    clat <- click$lat
    clng <- click$lng
    content <- paste(
      "<b>Lat: ",round(clat, 5),"</b>",
      "<br>",
      "<b>Long: ", round(clng,5), "</b>"    )


    leafletProxy("ss_maps") %>%
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
      addCircles(lng=clng, lat=clat) %>%
      addPopups(lng = clng, lat = clat, content)

    ss_stats <- reactive({

      state <-   geocode_rev(c(clat,clng)) %>% dplyr::select(state)
      state <-  state.abb[grep(paste(state$state), state.name)]

      df1 <- streamstats::delineateWatershed(clng,clat, rcode = state, crs = 4326)

      df_poly <- df1 %>%
        streamstats::writeGeoJSON(., file.path(tempdir(),"ss_tmp.json"))


      df_poly <- df_poly %>% geojsonsf::geojson_sf() %>%
        sf::st_as_sf() %>% dplyr::mutate(ID = state, long = clng, lat = clat)



      wkID <- df1$workspaceID

      incProgress(detail = paste("Computing Basin Characteristics"))

      stats <- streamstats::computeChars(workspaceID = wkID, rcode = state)



      stats <- stats$parameters %>% mutate(ID = state, workspaceID = wkID)

      flow_stats <- stats %>% dplyr::select(ID, code, value) %>%
        pivot_wider(names_from = "code")

      df_poly <- df_poly %>% dplyr::select(Shape_Area, ID, lat, long) %>% left_join(flow_stats, by = "ID")

      ss_list$stats <- stats
      #write_rds(stats, file.path(tempdir(), "stats"), compress = "none")
      #st_write(df_poly, dsn = file.path(tempdir()), layer = "df_poly", driver = "ESRI Shapefile", delete_layer = TRUE)

     list(stats = stats, df_poly = df_poly, state = state, wkID = wkID)

    })




    output$ss_table = renderDataTable({DT::datatable(ss_stats()$stats, options = list(pageLength = 25))})


    withProgress(

      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{


        setProgress(1/3, detail = paste("Delineating Watershed"))

        ss_stats()



      }
    )


    map_leaf <- reactive({

      trexr:::base_map() %>%
        addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
        addCircleMarkers(data = ss_stats()$df_poly, lng = ss_stats()$df_poly$long, lat = ss_stats()$df_poly$lat, layerId = ~paste("Drain Point"), color = "red") %>%
        addPolygons(data = ss_stats()$df_poly,popup = paste0(
          "<p style=line-height:30px;margin:0px;>",
          "<b>Drainage Area: </b>", paste(ss_stats()$df_poly$CONTDA, " sq.mi"),
          "<br>","<b>Precipitation: </b>",ss_stats()$df_poly$PRECIP,
          "<br>","<b>Forest (per): </b>",ss_stats()$df_poly$FOREST,
          "<br>","<b>Temperature: </b>",ss_stats()$df_poly$TEMP,
          "<br>","<b>Max Elevation: </b>",ss_stats()$df_poly$ELEVMAX,
          "<br>","<b>Slope abv 30% (per): </b>",ss_stats()$df_poly$SLOP30_30M,
          "<br>","<b>Slope abv 50% (per): </b>",ss_stats()$df_poly$SLOP50_30M), group = "poly") %>% addLayersControl(overlayGroups = "poly")})

    output$ss_maps <- renderLeaflet({

      map_leaf()
    })

    withProgress(

      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{


        setProgress(1/2, detail = paste("Generating Map Image"))


        mapshot(x = map_leaf()
                , file = file.path(tempdir(), "customleaf.png")
        )




      }
    )




  })

  #   * *  5.1 Plot ----

  cul <- reactiveValues()
  observeEvent(input$peak, {

    wkID <- input$wkID

    state <- input$state





    ss_peak_ <- reactive({
      base_url <- paste0(
        "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
        wkID,
        "&includeflowtypes=true"
      )


      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "peak_tmp.json"),overwrite = TRUE))


      peak <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))

      peak <- (peak$RegressionRegions[[1]])[[6]][[1]] %>%  mutate(ReturnInterval = c(1.5,2,2.33,5,10,25,50,100,200,500), code = fct_reorder(code, ReturnInterval))

      # param <- (peak$RegressionRegions[[1]])[[5]][[1]]
      #
      # param <- param %>% select(Name, Code, Value)

      cul$culvert_usgs <- peak
      # write_rds(peak, path = file.path(tempdir(), "culvert_usgs"), compress = "none")

      list(peak = peak)
    })

    withProgress(

      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{


        setProgress(1/2, detail = paste("Calculating Peak Flow"))

        ss_peak_()
      })

    output$ss_peak <- renderPlot({

      if (length(ss_peak_()$peak) > 7) {

        ggplot(ss_peak_()$peak , aes(ReturnInterval, Value)) + geom_point() +
          geom_line() + geom_ribbon(aes(ymin=IntervalBounds$Lower, ymax=IntervalBounds$Upper), linetype=1, alpha=0.3)+
          theme_light() +
          labs(x = "Return Interval", y = "Discharge (cfs)", title = "USGS RRE")+theme(text = element_text(size=20))
      } else {

        ggplot(ss_peak_()$peak , aes(ReturnInterval, Value)) + geom_point(size = 2) +
          geom_line(size = 1.5) + # geom_ribbon(aes(ymin=IntervalBounds$Lower, ymax=IntervalBounds$Upper), linetype=1, alpha=0.3)+
          theme_light() +
          annotate("text", x = 200, y = max(ss_peak_()$peak$Value*0.35), label = "One or more of the parameters \n is outside the suggested range. \n Estimates were extrapolated with unknown errors.", size = 9, color = "red") +
          labs(x = "Return Interval", y = "Discharge (cfs)", title = "USGS RRE")+theme(text = element_text(size=20))

      }
    })

    output$ss_peak_table = renderDataTable({

      if (length(ss_peak_()$peak) > 7) {
        DT::datatable(ss_peak_()$peak %>% dplyr::select(Name,ReturnInterval,Description,IntervalBounds, Value), options = list(pageLength = 25))

      } else {


        DT::datatable(ss_peak_()$peak %>% dplyr::select(Name,ReturnInterval,Description, Value), options = list(pageLength = 25))

      }

    })
  })




  # ** 5.2 Culvert sizing ----

  observeEvent(input$peak,{

    precipUpdate <- ss_list$stats %>%
      filter(str_detect(description, "Mean Annual Precip"))

    daUpdate <- ss_list$stats %>%
      filter(str_detect(description, "Area that contributes flow to a point on a stream"))


    forUpdate <- ss_list$stats %>%
      filter(str_detect(description, "Percentage of area covered by forest"))

    # This will change the value of input$inText, based on x
    updateTextInput(session, "precip_drain", value = precipUpdate$value)
    updateTextInput(session, "drain_area", value = daUpdate$value)
    updateTextInput(session, "for_known", value = forUpdate$value)
  })



  observeEvent(input$calculate_culvert, {
    culvert <- reactive({


      drain_area <- isolate(input$drain_area)

      precip_drain <- isolate(input$precip_drain)

      bf_known <- isolate(input$bf_known)

      bd_known <- isolate(input$bd_known)

      acw_known <- isolate(input$acw_known)

      geo_known <- isolate(input$geo_known)

      for_known <- isolate(input$for_known)



      bf_regres <- if(precip_drain < 30) {
        3.99*drain_area^0.441
      } else if (precip_drain > 45) {

        7.7*drain_area^0.441
      } else {

        6.04*drain_area^0.441
      }




      if (isolate(input$use_reg) == "Yes") {
        Omang_parrett_hull_flows <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                               basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                                              0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                                              0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                                              0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                                               bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_known^1.14),
                                                                  0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_known^1.02),
                                                                  0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_known^1.01),
                                                                  0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_known^1)),
                                               source = c("Omang, Parrett and Hull"))

        parrett_and_johnson <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                           basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                                          8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                                          13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                                          18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                                           bankfull_width = c(0.281*(bf_known^1.98),
                                                              1.75*(bf_known^1.72),
                                                              2.34*(bf_known^1.69),
                                                              2.99*(bf_known^1.66)),
                                           active_width = c(1.11*(acw_known^1.74),
                                                            5.81*(acw_known^1.51),
                                                            7.61*(acw_known^1.48),
                                                            9.57*(acw_known^1.45)),
                                           source = c("Parrett & Johnson"))
      } else {

        Omang_parrett_hull_flows <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                               basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                                              0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                                              0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                                              0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                                               bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_regres^1.14),
                                                                  0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_regres^1.02),
                                                                  0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_regres^1.01),
                                                                  0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_regres^1)),
                                               source = c("Omang, Parrett and Hull"))

        parrett_and_johnson <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                           basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                                          8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                                          13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                                          18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                                           bankfull_width = c(0.281*(bf_regres^1.98),
                                                              1.75*(bf_regres^1.72),
                                                              2.34*(bf_regres^1.69),
                                                              2.99*(bf_regres^1.66)),
                                           active_width = c("No Calculation"),
                                           source = c("Parrett & Johnson"))
      }

      #read in some files for reporting and finishing the culvert estimations

      culvert_usgs <- cul$culvert_usgs

      stats_usgs_cul <- ss_list$stats

      #customleaf <- file.path(tempdir(), "customleaf.png")




      if (is.null(culvert_usgs)) {

        culvert_usgs <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                   basin_char = rep(0))

      } else {




        culvert_size <- function(x) {
          ifelse(x < 11, "(24 in)",
                 ifelse(x >= 11 & x < 30,"(36 in)",
                        ifelse(x >= 30 & x < 65,"(48 in)",
                               ifelse(x >= 65 & x <110,"(60 in)",
                                      ifelse(x >= 110 & x < 180,"(72 in)",
                                             ifelse(x >= 180 & x < 290,"(84 in)",
                                                    ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)")))))))}


        culvert_usgs <- culvert_usgs %>% dplyr::select(basin_char = Value, ReturnInterval) %>%
          mutate(source = "USGS Regression") %>% filter(ReturnInterval %in% c(2, 25, 50, 100))}

      together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson, culvert_usgs)

      together <- together %>% mutate(RI = parse_number(ReturnInterval))

      if (isolate(input$use_reg) == "No") {

        together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
          mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

      } else {
        together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width", "active_width"), names_to = "Method") %>%
          mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))}

      # write_rds(together_long, file.path(tempdir(), "together_long"))

      ss_list$together_long <- together_long

      list(together_long = together_long)
    })


    #observeEvent(input$calculate_culvert, {


    output$culvert_plot <- renderPlotly({

      print(ggplotly(culvert()$together_long %>% ggplot(aes(RI, value, color = source)) +
        geom_point() +
        geom_line() +
        facet_wrap(~Method) +
        theme_bw()))


    })

    output$culvert_table <- renderDataTable(DT::datatable(culvert()$together_long %>% filter(RI %in% c(50,100)) %>%
                                                            dplyr::select(ReturnInterval,Source = source, Method, value, Size), options = list(pageLength = 25)))

  })

  final_cul <- reactive({

    customleaf <- file.path(tempdir(), "customleaf.png")

    stats_usgs_cul <- ss_list$stats

    together_long <- ss_list$together_long

    drain_name <- input$drain_name

    list(together_long = together_long,
         stats_usgs_cul = stats_usgs_cul, drain_name = drain_name,
         customleaf = customleaf)
  })

  output$report_culvert <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('rshiny_talk/report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)


      out <- render('report.Rmd',
                    params = list(set_author = input$author),

                    switch(
                      input$format,
                      HTML = html_document(), Word = word_document()
                    ))
      file.rename(out, file)
    }
  )


}

shinyApp(ui, server)
