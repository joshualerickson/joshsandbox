library(shiny)
library(tableHTML)
ui <- fluidPage(
    column(8,plotOutput('plot_shear')),
    radioButtons('num', label = 'Select Widget for Input', choices = c('Numbers', 'Sliders'),
                 selected = 'Sliders', inline = T),
            conditionalPanel("input.num === 'Sliders'", column(4,
                                                           sliderInput("shields_50", "Shields D50:",
                                                                       min = 0.039, max = 0.054,
                                                                       value = .050, step = 0.001),
                                                           sliderInput("d_84", "Partical of Interest, P_i:",
                                                                       min = 1, max = 256,
                                                                       value = 120, step = 1),
                                                           sliderInput("d_50", "D50:",
                                                                       min = 1, max = 256,
                                                                       value = 52, step = 1),
                                                           sliderInput("hyd_rad", "Hydraulic Radius:",
                                                                       min = 0, max = 5,
                                                                       value = 1, step = 0.01),
                                                           sliderInput("slope", "Water Surface Slope:",
                                                                       min = 0, max = 0.2,
                                                                       value = .0125, step = .0001))),
            conditionalPanel("input.num === 'Numbers'", column(4,
                                                               numericInput("shields_50_n", "Shields D50:",
                                                                           min = 0.039, max = 0.054,
                                                                           value = .050),
                                                               numericInput("d_84_n", "Partical of Interest, P_i:",
                                                                           min = 1, max = 256,
                                                                           value = 120),
                                                               numericInput("d_50_n", "D50:",
                                                                           min = 1, max = 256,
                                                                           value = 52),
                                                               numericInput("hyd_rad_n", "Hydraulic Radius:",
                                                                           min = 0, max = 5,
                                                                           value = 1),
                                                               numericInput("slope_n", "Water Surface Slope:",
                                                                           min = 0, max = 0.2,
                                                                           value = .0125))),
    fluidRow(tableOutput('table')), fluidRow(column(width = 2, align = "center",
                                                    img(src="D:/R_folder/GIT/random/joshsandbox/rshiny_talk/rshiny_talk_files/images/www/shields.png", width=100)))

)

server <- function(input, output, session) {

df <- reactiveValues()
  observe({    if(input$num == 'Sliders'){

    x <- reactive({(102.6*input$shields_50*(input$d_84*0.00328084)^0.3)*(input$d_50*0.00328084)^0.7})
    y <- reactive({62.4*input$hyd_rad*input$slope})

  } else if (input$num == 'Numbers'){

    x <- reactive({(102.6*input$shields_50_n*(input$d_84_n*0.00328084)^0.3)*(input$d_50_n*0.00328084)^0.7})
    y <- reactive({62.4*input$hyd_rad_n*input$slope_n})

  }

    df$reac <- reactive({cbind.data.frame(SS = x(),BD = y())})

    })

  output$plot_shear <- renderPlot({


    ggplot(data = df$reac(), aes(x = SS, y = BD)) + geom_point(size = 2) +
      geom_label_repel(force = 100, aes(label = paste0("Shear Stress = ", round(SS, 3), " \n", "Boundary = ", round(BD,3)))) +
      labs(x = "Shear Stress at D-84 Grain Size", y = "Boundary Shear Stress") + theme(text = element_text(face = "bold", size = 12))

  })


  output$table <- renderTable({
    df$reac()
})


}

shinyApp(ui, server)
