visualizeServer <- function(input, output, data, cataChoices, numChoices) {

  observeEvent({
    input$x_var
    input$y_var
    input$allow_choas
  }, {
    req(data(), input$x_var, input$y_var)
    cataChoices = cataChoices()
    numChoices = numChoices()

    all_possible_choices <- c(
      "Scatter Plot" = "point",
      "Line Plot" = "line",
      "Bar plot" = "bar",
      "Trent Line" = "smooth"
    )

    if (isTRUE(input$allow_choas)){
      valid_choices = all_possible_choices
    } else {
       data <- data()
       if (input$x_var %in% numChoices && input$y_var %in% numChoices){
        valid_choices <- c("Scatter Plot" = "point", "Line Plot" = "line", "Trend Line" = "smooth")
       } else if (input$x_var %in% numChoices && input$y_var %in% cataChoices) {
        valid_choices <- c("Bar Chart (Averages)" = "bar", "Box Plot" = "boxplot")
       } else if (input$x_var %in% cataChoices && input$y_var %in% numChoices) {
        valid_choices <- c("Bar Chart (Averages)" = "bar", "Box Plot" = "boxplot")
       } else { 
        valid_choices <- c("Jittered Count Plot" = "jitter") #Both cata
       }
    }
    
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "plot_type",
      choices = valid_choices
    )
  }
  
  ) 



  plot_data <- eventReactive(input$go_plot, {
    req(data(), input$x_var, input$y_var, input$plot_type)
    data <- data()

    p <- ggplot(data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      theme_minimal()

    if (isTRUE(input$allow_choas)) {
      if (input$plot_type == "point") p <- p + geom_point(alpha = 0.6)
      else if (input$plot_type == "line") p <- p + geom_line()
      else if (input$plot_type == "bar") p <- p + geom_col()
      else if (input$plot_type == "smooth") p <- p + geom_smooth( se =FALSE)

    } else {
      p <- switch(input$plot_type,
        "point" = p + geom_point(alpha = 0.6),
        "line" = p + geom_line(),
        "Smooth" = p + geom_smooth(se = FALSE),
        "bar" = p + geom_bar(stat = "summary", fun = "mean", fill = "#008cff"),
        "boxplot" = p + geom_boxplot(fill = "#5e5e5e"),
        "jitter" = p + geom_jitter(alpha = 0.5, width = 0.2, height = 0.2)
      
      )
    }

    ggplotly(p)

  })





  output$v_plot <- renderPlotly({ plot_data() })
}
