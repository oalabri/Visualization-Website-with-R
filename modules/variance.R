varianceServer <- function(input, output, data, cataChoices, numChoices) { 

    variance_compare <- eventReactive(input$go_variance_compare, {
        req(data(), input$x_variance_compare, input$y_variance_compare)
        data <- data()
        cataChoices <- cataChoices()
        numChoices <- numChoices()

        x_var <- input$x_variance_compare
        y_var <- input$y_variance_compare
        x_var_data <- na.omit(data[[x_var]])
        y_var_data <- na.omit(data[[y_var]])

        if(x_var %in% numChoices && y_var %in% numChoices){

            corr <- cor(x_var_data, y_var_data, method = "pearson")
            f_result <- var.test(x_var_data, y_var_data)
            
            info_table <- data.frame(
                Metric = c("Variance", "Standard Deviation", "The Mean", "Coefficient of Variation CV"),
                x = c(var(x_var_data), sd(x_var_data), mean(x_var_data), sd(x_var_data)/mean(x_var_data)),
                y = c(var(y_var_data), sd(y_var_data), mean(y_var_data), sd(y_var_data)/mean(y_var_data))
            )
            names(info_table)[2:3] <- c(as.character(x_var), as.character(y_var))

            info_compare <- glue::glue("
            ===========================================================
            ===========You have entered two numerical colums===========
            ============ Comparison Info ==============================
            Correlation Coefficient r: {corr},
            F-Statistic value : {f_result$statistic},
            Comparison p_value : {f_result$p.value},
            Degrees of Freedom : Numerator = {f_result$parameter[1]}, Denominator = {f_result$parameter[2]},
            95% Confidence Interval for the ratio of variances : ({f_result$conf.int[1]}, {f_result$conf.int[2]})
            =============================================================
            ")

        } else if (x_var %in% cataChoices && y_var %in% cataChoices) {

            cont_table <- table(x_var_data, y_var_data)
            
            info_table <- cont_table


            chi_test <- chisq.test(cont_table)

            n <- sum(cont_table)
            r <- nrow(cont_table)
            k <- ncol(cont_table)

            cramer_v <- sqrt(
            as.numeric(chi_test$statistic) /
            (n * min(r - 1, k - 1))
            )

            calculate_entropy <- function(x) {
            probs <- table(x) / length(x)
            -sum(probs * log(probs))
            }

           entropy_x <- calculate_entropy(table(x_var_data))
           entropy_y <- calculate_entropy(table(y_var_data))

           info_compare <- glue::glue("
           ============================================================
           ==========You have entered two catagorical columns==========
           ===========Comparison Info =================================
           Chi-square statistic : {chi_test$statistic},
           Chi-square p_value : {chi_test$p.value},
           Cramer's V : {cramer_v},
           Entropy_x : {entropy_x},
           Entropy_y : {entropy_y}
           ")



        } else {
            # get the catagorical variable
            if (y_var %in% cataChoices){
                cata_var <- y_var
                num_var <- x_var
            } else {
               cata_var <- x_var
               num_var <- y_var
            }


            info_table <- data %>%
                group_by(.data[[cata_var]]) %>%
                summarise(
                    Variance = var(.data[[num_var]], na.rm = TRUE),
                    SD = sd(.data[[num_var]], na.rm = TRUE),
                    count = n()
                )

            formula <- as.formula(paste(num_var, "~", cata_var))


            group_medians <- ave(
                data[[num_var]],
                data[[cata_var]],
                FUN = median
            )

            abs_dev <- abs(data[[num_var]] - group_medians)
            levene_model <- aov(abs_dev ~ data[[cata_var]])
            p_val_levene <- summary(levene_model)[[1]][["Pr(>F)"]][1]


            anova_res <- aov(formula, data = data)
            p_val_anova <- summary(anova_res)[[1]][["Pr(>F)"]][1]

            info_compare <- glue::glue("
            ==============================================================
            ====You have entered a catagorical and a numerical colums=====
            ==================Comparison Info=============================
            Levene's Test p_value : {p_val_levene},
            ANOVA p_value : {p_val_anova}
            ==============================================================
            ")
           
        }

    return(list(table = info_table, info = info_compare))


        
    })





    variance_plot <- eventReactive(input$go_variance_plot, {
        req(data(), input$variance_plot_var, input$variance_plot_type)
        data <- data()
        cataChoices <- cataChoices()
        numChoices <- numChoices()


        plots_list <- lapply(input$variance_plot_var, function(var){
            if (var %in% numChoices && input$variance_plot_type == "Boxplot"){
                p <- ggplot(data, aes(x = "", y = .data[[var]]))+
                geom_boxplot()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

                return(plotly::ggplotly(p))

            } else if (var %in% numChoices && input$variance_plot_type == "Violen") {
                p <- ggplot(data, aes(x = "", y = .data[[var]]))+
                geom_violin()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

                return(plotly::ggplotly(p))

            } else if(var %in% cataChoices){
                p <- ggplot(data, aes(x = .data[[var]]))+
                geom_bar()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

                return(plotly::ggplotly(p))

            }else {
                print("Issue with plotting ifs")
               return(NULL)
            }
        })

        plots_list = purrr::compact(plots_list)
        if (length(plots_list) == 0) return(NULL)
        num_cols <- if (length(plots_list) > 3) 2 else length(plots_list)
        num_rows = ceiling(length(plots_list) / num_cols)

        combined_plots = plotly::subplot(
            plots_list,
            nrows = num_rows,
            margin = 0.1,
            titleX = TRUE,
            titleY = TRUE
        )

        combined_plots = plotly::layout(
            combined_plots,
            height = 400 * num_rows,
            autosize = TRUE
        )


        return(combined_plots)


    })



    output$plot_variance_out <- renderPlotly({
        variance_plot()
    })

    output$info_table <- renderTable({
        variance_compare()$table

    })

    output$info_compare <- renderText({
        variance_compare()$info
    })
    

}