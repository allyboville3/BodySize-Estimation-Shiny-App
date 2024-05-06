#Libraries Used
library(tidyverse)
library(shiny)
library(DT)
library(broom)
library(dplyr)
library(plotly)


# Loading in Dataset
rodents <- "https://raw.githubusercontent.com/allyboville3/Data-Replication-Assignment/main/Hopkins%202008%20-%20Appendix1.csv"
rodent_df <- read_csv(rodents, col_names = TRUE)

## Create columns for Rectangular Toothrow Area (RTRA)
# RTRA  = (LTRL X M1 width)
# With the acknowledgement that we are making the assumption that rodent cheek teeth are essentially rectangular in shape 
# (and that this may result in an overestimation of body mass), area was calculated by multiplying the length of the toothrow 
# by the width of the first lower molar. The first is most often the largest while the first and second molar are the widest. 
# M1 width in this data refers to either the first or second molar width which can be assumed to be the same and representative 
# of the maximum width of the toothrow. 

rodent_with_RTRA <- rodent_df %>% 
  mutate(RTRA = (`LTRL (mm)` * `M1 width (mm)`))
rodent_with_RTRA

# Averaging Values and Cleaning Up Spreadsheet for Interspecises Comparisons

rodent_avg <- rodent_with_RTRA %>% 
  mutate(Mass = as.numeric(`mass (g)`), Pub_avg_mass = as.numeric(`pub. avg. mass (g)`), RTRA = as.numeric(`RTRA`)) %>% #change data for Mass, Published Average Mass, and Retangular Toothrow Area from type chr to dbl
  group_by(Species) %>% #grouping data by Species
  tidyr::fill(Pub_avg_mass, .direction ="down") %>% #filling in NA's in published averaged masses to make the following code happy
  #"The average body mass reported by Ernest (2003) was used for 3 species (Hydrochoerus hydrochaeris, Graphiurus murinus, and Cryptomys hottentotus) for which museum specimens lacked data on body mass." - Copied from Hopkins 2008
  mutate(Mass = coalesce(Mass,Pub_avg_mass)) %>% #Sub NA values for Mass with Previously published mass averages
  summarize(
    Mass = mean(`Mass`), #averages mass for each species
    LTRL = mean(`LTRL (mm)`), #averages toothrow length for each species
    `Toothrow_Width` = mean(`M1 width (mm)`), #averages toothrow width (based off of M1 width) for each species
    RTRA = mean(RTRA),
    `log(Mass)` = log(Mass),
    `log(LTRL)` = log(LTRL),
    `log(RTRA)` = log(RTRA)
  ) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) # rounding data to only have two decimal places
rodent_avg

r <- "log(Mass)"
p <- c("log(LTRL)", "log(RTRA)")


# ui section

ui <- fluidPage(titlePanel(h1("Rodent Body Mass Estimator")),
                sidebarLayout(
                  sidebarPanel(width = 5,
                               selectInput(
                                 "response",
                                  label = "Select Response Variable log(Mass)",
                                  choices = c("", r)
                  ), 
                                selectInput(
                                  "predictors",
                                   label = "Choose the predictor variable",
                                   choices = p,
                                   multiple = FALSE
                  ), 
                  textOutput("model"), 
                  tableOutput("modelresults"),
                  br(),
                  numericInput("LTRL_predict", label = h5("Enter a number value below for LTRL"), value = 0),
                  numericInput("RTRA_predict", label = h5("Enter a number value below for RTRA"), value = 0),
                  actionButton("predict_button", "Generate Predicted Body Mass", class = "factor"),
                  verbatimTextOutput("prediction_output"),
                  plotOutput("Body Size Estimation Plotted"),
                  
                 
                  ),
                  mainPanel(width = 7,
                           dataTableOutput("datatable"),
                           plotOutput("plot"),
                          
                  )
                            
                            
                   # tabsetPanel(type = "tabs",
                                        #tabPanel("Prediction",
                                                # fluidPage(
                                                  # titlePanel(title = div("Monthly Sales Volume", style = "color: #333333; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
                                                   #sidebarLayout(
                                                     
                                                    # mainPanel( 
                                                      
                                                #   )))
                                            #)
                  )
                )
           # )
#)

#server section
server <- function(input, output) {
  m <- reactive({
    mod <- NULL
    if (input$response == "" | length(input$predictors) == 0) {
      return(mod)
    }
    mod <- paste0(input$response, " ~ ", input$predictors[1])
    return(mod)
  })

  output$modelresults <- renderTable({
    if (!is.null(m())) {
      res <- lm(data = rodent_avg, formula = m())
      tidy(res) %>% 
        select(term, estimate, p.value)
      }
  }, width = "100%", rownames = TRUE, striped = TRUE, spacing = "s", bordered = TRUE,
  align = "c", digits = 2)
  
  output$model <- renderText({
    paste0("Model: ", print(m()))
  })
  
  output$datatable <-
    renderDataTable(rodent_avg, options = list(
      paging = TRUE,
      lengthMenu = list(c(5, 15, -1), c('5', '15' , 'All')),
      pageLength = 5
    ))
  
  output$plot <- renderPlot({
    if (!is.null(m()) & length(input$predictors) == 1) {
      y <- input$response
      x <- input$predictors
      if (class(rodent_avg[[x]]) != "factor") {
        p <- ggplot(data = rodent_avg, aes(x = .data[[x]], y = .data[[y]])) + geom_point() +
          geom_smooth(method = lm, color = "coral") + theme_bw()
      } 
      p <- p + xlab(x) + ylab(y) + theme(axis.text.x = element_text(angle = 90,
                                                                    hjust = 1)) + theme_bw()
      p
    } 
  })
  
  
  observeEvent(input$predict_button,{
    new_measurement_data <- data.frame(
      LTRL = as.numeric(input$LTRL_predict),
      RTRA = as.numeric(input$RTRA_predict)
      )
    
    LTRL_lm <- lm(log(Mass) ~ log(LTRL) , data = rodent_avg)
    RTRA_lm <- lm(log(Mass) ~ log(RTRA) , data = rodent_avg)
    
    predicted_BodyMass_ltrl <- predict(LTRL_lm, newdata = new_measurement_data)
    predicted_BodyMass_ltrl <- exp(predicted_BodyMass_ltrl)
    #predicted_BodyMass_ltrl <- mean(predicted_BodyMass_ltrl)
    
    predicted_BodyMass_rtra <- predict(RTRA_lm, newdata = new_measurement_data)
    #predicted_BodyMass_rtra <- mean(predicted_BodyMass_rtra)
    predicted_BodyMass_rtra <- exp(predicted_BodyMass_rtra)
    
    output$prediction_output <- renderText({
        if (new_measurement_data$LTRL > 0  & new_measurement_data$RTRA == 0) { 
        paste0("Prediction of Body Mass from Lower Toothrow Length ", print(predicted_BodyMass_ltrl), " grams")
       }
        else if (new_measurement_data$LTRL > 0 & new_measurement_data$RTRA > 0) { 
          paste0("Prediction of Body Mass from Lower Toothrow Length is ", print(predicted_BodyMass_ltrl), "and Body Mass from Rectangular Toothrow Area is ", print(predicted_BodyMass_rtra), " grams")
      }
        else if (new_measurement_data$RTRA > 0 & new_measurement_data$LTRL == 0) { 
          paste0("Prediction of Body Mass from Rectangular Toothrow Area ", print(predicted_BodyMass_rtra), " grams")
      }
        else if (new_measurement_data$RTRA == 0 & new_measurement_data$LTRL == 0) {
          print("Enter in a positive numeric value for LTRL or RTRA in mm")
      }
        else{
          print("Enter in a postive numeric value for LTRL or RTRA in mm")
      }
     })
    
    output$`Body Size Estimation Plotted` <- renderPlot({ #values less than 2 plot off of graph
      if (new_measurement_data$LTRL > 2  &  new_measurement_data$LTRL < 76 & new_measurement_data$RTRA == 0) { 
        predict_bm <- log(predicted_BodyMass_ltrl)
        input_ltrl <- log(new_measurement_data$LTRL)
        plot(rodent_avg$`log(LTRL)`, rodent_avg$`log(Mass)`, col = "black", xlab = "Log(LTRL)", ylab = "Log(Mass)", main = "Linear Model with Predicted Body Mass", pch = 16)
        points(input_ltrl, predict_bm, col = "red", pch = 16)
      legend("bottomright", legend = c("Actual Mass Data", "Predicted Mass Data"), col = c("black", "red"), pch = c(16,16))
      }
      else if (new_measurement_data$RTRA > 2 & new_measurement_data$RTRA < 900 & new_measurement_data$LTRL == 0) { 
        predict_bm_rtra <- log(predicted_BodyMass_rtra)
        input_rtra <- log(new_measurement_data$RTRA)
        plot(rodent_avg$`log(RTRA)`, rodent_avg$`log(Mass)`, col = "black", xlab = "Log(RTRA)", ylab = "Log(Mass)", main = "Linear Model with Predicted Body Mass", pch = 16)
        points(input_rtra, predict_bm_rtra, col = "blue", pch = 16)
        legend("bottomright", legend = c("Actual Mass Data", "Predicted Mass Data"), col = c("black", "blue"), pch = c(16,16))
      }
      else if (new_measurement_data$LTRL > 2 & new_measurement_data$LTRL < 76 & new_measurement_data$RTRA > 2 & new_measurement_data$RTRA < 900 ) { 
        predict_bm <- log(predicted_BodyMass_ltrl)
        input_ltrl <- log(new_measurement_data$LTRL)
        predict_bm_rtra <- log(predicted_BodyMass_rtra)
        input_rtra <- log(new_measurement_data$RTRA)
        par(mfrow=c(2,1))
        par(xpd=TRUE)
        plot(rodent_avg$`log(LTRL)`, rodent_avg$`log(Mass)`, col = "black", xlab = "Log(LTRL)", ylab = "Log(Mass)", main = "Linear Model with Predicted Body Mass (LTRL)", pch = 16)
        points(input_ltrl, predict_bm, col = "red", pch = 16)
        legend(1.8,14, legend = c("Actual Mass Data", "Predicted Mass Data"), horiz=TRUE, bty='n', cex=0.8, col = c("black", "red"), pch = c(16,16))
        plot(rodent_avg$`log(RTRA)`, rodent_avg$`log(Mass)`, col = "black", xlab = "Log(RTRA)", ylab = "Log(Mass)", main = "Linear Model with Predicted Body Mass (RTRA)", pch = 16)
        points(input_rtra, predict_bm_rtra, col = "blue", pch = 16)
        legend(1.8,14, legend=c("Actual Mass Data","Predicted Mass Data"), horiz=TRUE, bty='n', cex=0.8, col = c("black", "blue"),  pch=c(16,16), )
      }
    })
  })
}
  
shinyApp(ui = ui, server = server)

