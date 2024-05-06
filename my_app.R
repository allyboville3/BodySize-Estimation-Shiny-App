#Libraries Used
library(tidyverse)
library(shiny)
library(DT)
library(broom)
library(dplyr)
library(plotly)


# Loading in Dataset
#this data is adapted from My work for the Data Replication Assignment
rodents <- "https://raw.githubusercontent.com/allyboville3/Data-Replication-Assignment/main/Hopkins%202008%20-%20Appendix1.csv"
rodent_df <- read_csv(rodents, col_names = TRUE)

# Cleaning Up Data

# Create columns for Rectangular Toothrow Area (RTRA)
# RTRA  = (LTRL X M1 width)
# With the acknowledgement that we are making the assumption that rodent cheek teeth are essentially rectangular in shape 
# (and that this may result in an overestimation of body mass), area was calculated by multiplying the length of the toothrow 
# by the width of the first lower molar. The first is most often the largest while the first and second molar are the widest. 
# M1 width in this data refers to either the first or second molar width which can be assumed to be the same and representative 
# of the maximum width of the toothrow. 

rodent_with_RTRA <- rodent_df %>% 
  mutate(RTRA = (`LTRL (mm)` * `M1 width (mm)`))
rodent_with_RTRA

# Averaging Values and Cleaning Up Spreadsheet for Interspecies Comparisons

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

#creating response and predictor variables
r <- "log(Mass)"
p <- c("log(LTRL)", "log(RTRA)")


# ui section
ui <- fluidPage(titlePanel(h1("Rodent Body Mass Estimator")),
                sidebarLayout(
                  sidebarPanel(width = 5,
                               selectInput( #selecting log(Mass) as response variable
                                 "response",
                                  label = "Select Response Variable log(Mass)",
                                  choices = c("", r)
                  ), 
                                selectInput( # choosing between Lower toothrow length and Rectangular toothrow area for predictor variables
                                  "predictors",
                                   label = "Choose the predictor variable",
                                   choices = p,
                                   multiple = FALSE
                  ), 
                  textOutput("model"), 
                  tableOutput("modelresults"),
                  br(),
                  numericInput("LTRL_predict", label = h5("Enter a number value below for LTRL"), value = 0), # allows for a positive numeric value to be entered for lower toothrow length
                  numericInput("RTRA_predict", label = h5("Enter a number value below for RTRA"), value = 0), # allows for a positive numeric value to be entered for rectangular toothrow area
                  actionButton("predict_button", "Generate Predicted Body Mass", class = "factor"), # action buttion to generate predicted body mass value
                  verbatimTextOutput("prediction_output"),
                  plotOutput("Body Size Estimation Plotted"),
                  
                 
                  ),
                  mainPanel(width = 7,
                           dataTableOutput("datatable"),
                           plotOutput("plot"),
                          
                    )
                  )
                )

#server section

#Running linear model from response and predictor categories selected
server <- function(input, output) {
  m <- reactive({
    mod <- NULL
    if (input$response == "" | length(input$predictors) == 0) {
      return(mod)
    }
    mod <- paste0(input$response, " ~ ", input$predictors[1])
    return(mod)
  })
  #outputs from liner model to be shwon in app
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
  
#Predictor Part 

# Uses linear models to predict body mass from numeric values entered for each measurement
  observeEvent(input$predict_button,{
    new_measurement_data <- data.frame(
      LTRL = as.numeric(input$LTRL_predict),
      RTRA = as.numeric(input$RTRA_predict)
      )
    
    LTRL_lm <- lm(log(Mass) ~ log(LTRL) , data = rodent_avg) # model 1
    RTRA_lm <- lm(log(Mass) ~ log(RTRA) , data = rodent_avg) # model 2
    
    predicted_BodyMass_ltrl <- predict(LTRL_lm, newdata = new_measurement_data)
    predicted_BodyMass_ltrl <- exp(predicted_BodyMass_ltrl)
    #predicted_BodyMass_ltrl <- mean(predicted_BodyMass_ltrl)
    
    predicted_BodyMass_rtra <- predict(RTRA_lm, newdata = new_measurement_data)
    #predicted_BodyMass_rtra <- mean(predicted_BodyMass_rtra)
    predicted_BodyMass_rtra <- exp(predicted_BodyMass_rtra)
    
    output$prediction_output <- renderText({ # printing statements with predicted mass value for each condition of LTRTL or RTRA
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
    
    # plotting predicted value with points from linear model, visualized in sidebar so can be compared with original plot of model
    # plots made in base r compared to ggplot 2 above
    # different conditional statements based off of numeric values entered for each measurement
    output$`Body Size Estimation Plotted` <- renderPlot({ #values less than 2 plot off of graph, and if LTRL > 76 and RTRA > 900
      if (new_measurement_data$LTRL > 2  &  new_measurement_data$LTRL < 76 & new_measurement_data$RTRA == 0) { 
        predict_bm <- log(predicted_BodyMass_ltrl) # need to log values again because I reversed this above to get numeric mass value
        input_ltrl <- log(new_measurement_data$LTRL)
        plot(rodent_avg$`log(LTRL)`, rodent_avg$`log(Mass)`, col = "black", xlab = "Log(LTRL)", ylab = "Log(Mass)", main = "Linear Model with Predicted Body Mass", pch = 16)
        points(input_ltrl, predict_bm, col = "red", pch = 16)
      legend("bottomright", legend = c("Actual Mass Data", "Predicted Mass Data"), col = c("black", "red"), pch = c(16,16))
      }
      else if (new_measurement_data$RTRA > 2 & new_measurement_data$RTRA < 900 & new_measurement_data$LTRL == 0) { 
        predict_bm_rtra <- log(predicted_BodyMass_rtra) # need to log values again because I reversed this above to get numeric mass value
        input_rtra <- log(new_measurement_data$RTRA)
        plot(rodent_avg$`log(RTRA)`, rodent_avg$`log(Mass)`, col = "black", xlab = "Log(RTRA)", ylab = "Log(Mass)", main = "Linear Model with Predicted Body Mass", pch = 16)
        points(input_rtra, predict_bm_rtra, col = "blue", pch = 16)
        legend("bottomright", legend = c("Actual Mass Data", "Predicted Mass Data"), col = c("black", "blue"), pch = c(16,16))
      }
      else if (new_measurement_data$LTRL > 2 & new_measurement_data$LTRL < 76 & new_measurement_data$RTRA > 2 & new_measurement_data$RTRA < 900 ) { 
        predict_bm <- log(predicted_BodyMass_ltrl) # need to log values again because I reversed this above to get numeric mass value
        input_ltrl <- log(new_measurement_data$LTRL)
        predict_bm_rtra <- log(predicted_BodyMass_rtra) # need to log values again because I reversed this above to get numeric mass value
        input_rtra <- log(new_measurement_data$RTRA)
        par(mfrow=c(2,1)) #allows for plots to occur together in two rows
        par(xpd=TRUE) # allows for legend to be created outside of plot
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

# creating app
shinyApp(ui = ui, server = server)

