rm(list = ls())

source("C:/Users/LENOVO/Desktop/3CodingRStudio/Regression.R")
source("C:/Users/LENOVO/Desktop/3CodingRStudio/QuadSplineInt.R")
source("C:/Users/LENOVO/Desktop/3CodingRStudio/MinSimplex.R")
data <- read.csv("Simplex.csv")
options(max.print = 5000)

library(bslib)
library(shiny)
library(DT)
library(shinyjs)
library(fresh)
library(knitr)
library(kableExtra)
library(stringr)

# library(DT)
# AlgeBuddy: Your Neighborhood Generic Solver
# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "superhero"),
  shinyjs::useShinyjs(), 
  titlePanel(tags$h1("GENERIC PROBLEM SOLVER", style = "font-size: 40px; margin-left: 15px; margin-top: 18px; font-weight: bold; background-color: none; padding: 20px;")
  ),
  actionButton("usersmanualBtn", "USER'S MANUAL", style = "margin-top: -130px;font-size: 20px; margin-right:20px; background-color: none; border: none; outline: none; margin-left: 840px; box-shadow: none; border-radius: 10px;"),
  actionButton("regressionBtn", "REGRESSION", style = "margin-top: -130px; font-size: 20px; margin-right:20px; background-color: none; border: none; outline: none; box-shadow: none; border-radius: 10px;"),
  actionButton("interpolationBtn", "INTERPOLATION", style = "margin-top: -130px;font-size: 20px; margin-right:20px; background-color: none; border: none; outline: none; box-shadow: none; border-radius: 10px;"),
  actionButton("simplexBtn", "SIMPLEX", style = "margin-top: -130px;  font-size: 20px; margin-right:20px; background-color: none; border: none; outline: none; box-shadow: none; border-radius: 10px;"),
  
  div(style = "margin-bottom:20px;"),
  
  mainPanel(
    width = 12,
    style = "margin: auto;",  # Center the mainPanel
    div(
      textOutput("mainText"),
      uiOutput("mainUI"),
      style = "padding-top: 16px; padding-left: 10px; height: 600px; margin-left: 0px;background-color: transparent"
    )
  )
  
)

# Define server
server <- function(input, output, session) {
  # Add server logic as needed
  btn_states <- reactiveValues(regression = 0, interpolation = 0, simplex = 0, usersmanual = 1)
  
  output$mainText <- renderText({
  })
  
  output$mainUI <- renderUI({
    if (btn_states$regression > 0) {
      fluidRow(
        column(3,
               tags$h4("REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION", style = "font-weight: bold; position: absolute; top: 450px; margin-bottom: 0px; margin-left:70px; font-size: 43px; opacity: 0.1; padding-left: 10px; margin-top:130px"),
               br(),
               div(
                 fileInput("csvFile", "Input CSV file", accept = ".csv", multiple = FALSE),
                 style = "font-size: 16px; margin-top: -10px; margin-left:20px;"
               ),
               div(
                 textInput("order", "Degree for Regression:"),
                 style = "font-size: 16px; margin-top: 40px; margin-left:20px;"
               ),
               br(),
               div(
                 textInput("estimate", "Estimate function at x ="), style = "",
                 br(),
                 actionButton("solveBtn", "Solve", style = "margin-top: -32px; margin-left:73px; margin-right: 20px; font-size: 15px; background-color: #13293d; border: none; outline: none; box-shadow: none; border-radius: 10px; color: white;"),  # Added Calculate button
                 actionButton("resetBtn", "Reset", style = "margin-top: -32px; font-size: 15px; background-color: darkred; border: none; outline: none; box-shadow: none; border-radius: 10px; color:white;"),   # Added Reset button
                 style = "font-size: 16px; margin-top: -20px;margin-left:20px"
               ),
               br(),
               
        ),
        column(9,
               div(
                 tags$h4("Results", style = "font-size: 30px;font-weight: bold; margin-top: 10px; margin-bottom: 0px; margin-left:5px;opacity: 0.8"),
                 br(),
                 div(
                   verbatimTextOutput("regressionResults"),
                   style = "height: 450px; overflow-y: scroll; margin-right: 13px; "
                 )
               ),
        )
      )
    } else if (btn_states$interpolation > 0) {
      fluidRow(
        column(3,
               br(),
               tags$h4("QUADRATIC SPLINE INTERPOLATION QUADRATIC        SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION QUADRATIC SPLINE INTERPOLATION", style = "font-weight: bold; position: absolute; top: 290px; margin-bottom: 0px; margin-left:75px; font-size: 38px; opacity: 0.15; padding-left: 10px; color: #bfc0c0; margin-top: 270px"),
               br(),
               div(
                 fileInput("csvFile1", "Input CSV file", accept = ".csv", multiple = FALSE),
                 style = "font-size: 16px; margin-top: -30px; margin-left:20px"
               ),
               br(),
               div(
                 textInput("estimate1", "Estimate function at x ="),
                 style = "font-size: 16px; margin-top: -20px; margin-left:20px"
               ),
               br(),
               div(
                 br(),
                 actionButton("solveBtn1", "Solve", style = "margin-top:0px; margin-left:73px; margin-right: 20px; font-size: 15px; background-color: #13293d; border: none; outline: none; box-shadow: none; border-radius: 10px; color: white;"),  # Added Calculate button
                 actionButton("resetBtn1", "Reset", style = "margin-top: 0px; font-size: 15px; background-color: darkred; border: none; outline: none; box-shadow: none; border-radius: 10px; color:white;"),   # Added Reset button
                 style = "font-size: 16px; margin-top: -20px;margin-left:20px"
               ),
               br(),
        ),
        column(9,
               div(
                 tags$h4("Results", style = "font-size: 30px;font-weight: bold; margin-top: 10px; margin-bottom: 0px; margin-left:5px;opacity: 0.8"),                 
                 br(),
                 div(
                   verbatimTextOutput("interpolationResults"),
                   style = "height: 500px; overflow-y: scroll; margin-right: 13px"
                 )
               ),
               
        )
      )
    } 
    else if (btn_states$simplex > 0) {
      fluidRow(
        # style = "display: flex; justify-content: center; align-items: center;",
        column(12,
               br(),
               div(
                 style = "display: flex; justify-content: center; align-items: center;",
                 column(2,
                        checkboxGroupInput("group1", "Select foods for your diet:",
                                           choices = c("Frozen Broccoli", "Carrots, Raw", "Celery, Raw", "Frozen Corn", "Lettuce, Iceberg, Raw", "Peppers, Sweet, Raw", "Potatoes, Baked", "Tofu", "Roasted Chicken", "Spaghetti W/ Sauce", "Tomato, Red, Ripe, Raw", "Apple, Raw, W/ Skin", "Banana", "Grapes", "Kiwifruit, Raw, Fresh")
                        )
                 ),
                 column(2,
                        checkboxGroupInput("group2", " ",
                                           choices = c("Oranges", "Bagels", "Wheat Bread", "White Bread", "Oatmeal Cookies", "Apple Pie", "Chocolate Chip Cookies", "Butter, Regular", "Cheddar Cheese", "3.3% Fat, Whole Milk", "2% Lowfat Milk", "Skim Milk", "Poached Eggs", "Scrambled Eggs", "Bologna, Turkey")
                        )
                 ),
                 column(2,
                        checkboxGroupInput("group3", " ",
                                           choices = c("Frankfurter, Beef", "Ham, Sliced, Extralean", "Kielbasa, Prk", "Cap'N Crunch", "Cheerios", "Corn Flks, Kellogg'S", "Raisin Brn, Kellogg'S", "Rice Krispies", "Special K", "Oatmeal", "Malt-O-Meal, Choc", "Pizza W/ Pepperoni", "Taco", "Hamburger W/ Toppings", "Hotdog, Plain")
                        )
                 ),
                 column(2,
                        checkboxGroupInput("group4", " ",
                                           choices = c("Couscous", "White Rice", "Macaroni, Ckd", "Peanut Butter", "Pork", "Sardines in Oil", "White Tuna in Water", "Popcorn, Air-Popped", "Potato Chips, Bbqflvr", "Pretzels", "Tortilla Chip", "Chicknoodle Soup", "Splt Pea&Hamsoup", "Vegetbeef Soup", "Neweng Clamchwd")
                        )
                 ),
                 column(2,
                        checkboxGroupInput("group5", " ",  
                                           choices = c("Tomato Soup", "New E Clamchwd,W/ Milk", "Cream Mushroom Soup, W/Milk", "Beanbacn Soup, W/Water")
                        )
                 ),
                 
               ),
                        
        ),
               
        column(12,
               style = "display: flex; justify-content: center; align-items: center; margin-top: 20px",
               br(),
                 actionButton("solveBtn2", "Solve", style = "margin-right: 20px"),
                 actionButton("resetAllBtn", "Reset All", style = "margin-right: 20px"),
                 actionButton("selectAllBtn", "Select All", style = "margin-right: 20px"),
               br(),
        ),
        
          column(12,
                 div(
                   tags$h4("Results", style = "font-size: 30px;font-weight: bold; margin-top: 10px; margin-bottom: 0px; margin-left:5px;opacity: 1"),
                   br(),
                   div(
                     verbatimTextOutput("simplexResults"),
                   ),
                   
                   tags$h4("You selected the following foods:", style = "font-size: 15px;font-weight: bold; margin-top: 10px; margin-bottom: 0px; margin-left:5px;opacity: 1"),
                   DTOutput("foodListOutput"),
                   br(),
                   tags$h4("The Optimized Menu", style = "font-size: 25px;font-weight: bold; margin-top: 10px; margin-bottom: 10px; margin-left:5px;opacity: 0.9"),
                   
                   tags$h4("The Solution and Cost Breakdown by Food", style = "font-size: 25px;font-weight: bold; margin-top: 10px; margin-bottom: 10px; margin-left:5px;opacity: 0.9"),
                   DTOutput("TableOutput"),
                   
                   tags$h4("Initial Tableau: ", style = "font-size: 25px;font-weight: bold; margin-top: 10px; margin-bottom: 15px; margin-left:5px;opacity: 0.9"),
                   DTOutput("initialTableauOutput"),
                   br(),
                   div(
                     verbatimTextOutput("simplexResults1"),
                     style = "height: 500px; overflow-y: scroll; margin-right: 13px"
                   ),
                   tags$h4("Final Solution", style = "font-size: 25px;font-weight: bold; margin-top: 10px; margin-bottom: 0px; margin-left:5px;opacity: 0.9"),
                   DTOutput("finalSolutionOutput")
                   
              )
           )
        )
    } else if (btn_states$usersmanual > 0) {
      fluidRow(
        column(12,
               br(),
               tags$h4("USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL USER'S MANUAL", style = "font-weight: bold; position: absolute; top: 290px; margin-bottom: 0px; margin-left:64px; font-size: 38px; opacity: 0.15; padding-left: 10px; color: #bfc0c0; margin-top: 310px; margin-right: 40px; align: center;"),
               br(),
               div(
                 tags$h4("Q: How to use this generic problem solver?", style = "font-weight: bold; align: center; margin-left: 60px"),
                 tags$h4("1) Choose desired solver for your problem.", style = "margin-left: 60px"),
                 tags$h4("2) Input CSV file for Regression and Quadratic Spline Interpolation. For Simplex, check boxes to serve as your input.", style = "margin-left: 60px"),
                 tags$h4("3) Make sure to fill out all the required fields.", style = "margin-left: 60px"),
                 tags$h4("4) Hit solve.", style = "margin-left: 60px"),
                 tags$h4("5) Click reset to clear all inputs. For simplex, you may click 'Check All' or 'Reset All' to check or uncheck all checkboxes respectively.", style = "margin-left: 60px")
               ),
               br(),
               div(
                 br(),
        ),
               
        )
      )
    } 
  })
  
  observeEvent(input$solveBtn, {
    if (input$solveBtn > 0) {
      # Clear the output
      output$regressionResults <- cat("")
      x_values = c()
      y_values = c()
      if (!is.null(input$csvFile)) {
          # If a CSV file is provided, read data from the file
          file_path <- input$csvFile$datapath
          data <- read.csv(file_path, header = FALSE)  # No column names
          # Check if there are at least two columns
          if (ncol(data) < 2) {
            shinyalert::shinyalert("CSV file must have x and y values.", type = "warning")
          }
          x_values <- data[, 1]
          y_values <- data[, 2]
      }
      
      estimate <- tryCatch(
        expr = as.numeric(input$estimate),
        error = function(err) {
          # Handle the error here, for example, display a message or set a default value
          print(paste("Error converting 'estimate':", conditionMessage(err)))
          # Set a default value, for example, NA
          NA
        }
      )
      
      order_input <- tryCatch(
        expr = as.numeric(input$order),
        error = function(err) {
          # Handle the error here, for example, display a message or set a default value
          print(paste("Error converting 'order_input':", conditionMessage(err)))
          # Set a default value, for example, NA
          NA
        }
      )
      
      if (is.na(estimate) || is.na(order_input)) {
        shinyalert::shinyalert("Please input numbers only.", type = "warning")
      }
      else {
        
        if (length(x_values) != length(y_values)) {
          shinyalert::shinyalert("Mismatch number of x and y values.", type = "warning")
        }
        else if (order_input > length(x_values)) {
          shinyalert::shinyalert("Degree must not be greater than the number of datapoints.", type = "warning")
        }
        else if (length(x_values) == 1 || length(y_values) == 1) {
          shinyalert::shinyalert("Invalid input: x and y values", type = "warning")
        }
        else {
          # Check if input values are valid
          if (!any(is.na(x_values)) && !any(is.na(y_values)) && !any(is.na(order_input)) && !any(is.na(estimate))) {
            data = list(x = x_values, y = y_values)
            results <- PolynomialRegression(order_input, data)
            
            output$regressionResults <- renderPrint({
              # print(results)
              cat("x values:", x_values, "\n")
              cat("y values:", y_values, "\n\n")

              cat("Augmented Coefficient Matrix:\n")
              print(results$augcoeffmatrixREG)
              cat("\n")

              cat("Coefficients:\n")
              print(results$coefficients)
              cat("\n")

              cat("Polynomial String:\n")
              cat(results$polynomial_string, "\n")
              cat("\n")

              cat("Polynomial Function:\n")
              print(results$polynomial_function)
              cat("\n")

              cat("Estimate function at x =", estimate, "\n")
              cat("f(",estimate,") = ", results$polynomial_function(estimate))
            })
            
            
          }
          else {
            shinyalert::shinyalert("Incomplete input. Please check again. :D", type = "warning")
          }
        }
      }
        
    }
  })
  
  rv <- reactiveValues(data = NULL)
  
  observeEvent(input$resetBtn, {
    # Reset all input fields
    updateTextInput(session, "order", value = "")
    updateTextInput(session, "estimate", value = "")

    # Clear the output
    output$regressionResults <- cat("")
    
    # Reset file input
    output$resetFileInput <- renderUI({
      tags$input(type = "button", value = "Reset", id = "resetFileInput", style = "display: none;")
    })
    rv$data <- NULL
    shinyjs::reset("csvFile")
  })

  observeEvent(input$solveBtn1, {
    if (input$solveBtn1 > 0) {
      x_values1 = c()
      y_values1 = c()
      output$interpolationResults <- cat("")
      if (!is.null(input$csvFile1)) {
          # If a CSV file is provided, read data from the file
          file_path1 <- input$csvFile1$datapath
          data1 <- read.csv(file_path1, header = FALSE)  # No column names
          # Check if there are at least two columns
          if (ncol(data1) < 2) {
            shinyalert::shinyalert("CSV file must have at least two columns.", type = "warning")
          }
          x_values1 <- data1[, 1]
          y_values1 <- data1[, 2]
        } 
        
        estimate1 <- tryCatch(
          expr = as.numeric(input$estimate1),
          error = function(err) {
            # Handle the error here, for example, display a message or set a default value
            print(paste("Error converting 'estimate':", conditionMessage(err)))
            NA
          }
        )
        
        if (is.na(estimate1)) {
          shinyalert::shinyalert("Please input numbers only.", type = "warning")
        }
        else {
          if (length(x_values1) != length(y_values1)) {
            shinyalert::shinyalert("Mismatch number of x and y values.", type = "warning")
          }
          else if (length(x_values1) < 3 || length(y_values1) < 3) {
            shinyalert::shinyalert("Incomplete input: AT LEAST 3 x and y values for QSI.", type = "warning")
          }
          else if (estimate1 >= max(x_values1)) {
            shinyalert::shinyalert("Estimate must be less than the max value of x values.", type = "warning")
          }
          else if (estimate1 <= min(x_values1)) {
            shinyalert::shinyalert("Estimate must be greater than the minimum value of x values.", type = "warning")
          }
          else {
            if (!any(is.na(x_values1)) && !any(is.na(y_values1)) && !any(is.na(estimate1))) {
              
              results1 <- QuadraticSplineInterpolation(x_values1, y_values1, estimate1)
              
              output$interpolationResults <- renderPrint({
                cat("x values:", results1$x_val, "\n")
                cat("y values:", results1$y_val, "\n\n")
                
                cat("Augmented Coefficient Matrix:\n")
                print(results1$matrix$augcoeffmatrix)
                cat("\n")
                
                cat("Variables:\n")
                print(results1$matrix$variables)
                cat("\n")
                
                cat("Solution:\n")
                print(results1$matrix$solution)
                cat("\n")
                
                cat("Polynomial Equations:\n")
                lapply(results1$equations, cat, sep = "\n")
                cat("\n")
                
                cat("Polynomial Function:\n")
                print(results1$functions)
                
                cat("Estimate function at x =", estimate1, "\n")
                cat(results1$approximation)
                
                cat("\n\nChosen function: f(", results1$intervalInd, ") for interval ", results1$lowerbound, ",", results1$upperbound, "\n")
                print(results1$chosenfxn)
                
                # print(results1)
              })
            }
            else {
              shinyalert::shinyalert("Incomplete input. Please check again. :D", type = "warning")
            }
          }
        }
      }
  })

  rv <- reactiveValues(data = NULL)

  observeEvent(input$resetBtn1, {
    updateTextInput(session, "estimate1", value = "")

    # Clear the output
    output$interpolationResults <- cat("")

    # Reset file input
    output$resetFileInput <- renderUI({
      tags$input(type = "button", value = "Reset", id = "resetFileInput", style = "display: none;")
    })
    rv$data <- NULL
    shinyjs::reset("csvFile1")
  })
  
  observeEvent(input$solveBtn2, {
    if (input$solveBtn2 > 0) {
      output$simplexResults <- cat("")
      
      checkboxGroups <- c("group1", "group2", "group3", "group4", "group5")
      
      food = c()
      foodNutriValue = list()
      
      # Loop through each checkbox group and append the selected choices to the vector
      for (group_id in checkboxGroups) {
        food <- c(food, input[[group_id]])
      }
      
      # Filter out NULL values (unchecked checkboxes) and flatten the list
      food <- unlist(Filter(Negate(is.null), food))
      
      
      if (is.null(food)) {
        shinyalert::shinyalert("Please select foods first. :D", type = "warning")
      }
      else {
        allFoods = list()
        allFoods <- data$Foods
        costAllFoods = data$Price.Serving
        # print(allFoods)
        n = length(food)
        print(n)
        
        for (i in 1:length(allFoods)) {
          match_found <- FALSE
          
          for (j in 1:length(food)) {
            if (tolower(food[j]) == tolower(allFoods[i])) {
              print(paste("Match found:", food[j], "at index", i))
              match_found <- TRUE
              
              foodNutriValue[[allFoods[i]]] <- data[data$Foods == allFoods[i], ]
            }
          }
          
          if (!match_found) {
            foodNutriValue[[allFoods[i]]] <- NULL
          }
        }
        cat("length of foodnutrival ", length(foodNutriValue), "\n")
        
        results2 <- Simplex(food, foodNutriValue)
        
        emptymat = matrix("", nrow = 1)

        output$simplexResults <- renderPrint({
          # print(results2)
          if (any(is.na(results2))) {
            cat("You selected", length(food), "to consider for your diet.\n")
            print(results2$foodInput)
            cat("The problem is infeasible.\n")
            cat("It is not possible to meet the nutritional constraints with the foods that you have selected. The possible reason is that", results2$reason)
            output$TableOutput <- renderDataTable({
              DT::datatable(emptymat,
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            output$foodListOutput <- renderDataTable({
              DT::datatable(emptymat,
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            output$initialTableauOutput <- renderDataTable({
              DT::datatable(emptymat,
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            output$finalSolutionOutput <- renderDataTable({
              DT::datatable(emptymat,
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            output$simplexResults1 <- renderPrint({ 
              cat("")
            })
          }
          else {
            output$foodListOutput <- renderDataTable({
              DT::datatable(do.call(rbind, results2$foodInput))
            })
            
            cat("The cost of this optimal diet is $",results2$costOpt,"per day. \n")
            
            cat("Resulting value of the Objective Function: \n", results2$costOpt, "\n")
            
            output$TableOutput <- renderDataTable({
              DT::datatable(results2$menu,
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            
          
            output$initialTableauOutput <- renderDataTable({
              DT::datatable(results2$initialTableau,
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            
            output$simplexResults1 <- renderPrint({ 
              cat("NUMBER OF ITERATIONS", length(results2$basicSolution), "\n")
              for(i in 1:length(results2$basicSolution)) {
                cat("ITERATION ", i, "\n")
                print("Resulting Tableau")
                print(results2$tableauAllIterations[[i]])
                print("Basic Solution")
                print(results2$basicSolution[[i]])
              }
            })
            
            output$finalSolutionOutput <- renderDataTable({
              DT::datatable(t(results2$finalSolution),
                            options = list(
                              scrollY = TRUE,  
                              paging = FALSE   
                            )
              )
            })
            

          }
          output$initialTableauOutput <- renderDataTable({
            DT::datatable(results2$initialTableau)
          })    
        })
      }
    }
    
  })
  
  rv <- reactiveValues(data = NULL)
  
  observeEvent(input$resetAllBtn, {
    emptymat = matrix("", nrow = 1)
    output$simplexResults <- renderPrint({ 
      cat("")
    })
    
    updateCheckboxGroupInput(session, "group1", selected = character(0))
    updateCheckboxGroupInput(session, "group1", selected = NULL)
    updateCheckboxGroupInput(session, "group2", selected = character(0))
    updateCheckboxGroupInput(session, "group2", selected = NULL)
    updateCheckboxGroupInput(session, "group3", selected = character(0))
    updateCheckboxGroupInput(session, "group3", selected = NULL)
    updateCheckboxGroupInput(session, "group4", selected = character(0))
    updateCheckboxGroupInput(session, "group4", selected = NULL)
    updateCheckboxGroupInput(session, "group5", selected = character(0))
    updateCheckboxGroupInput(session, "group5", selected = NULL)
    
    output$TableOutput <- renderDataTable({
      DT::datatable(emptymat,
                    options = list(
                      scrollY = TRUE,  
                      paging = FALSE   
                    )
      )
    })
    output$foodListOutput <- renderDataTable({
      DT::datatable(emptymat,
                    options = list(
                      scrollY = TRUE,  
                      paging = FALSE   
                    )
      )
    })
    output$initialTableauOutput <- renderDataTable({
      DT::datatable(emptymat,
                    options = list(
                      scrollY = TRUE,  
                      paging = FALSE   
                    )
      )
    })
    output$finalSolutionOutput <- renderDataTable({
      DT::datatable(emptymat,
                    options = list(
                      scrollY = TRUE,  
                      paging = FALSE   
                    )
      )
    })
    output$simplexResults1 <- renderPrint({ 
      cat("")
    })
  }) 
  
   observeEvent(input$selectAllBtn, {
     choices1 = c("Frozen Broccoli",
                  "Carrots, Raw",
                  "Celery, Raw",
                  "Frozen Corn",
                  "Lettuce, Iceberg, Raw",
                  "Peppers, Sweet, Raw",
                  "Potatoes, Baked",
                  "Tofu",
                  "Roasted Chicken",
                  "Spaghetti W/ Sauce",
                  "Tomato, Red, Ripe, Raw",
                  "Apple, Raw, W/ Skin",
                  "Banana",
                  "Grapes",
                  "Kiwifruit, Raw, Fresh")
     choices2 = c("Oranges", "Bagels", "Wheat Bread", "White Bread", "Oatmeal Cookies", "Apple Pie", "Chocolate Chip Cookies", "Butter, Regular", "Cheddar Cheese", "3.3% Fat, Whole Milk", "2% Lowfat Milk", "Skim Milk", "Poached Eggs", "Scrambled Eggs", "Bologna, Turkey")
     choices3 = c("Frankfurter, Beef", "Ham, Sliced, Extralean", "Kielbasa, Prk", "Cap'N Crunch", "Cheerios", "Corn Flks, Kellogg'S", "Raisin Brn, Kellogg'S", "Rice Krispies", "Special K", "Oatmeal", "Malt-O-Meal, Choc", "Pizza W/ Pepperoni", "Taco", "Hamburger W/ Toppings", "Hotdog, Plain")
     choices4 = c("Couscous", "White Rice", "Macaroni, Ckd", "Peanut Butter", "Pork", "Sardines in Oil", "White Tuna in Water", "Popcorn, Air-Popped", "Potato Chips, Bbqflvr", "Pretzels", "Tortilla Chip", "Chicknoodle Soup", "Splt Pea&Hamsoup", "Vegetbeef Soup", "Neweng Clamchwd")
     choices5 = c("Tomato Soup", "New E Clamchwd,W/ Milk", "Cream Mushroom Soup, W/Milk", "Beanbacn Soup, W/Water")
     updateCheckboxGroupInput(session, "group1", choices = choices1, selected = choices1)
     updateCheckboxGroupInput(session, "group2", choices = choices2, selected = choices2)
     updateCheckboxGroupInput(session, "group3", choices = choices3, selected = choices3)
     updateCheckboxGroupInput(session, "group4", choices = choices4, selected = choices4)
     updateCheckboxGroupInput(session, "group5", choices = choices5, selected = choices5)
  }) 
  
  observeEvent(input$exitBtn, {
    stopApp()  # This stops the Shiny app
  })
  
  observeEvent(input$regressionBtn, {
    btn_states$usersmanual = 0
    btn_states$regression = 1
    btn_states$simplex = 0# Reset regression button state
    btn_states$interpolation <- 0  # Reset regression button state
  })
  
  observeEvent(input$interpolationBtn, {
    btn_states$usersmanual = 0
    btn_states$regression = 0
    btn_states$simplex = 0# Reset regression button state
    btn_states$interpolation <- 1
  })
  
  observeEvent(input$simplexBtn, {
    btn_states$usersmanual = 0
    btn_states$regression = 0
    btn_states$interpolation <- 0 
    btn_states$simplex <- 1  # Reset regression button state
  })
  
  observeEvent(input$usersmanualBtn, {
    btn_states$usersmanual = 1
    btn_states$regression = 0
    btn_states$interpolation <- 0 
    btn_states$simplex <- 0  # Reset regression button state
  })
  
}

# Run the app
shinyApp(ui, server)
