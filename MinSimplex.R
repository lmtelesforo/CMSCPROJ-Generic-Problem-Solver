setwd("C:/Users/LENOVO/Desktop/3CodingRStudio")
# 
data <- read.csv("Simplex.csv")
current_max_print <- getOption("max.print")

# Set a new value for max.print (e.g., 5000)
options(max.print = 5000)
# # #
food = c()
# # food = data$Foods
# food = c("Frozen Broccoli",
#          "Carrots, Raw",
#          "Celery, Raw",
#          "Frozen Corn",
#          "Lettuce, Iceberg, Raw",
#          "Peppers, Sweet, Raw",
#          "Potatoes, Baked",
#          "Tofu",
#          "Roasted Chicken",
#          "Spaghetti W/ Sauce",
#          "Tomato, Red, Ripe, Raw",
#          "Apple, Raw, W/ Skin",
#          "Banana",
#          "Grapes",
#          "Kiwifruit, Raw, Fresh", "Oranges", "Bagels", "Wheat Bread", "White Bread", "Oatmeal Cookies")
# foodNutriValue = list()
# allFoods = list()
# allFoods <- data$Foods
# costAllFoods = data$Price.Serving
# # print(allFoods)
# n = length(food)
# # print(n)
# 
# for (i in 1:length(allFoods)) {
#   match_found <- FALSE
# 
#   for (j in 1:length(food)) {
#     if (tolower(food[j]) == tolower(allFoods[i])) {
#       # print(paste("Match found:", food[j], "at index", i))
#       match_found <- TRUE
# 
#       foodNutriValue[[allFoods[i]]] <- data[data$Foods == allFoods[i], ]
#     }
#   }
# 
#   if (!match_found) {
#     foodNutriValue[[allFoods[i]]] <- NULL
#   }
# }

Simplex = function (food, foodNutriValue) {
  allFoods <- data$Foods
  if (length(food) == 1) {
    infeasible = list(foodInput = food, reason = "only one food is selected.", feasibleYN = NA)
    return(infeasible)
  }
  
  # print(foodNutriValue)
  
  foundFoods <- names(foodNutriValue)[!sapply(foodNutriValue, is.null)]
  print(foundFoods)
  # caloriesValue <- foodNutriValue[["Frozen Broccoli"]]["Calories"]
  #
  n = length(foundFoods)
  cat(n, "foundFoods\n")
  
  num_rows = 22 + n + 1  # 22 nutrient constraints(min and max) + n serving size constraints (yung >= 10) + 1 objective function
  num_cols = n + 1  # n foods + 1 for the constraint
  
  initialmatrix <- matrix(0, nrow = num_rows - n - 1, ncol = num_cols -1)
  limitsmatrix = matrix(0, nrow = num_rows - n - 1)
  servingslimitmatrix = matrix(0, nrow = n, ncol = num_cols)
  objectivematrix = matrix(0, nrow = 1, ncol = num_cols)
  
  values = list()
  negatedvalues = list()
  pricelist = list()
  minlimits = c(2000, 0, 0, 0, 0, 25, 50, 5000, 50, 800, 10)
  maxlimits = c(2250, 300, 65, 2400, 300, 100, 100, 50000, 20000, 1600, 30)
  
  # After getting lahat ng values and inequality eqns, turn to >= lahat by multiplying by -1
  for (i in 1:length(foundFoods)) {
    numericValues <- unname(unlist(foodNutriValue[[foundFoods[i]]]))
    price = as.numeric(numericValues[2])
    numericValues <- numericValues[-c(1, 2)]
    negative = -(as.numeric(numericValues))
    
    values[[foundFoods[i]]] <- numericValues
    pricelist[[foundFoods[i]]] = price
    negatedvalues[[foundFoods[i]]] = negative
  }
  
  for (i in 1:num_cols-1) {
    servingslimitmatrix[i, i] =  -1
  }
  servingslimitmatrix[, num_cols] <- -10
# 
#   print("servingslimitmatrix")
# 
#   print(servingslimitmatrix)
#   print("limitsmatrix")
# 
#   print(limitsmatrix)
#   print("initialmatrix")
# 
#   print(initialmatrix)

  placeholder1 = matrix(0, nrow = length(foundFoods), ncol = 11)

  print(placeholder1) 
  row = 1
  for (i in 1:length(foundFoods)) {
    placeholder1[row, ] = as.numeric(values[[foundFoods[i]]])
    row = row + 1
  }
  print(placeholder1)

  row = 1
  placeholder2 = matrix(0, nrow = length(foundFoods), ncol = 11)
  for (i in 1:length(foundFoods)) {
    placeholder2[row, ] = -as.numeric(values[[foundFoods[i]]])
    row = row + 1
  }
  print(placeholder2)
  # min and max limits to ng nutrients

  nutriconstraintsmatrix = cbind(placeholder2, placeholder1)
  nutriconstraintsmatrix = t(nutriconstraintsmatrix)
  print(nutriconstraintsmatrix)

  placeholder1 = matrix(0, nrow = length(minlimits), ncol = 1)

  row = 1
  for (i in 1:length(minlimits)) {
    placeholder1[row, ] = minlimits[i]
    row = row + 1
  }
  # print(placeholder1)

  row = 1
  placeholder2 = matrix(0, nrow = length(maxlimits), ncol = 1)
  for (i in 1:length(maxlimits)) {
    placeholder2[row, ] = -(maxlimits[i])
    row = row + 1
  }
  # print(placeholder2)

  limitsmatrix = rbind(placeholder2, placeholder1)
  print(limitsmatrix) # nutrients and max min constraints sa last col
# 
  # print("HERE")
  # print(ncol(limitsmatrix))
  nutriconstraintsmatrix = cbind(nutriconstraintsmatrix, limitsmatrix)
  # Magkasamang nutrients depende sa input and limits ng max and min
  finalInitialmatrix = rbind(nutriconstraintsmatrix, servingslimitmatrix)
  print("FINAL INITIAL MATRIX")
  print(finalInitialmatrix)

  for (i in 1:length(foundFoods)) {
    objectivematrix [1, i] = (pricelist[[foundFoods[[i]]]])
  }

  # Cost of each food
  objectivematrix[num_cols] = 1
  finalInitialmatrix = rbind(finalInitialmatrix, objectivematrix)
  print(finalInitialmatrix)
#
  # Matrix is to be transposed na
  t_finalInitialmatrix = t(finalInitialmatrix)
  print("TRANSPOSED")
  print(t_finalInitialmatrix)

  objectivefunc <- t_finalInitialmatrix[nrow(t_finalInitialmatrix), ]
  print(objectivefunc)

  objectivefunc[-length(objectivefunc)] <- -objectivefunc[-length(objectivefunc)]

  newobj <- objectivefunc[-length(objectivefunc)]
  print(newobj) # New objective after i-transpose
  
  constraints = t_finalInitialmatrix[-nrow(t_finalInitialmatrix), -ncol(t_finalInitialmatrix)]

  # x is variables num food or num col ng constraints
  # Z ay sa objective function lang 1
#   # print(length(nrow(constraints))+1)
  slackvar = matrix(0, nrow = nrow(constraints)+1, ncol = nrow(constraints)+2)
#  After matranspose need na ng slack variables
  for (i in 0:nrow(constraints)+1) {
    slackvar[i, i] =  1
  }
  print("SLACKVAR")
  # print(slackvar)
  solution = t_finalInitialmatrix[, ncol(t_finalInitialmatrix)]
  solution = head(solution, -1) # No last element
  solution <- c(solution, 0)
  slackvar[, ncol(slackvar)] <- solution
  # print(slackvar)

  # constraints = cbind(constraints, slackvar)
  # print(constraints)

  initialtableau = rbind(constraints, newobj)
  # print(initialtableau)
#
  initialtableau = cbind(initialtableau, slackvar)
#   dimnames(initialtableau) <- list(NULL, NULL)
#
  print("-------------- INITIAL TABLEAU -------------")
  print(initialtableau)
  firstTableau = initialtableau
  initial_tableau_list = list()
  # initial_tableau_list[[length(initial_tableau_list) + 1]] <- initialtableau

  # Minus last row when finding pivot col
  matrix4pivotcol <- initialtableau[, -ncol(initialtableau)]

  ite <- 1

  basicSol <- numeric() 
  feasibleYN = FALSE

  basic_sol_list = list() # list ng matrices

    while (anyNA(initialtableau[nrow(initialtableau), ]) || any(initialtableau[nrow(initialtableau), ] < 0)) {
    cat("ITERATION ", ite, "\n")

    for (i in 1:(ncol(initialtableau))-1) {
      # (rowNumber - 1) number of zeroes and only 1 count of 1 in the column
      if (sum(initialtableau[, i] == 0) == nrow(initialtableau) - 1 && sum(initialtableau[, i] == 1) == 1) {
        # row index where the 1 is present
        row_index <- which(initialtableau[, i] == 1)
        # print(row_index)
        # append
        basicSol <- c(basicSol, initialtableau[row_index, ncol(initialtableau)])
      } else {
        # If conditions are not met, append 0 to basicSol
        basicSol <- c(basicSol, 0)
      }

      basicSolMatrix <- matrix(basicSol, nrow = 1)

    }

    # print("BASIC SOLUTION VECTOR")
    # print(ite)
    # print(basicSolMatrix)

    matrix4pivotcol <- initialtableau[, -ncol(initialtableau)]  # Recalculate matrix4pivotcol inside the loop

    matrix4pivotcol <- initialtableau[, -ncol(initialtableau)]
    last_row <- matrix4pivotcol[nrow(matrix4pivotcol), ]
    negative_values <- last_row[last_row < 0]
    # print(negative_values)

    if (length(negative_values) > 0) {
      pcIndex <- which(last_row == min(negative_values))
      cat(pcIndex, " max negative num ", min(negative_values), "\n")
    }

    min_TR <- Inf
    prIndex <- Inf
    TRfound = 0

    last_column <- initialtableau[, ncol(initialtableau)]
    for (i in 1:(nrow(initialtableau) - 1)) {
      TR <- last_column[i] / initialtableau[i, pcIndex]

      if (TR < min_TR && TR >= 0) { # Test ratio will always be < infinity
        min_TR <- TR
        prIndex <- i
        TRfound = 1
      }
    }

    if (TRfound == 0) {
      cat("all test ratios are negative.\n")
      feasibleYN = NA

      infeasible = list(foodInput = food, reason = "all test ratios are negative.", feasibleYN = NA)
      return(infeasible)
    }

    pivotElement <- initialtableau[prIndex, pcIndex]
    if (pivotElement == 0) {
      cat("pivot element is zero.\n")
      feasibleYN = NA

      infeasible = list(foodInput = food, reason = "pivot element is zero.", feasibleYN = NA)
      return(infeasible)
    }
    nPR <- (initialtableau[prIndex, ])/pivotElement

    # Perform subtraction by npr*C
    for (i in 1:nrow(initialtableau)) {
      # print(initialtableau)
      if (i != prIndex) {
        initialtableau[i, ] <- initialtableau[i, ] - (nPR * initialtableau[i, pcIndex])
      } else {
        initialtableau[i, ] <- nPR
      }
    }

    # Get negative values for new iteration
    negative_values <- matrix4pivotcol[nrow(matrix4pivotcol), ][matrix4pivotcol[nrow(matrix4pivotcol), ] < 0]


    basic_sol_list[[length(basic_sol_list) + 1]] <- basicSolMatrix
    basicSol = c() # reset
    initial_tableau_list[[length(initial_tableau_list) + 1]] <- initialtableau


    ite <- ite + 1
  }
  # There are no more negative values
  feasibleYN = TRUE
  optimizedCost = initialtableau[nrow(initialtableau), ncol(initialtableau)]

  finalmatrix = matrix(0, nrow=nrow(initialtableau), ncol=ncol(initialtableau))
  finalmatrix = initialtableau
  print("FINAL MATRIX-----------------\n")
  print(finalmatrix)
  finalmatrix_df <- as.data.frame(finalmatrix)


  n <- length(foundFoods)

  # Select columns from 22 plus n until the last column
  # 11 nutri + 11 negated nutri
  # minus 2 columns kasi pang total cost sya
  finalmatrixex2col <- finalmatrix[, -c((ncol(finalmatrix) - 1):ncol(finalmatrix))]

  selected_columns <- finalmatrix[, (22 + n+1):ncol(finalmatrixex2col)]
  # print(selected_columns)
  
  # For final solution
  lastRowSelected <- selected_columns[nrow(selected_columns), ]
  # print(lastRowSelected)
  optimizedFoodIndex <- which(lastRowSelected != 0)
  optimizedFoodStrings <- foundFoods[optimizedFoodIndex]
  # print(optimizedFoodIndex)

  # print(optimizedFoodStrings)
  # print(optimizedCost)
  
  # Get values kung saan di siya zero
  servingEachFood <- lastRowSelected[optimizedFoodIndex]
  # print(servingEachFood)

  cost <- list()

  # Get cost
  for (i in 1:length(allFoods)) {
    match_indices <- which(tolower(optimizedFoodStrings) == tolower(allFoods[i]))

    if (length(match_indices) > 0) {
      print(paste("Match found:", optimizedFoodStrings[match_indices], "at index", i))

      cost[[allFoods[i]]] <- data$Price.Serving[match_indices]
    } else {
      cost[[allFoods[i]]] <- NULL
    }
  }

  # print(cost)
  costVector <- unlist(cost)
  # print(length(costVector))

  menuBreakdown = matrix(0, nrow = length(optimizedFoodStrings), ncol = 3)
  finalSolutionmat = matrix(optimizedFoodStrings, nrow = 1)
  
  costxserving = costVector*servingEachFood

  menuBreakdown <- data.frame(
    Food = optimizedFoodStrings,
    Serving = servingEachFood,
    Cost = costxserving
  )
  # menuBreakdown <- data.frame(menuBreakdown, row.names = NULL)
  # rownames(menuBreakdown) <- NULL
  lastRowSelected = as.matrix(lastRowSelected)
  # foodInput = as.matrix(foundFoods)
  # foodInput = matrix(foundFoods, nrow = length(foundFoods), ncol=1)
  # print(foundFoods)
  foundFoodsList <- as.list(foundFoods)
  
  print(foundFoodsList)

  print(menuBreakdown)
  optimized = list(solvedmatrix = finalmatrix, foodInput = foundFoodsList, costOpt = optimizedCost, feasible = feasibleYN, servings = servingEachFood, costAll = costxserving, basicSolution = basic_sol_list, menu = menuBreakdown, iterations = ite, initialTableau = firstTableau, tableauAllIterations = initial_tableau_list, finalSolution = lastRowSelected, objfxn = objectivefunc)

  return(optimized)
}


# opt = Simplex(food, foodNutriValue)
# print(opt)