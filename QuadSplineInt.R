# # Given pairs of x and y values
# x = c(2, 1, 3, 4, 5)
# y = c(3530, 4141, 2277, 9031, 4005) # or f(x)
# est = 3.9
# # # x = c(3, 4.5, 7, 9)
# # # y = c(2.5, 1, 2.5, 0.5)

GaussJordan1 = function(result, n) {
  augcoeffmatrix = result
  rowNum = nrow(augcoeffmatrix)
  rowNumMinus = rowNum - 1
  colNum = ncol(augcoeffmatrix)
  colNumMinus = colNum - 1
  variables = list()
  
  variables <- c()
  for (i in 1:3) {
    variables <- c(variables, paste("a", i, sep = ""))
    variables <- c(variables, paste("b", i, sep = ""))
    variables <- c(variables, paste("c", i, sep = ""))
  }  
  
  indexMax = 0
  rowIndex= c(0)
  
  # ALGORITHM 3: Gauss-Jordan Elimination
  for (i in 1:rowNum) {
    if (i != rowNum) {
      # finding pivot row
      augcoeffmatrix = as.numeric(unlist(augcoeffmatrix))
      augcoeffmatrix = matrix(as.numeric(augcoeffmatrix), ncol = colNum)
      rowMax = max(abs(augcoeffmatrix[i:rowNum, i]))
      
      # row index where the maximum value is located
      rowIndex = which(abs(augcoeffmatrix[i:rowNum, i]) == rowMax)[1] + i - 1
      pivotRow = rowIndex
      indexSwap = i
      
      # check if a solution exists
      if (!is.na(augcoeffmatrix[pivotRow, i]) && augcoeffmatrix[pivotRow, i] == 0) {
        # print("No unique solution exists.")
        cat("NA")
        return (NA)
      }
      else { # partial pivoting
        temp = augcoeffmatrix[pivotRow, ]
        augcoeffmatrix[pivotRow, ] = augcoeffmatrix[indexSwap, ]
        augcoeffmatrix[indexSwap, ] = temp
      }
    }
    print(augcoeffmatrix)
    # a[i,] : a[i,] / a[i, i]
    augcoeffmatrix[i, ] = augcoeffmatrix[i, ] / augcoeffmatrix[i, i]
    
    for (j in 1:rowNum) {
      if (i == j) {
        next # ito ba meaning nung continue
      }
      # normalized row and a[j,] = a[j,] - normalized row
      normalizedRow = augcoeffmatrix[j, i] * augcoeffmatrix[i, ]
      augcoeffmatrix[j, ] = augcoeffmatrix[j, ] - normalizedRow
    }
  }
  
  has_na = any(is.na(augcoeffmatrix)) # if may NA na value sa matrix, return NA
  if (any(is.na(augcoeffmatrix)) || any(is.nan(augcoeffmatrix))) {
    cat("NA")
    return(NA)
  }
  
  # Solutions for the system of equations
  soln = c(as.vector(augcoeffmatrix[, ncol(augcoeffmatrix)])) # get only the last column
  identity = augcoeffmatrix
  print(augcoeffmatrix)
  
  # Create a matrix with 3 columns
  solution_matrix <- matrix(soln, nrow = n, ncol = 3, byrow = TRUE)
  
  # Print the resulting matrix
  # print(solution_matrix)
  
  gaussjordanMethod = list(variables = variables, augcoeffmatrix = result, solution = solution_matrix)
  
  return(gaussjordanMethod) 
}

QuadraticSplineInterpolation = function(unsorted_x, unsorted_y, est) {
  num_datapoints <- length(unsorted_x)
  n <- num_datapoints - 1
  num_equations <- 3 * n
  row = 1
  stagger = 0
  
  sorting_indices <- order(unsorted_x)
  
  # Rearrange x and y
  x <- unsorted_x[sorting_indices]
  y <- unsorted_y[sorting_indices]
  
  # Print the sorted values
  cat("Sorted x:", x, "\n")
  cat("Sorted y:", y, "\n")
  
  augmented_matrix <- matrix(0, nrow = num_equations, ncol = num_equations+1)
  
  augmented_matrix[num_equations, 1] = 1
  augmented_matrix[num_equations, num_equations] = 0
  
  # condition 1: 
  prev_start_col <- 1
  prev_end_col <- 3
  # while (counter <= 2) {
  for (i in 2:n) {
    a_prev <- (x[i-1])^2
    b_prev <- (x[i-1])
    c_prev <- 1
    yfx_prev = y[i-1]
    
    # print("count1-----")
    # cat("row", row, "\n")
    # print("start")
    # print(prev_start_col)
    # print("end")
    # print(prev_end_col)
    # print(a_prev)
    # print(b_prev)
    # print(c_prev)
    # print(yfx_prev)
    
    # input to matrix. when row is even or when row = 1, follow prev_startcol and prev end col
    # when row is odd, stagger integers by 3 columns
    
    # Check if row is even or the first row
    if (row %% 2 == 0 || row == 1) {
      augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_prev, b_prev, c_prev)
      augmented_matrix[row, num_equations + 1] <- yfx_prev
      print("follow")
      stagger = 0 
    } else {
      augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_prev, b_prev, c_prev)
      augmented_matrix[row, num_equations + 1] <- yfx_prev
      print("Stagger=---")
      stagger = 1
    }
    
    row = row + 1

    cat("i = ", i, "\n")
    a_curr <- (x[i])^2
    b_curr <- (x[i])
    c_curr <- 1
    yfx_curr = y[i]

    print("count2----")
    cat("row", row, "\n")
    print(a_curr)
    print(b_curr)
    print(c_curr)
    print(yfx_curr)

    print("start")

    print(prev_start_col)
    print("end")
    print(prev_end_col)
    
    if (row %% 2 == 0 || row == 1) {
      augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_curr, b_curr, c_curr)
      augmented_matrix[row, num_equations + 1] <- yfx_curr
      print("follow")
      stagger = 0
      
    } else {
      # Stagger integers by 3 columns for odd rows
      augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_curr, b_curr, c_curr)
      augmented_matrix[row, num_equations + 1] <- yfx_curr
      print("Stagger=---")
      stagger = 1
    }
    if (stagger == 0) {
      prev_start_col <- prev_start_col + 3
      prev_end_col <- (prev_end_col + 3)
    }
    
    stagger = 0
    row = row+1
    print("new iteration")
  }
  
  # condition 2:
  a_prev <- (x[n])^2
  b_prev <- (x[n])
  c_prev <- 1
  yfx_prev = y[n]
  
  if (row %% 2 == 0 || row == 1) {
    augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_prev, b_prev, c_prev)
    augmented_matrix[row, num_equations + 1] <- yfx_prev
    print("follow")
    stagger = 0 
  } else {
    augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_prev, b_prev, c_prev)
    augmented_matrix[row, num_equations + 1] <- yfx_prev
    print("Stagger=---")
    stagger = 1
  }
  
  print("row")
  row = row + 1
  
  cat("i = ", i, "\n")
  a_curr <- (x[num_datapoints])^2
  b_curr <- (x[num_datapoints])
  c_curr <- 1
  yfx_curr = y[num_datapoints]
  
  if (row %% 2 == 0 || row == 1) {
    augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_curr, b_curr, c_curr)
    augmented_matrix[row, num_equations + 1] <- yfx_curr
    print("follow")
    stagger = 0
    
  } else {
    # Stagger integers by 3 columns for odd rows
    augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_curr, b_curr, c_curr)
    augmented_matrix[row, num_equations + 1] <- yfx_curr
    print("Stagger=---")
    stagger = 1
  }
  if (stagger == 0) {
    prev_start_col <- prev_start_col + 3
    prev_end_col <- (prev_end_col + 3)
  }
  
  newrow = 0
  row = row+1
  print("row")
  print(row)
  
  prev_start_col <- 1
  prev_end_col <- 6
  
  # condition 3
  for (i in 2:n) {
    a_prev <- 2*(x[i])
    b_prev <- 1
    c_prev <- 0
    yfx_prev = 0
    
    print("count1-----")
    cat("row", row, "\n")
    print("start")
    print(prev_start_col)
    print("end")
    print(prev_end_col)
    print(a_prev)
    print(b_prev)
    print(c_prev)
    print(yfx_prev)

    cat("i = ", i, "\n")
    a_curr <- -2*(x[i])
    b_curr <- -1
    c_curr <- 0
    yfx_curr = 0

    print("count2----")
    cat("row", row, "\n")
    print(a_curr)
    print(b_curr)
    print(c_curr)
    print(yfx_curr)

    print("start")

    print(prev_start_col)
    print("end")
    print(prev_end_col)
    
    if (newrow == 0) {
      augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_prev, b_prev, c_prev, a_curr, b_curr, c_curr)
      augmented_matrix[row, num_equations + 1] <- yfx_curr
      print("follow")
    } else {
      # Stagger integers by 3 columns for odd rows
      augmented_matrix[row, seq(prev_start_col, prev_end_col)] <- c(a_prev, b_prev, c_prev, a_curr, b_curr, c_curr)
      augmented_matrix[row, num_equations + 1] <- yfx_curr
      print("Stagger=---")
    }
    newrow = 1
    if (newrow == 1) {
      prev_start_col <- prev_end_col - 2
      prev_end_col <- (prev_end_col + 3)
    }
    row = row+1
    print("new iteration")
  }
  
  print(augmented_matrix)
  solvedmatrix = GaussJordan1(augmented_matrix, n)
  
  num_rows <- nrow(solvedmatrix$solution)
  
  polynomial_strings <- list()
  
  for (i in 1:num_rows) {
    # Get the coefficients for the current row
    coefficients <- solvedmatrix$solution[i, ]
    
    # Construct the polynomial string for the current row
    polynomial_string <- paste0("function (x) ", coefficients[1], "*x^2 + ", coefficients[2], "*x + ", coefficients[3])
    
    # Add the polynomial string to the list
    polynomial_strings[[i]] <- polynomial_string
  }
  
  for (i in 1:num_rows) {
    print(polynomial_strings[[i]])
  }
  solution_functions = c()
  for (i in 1:num_rows) {
    polynomial_function <- eval(parse(text = polynomial_strings[i]))
    # Add the function to the list
    solution_functions[[i]] <- polynomial_function
  }
  print(solution_functions)
  
  interval_index <- findInterval(est, x)
  # Returns the indices of the intervals to which the elements of est belong
  
  # Choose the appropriate function for the interval and substitute estimate to it
              # f[[index]](est)
  estimate <- solution_functions[[interval_index]](est)
  cat("Chosen function: ")
  print(solution_functions[[interval_index]])
  
  low <- x[interval_index]
  up <- x[interval_index + 1]
    
  # print(estimate)
  
  # print(x)
  # print(y)
  quadspline <- list(matrix = solvedmatrix, solution = solvedmatrix$solution, equations = polynomial_strings, functions = solution_functions, approximation = estimate, chosenfxn = solution_functions[[interval_index]], x_val = x, y_val = y, intervalInd = interval_index, upperbound = up, lowerbound = low)
  # print(quadspline)
  # print(quadspline$equations)
  # print(quadspline$functions[[2]](2))
  # print(quadspline)
  return(quadspline)
}

# mat = QuadraticSplineInterpolation(x, y, est)