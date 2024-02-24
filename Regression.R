# a = c(5, 2, 6, 3)
# b = c(3, 21, 65, 5)
# 
# data = list(x = a, y = b)
# 
# mat = PolynomialRegression(2, data)
# print(mat)

GaussJordan = function(result, n) {
  print("GAUSS JORDAN REGRESSION")
  augcoeffmatrix = result
  rowNum = nrow(augcoeffmatrix)
  rowNumMinus = rowNum - 1
  colNum = ncol(augcoeffmatrix)
  colNumMinus = colNum - 1
  variables = list()
  
  for (i in 1:colNumMinus) {
    variables = append(variables, paste("x^", i, sep = ""))
  }
  print(variables)
  
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
  
  has_na = any(is.na(augcoeffmatrix))  # if may NA na value sa matrix, return NA
  if (has_na || any(is.nan(augcoeffmatrix))) {
    return(NA)
  }
  
  # solutions for the system of equations
  soln = as.vector(augcoeffmatrix[, ncol(augcoeffmatrix)]) # get only the last column
  print("SOLUTION\n")
  print(soln)
  identity = augcoeffmatrix
  
  gaussjordanMethod = list(variables = variables, augcoeffmatrix = result, solution = soln)
  return(gaussjordanMethod) 
}

PolynomialRegression = function(order, data) {
  # print(data$x)
  n = 1
  # print(data$y)
  num_var_x = length(data$x)
  num_var_y = length(data$y)
  
  if (num_var_x != num_var_y) {
    return(NA)
  }
  if (order > num_var_x) {
    print("The highest degree of the polynomial in polynomial regression should be n−1, given n datapoints.")
    return(NA)
  }
  
  list_matrix = matrix(nrow = num_var_x, ncol = 2)
  list_matrix[, 1] = data$x # vectors
  list_matrix[, 2] = data$y
  colnames(list_matrix) = c("xi", "yi") 
  rows = order+1
  cols = order+2
  xi_i_list = vector("list", length = rows)
  sum_xi_i_list = vector("numeric", length = rows + 1) 
  sum_xi_yi_i_list = vector("numeric", length = rows-1)
  final_sum_xi_yi_i_list = vector("numeric")
  augcoeffmatrix = matrix(0, nrow = (order+1), ncol = (order+2))
  
  sum_xi = sum(data$x)
  sum_yi = sum(data$y)
  ave_xi = sum_xi/length(data$x)
  ave_yi = sum_yi/length(data$y)
  
  for (i in 1:(rows+1)) {
    var_name = paste("xi_", (i), sep = "")  # generate variable namee
    assign(var_name, data$x^(i))  # create new variable with the squared value
    # cat(var_name, ": ", get(var_name), "\n") # xi_squared, cubed, etc.
    
    var_name1 = paste("sum_xi_", (i), sep = "")  
    assign(var_name1, sum(get(var_name)))
    # cat(var_name1, ": ", get(var_name1), "\n")  # sum of xi_squared, cubed, etc.
    
    xi_i_list[[i]] = get(var_name) # contains vector xi^m 
    sum_xi_i_list[[i]] = get(var_name1) # contains vector xi^m 
  }
  
  for (j in 1:(rows-1)) {
    var_name2 = paste("sum_mult_yi_xi_", (j), sep = "")  
    assign(var_name2, sum(xi_i_list[[j]] * data$y))
    # cat(var_name2, ": ", get(var_name2), "\n")  # sum of xi_squared, cubed, etc.
    
    sum_xi_yi_i_list[[j]] = get(var_name2) # contains vector sum of xi^m * yi 
  }
  
  final_sum_xi_yi_i_list = append(final_sum_xi_yi_i_list, sum_yi)
  final_sum_xi_yi_i_list = append(final_sum_xi_yi_i_list, sum_xi_yi_i_list)
  
  for (i in 1:(order+1)) { # lagay values sa matrix
    for (j in 1:(order+1)) {
      augcoeffmatrix[i,j] = sum((data$x)^(i+j-2)) # if i = 1 and j = 1, minus 2 ay 0 for the first iteration which is tama
    }
  }
  
  augcoeffmatrix[,ncol(augcoeffmatrix)] = matrix(final_sum_xi_yi_i_list)
  
  result = GaussJordan(augcoeffmatrix, n)
  
  polynomial_string = "function(x) "

  # loop para magawa polynomial string
  for (i in 1:length(result$solution)) {
    if (i == 1) {
      polynomial_string = paste(polynomial_string, result$solution[i], " + ", sep = "")
    } else {
      polynomial_string = paste(polynomial_string, result$solution[i], " * x^", i - 1, " + ", sep = "")
    }
  }

  # alisin trailing + and space
  polynomial_string = substr(polynomial_string, 1, nchar(polynomial_string) - 3)
  #
  # print(polynomial_string)
  #
  polynomial_fxn = eval(parse(text = polynomial_string))

  # cat("Degree = ", order, "\n\n")
  polynomialregression = list(augcoeffmatrixREG = result$augcoeffmatrix, coefficients = result$solution, polynomial_string = polynomial_string, polynomial_function = polynomial_fxn)

  print(polynomialregression)
  return(polynomialregression)
}
