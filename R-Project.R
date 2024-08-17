#211805048-Recep Sami Özdemir, 211805054-Cemre Polat
#Please change a directory
setwd("...//Project 1//211805048_211805054_project1")
datas <- data.frame(read.table("DatasetNA.txt"))
colNames <- c("IdNo", "Group", "Gender", "Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")
colnames(datas) <- colNames

#This function callculates minimum value of given data column
minVal <- function(column) {
  minVal <- Inf
  
  for (i in 1:length(column)) {
    if (!is.na(column[i]) && column[i] < minVal) {
      minVal <- column[i]
    }
  }
  return(as.double(minVal))
}
#This function callculates maximum value of given data column
maxVal <- function(column) { 
  maxVar <- -Inf
  
  for (i in 1:length(column)) {
    if (!is.na(column[i]) && column[i] > maxVar) {
      maxVar <- column[i]
    }
  }
  return(as.double(maxVar))
}
#This function takes muliple data columns and returns requested value by min_max parameter
rangeFunMulti <- function(..., min_max = "min") {
  all_data <- unlist(list(...), use.names = FALSE)
  all_data <- all_data[!is.na(all_data)]
  
  if (length(all_data) == 0) {
    warning("All values are NA, returning Inf.")
    return(Inf)
  }
  
  if (min_max == "min") {
    return(min(all_data))
  } else if (min_max == "max") {
    return(max(all_data))
  } else {
    stop("Invalid value for min_max parameter. Please specify 'min' or 'max'.")
  }
}
#This function gets a value and data column and calculates frequency and separet bins by given value 
freqAndBinsFun <- function(x, data_column) {
  bins <- seq(min(data_column, na.rm = TRUE), max(data_column, na.rm = TRUE), length.out = x)
  freq_value <- cut(data_column, bins, include.lowest = TRUE)
  freq_counts <- as.vector(table(freq_value))
  
  return(list(bins = bins, freq_counts = freq_counts))
}
#This function calculates Q1, Q3 and Median value of given data column
medianAndQuartersMulti <- function(...) {
  data <- unlist(list(...))
  sortedData <- sort(data)
  lengthData <- length(data)
  medianNumber <- 0
  Q1 <- 0
  Q3 <- 0
  
  if ((lengthData %% 2) == 0) {
    medianNumber <- (sortedData[lengthData / 2] + sortedData[lengthData / 2 + 1]) / 2
  } else {
    medianNumber <- sortedData[(lengthData + 1) / 2]
  }
  
  if ((lengthData %% 4) == 0) {
    Q1 <- (sortedData[lengthData / 4] + sortedData[(lengthData + 1) / 4]) / 2
    Q3 <- (sortedData[(lengthData * 3 / 4)] + sortedData[(lengthData * 3 / 4) + 1]) / 2
  } else {
    Q1 <- sortedData[(lengthData + 2) / 4]
    Q3 <- sortedData[((lengthData * 3) + 2) / 4]
  }

  return(list(medianNumber = medianNumber, Q1 = Q1, Q3 = Q3))
}
# This function calculates how many members in given dataset by gender with observationGender(datas$Gender)
observationGender <- function(x) {
  FemaleCount <- 0
  MaleCount <- 0
  
  for(i in 1:length(x)) {
    if(datas$Gender[i] == "Female") {
      FemaleCount <- FemaleCount + 1
    } else if (datas$Gender[i] == "Male") {
      MaleCount <- MaleCount + 1
    } else{
      stop("There is a undefined gender in dataset")
    }
  }
  return(list(Female = FemaleCount, Male = MaleCount))
}
#this funciton calculates how many member in given dataset by group variable with observationGroup(datas$Group)
observationGroup <- function(x) {
  group1Count <- 0
  group2Count <- 0
  group3Count <- 0
  group4Count <- 0
  
  for(i in 1:length(x)) {
    if(datas$Group[i] == "Group1") {
      group1Count <- group1Count + 1
    } else if (datas$Group[i] == "Group2") {
      group2Count <- group2Count + 1
    } else if (datas$Group[i] == "Group3") {
      group3Count <- group3Count + 1
    } else if (datas$Group[i] == "Group4"){
      group4Count <- group4Count + 1
    } else {
      stop("There was a undefined group in dataset!")
    }
  }
  return(
    list(Group1 = group1Count, Group2 = group2Count, Group3 = group3Count, Group4 = group4Count))
}
#This function display two graph side by side horizontally
horizontalPlots <- function(x, y) {
  par(mfrow = c(1,2))
  x
  y
}
#This function display two graph side by side vertically
verticalPlots <- function(x, y) {
  par(mfrow = c(2,1))
  x
  y
}
#This function draws bar plot for specifically by gender
barplotGender <- function(b) {
  x <- c("female", "male")
  b <- observationGender(b)
  varNumeric <- as.numeric(b)
  y <- c(varNumeric)
  
  bar_width <- 0.8
  xlim <- c(0, length(x) + 1)
  ylim <- c(0, max(y, na.rm = TRUE) * 1.2)
  
  plot(1, type = "n", xlab = "Gender", ylab = "Values", xaxt = "n", xlim = xlim, ylim = ylim, axes = FALSE)
 
  for (i in seq_along(x)) {
    polygon(
      x = c(i - bar_width / 2, i + bar_width / 2, i + bar_width / 2, i - bar_width / 2),
      y = c(0, 0, y[i], y[i]),
      col = c("pink", "lightblue")[i],
      border = c("purple", "dodgerblue4")[i]
    )
  }
  
  text(x = seq_along(x), y = y/2, labels = paste(y), pos = 3, cex = 0.8)
  
  axis(1, at = seq_along(x), labels = x, lwd = 0)
  axis(2)
  
  title(main = "Gender Barplot")
}
#This function draws bar plot for specifically by group variable
barplotGroup <- function(b) {
  x <- c("Group1", "Group2","Group3","Group4")
  b <- observationGroup(b)
  varNumeric <- as.numeric(b)
  y <- c(varNumeric)
  
  bar_width <- 0.8
  xlim <- c(0, length(x) + 1)
  ylim <- c(0, max(y, na.rm = TRUE) * 1.2)
  
  plot(1, type = "n", xlab = "Group", ylab = "Values", xaxt = "n", xlim = xlim, ylim = ylim, axes = FALSE)
  
  for (i in seq_along(x)) {
    polygon(
      x = c(i - bar_width / 2, i + bar_width / 2, i + bar_width / 2, i - bar_width / 2),
      y = c(0, 0, y[i], y[i]),
      col = c("chartreuse2", "darkgoldenrod2", "deeppink2", "cyan2")[i],
      border = "gray9"
    )
  }
  
  text(x = seq_along(x), y = y/2, labels = paste(y), pos = 3, cex = 0.8)
  
  axis(1, at = seq_along(x), labels = x, lwd = 0)
  axis(2)
  
  title(main = "Groups Barplot")
}
# This function can gets multiple data columns and draws them in a same graphic. 
histogramFun <- function(...) {
  data <- list(...)
  num_columns <- length(data)
  rowNumber <- 11
  max_freq_count_length <- max(sapply(data, function(x) length(freqAndBinsFun(x = rowNumber, data_column = x)$freq_counts)))
  bins <- matrix(data = 0, nrow = num_columns, ncol = rowNumber)
  freqs <- matrix(data = 0, nrow = num_columns, ncol = max_freq_count_length)
  
  for (i in 1:num_columns) {
    result <- freqAndBinsFun(x = rowNumber, data_column = data[[i]])
    bins[i, 1:length(result$bins)] <- result$bins
    freq_counts <- result$freq_counts
    freqs[i, 1:length(freq_counts)] <- freq_counts
  } 
  
  minXRange <- rangeFunMulti(data, min_max = "min")
  maxXRange <- rangeFunMulti(data, min_max = "max")
  
  plot(c(minXRange, maxXRange), c(0, max(freqs) * 1.1), type = "n", xlab = "Columns Variables", ylab = "Frequency", main = "Histogram")
  colors <- heat.colors(num_columns)
  
  for (i in 1:num_columns) {
    for (j in 1:max_freq_count_length) {
      bottom_points <- c(bins[i,j],bins[i, j+1],bins[i, j+1],bins[i, j])
      top_points <- c(0,0,freqs[i,j],freqs[i,j])
      polygon(bottom_points, top_points, col = adjustcolor(colors[i], alpha.f = 0.5), border = "purple")
    }
  }
  legend("topright", legend = paste("Var", 1:num_columns), fill = heat.colors(num_columns), bty = "n")
  
}
# This function can gets multiple data columns and draws all box plots in same graphic
boxPlotMulti <- function(...){
  data <- list(...)
  data <- data[!is.na(data)]
  data_names <- lapply(substitute(list(...))[-1], deparse)
  column_names <- sapply(data_names, function(x) tail(strsplit(x, "\\$")[[1]], 1))
  num_columns <- length(data)
  minYRange <- rangeFunMulti(data, min_max = "min")
  maxYRange <- rangeFunMulti(data, min_max = "max")
  boxMins <- matrix(data = 0, nrow = num_columns, ncol = 1)
  boxMaxs <- matrix(data = 0, nrow = num_columns, ncol = 1)
  boxQ1s <- matrix(data = 0, nrow = num_columns, ncol = 1)
  boxQ3s <- matrix(data = 0, nrow = num_columns, ncol = 1)
  boxMedians <- matrix(data = 0, nrow = num_columns, ncol = 1)
  
  for (i in 1:num_columns) {
    boxMins[i, ] <- rangeFunMulti(data[[i]], min_max = "min")
    boxMaxs[i, ] <- rangeFunMulti(data[[i]], min_max = "max")
    boxQ1s[i, ] <- medianAndQuartersMulti(data[[i]])$Q1
    boxQ3s[i, ] <- medianAndQuartersMulti(data[[i]])$Q3
    boxMedians[i, ] <- medianAndQuartersMulti(data[[i]])$medianNumber
  }
  x_lim <- c(1, num_columns +2)
  y_lim <- c(minYRange - 0.5, maxYRange +0.5)
  
  plot(x_lim, y_lim ,xaxt = "n" , xlab = "", ylab = "", main = "Boxplot",)
  
  for (i in 1:num_columns) {
    position <- (i + 1)
    boxSize <- 0.4
    xSize <- c(position - boxSize/2, position + boxSize/2, position + boxSize/2, position - boxSize/2)
    ySize <- c(boxQ1s[i], boxQ1s[i], boxQ3s[i], boxQ3s[i])
    all_colors <- colors()
    color <- adjustcolor(sample(all_colors, 1), alpha.f = 0.8)
    
    polygon(xSize, ySize, col = color, border = "black")
    segments(x0 = position, y0 = boxMins[i] , x1 = position , y1 = boxQ1s[i], col = "black", lwd = boxSize*5)
    segments(x0 = position, y0 = boxMaxs[i] , x1 = position , y1 = boxQ3s[i], col = "black", lwd = boxSize*5)
    segments(x0 = position - boxSize/2, y0 =  boxMedians[i] ,x1 = position + boxSize/2, y1 = boxMedians[i], col = "black", lwd = boxSize*5)
    segments(x0 = position - 0.02, y0 = boxMins[i], x1= position +0.02, y1 = boxMins[i])
    segments(x0 = position - 0.02, y0 = boxMaxs[i], x1= position +0.02, y1 = boxMaxs[i])
    
    axis(1, at = position, labels = column_names[i], tick = FALSE, line = -1)
  }
}
# This function gets two data columns which first one includes categorical variables and second one includes continous variable draws graphic
# It doesn't specified for gender or groups. It doesn't matter which one you gave. ıt draws correctly
boxPlotWithCat <- function(x, y) {
  Id <- list(datas$IdNo)
  Id_length <- length(Id)
  categories <- sort(unique(x))
  categories_length <- length(categories)
  separated_categories_list <- list()
  
  boxMins <- matrix(data = 0, nrow = categories_length, ncol = 1)
  boxMaxs <- matrix(data = 0, nrow = categories_length, ncol = 1)
  boxQ1s <- matrix(data = 0, nrow = categories_length, ncol = 1)
  boxQ3s <- matrix(data = 0, nrow = categories_length, ncol = 1)
  boxMedians <- matrix(data = 0, nrow = categories_length, ncol = 1)
  
  for (i in 1:categories_length) {
    category <- categories[i]
    category_values <- y[x == category]
    category_values <- category_values[!is.na(category_values)]
    
    
    if (any(!is.na(category_values))) {
      separated_categories_list[[category]] <- category_values
    }
    
    boxMins[i] <- rangeFunMulti(category_values, min_max = "min")
    boxMaxs[i] <- rangeFunMulti(category_values, min_max = "max")
    boxQ1s[i] <- medianAndQuartersMulti(category_values)$Q1
    boxQ3s[i] <- medianAndQuartersMulti(category_values)$Q3
    boxMedians[i] <- medianAndQuartersMulti(category_values)$medianNumber
    
  }
  
  minYRange <- minVal(boxMins)
  maxYRange <- maxVal(boxMaxs)
  
  x_lim <- c(1, categories_length +2)
  y_lim <- c(minYRange - 0.5, maxYRange +0.5)
  
  plot(x_lim, y_lim ,xaxt = "n" , xlab = "", ylab = "", main = "Boxplot With Category",)
  
  for (i in 1:categories_length) {
    position <- (i + 1)
    boxSize <- 0.4
    xSize <- c(position - boxSize/2, position + boxSize/2, position + boxSize/2, position - boxSize/2)
    ySize <- c(boxQ1s[i], boxQ1s[i], boxQ3s[i], boxQ3s[i])
    all_colors <- colors()
    color <- adjustcolor(sample(all_colors, 1), alpha.f = 0.8)
    
    polygon(xSize, ySize, col = color, border = "black")
    segments(x0 = position, y0 = boxMins[i] , x1 = position , y1 = boxQ1s[i], col = "black", lwd = boxSize*5)
    segments(x0 = position, y0 = boxMaxs[i] , x1 = position , y1 = boxQ3s[i], col = "black", lwd = boxSize*5)
    segments(x0 = position - boxSize/2, y0 =  boxMedians[i] ,x1 = position + boxSize/2, y1 = boxMedians[i], col = "black", lwd = boxSize*5)
    segments(x0 = position - 0.02, y0 = boxMins[i], x1= position +0.02, y1 = boxMins[i])
    segments(x0 = position - 0.02, y0 = boxMaxs[i], x1= position +0.02, y1 = boxMaxs[i])
    
    axis(1, at = position, labels = categories[i], tick = FALSE, line = -1)
    
  }
}