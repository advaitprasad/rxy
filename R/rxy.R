#' Correlation package that conducts tests between variables in a data set.
#'
#' This function computes correlation coefficients and their associated p-values
#' between a dependent variable and specified independent variables.
#'
#' @param data The data set to perform correlation tests on.
#' @param y The dependent variable.
#' @param x The independent variable.
#' @return A list containing information about the correlation tests, p-values
#' and significance codes.
#'
#' @examples
#' data(mtcars)
#' result <- ryx(mtcars, y = "mpg", x = c("disp", "hp", "wt"))
#' print(result)
#' plot(result)
#' summary(result)



ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}



# Print function
print.ryx <- function(x) {
  cat("Correlations of", x$y, "with\n")
  print(x$df[, c("variable", "r", "p", "sigif")])
}


# Summary function

summary.ryx <- function(object) {
  cat("Correlating", object$y, "with", paste(object$x, collapse = " "), "\n")

  # Calculate median absolute correlation and count significant variables
  median_abs_corr <- median(abs(object$df$r))
  significant_vars <- sum(object$df$p < 0.05)

  cat("The median absolute correlation was", round(median_abs_corr, 3),
      "with a range from", round(min(object$df$r), 3), "to", round(max(object$df$r), 3), "\n")

  cat(significant_vars, "out of", nrow(object$df), "variables were significant at the p < 0.05 level.\n")
}

# Plot function

# Define plot function for "ryx" class
plot.ryx <- function(x, ...) {
  rev_df <- x$df[nrow(x$df):1, ]


  plot(NULL, xlim = c(0, 1), ylim = c(0, nrow(x$df) + 1),
       xlab = "Absolute Correlation Coefficient", ylab = "Variables", yaxt = "n", ...)


  abline(h = 1:nrow(x$df), col = "lightgray", lty = "dotted")

  # Plot lines for correlations
  for (i in 1:nrow(rev_df)) {
    # Define x-coordinate for lines
    x_coords <- c(0, abs(rev_df$r[i]))

    # Plot lines
    lines(x_coords, c(i, i), col = "black", lwd = 2)

    # Plot circles at the end of lines with color based on correlation sign
    circle_col <- ifelse(rev_df$r[i] > 0, "blue", "red")
    points(abs(rev_df$r[i]), i, col = circle_col, pch = 19, cex = 1.5)
  }

  # Add labels for variables on the y-axis
  axis(2, at = 1:nrow(x$df), labels = rev_df$variable, las = 1, col.axis = "black", ...)

  # Add legend for positive and negative correlations
  legend("bottomright", legend = c("Negative", "Positive"), pch = 19,
         col = c("red", "blue"), bty = "n", cex = 1.2)
}
