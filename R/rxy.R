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
#'
#' @export



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

library(ggplot2)
library(dplyr)

plot_gg <- function(x) {
  x$df <- mutate(x$df, r_abs = abs(r), sign = ifelse(r > 0, "Positive", "Negative"))

  ggplot(x$df, aes(x = r_abs, y = reorder(variable, r_abs), color = sign)) +
    geom_segment(aes(x = 0, xend = r_abs, y = reorder(variable, r_abs), yend = reorder(variable, r_abs)), size = 0.5, color = "gray") +
    geom_point(size = 4, aes(fill = sign), shape = 21) +
    scale_color_manual(values = c("black", "black")) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Absolute Correlation Coefficient", y = "Variables") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())
}
