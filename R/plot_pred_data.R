#' Plot model predictions
#'
#' @param pred_data A data frame of new data to make model predictions on
#' @param mod_data A data frame of the original data used to fit the model
#' @param x_var Name of the x-axis variable
#' @param x_lab Label for x-axis
#' @param x_breaks_original Numeric vector of breaks for x variable in original
#' @param x_int Value of x-intercept for vline
#'   non-logged scale
#'
#' @returns A ggplot object
plot_pred_data <- function(pred_data, 
                           mod_data, 
                           x_var, 
                           x_lab, 
                           x_breaks_original,
                           x_int,
                           title,
                           show_data = TRUE) {
  x_var <- rlang::sym(x_var)
  
  p <- ggplot(pred_data, aes(x = !!x_var, y = predicted_prob)) +
    geom_line(color = "#0f85a0", linewidth = 1) +
    scale_x_continuous(
      x_lab,
      breaks = log(x_breaks_original),
      labels = x_breaks_original
    ) +
    geom_vline(
      xintercept = log(x_int), 
      linetype = "dashed", 
      color = "#dd4124",
      linewidth = 1
    ) +
    labs(
      title = title, 
      y = "Probability of Positive Result"
    ) +
    geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray") +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      plot.title = element_text(face = "bold")
    )
  
  # Conditionally add observed data points
  if (show_data) {
    p <- p +
      geom_jitter(
        data = mod_data, 
        aes(x = !!x_var, y = ifelse(result == "positive", 1, 0)), 
        height = 0.02, alpha = 0.3, color = "#edd746"
      )
  }
  
  return(p)
}