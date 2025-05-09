#' Plot model predictions
#'
#' @description Creates a line plot visualizing model predictions along a
#' continuous variable, with optional data points overlay. Primarily used for
#' visualizing the relationship between a predictor variable and the predicted
#' probability of a positive result in a logistic regression model.
#'
#' @param pred_data A data frame containing new data with model predictions.
#'   Must include the x-variable and a 'predicted_prob' column.
#' @param mod_data A data frame of the original data used to fit the model. Used
#'   for plotting the actual observed data points when show_data=TRUE.
#' @param x_var Character string specifying the name of the x-axis variable in
#'   pred_data.
#' @param x_lab Character string for the x-axis label.
#' @param x_breaks_original Numeric vector of breaks for x variable in original
#'   scale. These will be log-transformed internally for proper placement on the
#'   log scale.
#' @param x_int Numeric value for the x-intercept where a vertical reference
#'   line should be drawn. Will be log-transformed internally to match the
#'   x-axis scale.
#' @param title Character string for the plot title.
#' @param show_data Logical; whether to display the original data points
#'   (default: TRUE).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Prepare prediction data
#' pred_data <- predict_result(
#'   ck_mb = log(5),
#'   troponin = log_troponin_seq,  # sequence of troponin values
#'   model = logistic_model,
#'   model_data = model_data
#' )
#'
#' plot_pred_data(
#'   pred_data = pred_data,
#'   mod_data = model_data,
#'   x_var = "log_troponin",
#'   x_lab = "Troponin (ng/mL)",
#'   x_breaks_original = c(0.001, 0.01, 0.1, 1, 10),
#'   x_int = 0.01,
#'   title = "Effect of Troponin on Heart Attack Probability",
#'   show_data = TRUE
#' )
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