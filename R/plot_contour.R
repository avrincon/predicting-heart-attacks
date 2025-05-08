#' Create a contour plot of heart attack probability
#'
#' This function creates a contour plot showing the probability of heart attack
#' based on troponin and CK-MB levels. It visualizes the prediction surface from
#' a logistic regression model with filled contours representing probability
#' ranges.
#'
#' @param prediction_grid A data frame containing the prediction surface with
#'   columns: log_troponin, log_ck_mb, and probability
#' @param model_data A data frame of the original data used to fit the model
#' @param user_data A data frame with a single row containing the user's
#'   selected values: log_troponin and log_ck_mb
#' @param show_data Logical; whether to display the original data points on the
#'   plot (default: TRUE)
#'
#' @return A ggplot object with the contour plot visualization
#'
#' @examples
#' user_point <- tibble(log_troponin = log(0.01), log_ck_mb = log(1))
#' plot_contour(prediction_grid, model_data, user_point)
plot_contour <- function(prediction_grid, 
                         model_data, 
                         user_data, 
                         show_data = TRUE) {
  model_data <- 
    model_data |> 
    mutate(across(result, stringr::str_to_title)) 
  
  # Add user data point with aes() mapping to include in legend
  # Add "Current Patient" to user_data for legend entry
  user_data$type <- "Current Patient"
  
  p <- ggplot() +
    # Add filled contours with continuous scale
    geom_contour_filled(
      data = prediction_grid,
      aes(x = log_troponin, y = log_ck_mb, z = probability),
      breaks = seq(0, 1, by = 0.1)
    ) +
    # Add decision boundary (probability = 0.5)
    geom_contour(
      data = prediction_grid,
      aes(x = log_troponin, y = log_ck_mb, z = probability),
      breaks = 0.5,
      color = "white",
      linewidth = 1
    )

  # Conditionally add observed data points
  if (show_data) {
    p <- p +
      # Add points for observed data
      geom_point(
        data = model_data,
        aes(x = log_troponin, y = log_ck_mb, shape = result, color = result),
        alpha = 0.7,
        size = 3
      )
  } else {
    # When not showing data, add dummy points that are invisible but maintain legend structure
    p <- p +
      geom_point(
        data = data.frame(
          log_troponin = log(0.1),
          log_ck_mb = log(5),
          result = c("Positive", "Negative")
        ),
        aes(x = log_troponin, y = log_ck_mb, shape = result, color = result),
        alpha = 0,  # Make them invisible
        size = 0
      )
  }
 

  # Rest of plot formatting
  p <- p +
    # Add user data point with aesthetic mapping for legend entry
    geom_point(
      data = user_data,
      aes(x = log_troponin, y = log_ck_mb, shape = type, color = type),
      # color = "black",
      size = 6
    ) +
    scale_x_continuous(
      "Troponin (ng/mL)",
      breaks = log(troponin_x_breaks_lab),
      labels = troponin_x_breaks_lab
    ) +
    scale_y_continuous(
      "Creatine kinase-MB (ng/mL)",
      breaks = log(ck_mb_x_breaks_lab),
      labels = ck_mb_x_breaks_lab
    ) +
    scale_fill_viridis_d(
      option = "plasma",
      name = "Probability",
      labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
                 "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
    ) +
    scale_shape_manual(
      values = c("Negative" = 16, "Positive" = 17, "Current Patient" = 15),
      name = "Data Points"
    ) +
    scale_color_manual(
      values = c("Negative" = "#2E9093", 
                 "Positive" = "#DD4124", 
                 "Current Patient" = "black"),
      name = "Data Points"
    ) +
    labs(
      title = "Heart Attack Probability by Troponin and CK-MB Levels",
      subtitle = "White line shows decision boundary (50% probability)",
      caption = "Other variables were at their mean or reference values."
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 16),
      legend.position = "right",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold")
    )
  return(p)
}
