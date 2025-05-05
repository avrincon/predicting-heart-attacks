plot_contour <- function(prediction_grid, model_data, user_data) {
  ggplot() +
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
    ) +
    # Add points for observed data
    geom_point(
      data = model_data,
      aes(x = log_troponin, y = log_ck_mb, shape = result, color = result),
      alpha = 0.7,
      size = 3
    ) +
    # add example user selected values
    geom_point(
      data = user_data,
      aes(x = log_troponin, y = log_ck_mb),
      color = "black",
      shape = 15,
      # alpha = 0.7,
      size = 6
    ) +
    scale_x_continuous(
      "Troponin (ng/mL)",
      breaks = log(troponin_x_breaks_lab),
      labels = troponin_x_breaks_lab
    ) +
    scale_y_continuous(
      "CK-MB (ng/mL)",
      breaks = log(ck_mb_x_breaks_lab),
      labels = ck_mb_x_breaks_lab
    ) +
    scale_color_manual(
      values = c("negative" = "#2E9093", "positive" = "#DD4124"),
      name = "Heart Attack"
    ) +
    scale_shape_manual(
      values = c("negative" = 16, "positive" = 17),
      name = "Heart Attack"
    ) +
    scale_fill_viridis_d(
      option = "plasma",
      name = "Probability",
      labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                 "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
    ) +
    labs(
      title = "Heart Attack Probability by Troponin and CK-MB Levels",
      subtitle = "White line shows decision boundary (50% probability)",
      caption = "All other variables held at their mean values"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold")
    )
}