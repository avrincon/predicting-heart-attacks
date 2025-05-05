#' Model Predictions
#' 
#' Get logistic model predictions on new data
#'
#' @param age Age in years
#' @param gender Gender. One of "Female" or "Male".
#' @param heart_rate Heart Rate in bpm 
#' @param systolic_blood_pressure Systolic blood pressure
#' @param diastolic_blood_pressure Diastolic blood pressure
#' @param blood_sugar Blood sugar levels
#' @param ck_mb Creatine kinase-MB (ng/mL)
#' @param troponin Troponin (ng/mL)
#' @param logistic_model Logistic model object
#'
#' @returns A tibble with predicted results based on data and logistic model
predict_result <- function(
    # age,
    # gender,
    # heart_rate,
    # systolic_blood_pressure,
    # diastolic_blood_pressure,
    # blood_sugar,
    ck_mb,
    troponin,
    model
) {
  
  pred_data <- 
    tibble(
      # age = age,
      # gender = gender,
      # heart_rate = heart_rate,
      # systolic_blood_pressure = systolic_blood_pressure,
      # diastolic_blood_pressure = diastolic_blood_pressure,
      # blood_sugar = blood_sugar,
      log_ck_mb = ck_mb,
      log_troponin = troponin
    )
  
  pred_data$predicted_prob <- 
    predict(
      model,
      newdata = pred_data, 
      type = "response"
    )
  
  pred_data
}