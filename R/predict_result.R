#' Model Predictions
#' 
#' Get logistic model predictions on new data
#'
#' @param ck_mb Creatine kinase-MB (ng/mL)
#' @param troponin Troponin (ng/mL)
#' @param model Logistic model object
#' @param model_data Data used to fit model
#'
#' @returns A tibble with predicted results based on data and logistic model
predict_result <- function(
    ck_mb,
    troponin,
    model,
    model_data
) {
  
  pred_data <- 
    tibble(
      age = mean(model_data$age),
      gender = levels(model_data$gender)[1],
      heart_rate = mean(model_data$heart_rate),
      systolic_blood_pressure = mean(model_data$systolic_blood_pressure),
      diastolic_blood_pressure = mean(model_data$diastolic_blood_pressure),
      blood_sugar = mean(model_data$blood_sugar),
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