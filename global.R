library(shiny)
library(bslib)
library(bsicons)
library(shinyFeedback)
library(shinyWidgets)
library(htmltools)
library(markdown)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# for loading support functions during development
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

logistic_model <- readRDS("model/logistic_model.rds")
reduced_model <- readRDS("model/reduced_model.rds")
model_data <- readRDS("model/model_data.rds")
log_troponin_seq <- readRDS("model/log_troponin_seq.rds")
log_ckmb_seq <- readRDS("model/log_ckmb_seq.rds")
prediction_grid <- readRDS("model/prediction_grid.rds")