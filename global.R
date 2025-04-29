library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)

# for loading support functions during development
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

logistic_model <- readRDS("model/logistic_model.rds")
model_data <- readRDS("model/model_data.rds")