rm(list = ls())

library(readr)
library(forecast)

lifeSatisfaction <- read_csv("data/Life Satisfaction/life_satisfaction_data.csv")
affect <- read_csv("data/Affect on Social Media/affect_data.csv")
politicalIdeology <- read_csv("data/Political Ideology/political_ideology_data.csv")
politicalPolarization <- read_csv("data/Political Polarization/political_polarization_data.csv")

# Life satisfaction
mod_lifeSatisfaction <- auto.arima(lifeSatisfaction[,2], seasonal = FALSE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_lifeSatisfaction)
forecast_lifeSatisfaction <- as.vector(forecast(mod_lifeSatisfaction, h = 12)$mean)

# Affect
# negativeAffect
mod_negativeAffect <- auto.arima(affect[,2], seasonal = TRUE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_negativeAffect)
forecast_negativeAffect <- as.vector(forecast(mod_negativeAffect, h = 12)$mean)
# positiveAffect
mod_positiveAffect <- auto.arima(abs(affect[,3]), seasonal = TRUE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_positiveAffect)
forecast_positiveAffect <- -1*as.vector(forecast(mod_positiveAffect, h = 12)$mean)

# Political Ideology
# Support for democrats
mod_ideologySuppDemocrats <- auto.arima(politicalIdeology[,2], seasonal = FALSE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_ideologySuppDemocrats)
forecast_ideologySuppDemocrats <- as.vector(forecast(mod_ideologySuppDemocrats, h = 12)$mean)
# Support for republicans
mod_ideologySuppRepublicans <- auto.arima(politicalIdeology[,3], seasonal = FALSE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_ideologySuppRepublicans)
forecast_ideologySuppRepublicans <- as.vector(forecast(mod_ideologySuppRepublicans, h = 12)$mean)

# Political Polarization
# Democrat approvals
mod_politicalPolarizationDem <- auto.arima(politicalPolarization[,2], seasonal = TRUE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_politicalPolarizationDem)
forecast_politicalPolarizationDem <- as.vector(forecast(mod_politicalPolarizationDem, h = 12)$mean)
# Republican approvals
mod_politicalPolarizationRep <- auto.arima(politicalPolarization[,3], seasonal = TRUE, stationary = TRUE, stepwise = FALSE, lambda = "auto", biasadj = TRUE)
checkresiduals(mod_politicalPolarizationRep)
forecast_politicalPolarizationRep <- as.vector(forecast(mod_politicalPolarizationRep, h = 12)$mean)

forecast_politicalPolarization <- forecast_politicalPolarizationRep - forecast_politicalPolarizationDem

