setwd("~/Dropbox/Urbanisation of conflict")

library(dplyr)
library(MASS)
library(ggcorrplot)
library(car)
library(tidyr)
library(stringr)
library(psych)
library(gdata)
library(sjPlot)
library(ggpubr)
library(AER)
library(ggeffects)

panel = read.csv("data outputs/africa_panel_all_events.csv", sep = ",") %>%
  dplyr::select(-X) %>%
  dplyr::mutate(country_iso = as.factor(country_iso)) %>%
  dplyr::mutate(time_trend_square = time_trend^2)

corr_matrix = panel %>%
  dplyr::ungroup() %>%
  dplyr::select(c(time_trend, urbanisation, urban_growth, log_total_population,
                  v2x_polyarchy, log_wb_gdp)) %>%
  cor() %>%
  ggcorrplot(., hc.order = TRUE, type = "lower", lab = TRUE)

descriptive_table = panel %>%
  dplyr::ungroup() %>%
  dplyr::select(c(urbanisation, urban_growth, log_total_population, v2x_polyarchy, log_wb_gdp)) %>%
  psych::describe() %>%
  dplyr::select(c(mean, sd, median, min, max, se)) %>%
  round(digits = 3)

# 1) Negative binomial models --------------------------------------------------

#full negative binomial count models of conflict broken down by GED violence type 
ged_dv = as.list(c("state_based_urban_centre_count", "state_based_urban_cluster_count","state_based_rural_count",
                   "non_state_urban_centre_count", "non_state_urban_cluster_count","non_state_rural_count",
                   "one_sided_urban_centre_count", "one_sided_urban_cluster_count","one_sided_rural_count"))


# with country fe 
#ged models FE
ged_models_FE = list()
ged_plots_FE = list()

for(i in 1:length(ged_dv)){
  
  m1 = glm(as.formula(paste(ged_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel, 
           family="poisson") # fit models
  
  dispersion_test = AER::dispersiontest(m1)
  dispersion_est = dispersion_test$estimate
  
  # fit negative binomial models with dispersion parameter
  m2 = glm(as.formula(paste(ged_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel, family = negative.binomial(dispersion_est), maxit = 1000) 
  
  ged_models_FE[[i]] = m2
  
  model_plot = ggpredict(m2, terms = "time_trend", ci_level = 0.5) %>% 
    plot() + 
    theme_bw() +
    ggtitle("") +  # Add this line
    xlab("") +
    ylab("Predicted event count") + 
    scale_x_continuous(labels = c(2000, 2005, 2010, 2015, 2020), breaks = c(4, 9, 14, 19, 24))
  
  ged_plots_FE[[i]] = model_plot
  
}

#table 3. ged country fe models
stargazer::stargazer(ged_models_FE, 
                     type = "html", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural"), 
                     dep.var.labels.include = F, model.names = F)

# Arrange the plots using ggarrange
ggarrange(plotlist = ged_fe_plot, ncol = 3, nrow = 3, 
          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)"))


#acled models
acled_dv = as.list(c("urban_centre_riot_count", "urban_cluster_riot_count", "rural_riot_count",
                     "urban_centre_protest_count", "urban_cluster_protest_count", "rural_protest_count"))

#create lists to store models
acled_models_FE = list()
acled_plots_FE = list()

#check validity of poisson model
lapply(acled_dv, function(dv) {
  # Fit a Poisson regression model
  m = glm(as.formula(paste(dv, " ~ time_trend + I(time_trend^2) + urbanisation +
                urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), 
          data = panel, family = "poisson") # fit models
  
  # Extract residual deviance and degrees of freedom
  residual_deviance = deviance(m)
  df = df.residual(m)
  
  # Calculate the ratio of residual deviance to degrees of freedom
  ratio = residual_deviance / df
  
  # Perform a chi-squared test for overdispersion
  p_value = 1 - pchisq(residual_deviance, df)
  
  # Check for overdispersion and print results
  cat("Model:", dv, "\n")
  cat("Residual Deviance:", residual_deviance, "\n")
  cat("Degrees of Freedom:", df, "\n")
  cat("Residual Deviance / Degrees of Freedom:", ratio, "\n")
  cat("Chi-squared test p-value:", p_value, "\n")
  
  # If the model shows evidence of overdispersion, print a warning
  if (ratio > 1.2) {
    cat("Warning: The model may be overdispersed.\n")
  } else {
    cat("The model seems adequately dispersed.\n")
  }
  
  # Store the model in the list
  ged_models[[dv]] = m
})

#all models show evidence of overdispersion s use negative binomial

for(i in 1:length(acled_dv)){
  
  m1 = glm(as.formula(paste(acled_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel, 
           family="poisson") # fit models
  
  dispersion_test = AER::dispersiontest(m1)
  dispersion_est = dispersion_test$estimate
  
  # fit models
  m2 = glm(as.formula(paste(acled_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel, family = negative.binomial(dispersion_est), maxit = 1000) 
  
  acled_models_FE[[i]] = m2
  
  model_plot = ggpredict(m2, terms = "time_trend", ci_level = 0.95) %>% 
    plot() + 
    theme_bw() +
    ggtitle("") +  # Add this line
    xlab("") +
    ylab("Predicted event count") + 
    scale_x_continuous(labels = c(2000, 2005, 2010, 2015, 2020), breaks = c(4, 9, 14, 19, 24))
  
  acled_plots_FE[[i]] = model_plot
  
}

#table 4. acled country fe models
stargazer::stargazer(acled_models_FE, 
                     type = "html", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural"), 
                     dep.var.labels.include = F, model.names = F)


# Arrange the plots using ggarrange
ggarrange(plotlist = acled_fe_plot, ncol = 2, nrow = 3, 
          labels = c("a)", "b)", "c)", "d)", "e)", "f)"))

