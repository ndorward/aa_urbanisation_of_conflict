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

panel = read.csv("data outputs/africa_panel.csv", sep = ",") %>%
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

#create lists to store models
ged_models = list() 
ged_plots = list()

lapply(ged_dv, function(dv) {
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

#all models show evidence of overdispersion so use negative binomial model

#ged models without FE

for(i in 1:length(ged_dv)){
  
  m1 = glm(as.formula(paste(ged_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), 
           data=panel, 
           family="poisson") # fit models
  
  dispersion_test = AER::dispersiontest(m1)
  dispersion_est = dispersion_test$estimate
  
  # fit negative binomial models with dispersion parameter
  m2 = glm(as.formula(paste(ged_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), 
           data=panel, family = negative.binomial(dispersion_est), maxit = 1000) 
  
  ged_models[[i]] = m2
  
  model_plot = ggpredict(m2, terms = "time_trend", ci_level = 0.95) %>% 
    plot() + 
    theme_bw() +
    ggtitle("") +  # Add this line
    xlab("") +
    ylab("Predicted event count") + 
    scale_x_continuous(labels = c(2000, 2005, 2010, 2015, 2020), breaks = c(4, 9, 14, 19, 24))
  
  ged_plots[[i]] = model_plot
  
}

#Table 1. ged models
stargazer::stargazer(ged_models, type = "text", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("urban centre", "urban cluster", "rural", 
                                       "urban centre", "urban cluster", "rural", 
                                       "urban centre", "urban cluster", "rural"), 
                     dep.var.labels.include = F, model.names = F)


ggarrange(ged_plots[[1]], ged_plots[[2]], ged_plots[[3]], ged_plots[[4]],
          ged_plots[[5]], ged_plots[[6]], ged_plots[[7]], ged_plots[[8]],
          ged_plots[[9]], ncol = 3, nrow = 3, 
          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)"))

#acled models
acled_dv = as.list(c("urban_centre_riot_count", "urban_cluster_riot_count", "rural_riot_count",
                     "urban_centre_protest_count", "urban_cluster_protest_count", "rural_protest_count"))

#create lists to store models
acled_models = list() 
acled_plots = list()

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
  
  m1 = glm(as.formula(paste(acled_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), 
           data=panel, 
           family="poisson") # fit models
  
  dispersion_test = AER::dispersiontest(m1)
  dispersion_est = dispersion_test$estimate
  
  # fit models
  m2 = glm(as.formula(paste(acled_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), 
           data=panel, family = negative.binomial(dispersion_est), maxit = 1000) 
  
  acled_models[[i]] = m2
  
  model_plot = ggpredict(m2, terms = "time_trend", ci_level = 0.95) %>% 
    plot() + 
    theme_bw() +
    ggtitle("") +  # Add this line
    xlab("") +
    ylab("Predicted event count") + 
    scale_x_continuous(labels = c(2000, 2005, 2010, 2015, 2020), breaks = c(4, 9, 14, 19, 24))
  
  acled_plots[[i]] = model_plot
  
}

#Table 2. acled models 
stargazer::stargazer(acled_models[[1]], acled_models[[2]], acled_models[[3]], acled_models[[4]],
                     acled_models[[5]], acled_models[[6]], type = "html", omit = "country_iso")

# figure 2. acled plots 
ggarrange(acled_plots[[1]], acled_plots[[2]], acled_plots[[3]], acled_plots[[4]],
          acled_plots[[5]], acled_plots[[6]], ncol = 2, nrow = 3, 
          labels = c("a)", "b)", "c)", "d)", "e)", "f)"))


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
                     type = "text", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural"), 
                     dep.var.labels.include = F, model.names = F)

ged_fe_plot = map(ged_plots_FE, ~ .x)

# Arrange the plots using ggarrange
ggarrange(plotlist = ged_fe_plot, ncol = 3, nrow = 3, 
          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)"))

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
stargazer::stargazer(acled_models_FE[[1]], acled_models_FE[[2]], acled_models_FE[[3]], 
                     acled_models_FE[[4]], acled_models_FE[[5]], acled_models_FE[[6]], 
                     type = "html", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural"), 
                     dep.var.labels.include = F, model.names = F)

acled_fe_plot = map(acled_plots_FE, ~ .x)

# Arrange the plots using ggarrange
ggarrange(plotlist = acled_fe_plot, ncol = 2, nrow = 3, 
          labels = c("a)", "b)", "c)", "d)", "e)", "f)"))




# appendix material/robustness --------------------------------------------


# Appendix 4. combined riot and protest categories ------------------------

contentious_dv = as.list(c("urban_centre_contentious_count", 
                           "urban_cluster_contentious_count",
                           "rural_contentious_count"))

#create lists to store models
contentious_models = list()
contentious_plots = list()


#all models show evidence of overdispersion s use negative binomial

for(i in 1:length(contentious_dv)){
  
  m1 = glm(as.formula(paste(contentious_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel, 
           family="poisson") # fit models
  
  dispersion_test = AER::dispersiontest(m1)
  dispersion_est = dispersion_test$estimate
  
  # fit models
  m2 = glm(as.formula(paste(contentious_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel, family = negative.binomial(dispersion_est), maxit = 1000) 
  
  contentious_models[[i]] = m2
  
  model_plot = ggpredict(m2, terms = "time_trend", ci_level = 0.95) %>% 
    plot() + 
    theme_bw() +
    ggtitle("") +  # Add this line
    xlab("") +
    ylab("Predicted event count") + 
    scale_x_continuous(labels = c(2000, 2005, 2010, 2015, 2020), breaks = c(4, 9, 14, 19, 24))
  
  contentious_plots[[i]] = model_plot
  
}

#table 4. acled country fe models
stargazer::stargazer(contentious_models[[1]], contentious_models[[2]],
                     contentious_models[[3]], 
                     type = "html", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("Urban centre", "Urban cluster", "Rural", 
                                       "Urban centre", "Urban cluster", "Rural"), 
                     dep.var.labels.include = F, model.names = F)




# proportion models -------------------------------------------------------

proportion_dv = as.list(c("urban_state_based_share", "urban_non_state_share",
                           "urban_one_sided_share", "urban_riot_share", 
                          "urban_protest_share"))

#create lists to store models
proportion_models = list()



#all models show evidence of overdispersion s use negative binomial

for(i in 1:length(proportion_dv)){
  
  # fit models
  m2 = lm(as.formula(paste(proportion_dv[i],"~ time_trend + time_trend_square + urbanisation + urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp + country_iso")), 
           data=panel) 
  
  proportion_models[[i]] = m2
  
}

#table 4. acled country fe models
stargazer::stargazer(proportion_models, 
                     type = "html", omit = "country_iso", 
                     star.cutoffs = c(.05, .01, .001), 
                     column.labels = c("State-based", "Non-state", "One-sided", 
                                       "Riots", "Protests"), 
                     dep.var.labels.include = F, model.names = F)
















#run as bayes model 
ged_bayes_models <- list()

#change to foreach 
for(i in 1:length(ged_dv)){
  
  m <- brm(as.formula(paste(ged_dv[i],"~ time_trend + I(time_trend^2) + urbanisation +
                urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), data=panel, 
           family = negbinomial(),
           warmup = 2500, iter = 5000, 
           chains = 2, cores = 8, threads = threading(4),
           inits = "0", control = list(adapt_delta = .99),
           seed = 117)
  ged_bayes_models[[i]] = m
  
}

extract_re = function(input_model){
  re_table = as.data.frame(fixef(input_model, summary = T, probs = c(0.025, 0.975))) %>%
    round (digits = 2) %>%
    dplyr::mutate(across(c("Q2.5", "Q97.5"), round, 1)) %>%
    unite("CI 95%", c("Q2.5", "Q97.5"), sep = ", ", remove = FALSE) %>%
    dplyr::select(-c(Est.Error, "Q2.5", "Q97.5")) %>%
    dplyr::mutate("CI 95%" = str_trim(`CI 95%`, side = "both"), 
                  "CI 95%" = paste0("[",format(unlist(.["CI 95%"])),"]")) 
  return(re_table)
}

#note will need to manually edit as all labeled as log_world_pop currently
table_1 = gdata::cbindX(extract_re(ged_bayes_models[[1]]), extract_re(ged_bayes_models[[2]]), 
                        extract_re(ged_bayes_models[[3]]), extract_re(ged_bayes_models[[4]]), 
                        extract_re(ged_bayes_models[[5]]), extract_re(ged_bayes_models[[6]]),
                        extract_re(ged_bayes_models[[7]]), extract_re(ged_bayes_models[[8]]), 
                        extract_re(ged_bayes_models[[9]])) %>%
  `rownames<-` (rownames(extract_re(ged_bayes_models[[1]]))) %>%
  replace(is.na(.), "Null") %>%
  `colnames<-` (1:18) %>%
  mutate(across(everything(), as.character)) %>%
  mutate_all(funs(str_replace_all(., "Null", ""))) %>%
  mutate_all(funs(str_replace_all(., " ", ""))) %>%
  mutate_all(funs(str_replace_all(., ",", ", ")))

htmlTable(table_1)

#alt table using sjplot
tab_model(ged_bayes_models[[1]], ged_bayes_models[[2]], ged_bayes_models[[3]], ged_bayes_models[[4]],
          ged_bayes_models[[5]], ged_bayes_models[[6]], ged_bayes_models[[7]], ged_bayes_models[[8]],
          ged_bayes_models[[9]], transform = NULL, collapse.ci = TRUE, show.p = T, p.style = "stars")

msummary(c(ged_bayes_models[[1]], ged_bayes_models[[2]], ged_bayes_models[[3]], ged_bayes_models[[4]],
           ged_bayes_models[[5]], ged_bayes_models[[6]], ged_bayes_models[[7]], ged_bayes_models[[8]],
           ged_bayes_models[[9]]), stars = T)


#run as correlated response with mvbind

#run as seemingly unrelated regression

#run as multilevel model with geography as group
















#using glm.b
ged_models <- list() #create list to store models 

for(i in 1:length(ged_dv)){
  
  # fit models
  m = glm.nb(as.formula(paste(ged_dv[i],"~ time_trend + I(time_trend^2) + urbanisation +
                urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp")), data=panel) 
  
  ged_models[[i]] = m
  
}

ged_models$type = "html"
do.call( stargazer, ged_models) 

#option - could run glm.nb models 1:8 in loop storing the estimated theta and 

#models using glm.nb and estimated theta
model1a = glm.nb(state_based_urban_centre_count ~ time_trend + I(time_trend^2) + urbanisation +
                     urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                   data = panel)

model1b = glm.nb(state_based_urban_cluster_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1c = glm.nb(state_based_rural_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1d = glm.nb(non_state_urban_centre_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1e = glm.nb(non_state_urban_cluster_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1f = glm.nb(non_state_rural_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1g = glm.nb(one_sided_urban_centre_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1h = glm.nb(one_sided_urban_cluster_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

model1i = glm.nb(one_sided_rural_count ~ time_trend + I(time_trend^2) + urbanisation +
                   urban_growth + log_total_population + v2x_polyarchy + log_wb_gdp, 
                 data = panel)

(2 * (logLik(model1b) - logLik(model1a)), df = 3, lower.tail=FALSE)
vuong(model1a, model1b) #zero inflated model preferable


#full negative binomial count models of conflict broken down by GED violence type 
stargazer::stargazer(model1a, model1b,
                     type = "html")

test = brm(mvbind(state_based_urban_centre_count, state_based_urban_cluster_count, 
                  state_based_rural_count) ~ time_trend + I(time_trend^2) + log_internet_users + 
             urbanisation + log_total_population + v2x_polyarchy + log_wb_gdp, 
           data = panel, 
           family = negbinomial(),
           warmup = 2500, iter = 5000, 
           chains = 2, cores = 8, threads = threading(4),
           inits = "0", 
           backend = "cmdstanr", control = list(adapt_delta = .99, max_treedepth = 20),
           seed = 117)