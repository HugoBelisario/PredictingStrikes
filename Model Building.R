## Model Building

# Loading Packages --------------------------------------------------------

library(MASS)
library(tidyverse)
library(modelr)
library(janitor)
library(broom)
library(skimr)
library(leaps)
library(glmnet)
library(glmnetUtils)
library(pls)
set.seed(42)


# Importing and Setting Up Data -------------------------------------------

bullpen_mod_dat <- read_csv(file = "Data/Processed/Building Data.csv")

bullpen_bldg <- bullpen_mod_dat %>% sample_frac(0.85)
bullpen_comp <- bullpen_mod_dat %>% setdiff(bullpen_bldg)

## Selecting variables that were found to be useful by EDA; Comparing dummies to a 4-seam fastball thrown by Chapman
bullpen_bldg <- bullpen_bldg %>% 
  mutate(release_spin_rate = as.numeric(release_spin_rate),
         field_lead = fld_score - bat_score,
         extras = ifelse(inning > 9, 1, 0)) %>% 
  dplyr::select(release_speed:release_pos_z, zone, stand, p_throws, balls, strikes, outs_when_up, inning, effective_speed:release_extension, 
                knuckle_curve, slider, cutter, two_seam, sinker, split_finger, changeup, strike:down_zone, betances:ottavino, pitch_count:extras) %>% 
  na.omit()

bullpen_comp <- bullpen_comp %>% 
  mutate(release_spin_rate = as.numeric(release_spin_rate),
         field_lead = fld_score - bat_score,
         extras = ifelse(inning > 9, 1, 0)) %>% 
  dplyr::select(release_speed:release_pos_z, zone, stand, p_throws, balls, strikes, outs_when_up, inning, effective_speed:release_extension, 
                knuckle_curve, slider, cutter, two_seam, sinker, split_finger, changeup, strike:down_zone, betances:ottavino, pitch_count:extras) %>% 
  na.omit()


# Ridge Regression --------------------------------------------------------

## Finding best lambda values
lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- bullpen_bldg %>% 
  cv.glmnet(formula = strike ~ .,
            data = ., alpha = 0, nfolds = 10, lambda = lambda_grid, family = "binomial")

plot(ridge_cv)    
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

#Models
bullpen_ridge <- tibble(train = bullpen_bldg %>% list(),
                       test  = bullpen_comp %>% list()) %>% 
  mutate(ridge_min = map(train, ~ glmnet(strike ~ ., data = .x,
                                         alpha = 0, lambda = ridge_lambda_min)),
         ridge_1se = map(train, ~ glmnet(strike ~ ., data = .x,
                                         alpha = 0, lambda = ridge_lambda_1se))) %>% 
  gather(key = method, value = fit, -test, -train)

bullpen_ridge %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>% 
  reduce(full_join, by = "name") %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y) %>% 
  kable()

bullpen_ridge %>% 
  mutate(pred = map2(fit, test, predict, type = "response"),
         test_strike = map(pred, ~ if_else(.x > 0.5, 1, 0)),
         test_error = map2_dbl(test, test_strike, ~ mean((.x$strike != .y)))) %>% 
  unnest(test_strike, test) %>%
  group_by(method, test_error) %>% 
  count(test_strike, strike)


# Lasso -------------------------------------------------------------------

## Finding best lambda values
lasso_cv <- bullpen_bldg %>% 
  cv.glmnet(formula = strike ~ .,
            data = ., alpha = 1, nfolds = 10, family = "binomial")

plot(lasso_cv)
lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_min <- lasso_cv$lambda.min

#Models
bullpen_lasso <- tibble(train = bullpen_bldg %>% list(),
                       test  = bullpen_comp %>% list()) %>% 
  mutate(lasso_min = map(train, ~ glmnet(strike ~ ., data = .x,
                                         alpha = 1, lambda = lasso_lambda_min)),
         lasso_1se = map(train, ~ glmnet(strike ~ ., data = .x,
                                         alpha = 1, lambda = lasso_lambda_1se))) %>% 
  gather(key = method, value = fit, -test, -train)

bullpen_lasso %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>% 
  reduce(full_join, by = "name") %>% 
  rename(lasso_min = s0.x,
         lasso_1se = s0.y) %>% 
  kable()

#Test errors
bullpen_lasso %>% 
  mutate(pred = map2(fit, test, predict, type = "response"),
         test_strike = map(pred, ~ if_else(.x > 0.5, 1, 0)),
         test_error = map2_dbl(test, test_strike, ~ mean((.x$strike != .y)))) %>% 
  unnest(test_strike, test) %>%
  group_by(method, test_error) %>% 
  count(test_strike, strike)

# LDA ---------------------------------------------------------------------

#### Helper Functions

error_rate_lda <- function(data, model){
  data %>% 
    mutate(pred_strike = predict(model, newdata = data) %>% 
             pluck("class"),
           error = pred_strike != strike) %>% 
    pull(error) %>% 
    mean()
}

confusion_mat_lda <- function(data, model){
  data %>% 
    mutate(pred_strike = predict(model, newdata = data) %>% 
             pluck("class")) %>% 
    count(strike, pred_strike) %>% 
    mutate(prop = n / sum(n))
}

### Model

tibble(train = bullpen_bldg %>% list(),
       test  = bullpen_comp %>% list()) %>% 
  mutate(mod_01 = map(train, ~ lda(formula = strike ~ .,
                                   data = .x))) %>% 
  gather(key = model_name, value = model_fit, contains("mod_")) %>% 
  mutate(test_error = map2_dbl(test, model_fit, error_rate_lda),
         confusion_mat = map2(test, model_fit, confusion_mat_lda)) %>% 
  unnest(confusion_mat)



# QDA ---------------------------------------------------------------------

### Helper Functions
error_rate_qda <- function(data, model){
  data %>% 
    mutate(pred_strike = predict(model, newdata = data) %>% 
             pluck("class"),
           error = pred_strike != strike) %>% 
    pull(error) %>% 
    mean()
}


confusion_mat_qda <- function(data, model){
  data %>% 
    mutate(pred_strike = predict(model, newdata = data) %>% 
             pluck("class")) %>% 
    count(strike, pred_strike) %>% 
    mutate(prop = n / sum(n))
}

### Model
tibble(train = bullpen_bldg %>% list(),
       test  = bullpen_comp %>% list()) %>% 
  mutate(mod_01 = map(train, ~ qda(formula = strike ~ zone + strikes + inning + release_spin_rate + up_zone + down_zone + betances + britton
                                   + green + ottavino,
                                   data = .x))) %>% 
  gather(key = model_name, value = model_fit, contains("mod_")) %>% 
  mutate(test_error = map2_dbl(test, model_fit, error_rate_qda),
         confusion_mat = map2(test, model_fit, confusion_mat_qda)) %>% 
  unnest(confusion_mat)


### Best model is ridge1se