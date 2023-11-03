#Chapter 3 ----
#A Review of R Modeling Fundamentals

library(tidyverse)
library(broom)
library(tidymodels)

data(crickets, package = "modeldata")
names(crickets)

# Plot the temperature on the x-axis, the chirp rate on the y-axis. The plot
# elements will be colored differently for each species:
ggplot(crickets, 
       aes(x = temp, y = rate, color = species, pch = species, lty = species)) + 
        # Plot points for each data point and color by species
        geom_point(size = 2) + 
        # Show a simple linear model fit created separately for each species:
        geom_smooth(method = lm, se = FALSE, alpha = 0.5) + 
        scale_color_brewer(palette = "Paired") +
        labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")

interaction_fit <-  lm(rate ~ (temp + species)^2, data = crickets) 

# To print a short summary of the model:
interaction_fit

# Place two plots next to one another:
par(mfrow = c(1, 2))

# Show residuals vs predicted values:
plot(interaction_fit, which = 1)

# A normal quantile plot on the residuals:
plot(interaction_fit, which = 2)

# Fit a reduced model:
main_effect_fit <-  lm(rate ~ temp + species, data = crickets) 

# Compare the two:
anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)

predict(main_effect_fit, new_values)

# Add a missing value to the prediction set
new_values$temp[1] <- NA

# The predict method for `lm` defaults to `na.pass`:
predict(main_effect_fit, new_values)

# Alternatively 
predict(main_effect_fit, new_values, na.action = na.fail)

predict(main_effect_fit, new_values, na.action = na.omit)

corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

# The first of ten results in the vector: 
corr_res[[1]]

tidy(corr_res[[1]])

corr_res %>% 
        # Convert each to a tidy format; `map_dfr()` stacks the data frames 
        map_dfr(tidy, .id = "predictor") %>% 
        ggplot(aes(x = fct_reorder(predictor, estimate))) + 
        geom_point(aes(y = estimate)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
        labs(x = NULL, y = "Correlation with mpg")

split_by_species <- 
        crickets %>% 
        group_nest(species) 
split_by_species

model_by_species <- 
        split_by_species %>% 
        mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))
model_by_species

model_by_species %>% 
        mutate(coef = map(model, tidy)) %>% 
        select(species, coef) %>% 
        unnest(cols = c(coef))

#Chapter 4 ----
#The Ames Housing Data
data(ames)

dim(ames)

tidymodels_prefer()

ggplot(ames, aes(x = Sale_Price)) + 
        geom_histogram(bins = 50, col= "white")

ggplot(ames, aes(x = Sale_Price)) + 
        geom_histogram(bins = 50, col= "white") +
        scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


#Chapter 5 ----
#Spending our Data
library(tidymodels)
tidymodels_prefer()

# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(501)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

#Chapter 6 ----
#Fitting Models with Parsnip

library(tidymodels)
tidymodels_prefer()

linear_reg() %>% set_engine("lm")

linear_reg() %>% set_engine("glmnet") 

linear_reg() %>% set_engine("stan")

linear_reg() %>% set_engine("lm") %>% translate()

linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()

linear_reg() %>% set_engine("stan") %>% translate()

lm_model <- 
        linear_reg() %>% 
        set_engine("lm")

lm_form_fit <- 
        lm_model %>% 
        # Recall that Sale_Price has been pre-logged
        fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
        lm_model %>% 
        fit_xy(
                x = ames_train %>% select(Longitude, Latitude),
                y = ames_train %>% pull(Sale_Price)
        )

lm_form_fit

lm_xy_fit

rand_forest(trees = 1000, min_n = 5) %>% 
        set_engine("ranger") %>% 
        set_mode("regression") %>% 
        translate()

rand_forest(trees = 1000, min_n = 5) %>% 
        set_engine("ranger", verbose = TRUE) %>% 
        set_mode("regression")

lm_form_fit %>% extract_fit_engine()

lm_form_fit %>% extract_fit_engine() %>% vcov()

model_res <- 
        lm_form_fit %>% 
        extract_fit_engine() %>% 
        summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
param_est

tidy(lm_form_fit)

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

ames_test_small %>% 
        select(Sale_Price) %>% 
        bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
        # Add 95% prediction intervals to the results:
        bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int")) 

tree_model <- 
        decision_tree(min_n = 2) %>% 
        set_engine("rpart") %>% 
        set_mode("regression")

tree_fit <- 
        tree_model %>% 
        fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
        select(Sale_Price) %>% 
        bind_cols(predict(tree_fit, ames_test_small))

#Chapter 7 ----
#A Model Workflow

tidymodels_prefer()

lm_model <- 
        linear_reg() %>% 
        set_engine("lm")

lm_wflow <- 
        workflow() %>% 
        add_model(lm_model)

lm_wflow

lm_wflow <- 
        lm_wflow %>% 
        add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% update_formula(Sale_Price ~ Longitude)

lm_wflow <- 
        lm_wflow %>% 
        remove_formula() %>% 
        add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))
lm_wflow

fit(lm_wflow, ames_train)

library(lme4)
data(Orthodont, package = "nlme")
lmer(distance ~ Sex + (age | Subject), data = Orthodont)

model.matrix(distance ~ Sex + (age | Subject), data = Orthodont)

library(multilevelmod)

multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
        workflow() %>%  
        add_variables(outcome = Sale_Price, predictors = c(Lot_Area, Neighborhood, Overall_Cond)) %>% 
        add_model(multilevel_spec, 
                  formula = Sale_Price ~ Overall_Cond + (Lot_Area | Neighborhood))

multilevel_fit <- fit(multilevel_workflow, data = ames_train)
multilevel_fit

library(censored)

parametric_spec <- survival_reg()

parametric_workflow <- 
        workflow() %>% 
        add_variables(outcome = c(fustat, futime), predictors = c(age, rx)) %>% 
        add_model(parametric_spec, 
                  formula = Surv(futime, fustat) ~ age + strata(rx))

parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit

location <- list(
        longitude = Sale_Price ~ Longitude,
        latitude = Sale_Price ~ Latitude,
        coords = Sale_Price ~ Longitude + Latitude,
        neighborhood = Sale_Price ~ Neighborhood
)

library(workflowsets)
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
location_models$info[[1]]
extract_workflow(location_models, id = "coords_lm")

location_models <-
        location_models %>%
        mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]

final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)

#Chapter 8 ----
#Feature Engineering with recipes

lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames)

library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_ames <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
               data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_dummy(all_nominal_predictors())
simple_ames

lm_wflow <- 
        lm_wflow %>% 
        remove_variables() %>% 
        add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)

predict(lm_fit, ames_test %>% slice(1:3))

# Get the recipe after it has been estimated:
lm_fit %>% 
        extract_recipe(estimated = TRUE)

# To tidy the model fit: 
lm_fit %>% 
        # This returns the parsnip object:
        extract_fit_parsnip() %>% 
        # Now tidy the linear model object:
        tidy() %>% 
        slice(1:5)

simple_ames <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
               data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors())

ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
        geom_point(alpha = .2) + 
        facet_wrap(~ Bldg_Type) + 
        geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
        scale_x_log10() + 
        scale_y_log10() + 
        labs(x = "Gross Living Area", y = "Sale Price (USD)")

simple_ames <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
               data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        # Gr_Liv_Area is on the log scale from a previous step
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
        ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
                geom_point(alpha = .2) + 
                scale_y_log10() +
                geom_smooth(
                        method = lm,
                        formula = y ~ ns(x, df = deg_free),
                        color = "lightblue",
                        se = FALSE
                ) +
                labs(title = paste(deg_free, "Spline Terms"),
                     y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )

recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, deg_free = 20)

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, Longitude, deg_free = 20)

tidy(ames_rec)

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- 
        workflow() %>% 
        add_model(lm_model) %>% 
        add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

estimated_recipe <- 
        lm_fit %>% 
        extract_recipe(estimated = TRUE)

tidy(estimated_recipe, id = "my_id")

tidy(estimated_recipe, number = 2)

ames_rec %>% update_role(address, new_role = "street address")

#Chapter 9 ----
#Judging Model Effectiveness

##Data transformation
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
        workflow() %>% 
        add_model(lm_model) %>% 
        add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)
####

ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res

ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
        # Create a diagonal line:
        geom_abline(lty = 2) + 
        geom_point(alpha = 0.5) + 
        labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
        # Scale and size the x- and y-axis uniformly:
        coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

data(two_class_example)
tibble(two_class_example)

# A confusion matrix: 
conf_mat(two_class_example, truth = truth, estimate = predicted)

# Accuracy:
accuracy(two_class_example, truth, predicted)

# Matthews correlation coefficient:
mcc(two_class_example, truth, predicted)

# F1 metric:
f_meas(two_class_example, truth, predicted)

# Combining these three classification metrics together
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

f_meas(two_class_example, truth, predicted, event_level = "second")

two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve

roc_auc(two_class_example, truth, Class1)

autoplot(two_class_curve)

data(hpc_cv)
tibble(hpc_cv)

accuracy(hpc_cv, obs, pred)

mcc(hpc_cv, obs, pred)

class_totals <- 
        count(hpc_cv, obs, name = "totals") %>% 
        mutate(class_wts = totals / sum(totals))
class_totals

cell_counts <- 
        hpc_cv %>% 
        group_by(obs, pred) %>% 
        count() %>% 
        ungroup()

# Compute the four sensitivities using 1-vs-all
one_versus_all <- 
        cell_counts %>% 
        filter(obs == pred) %>% 
        full_join(class_totals, by = "obs") %>% 
        mutate(sens = n / totals)
one_versus_all

# Three different estimates:
one_versus_all %>% 
        summarize(
                macro = mean(sens), 
                macro_wts = weighted.mean(sens, class_wts),
                micro = sum(n) / sum(totals)
        )

sensitivity(hpc_cv, obs, pred, estimator = "macro")
sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")
sensitivity(hpc_cv, obs, pred, estimator = "micro")

roc_auc(hpc_cv, obs, VF, F, M, L)

roc_auc(hpc_cv, obs, VF, F, M, L, estimator = "macro_weighted")

hpc_cv %>% 
        group_by(Resample) %>% 
        accuracy(obs, pred)

# Four 1-vs-all ROC curves for each fold
hpc_cv %>% 
        group_by(Resample) %>% 
        roc_curve(obs, VF, F, M, L) %>% 
        autoplot()

#Chapter 10 ----
#Resampling for Evaluating Performance

rf_model <- 
        rand_forest(trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("regression")

rf_wflow <- 
        workflow() %>% 
        add_formula(
                Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                        Latitude + Longitude) %>% 
        add_model(rf_model) 

rf_fit <- rf_wflow %>% fit(data = ames_train)

estimate_perf <- function(model, dat) {
        # Capture the names of the `model` and `dat` objects
        cl <- match.call()
        obj_name <- as.character(cl$model)
        data_name <- as.character(cl$dat)
        data_name <- gsub("ames_", "", data_name)
        
        # Estimate these metrics:
        reg_metrics <- metric_set(rmse, rsq)
        
        model %>%
                predict(dat) %>%
                bind_cols(dat %>% select(Sale_Price)) %>%
                reg_metrics(Sale_Price, .pred) %>%
                select(-.estimator) %>%
                mutate(object = obj_name, data = data_name)
}

estimate_perf(rf_fit, ames_train)

estimate_perf(lm_fit, ames_train)

estimate_perf(rf_fit, ames_test)

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds

# For the first fold:
ames_folds$splits[[1]] %>% analysis() %>% dim()

vfold_cv(ames_train, v = 10, repeats = 5)

mc_cv(ames_train, prop = 9/10, times = 20)

set.seed(1002)
val_set <- validation_split(ames_train, prop = 3/4)
val_set

bootstraps(ames_train, times = 5)

time_slices <- 
        tibble(x = 1:365) %>% 
        rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)

data_range <- function(x) {
        summarize(x, first = min(x), last = max(x))
}

map_dfr(time_slices$splits, ~   analysis(.x) %>% data_range())
map_dfr(time_slices$splits, ~ assessment(.x) %>% data_range())

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- 
        rf_wflow %>% 
        fit_resamples(resamples = ames_folds, control = keep_pred)
rf_res

collect_metrics(rf_res)

assess_res <- collect_predictions(rf_res)
assess_res

assess_res %>% 
        ggplot(aes(x = Sale_Price, y = .pred)) + 
        geom_point(alpha = .15) +
        geom_abline(color = "red") + 
        coord_obs_pred() + 
        ylab("Predicted")

over_predicted <- 
        assess_res %>% 
        mutate(residual = Sale_Price - .pred) %>% 
        arrange(desc(abs(residual))) %>% 
        slice(1:2)
over_predicted

ames_train %>% 
        slice(over_predicted$.row) %>% 
        select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)

val_res <- rf_wflow %>% fit_resamples(resamples = val_set)
val_res

collect_metrics(val_res)

# The number of physical cores in the hardware:
parallel::detectCores(logical = FALSE)

# The number of possible independent processes that can 
# be simultaneously used:  
parallel::detectCores(logical = TRUE)

# All operating systems
library(doParallel)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

# Now run fit_resamples()`...
set.seed(1003)
rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)

stopCluster(cl)

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <-  
        workflow() %>% 
        add_recipe(ames_rec) %>% 
        add_model(linear_reg() %>% set_engine("lm")) 

lm_fit <- lm_wflow %>% fit(data = ames_train)

# Select the recipe: 
extract_recipe(lm_fit, estimated = TRUE)

get_model <- function(x) {
        extract_fit_parsnip(x) %>% tidy()
}

# Test it using: 
get_model(lm_fit)

#tidy apparently doesn't work here.
# ctrl <- control_resamples(extract = extract_fit_parsnip)
# 
# lm_res <- lm_wflow %>%  fit_resamples(resamples = ames_folds, control = ctrl)
# lm_res
# 
# lm_res$.extracts[[1]]
# # To get the results
# lm_res$.extracts[[1]][[1]]
# 
# all_coef <- map_dfr(lm_res$.extracts, ~ .x[[1]][[1]])
# # Show the replicates for a single predictor:
# filter(all_coef, term == "Year_Built")
# 
#Chapter 11 ----
#Comparing Models with Resampling
#
#Data Transformation
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
        workflow() %>% 
        add_model(lm_model) %>% 
        add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

rf_model <- 
        rand_forest(trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("regression")

rf_wflow <- 
        workflow() %>% 
        add_formula(
                Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                        Latitude + Longitude) %>% 
        add_model(rf_model) 

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)
####

library(tidymodels)
tidymodels_prefer()

basic_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors())

interaction_rec <- 
        basic_rec %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
        interaction_rec %>% 
        step_ns(Latitude, Longitude, deg_free = 50)

preproc <- 
        list(basic = basic_rec, 
             interact = interaction_rec, 
             splines = spline_rec
        )

lm_models <- workflow_set(preproc, list(lm = linear_reg()), cross = FALSE)
lm_models

lm_models <- 
        lm_models %>% 
        workflow_map("fit_resamples", 
                     # Options to `workflow_map()`: 
                     seed = 1101, verbose = TRUE,
                     # Options to `fit_resamples()`: 
                     resamples = ames_folds, control = keep_pred)

lm_models

collect_metrics(lm_models) %>% 
        filter(.metric == "rmse")

four_models <- 
        as_workflow_set(random_forest = rf_res) %>% 
        bind_rows(lm_models)
four_models

library(ggrepel)
autoplot(four_models, metric = "rsq") +
        geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
        theme(legend.position = "none")

rsq_indiv_estimates <- 
        collect_metrics(four_models, summarize = FALSE) %>% 
        filter(.metric == "rsq") 

rsq_wider <- 
        rsq_indiv_estimates %>% 
        select(wflow_id, .estimate, id) %>% 
        pivot_wider(id_cols = "id", names_from = "wflow_id", values_from = ".estimate")

corrr::correlate(rsq_wider %>% select(-id), quiet = TRUE)

rsq_indiv_estimates %>% 
        mutate(wflow_id = reorder(wflow_id, .estimate)) %>% 
        ggplot(aes(x = wflow_id, y = .estimate, group = id, color = id)) + 
        geom_line(alpha = .5, lwd = 1.25) + 
        theme(legend.position = "none")

rsq_wider %>% 
        with( cor.test(basic_lm, splines_lm) ) %>% 
        tidy() %>% 
        select(estimate, starts_with("conf"))

compare_lm <- 
        rsq_wider %>% 
        mutate(difference = splines_lm - basic_lm)

lm(difference ~ 1, data = compare_lm) %>% 
        tidy(conf.int = TRUE) %>% 
        select(estimate, p.value, starts_with("conf"))

# Alternatively, a paired t-test could also be used: 
rsq_wider %>% 
        with( t.test(splines_lm, basic_lm, paired = TRUE) ) %>%
        tidy() %>% 
        select(estimate, p.value, starts_with("conf"))

library(tidyposterior)
library(rstanarm)

# The rstanarm package creates copious amounts of output; those results
# are not shown here but are worth inspecting for potential issues. The
# option `refresh = 0` can be used to eliminate the logging. 
rsq_anova <-
        perf_mod(
                four_models,
                metric = "rsq",
                prior_intercept = rstanarm::student_t(df = 1),
                chains = 4,
                iter = 5000,
                seed = 1102
        )

model_post <- 
        rsq_anova %>% 
        # Take a random sample from the posterior distribution
        # so set the seed again to be reproducible. 
        tidy(seed = 1103) 

glimpse(model_post)

model_post %>% 
        mutate(model = forcats::fct_inorder(model)) %>%
        ggplot(aes(x = posterior)) + 
        geom_histogram(bins = 50, color = "white", fill = "blue", alpha = 0.4) + 
        facet_wrap(~ model, ncol = 1)

autoplot(rsq_anova) +
        geom_text_repel(aes(label = workflow), nudge_x = 1/8, nudge_y = 1/100) +
        theme(legend.position = "none")

rqs_diff <-
        contrast_models(rsq_anova,
                        list_1 = "splines_lm",
                        list_2 = "basic_lm",
                        seed = 1104)

rqs_diff %>% 
        as_tibble() %>% 
        ggplot(aes(x = difference)) + 
        geom_vline(xintercept = 0, lty = 2) + 
        geom_histogram(bins = 50, color = "white", fill = "red", alpha = 0.4)

summary(rqs_diff) %>% 
        select(-starts_with("pract"))

summary(rqs_diff, size = 0.02) %>% 
        select(contrast, starts_with("pract"))

autoplot(rsq_anova, type = "ROPE", size = 0.02) +
        geom_text_repel(aes(label = workflow)) +
        theme(legend.position = "none")

#Chapter 12 ----
#Model Tuning and the Dangers of Overfitting

#Data Transformation
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train) %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
        workflow() %>% 
        add_model(lm_model) %>% 
        add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

rf_model <- 
        rand_forest(trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("regression")

rf_wflow <- 
        workflow() %>% 
        add_formula(
                Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                        Latitude + Longitude) %>% 
        add_model(rf_model) 

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)
####

neural_net_spec <- 
        mlp(hidden_units = tune()) %>%
        set_mode("regression") %>%
        set_engine("keras")

ames_rec <- 
        recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                       Latitude + Longitude, data = ames_train)  %>%
        step_log(Gr_Liv_Area, base = 10) %>% 
        step_other(Neighborhood, threshold = tune()) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
        step_ns(Longitude, deg_free = tune("longitude df")) %>% 
        step_ns(Latitude,  deg_free = tune("latitude df"))

recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param

wflow_param <- 
        workflow() %>% 
        add_recipe(ames_rec) %>% 
        add_model(neural_net_spec) %>% 
        extract_parameter_set_dials()
wflow_param

# identify the parameter using the id value:
wflow_param %>% extract_parameter_dials("threshold")

extract_parameter_set_dials(ames_rec) %>% 
        update(threshold = threshold(c(0.8, 1.0)))

rf_spec <- 
        rand_forest(mtry = tune()) %>% 
        set_engine("ranger", regularization.factor = tune("regularization")) %>%
        set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_spec)
rf_param

pca_rec <- 
        recipe(Sale_Price ~ ., data = ames_train) %>% 
        # Select the square-footage predictors and extract their PCA components:
        step_normalize(contains("SF")) %>% 
        # Select the number of components needed to capture 95% of
        # the variance in the predictors. 
        step_pca(contains("SF"), threshold = .95)

updated_param <- 
        workflow() %>% 
        add_model(rf_spec) %>% 
        add_recipe(pca_rec) %>% 
        extract_parameter_set_dials() %>% 
        finalize(ames_train)
updated_param

updated_param %>% extract_parameter_dials("mtry")
