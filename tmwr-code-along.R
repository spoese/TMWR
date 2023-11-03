#Chapter 3 ----
#Code-along

library(tidyverse)
library(broom)
library(tidymodels)

source("DataConnection.R")
students_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202124,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE,
               MSFSTDN_SEX, MSFSTDN_OVERALL_GPA, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FOR_CREDIT_COURSES, MSFSTDN_TOTAL_DAY_BILL_HR,
               MSFSTDN_TOTAL_EVE_BILL_HR, MSFSTDN_ROCKVILLE_BILL_HR,
               MSFSTDN_GERMANTOWN_BILL_HR, MSFSTDN_TAK_PARK_BILL_HR,
               MSFSTDN_OFF_CAMPUS_BILL_HRS) %>% 
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_"))

finaid_backup <- tbl(con, "MSFFAIS") %>% 
        filter(MSFFAIS_COLLEC_YR_S5 >= 2018) %>% 
        select(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL, MSFFAIS_PAID_AMT) %>%
        collect() %>% 
        group_by(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL) %>% 
        summarize(MSFFAIS_PAID_AMT = sum(MSFFAIS_PAID_AMT)) %>% 
        rename_with(~str_remove(., "MSFFAIS_"))

scores_backup <- tbl(con, "SORTEST") %>% 
        filter(SORTEST_TESC_CODE %in% c("S01", "S02", "S03", "S04",
                                        "S05", "S06", "S07", "S08",
                                        "S09", "S11", "S12", "S13",
                                        "S14", "S15")) %>% 
        select(SORTEST_PIDM, SORTEST_TESC_CODE, SORTEST_TEST_DATE, SORTEST_TEST_SCORE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "SORTEST_"))

DBI::dbDisconnect(con)

names(students_backup)

scores <- scores_backup %>% 
        select(PIDM) %>% 
        distinct() %>% 
        mutate(TookSAT = 1)

students <- students_backup %>% 
        filter(SEX %in% c("M","F")) %>% 
        left_join(finaid_backup, by = c("PIDM", "TERM_CODE" = "TERM_CODE_FALL")) %>% 
        left_join(scores) %>% 
        replace_na(list(PAID_AMT = 0, TookSAT = 0))


# Plot the age on the x-axis, the GPA on the y-axis. The plot
# elements will be colored differently for each gender:
ggplot(students, 
       aes(x = AGE_AT_TERM_START, y = OVERALL_GPA, color = SEX, pch = SEX, lty = SEX)) + 
        # Plot points for each data point and color by gender
        geom_point(size = 2) + 
        # Show a simple linear model fit created separately for each gender:
        geom_smooth(method = lm, se = FALSE, alpha = 0.5) + 
        scale_color_brewer(palette = "Paired") +
        labs(x = "Age", y = "GPA")

student_interaction_fit <- lm(OVERALL_GPA ~ (AGE_AT_TERM_START + SEX)^2, data = students)

# To print a short summary of the model:
student_interaction_fit

# Place two plots next to one another:
par(mfrow = c(1, 2))

# Show residuals vs predicted values:
plot(student_interaction_fit, which = 1)

# A normal quantile plot on the residuals:
plot(student_interaction_fit, which = 2)

# Fit a reduced model:
student_main_effect_fit <-  lm(OVERALL_GPA ~ AGE_AT_TERM_START + SEX, data = students) 

# Compare the two:
anova(student_main_effect_fit, student_interaction_fit)

summary(student_interaction_fit)

student_new_values <- data.frame(SEX = "M", AGE_AT_TERM_START = 18:23)

predict(student_interaction_fit, student_new_values)

# Add a missing value to the prediction set
student_new_values$AGE_AT_TERM_START[1] <- NA

# The predict method for `lm` defaults to `na.pass`:
predict(student_interaction_fit, student_new_values)

# Alternatively 
predict(student_interaction_fit, student_new_values, na.action = na.fail)

predict(student_interaction_fit, student_new_values, na.action = na.omit)

student_corr_res <- map(students %>% select(-PIDM, -TERM_CODE, -SEX, -OVERALL_GPA), 
                        cor.test, y = students$OVERALL_GPA)

# The first of ten results in the vector: 
student_corr_res[[1]]

tidy(student_corr_res[[1]])

student_corr_res %>% 
        # Convert each to a tidy format; `map_dfr()` stacks the data frames 
        map_dfr(tidy, .id = "predictor") %>% 
        ggplot(aes(x = fct_reorder(predictor, estimate))) + 
        geom_point(aes(y = estimate)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
        labs(x = NULL, y = "Correlation with GPA")

split_by_gender <- 
        students %>% 
        group_nest(SEX) 
split_by_gender

model_by_gender <- 
        split_by_gender %>% 
        mutate(model = map(data, ~ lm(OVERALL_GPA ~ AGE_AT_TERM_START, data = .x)))
model_by_gender

model_by_gender %>% 
        mutate(coef = map(model, tidy)) %>% 
        select(SEX, coef) %>% 
        unnest(cols = c(coef))

#Chapter 4 ----
#Code-along

dim(students)

tidymodels_prefer()

ggplot(students, aes(x = OVERALL_GPA)) + 
        geom_histogram(bins = 50, col= "white")

#Not going to log-transform
#AGE_AT_TERM_START, FOR_CREDIT_COURSES, TOTAL_EVE_BILL_HR, GERMANTOWN_BILL_HR
#TAK_PARK_BILL_HR, PAID_AMT are skewed right
#Only 17 students with OFF_CAMPUS_BILL_HRS
#TOTAL_DAY_BILL_HR are skewed left
#Many variables have mostly 0

#Chapter 5 ----
#Code along
library(tidymodels)
tidymodels_prefer()

# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(501)

# Save the split information for an 80/20 split of the data
students_split <- initial_split(students, prop = 0.80)
students_split

students_train <- training(students_split)
students_test  <-  testing(students_split)

dim(students_train)

set.seed(502)
students_split <- initial_split(students, prop = 0.80, strata = OVERALL_GPA)
students_train <- training(students_split)
students_test  <-  testing(students_split)

dim(students_train)

student_lm_model <- 
        linear_reg() %>% 
        set_engine("lm")

student_lm_form_fit <- 
        student_lm_model %>% 
        fit(OVERALL_GPA ~ AGE_AT_TERM_START + SEX, data = students_train)

student_lm_xy_fit <- 
        student_lm_model %>% 
        fit_xy(
                x = students_train %>% select(AGE_AT_TERM_START, SEX),
                y = students_train %>% pull(OVERALL_GPA)
        )

student_lm_form_fit

student_lm_xy_fit

student_lm_form_fit %>% extract_fit_engine()

student_lm_form_fit %>% extract_fit_engine() %>% vcov()

student_model_res <- 
        student_lm_form_fit %>% 
        extract_fit_engine() %>% 
        summary()

# The model coefficient table is accessible via the `coef` method.
student_param_est <- coef(student_model_res)
class(student_param_est)
student_param_est

tidy(student_lm_form_fit)

student_test_small <- students_test %>% slice(1:5)
predict(student_lm_form_fit, new_data = student_test_small)

student_test_small %>% 
        select(OVERALL_GPA) %>% 
        bind_cols(predict(student_lm_form_fit, student_test_small)) %>% 
        # Add 95% prediction intervals to the results:
        bind_cols(predict(student_lm_form_fit, student_test_small, type = "pred_int"))

student_tree_model <- 
        decision_tree(min_n = 2) %>% 
        set_engine("rpart") %>% 
        set_mode("regression")

student_tree_fit <- 
        tree_model %>% 
        fit(OVERALL_GPA ~ AGE_AT_TERM_START + SEX, data = students_train)

student_test_small %>% 
        select(OVERALL_GPA) %>% 
        bind_cols(predict(student_tree_fit, student_test_small))

#Chapter 7 ----
#A Model Workflow

#Data Transformation for Future Chapters ----
library(tidymodels)
source("DataConnection.R")
students_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202124,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE,
               MSFSTDN_SEX, MSFSTDN_OVERALL_GPA, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FOR_CREDIT_COURSES, MSFSTDN_TOTAL_DAY_BILL_HR,
               MSFSTDN_TOTAL_EVE_BILL_HR, MSFSTDN_ROCKVILLE_BILL_HR,
               MSFSTDN_GERMANTOWN_BILL_HR, MSFSTDN_TAK_PARK_BILL_HR,
               MSFSTDN_OFF_CAMPUS_BILL_HRS) %>% 
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_"))

finaid_backup <- tbl(con, "MSFFAIS") %>% 
        filter(MSFFAIS_COLLEC_YR_S5 >= 2018) %>% 
        select(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL, MSFFAIS_PAID_AMT) %>%
        collect() %>% 
        group_by(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL) %>% 
        summarize(MSFFAIS_PAID_AMT = sum(MSFFAIS_PAID_AMT)) %>% 
        rename_with(~str_remove(., "MSFFAIS_"))

scores_backup <- tbl(con, "SORTEST") %>% 
        filter(SORTEST_TESC_CODE %in% c("S01", "S02", "S03", "S04",
                                        "S05", "S06", "S07", "S08",
                                        "S09", "S11", "S12", "S13",
                                        "S14", "S15")) %>% 
        select(SORTEST_PIDM, SORTEST_TESC_CODE, SORTEST_TEST_DATE, SORTEST_TEST_SCORE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "SORTEST_"))
DBI::dbDisconnect(con)

scores <- scores_backup %>% 
        select(PIDM) %>% 
        distinct() %>% 
        mutate(TookSAT = 1)

students <- students_backup %>% 
        filter(SEX %in% c("M","F")) %>% 
        left_join(finaid_backup, by = c("PIDM", "TERM_CODE" = "TERM_CODE_FALL")) %>% 
        left_join(scores) %>% 
        replace_na(list(PAID_AMT = 0, TookSAT = 0))

set.seed(502)
students_split <- initial_split(students, prop = 0.80, strata = OVERALL_GPA)
students_train <- training(students_split)
students_test  <-  testing(students_split)

student_lm_model <- linear_reg() %>% set_engine("lm")
####

tidymodels_prefer()

student_lm_model <- 
        linear_reg() %>% 
        set_engine("lm")

student_lm_wflow <- 
        workflow() %>% 
        add_model(student_lm_model)

student_lm_wflow

student_lm_wflow <- 
        student_lm_wflow %>% 
        add_formula(OVERALL_GPA ~ AGE_AT_TERM_START + SEX)

student_lm_wflow

student_lm_fit <- fit(student_lm_wflow, students_train)
student_lm_fit

predict(student_lm_fit, students_test %>% slice(1:3))

student_lm_fit %>% update_formula(OVERALL_GPA ~ AGE_AT_TERM_START)

student_lm_wflow <- 
        student_lm_wflow %>% 
        remove_formula() %>% 
        add_variables(outcome = OVERALL_GPA, predictors = c(AGE_AT_TERM_START, SEX))
student_lm_wflow

fit(student_lm_wflow, students_train)

library(lme4)
lmer(OVERALL_GPA ~ SEX + (TOTAL_DAY_BILL_HR | FOR_CREDIT_COURSES), data = students_train)

model.matrix(OVERALL_GPA ~ SEX + (TOTAL_DAY_BILL_HR | FOR_CREDIT_COURSES), data = students_train)

library(multilevelmod)

student_multilevel_spec <- linear_reg() %>% set_engine("lmer")

student_multilevel_workflow <- 
        workflow() %>%  
        add_variables(outcome = OVERALL_GPA, predictors = c(SEX, TOTAL_DAY_BILL_HR, FOR_CREDIT_COURSES)) %>% 
        add_model(student_multilevel_spec, 
                  formula = OVERALL_GPA ~ SEX + (TOTAL_DAY_BILL_HR | FOR_CREDIT_COURSES))

student_multilevel_fit <- fit(student_multilevel_workflow, data = students_train)
student_multilevel_fit

source("DataConnection.R")
bmgt_fall <- tbl(con, "MSFSTDN_STATIC") %>% 
        filter(str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               MSFSTDN_MAJOR_CODE_1 == "006",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0,
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N") %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FULL_PART_IND) %>% 
        left_join(tbl(con, dbplyr::in_schema("RAVEN", "MSFDEGS")) %>% 
                          select(PIDM, MAJR_CODE_1, GRAD_TERM),
                  by = c("MSFSTDN_PIDM" = "PIDM")
        ) %>% 
        collect()
DBI::dbDisconnect(con)

library(censored)

bmgt <- bmgt_fall %>% 
        mutate(across(c(MSFSTDN_TERM_CODE, GRAD_TERM), as.integer),
               years = round((GRAD_TERM-MSFSTDN_TERM_CODE)/100, 0),
               leftovers = (GRAD_TERM-MSFSTDN_TERM_CODE)/100%%1,
               days = case_when(
                       leftovers == 0 ~ 4/12,
                       leftovers < .09 ~ 5/12,
                       leftovers > .09 ~ 9/12,
                       leftovers == .2 ~ 10/12,
                       leftovers == .9 ~ 11/12
               ),
               futime = years+days,
               fustat = ifelse(!is.na(GRAD_TERM),1,0),
               )

student_parametric_spec <- survival_reg()

student_parametric_workflow <- 
        workflow() %>% 
        add_variables(outcome = c(fustat, futime), 
                      predictors = c(MSFSTDN_AGE_AT_TERM_START, 
                                     MSFSTDN_FULL_PART_IND)) %>% 
        add_model(student_parametric_spec, 
                  formula = Surv(futime, fustat) ~ MSFSTDN_AGE_AT_TERM_START + strata(MSFSTDN_FULL_PART_IND))

student_parametric_fit <- fit(student_parametric_workflow, data = bmgt)
student_parametric_fit

student_location <- list(
        rockville = OVERALL_GPA ~ ROCKVILLE_BILL_HR,
        germantown = OVERALL_GPA ~ GERMANTOWN_BILL_HR,
        takoma = OVERALL_GPA ~ TAK_PARK_BILL_HR,
        off_campus = OVERALL_GPA ~ OFF_CAMPUS_BILL_HRS
)

library(workflowsets)
student_location_models <- workflow_set(preproc = student_location, models = list(lm = lm_model))
student_location_models
student_location_models$info[[1]]
extract_workflow(student_location_models, id = "off_campus_lm")

student_location_models <-
        student_location_models %>%
        mutate(fit = map(info, ~ fit(.x$workflow[[1]], students_train)))
student_location_models
student_location_models$fit[[1]]

student_final_lm_res <- last_fit(student_lm_wflow, students_split)
student_final_lm_res

student_fitted_lm_wflow <- extract_workflow(student_final_lm_res)

collect_metrics(student_final_lm_res)
collect_predictions(student_final_lm_res) %>% slice(1:5)

#Chapter 8 ----
#Feature Engineering with recipes

#Data transformation
library(tidymodels)
source("DataConnection.R")
students_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202124,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE, MSFSTDN_ZIP,
               MSFSTDN_SEX, MSFSTDN_OVERALL_GPA, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FOR_CREDIT_COURSES, MSFSTDN_TOTAL_DAY_BILL_HR,
               MSFSTDN_TOTAL_EVE_BILL_HR, MSFSTDN_ROCKVILLE_BILL_HR,
               MSFSTDN_GERMANTOWN_BILL_HR, MSFSTDN_TAK_PARK_BILL_HR,
               MSFSTDN_OFF_CAMPUS_BILL_HRS) %>% 
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_"))

finaid_backup <- tbl(con, "MSFFAIS") %>% 
        filter(MSFFAIS_COLLEC_YR_S5 >= 2018) %>% 
        select(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL, MSFFAIS_PAID_AMT) %>%
        collect() %>% 
        group_by(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL) %>% 
        summarize(MSFFAIS_PAID_AMT = sum(MSFFAIS_PAID_AMT)) %>% 
        rename_with(~str_remove(., "MSFFAIS_"))

scores_backup <- tbl(con, "SORTEST") %>% 
        filter(SORTEST_TESC_CODE %in% c("S01", "S02", "S03", "S04",
                                        "S05", "S06", "S07", "S08",
                                        "S09", "S11", "S12", "S13",
                                        "S14", "S15")) %>% 
        select(SORTEST_PIDM, SORTEST_TESC_CODE, SORTEST_TEST_DATE, SORTEST_TEST_SCORE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "SORTEST_"))
DBI::dbDisconnect(con)

scores <- scores_backup %>% 
        select(PIDM) %>% 
        distinct() %>% 
        mutate(TookSAT = 1)


students <- students_backup %>% 
        filter(SEX %in% c("M","F")) %>% 
        left_join(finaid_backup, by = c("PIDM", "TERM_CODE" = "TERM_CODE_FALL")) %>% 
        left_join(scores) %>% 
        replace_na(list(PAID_AMT = 0, TookSAT = 0))

set.seed(502)
students_split <- initial_split(students, prop = 0.80, strata = OVERALL_GPA)
students_train <- training(students_split)
students_test  <-  testing(students_split)

student_lm_model <- linear_reg() %>% set_engine("lm")

student_lm_wflow <-
        workflow() %>% 
        add_model(student_lm_model) %>% 
        add_variables(outcome = OVERALL_GPA, predictors = c(AGE_AT_TERM_START, SEX))

student_lm_fit <- fit(student_lm_wflow, students_train)
####

lm(OVERALL_GPA ~ ZIP + log10(AGE_AT_TERM_START) + log10(PAID_AMT+1) + SEX, data = sample_n(students,size = 500))

library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_students <- 
        recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX,
               data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_dummy(all_nominal_predictors())
simple_students

student_lm_wflow <- student_lm_wflow %>%
        remove_variables() %>% 
        add_recipe(simple_students)
student_lm_wflow

student_lm_fit <- fit(student_lm_wflow, students_train)

predict(student_lm_fit, students_test %>% slice(1:3))

# Get the recipe after it has been estimated:
student_lm_fit %>% 
        extract_recipe(estimated = TRUE)

# To tidy the model fit: 
student_lm_fit %>% 
        # This returns the parsnip object:
        extract_fit_parsnip() %>% 
        # Now tidy the linear model object:
        tidy() %>% 
        slice(1:5)

simple_students <- 
        recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX,
               data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors())

ggplot(students_train, aes(x = AGE_AT_TERM_START, y = 10^OVERALL_GPA)) + 
        geom_point(alpha = .2) + 
        facet_wrap(~ SEX) + 
        geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
        scale_x_log10() + 
        scale_y_log10() + 
        labs(x = "Age", y = "GPA")

simple_students <- 
        recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX,
               data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        # AGE_AT_TERM_START is on the log scale from a previous step
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_"))
        
library(patchwork)
library(splines)

student_plot_smoother <- function(deg_free) {
        ggplot(students_train, aes(x = TOTAL_DAY_BILL_HR, y = 10^OVERALL_GPA)) + 
                geom_point(alpha = .2) + 
                scale_y_log10() +
                geom_smooth(
                        method = lm,
                        formula = y ~ ns(x, df = deg_free),
                        color = "lightblue",
                        se = FALSE
                ) +
                labs(title = paste(deg_free, "Spline Terms"),
                     y = "GPA")
}

( student_plot_smoother(2) + student_plot_smoother(5) ) / ( student_plot_smoother(20) + student_plot_smoother(100) )

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
       data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)

tidy(student_rec)

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                      data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01, id = "my_id") %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)

student_lm_wflow <- 
        workflow() %>% 
        add_model(student_lm_model) %>% 
        add_recipe(student_rec)

student_lm_fit <- fit(student_lm_wflow, students_train)

student_estimated_recipe <- 
        student_lm_fit %>% 
        extract_recipe(estimated = TRUE)

tidy(student_estimated_recipe, id = "my_id")

tidy(student_estimated_recipe, number = 4)

student_rec %>% update_role(PIDM, new_role = "identifier")


#Chapter 9 ----
#Judging Model Effectiveness

##Data transformation
library(tidyverse)
library(tidymodels)
source("DataConnection.R")
students_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202124,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE, MSFSTDN_ZIP,
               MSFSTDN_SEX, MSFSTDN_OVERALL_GPA, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FOR_CREDIT_COURSES, MSFSTDN_TOTAL_DAY_BILL_HR,
               MSFSTDN_TOTAL_EVE_BILL_HR, MSFSTDN_ROCKVILLE_BILL_HR,
               MSFSTDN_GERMANTOWN_BILL_HR, MSFSTDN_TAK_PARK_BILL_HR,
               MSFSTDN_OFF_CAMPUS_BILL_HRS, MSFSTDN_FULL_PART_IND) %>%
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_"))

retained_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202224,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_")) %>% 
        rowwise() %>% 
        mutate(to_check = paste(PIDM, TERM_CODE, sep = ","))

finaid_backup <- tbl(con, "MSFFAIS") %>% 
        filter(MSFFAIS_COLLEC_YR_S5 >= 2018) %>% 
        select(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL, MSFFAIS_PAID_AMT) %>%
        collect() %>% 
        group_by(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL) %>% 
        summarize(MSFFAIS_PAID_AMT = sum(MSFFAIS_PAID_AMT)) %>% 
        rename_with(~str_remove(., "MSFFAIS_"))

scores_backup <- tbl(con, "SORTEST") %>% 
        filter(SORTEST_TESC_CODE %in% c("S01", "S02", "S03", "S04",
                                        "S05", "S06", "S07", "S08",
                                        "S09", "S11", "S12", "S13",
                                        "S14", "S15")) %>% 
        select(SORTEST_PIDM, SORTEST_TESC_CODE, SORTEST_TEST_DATE, SORTEST_TEST_SCORE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "SORTEST_"))
DBI::dbDisconnect(con)

scores <- scores_backup %>% 
        select(PIDM) %>% 
        distinct() %>% 
        mutate(TookSAT = 1)


students <- students_backup %>% 
        filter(SEX %in% c("M","F")) %>% 
        left_join(finaid_backup, by = c("PIDM", "TERM_CODE" = "TERM_CODE_FALL")) %>% 
        left_join(scores) %>% 
        replace_na(list(PAID_AMT = 0, TookSAT = 0)) %>% 
        mutate(Retained = factor(paste(PIDM,as.character(as.integer(TERM_CODE)+100), 
                      sep = ",") %in% retained_backup$to_check),
               GPA = factor(
                       case_when(
                               OVERALL_GPA > 3.5 ~ "3.51-4.00",
                               OVERALL_GPA > 2 ~ "2.01-3.50",
                               TRUE ~ "0.00-2.00"
                       )))



set.seed(502)
students_split <- initial_split(students, prop = 0.80, strata = OVERALL_GPA)
students_train <- training(students_split)
students_test  <-  testing(students_split)

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                      data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)
        

student_lm_model <- linear_reg() %>% set_engine("lm")

student_lm_wflow <-
        workflow() %>% 
        add_model(student_lm_model) %>% 
        add_recipe(student_rec)

student_lm_fit <- fit(student_lm_wflow, students_train)
####

student_test_res <- predict(student_lm_fit, new_data = students_test %>% select(-OVERALL_GPA))
student_test_res

student_test_res <- bind_cols(student_test_res, students_test %>% select(OVERALL_GPA))
student_test_res

ggplot(student_test_res, aes(x = OVERALL_GPA, y = .pred)) + 
        # Create a diagonal line:
        geom_abline(lty = 2) + 
        geom_point(alpha = 0.5) + 
        labs(y = "Predicted GPA", x = "GPA") +
        # Scale and size the x- and y-axis uniformly:
        coord_obs_pred()

rmse(student_test_res, truth = OVERALL_GPA, estimate = .pred)

student_metrics <- metric_set(rmse, rsq, mae)
student_metrics(student_test_res, truth = OVERALL_GPA, estimate = .pred)

#Created a logisitic regression for retention
retained_rec <- recipe(Retained ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + 
                               TOTAL_DAY_BILL_HR + FULL_PART_IND + OVERALL_GPA,
                       data = students_train) %>% 
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)

retained_log_model <- logistic_reg() %>% set_engine("glm")

retained_log_wflow <-
        workflow() %>% 
        add_model(retained_log_model) %>% 
        add_recipe(retained_rec)

retained_log_fit <- fit(retained_log_wflow, students_train)

retained_test_res <- predict(retained_log_fit, 
                             new_data = students_test %>% select(-Retained),
                             type = "prob")

retained_test_res <- bind_cols(retained_test_res, 
                               students_test %>% select(Retained),
                               predict(retained_log_fit,
                                       new_data = students_test %>% select(-Retained)))

# A confusion matrix: 
conf_mat(retained_test_res, truth = Retained, estimate = .pred_class)

# Accuracy:
accuracy(retained_test_res, Retained, .pred_class)

# Matthews correlation coefficient:
mcc(retained_test_res, Retained, .pred_class)

# F1 metric:
f_meas(retained_test_res, Retained, .pred_class)

# Combining these three classification metrics together
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(retained_test_res, truth = Retained, estimate = .pred_class)

f_meas(retained_test_res, Retained, .pred_class, event_level = "second")

retained_curve <- roc_curve(retained_test_res, Retained, .pred_FALSE)
retained_curve

roc_auc(retained_test_res, Retained, .pred_FALSE)

autoplot(retained_curve)

#Creating multiclass logisitic regression for GPA ranges
multi_rec <- recipe(GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                    data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)

multi_log_model <- multinom_reg() %>% set_engine("nnet")

multi_log_wflow <-
        workflow() %>% 
        add_model(multi_log_model) %>% 
        add_recipe(multi_rec)

multi_log_fit <- fit(multi_log_wflow, students_train)

multi_test_res <- predict(multi_log_fit, 
                             new_data = students_test %>% select(-GPA),
                             type = "prob")

multi_test_res <- bind_cols(multi_test_res, 
                               students_test %>% select(GPA),
                               predict(multi_log_fit,
                                       new_data = students_test %>% select(-GPA)))

accuracy(multi_test_res, GPA, .pred_class)

mcc(multi_test_res, GPA, .pred_class)

multi_totals <- 
        count(multi_test_res, GPA, name = "totals") %>% 
        mutate(class_wts = totals / sum(totals))
multi_totals

multi_counts <- 
        multi_test_res %>% 
        group_by(GPA, .pred_class) %>% 
        count() %>% 
        ungroup()

# Compute the four sensitivities using 1-vs-all
gpa_one_versus_all <- 
        multi_counts %>% 
        filter(GPA == .pred_class) %>% 
        full_join(multi_totals, by = "GPA") %>% 
        mutate(sens = n / totals)
gpa_one_versus_all

# Three different estimates:
gpa_one_versus_all %>% 
        summarize(
                macro = mean(sens), 
                macro_wts = weighted.mean(sens, class_wts),
                micro = sum(n) / sum(totals)
        )

sensitivity(multi_test_res, GPA, .pred_class, estimator = "macro")
sensitivity(multi_test_res, GPA, .pred_class, estimator = "macro_weighted")
sensitivity(multi_test_res, GPA, .pred_class, estimator = "micro")

roc_auc(multi_test_res, GPA, names(multi_test_res)[1:3])

roc_auc(multi_test_res, GPA, names(multi_test_res)[1:3], estimator = "macro_weighted")

multi_test_res %>% 
        mutate(Resample = c(rep("Fold01", 338),
                            rep("Fold02", 338),
                            rep("Fold03", 338),
                            rep("Fold04", 338),
                            rep("Fold05", 338),
                            rep("Fold06", 338),
                            rep("Fold07", 338),
                            rep("Fold08", 338),
                            rep("Fold09", 338),
                            rep("Fold10", 337))) %>% 
        group_by(Resample) %>% 
        accuracy(GPA, .pred_class)

# Four 1-vs-all ROC curves for each fold
multi_test_res %>% 
        mutate(Resample = c(rep("Fold01", 338),
                            rep("Fold02", 338),
                            rep("Fold03", 338),
                            rep("Fold04", 338),
                            rep("Fold05", 338),
                            rep("Fold06", 338),
                            rep("Fold07", 338),
                            rep("Fold08", 338),
                            rep("Fold09", 338),
                            rep("Fold10", 337))) %>% 
        group_by(Resample) %>% 
        roc_curve(GPA, names(multi_test_res)[1:3]) %>% 
        autoplot()

#Chapter 10 ----
#Resampling for Evaluating Performance

student_rf_model <- 
        rand_forest(trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("regression")

student_rf_wflow <- 
        workflow() %>% 
        add_formula(
                OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR) %>% 
        add_model(student_rf_model) 

student_rf_fit <- student_rf_wflow %>% fit(data = students_train)

my_estimate_perf <- function(model, dat) {
        # Capture the names of the `model` and `dat` objects
        cl <- match.call()
        obj_name <- as.character(cl$model)
        data_name <- as.character(cl$dat)
        data_name <- gsub("students_", "", data_name)
        
        # Estimate these metrics:
        reg_metrics <- metric_set(rmse, rsq)
        
        model %>%
                predict(dat) %>%
                bind_cols(dat %>% select(OVERALL_GPA)) %>%
                reg_metrics(OVERALL_GPA, .pred) %>%
                select(-.estimator) %>%
                mutate(object = obj_name, data = data_name)
}

my_estimate_perf(student_rf_fit, students_train)

my_estimate_perf(student_lm_fit, students_train)

my_estimate_perf(student_rf_fit, students_test)

set.seed(1001)
students_folds <- vfold_cv(students_train, v = 10)
students_folds

# For the first fold:
students_folds$splits[[1]] %>% analysis() %>% dim()

vfold_cv(students_train, v = 10, repeats = 5)

mc_cv(students_train, prop = 9/10, times = 20)

set.seed(1002)
student_val <- validation_split(students_train, prop = 3/4)
student_val

bootstraps(students_train, times = 5)

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
student_rf_res <- 
        student_rf_wflow %>% 
        fit_resamples(resamples = students_folds, control = keep_pred)
student_rf_res

collect_metrics(student_rf_res)

student_assess_res <- collect_predictions(student_rf_res)
student_assess_res

student_assess_res %>% 
        ggplot(aes(x = OVERALL_GPA, y = .pred)) + 
        geom_point(alpha = .15) +
        geom_abline(color = "red") + 
        coord_obs_pred() + 
        ylab("Predicted")

student_under_predicted <- 
        student_assess_res %>% 
        mutate(residual = OVERALL_GPA - .pred) %>% 
        arrange(desc(residual)) %>% 
        slice(1:2)
student_under_predicted

students_train %>% 
        slice(student_under_predicted$.row) %>% 
        select(ZIP, SEX, AGE_AT_TERM_START, FOR_CREDIT_COURSES, OFF_CAMPUS_BILL_HRS, FULL_PART_IND,
               PAID_AMT, TookSAT)

student_val_res <- student_rf_wflow %>% fit_resamples(resamples = student_val)
student_val_res

collect_metrics(student_val_res)

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
studentrf_res <- student_rf_wflow %>% fit_resamples(resamples = students_folds, control = keep_pred)

stopCluster(cl)

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                      data = students_train) %>%
        step_mutate(ZIP = as.factor(stringr::str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)

student_lm_wflow <-  
        workflow() %>% 
        add_recipe(student_rec) %>% 
        add_model(linear_reg() %>% set_engine("lm")) 

student_lm_fit <- student_lm_wflow %>% fit(data = students_train)

# Select the recipe: 
extract_recipe(student_lm_fit, estimated = TRUE)

get_model <- function(x) {
        extract_fit_parsnip(x) %>% tidy()
}

# Test it using: 
get_model(student_lm_fit)

#tidy apparently doesn't work here.
# ctrl <- control_resamples(extract = extract_fit_parsnip)
# 
# student_lm_res <- student_lm_wflow %>%  fit_resamples(resamples = students_folds, control = ctrl)
# student_lm_res
# 
# student_lm_res$.extracts[[1]]
# # To get the results
# student_lm_res$.extracts[[1]][[1]]
# 
# student_all_coef <- map_dfr(student_lm_res$.extracts, ~ .x[[1]][[1]])
# # Show the replicates for a single predictor:
# filter(student_all_coef, term == "PAID_AMT")

#Chapter 11 ----
#Comparing Models with Resampling
#
#Data Transformation
library(tidyverse)
library(tidymodels)
source("DataConnection.R")
students_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202124,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE, MSFSTDN_ZIP,
               MSFSTDN_SEX, MSFSTDN_OVERALL_GPA, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FOR_CREDIT_COURSES, MSFSTDN_TOTAL_DAY_BILL_HR,
               MSFSTDN_TOTAL_EVE_BILL_HR, MSFSTDN_ROCKVILLE_BILL_HR,
               MSFSTDN_GERMANTOWN_BILL_HR, MSFSTDN_TAK_PARK_BILL_HR,
               MSFSTDN_OFF_CAMPUS_BILL_HRS, MSFSTDN_FULL_PART_IND) %>%
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_"))

retained_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202224,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_")) %>% 
        rowwise() %>% 
        mutate(to_check = paste(PIDM, TERM_CODE, sep = ","))

finaid_backup <- tbl(con, "MSFFAIS") %>% 
        filter(MSFFAIS_COLLEC_YR_S5 >= 2018) %>% 
        select(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL, MSFFAIS_PAID_AMT) %>%
        collect() %>% 
        group_by(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL) %>% 
        summarize(MSFFAIS_PAID_AMT = sum(MSFFAIS_PAID_AMT)) %>% 
        rename_with(~str_remove(., "MSFFAIS_"))

scores_backup <- tbl(con, "SORTEST") %>% 
        filter(SORTEST_TESC_CODE %in% c("S01", "S02", "S03", "S04",
                                        "S05", "S06", "S07", "S08",
                                        "S09", "S11", "S12", "S13",
                                        "S14", "S15")) %>% 
        select(SORTEST_PIDM, SORTEST_TESC_CODE, SORTEST_TEST_DATE, SORTEST_TEST_SCORE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "SORTEST_"))
DBI::dbDisconnect(con)

scores <- scores_backup %>% 
        select(PIDM) %>% 
        distinct() %>% 
        mutate(TookSAT = 1)


students <- students_backup %>% 
        filter(SEX %in% c("M","F")) %>% 
        left_join(finaid_backup, by = c("PIDM", "TERM_CODE" = "TERM_CODE_FALL")) %>% 
        left_join(scores) %>% 
        replace_na(list(PAID_AMT = 0, TookSAT = 0)) %>% 
        mutate(Retained = factor(paste(PIDM,as.character(as.integer(TERM_CODE)+100), 
                                       sep = ",") %in% retained_backup$to_check),
               GPA = factor(
                       case_when(
                               OVERALL_GPA > 3.5 ~ "3.51-4.00",
                               OVERALL_GPA > 2 ~ "2.01-3.50",
                               TRUE ~ "0.00-2.00"
                       )))



set.seed(502)
students_split <- initial_split(students, prop = 0.80, strata = OVERALL_GPA)
students_train <- training(students_split)
students_test  <-  testing(students_split)

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                      data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)


student_lm_model <- linear_reg() %>% set_engine("lm")

student_lm_wflow <-
        workflow() %>% 
        add_model(student_lm_model) %>% 
        add_recipe(student_rec)

student_lm_fit <- fit(student_lm_wflow, students_train)

student_rf_model <- 
        rand_forest(trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("regression")

student_rf_wflow <- 
        workflow() %>% 
        add_formula(
                OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR) %>% 
        add_model(student_rf_model) 

set.seed(1001)
students_folds <- vfold_cv(students_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
student_rf_res <- student_rf_wflow %>% fit_resamples(resamples = students_folds, control = keep_pred)
####

library(tidymodels)
tidymodels_prefer()

basic_student_rec <- 
        recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
               data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors())

interaction_student_rec <- 
        basic_student_rec %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_"))

spline_student_rec <- 
        interaction_student_rec %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)

preproc_student <- 
        list(basic = basic_student_rec, 
             interact = interaction_student_rec, 
             splines = spline_student_rec
        )

student_lm_models <- workflow_set(preproc_student, list(lm = linear_reg()), cross = FALSE)
student_lm_models

student_lm_models <- 
        student_lm_models %>% 
        workflow_map("fit_resamples", 
                     # Options to `workflow_map()`: 
                     seed = 1101, verbose = TRUE,
                     # Options to `fit_resamples()`: 
                     resamples = students_folds, control = keep_pred)

student_lm_models

collect_metrics(student_lm_models) %>% 
        filter(.metric == "rmse")

four_student_models <- 
        as_workflow_set(random_forest = student_rf_res) %>% 
        bind_rows(student_lm_models)
four_student_models

library(ggrepel)
autoplot(four_student_models, metric = "rsq") +
        geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
        theme(legend.position = "none")

student_rsq_indiv_estimates <- 
        collect_metrics(four_student_models, summarize = FALSE) %>% 
        filter(.metric == "rsq") 

student_rsq_wider <- 
        student_rsq_indiv_estimates %>% 
        select(wflow_id, .estimate, id) %>% 
        pivot_wider(id_cols = "id", names_from = "wflow_id", values_from = ".estimate")

corrr::correlate(student_rsq_wider %>% select(-id), quiet = TRUE)

student_rsq_indiv_estimates %>% 
        mutate(wflow_id = reorder(wflow_id, .estimate)) %>% 
        ggplot(aes(x = wflow_id, y = .estimate, group = id, color = id)) + 
        geom_line(alpha = .5, lwd = 1.25) + 
        theme(legend.position = "none")

student_rsq_wider %>% 
        with( cor.test(basic_lm, splines_lm) ) %>% 
        tidy() %>% 
        select(estimate, starts_with("conf"))

compare_student_lm <- 
        student_rsq_wider %>% 
        mutate(difference = splines_lm - basic_lm)

lm(difference ~ 1, data = compare_student_lm) %>% 
        tidy(conf.int = TRUE) %>% 
        select(estimate, p.value, starts_with("conf"))

# Alternatively, a paired t-test could also be used: 
student_rsq_wider %>% 
        with( t.test(splines_lm, basic_lm, paired = TRUE) ) %>%
        tidy() %>% 
        select(estimate, p.value, starts_with("conf"))

library(tidyposterior)
library(rstanarm)

# The rstanarm package creates copious amounts of output; those results
# are not shown here but are worth inspecting for potential issues. The
# option `refresh = 0` can be used to eliminate the logging. 
student_rsq_anova <-
        perf_mod(
                four_student_models,
                metric = "rsq",
                prior_intercept = rstanarm::student_t(df = 1),
                chains = 4,
                iter = 5000,
                seed = 1102
        )

student_model_post <- 
        student_rsq_anova %>% 
        # Take a random sample from the posterior distribution
        # so set the seed again to be reproducible. 
        tidy(seed = 1103) 

glimpse(student_model_post)

student_model_post %>% 
        mutate(model = forcats::fct_inorder(model)) %>%
        ggplot(aes(x = posterior)) + 
        geom_histogram(bins = 50, color = "white", fill = "blue", alpha = 0.4) + 
        facet_wrap(~ model, ncol = 1)

autoplot(student_rsq_anova) +
        geom_text_repel(aes(label = workflow), nudge_x = 1/8, nudge_y = 1/100) +
        theme(legend.position = "none")

student_rqs_diff <-
        contrast_models(student_rsq_anova,
                        list_1 = "splines_lm",
                        list_2 = "basic_lm",
                        seed = 1104)

student_rqs_diff %>% 
        as_tibble() %>% 
        ggplot(aes(x = difference)) + 
        geom_vline(xintercept = 0, lty = 2) + 
        geom_histogram(bins = 50, color = "white", fill = "red", alpha = 0.4)

summary(student_rqs_diff) %>% 
        select(-starts_with("pract"))

summary(student_rqs_diff, size = 0.02) %>% 
        select(contrast, starts_with("pract"))

autoplot(student_rsq_anova, type = "ROPE", size = 0.02) +
        geom_text_repel(aes(label = workflow)) +
        theme(legend.position = "none")

#Chapter 12 ----
#Model Tuning and the Dangers of Overfitting
#
#Data Transformation
library(tidyverse)
library(tidymodels)
source("DataConnection.R")
students_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202124,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               str_sub(MSFSTDN_OIRA_STUDENT_TYPE, end = 1) == "N",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE, MSFSTDN_ZIP,
               MSFSTDN_SEX, MSFSTDN_OVERALL_GPA, MSFSTDN_AGE_AT_TERM_START,
               MSFSTDN_FOR_CREDIT_COURSES, MSFSTDN_TOTAL_DAY_BILL_HR,
               MSFSTDN_TOTAL_EVE_BILL_HR, MSFSTDN_ROCKVILLE_BILL_HR,
               MSFSTDN_GERMANTOWN_BILL_HR, MSFSTDN_TAK_PARK_BILL_HR,
               MSFSTDN_OFF_CAMPUS_BILL_HRS, MSFSTDN_FULL_PART_IND) %>%
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_"))

retained_backup <- tbl(con, "MSFSTDN") %>% 
        filter(MSFSTDN_TERM_CODE > 201800,
               MSFSTDN_TERM_CODE < 202224,
               str_sub(MSFSTDN_TERM_CODE, start = -2) == "20",
               MSFSTDN_FOR_CREDIT_COURSES > 0,
               MSFSTDN_TOTAL_BILL_HR > 0) %>% 
        select(MSFSTDN_PIDM, MSFSTDN_TERM_CODE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "MSFSTDN_")) %>% 
        rowwise() %>% 
        mutate(to_check = paste(PIDM, TERM_CODE, sep = ","))

finaid_backup <- tbl(con, "MSFFAIS") %>% 
        filter(MSFFAIS_COLLEC_YR_S5 >= 2018) %>% 
        select(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL, MSFFAIS_PAID_AMT) %>%
        collect() %>% 
        group_by(MSFFAIS_PIDM, MSFFAIS_TERM_CODE_FALL) %>% 
        summarize(MSFFAIS_PAID_AMT = sum(MSFFAIS_PAID_AMT)) %>% 
        rename_with(~str_remove(., "MSFFAIS_"))

scores_backup <- tbl(con, "SORTEST") %>% 
        filter(SORTEST_TESC_CODE %in% c("S01", "S02", "S03", "S04",
                                        "S05", "S06", "S07", "S08",
                                        "S09", "S11", "S12", "S13",
                                        "S14", "S15")) %>% 
        select(SORTEST_PIDM, SORTEST_TESC_CODE, SORTEST_TEST_DATE, SORTEST_TEST_SCORE) %>% 
        collect() %>% 
        rename_with(~str_remove(., "SORTEST_"))
DBI::dbDisconnect(con)

scores <- scores_backup %>% 
        select(PIDM) %>% 
        distinct() %>% 
        mutate(TookSAT = 1)


students <- students_backup %>% 
        filter(SEX %in% c("M","F")) %>% 
        left_join(finaid_backup, by = c("PIDM", "TERM_CODE" = "TERM_CODE_FALL")) %>% 
        left_join(scores) %>% 
        replace_na(list(PAID_AMT = 0, TookSAT = 0)) %>% 
        mutate(Retained = factor(paste(PIDM,as.character(as.integer(TERM_CODE)+100), 
                                       sep = ",") %in% retained_backup$to_check),
               GPA = factor(
                       case_when(
                               OVERALL_GPA > 3.5 ~ "3.51-4.00",
                               OVERALL_GPA > 2 ~ "2.01-3.50",
                               TRUE ~ "0.00-2.00"
                       )))



set.seed(502)
students_split <- initial_split(students, prop = 0.80, strata = OVERALL_GPA)
students_train <- training(students_split)
students_test  <-  testing(students_split)

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                      data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = 0.01) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = 2)


student_lm_model <- linear_reg() %>% set_engine("lm")

student_lm_wflow <-
        workflow() %>% 
        add_model(student_lm_model) %>% 
        add_recipe(student_rec)

student_lm_fit <- fit(student_lm_wflow, students_train)

student_rf_model <- 
        rand_forest(trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("regression")

student_rf_wflow <- 
        workflow() %>% 
        add_formula(
                OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR) %>% 
        add_model(student_rf_model) 

set.seed(1001)
students_folds <- vfold_cv(students_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
student_rf_res <- student_rf_wflow %>% fit_resamples(resamples = students_folds, control = keep_pred)
####

neural_net_spec <- 
        mlp(hidden_units = tune()) %>%
        set_mode("regression") %>%
        set_engine("keras")

student_rec <- recipe(OVERALL_GPA ~ ZIP + AGE_AT_TERM_START + PAID_AMT + SEX + TOTAL_DAY_BILL_HR,
                      data = students_train) %>%
        step_mutate(ZIP = as.factor(str_sub(ZIP, end = 5))) %>% 
        step_log(AGE_AT_TERM_START, base = 10) %>% 
        step_log(PAID_AMT, base = 10, offset = 1) %>% 
        step_other(ZIP, threshold = tune()) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_interact( ~ AGE_AT_TERM_START:starts_with("SEX_")) %>% 
        step_ns(TOTAL_DAY_BILL_HR, deg_free = tune("bill_hr df"))

student_recipes_param <- extract_parameter_set_dials(student_rec)
student_recipes_param

student_wflow_param <- 
        workflow() %>% 
        add_recipe(student_rec) %>% 
        add_model(neural_net_spec) %>% 
        extract_parameter_set_dials()
student_wflow_param

# identify the parameter using the id value:
student_wflow_param %>% extract_parameter_dials("threshold")

extract_parameter_set_dials(student_rec) %>% 
        update(threshold = threshold(c(0, 0.2)))

rf_spec <- 
        rand_forest(mtry = tune()) %>% 
        set_engine("ranger", regularization.factor = tune("regularization")) %>%
        set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_spec)
rf_param

student_pca_rec <- 
        recipe(OVERALL_GPA ~ ., data = students_train) %>% 
        step_rm(Retained, GPA) %>% 
        # Select the square-footage predictors and extract their PCA components:
        step_normalize(contains("BILL_HR")) %>% 
        # Select the number of components needed to capture 95% of
        # the variance in the predictors. 
        step_pca(contains("BILL_HR"), threshold = .95)

student_updated_param <- 
        workflow() %>% 
        add_model(rf_spec) %>% 
        add_recipe(student_pca_rec) %>% 
        extract_parameter_set_dials() %>% 
        finalize(students_train)
student_updated_param

student_updated_param %>% extract_parameter_dials("mtry")
