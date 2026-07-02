## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(statease)

## ----eval=FALSE---------------------------------------------------------------
# # Install from CRAN
# install.packages("statease")
# 
# # Load the package
# library(statease)

## -----------------------------------------------------------------------------
set.seed(42)

tutorial_data <- data.frame(
  student_id = 1:90,
  method     = rep(c("Traditional", "Online", "Hybrid"), each = 30),
  gender     = rep(c("Male", "Female"), times = 45),
  exam_score = c(
    round(rnorm(30, mean = 65, sd = 10)),
    round(rnorm(30, mean = 72, sd = 10)),
    round(rnorm(30, mean = 78, sd = 10))
  ),
  pre_test = c(
    round(rnorm(30, mean = 55, sd = 10)),
    round(rnorm(30, mean = 58, sd = 10)),
    round(rnorm(30, mean = 57, sd = 10))
  ),
  age        = round(rnorm(90, mean = 22, sd = 3)),
  passed     = rbinom(90, 1, prob = 0.7)
)

head(tutorial_data)

## -----------------------------------------------------------------------------
result <- describe(tutorial_data$exam_score,
                   var_name = "Exam Score")
print(result)

## -----------------------------------------------------------------------------
result$mean
result$sd
result$skew_label

## -----------------------------------------------------------------------------
males   <- tutorial_data$exam_score[tutorial_data$gender == "Male"]
females <- tutorial_data$exam_score[tutorial_data$gender == "Female"]

result <- ttest_interpret(
  males, females,
  var_name = "Exam Score by Gender"
)
print(result)

## -----------------------------------------------------------------------------
result <- ttest_interpret(
  tutorial_data$exam_score,
  mu = 70,
  var_name = "Exam Score"
)
print(result)

## -----------------------------------------------------------------------------
result <- ttest_interpret(
  tutorial_data$exam_score,
  tutorial_data$pre_test,
  paired   = TRUE,
  var_name = "Score Improvement"
)
print(result)

## -----------------------------------------------------------------------------
result <- anova_interpret(
  exam_score ~ method,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
result <- anova2_interpret(
  exam_score ~ method * gender,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
result <- manova_interpret(
  cbind(exam_score, pre_test) ~ method,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
tutorial_data$passed_label <- ifelse(tutorial_data$passed == 1,
                                      "Pass", "Fail")

result <- chisq_interpret(
  tutorial_data$method,
  tutorial_data$passed_label
)
print(result)

## -----------------------------------------------------------------------------
result <- cor_interpret(
  tutorial_data$pre_test,
  tutorial_data$exam_score,
  var1_name = "Pre-Test Score",
  var2_name = "Exam Score"
)
print(result)

## -----------------------------------------------------------------------------
result <- cor_interpret(
  tutorial_data$pre_test,
  tutorial_data$exam_score,
  method    = "spearman",
  var1_name = "Pre-Test Score",
  var2_name = "Exam Score"
)
print(result)

## -----------------------------------------------------------------------------
result <- reg_interpret(
  exam_score ~ pre_test,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
result <- mlr_interpret(
  exam_score ~ pre_test + age,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
result <- logistic_interpret(
  passed ~ pre_test + age,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
result <- mannwhitney_interpret(
  males, females,
  var_name = "Exam Score by Gender"
)
print(result)

## -----------------------------------------------------------------------------
result <- wilcoxon_interpret(
  tutorial_data$exam_score,
  tutorial_data$pre_test,
  var_name = "Score Improvement"
)
print(result)

## -----------------------------------------------------------------------------
result <- kruskal_interpret(
  exam_score ~ method,
  data = tutorial_data
)
print(result)

## -----------------------------------------------------------------------------
result <- interpret_p(
  0.03,
  context = "teaching method effect on exam scores"
)
print(result)

## -----------------------------------------------------------------------------
# Descriptive statistics
analyze(x = tutorial_data$exam_score, var_name = "Exam Score")

## -----------------------------------------------------------------------------
# Auto t-test
analyze(
  x        = males,
  y        = females,
  var_name = "Exam Score by Gender"
)

## -----------------------------------------------------------------------------
# Auto ANOVA
analyze(formula = exam_score ~ method, data = tutorial_data)

## -----------------------------------------------------------------------------
# Auto non-parametric
analyze(
  formula  = exam_score ~ method,
  data     = tutorial_data,
  nonparam = TRUE
)

## -----------------------------------------------------------------------------
# Auto regression
analyze(formula = exam_score ~ pre_test, data = tutorial_data)

## -----------------------------------------------------------------------------
# Auto MANOVA
analyze(
  formula = cbind(exam_score, pre_test) ~ method,
  data    = tutorial_data
)

## -----------------------------------------------------------------------------
result <- fisher_interpret(
  tutorial_data$method,
  tutorial_data$passed_label
)
print(result)

## -----------------------------------------------------------------------------
pre_pass  <- sample(c("Yes","No"), 90, replace = TRUE, 
                    prob = c(0.4, 0.6))
post_pass <- sample(c("Yes","No"), 90, replace = TRUE, 
                    prob = c(0.7, 0.3))

result <- mcnemar_interpret(pre_pass, post_pass)
print(result)

## -----------------------------------------------------------------------------
friedman_data <- data.frame(
  score   = c(23,45,12,67,34,89,56,43,78,90,11,34),
  time    = rep(c("T1","T2","T3"), each = 4),
  subject = rep(1:4, times = 3)
)

result <- friedman_interpret(score ~ time | subject, 
                             data = friedman_data)
print(result)

## -----------------------------------------------------------------------------
result <- check_assumptions(
  "ttest",
  x = males,
  y = females
)
print(result)

## -----------------------------------------------------------------------------
result <- power_interpret(
  "ttest.two",
  effect_size = 0.5
)
print(result)

## ----eval=FALSE---------------------------------------------------------------
# statease::run_app()

