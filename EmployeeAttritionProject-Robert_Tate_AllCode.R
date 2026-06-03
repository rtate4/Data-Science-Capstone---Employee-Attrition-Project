##########################################################
# Load dataset
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(recipes)) install.packages("recipes", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
if(!require(themis)) install.packages("themis", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(purrr)
library(gbm)
library(randomForest)
library(rpart)
library(rpart.plot)
library(recipes)
library(ROSE)
library(themis)
library(MLmetrics)

dataurl <- "https://raw.githubusercontent.com/rtate4/Data-Science-Capstone---Employee-Attrition-Project/refs/heads/main/HR-Employee-Attrition.csv"

attrition_data <- read.csv(dataurl)

# Check for missing data
attrition_data |>
  filter(if_any(everything(), is.na))

# Check for duplicates
attrition_data |>
  count(EmployeeNumber) |>
  filter(n > 1)

# Create training and holdout test datasets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = attrition_data$Attrition, times = 1, p = 0.20, list = FALSE)
att_train <- attrition_data[-test_index,]
att_test <- attrition_data[test_index,]


##########################################################
# Data analysis and visualization 
##########################################################

# Summary of original data file

dataset_summary <- attrition_data |>
  summarize(across(everything(), list(DataType = ~ class(.)[1], MinValue  = ~ min(as.character(.), na.rm = TRUE), MaxValue  = ~ max(as.character(.), na.rm = TRUE)))) |>
  pivot_longer(cols = everything(), names_to = c("Field", ".value"), names_sep = "_")

dataset_summary2 <- map_df(names(attrition_data), function(col_name) {
  col_data <- attrition_data[[col_name]]
  tibble(Field = col_name, `Data Type` = class(col_data)[1],
    `Smallest Value` = if(all(is.na(col_data))) NA_character_ else as.character(min(col_data, na.rm = TRUE)),
    `Largest Value`  = if(all(is.na(col_data))) NA_character_ else as.character(max(col_data, na.rm = TRUE)))})

#Analysis of training set

# Overall counts (Attrition = Yes means employee left)
att_train |>
  group_by(Attrition) |>
  summarize(Total = n())

#Look for trends in individual data points

data_range <- function(f){
  list(Low = min(f), Avg = mean(f), High = max(f))
}

# High, low, and average of all numeric data ponts
summary_one <- att_train |>
  select(Age, DailyRate, DistanceFromHome, Education, EnvironmentSatisfaction, HourlyRate, JobInvolvement, 
         JobLevel, JobSatisfaction, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike, PerformanceRating, 
         RelationshipSatisfaction, StandardHours, StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, 
         YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager) |>
  pivot_longer(cols = everything(), names_to = "Data_Point", values_to = "Value") |>
  group_by(Data_Point) |>
  reframe(stats = list(data_range(Value))) |>
  unnest_wider((stats))
print(summary_one, n = 24)
  
summary_travel <- att_train |>
  group_by(BusinessTravel) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_travel

summary_gender <- att_train |>
  group_by(Gender) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_gender

summary_department <- att_train |>
  group_by(Department) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_department

summary_field <- att_train |>
  group_by(EducationField) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_field

summary_jobtitle <- att_train |>
  group_by(JobRole) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_jobtitle

summary_MaritalStatus <- att_train |>
  group_by(MaritalStatus) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_MaritalStatus

summary_OverTime <- att_train |>
  group_by(OverTime) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())
summary_OverTime

# Create charts for various factors

ggplot(att_train, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Gender", x = "Gender", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
ggplot(att_train, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Department", x = "Department", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
ggplot(att_train, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Job Title", x = "Job Title", y = "Employees", fill = "Left Company") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

Sys.sleep(1)
att_train |>
  mutate(age_groups = cut(Age, breaks = 5),
         age_groups = as.character(age_groups),
         age_groups = str_replace_all(age_groups, "\\(", ""),
         age_groups = str_replace_all(age_groups, "\\]", ""),
         age_groups = str_replace_all(age_groups, ",", " - "),
         age_groups = factor(age_groups, levels = unique(age_groups[order(Age)]))) |>
  ggplot(aes(x = age_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Age", x = "Age Grouping", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
att_train |>
  mutate(tenure_groups = cut(YearsAtCompany, breaks = 5),
         tenure_groups = as.character(tenure_groups),
         tenure_groups = str_replace_all(tenure_groups, "\\(", ""),
         tenure_groups = str_replace_all(tenure_groups, "\\]", ""),
         tenure_groups = str_replace_all(tenure_groups, ",", " - "),
         tenure_groups = factor(tenure_groups, levels = unique(tenure_groups[order(YearsAtCompany)]))) |>
  ggplot(aes(x = tenure_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Tenure", x = "Tenure Grouping (Years)", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
att_train |>
  mutate(salary_groups = cut(HourlyRate, breaks = 5),
         salary_groups = as.character(salary_groups),
         salary_groups = str_replace_all(salary_groups, "\\(", ""),
         salary_groups = str_replace_all(salary_groups, "\\]", ""),
         salary_groups = str_replace_all(salary_groups, ",", " - "),
         salary_groups = factor(salary_groups, levels = unique(salary_groups[order(HourlyRate)]))) |>
  ggplot(aes(x = salary_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Pay (Hourly Rate)", x = "Pay Grouping", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
att_train |>
  mutate(satisfaction_groups = cut(JobSatisfaction, breaks = 4, labels = c(1, 2,3, 4)),
         satisfaction_groups = as.character(satisfaction_groups),
         satisfaction_groups = str_replace_all(satisfaction_groups, "\\(", ""),
         satisfaction_groups = str_replace_all(satisfaction_groups, "\\]", ""),
         satisfaction_groups = str_replace_all(satisfaction_groups, ",", " - "),
         satisfaction_groups = factor(satisfaction_groups, levels = unique(satisfaction_groups[order(JobSatisfaction)]))) |>
  ggplot(aes(x = satisfaction_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Satisfaction Score", x = "Satisfacton Score", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
att_train |>
  mutate(promotion_groups = cut(YearsSinceLastPromotion, breaks = 5),
         promotion_groups = as.character(promotion_groups),
         promotion_groups = str_replace_all(promotion_groups, "\\(", ""),
         promotion_groups = str_replace_all(promotion_groups, "\\]", ""),
         promotion_groups = str_replace_all(promotion_groups, ",", " - "),
         promotion_groups = factor(promotion_groups, levels = unique(promotion_groups[order(YearsSinceLastPromotion)]))) |>
  ggplot(aes(x = promotion_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Years Since Last Promotion", x = "Grouping", y = "Employees", fill = "Left Company") +
  theme_minimal()

Sys.sleep(1)
att_train |>
  mutate(satisfaction_groups = cut(JobSatisfaction, breaks = 4, labels = c(1, 2,3, 4)),
         satisfaction_groups = as.character(satisfaction_groups),
         satisfaction_groups = str_replace_all(satisfaction_groups, "\\(", ""),
         satisfaction_groups = str_replace_all(satisfaction_groups, "\\]", ""),
         satisfaction_groups = str_replace_all(satisfaction_groups, ",", " - "),
         satisfaction_groups = factor(satisfaction_groups, levels = unique(satisfaction_groups[order(JobSatisfaction)]))) |>
  group_by(Department) |>
  ggplot(aes(x = satisfaction_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Department and Satisfaction Score", x = "Satisfacton Score", y = "Employees", fill = "Left Company") +
  facet_wrap(~Department, axes = "all", axis.labels = "all", ncol = 2) +
  theme_minimal()

Sys.sleep(1)
att_train |>
  mutate(salary_groups = cut(HourlyRate, breaks = 5),
         salary_groups = as.character(salary_groups),
         salary_groups = str_replace_all(salary_groups, "\\(", ""),
         salary_groups = str_replace_all(salary_groups, "\\]", ""),
         salary_groups = str_replace_all(salary_groups, ",", " - "),
         salary_groups = factor(salary_groups, levels = unique(salary_groups[order(HourlyRate)]))) |>
  group_by(JobRole) |>
  ggplot(aes(x = salary_groups, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(name = "Employee Status", values = c("No" = "seagreen", "Yes" = "firebrick"), labels = c("No" = "Remain", "Yes" = "Left")) +
  labs(title = "Emplyee Status By Job Title and Pay (Hourly)", x = "Pay Grouping (Hourly)", y = "Employees", fill = "Left Company") +
  facet_wrap(~JobRole, axes = "all", axis.labels = "all", ncol = 2) +
  theme(plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 15)),
        legend.position = "inside", 
        legend.position.inside = c(0.75, 0.075), 
        panel.spacing = unit(0.5, "lines"))

# Change character data to factors

att_train <- att_train |>
  select(EmployeeNumber, Age, BusinessTravel, Department, DistanceFromHome, Education, EducationField, Gender, HourlyRate, JobInvolvement, JobLevel, JobRole, 
         JobSatisfaction, MaritalStatus, NumCompaniesWorked, OverTime, PercentSalaryHike, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, 
         TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, Attrition) |>
  mutate(across(where(is.character), as.factor))

att_test <- att_test |>
  select(EmployeeNumber, Age, BusinessTravel, Department, DistanceFromHome, Education, EducationField, Gender, HourlyRate, JobInvolvement, JobLevel, JobRole, 
         JobSatisfaction, MaritalStatus, NumCompaniesWorked, OverTime, PercentSalaryHike, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, 
         TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, Attrition) |>
  mutate(across(where(is.character), as.factor))

##########################################################
# Machine Learning Models
##########################################################

# Model 1: glm

# Train model and print performance from cross-validation
cval_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary, sampling = "smote")
model_01 <- train(Attrition ~ . - EmployeeNumber, data = att_train, method = "glm", family = "binomial", metric = "AUC", trControl = cval_control)
print(model_01)
confusionMatrix(model_01)

# Use model to make predictions with test set and print performance
model_01_predictions <- predict(model_01, newdata = att_test)
cm_01 <- confusionMatrix(model_01_predictions, att_test$Attrition)
cm_01_result <- c(Model = "Logistic Regression (glm)", cm_01$overall["Accuracy"], cm_01$byClass[c("Sensitivity", "Specificity", "Precision", "F1", "Balanced Accuracy", "Prevalence")])
cm_01$table

# Model 2: Random Forest

# Train model and print performance from cross-validation
model_02 <- train(Attrition ~ . - EmployeeNumber, data = att_train, method = "rf", metric = "AUC", trControl = cval_control)
print(model_02)
confusionMatrix(model_02)

# Use model to make predictions with test set and print performance
model_02_predictions <- predict(model_02, newdata = att_test)
cm_02 <- confusionMatrix(model_02_predictions, att_test$Attrition)
cm_02_result <- c(Model = "Random Forest (rf)", cm_02$overall["Accuracy"], cm_02$byClass[c("Sensitivity", "Specificity", "Precision", "F1", "Balanced Accuracy", "Prevalence")])
cm_02$table

# Model 3: gbm

# Train model and print performance from cross-validation
model_03 <- train(Attrition ~ . - EmployeeNumber, data = att_train, method = "gbm", metric = "AUC", trControl = cval_control, verbose = FALSE)
print(model_03)
confusionMatrix(model_03)

# Use model to make predictions with test set and print performance
model_03_predictions <- predict(model_03, newdata = att_test)
cm_03 <- confusionMatrix(model_03_predictions, att_test$Attrition)
cm_03_result <- c(Model = "Gradient Boosting (gbm)", cm_03$overall["Accuracy"], cm_03$byClass[c("Sensitivity", "Specificity", "Precision", "F1", "Balanced Accuracy", "Prevalence")])
cm_03$table

# Combined Model 1 & Model 3 Classification

model_04_predictions <- bind_cols(att_test |> select(EmployeeNumber, Attrition), m1_prediction = model_01_predictions, m3_prediction = model_03_predictions)
model_04_predictions <- model_04_predictions |>
  mutate(Emp_Category = case_when(
    m3_prediction == "Yes" ~ "High Risk",
    m3_prediction == "No" & m1_prediction == "Yes" ~ "At Risk",
    TRUE ~ "Normal Risk"
  ))

model_04_Summary <- model_04_predictions |>
  group_by(Emp_Category) |>
  summarize(Leave = sum(Attrition == "Yes"), Total = n(), Pct_Leave = sum(Attrition == "Yes")/n())


# Compare Model Results

model_comparison <- bind_rows(cm_01_result, cm_02_result, cm_03_result)


# Get factor importance information from models and create visualization
Sys.sleep(1)
m01_importance <- varImp(model_01, scale = TRUE)$importance |>
  as.data.frame() |>
  rownames_to_column(var = "Variable") |>
  slice_max(Overall, n = 10)
ggplot(m01_importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Drivers of Employee Attrition - Logistic Regression Model", x = "Variables", y = "Relative Importance (0-100)") +
  theme_minimal()

Sys.sleep(1)
m02_importance <- varImp(model_02, scale = TRUE)$importance |>
  as.data.frame() |>
  rownames_to_column(var = "Variable") |>
  slice_max(Overall, n = 10)
ggplot(m02_importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "seagreen4") +
  coord_flip() +
  labs(title = "Drivers of Employee Attrition - Random Forest Model", x = "Variables", y = "Relative Importance (0-100)") +
  theme_minimal()

Sys.sleep(1)
m03_importance <- varImp(model_03, scale = TRUE)$importance |>
  as.data.frame() |>
  rownames_to_column(var = "Variable") |>
  slice_max(Overall, n = 10)
ggplot(m03_importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "mediumpurple3") +
  coord_flip() +
  labs(title = "Drivers of Employee Attrition - Gradient Boosting Model", x = "Variables", y = "Relative Importance (0-100)") +
  theme_minimal()

# Create a single decision tree for analysis
Sys.sleep(1)
tree_rules <- rpart.control(minsplit = 50, minbucket = 20, maxdepth = 5)
dec_tree <- rpart(Attrition ~ . - EmployeeNumber, data = att_train, method = "class", control = tree_rules)
rpart.plot(dec_tree, type = 2, extra = 104, main = "Employee Attrition Risk", under = TRUE, cex = 0.8)

