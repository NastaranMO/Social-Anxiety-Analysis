# ==== Load Libraries ====
# Clean the column names to a suitable format
library(janitor)
library(ggplot2)
library(dplyr)
library(glmnet)
# ==== Set Working Directory ====
setwd("/Users/nastaran/documents/dev/Stat/A3")
getwd()

# ==== Load the Dataset ====
ds <- read.csv("enhanced_anxiety_dataset.csv")

str(ds)

# Convert char types to Factor
ds$Gender <- as.factor(ds$Gender)
ds$Occupation <- as.factor(ds$Occupation)
ds$Smoking <- as.factor(ds$Smoking)
ds$Family.History.of.Anxiety <- as.factor(ds$Family.History.of.Anxiety)
ds$Dizziness <- as.factor(ds$Dizziness)
ds$Medication <- as.factor(ds$Medication)
ds$Recent.Major.Life.Event <- as.factor(ds$Recent.Major.Life.Event)

# ==== Analysis ====
str(ds)
summary(ds)

# Check missing values
colSums(is.na(ds))
sum(is.na(ds))
# Clean the column names "affeine.Intake..mg.day." to "caffeine_intake_mg_day"
ds <- clean_names(ds)

# Analysis if categorical variables
cat_vars <- names(ds)[sapply(ds, is.factor)]

for (var in cat_vars) {
  print(
    ggplot(ds, aes_string(x = var, y = "anxiety_level_1_10")) +
      geom_boxplot() +
      labs(title = paste("Anxiety Level by", var), x = var, y = "Anxiety Level")
  )
}

# Analysis if numerical variables
num_vars <- sapply(ds, is.numeric)
num_vars_names <- names(ds)[num_vars]
num_vars_names

for (var in num_vars_names[num_vars_names != "anxiety_level_1_10"]) {
  print(
    ggplot(ds, aes_string(x = var, y = "anxiety_level_1_10")) +
      geom_jitter(height = 0.2, alpha = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Anxiety Level vs", var))
  )
}

numeric_ds <- ds[num_vars]
hist(cor(numeric_ds), main = "Pairwise correlations", cex = .7,
     cex.lab = .7, cex.axis = .7, cex.main = .7)

# ==== Fit LASSO ====
set.seed(42)
y <- ds$anxiety_level_1_10
n <- length(y)
x <- model.matrix(anxiety_level_1_10 ~ ., data=ds)
train_idx <- sample(1:n, size=n*0.8)
mod_l <- cv.glmnet(x = x[train_idx, ], y = y[train_idx], nfolds = 10)
mod_l
plot(mod_l)
coef(mod_l, s = "lambda.1se")
coef(mod_l, s = "lambda.min")
# ==== Fit Ridge ====
mod_r <- cv.glmnet(x = x[train_idx, ], y = y[train_idx], nfolds = 10, alpha=0)
mod_r
plot(mod_r)
coef(mod_r, s = "lambda.1se")
# Scale the features
ds_scaled <- scale(ds[, num_vars_names])
