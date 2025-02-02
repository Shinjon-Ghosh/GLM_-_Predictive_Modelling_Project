# Install and load the readxl package
install.packages("readxl")
library(readxl)
library(lubridate)
library(dplyr)
library(statmod)
library(GLMsData)
library(tidyverse)

# Specify the Excel file path
file_path <-("F:\\Applied Statistics Course - ISU\\GLM & Predictive Modeling\\Project\\Netflix Userbase.xlsx")
# Read the Excel file
nf <- read_excel(file_path)

# Explore the data
head(nf)
str(nf)
##Data Processing
date_columns <- c("Last Payment Date")  

for (col in date_columns) {
  nf[[col]] <- ymd(nf[[col]]) 
  
}

head(nf)
str(nf)
my_date <- as.Date("2023-07-25")
days_difference <- my_date - nf$'Last Payment Date'
nf$active <- ifelse(days_difference < 30, 0, 1)
head(nf)
str(nf)

num_zeros <- sum(nf$active == 0)
num_zeros
num_ones <- sum(nf$active ==1)
num_ones

# Convert the categorical variable to a factor
sb0 <- c("Basic")
nf$Subscription <- factor(ifelse(nf$Subscription %in% sb0, 0, 1))
print(nf)
sb1 <- sum(nf$Subscription == "0")
sb1

nf$Gender <- factor(nf$Gender, levels = c("Female", "Male"))
summary(nf$Gender)
gd0 <- sum(nf$Gender == "Male")
gd0

# List of countries to label as 0
ct0 <- c("Brazil", "Canada", "Italy")

# Create a binary factor based on the specified countries
nf$Country <- factor(ifelse(nf$Country %in% ct0, 0, 1))
print(nf)
num0 <- sum(nf$Country == 0)
num0
num1 <- sum(nf$Country ==1)
num1

dv0 <-c("Laptop")
nf$Device <- factor(ifelse(nf$Device %in% dv0, 0, 1))
print(nf)



## Model Analysis
nf1 <- glm(active ~ Gender + Age*Revenue + Country*Subscription, data = nf, 
           family = binomial("probit"))
summary(nf1)

nf2 <- glm(active ~ Gender + Age*Revenue + Country*Subscription, data = nf, 
           family = binomial("logit"))
summary(nf2)

nf3 <- glm(active ~ Gender + Age*Revenue + Country*Subscription, data = nf, 
            family = binomial("cloglog"))
summary(nf3)


nf4 <- glm(active ~ Age + Revenue + Country +Subscription + Gender, data = nf,
           family = binomial(link ="logit"))
summary(nf4)

nf5 <- glm(active ~ Country +Subscription + Gender, data = nf,
           family = binomial(link ="logit"))
summary(nf5)



## for,back,step
initial_model <- glm(active ~ 1, data = nf, family = binomial(link="probit"))
full_model <- glm(active ~ Age + Gender + Revenue + Country + Subscription + Device,
                  data = nf, family = binomial(link = "probit"))

nf6 <- step(initial_model, direction = "forward",
                  scope = list(lower=initial_model, upper = full_model))
nf7 <- step(full_model, direction = "backward",
                  scope = list(lower=initial_model, upper = full_model))
nf8 <- step(initial_model, direction = "both",
                  scope = list(lower=initial_model, upper = full_model))
summary(nf6)
summary(nf7)
summary(nf8)

nf9 <- glm(active ~ Gender + Country*Subscription, data = nf, 
           family = binomial(link = "probit"))
summary(nf9)

nf10 <- glm(active ~ Gender + Country*Subscription, data = nf, 
           family = binomial(link = "logit"))
summary(nf10)

nf11 <- glm(active ~ Gender + Country*Subscription, data = nf, 
           family = binomial(link = "cloglog"))
summary(nf11)



##
cook_distances <- cooks.distance(nf9)
cook_distances

inf.mea = influence.measures(nf9)
inf.mea


library(car)
vif(nf2, type = "terms")
vif(nf3, type = "terms")
vif(nf9)

##
Pearson <- residuals(nf9, type = "pearson")
Pearson

deviance_resid <- residuals(nf9, type = "deviance")
deviance_resid



##quantile residual
qqnorm(qresid(nf9),
       ylim = c(-3, 3), 
       xlim = c(-3,3))
qqline(qresid(nf9))


fitted_values <- nf9$fitted.values
fitted_values
quantile_resid <- qresid(nf9)
quantile_resid

plot(fitted_values, quantile_resid,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Fitted Values")
abline(h = 0, col = "blue")

plot(fitted_values, deviance_resid,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red")


confint(nf9,level= 0.95)

printCoefmat(coef(summary(nf9)))


nf12 <- glm(active ~ Gender + Country*Subscription + Age + Revenue, data = nf, family = binomial(link = "probit"))
lrt_result <- anova(nf9, nf12, test = "Chisq")
print("Likelihood Ratio Test Result:")
print(lrt_result)

nf0 <- glm(active ~ 1, data=nf, family=binomial)
z.score <- glm.scoretest(nf0, nf$active)
P.score <- 2*(1-pnorm(abs(z.score)))
c(z.score, P.score)

nf13 <- glm(active ~ Gender + Country*Subscription, data=nf,
           family=binomial(link = "probit"))
z.score <- glm.scoretest(nf13, nf$active)
P.score <- 2*(1-pnorm(abs(z.score)))
c(z.score, P.score)

##
library(ResourceSelection)
hoslem_result <- hoslem.test(nf9$y, fitted(nf9), g = 3)
print(hoslem_result)

##
odds_ratios <- exp(coef(nf9))
print(odds_ratios)





