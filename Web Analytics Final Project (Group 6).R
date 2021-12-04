################################################################################
################### Web and Network Analytics Assignment########################
################################################################################

# Load dataset from github
coders_response <- read.table("https://raw.githubusercontent.com/freeCodeCamp/2017-new-coder-survey/ed8a2c5118209fa26cc823fd33fedcf6fe5661ec/clean-data/2017-fCC-New-Coders-Survey-Data.csv",
                               header=TRUE, sep=",")

# importing the dataset (only if dataset couldn't be loaded from github)
# coders_response <- read.csv('2017-fCC-New-Coders-Survey-Data.csv',header = TRUE,
#                             na.strings = NA, stringsAsFactors = FALSE) 

# install necessary packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("gmodels")
# install.packages("scales")
# install.packages("ggrepel")
# install.packages("forcats")
# install.packages("tidyverse")
# install.packages("glmnet")
# install.packages("InformationValue")
# install.packages("caret")
# install.packages("car")
# install.packages("corrplot")
# install.packages("readr")
# install.packages("rpart")
# install.packages("rpart.plot")

# load necessary packages
library(dplyr)
library(ggplot2) # Data visualization
library(tidyr)
library(gmodels)
library(scales) # to calculate percentages 
library(ggrepel) # to create labels for pie chart
library(forcats)
library(tidyverse)
library(glmnet) # fit glms with elastic net
library(InformationValue) # analyze how well our model performs on the test dataset
library(caret) # to compute importance of each predictor variable
library(car) # to calculate VIF values of each variable in the model
library(corrplot) # to generate correlation plot
library(readr)
library(rpart)
library(rpart.plot)

################################################################################
########################## Exploratory Analysis ################################
################################################################################

# get an overview of the structure of the data
glimpse(coders_response)

# Get summary of data set
summary(coders_response)

# View the first 5 rows
head(coders_response)

# Get number of rows and columns
ncol(coders_response)
nrow(coders_response)

# Get column names
names(coders_response)

# Get class information
str(coders_response)

# Check distinct Age Values
unique(coders_response$Age)

max(coders_response$Age, na.rm = T)

######################### Data Preparation #####################################

coders_response$Agegrp[coders_response$Age > 0  & coders_response$Age <= 10] <- '1-10'
coders_response$Agegrp[coders_response$Age > 10 & coders_response$Age <= 20] <- '11-20'
coders_response$Agegrp[coders_response$Age > 20 & coders_response$Age <= 30] <- '21-30'
coders_response$Agegrp[coders_response$Age > 30 & coders_response$Age <= 40] <- '31-40'
coders_response$Agegrp[coders_response$Age > 40 & coders_response$Age <= 50] <- '41-50'
coders_response$Agegrp[coders_response$Age > 50 & coders_response$Age <= 60] <- '51-60'
coders_response$Agegrp[coders_response$Age > 60] <- '60 AND ABOVE'

# check distinct Agegrp Values
unique(coders_response$Agegrp)

# check distinct Gender Values
unique(coders_response$Gender)

# check frequency of each gender group
table(coders_response$Gender)

# Merge agender, trans and genderqueer into one category to main data frame
coders_response$Gendergrp[coders_response$Gender == 'agender'] <- 'others'
coders_response$Gendergrp[coders_response$Gender == 'trans'] <- 'others'
coders_response$Gendergrp[coders_response$Gender == 'genderqueer'] <- 'others'
coders_response$Gendergrp[is.na(coders_response$Gender)] <- 'others'
coders_response$Gendergrp[coders_response$Gender == 'female'] <- 'female'
coders_response$Gendergrp[coders_response$Gender == 'male'] <- 'male'

# check distinct Gender Values
unique(coders_response$Gendergrp)

# check frequency of each gender group
table(coders_response$Gendergrp)

# count frequency for each age group by gender group
age_gender_freq = coders_response %>%
  # select(Age, Gender)%>%
  group_by(Agegrp, Gendergrp)%>%
  summarize(count=n())

###########################  Pie chart of Gender ###############################

table(Gender = coders_response$Gender) %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  mutate(prop = percent(Freq / sum(Freq))) %>%
  ggplot(aes( x= '', y=Freq,  fill = fct_inorder(Gender))) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer("Blues") + 
  geom_label_repel(aes(label = prop), size=5,show.legend = F, nudge_x = 2) +
  guides(fill = guide_legend(title = "Gender")) +
  ggtitle('Pie Chart of Gender Distribution')

################ Bar Chart of Age Distribution by Gender Group ##################

coders_response %>% 
  select(Age, Gendergrp)%>%
  filter(Gendergrp %in% c('female', 'male', 'others')) %>%
  filter(Age > 0) %>%
  ggplot(aes(x=Age)) + 
  geom_bar(aes(fill=Gendergrp), position = position_dodge()) +
  ggtitle('Distribution of Age with Gender Group') +
  labs(x ="AGE", Y="COUNT") + 
  scale_fill_discrete(name="GENDER GROUP")

############### Geom jitter plot of income by gender groups ###################

coders_response %>% 
  select(Gendergrp, Income) %>% 
  ggplot(aes(Gendergrp,Income, color = Gendergrp, fill=Gendergrp)) +
  geom_jitter() +
  ggtitle("Income by genders") 


################# Jitter plot of Income by AGe group ###########################

coders_response %>% 
  select(Agegrp, Income)%>%
  filter(!is.na(Income),!is.na(Agegrp))%>%
  ggplot(aes(Agegrp,Income, color = Agegrp, fill=Agegrp)) +
  geom_jitter() +
  labs(title = "Jitter Plot of Income by Age Group",
       x ="AGE GROUP", Y="INCOME") 

#################### Box Plot of Income by Age Group ###########################

coders_response %>% 
  select(Agegrp, Income)%>%
  filter(!is.na(Income),!is.na(Agegrp))%>%
  ggplot(aes(Agegrp,Income, color = Agegrp, fill=Agegrp)) +
  geom_boxplot() +
  stat_summary(fun=median, geom="point", shape=20, size=4, 
               color="black", fill="black") + 
  labs(title = "Box Plot of Income by Age Group",
       caption = "(black dot indicates Median Income)",
       x ="AGE GROUP", Y="INCOME") 

################## Heat map of Current job vs ideal job ########################

table(current_job = coders_response$EmploymentStatus,
      ideal_job = coders_response$JobPref) %>%
  as.data.frame() %>%
  ggplot(aes(ideal_job,current_job))+
  geom_tile(aes(fill = Freq), colour = "white")+
  coord_fixed(ratio = 1)+
  theme(axis.text.x=element_text(angle=45,vjust = 1, hjust = 1)) +
  labs(title = "Current Job VS Ideal Job",
       x = "IDEAL JOB", y = "CURRENT JOB") +
  geom_text(aes(label=Freq)) +
  scale_fill_gradient(low = "#24b4ab", high = "white")


################################################################################
####################### Logistic Regression Analysis ###########################
################################################################################

# Find the maximum learning hours per week
max(coders_response$HoursLearning, na.rm = T)

# create a subset of the data with only our variables of interest
dataSubset <- coders_response %>%
  filter(Age > 10) %>% # filter data that doesn't make sense
  filter(HoursLearning < 105) %>% 
  mutate(IsMale = as.integer(Gender == "male")) %>%
  mutate(SchoolDegree = as.numeric(factor(SchoolDegree, 
                                          levels=c("no high school (secondary school)", 
                                                   "some high school", 
                                                   "high school diploma or equivalent (GED)",
                                                   "trade, technical, or vocational training",
                                                   "some college credit, no degree",
                                                   "associate's degree",
                                                   "bachelor's degree",
                                                   "master's degree (non-professional)",
                                                   "professional degree (MBA, MD, JD, etc.)",
                                                   "Ph.D.")))) %>%
  select(Age, AttendedBootcamp, SchoolDegree,
         Income,IsUnderEmployed, IsMale,
         HoursLearning, MonthsProgramming ) %>%
  na.omit() # remove non-numeric values

# Understand the structure of the data
glimpse(dataSubset)
summary(dataSubset)

# Compute correlation matrix
correlations <- cor(dataSubset[,1:8])

# Compute correlation plot
par(mfrow = c(1,1)) 
corrplot(correlations, method="circle")

# covert all categorical variables to factors
dataSubset$AttendedBootcamp <- as.factor(dataSubset$AttendedBootcamp)
dataSubset$SchoolDegree <- as.factor(dataSubset$SchoolDegree)
dataSubset$IsMale <- as.factor(dataSubset$IsMale)

# Take another look at the datset
glimpse(dataSubset)
summary(dataSubset)

# Understand the data set by checking the freqeuncy count, mean and proportion
table(dataSubset$AttendedBootcamp)
table(dataSubset$IsUnderEmployed)
mean(dataSubset$IsUnderEmployed)

# Check on the proportion
prop.table(table(dataSubset$IsUnderEmployed))

# find the total rows in dataset
nrow(dataSubset)

###################### Create Training and Test Samples #########################

#  initialize a pseudorandom number generator to make this example reproducible 
set.seed(1)

# Use 70% of data set as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(dataSubset), replace=TRUE, prob=c(0.7,0.3))
train <- dataSubset[sample, ]
test <- dataSubset[!sample, ]  

############################# Fitting a model ###################################

# convert input variables to matrix
input <- train %>%
  select(-IsUnderEmployed) %>% # Exclude variable to be predicted
  data.matrix()  %>%
  na.omit() 

# get a vector with our output variable
output <- train$IsUnderEmployed

# use 10-fold cross-validation to fit a bunch of models using elastic net
cv_fit <- cv.glmnet(input, output, family = "binomial") 

# get coefficients for the best model
coef(cv_fit, s = "lambda.min")

# get a (non-sparse) matrix of the coefficients for the best model
coef_matrix <- coef(cv_fit, s = "lambda.min") %>% 
  as.matrix()
coef_matrix

# get the variables with a coefficient that's not 0 
variables <- row.names(coef_matrix)[coef_matrix != 0] %>% 
              setdiff("(Intercept)") #remove the intercept 
# check on the variables that only has our selected features
variables

# turn list of formulas into a variable
variables_selected <- paste(variables, collapse="+")
variables_selected

formula <- paste("IsUnderEmployed ~ ",variables_selected,sep = "") %>%
  as.formula()
formula

# fit logistic regression model with general linear model (glm)
model <- glm(formula, data = train, 
             family = binomial(link="logit")) 

# view model summary
summary(model)

# Check the confidence level for this model
confint(model)

# added-variable (partial regression) plots for model
avPlots(model)


########################### Assessing Model Fit ################################

# Variable of importance
varImp(model)

# calculate VIF values for each predictor variable in our model
vif(model)

# # to analyze the table of deviance
# anova(model, test="Chisq")

####################### Use the model to make predictions ######################

# calculate probability of default for each individual in test data set
predicted <- predict(model, test, type="response")

############################## Model Diagnostics ###############################

# find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$IsUnderEmployed, predicted)[1]
optimal

# calculate total classifications error rate
misClassError(test$IsUnderEmployed, predicted, threshold=optimal)

# plot the ROC curve
plotROC(test$IsUnderEmployed, predicted)

################################################################################
########################## Decision Tree Analysis ##############################
################################################################################

#remove all NA observations
na.coders_response <- coders_response[complete.cases(coders_response[ , 5:6]),]

#build models
tree <- rpart(IsSoftwareDev~ Age + Income, method = "class", na.coders_response)

#plot & visualize decision tree, remove scientific notation
rpart.plot(tree, box.palette="RdBu",digits = -2)







