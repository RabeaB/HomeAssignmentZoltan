rm(list=ls(all=TRUE))

#######################################################

## Home Assignment Zoltan ##

## Question 1 ##

#######################################################


# set working directory using the dropdown menu

# load packages
library(psych)
library(dplyr)
library(gsheet)
library(ggplot2)
library(lm.beta)
library(car)
library(rgl)
library(lmtest) # bptest 
library(sandwich)


# load data

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv") 


### custom error plot function from Zoltans code

error_plotter <- function(mod, col = "black", x_var = NULL){
  
  mod_vars = as.character(mod$call[2])
  
  data = eval(parse(text = as.character(mod$call[3])))
  
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  
  
  
  data$pred = predict(mod)
  
  
  
  if(x == "1" & is.null(x_var)){x = "response_ID"
  
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}
  
  
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  
  abline(mod)
  
  
  
  for(i in 1:nrow(data)){
    
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    
    abline(v = data[i,x], lty = 2, col = col)
    
  }
  
  
  
}



##############################################################################
#                         Check data for irregularities
##############################################################################


############
# CHECK THE DATA SET
############

View(data_sample_1)

# Describe and Histogrammes
describe(data_sample_1$pain)
hist(data_sample_1$pain, breaks = 20, main="Histogram of Pain", xlab="Pain", col="lightgray", xlim=c(0, 10))

describe(data_sample_1$age)
hist(data_sample_1$age, breaks = 20, main="Histogram of Age", xlab="Age", col="lightgray", xlim=c(0, 100))

describe(data_sample_1$STAI_trait)
hist(data_sample_1$STAI_trait, breaks = 20, main="Histogram of STAI", xlab="Anxiety", col="lightgray", xlim=c(20, 60))

describe(data_sample_1$pain_cat)
hist(data_sample_1$pain_cat, breaks = 20, main="Histogram of Pain Catastrophizing", xlab="Pain Catastrophizing", col="lightgray", xlim=c(10, 50))

describe(data_sample_1$cortisol_serum)
hist(data_sample_1$cortisol_serum, breaks = 20, main="Histogram of Cortisol Serum", xlab="Cortisol in Serum", col="lightgray", xlim=c(0, 10))

describe(data_sample_1$cortisol_saliva)
hist(data_sample_1$cortisol_saliva, breaks = 20, main="Histogram of Cortisol Saliva", xlab="Cortisol in Saliva", col="lightgray", xlim=c(0, 10))

describe(data_sample_1$mindfulness)
hist(data_sample_1$mindfulness, breaks = 20, main="Histogram of Mindfulness", xlab="Mindfulness", col="lightgray", xlim=c(0, 7))


table(data_sample_1$sex)

# Scatterplot for Pain and Age

plot(pain ~ age, data = data_sample_1, main="Scatterplot Pain and Age", xlab ="Age", ylab="Pain")

plot(pain ~ sex, data = data_sample_1, main="Scatterplot Pain and Sex", xlab ="Sex", ylab="Pain")

plot(pain ~ STAI_trait, data = data_sample_1, main="Scatterplot Pain and STAI", xlab ="STAI", ylab="Pain")

plot(pain ~ pain_cat, data = data_sample_1, main="Scatterplot Pain and Pain Catastrophizing", xlab ="Pain Cat", ylab="Pain")

plot(pain ~ cortisol_serum, data = data_sample_1, main="Scatterplot Pain and Cortisol in Serum", xlab ="Cortisol", ylab="Pain")

plot(pain ~ cortisol_saliva, data = data_sample_1, main="Scatterplot Pain and Cortisol in Saliva", xlab ="Cortisol", ylab="Pain")

plot(pain ~ mindfulness, data = data_sample_1, main="Scatterplot Pain and Mindfulness", xlab ="Mindfulness", ylab="Pain")


######################## cleaning the data ##################################

# some cases with invalid data were identified, these need to be excluded

### Age ### 
#-> ID_28 with age of 222 --> is not possible! Exclude data!
data_cleaned = data_sample_1

data_cleaned = data_cleaned[-which(data_cleaned[, "ID"] == "ID_28"), ]

# In Case we want to recode and not get rid of it!
#describe(data_sample_1$age)

#data_cleaned = data_sample_1 # create a copy of the data where which will be cleaned 

#data_cleaned[data_cleaned[, "age"] == "222", "age"] = 22 
#data_cleaned[, "age"] = as.numeric(data_cleaned[, "age"]) 

describe(data_cleaned$age)
plot(pain ~ age, data = data_cleaned, main="Scatterplot Pain and Age", xlab ="Age", ylab="Pain")

### Mindfulness ###
# total score of questionnaire must be between 1 and 6 --> exclude those with scores below 6!

data_cleaned = data_cleaned[-which(data_cleaned[, "ID"] == "ID_112"), ]
data_cleaned = data_cleaned[-which(data_cleaned[, "ID"] == "ID_146"), ]

### Descriptive Statistics ###
describe(data_cleaned$age)
table(data_cleaned$sex)


###############
# MODEL DIAGNOSITCS FOR MODEL 1
###############
#check the variables included in model 1 for influential outliers

mod1<-lm(pain~age + sex, data=data_cleaned)
mod1
summary(mod1)
# calculate Cook's Distance to identify influential outliers

     # Influential Observations
     # added variable plots 
av.Plots(mod1)
     # Cook's D plot
     # identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data_cleaned)-length(mod1$coefficients)-1)) 
plot(mod1, which=4, cook.levels=cutoff)
     # Influence Plot 
influencePlot(mod1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# points with relevant Cook's Distance identified --> check if regression coefficients are
# substantially different!

  #      StudRes        Hat       CookD
  #10  2.5296486 0.01905635 0.040034063
  #44  1.8674918 0.10095891 0.128470160
  #56 -2.4818985 0.01480606 0.029857384
  #84  0.3623292 0.05547559 0.002584824
  #86 -2.1034465 0.03627274 0.054302064

mod1_C<-lm(pain~age + sex, data=data_cleaned, subset=-44)
mod1_C

data_cleaned1=data_cleaned[-which(data_cleaned[, "ID"] == "ID_44"), ]
data_cleaned1=data_cleaned[-which(data_cleaned[, "ID"] == "ID_10"), ]
data_cleaned1=data_cleaned[-which(data_cleaned[, "ID"] == "ID_56"), ]
data_cleaned1=data_cleaned[-which(data_cleaned[, "ID"] == "ID_84"), ]
data_cleaned1=data_cleaned[-which(data_cleaned[, "ID"] == "ID_86"), ]

mod1_CA<-lm(pain~age + sex, data=data_cleaned1)
mod1_CA
mod1

# the difference between the coeefficients is minimal therefore, we will not exclude them

rm(mod1_C) # remove testing models for better overview in the environment
rm(mod1_CA)
rm(data_cleaned1)

###############
# MODEL DIAGNOSTICS FOR MODEL 2
###############
#check the variables included in model 2 for influential outliers


mod2<-lm(pain~age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data=data_cleaned)
mod2
summary(mod2)
# calculate Cook's Distance to identify influential outliers

# Influential Observations
# added variable plots 
av.Plots(mod1)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data_cleaned)-length(mod2$coefficients)-1)) 
plot(mod2, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(mod2, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# points with relevant Cook's Distance identified --> check if regression coefficients are
# substantially different!

  #      StudRes        Hat       CookD
  #44   1.8849619 0.11494198 0.056707906
  #45   0.7036449 0.11768321 0.008282875
  #71   2.6247549 0.10738773 0.099665315
  #90   0.5067229 0.17671415 0.006923784
  #100 -2.8531819 0.01665681 0.016448490

#excluding the case with the largest cooks distance
mod2_C<-lm(pain~age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data=data_cleaned, subset=-71)
mod2_C
mod2
#difference is minimal therefore no data is excluded

#exclude all found
data_cleaned2=data_cleaned[-which(data_cleaned[, "ID"] == "ID_44"), ]
data_cleaned2=data_cleaned[-which(data_cleaned[, "ID"] == "ID_45"), ]
data_cleaned2=data_cleaned[-which(data_cleaned[, "ID"] == "ID_71"), ]
data_cleaned2=data_cleaned[-which(data_cleaned[, "ID"] == "ID_90"), ]
data_cleaned2=data_cleaned[-which(data_cleaned[, "ID"] == "ID_100"), ]

mod2_CA<-lm(pain~age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data=data_cleaned2)
mod2_CA
mod2
#difference is minimal, therefore not data is excluded

rm(mod2_C)  # remove testing models for better overview in the environment
rm(mod2_CA)
rm(data_cleaned2)

######################################################################################
############################Check the final model to see if the assumptions of linear regression hold true
######################################################################################

####### Normality (of the residuals) #########

# Histogram for visual check

hist( x = residuals( mod1 ), # data are the residuals
      xlab = "Value of residual", # x-axis label
      main = "Check Normality for Model 1",
      breaks = 20 # lots of breaks
)

hist( x = residuals( mod2 ), # data are the residuals
xlab = "Value of residual", # x-axis label
main = "Check Normality for Model 2",
breaks = 20 # lots of breaks
)

# QQ-Plot

plot( x = mod1, which = 2 )
plot( x = mod2, which = 2 )

#Shapiro Wilk Test for all variables

shapiro.test(data_cleaned$pain)
shapiro.test(data_cleaned$sex)
shapiro.test(data_cleaned$age)
shapiro.test(data_cleaned$STAI_trait)
shapiro.test(data_cleaned$pain_cat)
shapiro.test(data_cleaned$cortisol_serum)
shapiro.test(data_cleaned$cortisol_saliva)
shapiro.test(data_cleaned$mindfulness)

# Shapiro-Wilk-Test significant for STAI_trait

hist(data_sample_1$STAI_trait, breaks = 20, main="Histogram of STAI_trait", xlab="STAI_trait", col="lightgray", xlim=c(0, 100))

# But since the other indicators are ok and the significance is not great,
# STAI_trait will remain in the analysis

############ Linearity (of the relationship) ################

residualPlots( model = mod1)
residualPlots( model = mod2)

# Tukey Test is not significant, therefore we can assume linearity 


############# Homogeneity of variance (also called homoscedasticity) ################

plot(x = mod1, which = 3)
plot(x = mod2, which = 3)

ncvTest(mod1)
ncvTest(mod2)

# from the non-constant variance test, we can see that there is no violation of the homogeneity of variance


############# that there is no excess multicollinearity ("uncorrelated predictors" in Navarro's words)

vif( mod = mod1) 
vif( mod = mod2)

# only the two cortisol measures show high levels, but we would assume a high correlation of these values, so they stay

######################################################################################
# If you find anything amiss during these checks, make the appropriate decision
# or correction and report your findings and actions
######################################################################################

# As described above, there is not enough evidence to exclude any data at this point



#########################################################################################
#Report the results of model 1 and model 2: For both: model test statistics
#(R2, F, df, and p value), statistics describing the coefficients of the predictors
#in a table format (unstandardized regression coefficients and 95% confidence intervals,
#standardized regression coefficients (B and Beta values), and p values). 
#########################################################################################

summary(mod1)
summary(mod2)
AIC(mod1)
AIC(mod2)
confint(mod1)
confint(mod2)
confint(mod1, level= .99) # interpret more cautiously bc normality of STAI
confint(mod2, level = .99)
lm.beta(mod1)
lm.beta(mod2)

######################################################################

# Write up Regression Equation for Model 
#####################################################################


##################################################################################
#Compare the two models in terms of how much variance they explain of 
# pain's variability in the sample. Report Akaike information criterion (AIC) 
# for both models and the F test statistic and p value of the model comparison 
#returned by the anova() function
##################################################################################


anova(mod1, mod2)

AIC(mod1, mod2)
AIC(mod1)




###########################################################################################
###########################################################################################

#                                  RESEARCH QUESTION 2 
#                                 Backward Regression

###########################################################################################
###########################################################################################


#####################
# First, you will have to run a backward regression to confirm her claim.
# She used the following variables as predictors in the initial model 
# (before stepwise exclusion): age, sex, weight, STAI, pain catastrophizing,
# mindfulness, serum cortisol. She excluded salivary cortisol because, as she wrote, 
# "it was essentially identical to serum cortisol". Run a backward regression using 
# these predictors as an initial model.

    # Use data file 1 to run this regression (the one called 'home_sample_1.csv', 
    # the same as the one used in assignment 1). If you have excluded any cases (participants)
    # from analysis in assignment 1 for any reason, exclude them here as well. 
    # (Before you run the actual backward regression, you will have to re-run the data 
    # and model diagnostics, as there is a new variable, "weight" that was not used in your
    # previous model).
#####################

# use data from data_cleaned because it has all cases excluded that needed to be excluded

summary(data_cleaned)

# generate regression with the above listed variables

full.model<-lm(pain~age + sex + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness, data=data_cleaned)

####################################################################
# Run data and model diagnostics
####################################################################

# added variable plots 
av.Plots(full.model)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data_cleaned)-length(full.model$coefficients)-1)) 
plot(full.model, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(full.model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# points with relevant Cook's Distance identified --> check if regression coefficients are
# substantially different!

#      StudRes        Hat       CookD
#19   2.3367042 0.06528929 0.046288462
#24   2.5263909 0.02631235 0.020808410
#44   1.8265260 0.11413305 0.052899081
#45   0.6180260 0.11883896 0.006465948
#90   0.5461446 0.17590533 0.007996085
#100 -2.9429856 0.02355736 0.024842255

#excluding the case with the largest cooks distance
full.mod_C<-lm(pain~age + sex + pain_cat + STAI_trait + cortisol_serum + mindfulness + weight, data=data_cleaned, subset=-44)
full.mod_C
full.model

#difference is minimal therefore no data is excluded


#exclude all found
data_cleaned4=data_cleaned[-which(data_cleaned[, "ID"] == "ID_19"), ]
data_cleaned4=data_cleaned[-which(data_cleaned[, "ID"] == "ID_24"), ]
data_cleaned4=data_cleaned[-which(data_cleaned[, "ID"] == "ID_44"), ]
data_cleaned4=data_cleaned[-which(data_cleaned[, "ID"] == "ID_45"), ]
data_cleaned4=data_cleaned[-which(data_cleaned[, "ID"] == "ID_90"), ]
data_cleaned4=data_cleaned[-which(data_cleaned[, "ID"] == "ID_100"), ]

full.mod_CA<-lm(pain~age + sex + pain_cat + STAI_trait + cortisol_serum + mindfulness + weight, data=data_cleaned4)
full.mod_CA
full.model

#difference is minimal, therefore not data is excluded

rm(data_cleaned4)
rm(full.mod_C)
rm(full.mod_CA)

####### Normality (of the residuals) #########

# Histogram for visual check

hist( x = residuals( full.model ), # data are the residuals
      xlab = "Value of residual", # x-axis label
      main = "Check Normality for Full Model",
      breaks = 20 # lots of breaks
)

#Shapiro Wilk Test for all variables

shapiro.test(data_cleaned$pain)
shapiro.test(data_cleaned$sex)
shapiro.test(data_cleaned$age)
shapiro.test(data_cleaned$pain_cat)
shapiro.test(data_cleaned$cortisol_serum)
shapiro.test(data_cleaned$weight)
shapiro.test(data_cleaned$mindfulness)
shapiro.test(data_cleaned$STAI_trait)

# QQ-Plot

plot( x = full.model, which = 2 )

# Shapiro Wilk Test for STAI_trait is slightly significant, but other Test do not show this 
# not excluded (see Assignment 1)

############ Linearity (of the relationship) ################

residualPlots( model = full.model)


# Tukey Test is not significant, therefore we can assume linearity 


############# Homogeneity of variance (also called homoscedasticity) ################

plot(x = full.model, which = 3)


ncvTest(full.model)


# from the non-constant variance test, we can see that there is no violation of the homogeneity of variance


############# that there is no excess multicollinearity ("uncorrelated predictors" in Navarro's words)

vif( mod = full.model) 

#########
# RUN BACKWARD REGRESSION
#########
step( object = full.model, direction = "backward")

# R deletes weight and STAI_trait

###############################################################################
# Run a new regression model now only using the predictors that were retained
# in the end of the backward regression, and save this model in a new R object.
# We will refer to this model as the "backward model". Run the full regression 
# model you arrived at in the end of assignment 1 again, and save this model 
# in another R object. We will refer to this model as the "theory-based model".
# Compare the backward model and the theory-based model based on AIC and using 
# the anova() function.
###############################################################################

# retained predictors: sex, mindfulness, pain_cat, age, cortisol_serum
#Run Regressions
backward.model<- lm(pain~ sex + mindfulness + pain_cat + age + cortisol_serum, data=data_cleaned)
theory.model<- lm(pain~age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data=data_cleaned)

summary(backward.model)
summary(theory.model)
summary(full.model)
confint(backward.model)
confint(theory.model)
lm.beta(backward.model)
lm.beta(theory.model)

AIC(backward.model, full.model)
anova(full.model, backward.model)

AIC(backward.model, theory.model)

anova(backward.model, theory.model)


###############################################################################
#	After this, you decide to put the two models to the test on some new data.
# You collected new data from another 160 participants in the same way as you 
# did in the first study described in Assignment 1.
  # On data file 2, make predictions on pain using the regression models or
  # equations of the backward model and the theory-based model which were 
  # "trained" on data file 1. (IMPORTANT: do not fit the regression models on 
  # data file 2 (don't re-train your models), just use the regression equations 
  # that you derived based on data file 1. These regression equations should be 
  # applied on the new data (data file 2), to predict pain.) 
  # Compare the predicted values with the actual pain ratings. Which model was
  # able to predict the actual pain ratings in data file 2 better?
###############################################################################

data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")

#########
# CHECK DATA
#########

View(data_sample_2)

# Describe and Histogrammes
describe(data_sample_2$pain)
hist(data_sample_2$pain, breaks = 20, main="Histogram of Pain", xlab="Pain", col="lightgray", xlim=c(0, 10))

describe(data_sample_2$age)
hist(data_sample_2$age, breaks = 20, main="Histogram of Age", xlab="Age", col="lightgray", xlim=c(0, 100))

describe(data_sample_2$STAI_trait)
hist(data_sample_2$STAI_trait, breaks = 20, main="Histogram of STAI", xlab="Anxiety", col="lightgray", xlim=c(20, 60))

describe(data_sample_2$pain_cat)
hist(data_sample_2$pain_cat, breaks = 20, main="Histogram of Pain Catastrophizing", xlab="Pain Catastrophizing", col="lightgray", xlim=c(10, 50))

describe(data_sample_2$cortisol_serum)
hist(data_sample_2$cortisol_serum, breaks = 20, main="Histogram of Cortisol Serum", xlab="Cortisol in Serum", col="lightgray", xlim=c(0, 10))

describe(data_sample_2$cortisol_saliva)
hist(data_sample_2$cortisol_saliva, breaks = 20, main="Histogram of Cortisol Saliva", xlab="Cortisol in Saliva", col="lightgray", xlim=c(0, 10))

describe(data_sample_2$mindfulness)
hist(data_sample_2$mindfulness, breaks = 20, main="Histogram of Mindfulness", xlab="Mindfulness", col="lightgray", xlim=c(0, 7))


table(data_sample_2$sex)

# Scatterplot for Pain and Age

plot(pain ~ age, data = data_sample_2, main="Scatterplot Pain and Age", xlab ="Age", ylab="Pain")

plot(pain ~ sex, data = data_sample_2, main="Scatterplot Pain and Sex", xlab ="Sex", ylab="Pain")

plot(pain ~ STAI_trait, data = data_sample_2, main="Scatterplot Pain and STAI", xlab ="STAI", ylab="Pain")

plot(pain ~ pain_cat, data = data_sample_2, main="Scatterplot Pain and Pain Catastrophizing", xlab ="Pain Cat", ylab="Pain")

plot(pain ~ cortisol_serum, data = data_sample_2, main="Scatterplot Pain and Cortisol in Serum", xlab ="Cortisol", ylab="Pain")

plot(pain ~ cortisol_saliva, data = data_sample_2, main="Scatterplot Pain and Cortisol in Saliva", xlab ="Cortisol", ylab="Pain")

plot(pain ~ mindfulness, data = data_sample_2, main="Scatterplot Pain and Mindfulness", xlab ="Mindfulness", ylab="Pain")

############# Data Cleaning ################

# two participants reported Mindfulness below 1, which is an invalid value, therefore they are removed

#113, 123

data_cleaned_RQ2 = data_sample_2
data_cleaned_RQ2 = data_cleaned_RQ2[-which(data_cleaned_RQ2[, "ID"] == "ID_113"), ]
data_cleaned_RQ2 = data_cleaned_RQ2[-which(data_cleaned_RQ2[, "ID"] == "ID_123"), ]
View(data_cleaned_RQ2)





######
# Use regression equations on new data
######




# calculate predicted values 
pred_theory.model <- predict(theory.model, data_cleaned_RQ2) 
pred_backward.model <- predict(backward.model, data_cleaned_RQ2)
# now we calculate the sum of squared residuals 
RSS_test_theory = sum((data_cleaned_RQ2[, "pain"] - pred_theory.model)^2) 
RSS_test_back = sum((data_cleaned_RQ2[, "pain"] - pred_backward.model)^2) 
RSS_test_theory
RSS_test_back

###########################################################################################
###########################################################################################

#                                  RESEARCH QUESTION 3 
#                                 

###########################################################################################
###########################################################################################

# import data
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")

#####################
library(reshape2) # for melt function 
library(ggplot2) # for ggplot 
library(cAIC4) # for cAIC 
library(r2glmm) # for r2beta
library(influence.ME)
library(lattice)
#####################

###############
#   Custom Function
###############

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy return(data.frame(stdcoef = sc, stdse = se)) 
  }

# assign ID and sex as factors
data_sample_3$ID = factor(data_sample_3$ID) 
data_sample_3$sex = factor(data_sample_3$sex) 

View(data_sample_3)

# Check Data

describe(data_sample_3) 
table(data_sample_3[, "sex"])

hist(data_sample_3$pain1)
hist(data_sample_3$pain2)
hist(data_sample_3$pain3)
hist(data_sample_3$pain4)
hist(data_sample_3$age)
hist(data_sample_3$STAI_trait)
hist(data_sample_3$pain_cat)
hist(data_sample_3$cortisol_serum)
hist(data_sample_3$mindfulness)


################
# Check for Clustering
################

# designate which are the repeated varibales 
repeated_variables = c("pain1", "pain2", "pain3", "pain4")
# correlation of repeated variables 
cor(data_sample_3[, repeated_variables])

################
# format from wide to long format
################

data_long = melt(data_sample_3, measure.vars = repeated_variables, variable.name = "day", value.name = "pain_rating") 

# order data frame by participant ID
data_long = data_long[order(data_long[, "ID"]), ]
# change the time/day variable to a numerical vector 
data_long$day = as.numeric(data_long$day)

View(data_long)


################################
#   BUILD THE MODEL
################################

mod_rep_int = lmer(pain_rating ~ day + sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness +  (1 | ID), data = data_long) 
mod_rep_slope = lmer(pain_rating ~ day + sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness +  (day | ID), data = data_long)

#####################
# Comparing the Models
#####################

# generate dataset with predictions
data_long_withpreds = data_long 
data_long_withpreds$pred_int = predict(mod_rep_int) 
data_long_withpreds$pred_slope = predict(mod_rep_slope)

# Plot for random intercept model
ggplot(data_long_withpreds, aes(y = pain_rating, x = day, group = ID)) + geom_point(size = 3) + geom_line(color = "red", aes(y = pred_int, x = day)) + facet_wrap(~ID, ncol = 5)

# Plots for random intercept and slope model
ggplot(data_long_withpreds, aes(y = pain_rating, x = day, group = ID)) + geom_point(size = 3) + geom_line(color = "red", aes(y = pred_slope, x = day)) + facet_wrap(~ID, ncol = 5)

# cAIC for both models
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

# ANOVA
anova(mod_rep_int, mod_rep_slope)


##################
# add model with time-squared to SlopeModel
##################

mod_rep_slope_quad = lmer(pain_rating ~ day + I(day^2) + sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (day | ID), data = data_long) 

# add predictions to predictions-dataset
data_long_withpreds$pred_slope_quad = predict(mod_rep_slope_quad)

##################
# Comparing the Models
##################

plot_quad = ggplot(data_long_withpreds, aes(y = pain_rating, x = day, group = ID)) + geom_point(size = 3) + geom_line(color = "red", aes(y = pred_slope_quad, x = day))+ facet_wrap(~ID, ncol = 5)
plot_quad

# cAIC for Slope and Time^2 Model
cAIC(mod_rep_slope)$caic
cAIC(mod_rep_slope_quad)$caic
cAIC(mod_rep_int)$caic

# ANOVA
anova(mod_rep_slope, mod_rep_slope_quad)


####################
# Center time to avoid problems
####################

data_long_centered_day = data_long 
data_long$day_centered = data_long$day - mean(data_long$day)
mod_rep_slope_quad = lmer(pain_rating ~ day_centered + I(day^2) + sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness  + (day_centered | ID), data = data_long)
data_long$day_centered_2 = data_long$day_centered^2

###################
# REPORT MODEL
###################
r2beta(mod_rep_slope_quad, method = "nsj", data = data_long)

cAIC(mod_rep_slope_quad)$caic
anova(mod_rep_slope_quad, mod_rep_slope)
summary(mod_rep_slope_quad)

confint(mod_rep_slope_quad)

stdCoef.merMod <- function(object) { sdy <- sd(getME(object, "y")) 
sdx <- apply(getME(object, "X"), 2, sd) 
sc <- fixef(object) * sdx/sdy 
se.fixef <- coef(summary(object))[, "Std. Error"] 
se <- se.fixef * sdx/sdy 
return(data.frame(stdcoef = sc, stdse = se)) }
stdCoef.merMod(mod_rep_slope_quad)

##########################################
# RUN MODEL DIAGNOSTICS ON FINAL MODEL
##########################################

# final model is mod_rep_slope_quad

#############
# Check for Outliers --> non found
#############
pred_names = colnames(influence_group)

influence_observation = influence(mod_rep_slope_quad, obs = T)$alt.fixed 
influence_group = influence(mod_rep_slope_quad, group = "ID")$alt.fixed 
 
boxplot(influence_observation[, "day_centered"])

# new par(mfrow) because initial function gave error message
par(mfrow = c(1, 1,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1)) 
for (i in 1:length(pred_names)) { boxplot(influence_observation[, pred_names[i]], main = pred_names[i]) }

par(mfrow = c(1, 1,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1)) 
for (i in 1:length(pred_names)) { boxplot(influence_group[, pred_names[i]], main = pred_names[i]) }


##############
# Check for Normality --> Q-Q-Plot follows straight line (roughly)
##############

qqmath(mod_rep_slope_quad, id = 0.05)
qqmath(ranef(mod_rep_slope_quad))

##############
# Check for Linearity
##############

plot(mod_rep_slope_quad, arg ="pearson")


data_long_with_resid = data_long 
data_long_with_resid$resid = residuals(mod_rep_slope_quad)

plot(resid ~ day_centered, data = data_long_with_resid)
plot(resid ~ day_centered_2, data = data_long_with_resid)
plot(resid ~ sex, data = data_long_with_resid)
plot(resid ~ age, data = data_long_with_resid)
plot(resid ~ STAI_trait, data = data_long_with_resid)
plot(resid ~ pain_cat, data = data_long_with_resid)
plot(resid ~ cortisol_serum, data = data_long_with_resid)
plot(resid ~ mindfulness, data = data_long_with_resid)
plot(resid ~ pain_rating, data = data_long_with_resid)

##############
# Check Homoscedasticity
##############

plot(mod_rep_slope_quad, arg = "pearson")

homosced_mod = lm(data_long_with_resid$resid^2 ~ data_long_with_resid$ID)
summary(homosced_mod) 

# caluclate interquartile range within each cluster 
IQR_of_residuals_by_participant = sapply(split(data_long_with_resid, f = data_long_with_resid$ID), function(x) IQR(x$resid)) 
# rank ordering them 
rank = rank(IQR_of_residuals_by_participant) 
# adding rank to the dataframe containing the residuals 
data_long_with_resid$rank = rep(rank, each = length(repeated_variables)) 
# creating a vector of participant IDs ordered based on the 
# rank, this will be used as labels 
IDforplot = unique(data_long_with_resid$ID[order(data_long_with_resid$rank)])


# create the plot 
ggplot(data_long_with_resid, aes(y = resid, x = factor(rank),labels = ID)) + geom_boxplot() + scale_x_discrete(labels = IDforplot) + coord_flip()

###################
# Check for Multicollinearity
###################

pairs.panels(data_long[, c("day_centered", "day_centered_2", "sex", "age", "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness", "weight")], col = "red", lm = T)


