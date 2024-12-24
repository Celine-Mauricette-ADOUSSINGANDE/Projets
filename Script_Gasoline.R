# -----------------------------------------------
#title: "Econometrics project"
#subtitle: "The U.S. Gasoline Market"
#date: "2024-12-05"
#author: "Céline ADOUSSINGANDE"
#---
 # **Filière** : Master 1 Econométrie-statistique
#------------------------------------------------

# Introduction ----------------------------------

#Definition of the variable------------------- 

#YEAR = Year, 1953-2004,
#GASExp = Total U.S. gasoline expenditure,
#POP = U.S. total population in thousands
#GASP = Price index for gasoline,
#Income = Per capita disposable income,
#PNC = Price index for new cars,
#PUC = Price index for used cars,
#PPT = Price index for public transportation,
#PD = Aggregate price index for consumer durables,
#PN = Aggregate price index for consumer nondurables,
#PS = Aggregate price index for consumer services.

#Library
library(tidyverse)
library(gt)
library(stargazer)

#Read the SCV file
Gasoline_cons <- read.csv("C:/Users/Lenovo/OneDrive/Documents/TD_Econométrie/Data/TableF2-2.csv")

#Show the 2 first line of the Gasoline_cons
head(Gasoline_cons, 2)

#Number of columns of the Gasoline_cons
ncol(Gasoline_cons)

#The Gasoline_cons columns names
colnames(Gasoline_cons)

# The sum of the missing value
sum(is.na(Gasoline_cons))

#Descriptive statistics of variables
summary(Gasoline_cons)



# Question 1.---------------------------
#1. The consumption data is expressed as total expenditures. To obtain per capita ---------
#consumption of gasoline, divide GASEXP by GASP times POP.
#Gasoline_cons_percapita: per capita comsumption of gasoline

Gasoline_cons$GASCONS <- Gasoline_cons$GASEXP/(Gasoline_cons$GASP * Gasoline_cons$POP)

#Display the first two rows of the Gasoline_cons in a table
head(Gasoline_cons,2)%>%
  gt()%>%
  tab_options(
    table.font.size = "small",   
    data_row.padding = px(15)
  )


# 2. Compute the multiple regression model of ln per capita consumption of gaso ---------- 
#line on ln per capita income, the ln price of gasoline, all the ln of other prices
 #and a time trend. Report the results. Do the signs of the estimates agree with
 #your expectations? Explain.

# The log of the variables
Gasoline_cons$logGASCONS<-log(Gasoline_cons$GASCONS)
Gasoline_cons$logGASP<-log(Gasoline_cons$GASP)
Gasoline_cons$logINCOME<-log(Gasoline_cons$INCOME)
Gasoline_cons$logPNC<-log(Gasoline_cons$PNC)
Gasoline_cons$logPUC<-log(Gasoline_cons$PUC)
Gasoline_cons$logPPT<-log(Gasoline_cons$PPT)
Gasoline_cons$logPD<-log(Gasoline_cons$PD)
Gasoline_cons$logPN<-log(Gasoline_cons$PN)
Gasoline_cons$logPS<-log(Gasoline_cons$PS)

#The new data (Gasoline_cons_log) whith the log of the variables
Gasoline_cons_log<-Gasoline_cons %>%
  select(YEAR,logGASCONS,logGASP,logINCOME,logPNC,logPUC,logPPT,logPD,logPN,logPS)

#The multiple regression model with Gasoline_cons_log
model <- lm(logGASCONS ~ logGASP +logINCOME+logPNC +logPUC +logPPT +YEAR +logPD +logPN +logPS, data=Gasoline_cons_log)

# Display the summary of the model with stargazer
stargazer(model, type = "text", title = "Résult of the regression model", 
          align = TRUE, digits = 3, 
          star.cutoffs = c(0.05, 0.01, 0.001))



#3-We are now interested in investigating the change in the gasoline market that ----------
#occurred in 1973. First, compute the average values of the ln of per capita
#gasoline consumption in the years 1953-1973 and 1974-2004 and report the
#difference. Then, divide the sample into these two groups of observations and
#estimate the model for the two samples. Show how you can decompose the
#change in expected value of the ln of consumption into a change in the model
#coefficients (as in a Oaxaca-Blinder decomposition). Using the results, com
#pute a confidence interval for the part of the change that can be attributed to
#structural change in the market, that is, change in the regression coefficients

#a- compute the average values of the ln of per capita ----------
#gasoline consumption in the years 1953-1973 and 1974-2004 and report the
#difference.

# The data for the two periods
GASCONS_1 <- Gasoline_cons_log[Gasoline_cons_log$YEAR >= 1953 & Gasoline_cons_log$YEAR <= 1973,]
GASCONS_2 <- Gasoline_cons_log[Gasoline_cons_log$YEAR >= 1974 & Gasoline_cons_log$YEAR <= 2004,]


# The mean of logGASCONS for each period (periode 1: 1953-1973 and periode 2: 1974-2004)
mean_GASCONS_1 <- mean(GASCONS_1$logGASCONS, na.rm = TRUE)
mean_GASCONS_1
mean_GASCONS_2 <- mean(GASCONS_2$logGASCONS, na.rm = TRUE)
mean_GASCONS_2

# Compute the difference
dif_mean_GASCONS <- mean_GASCONS_2 - mean_GASCONS_1
dif_mean_GASCONS

#Divide the sample into these two groups of observations and
#estimate the model for the two samples. Show how you can decompose the
#change in expected value of the ln of consumption into a change in the model
#coefficients (as in a Oaxaca-Blinder decomposition)

#Estimate model for the two periodes
#model 1 (1953-1973)
model1 <- lm(logGASCONS ~ logGASP +logINCOME+logPNC +logPUC +logPPT +YEAR +logPD +logPN +logPS, data=GASCONS_1)
summary(model1)

#model 2 (1974-2004)
model2 <- lm(logGASCONS ~ logGASP +logINCOME+logPNC +logPUC +logPPT +YEAR +logPD +logPN +logPS, data=GASCONS_2)
summary(model2)

#Display the summary of the two model with stargazer
stargazer(model1, model2, 
          type = "text", 
          title = "Résults of the regression model (model1: 1953-1973 et model1: 1974-2004)", 
          align = TRUE, 
          digits = 3, 
          column.labels = c("model1", "model1"),  
          star.cutoffs = c(0.05, 0.01, 0.001)) 

#b- Oaxaca-Blinder Decomposition -----------
#The means of predictors (explanatory variables) for both periods
means_predictors_period_1 <- colMeans(model.matrix(model1), na.rm = TRUE)
means_predictors_period_2 <- colMeans(model.matrix(model2), na.rm = TRUE)

# Coefficients from both models
coef_period_1 <- coef(model1)
coef_period_2 <- coef(model2)

# Explained Component
explained_component <- sum((means_predictors_period_2 - means_predictors_period_1) * coef_period_1)
explained_component

# Unexplained Component
unexplained_component <- sum(means_predictors_period_2 * (coef_period_2 - coef_period_1))
unexplained_component

#Total difference of the logGASCONS in the two periodes
total_difference <- explained_component + unexplained_component
total_difference

#Using the results, compute a confidence interval for the part of the change that can be attributed to
#structural change in the market, that is, change in the regression coefficients

# The covariance matrices
var_period_1 <- vcov(model1)  # Variance-covariance matrix for 1953-1973
var_period_2 <- vcov(model2)  # Variance-covariance matrix for 1974-2004

# The variance of the coefficient differences
var_coef_diff <- var_period_1 + var_period_2

# The variance of the unexplained component
var_unexplained_component <- t(means_predictors_period_2) %*% var_coef_diff %*% means_predictors_period_2

# Standard error
sd_error<-sqrt(var_unexplained_component)
               
# c- Confidence interval----------
critical_value <- 1.96  # For 95% confidence level
lower_value <- unexplained_component - critical_value * sd_error
lower_value
high_value <- unexplained_component + critical_value * sd_error
high_value




#Question 2.-------------
#1. Carry out a test of the hypothesis that the three aggregate price indices are----------
#not significant determinants of the demand for gasoline.

#model: logGASCONS = alpha + betha_GASP * logGASP + betha_INCOME * logINCOME +
  #gamma_PNC * logPNC + gamma_PUC * logPUC  + gamma_PPT * logPPT + 
  #teltha * YEAR + sigma_PD * logPD + sigma_PN * logPN + sigma_PS * logPS


#H0: sigma_PD = sigma_PN = sigma_PS = 0
#H1:sigma_PD, sigma_PN, sigma_PS =! 0

#Unrestricted model
model_unrestricted <- lm(logGASCONS ~ logGASP + logINCOME + logPNC + logPUC + logPPT + YEAR + logPD + logPN + logPS, data=Gasoline_cons_log)
summary(model_unrestricted)

#Restricted model: 
model_restricted <- lm(logGASCONS ~ logGASP + logINCOME + logPNC + logPUC + logPPT + YEAR, data=Gasoline_cons_log)
summary(model_unrestricted)

#Display the summary of the two model with stargazer
stargazer (model_unrestricted, 
           model_restricted,
           type = "text",
           title = "Results of the regression model",
           align = TRUE,
           digits = 3,
           column.labels = c("model_unrestricted", "model_restricted"),
           star.cutoffs = c(0.05, 0.01, 0.001))



#F-statistic

#The r_squared of the unrestricted model
rsqt_unrestricted<-summary(model_unrestricted)$r.squared
rsqt_unrestricted

#The r_squared of the restricted model
rsqt_restricted<-summary(model_restricted)$r.squared
rsqt_restricted

#Number of observation
n<-52

# Number of parameters
k<-10

#Number of restriction
q<-3

#The degre of freedom
df<- n-k
df

#The F_test
F_test<-(((rsqt_unrestricted-rsqt_restricted)/q)/((1-rsqt_unrestricted)/df))
F_test

#The distribution of F
f~F(q,df)
F_cv<-qf(0.95, df1=3, df2=42)
F_cv

#F_test > F_cv   => 23.24582 > 2.827049
# so we reject H0. Then, the price indices are significant determinants of gasoline demand.


#2-Test the Restrictions------------

#The unrestricted model
model_unrestricted <- lm(logGASCONS ~ logGASP + logINCOME + logPNC + logPUC + logPPT + YEAR + logPD + logPN + logPS, data=Gasoline_cons_log)
summary(model_unrestricted)

#a- the Linear Restriction--------------
#gamma_PNC = gamma_PUC 

# => logGASCONS = alpha + betha_GASP * logGASP + betha_INCOME * logINCOME +
  #gamma_PNC * logPNC + gamma_PNC * logPUC  + gamma_PPT * logPPT + 
  #teltha * YEAR + sigma_PD * logPD + sigma_PN * logPN + sigma_PS * logPS

# => logGASCONS = alpha + betha_GASP * logGASP + betha_INCOME * logINCOME +
#gamma_PNC * (logPNC + logPUC)  + gamma_PPT * logPPT + 
#teltha * YEAR + sigma_PD * logPD + sigma_PN * logPN + sigma_PS * logPS

Gasoline_cons_log$logPNC_PUC <- (Gasoline_cons_log$logPNC + Gasoline_cons_log$logPUC)

restricted_model_linear <- lm(logGASCONS ~ logGASP + logINCOME + logPNC_PUC + logPPT + YEAR + logPD + logPN + logPS, data=Gasoline_cons_log)
 summary(restricted_model_linear)
 
 #Display the summary of the model with stargazer
 stargazer (restricted_model_linear, 
            type = "text",
            title = "Results of the regression model",
            align = TRUE,
            digits = 3,
            column.labels = c("restricted_model_linear"),
            star.cutoffs = c(0.05, 0.01, 0.001))


# b- The Nonlinear Restriction-------------
#gamma_PNC * sigma_PS = gamma_PPT * sigma_PD
 
 #The coef of the tree variables
 gamma_PPT <- coef(model_unrestricted)["logPPT"]
 gamma_PPT
 sigma_PD <- coef(model_unrestricted)["logPD"]
 sigma_PD
 sigma_PS <- coef(model_unrestricted)["logPS"]
 sigma_PS

 #The value of gamma_PNC with the restriction
 gamma_PNC <- (gamma_PPT * sigma_PD) / sigma_PS
 gamma_PNC
 
 #The non linear model restriction
 model_restricted_nonlinear <- lm(logGASCONS ~ logGASP + logINCOME + offset(gamma_PNC * logPNC) + logPUC + logPPT + YEAR + logPD + logPN + logPS, data=Gasoline_cons_log)
 summary(model_restricted_nonlinear)
 
 #Display the summary of the tree model with stargazer
 stargazer (model_unrestricted, 
            restricted_model_linear,
            model_restricted_nonlinear,
            type = "text",
            title = "Results of the regression model",
            align = TRUE,
            digits = 3,
            column.labels = c("model_unrestricted", "model_restricted","model_restricted_nonlinear"),
            star.cutoffs = c(0.05, 0.01, 0.001))
 
 
 
 #3- Using the gasoline market data, test the two restrictions suggested here, se -----------
 #parately and jointly
 
 # a- Test for linear restriction --------------
 
 
 #F-statistic
 
 #The r_squared of the unrestricted model
 rsqt_unrestricted<-summary(model_unrestricted)$r.squared
 rsqt_unrestricted
 
 #The r_squared of the restricted model
 rsqt_restricted_linear<-summary(restricted_model_linear)$r.squared
 rsqt_restricted_linear
 
 #Number of observation
 n<-52
 
 # Number of parameters
 k<-10
 
 #Number of restriction
 q<-1
 
 #The degre of freedom
 df<- n-k
 df
 
 #The F_test
 F_test1<-(((rsqt_unrestricted-rsqt_restricted_linear)/q)/((1-rsqt_unrestricted)/df))
 F_test1
 
 #The distribution of F
 f~F(q,df)
 F_cv_linear<-qf(0.95, df1=1, df2=42)
 F_cv_linear
 
 #F_test < F_cv   => 1.353153 < 4.072654
 # so we fail to reject H0. Then, gamma_PNC = gamma_PUC
 
 
 # b- Test for non linear restriction -------------------
 
 #Log-likelihood  
 # Log-likelihood for the unrestricted model
 LL_unrestricted <- sum(dnorm(model_unrestricted$residuals, mean = 0, sd = sd(model_unrestricted$residuals), log = TRUE))
 LL_unrestricted
 # Log-likelihood for the restricted model
 LL_restricted <- sum(dnorm(model_restricted_nonlinear$residuals, mean = 0, sd = sd(model_restricted_nonlinear$residuals), log = TRUE))
 LL_restricted
 # Likelihood Test Statistics
 LR <- -2 * (LL_restricted - LL_unrestricted)
 LR
 
 # Number of restrictions
 k <- 1  
 
 # p-value
 p_value <- pchisq(LR, df = k, lower.tail = FALSE)
 p_value
 
 #p_value > 0.05 so we fail to reject H0
 



#F_test
 #The r_squared of the restricted model
 rsqt_restricted_nonlinear<-summary(model_restricted_nonlinear)$r.squared
 rsqt_restricted_nonlinear
 
 #Number of observation
 n<-52
 
 # Number of parameters
 k<-10
 
 #Number of restriction
 q<-1
 
 #The degre of freedom
 df<- n-k
 df
 
 #The F_test
 F_test2<-(((rsqt_unrestricted-rsqt_restricted_nonlinear)/q)/((1-rsqt_unrestricted)/df))
 F_test2
 
 #The distribution of F
 f~F(q,df)
 F_cv_nonlinear<-qf(0.95, df1=1, df2=42)
 F_cv_nonlinear
 
 #We fail to rejet H0
 
 #END