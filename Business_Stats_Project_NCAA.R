####   Case - College Basketball  ####
#### Melissa Eckert & Jay Loehner ####

#installing and loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) 
library(broom) 
library(purrr) 
library(plotly)
library(GGally)
library(MLmetrics)
library(caret)
library(GGally)
library(ggpubr)
library(pROC)
options(scipen = 12) #turning off scientific notation

#set working directory
setwd("X:/SCMA 632/Case")

#read in data from xlsx file
cbb_all <- read.csv("cbb.csv", header = TRUE)
cbb1 <- cbb_all


####WRANGLING AND EXPLORING DATA####
#cleaning data
cbb_all$CONF[cbb_all$CONF == "ind"] <- "Ind"

#dividing into training and test sets
#training set
cbb_train <- cbb_all %>%
  filter(YEAR != 2022 & YEAR != 2023)

#test set
cbb_test <- cbb_all %>%
  filter(YEAR == 2022 | YEAR == 2023)

#The first question we will explore is:
#Is the number of games won better explained by a team's offensive play or defensive play?

#Dependent Variable: W (# of games won)

#Offensive Explanatory Variables: EFG_O (effective field goal percentage shot), FTR (free throw rate), TOR (turnover rate), ORB (offensive rebound rate), ADJOE (adjusted offensive efficiency)

#Defensive Explanatory Variables: EFG_D (effective field goal percentage allowed), FTRD (free throw rate allowed), TORD (steal rate), DRB (offensive rebound rate allowed), ADJDE (adjusted defensive efficiency)

#cleaning data more
cbb <- cbb_train %>%
  select(W, EFG_O, FTR, TOR, ORB, ADJOE, EFG_D, FTRD, TORD, DRB, ADJDE) %>%
  drop_na()

#offensive 
cbb_o <- cbb %>%
  select(W, EFG_O, FTR, TOR, ORB, ADJOE)

#defensive 
cbb_d <- cbb %>%
  select(W, EFG_D, FTRD, TORD, DRB, ADJDE)

#exploring data visually

#full data
ggpairs(cbb_all[c(4, 5, 8, 10, 12, 14)]) #offensive play
ggpairs(cbb_all[c(4, 6, 9, 11, 13, 15)]) #defensive play

#training data
ggpairs(cbb_o[c(1, 2, 3, 4, 5, 6)]) #offensive play

#The distributions look approximately normal for most of the variables, with FTR having a bit of skew to the right, and W also being slightly skewed to the right. There are relatively strong positive relationships between W and ADJOE (0.739) and W and EFG_O (0.586), a moderately strong positive relationship between W and ORB (0.322), a weak positive relationship between W and FTR (0.189), and a moderately strong negative relationship between W and TOR (-0.440). ADJOE has a strong positive correlation with EFG_O (0.723) and a strong negative relationship with TOR (-0.607), which may cause issues with multicollinearity. The training data looks similar to the full data as well.

ggpairs(cbb_d[c(1, 2, 3, 4, 5, 6)]) #defensive play

#The distributions look approximately normal for most of the variables, with FTRD having a bit of skew to the right, and W also possibly being very slightly skewed to the right. There are relatively strong negative relationships between W and ADJDE (-0.655) and W and EFG_D (-0.582), a moderately strong negative relationship between W and DRB (-0.306), a weak negative relationship between W and FTRD (-0.253), and a weak positive relationship between W and TORD (0.133). ADJDE has a strong positive correlation with EFG_D (0.781) which may cause issues with multicollinearity. For the most part, the training data looks similar to the full data.

#more exploring
cbb_train %>%
  ggplot(aes(x = CONF, y = W)) +
  geom_boxplot() 

#from the side-by-side boxplots of Wins by Conference, looking at a few Conferences by Team and Year

#ACC
cbb_train %>% filter(CONF == "ACC") %>%
  ggplot(aes(x = TEAM, y = W, fill = as.factor(YEAR))) +
  geom_col() +
  labs(x = "Team", 
       y = "Wins") +
  guides(fill = guide_legend(title= "Year"))

#GWC
cbb_train %>% filter(CONF == "GWC") %>%
  ggplot(aes(x = TEAM, y = W, fill = as.factor(YEAR))) +
  geom_col() +
  labs(x = "Team", 
       y = "Wins") +
  guides(fill = guide_legend(title= "Year"))

#Looks suspect so looking at the data
cbb_train %>% filter(CONF == "GWC") #data only available for 2013

#Ind
cbb_train %>% filter(CONF == "Ind") %>%
  ggplot(aes(x = TEAM, y = W, fill = as.factor(YEAR))) +
  geom_col() +
  labs(x = "Team", 
       y = "Wins") +
  guides(fill = guide_legend(title= "Year"))

#Looks suspect so looking at the data
cbb_train %>% filter(CONF == "Ind") #data inconsistently available, and only between 2013-2015

#SEC
cbb_train %>% filter(CONF == "SEC") %>%
  ggplot(aes(x = TEAM, y = W, fill = as.factor(YEAR))) +
  geom_col() +
  labs(x = "Team", 
       y = "Wins") +
  guides(fill = guide_legend(title= "Year"))

#WCC
cbb_train %>% filter(CONF == "WCC") %>%
  ggplot(aes(x = TEAM, y = W, fill = as.factor(YEAR))) +
  geom_col() +
  labs(x = "Team", 
       y = "Wins") +
  guides(fill = guide_legend(title= "Year"))

#more exploring
ggpairs(cbb_train[c(4, 7, 20)]) #BARTHAG looks like logistic regressions

cbb_train %>%
  ggplot(aes(x = BARTHAG, y = W)) +
  geom_point(aes(color = SEED)) +
  theme_minimal()

cbb_train %>% filter(SEED != "NA") %>%
  ggplot(aes(x = as.factor(as.numeric(SEED)), y = W, fill = as.factor(YEAR))) +
  geom_col() +
  coord_flip() +
  guides(fill = guide_legend(title= "Year")) +
  labs(x = "Seed")

cbb_train %>% filter(SEED != "NA") %>%
  ggplot(aes(x = as.factor(as.numeric(SEED)), y = W)) +
  geom_boxplot() +
  labs(x = "Seed")

cbb_train %>%
  ggplot(aes(x = as.factor(as.numeric(SEED)), y = BARTHAG)) +
  geom_boxplot() +
  labs(x = "Seed")


####LINEAR REGRESSION####


#### OFFENSIVE MODEL ####

#First, we will look at a model considering offensive play only
#We will also first look at the model without any interactions to get an idea of the effect of each variable (holding all other variables constant), and then will start looking at interactions
cbb_lmo <- lm(W ~ EFG_O + FTR + TOR + ORB + ADJOE, data = cbb_o)
summary(cbb_lmo)

#Adjusted R-squared: 0.6184
#Residual standard error: 4.082

#Now, we'll look at interactions. We'll start with a model with all possible interactions, and make include/exclude decisions from there
cbb_lmo1 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(cbb_lmo1)

#Adjusted R-squared: 0.6229
#Residual standard error: 4.058

#We will start by making include/exclude decisions on the interaction terms
#The highest p-value is for the interaction between EFG_O and FTR (0.88509), so we will remove it
cbb_lmo2 <- lm(W ~ EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(cbb_lmo2)

#Adjusted R-squared: 0.6231 
#Residual standard error: 4.057 

#The highest p-value is for the interaction between ORB and TOR (0.84982), so we will remove it
cbb_lmo3 <- lm(W ~ EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(cbb_lmo3)

#Adjusted R-squared: 0.6232 
#Residual standard error: 4.056 

#The highest p-value is for the interaction between EFG_O and TOR (0.31511), so we will remove it
cbb_lmo4 <- lm(W ~ EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(cbb_lmo4)

#Adjusted R-squared: 0.6232 
#Residual standard error: 4.056 

#The highest p-value is for the interaction between ORB and EFG_O (0.15776), so we will remove it
cbb_lmo5 <- lm(W ~ EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(cbb_lmo5)

#Adjusted R-squared: 
#Residual standard error: 

#The highest p-value is for the interaction between ADJOE and EFG_O (0.09113), so we will remove it
cbb_lmo6 <- lm(W ~ EFG_O + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(cbb_lmo6)

#Adjusted R-squared: 0.623 
#Residual standard error: 4.057 

#The highest p-value is for the interaction between ADJOE and ORB (0.06963), so we will remove it
cbb_lmo7 <- lm(W ~ EFG_O + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE, data = cbb_o)
summary(cbb_lmo7)

#Adjusted R-squared: 0.6225 
#Residual standard error: 4.06 

#The highest p-value is for the interaction between FTR and ORB (0.06250), so we will remove it
cbb_lmo8 <- lm(W ~ EFG_O + FTR * TOR + ORB + FTR * ADJOE + TOR * ADJOE, data = cbb_o)
summary(cbb_lmo8)

#Adjusted R-squared: 0.6222 
#Residual standard error: 4.062 

#At this point, all of our interactions are significant at the 5% LOS, so we will move on to making include/exclude decisions about the individual terms.

#All of the individual terms are either significant or are part of significant interactions, so we will keep them. Therefore, we are at our final model.

#Confidence Intervals for Model Coefficients
confint(cbb_lmo8)

#Checking the model for multicollinearity, influential points, and that the errors are normally distributed with mean 0 and constant variance

#Checking for multicollinearity with VIFs
vif(cbb_lmo8, type = "predictor")
#The VIFs are all under 10, so multicollinearity is not super concerning here

#Checking for influential points
o_inf <- as.data.frame(influencePlot(cbb_lmo8, main = "Influence Plot", id=list(method = "noteworthy", 
                                                                                n=3, cex=1, col=carPalette()[1], location="lr")))
#It looks like there are some potentially concerning points

augment(cbb_lmo8)

#Cook's distance
o_cooks <- as.data.frame(cooks.distance(cbb_lmo8))
o_cooks <- o_cooks %>%
  rename(CooksD = 1) %>%
  mutate(ID = row_number()) %>%
  select(ID, CooksD)

View(o_cooks)
#There are no NaN points, which is good

#View hat values, cook's distance, and studentized residuals
o_influential <- augment(cbb_lmo8) %>%
  mutate(ID = row_number()) %>%
  select(ID, W:.std.resid)

View(o_influential)

#points with Cook's distance > 4/n
o_influential_points <- o_cooks %>%
  filter(CooksD > (4/nrow(o_cooks)) | is.nan(CooksD)) %>% 
  arrange(desc(CooksD))

View(o_influential_points)

#or points with Cook's distance > 4*mean
o_influential_points2 <- o_cooks %>%
  filter(CooksD > (4*mean(CooksD, na.rm = TRUE)) | is.nan(CooksD)) %>%
  arrange(desc(CooksD))

View(o_influential_points2)

4*mean(o_cooks$CooksD, na.rm = TRUE)

#None of these points are super concerning to me, even those with the highest Cook's distance, as they are not super high and looking at the handful with the highest values, they are certainly possible observations. Additionally, with the nature of the data, we are less concerned with people misreporting or lying on a survey, etc. So, we will keep all of the observations.

#Checking model assumptions - errors that are N~(0, sigma^2)

#Normality - qqplot
augment(cbb_lmo8) %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() + 
  stat_qq_line()
#the qqplot is indicating non-normality

#Normality test
shapiro.test(augment(cbb_lmo8)$.resid)
#The p-value is 0.002435, so the residuals are NOT normal

#Mean 0
mean(augment(cbb_lmo8)$.resid)
#This assumption holds, mean is ~0

#Constant variance (homoskedasticity)
augment(cbb_lmo8) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Residuals vs Fitted Values:  Offensive")
#There is some shape here, almost like a diamond, so heteroskedasticity may be a concern

#Plotting the results
#Predicted vs Actual 
augment(cbb_lmo8) %>%
  ggplot(aes(x = W, y = .fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Offensive")


#### LOG MODEL FOR OFFENSIVE PLAY ####

#Because we are seeing that our residuals may be heteroskedastic and are non-normal for the offensive model, we will perform the corrective action of using a natural log transformation of the dependent variable W

#Adding log(W) to the datasets
cbb_o$ln_W <- log(cbb_o$W)

#Removing the -inf ln_W observations
cbb_o <- cbb_o %>%
  filter(ln_W >= 0)

#Scatterplots
ggpairs(cbb_o[c(7, 2, 3, 4, 5, 6)])

#Let's try this again
o_log0 <- lm(log(W) ~ EFG_O + FTR + TOR + ORB + ADJOE, data = cbb_o)
summary(o_log0)

#Adjusted R-squared: 0.5798   
#Residual standard error: 0.3231   

#Now with interactions
o_log <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE, data = cbb_o)
summary(o_log)

#Adjusted R-squared: 0.5961  
#Residual standard error: 0.3167  

#The highest p-value is for the interaction between TOR and ADJOE (0.569815), so we will remove it
o_log1 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB + ORB * ADJOE, data = cbb_o)
summary(o_log1)

#Adjusted R-squared: 0.5962   
#Residual standard error: 0.3167 

#The highest p-value is for the interaction between ORB and ADJOE (0.241740), so we will remove it
o_log2 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB, data = cbb_o)
summary(o_log2)

#Adjusted R-squared: 0.5962    
#Residual standard error: 0.3167  

#The highest p-value is for the interaction between EFG_O and FTR (0.098150), so we will remove it
o_log3 <- lm(log(W) ~ EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB, data = cbb_o)
summary(o_log3)

#Adjusted R-squared: 0.5958      
#Residual standard error: 0.3169    

#The highest p-value is for the interaction between ADJOE and FTR (0.093459), so we will remove it
o_log4 <- lm(log(W) ~ EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + TOR * ORB, data = cbb_o)
summary(o_log4)

#Adjusted R-squared: 0.5955       
#Residual standard error: 0.317     

#At this point, all of our interactions are significant at the 5% LOS, so we will move on to making include/exclude decisions about the individual terms.

#All of the individual terms are either significant or are part of significant interactions, so we will keep them. Therefore, we are at our final model.

#Confidence Intervals for Model Coefficients
confint(o_log4)

#Checking the model for multicollinearity, influential points, and that the errors are normally distributed with mean 0 and constant variance

#Checking for multicollinearity with VIFs
vif(o_log4, type = "predictor")
#Although the VIFs are extremely high for some of the explanatory variables, we can also see that most of the variables are interacting with at least one other explanatory variable, which is probably what is driving up the VIFs.

#Checking for influential points
olog_inf <- as.data.frame(influencePlot(o_log4, main = "Influence Plot", id=list(method = "noteworthy", 
                                                                                 n=3, cex=1, col=carPalette()[1], location="lr")))
#It looks like there are some potentially concerning points

augment(o_log4)

#Cook's distance
olog_cooks <- as.data.frame(cooks.distance(o_log4))
olog_cooks <- olog_cooks %>%
  rename(CooksD = 1) %>%
  mutate(ID = row_number()) %>%
  select(ID, CooksD)

View(olog_cooks)
#There are no NaN points, which is good

olog_inf <- olog_cooks %>%
  filter(CooksD > (4*mean(CooksD, na.rm = TRUE)) | is.nan(CooksD)) %>%
  arrange(desc(CooksD))

View(olog_inf)
#Still no super concerning Cook's Distance values

#Checking model assumptions - errors that are N~(0, sigma^2)

#Normality - qqplot
augment(o_log4) %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() + 
  stat_qq_line()
#the qqplot is indicating non-normality

#Normality test
shapiro.test(augment(o_log4)$.resid)
#The p-value is ~0, so the residuals are NOT normal - and even further from normal than in the original space

#Mean 0
mean(augment(o_log4)$.resid)
#This assumption holds, mean is ~0

#Constant variance (homoskedasticity)
augment(o_log4) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Residuals vs Fitted Values:  Offensive Log")
#There is a definite cone shape here, with the variance getting smaller as the fitted values get larger. This seems more heteroskedastic than in the original space.

#More transformations and/or modeling are needed in order to generate a regression model that passes all model assumptions. In this case, we are more concerned with interpretability, so we will not continue with further transformations.

#Because both the original model and the log model did meet the normality and homoskedasticity assumptions, we will use both to predict on the test set and determine which to use from there.

#Plotting the results
#Predicted vs Actual 
cbb_o$log_fitted <- augment(o_log4)$.fitted
cbb_o$fitted <- exp(augment(o_log4)$.fitted)

cbb_o %>%
  ggplot(aes(x = W, y = fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Offensive Log")


#### DEFENSIVE MODEL ####

#Now, we will look at a model considering defensive play only
#We will first look at the model without any interactions to get an idea of the effect of each variable (holding all other variables constant), and then will start looking at interactions
cbb_lmd <- lm(W ~ EFG_D + FTRD + TORD + DRB + ADJDE, data = cbb_d)
summary(cbb_lmd)

#Adjusted R-squared: 0.4807 
#Residual standard error: 4.761 

#Now, we'll look at interactions. We'll start with a model with all possible interactions, and make include/exclude decisions from there
cbb_lmd1 <- lm(W ~ EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE, data = cbb_d)
summary(cbb_lmd1)

#Adjusted R-squared: 0.4892 
#Residual standard error: 4.722 

#We will start by making include/exclude decisions on the interaction terms
#The highest p-value is for the interaction between EFG_D and FTRD (0.71605), so we will remove it
cbb_lmd2 <- lm(W ~ EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE, data = cbb_d)
summary(cbb_lmd2)

#Adjusted R-squared: 0.4894 
#Residual standard error: 4.722 

#The highest p-value is for the interaction between DRB and EFG_D (0.338651), so we will remove it
cbb_lmd3 <- lm(W ~ EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE, data = cbb_d)
summary(cbb_lmd3)

#Adjusted R-squared: 0.4894 
#Residual standard error: 4.722 

#The highest p-value is for the interaction between DRB and ADJDE (0.368243), so we will remove it
cbb_lmd4 <- lm(W ~ EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE, data = cbb_d)
summary(cbb_lmd4)

#Adjusted R-squared: 0.4895 
#Residual standard error: 4.721 

#The highest p-value is for the interaction between DRB and TORD (0.091057), so we will remove it
cbb_lmd5 <- lm(W ~ EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * ADJDE, data = cbb_d)
summary(cbb_lmd5)

#Adjusted R-squared: 0.4891 
#Residual standard error: 4.723 

#The highest p-value is for the interaction between DRB and FTRD (0.059118), so we will remove it
cbb_lmd6 <- lm(W ~ EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + DRB + FTRD * ADJDE + TORD * ADJDE, data = cbb_d)
summary(cbb_lmd6)

#Adjusted R-squared: 0.4886 
#Residual standard error: 4.725 

#The highest p-value is for the interaction between ADJDE and FTRD (0.221240), so we will remove it
cbb_lmd7 <- lm(W ~ EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + DRB + TORD * ADJDE, data = cbb_d)
summary(cbb_lmd7)

#Adjusted R-squared: 0.4885 
#Residual standard error: 4.726 

#At this point, all of our interactions are significant at the 5% LOS, so we will move on to making include/exclude decisions about the individual terms.

#All of the individual terms are significant. Therefore, we are at our final model.

#Confidence Intervals for Model Coefficients
confint(cbb_lmd7)

#Checking the model for multicollinearity, influential points, and that the errors are normally distributed with mean 0 and constant variance

#Checking for multicollinearity with VIFs
vif(cbb_lmd7, type = "predictor")
#Although the VIFs are extremely high for some of the explanatory variables, we can also see that most of the variables are interacting with at least one, and in many cases, several other explanatory variables, which is probably what is driving up the VIFs.

#Checking for influential points
cbb_inf <- as.data.frame(influencePlot(cbb_lmd7, main = "Influence Plot", id=list(method = "noteworthy", 
                                                                                  n=3, cex=1, col=carPalette()[1], location="lr")))
#It looks like there are some potentially concerning points

augment(cbb_lmd7)

#Cook's distance
d_cooks <- as.data.frame(cooks.distance(cbb_lmd7))
d_cooks <- d_cooks %>%
  rename(CooksD = 1) %>%
  mutate(ID = row_number()) %>%
  select(ID, CooksD)

View(d_cooks)
#There are no NaN points, which is good

#View hat values, cook's distance, and studentized residuals
d_influential <- augment(cbb_lmd7) %>%
  mutate(ID = row_number()) %>%
  select(ID, W:.std.resid)

View(d_influential)

#points with Cook's distance > 4/n
d_influential_points <- d_cooks %>%
  filter(CooksD > (4/nrow(d_cooks)) | is.nan(CooksD)) %>% 
  arrange(desc(CooksD))

View(d_influential_points)

#or points with Cook's distance > 4*mean
d_influential_points2 <- d_cooks %>%
  filter(CooksD > (4*mean(CooksD, na.rm = TRUE)) | is.nan(CooksD)) %>%
  arrange(desc(CooksD))

View(d_influential_points2)

4*mean(d_cooks$CooksD, na.rm = TRUE)

#None of these points are super concerning to me, even those with the highest Cook's distance, as they are not super high and looking at the handful with the highest values, they are certainly possible observations. Additionally, with the nature of the data, we are less concerned with people misreporting or lying on a survey, etc. So, we will keep all of the observations.

#Checking model assumptions - errors that are N~(0, sigma^2)

#Normality - qqplot
augment(cbb_lmd7) %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() + 
  stat_qq_line()
#the qqplot is indicating that there could potentially be normality, but it's hard to say for sure from the visual

#Normality test
shapiro.test(augment(cbb_lmd7)$.resid)
#The p-value is 0.2465, so the residuals are normal

#Mean 0
mean(augment(cbb_lmd7)$.resid)
#This assumption holds, mean is ~0

#Constant variance (homoskedasticity)
augment(cbb_lmd7) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Residuals vs Fitted Values:  Defensive")
#There is potentially a very mild shape here, but I do not feel like there are concerns about heteroskedasticity here

#Our defensive model passes model assumptions.

#Plotting the results
#Predicted vs Actual 
augment(cbb_lmd7) %>%
  ggplot(aes(x = W, y = .fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Defensive")


#### PREDICTIONS ####

#Predict W values for a set of people out-of-sample

#cleaning data
cbb_test <- cbb_test %>%
  select(TEAM, W, EFG_O, FTR, TOR, ORB, ADJOE, EFG_D, FTRD, TORD, DRB, ADJDE, YEAR) %>%
  drop_na()

#exploring data visually
ggpairs(cbb_test[c(2, 3, 4, 5, 6, 7)]) #offensive play
ggpairs(cbb_test[c(2, 8, 9, 10, 11, 12)]) #defensive play
#these are looking super similar to the training set, which is good!

#First, we will predict based on offensive play

#Since these observations are out-of-sample, we use prediction
o_pred <- as.data.frame(predict(cbb_lmo8, newdata = cbb_test, interval = "prediction"))

#The predicted values (fit) and the prediction interval for each (lwr and upr) are in this dataframe
o_pred

#We will use MAPE to test how accurate our predictions are
#Combining the two tables
o_test_pred <- cbb_test %>%
  bind_cols(o_pred)

o_test_pred

o_accuracy <- data.frame(MAPE = MAPE(o_test_pred$fit, o_test_pred$W))

o_accuracy

#MAPE for this model on the test data is 0.2253 and indicates that we have approximately 22.53% error in our predictions.

#Plotting the results
#Predicted vs Actual 
o_test_pred %>%
  ggplot(aes(x = W, y = fit)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Offensive Test")

#It looks like the offensive model tends to under predict, but overall, it's a pretty good fit.

#Now, we will predict using the offensive log model

#cleaning data
#Adding log(W) to the datasets
cbb_test$ln_W <- log(cbb_test$W)

#Removing the -inf ln_W observations
cbb_test_log <- cbb_test %>%
  filter(ln_W >= 0)

#dropping N/As
cbb_test_log <- cbb_test_log %>%
  drop_na()

#exploring data visually
ggpairs(cbb_test_log[c(14, 3, 4, 5, 6, 7)]) #offensive play
#these look quite different from the training set

#Since these observations are out-of-sample, we use prediction
o_log_pred <- as.data.frame(predict(o_log4, newdata = cbb_test_log, interval = "prediction"))

#The predicted values (fit) and the prediction interval for each (lwr and upr) are in this dataframe
o_log_pred

#We will use MAPE to test how accurate our predictions are
#Combining the two tables
o_log_test_pred <- cbb_test_log %>%
  bind_cols(o_log_pred)

o_log_test_pred

o_log_test_pred <- o_log_test_pred %>%
  mutate(fit_og = exp(fit),
         lwr_og = exp(lwr),
         upr_og = exp(upr),
         resids_og = W - fit_og)

o_log_test_pred

o_log_accuracy <- data.frame(MAPE = MAPE(o_log_test_pred$fit_og, o_log_test_pred$W))

o_log_accuracy

#MAPE for this model on the test data is 0.2328 and indicates that we have approximately 23.28% error in our predictions.

#Plotting the results
#Predicted vs Actual
o_log_test_pred %>%
  ggplot(aes(x = W, y = fit_og)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Offensive Log Test")

#Once again, this model tends to under predict wins.

#The MAPE for the log model is close to the MAPE for the original model, but slightly higher. The visuals of Predicted vs Actual also seem like the original model is better, as well as the Adjusted R-squared values and the Residual standard errors. Additionally, going with the original model increases interpretability. So, we will go with the original model.

#Now, we will predict based on defensive play

#Since these observations are out-of-sample, we use prediction
d_pred <- as.data.frame(predict(cbb_lmd7, newdata = cbb_test, interval = "prediction"))

#The predicted values (fit) and the prediction interval for each (lwr and upr) are in this dataframe
d_pred

#We will use MAPE to test how accurate our predictions are
#Combining the two tables
d_test_pred <- cbb_test %>%
  bind_cols(d_pred)

d_test_pred

d_accuracy <- data.frame(MAPE = MAPE(d_test_pred$fit, d_test_pred$W))

d_accuracy

#MAPE for this model on the test data is 0.2721 and indicates that we have approximately 27.21% error in our predictions.

#Plotting the results
#Predicted vs Actual 
d_test_pred %>%
  ggplot(aes(x = W, y = fit)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Defensive Test")

#The defensive model does not appear to have a tendency toward under or over predicting, but it does not seem to predict as closely to the line as the offensive model.

#Getting predictions for specific teams for presentation
pres_teams_o <- o_test_pred %>%
  filter(TEAM == "VCU" | TEAM == "Virginia" | TEAM == "Duke" | TEAM == "Pittsburgh") %>%
  select(TEAM, W, fit, lwr, upr, YEAR)

pres_teams_o

pres_teams_d <- d_test_pred %>%
  filter(TEAM == "VCU" | TEAM == "Virginia" | TEAM == "Duke" | TEAM == "Pittsburgh") %>%
  select(TEAM, W, fit, lwr, upr, YEAR)

pres_teams_d


#### INTERPRETATIONS & CONCLUSIONS ####

#Offensive Model Interpretations

#The interpretation of the intercept (constant coefficient) (-56.248164) is that the estimated wins (W) when EFG_O, FTR, TOR, ORB, ADJOE, and all significant interactions are 0 is -56.248164. This is not a practical interpretation because W cannot be negative - the smallest value it can practically take on is 0.
#The interpretation of the coefficient for EFG_O (0.866224) is that, holding all other variables and interactions constant, for a one unit increase in effective field goal percentage shot (EFG_O), the estimated W will increase by 0.866224.
#The interpretation of the coefficient for ORB (0.508187) is that, holding all other variables and interactions constant, for a one unit increase in offensive rebound rate (ORB), the estimated W will increase by 0.508187.
#Because FTR, TOR, and ADJOE are part of interaction terms, it is difficult to understand and interpret their individual coefficients.
#However, for the interaction term between FTR and TOR, the interpretation of the coefficient (0.020810) is that, holding all other variables and interactions constant, as free throw rate (FTR) increases by one unit, the estimated change in W is dependent on turnover rate (TOR).
#For the interaction term between FTR and ADJOE, the interpretation of the coefficient (0.006390) is that, holding all other variables and interactions constant, as free throw rate (FTR) increases by one unit, the estimated change in W is dependent on adjusted offensive efficiency (ADJOE).
#For the interaction term between TOR and ADJOE, the interpretation of the coefficient (-0.020914) is that, holding all other variables and interactions constant, as turnover rate (TOR) increases by one unit, the estimated change in W is dependent on adjusted offensive efficiency (ADJOE).


#Defensive Model Interpretations

#The interpretation of the intercept (constant coefficient) (238.377212) is that the estimated wins (W) when EFG_D, FTRD, TORD, DRB, ADJDE, and all significant interactions are 0 is 238.377212. This is not a practical interpretation because the highest number of games a team can play in a season, including post season games, is 40.
#The interpretation of the coefficient for DRB (-0.346744) is that, holding all other variables and interactions constant, for a one unit increase in offensive rebound rate allowed (DRB), the estimated W will decrease by 0.346744.
#Because EFG_D, FTRD, TORD, and ADJDE are part of interaction terms, it is difficult to understand and interpret their individual coefficients.
#However, for the interaction term between EFG_D and TORD, the interpretation of the coefficient (0.081803) is that, holding all other variables and interactions constant, as effective field goal percentage allowed (EFG_D) increases by one unit, the estimated change in W is dependent on steal rate (TORD).
#For the interaction term between EFG_D and ADJDE, the interpretation of the coefficient (0.020387) is that, holding all other variables and interactions constant, as effective field goal percentage allowed (EFG_D) increases by one unit, the estimated change in W is dependent on adjusted defensive efficiency (ADJDE).
#For the interaction term between TORD and FTRD, the interpretation of the coefficient (0.018790) is that, holding all other variables and interactions constant, as steal rate (TORD) increases by one unit, the estimated change in W is dependent on free throw rate allowed (FTRD).
#For the interaction term between TORD and ADJDE, the interpretation of the coefficient (-0.023171) is that, holding all other variables and interactions constant, as steal rate (TORD) increases by one unit, the estimated change in W is dependent on adjusted defensive efficiency (ADJDE).


#Conclusions: Offensive Play vs. Defensive Play in Predicting Wins

#Both the offensive and defensive models ended up including all of the offensive or defensive explanatory variables, respectively, in at least one interaction term or as an individual variable. The defensive model had more interaction terms than the offensive model, indicating that the effect of several of the defensive explanatory variables on wins were dependent on each other at the 5% LOS. Interestingly, rebound rate (offensive (ORB) and allowed (DRB)) did not have significant interactions in either of the models.
#The Adjusted R-squared for the offensive model (0.6222) was higher than that of the defensive model (0.4885), the Residual standard error was lower for the offensive model (4.062) than that of the defensive model (4.726), and the MAPE was lower for the offensive model (0.2253) than for the defensive model (0.2721). Thus, the offensive model was able to provide a better fit for the training data and performed better on the test data than the defensive model, so we conclude that offensive play is a better predictor of wins (W) than defensive play.
#However, the Adjusted R-squared for the offensive model isn't super high, and the MAPE is okay but could certainly be lower. Realistically, offensive play AND defensive play are both important for a team to win games. Thus, it is likely that a model that considers both offensive and defensive explanatory variables would provide an even better fit and would provide even better predictions.


#### "FULL" MODEL ####

#Now, we will look at a model considering both offensive and defensive play
#We will first look at the model without any interactions to get an idea of the effect of each variable (holding all other variables constant), and then will start looking at interactions
cbb_lmod <- lm(W ~ EFG_O + FTR + TOR + ORB + ADJOE + EFG_D + FTRD + TORD + DRB + ADJDE, data = cbb)
summary(cbb_lmod)

#Adjusted R-squared: 0.8075 
#Residual standard error: 2.899 

#Now, we'll look at interactions. We'll start with a model with all possible interactions, and make include/exclude decisions from there
#This is going to be an insane model to start...
cbb_lmod1 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod1)

#Adjusted R-squared: 0.8186  
#Residual standard error: 2.814  

#We will start by making include/exclude decisions on the interaction terms
#The highest p-value is for the interaction between FTR and ORB (0.976980), so we will remove it
cbb_lmod2 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod2)

#Adjusted R-squared: 0.8187   
#Residual standard error: 2.814   

#The highest p-value is for the interaction between TORD and ADJDE (0.960272), so we will remove it
cbb_lmod3 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod3)

#Adjusted R-squared: 0.8187    
#Residual standard error: 2.813   

#The highest p-value is for the interaction between ADJOE and DRB (0.939257), so we will remove it
cbb_lmod4 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod4)

#Adjusted R-squared: 0.8188     
#Residual standard error: 2.813    

#The highest p-value is for the interaction between FTR and EFG_D (0.952700), so we will remove it
cbb_lmod5 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod5)

#Adjusted R-squared: 0.8189      
#Residual standard error: 2.812  

#The highest p-value is for the interaction between TOR and DRB (0.849200), so we will remove it
cbb_lmod6 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod6)

#Adjusted R-squared: 0.8189       
#Residual standard error: 2.812   

#The highest p-value is for the interaction between EFG_D and FTRD (0.812797), so we will remove it
cbb_lmod7 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod7)

#Adjusted R-squared: 0.819        
#Residual standard error: 2.811    

#The highest p-value is for the interaction between EFG_O and ORB (0.786302), so we will remove it
cbb_lmod8 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod8)

#Adjusted R-squared: 0.819        
#Residual standard error: 2.811    

#The highest p-value is for the interaction between EFG_O and ADJOE (0.732962), so we will remove it
cbb_lmod9 <- lm(W ~ EFG_O * FTR + EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod9)

#Adjusted R-squared: 0.8191        
#Residual standard error: 2.81 

#The highest p-value is for the interaction between EFG_O and FTR (0.756476), so we will remove it
cbb_lmod10 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod10)

#Adjusted R-squared: 0.8192         
#Residual standard error: 2.81

#The highest p-value is for the interaction between EFG_O and DRB (0.682368), so we will remove it
cbb_lmod11 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod11)

#Adjusted R-squared: 0.8192          
#Residual standard error: 2.81

#The highest p-value is for the interaction between ADJOE and ORB (0.633781), so we will remove it
cbb_lmod12 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod12)

#Adjusted R-squared: 0.8193           
#Residual standard error: 2.809 

#The highest p-value is for the interaction between ORB and TORD (0.349588), so we will remove it
cbb_lmod13 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod13)

#Adjusted R-squared: 0.8193            
#Residual standard error: 2.809

#The highest p-value is for the interaction between ORB and EFG_D (0.348244), so we will remove it
cbb_lmod14 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * FTRD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod14)

#Adjusted R-squared: 0.8193             
#Residual standard error: 2.809

#The highest p-value is for the interaction between TOR and FTRD (0.266875), so we will remove it
cbb_lmod15 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * FTRD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod15)

#Adjusted R-squared: 0.8193              
#Residual standard error: 2.809

#The highest p-value is for the interaction between ADJDE and FTRD (0.312580), so we will remove it
cbb_lmod16 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * FTRD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod16)

#Adjusted R-squared: 0.8193              
#Residual standard error: 2.809

#The highest p-value is for the interaction between ORB and FTRD (0.276203), so we will remove it
cbb_lmod17 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod17)

#Adjusted R-squared: 0.8192               
#Residual standard error: 2.809

#The highest p-value is for the interaction between EFG_O and FTRD (0.363824), so we will remove it
cbb_lmod18 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * TORD + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod18)

#Adjusted R-squared: 0.8193                
#Residual standard error: 2.809

#The highest p-value is for the interaction between EFG_O and TORD (0.378292), so we will remove it
cbb_lmod19 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod19)

#Adjusted R-squared: 0.8193                
#Residual standard error: 2.809

#The highest p-value is for the interaction between ADJOE and FTRD (0.271743), so we will remove it
cbb_lmod20 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod20)

#Adjusted R-squared: 0.8193                
#Residual standard error: 2.809

#The highest p-value is for the interaction between ADJOE and TORD (0.251760), so we will remove it
cbb_lmod21 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * TORD + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod21)

#Adjusted R-squared: 0.8192                 
#Residual standard error: 2.809

#The highest p-value is for the interaction between TOR and TORD (0.357474), so we will remove it
cbb_lmod22 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod22)

#Adjusted R-squared: 0.8192                 
#Residual standard error: 2.809

#The highest p-value is for the interaction between TOR and EFG_D (0.373573), so we will remove it
cbb_lmod23 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ORB + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod23)

#Adjusted R-squared: 0.8193                  
#Residual standard error: 2.809

#The highest p-value is for the interaction between TOR and ORB (0.352387), so we will remove it
cbb_lmod24 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + TOR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod24)

#Adjusted R-squared: 0.8193                  
#Residual standard error: 2.809

#The highest p-value is for the interaction between TOR and ADJOE (0.379553), so we will remove it
cbb_lmod25 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod25)

#Adjusted R-squared: 0.8193                  
#Residual standard error: 2.809

#The highest p-value is for the interaction between DRB and ORB (0.250255), so we will remove it
cbb_lmod26 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod26)

#Adjusted R-squared: 0.8193                  
#Residual standard error: 2.809

#The highest p-value is for the interaction between EFG_D and DRB (0.273771), so we will remove it
cbb_lmod27 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + DRB * ADJDE + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod27)

#Adjusted R-squared: 0.8193                  
#Residual standard error: 2.809

#The highest p-value is for the interaction between ADJDE and DRB (0.276484), so we will remove it
cbb_lmod28 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + EFG_D * TORD + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod28)

#Adjusted R-squared: 0.8192                   
#Residual standard error: 2.809

#The highest p-value is for the interaction between EFG_D and TORD (0.096599), so we will remove it
cbb_lmod29 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + TORD * DRB + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod29)

#Adjusted R-squared: 0.8191                    
#Residual standard error: 2.81 

#The highest p-value is for the interaction between EFG_D and ADJDE (0.144965), so we will remove it
cbb_lmod30 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + FTRD * TORD + FTRD * DRB + TORD * DRB + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod30)

#Adjusted R-squared: 0.8191                    
#Residual standard error: 2.811  

#The highest p-value is for the interaction between FTR and TORD (0.052155), so we will remove it
cbb_lmod31 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTR * ADJOE + FTRD * TORD + FTRD * DRB + TORD * DRB + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod31)

#Adjusted R-squared: 0.8189                     
#Residual standard error: 2.812   

#The highest p-value is for the interaction between FTR and ADJOE (0.129463), so we will remove it
cbb_lmod32 <- lm(W ~ EFG_O * TOR + FTR * TOR + FTRD * TORD + FTRD * DRB + TORD * DRB + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod32)

#Adjusted R-squared: 0.8188                      
#Residual standard error: 2.813  

#The highest p-value is for the interaction between TOR and FTR (0.177910), so we will remove it
cbb_lmod33 <- lm(W ~ EFG_O * TOR + FTRD * TORD + FTRD * DRB + TORD * DRB + EFG_O * EFG_D + EFG_O * ADJDE + FTR * FTRD + FTR * DRB + FTR * ADJDE + TOR * ADJDE + ORB * ADJDE + ADJOE * EFG_D + ADJOE * ADJDE, data = cbb)
summary(cbb_lmod33)

#Adjusted R-squared: 0.8187                       
#Residual standard error: 2.813   

#At this point, all of our interactions are significant at the 5% LOS, so we will move on to making include/exclude decisions about the individual terms.

#All of the individual terms are either significant or are part of significant interactions, so we will keep them. Therefore, we are at our final model.

#Adjusted R-squared for final model: 0.8187                       
#Residual standard error for final model: 2.813

#Confidence Intervals for Model Coefficients
confint(cbb_lmod33)

#Checking the model for multicollinearity, influential points, and that the errors are normally distributed with mean 0 and constant variance

#Checking for multicollinearity with VIFs
vif(cbb_lmod33, type = "predictor")
#Although the VIFs are extremely high for most of the explanatory variables, we can also see that most of the variables are interacting with at least one, and in many cases, several other explanatory variables, which is probably what is driving up the VIFs.

#Checking for influential points
cbb_inf <- as.data.frame(influencePlot(cbb_lmod33, main = "Influence Plot", id=list(method = "noteworthy", 
                                                                                    n=3, cex=1, col=carPalette()[1], location="lr")))
#It looks like there are some potentially concerning points

augment(cbb_lmod33)

#Cook's distance
cbb_cooks <- as.data.frame(cooks.distance(cbb_lmod33))
cbb_cooks <- cbb_cooks %>%
  rename(CooksD = 1) %>%
  mutate(ID = row_number()) %>%
  select(ID, CooksD)

View(cbb_cooks)
#There are no NaN points, which is good

#View hat values, cook's distance, and studentized residuals
cbb_influential <- augment(cbb_lmod33) %>%
  mutate(ID = row_number()) %>%
  select(ID, W:.std.resid)

View(cbb_influential)

#points with Cook's distance > 4/n
cbb_influential_points <- cbb_cooks %>%
  filter(CooksD > (4/nrow(cbb_cooks)) | is.nan(CooksD)) %>% 
  arrange(desc(CooksD))

View(cbb_influential_points)

#or points with Cook's distance > 4*mean
cbb_influential_points2 <- cbb_cooks %>%
  filter(CooksD > (4*mean(CooksD, na.rm = TRUE)) | is.nan(CooksD)) %>%
  arrange(desc(CooksD))

View(cbb_influential_points2)

4*mean(cbb_cooks$CooksD, na.rm = TRUE)

#None of these points are super concerning to me, even those with the highest Cook's distance, as they are not super high and looking at the handful with the highest values, they are certainly possible observations. Additionally, with the nature of the data, we are less concerned with people misreporting or lying on a survey, etc. So, we will keep all of the observations.

#Checking model assumptions - errors that are N~(0, sigma^2)

#Normality - qqplot
augment(cbb_lmod33) %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() + 
  stat_qq_line()
#the qqplot is indicating non-normality

#Normality test
shapiro.test(augment(cbb_lmod33)$.resid)
#The p-value is ~0, so the residuals are NOT normal

#Mean 0
mean(augment(cbb_lmod33)$.resid)
#This assumption holds, mean is ~0

#Constant variance (homoskedasticity)
augment(cbb_lmod33) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Residuals vs Fitted Values:  Full Final Model")
#We would like to see constant variance in this graph, but are NOT seeing this here. The variance takes on somewhat of a cone shape, with the variance getting larger as the fitted values get larger. Heteroskedasticity and non-normality often go together, which is what we've found here.

#Because we are seeing that our residuals are heteroskedastic and non-normal, we will perform the corrective action of using a natural log transformation of the dependent variable W

#Plotting the results
#Predicted vs Actual 
augment(cbb_lmod33) %>%
  ggplot(aes(x = W, y = .fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Full")


#### LOG MODEL FOR "FULL" ####

#Adding log(W) to the datasets
cbb$ln_W <- log(cbb$W)
cbb_o$ln_W <- log(cbb_o$W)
cbb_d$ln_W <- log(cbb_d$W)

#Removing the -inf ln_W observations
cbb <- cbb %>%
  filter(ln_W >= 0)

cbb_o <- cbb_o %>%
  filter(ln_W >= 0)

cbb_d <- cbb_d %>%
  filter(ln_W >= 0)

#Scatterplots
ggpairs(cbb_o[c(7, 2, 3, 4, 5, 6)]) #offensive play

ggpairs(cbb_d[c(7, 2, 3, 4, 5, 6)]) #defensive play

#Let's try this again
cbb_log <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ORB + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_log)

#Adjusted R-squared: 0.7992   
#Residual standard error: 0.2233 

#We will start by making include/exclude decisions on the interaction terms
#The highest p-value is for the interaction between TOR and ORB (0.908063), so we will remove it
cbb_log1 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * FTRD + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_log1)

#Adjusted R-squared: 0.7993    
#Residual standard error: 0.2233  

#The highest p-value is for the interaction between EFG_D and FTRD (0.861941), so we will remove it
cbb_log2 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ORB + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_log2)

#Adjusted R-squared: 0.7994     
#Residual standard error: 0.2232   

#The highest p-value is for the interaction between EFG_O and ORB (0.655923), so we will remove it
cbb_log3 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * TORD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_log3)

#Adjusted R-squared: 0.7994     
#Residual standard error: 0.2232

#The highest p-value is for the interaction between FTR and TORD (0.562954), so we will remove it
cbb_log4 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * DRB + ADJOE * ADJDE, data = cbb)
summary(cbb_log4)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2232

#The highest p-value is for the interaction between ADJOE and DRB (0.472914), so we will remove it
cbb_log5 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * DRB + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log5)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2232

#The highest p-value is for the interaction between ORB and DRB (0.457562), so we will remove it
cbb_log6 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * DRB + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log6)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2231 

#The highest p-value is for the interaction between FTR and DRB (0.355441), so we will remove it
cbb_log7 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * FTRD + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log7)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2231

#The highest p-value is for the interaction between FTR and FTRD (0.248699), so we will remove it
cbb_log8 <- lm(log(W) ~ EFG_O * FTR + EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log8)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2231

#The highest p-value is for the interaction between EFG_O and FTR (0.256480), so we will remove it
cbb_log9 <- lm(log(W) ~ EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * TORD + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log9)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2232 

#The highest p-value is for the interaction between TORD and FTRD (0.256768), so we will remove it
cbb_log10 <- lm(log(W) ~ EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + EFG_D * ADJDE + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log10)

#Adjusted R-squared: 0.7995      
#Residual standard error: 0.2232 

#The highest p-value is for the interaction between EFG_D and ADJDE (0.170291), so we will remove it
cbb_log11 <- lm(log(W) ~ EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ORB + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log11)

#Adjusted R-squared: 0.7994       
#Residual standard error: 0.2232 

#The highest p-value is for the interaction between FTR and ORB (0.119778), so we will remove it
cbb_log12 <- lm(log(W) ~ EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + FTR * ADJOE + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log12)

#Adjusted R-squared: 0.7993        
#Residual standard error: 0.2233 

#The highest p-value is for the interaction between ADJOE and FTR (0.224728), so we will remove it
cbb_log13 <- lm(log(W) ~ EFG_O * TOR + EFG_O * ADJOE + FTR * TOR + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log13)

#Adjusted R-squared: 0.7993        
#Residual standard error: 0.2233

#The highest p-value is for the interaction between EFG_O and TOR (0.050987), so we will remove it
cbb_log14 <- lm(log(W) ~ EFG_O * ADJOE + FTR * TOR + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * DRB + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log14)

#Adjusted R-squared: 0.7991         
#Residual standard error: 0.2234 

#The highest p-value is for the interaction between TOR and DRB (0.143157), so we will remove it
cbb_log15 <- lm(log(W) ~ EFG_O * ADJOE + FTR * TOR + TOR * ADJOE + ORB * ADJOE + EFG_D * TORD + EFG_D * DRB + FTRD * DRB + FTRD * ADJDE + TORD * DRB + TORD * ADJDE + DRB * ADJDE + EFG_O * EFG_D + EFG_O * FTRD + EFG_O * TORD + EFG_O * DRB + EFG_O * ADJDE + FTR * EFG_D + FTR * ADJDE + TOR * EFG_D + TOR * FTRD + TOR * TORD + TOR * ADJDE + ORB * EFG_D + ORB * FTRD + ORB * TORD + ORB * ADJDE + ADJOE * EFG_D + ADJOE * FTRD + ADJOE * TORD + ADJOE * ADJDE, data = cbb)
summary(cbb_log15)

#Adjusted R-squared: 0.799         
#Residual standard error: 0.2234

#At this point, all of our interactions are significant at the 5% LOS, so we will move on to making include/exclude decisions about the individual terms.

#All of the individual terms are either significant or are part of significant interactions, so we will keep them. The intercept is not significant at the 5% LOS but help anchor our model, so we will keep it. Therefore, we are at our final model.

#Adjusted R-squared for final model: 0.799                       
#Residual standard error for final model: 0.2234

#Confidence Intervals for Model Coefficients
confint(cbb_log15)

#Checking the model for multicollinearity, influential points, and that the errors are normally distributed with mean 0 and constant variance

#Checking for multicollinearity with VIFs
vif(cbb_log15, type = "predictor")
#The VIFs are still extremely high, but again, we can also see that they are interacting with several other explanatory variables, which is probably what is driving up the VIFs

cbb_log_cooks <- as.data.frame(cooks.distance(cbb_log15))
cbb_log_cooks <- cbb_log_cooks %>%
  rename(CooksD = 1) %>%
  mutate(ID = row_number()) %>%
  select(ID, CooksD)

View(cbb_log_cooks)
#Still no NaN values, which is good

cbb_log_inf <- cbb_log_cooks %>%
  filter(CooksD > (4*mean(CooksD, na.rm = TRUE)) | is.nan(CooksD)) %>%
  arrange(desc(CooksD))

View(cbb_log_inf)
#Still no super concerning Cook's Distance values

#Checking model assumptions in the log-transformed space - errors that are N~(0, sigma^2)

#Normality - qqplot
augment(cbb_log15) %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() + 
  stat_qq_line()
#the qqplot is indicating non-normality

#Normality test
shapiro.test(augment(cbb_log15)$.resid)
#The p-value is ~0, so the residuals are NOT normal

#Mean 0
mean(augment(cbb_log15)$.resid)
#This assumption holds, mean is ~0

#Constant variance (homoskedasticity)
augment(cbb_log15) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Residuals vs Fitted Values:  Full Final Log Model")
#There is even more of a cone shape here than in the original space... now the cone is in the opposite direction, where the variance gets smaller as the fitted values get larger.

#So we still have heteroskedasticity and non-normality
#I would even say that the original model was more normal and homoskedastic

#More transformations and/or modeling are needed in order to generate a regression model that passes all model assumptions. In this case, we are more concerned with interpretability, so we will not continue with further transformations.

#Because both the original model and the log model did meet the normality and homoskedasticity assumptions, we will use both to predict on the test set and determine which to use from there.

#Plotting the results
#Predicted vs Actual 
cbb$log_fitted <- augment(cbb_log15)$.fitted
cbb$fitted <- exp(augment(cbb_log15)$.fitted)

cbb %>%
  ggplot(aes(x = W, y = fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Full Log")


#### PREDICTIONS ####

#Predict W values for a set of people out-of-sample

#ORIGINAL MODEL

#cleaning data
#offensive play
cbb_test_o <- cbb_test %>%
  select(W, EFG_O, FTR, TOR, ORB, ADJOE) %>%
  drop_na()

#defensive play
cbb_test_d <- cbb_test %>%
  select(W, EFG_D, FTRD, TORD, DRB, ADJDE) %>%
  drop_na()

#combined
cbb_test <- cbb_test %>%
  select(W, EFG_O, FTR, TOR, ORB, ADJOE, EFG_D, FTRD, TORD, DRB, ADJDE, TEAM, YEAR) %>%
  drop_na()

#exploring data visually
ggpairs(cbb_test_o[c(1, 2, 3, 4, 5, 6)]) #offensive play
ggpairs(cbb_test_d[c(1, 2, 3, 4, 5, 6)]) #defensive play
#these are looking super similar to the training set, which is good!

#Since these observations are out-of-sample, we use prediction
cbb_pred <- as.data.frame(predict(cbb_lmod33, newdata = cbb_test, interval = "prediction"))

#The predicted values (fit) and the prediction interval for each (lwr and upr) are in this dataframe
cbb_pred

#We will use MAPE to test how accurate our predictions are
#Combining the two tables
cbb_test_pred <- cbb_test %>%
  bind_cols(cbb_pred)

cbb_test_pred

cbb_accuracy <- data.frame(MAPE = MAPE(cbb_test_pred$fit, cbb_test_pred$W))

cbb_accuracy

#MAPE for this model on the test data is 0.1451 and indicates that we have approximately 14.51% error in our predictions.

#Plotting the results
#Predicted vs Actual 
cbb_test_pred %>%
  ggplot(aes(x = W, y = fit)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Full Test")

#Once again, it looks like the model tends to under predict, but is overall a pretty good fit.

#LOG MODEL

#cleaning data
#Adding log(W) to the datasets
cbb_test$ln_W <- log(cbb_test$W)

#Removing the -inf ln_W observations
cbb_test <- cbb_test %>%
  filter(ln_W >= 0)

#dropping N/As
cbb_test <- cbb_test %>%
  drop_na()

#Since these observations are out-of-sample, we use prediction
cbblog_pred <- as.data.frame(predict(cbb_log15, newdata = cbb_test, interval = "prediction"))

#The predicted values (fit) and the prediction interval for each (lwr and upr) are in this dataframe
cbblog_pred

#We will use MAPE to test how accurate our predictions are
#Combining the two tables
cbblog_test_pred <- cbb_test %>%
  bind_cols(cbblog_pred)

cbblog_test_pred

cbblog_test_pred <- cbblog_test_pred %>%
  mutate(fit_og = exp(fit),
         lwr_og = exp(lwr),
         upr_og = exp(upr),
         resids_og = W - fit_og)

cbblog_test_pred

cbblog_accuracy <- data.frame(MAPE = MAPE(cbblog_test_pred$fit_og, cbblog_test_pred$W))

cbblog_accuracy

#MAPE for this model on the test data is 0.1528 and indicates that we have approximately 15.28% error in our predictions.

#Plotting the results
#Predicted vs Actual
cbblog_test_pred %>%
  ggplot(aes(x = W, y = fit_og)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Predicted vs Actual Wins: Full Log Test")

#The log model also tends to under predict wins.

#The MAPE for the log model is close to the MAPE for the original model, but slightly higher. The visuals of Predicted vs Actual also seem like the original model is better, as well as the Adjusted R-squared values and the Residual standard errors. Additionally, going with the original model increases interpretability. So, we will go with the original model.

#Getting predictions for specific teams for presentation
pres_teams_od <- cbb_test_pred %>%
  filter(TEAM == "VCU" | TEAM == "Virginia" | TEAM == "Duke" | TEAM == "Pittsburgh") %>%
  select(TEAM, W, fit, lwr, upr, YEAR)

pres_teams_od


#### INTERPRETATIONS & CONCLUSIONS ####

#Full Model Interpretations

#The interpretation of the intercept (constant coefficient) (-57.629989) is that the estimated wins (W) when EFG_D, FTRD, TORD, DRB, ADJDE, EFG_O, FTR, TOR, ORB, ADJOE, and all significant interactions are 0 is -57.629989. This is not a practical interpretation because W cannot be negative - the smallest value it can practically take on is 0.
#Because all of the explanatory variables are part of interaction terms, it is difficult to understand and interpret their individual coefficients.
#However, for the interaction term between EFG_O and TOR, the interpretation of the coefficient (-0.034534) is that, holding all other variables and interactions constant, as effective field goal percentage shot (EFG_O) increases by one unit, the estimated change in W is dependent on turnover rate (TOR).
#For the interaction term between FTRD and TORD, the interpretation of the coefficient (0.013539) is that, holding all other variables and interactions constant, as free throw rate allowed (FTRD) increases by one unit, the estimated change in W is dependent on steal rate (TORD).
#For the interaction term between FTRD and DRB, the interpretation of the coefficient (-0.009804) is that, holding all other variables and interactions constant, as free throw rate allowed (FTRD) increases by one unit, the estimated change in W is dependent on offensive rebound rate allowed (DRB).
#For the interaction term between TORD and DRB, the interpretation of the coefficient (0.023907) is that, holding all other variables and interactions constant, as steal rate (TORD) increases by one unit, the estimated change in W is dependent on offensive rebound rate allowed (DRB).
#For the interaction term between EFG_O and EFG_D, the interpretation of the coefficient (0.043687) is that, holding all other variables and interactions constant, as effective field goal percentage shot (EFG_O) increases by one unit, the estimated change in W is dependent on effective field goal percentage allowed (EFG_D).
#For the interaction term between EFG_O and ADJDE, the interpretation of the coefficient (-0.025184) is that, holding all other variables and interactions constant, as effective field goal percentage shot (EFG_O) increases by one unit, the estimated change in W is dependent on adjusted defensive efficiency (ADJDE).
#For the interaction term between FTRD and FTR, the interpretation of the coefficient (-0.004802) is that, holding all other variables and interactions constant, as free throw rate allowed (FTRD) increases by one unit, the estimated change in W is dependent on free throw rate (FTR).
#For the interaction term between DRB and FTR, the interpretation of the coefficient (-0.006404) is that, holding all other variables and interactions constant, as offensive rebound rate allowed (DRB) increases by one unit, the estimated change in W is dependent on free throw rate (FTR).
#For the interaction term between ADJDE and FTR, the interpretation of the coefficient (-0.007754) is that, holding all other variables and interactions constant, as adjusted defensive efficiency (ADJDE) increases by one unit, the estimated change in W is dependent on free throw rate (FTR).
#For the interaction term between TOR and ADJDE, the interpretation of the coefficient (0.020270) is that, holding all other variables and interactions constant, as turnover rate (TOR) increases by one unit, the estimated change in W is dependent on adjusted defensive efficiency (ADJDE).
#For the interaction term between ADJDE and ORB, the interpretation of the coefficient (-0.004785) is that, holding all other variables and interactions constant, as adjusted defensive efficiency (ADJDE) increases by one unit, the estimated change in W is dependent on offensive rebound rate (ORB).
#For the interaction term between EFG_D and ADJOE, the interpretation of the coefficient (-0.020769) is that, holding all other variables and interactions constant, as effective field goal percentage allowed (EFG_D) increases by one unit, the estimated change in W is dependent on adjusted offensive efficiency (ADJOE).
#For the interaction term between ADJDE and ADJOE, the interpretation of the coefficient (0.009465) is that, holding all other variables and interactions constant, as adjusted defensive efficiency (ADJDE) increases by one unit, the estimated change in W is dependent on adjusted offensive efficiency (ADJOE).

#Conclusions: Offensive Play vs. Defensive Play vs. Both in Predicting Wins

#The full model ended up including all of the offensive and defensive explanatory variables, and each of the explanatory variables was included in at least one interaction term. There were several interaction terms where the same measure for both offensive and defensive play were interacting with each other (examples: effective field goal percentage shot and effective field goal percentage allowed, free throw rate and free throw rate allowed, adjusted offensive efficiency and adjusted defensive efficiency), which was interesting. Additionally, adjusted defensive efficiency (ADJDE) interacted with five other explanatory variables, indicating that its effect on wins is dependent on several other explanatory variables in this model.
#The Adjusted R-squared for the full model (0.8179) was much higher than both the offensive (0.6222) and defensive (0.4885) models, the Residual standard error was much lower (2.81) than the offensive (4.062) and defensive (4.726) models, and the MAPE was much lower (0.1451) than the offensive (0.2253) and defensive (0.2721) models, supporting the claim that it is critical to consider both offensive and defensive play in predicting wins (W).


#The offensive vs defensive analysis showed how important BOTH are in explaining wins, with the full model doing significantly better than the models that considered only offensive or only defensive play. While the number of wins are obviously of interest in sports analytics of any sport, for college basketball in particular, predicting performance in March Madness may also be of equal interest. So, we are interested in what factors contribute to success in March Madness, and in comparing the model we build to predict this with the model that predicts wins.


####LOGISTIC REGRESSION####

#The second question we will explore is:
#What factors contribute to a team making it to the Sweet Sixteen?


####DATA WRANGLING AND FORMATTING####

#replace blanks  and NAs with N/A
cbb1$POSTSEASON<-cbb1$POSTSEASON |> replace_na('N/A')
cbb1$SEED<-cbb1$SEED |> replace_na('N/A')

#Check to see if all NAs are gone 
names(which(colSums(is.na(cbb1))>0))
cbb1[!complete.cases(cbb1), ]
colSums(is.na(cbb1))

#Create new columns 
cbb1 <- cbb1 %>% 
  mutate(POSTSEASON.orig = POSTSEASON)
cbb1 <- cbb1 %>% 
  mutate(POSTSEASON.YN = POSTSEASON)

#set categorical variables 1 for made postseason, 0 for did not make 
cbb1$POSTSEASON <-  if_else(cbb1$POSTSEASON == 'Champions', 1, 
                            if_else(cbb1$POSTSEASON == '2ND', 1,
                                    if_else(cbb1$POSTSEASON == 'F4', 1,
                                            if_else(cbb1$POSTSEASON == 'E8', 1,
                                                    if_else(cbb1$POSTSEASON == 'S16', 1,
                                                            0)))))

cbb1$SEED <- sapply(cbb1$SEED,gsub,pattern='N/A',replacement = '0' )

cbb1 <- cbb1 %>%
  mutate(POSTSEASON.YN = ifelse(POSTSEASON == 1, "Yes", "No"))

#Factor specific variables 
cbb1$TEAM <- as.factor(cbb1$TEAM)
cbb1$CONF <- as.factor(cbb1$CONF)
cbb1$SEED <- as.factor(cbb1$SEED)

#Change variable to integer
cbb1$POSTSEASON<- as.integer(cbb1$POSTSEASON)

#Create ID Column
cbb1<- tibble::rowid_to_column(cbb1, "ID")

train2 <- cbb1 %>%
  filter(YEAR < 2022)

test2 <- cbb1 %>%
  filter(YEAR > 2021)


#Breakdown of variables 

ggduo(data = cbb1, columnsX = "POSTSEASON.YN", columnsY = c('ADJOE', 'ADJDE','EFG_D', 'X2P_D', 'X3P_D', 'FTRD', 'DRB'))

ggduo(data = cbb1, columnsX = "POSTSEASON.YN", columnsY = c('ADJOE', 'ADJDE','EFG_D', 'X2P_D', 'X3P_D', 'X2P_O', 'X3P_O'))

ggduo(data = cbb1, columnsX ="POSTSEASON.YN", columnsY = c(
  'TOR', 'TORD', 'ORB', 'DRB', 'FTR', 'FTRD','BARTHAG', 'EFG_O','EFG_D', 'ADJ_T')) 


####LOGISTIC REGRESSION FORMULAS####

#We did not use the G, W or WAB variables due to the fact that they included the wins and games for the NCAA Tournament 

###
cbb1_lor1 <- glm(POSTSEASON ~ TEAM + CONF + G + W + ADJOE + ADJDE + BARTHAG + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + WAB + SEED + YEAR, data = train2, family = 'binomial')

summary(cbb1_lor1)
with(cbb1_lor1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


cbb1_lor26 <- glm(POSTSEASON ~ TEAM + CONF + ADJOE + ADJDE + BARTHAG + EFG_O + EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + SEED + YEAR, data = train2, family = 'binomial')
summary(cbb1_lor26)

cbb1_lor27 <- glm(POSTSEASON ~ CONF + ADJOE + ADJDE + BARTHAG + EFG_O + EFG_D + TOR + TORD + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + YEAR, data = train2, family = 'binomial')
summary(cbb1_lor27)

cbb1_lor28 <- glm(POSTSEASON ~ ADJOE + ADJDE + BARTHAG + EFG_O + EFG_D + TOR + TORD + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + YEAR, data = train2, family = 'binomial')
summary(cbb1_lor28)

cbb1_lor29 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T + YEAR, data = train2, family = 'binomial')
summary(cbb1_lor29)

cbb1_lor30 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T, data = train2, family = 'binomial')
summary(cbb1_lor30)

cbb1_lor31 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + DRB + FTRD + X2P_O + X2P_D + X3P_O + X3P_D + ADJ_T, data = train2, family = 'binomial')
summary(cbb1_lor31)

cbb1_lor32 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + DRB + FTRD + X2P_O + X2P_D + X3P_D + ADJ_T, data = train2, family = 'binomial')
summary(cbb1_lor32)

cbb1_lor33 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD + DRB + FTRD + X2P_O + X2P_D + X3P_D , data = train2, family = 'binomial')
summary(cbb1_lor33)

cbb1_lor34 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + DRB + FTRD + X2P_O + X2P_D + X3P_D , data = train2, family = 'binomial')
summary(cbb1_lor34)

cbb1_lor35 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_D + TOR + DRB + FTRD + X2P_O + X2P_D + X3P_D , data = train2, family = 'binomial')
summary(cbb1_lor35)

cbb1_lor36 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_D + DRB + FTRD + X2P_O + X2P_D + X3P_D , data = train2, family = 'binomial')
summary(cbb1_lor36)

cbb1_lor37 <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_D + DRB + FTRD + X2P_D + X3P_D , data = train2, family = 'binomial')
summary(cbb1_lor37)


with(cbb1_lor37, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#model is significant 

#We ended up choosing 37 as our final model.


####PREDICTING 2022 and 2023 SWEET SIXTEEEN TEAMS#### 

#exclude 2022/2023 used as test data 

#trained model on all data from 2013-2021

cbb1_lor37pred <- glm(POSTSEASON ~ ADJOE + ADJDE + EFG_D + DRB + FTRD + X2P_D + X3P_D, data = train2, family = 'binomial')


with(cbb1_lor37pred, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#predictions 
cbb1_lor_out2 <- augment(cbb1_lor37pred) %>%
  mutate(pred_prob = predict(cbb1_lor37pred, type = "response"))

cbb1_lor_out2

#test using 2022 & 2023 data 
cbb1_lor_test_out2 <- test2 %>%
  mutate(pred_prob = predict(cbb1_lor37pred, newdata = test2, type = "response")) 

cbb1_lor_test_out2

View(cbb1_lor_test_out2)


####ROC curves####
cbb_train_roc2 <- roc(as.factor(cbb1_lor_out2$POSTSEASON), cbb1_lor_out2$pred_prob)
cbb_train_roc_plot2 <- ggroc(cbb_train_roc2, legacy.axes = TRUE) +
  labs(x = 'False-positive rate', y = 'True-positive rate',
       title = 'ROC curve - training set') +
  annotate('text', x = 0.5, y = 0.5, label = paste0('AUC: ', round(auc(cbb_train_roc2), digits = 2)))

cbb_train_roc_plot2 

#ROC curve for test set
cbb_test_roc2 <- roc(as.factor(cbb1_lor_test_out2$POSTSEASON), cbb1_lor_test_out2$pred_prob)
cbb_test_roc_plot2 <- ggroc(cbb_test_roc2, legacy.axes = TRUE) +
  labs(x = 'False-positive rate', y = 'True-positive rate',
       title = 'ROC curve - test set') +
  annotate('text', x = 0.5, y = 0.5, label = paste0('AUC: ', round(auc(cbb_test_roc2), digits = 2)))

cbb_test_roc_plot2

#ROC curves, training .98 test .97, which is good 

ggarrange(cbb_train_roc_plot2 , cbb_test_roc_plot2 , nrow = 1)


####Sensitivity, specificity, accuracy of confusion matrices####  

calculateAccTable <- function(predictions, actuals, cutoff) {
  predicted_classes <- ifelse(predictions > cutoff, 1, 0)
  confusion_out <- confusionMatrix(as.factor(predicted_classes), as.factor(actuals), 
                                   positive = "1", mode = "everything")
  return(data.frame(Accuracy = confusion_out$overall[[1]], Precision = confusion_out$byClass[[5]], 
                    Sensitivity = confusion_out$byClass[[1]], Specificity = confusion_out$byClass[[2]]))
}


cutoffs <- seq(0.1, 0.9, by = 0.05)


acc_matrix_df2 <- data.frame()

#Calculate and store confusion matrix elements for each cutoff
for (cutoff in cutoffs) {
  acc_calc <- calculateAccTable(cbb1_lor_out2$pred_prob, as.factor(cbb1_lor_out2$POSTSEASON), cutoff)
  acc_matrix_df2 <- rbind(acc_matrix_df2, cbind(cutoff = cutoff, acc_calc))
}


#Print the dataframe with confusion matrix elements
acc_matrix_df2


####Sensitivity, specificity, accuracy of confusion matrices broken down further####  

calculateAccTable <- function(predictions, actuals, cutoff) {
  predicted_classes <- ifelse(predictions > cutoff, 1, 0)
  confusion_out <- confusionMatrix(as.factor(predicted_classes), as.factor(actuals), 
                                   positive = "1", mode = "everything")
  return(data.frame(Accuracy = confusion_out$overall[[1]], Precision = confusion_out$byClass[[5]], 
                    Sensitivity = confusion_out$byClass[[1]], Specificity = confusion_out$byClass[[2]]))
}


cutoffs <- seq(0.3, 0.5, by = 0.025)


acc_matrix_df2 <- data.frame()

#Calculate and store confusion matrix elements for each cutoff
for (cutoff in cutoffs) {
  acc_calc <- calculateAccTable(cbb1_lor_out2$pred_prob, as.factor(cbb1_lor_out2$POSTSEASON), cutoff)
  acc_matrix_df2 <- rbind(acc_matrix_df2, cbind(cutoff = cutoff, acc_calc))
}


#Print the dataframe with confusion matrix elements
acc_matrix_df2


#create a function to calculate confusion matrix for severa cutoff values
calculateConfusionMatrix <- function(predictions, actuals, cutoff) {
  predicted_classes <- ifelse(predictions > cutoff, 1, 0)
  confusion_matrix <- table(Predicted = predicted_classes, Actual = actuals) #manually generating the confusion matrix with a table  
  return(confusion_matrix)
}


#Calculate and store confusion matrices for each cutoff
confusion_matrices <- lapply(cutoffs, function(cutoff) {
  calculateConfusionMatrix(cbb1_lor_out2$pred_prob, as.factor(cbb1_lor_out2$POSTSEASON), cutoff)
})

#Print confusion matrices for cutoffs between .1-.7
for (i in 1:length(cutoffs)) {
  print(paste("cutoff:", cutoffs[i]))
  print(confusion_matrices[[i]])
  print("----------")
}

#chose cutoff of .3 as that gave us optimal positive predictions
chosen_cut <- 0.3

#generate the confusion matrix and performance measures
cbb1_lor_test_out2 <- cbb1_lor_test_out2 %>%
  mutate(pred_class = ifelse(pred_prob > chosen_cut, 1, 0))

cbb1_lor_test_out2

cbb1_test_result2 <- confusionMatrix(as.factor(cbb1_lor_test_out2$pred_class), as.factor(cbb1_lor_test_out2$POSTSEASON), 
                                     positive = "1", mode = "everything")

#confusion matrix 
cbb1_test_result2

# 19/32 correct 
19/32


cbbpredict.2023 <- cbb1_lor_test_out2 [cbb1_lor_test_out2$POSTSEASON.YN == 'Yes', ]

#Shows actual teams that made the sweet 16 in 2022 and 2023 and probability predictions using our model 
View(cbbpredict.2023)

#shows correct predictions model made, 19/32
cbbcorrectpredictedteams <- subset(cbbpredict.2023, pred_prob>.3) 
View(cbbcorrectpredictedteams)

#shows predictions for 2022, 10/16 ~63% success rate 
10/16
Teams2022 <- subset(cbbpredict.2023, YEAR == 2022 & pred_prob>.3) 
View(Teams2022)

#shows predictions for 2023, 9/16 ~56% success rate 
9/16
Teams2023 <- subset(cbbpredict.2023, YEAR == 2023 & pred_prob>.3) 
View(Teams2023)

####LOGISTIC REGRESSION CONCLUSIONS####
#These are the overall results of our model for predicting the 2022 and 2023 sweet sixteen. This shows that correct classification (accuracy) occurred ~96% of the time, precision showed that ~59% of the time the positive prediction was correct, the false positive rate (specificity) was low at ~98% but the sensitivity of ~59% showed there were many false negatives. Since our question is how many teams actually made it into the sweet sixteen we want true positives. Our true positive levels were okay and our false positive rate was less than ideal. In 2022 and 2023 our successful prediction rates were ~63% and ~56% respectively. Overall with our question of what factors influence teams making it to the sweet 16 we can say that  positive offensive efficiency (scoring more points) and a low defensive efficiency (giving up less points) were the biggest factors in determining if teams would make it to the sweet sixteen. This was followed by shooting percentage defense (EFG_D) and two point and three point shooting defense. This shows that both offense and defense are needed to win games and advance but teams that have strong defense are more likely to advance.


####FINAL REMARKS####
#While offensive play better predicted the number of wins than defensive play in our linear regression analysis, ultimately, considering both offensive and defensive play provided the best model in predicting the number of wins. When comparing that with our logistic regression model to predict whether a team makes it to the Sweet Sixteen round of the March Madness postseason tournament, we see that only considering offensive play would be a mistake: the model that best predicted making it to the Sweet Sixteen included several more defensive variables than offensive, indicating how important defensive play is in postseason success. Thus, while offensive play better explains wins, defensive play better explains qualifying for the Sweet Sixteen.