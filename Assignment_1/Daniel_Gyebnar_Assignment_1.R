################################################################
# Setting the scene
################################################################

rm(list=ls())

library(kableExtra)
library(tidyverse)
library(dplyr)
library(huxtable)
library(estimatr)
library(stargazer)
library(modelsummary)
library(grid)

## Accessing the data
data_all <- read_csv("https://raw.githubusercontent.com/DaniDataScience/Data_Analysis_2_repo/main/Assignment_1/morg-2014-emp.csv")

# Selecting observations
data <- data_all %>% filter(occ2012==0110)                # selecting Computer and Information System management
data <- data %>% filter(uhours>=40)                       # full-time employees
data <- data %>% filter(earnwke>10)                       # possible error

data %>% dplyr::select(earnwke,uhours) %>% summary()

# Generating variables
data <- data %>% mutate(female=as.numeric(sex==2)) %>%     # creation female variable
  mutate(w=earnwke/uhours) %>%                             # calculating hourly wage
  mutate(lnw=log(w))                                       # adding ln wage
data$female <- as.factor(data$female)

# Checking distribution
data %>% dplyr::select(earnwke,uhours,w) %>% summary()

# Creating bins for educational level
data <- data %>% mutate(eduation_bin = 1 +
                          1*as.numeric(data$grade92 >= 43 ) +
                          1*as.numeric(data$grade92 >= 44) +
                          1*as.numeric(data$grade92 >= 45) +
                          1*as.numeric(data$grade92 >= 46)) 

data <- data %>%  mutate(eduation_bin = recode(eduation_bin, 
                                               '1' = "No degree", 
                                               "2" = "Bachelors",
                                               "3" = "Masters",
                                               "4" = "Above masters",         #rationale: there are only a few observations for 4 and 5
                                               "5" = "Above masters"))

data$eduation_bin <- factor(data$eduation_bin, levels = c("No degree", "Bachelors", "Masters", "Above masters"));

# Creating dummy variables for education analysis
data <- data %>% mutate( No_degree = as.numeric( eduation_bin == "No degree" ),
                         Bachelors = as.numeric( eduation_bin == "Bachelors" ),
                         Masters = as.numeric( eduation_bin == "Masters" ),
                         Above_masters = as.numeric( eduation_bin == "Above masters" ))



################################################################
# Preliminary analysis - Distributions of key variables
################################################################



# Distribution of hourly earning is mostly symmetric. 
ggplot(data) +
  geom_histogram(aes(x=data$w, y = ..density..), binwidth = 8, fill="navyblue") +
  scale_x_continuous( limits = c( 0 , 80 ) ) +
  labs( x = "Hourly earnings in USD" , y = 'Relative Frequency', fill = "female" ) +
  geom_segment( aes(x = mean( data$w , na.rm = T ), y = 0, xend = mean( data$w , na.rm = T ), yend = 0.026) , color = 'red', size = 0.5 ) + 
  annotate( "text" , size=3, x = mean( data$w , na.rm = T )+5 , y = 0.03 , label = 'Mean' , color = 'red') +
  annotate( "text" , size=3, x = mean( data$w , na.rm = T )+5 , y = 0.028 , label = round(mean( data$w , na.rm = T ),2) , color = 'red') +
  geom_segment( aes(x = median( data$w , na.rm = T ), y = 0, xend = median( data$w , na.rm = T ), yend = 0.026) , color = 'orange', size = 0.5 ) + 
  annotate( "text" , size=3, x = median( data$w , na.rm = T )-5 , y = 0.03 , label = 'Median' , color = 'orange') +
  annotate( "text" , size=3, x = median( data$w , na.rm = T )-5 , y = 0.028 , label = round(median( data$w , na.rm = T ),2) , color = 'orange') +  theme_bw()

# Distribution of sex
table(data$occ2012,data$female)

# Distribution of education: majority has bachelor degrees
# it would make sense to have 4 categories for education: no degree, bachelors, masters, above masters
ggplot(data, aes(x=data$grade92))+
  geom_bar(fill="navyblue")  +
  theme_bw() 

## Distribution of wage per sex 
ggplot(data = data , aes( x = w , fill = female ) ) +
  geom_density(  aes( y = ..density.. ), alpha=0.3) +
  labs( x = "Hourly earnings in USD" , y = 'Relative Frequency', fill = "female" ) + 
  scale_x_continuous(limits = c(-10,100)) +
  theme_bw()

#  Statistics of wage per sex and eduation
datasummary(w*eduation_bin*female~ 
              Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing,
            data=data)

datasummary(w*eduation_bin~ 
              Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing,
            data=data)


#############################################
## Linear & multilinear regressions
#############################################

reg1 <- lm_robust(w ~ female, data = data, se_type = "HC1")
summary(reg1)

reg2 <- lm_robust(w ~  Bachelors + Masters + Above_masters, data = data, se_type = "HC1")
summary(reg2)

reg3 <- lm_robust(w ~ female + Bachelors + Masters + Above_masters, data = data, se_type = "HC1")
summary(reg3)

reg4 <- lm_robust(w ~ Bachelors*female + Masters*female + Above_masters*female, data = data, se_type = "HC1")
summary(reg4)

huxreg(reg1, reg2, reg3, reg4, statistics = c(N = "nobs", R2 = "r.squared"))

