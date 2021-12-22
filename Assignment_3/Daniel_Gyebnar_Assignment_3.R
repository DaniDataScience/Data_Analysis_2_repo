################################################################
# Setting the scene
################################################################

rm(list=ls())

library(tidyverse)
library(dplyr)
library(huxtable)
library(modelsummary)
library(grid)
library(fixest)
library(data.table)
library(corrplot)
library(lspline)
library(mfx)
library(estimatr)
install.packages("gcookbook")
library(gcookbook)
library(Knitr)
library(kableExtra)

##########
# 1) Research question:
#     - My aim is to understand the pattern (possible connection) between,
#               the unit price of a real estate (price per square foot) and its size!
#     We know as a rule of thumb that larger real estate (house/flat) have usually a lower unit price,
#     and now I want to deeper analyze this ruel of tumb, to see what the pattern is 
#     between real estate size and and unit price?
#     - I aim to reach high external validity, to be able to use the model for other locations as well
#
#     - Question: What is the pattern between real estate unit price and living space size?

## Accessing the data
df_raw <- read_csv("https://raw.githubusercontent.com/DaniDataScience/Data_Analysis_2_repo/main/Assignment_3/kc_house_data.csv")

## Data description

variables <- c("id","date","price","bedrooms","bathrooms","sqft_liv","sqft_lot",
               "floors","waterfront","view","condition","grade","sqft_above",
               "sqft_basmt","yr_built","yr_renov","zipcode","lat","long",
               "squft_liv15","squft_lot15","Shape_leng","Shape_Area")

describtion <- c( "Identification","Date sold","Sale price [USD]",
                 "Number of bedrooms","Number of bathrooms","Size of living area in square feet",
                 "Size of the lot in square feet","Number of floors",
                 "‘1’ if the property has a waterfront, ‘0’ if not.",
                 "An index from 0 to 4 of how good the view of the property was",
                 "Condition of the house, ranked from 1 to 5",
                 "Classification by construction quality which refers to the types of materials used and the quality of workmanship.",
                 "Square feet above ground","Square feet below ground","Year built",
                 "Year renovated. ‘0’ if never renovated","5 digit zip code",
                 "Latitude","Longitude",
                 "Average size of interior housing living space for the closest 15 houses, in square feet",
                 "Average size of land lots for the closest 15 houses, in square feet",
                 "Polygon length in meters","Polygon area in meters")

data_descr <- data.frame("Variable" = variables, "Description"=describtion)


# Data contains prices and attributes of real estates sold in King County, from 2014 march to 2015 march
# Variable description
# date:	Date sold
# price:	Sale price
# bedrooms:	Number of bedrooms
# bathrooms:	Number of bathrooms
# sqft_liv:	Size of living area in square feet
# sqft_lot:	Size of the lot in square feet
# floors:	Number of floors
# waterfront: 1 if the property has a waterfront, ?0? if not.
# view:	An index from 0 to 4 of how good the view of the property was
# condition:	Condition of the house, ranked from 1 to 5
# grade:	Classification by construction quality which refers to the types of materials used and the quality of workmanship. Buildings of better quality (higher grade) cost more to build per unit of measure and command higher value. Additional information in: KingCounty
# sqft_above:	Square feet above ground
# sqft_basmt:	Square feet below ground
# yr_built:	Year built
# yr_renov:	Year renovated. ?0? if never renovated
# zipcode:	5 digit zip code
# lat:	Latitude
# long:	Longitude
# squft_liv15:	Average size of interior housing living space for the closest 15 houses, in square feet
# squft_lot15:	Average size of land lots for the closest 15 houses, in square feet
# Shape_leng:	Polygon length in meters
# Shape_Area:  Polygon area in meters

#####
# Model setup
# Left-hand variable:      pps: price per square feet.
df_raw$pps <- df_raw$price/df_raw$sqft_living

# Right-hand variable:     sqft_living: living space in square feet
#
# Potential confounders to include:
#   metrics of lot: lot_sqft
#   metrics of layout parameters:- number of bedrooms and bathrooms
#   metrics of quality: - condition, grade, year built, year renovated
#   metrics of surroundings: - view, waterfront 
#
# Neglecting some confounders
# I neglect the following attributes as they would decrease external validity for other cities:
#  - lat, long, zip code, squft_liv15, squft_lot15
# I neglect the following attributes as they are complicated to take into account and interpret 
# - shape leng, shape area
# I neglect additional attributes:
# - sqft_basmt, sqft_above, floors

####
# 2) What are the possible problems with the data
# Real estates can be one-of-a-kind goods where price is more determined by the demand than the supply
# Data quality for size, year built and ect. can be easily wrongly recorded
# We do not know if these are all the transaction data from King County in the given time frame, 
# and if this is just a segment, then is it a representative segment?
# Subjectivity in data, e.g. grade, view, ect. , which can be preferences rather than universal truth

####
# 3) Descriptives
#
#

# Filtering out duplicates (same house sold in 2014 and in 2015 as well at different price)
dfx <- df_raw %>% group_by(id) %>% filter(n()>1) %>% arrange(id, date)  #selecting duplicates
dfx <- dfx[ c(seq(1,nrow(dfx),2)),]   #selecting the older record
df <- anti_join(df_raw, dfx)  #deselecting older record from data table
rm(dfx)

# looking at data
datasummary_skim( df_raw )

df_raw %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Check the main parameter of interests and potential confounders:
# right-hand variable
ggplot( df_raw , aes(x = pps)) +
  geom_histogram( binwidth = 30, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Price per square foot [USD]") +
  theme_bw()

# X
ggplot( df_raw , aes(x = sqft_living)) +
  geom_histogram( bins=50, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Living space [square fott]") +
  theme_bw()

# potential confounders
ggplot( df_raw , aes(x = price)) +
  geom_histogram( bins=50, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Price [USD]") +
  theme_bw()

ggplot( df_raw , aes(x = sqft_lot)) +
  geom_histogram( binwidth=20000, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Lot size [square fott]") +
  theme_bw()

ggplot( df_raw , aes(x = yr_built)) +
  geom_histogram( binwidth=1, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Year built") +
  theme_bw()

ggplot( df_raw , aes(x = yr_renovated)) +
  geom_histogram( binwidth=100, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Year renovated") +
  theme_bw()

table(df_raw$bedrooms)

table(df_raw)
table(round(df_raw$bathrooms))

table(df_raw$grade)

table(df$view)

table(df_raw$waterfront)

table(df_raw$condition)

# Restricting observations

# To achieve high external validity, I need to filter out one-of-a-kind observations
# I am also interested in the price of the house/flat and not he lot size,
# so I will filter out observations where the lot is too large or dominant

df <- df_raw %>% 
  filter(bedrooms <=10 & bedrooms !=0) %>% # restricting large mansions
  filter(sqft_living/sqft_lot > 0.01) %>% # filtering extreme large lots
  filter(bathrooms > 0.5)
rm(df_raw)

datasummary_skim( df )

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Transforming observations

# continuous variables (pps, sqft_living, sqft_lot, price) are right skewed, so log is suggested
# logs for Xand Y would allow to interpret result in differences,allowing higher external validity
df$ln_living <- log(df$sqft_living)
df$ln_lot <- log(df$sqft_lot)
df$ln_pps <- log(df$pps)
df$ln_price <- log(df$price)

# use age of the building instead of ear built
df$age <- 2015-df$yr_built  

# create bins for year of renovations, (now 0 is included as no renovation)
df <- df %>% mutate(renovation_bin = 0 +
                      1*as.numeric(df$yr_renovated >= 1980) +
                      1*as.numeric(df$yr_renovated >= 2000)) 

df <- df %>%  mutate(renovation_bin = dplyr::recode(renovation_bin, 
                                             "0" = "Not or before 1980",
                                             "1" = "1980-2000", 
                                             "2" = "Since 2000"))

table(df$renovation_bin)
df$renovation_bin <- factor(df$renovation_bin, level=c("Not or before 1980","1980-2000","Since 2000"))

# creating larger bins for categorical variables with too few observations

# number of bathrooms -> to few observations above 4
df$bathrooms <- round(df$bathrooms)
table(df$bathrooms)
df <- df %>% mutate(bathrooms2 =
                      df$bathrooms*as.numeric(df$bathrooms <= 4) +
                      5*as.numeric(df$bathrooms >= 5)) 
table(df$bathrooms2)

# number of bedrooms -> to few observations above 6
table(df$bedrooms)
df <- df %>% mutate(bedrooms2 =
                      df$bedrooms*as.numeric(df$bedrooms <= 5) +
                      6*as.numeric(df$bedrooms >= 6)) 
table(df$bedrooms2)

# grade -> to few observations below 5 and above 11
table(df$grade)
df <- df %>% mutate(grade2 =
                      1*as.numeric(df$grade <= 5) +
                      (df$grade-4)*as.numeric(df$grade <= 10 & df$grade >= 6) +
                      11*as.numeric(df$grade >= 11)) 
df <- df %>%  mutate(grade2 = dplyr::recode(grade2,
                                        "1" = 1, 
                                        "2" = 2, 
                                        "3" = 3, 
                                        "4" = 4, 
                                        "5" = 5,
                                        "6" = 6,
                                        "11" = 7))
table(df$grade2)

# view -> at least 300 observations in each bin, bin 1-2-3 has similar mean
table(df$view)
df <- df %>% mutate(view2 =
                      (df$view-1)*as.numeric(df$view == 2) +
                      (df$view-2)*as.numeric(df$view == 3) +
                      2*as.numeric(df$view >= 4)) 
table(df$view2)

# waterfront -> cannot simplify
table(df$waterfront)

# condition -> can merge 1&2
table(df$condition)
df <- df %>% mutate(condition2 =
                      1*as.numeric(df$condition <= 2) +
                      (df$condition-1)*as.numeric(df$condition >= 3)
                    ) 
table(df$condition2)

# selecting potential confounders
df <- df %>% dplyr::select( id, date,
                            ln_pps, ln_living,
                            ln_lot, bedrooms2, bathrooms2,
                            condition2, grade2, age, renovation_bin,
                            waterfront, view2
) %>% drop_na()

datasummary_skim(df) 

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# distribution of ln_pps (Y)
ggplot(df) +
  geom_histogram(aes(x=df$ln_pps, y = ..density..),bins=50, fill="navyblue", color="white") +
  scale_x_continuous( limits = c( 4 , 7 ) ) +
  labs( x = "Hourly earnings in USD" , y = 'Relative Frequency', fill = "female" ) +
  geom_segment( aes(x = mean( df$ln_pps , na.rm = T ), y = 0, xend = mean( df$ln_pps , na.rm = T ), yend = 1.1) , color = 'red', size = 1.5 ) + 
  annotate( "text" , size=3, x = mean( df$ln_pps , na.rm = T )+0.3 , y = 1.1 , label = 'Mean' , color = 'red') +
  annotate( "text" , size=3, x = mean( df$ln_pps , na.rm = T )+0.3 , y = 1.05 , label = round(mean( df$ln_pps , na.rm = T ),3) , color = 'red') +
  geom_segment( aes(x = median( df$ln_pps , na.rm = T ), y = 0, xend = median( df$ln_pps , na.rm = T ), yend = 1.1) , color = 'orange', size = 0.5 ) + 
  annotate( "text" , size=3, x = median( df$ln_pps , na.rm = T )-0.3 , y = 1.1 , label = 'Median' , color = 'orange') +
  annotate( "text" , size=3, x = median( df$ln_pps , na.rm = T )-0.3 , y = 1.05 , label = round(median( df$ln_pps , na.rm = T ),3) , color = 'orange') +  theme_bw()

# distribution of ln_living (X)
ggplot(df) +
  geom_histogram(aes(x=df$ln_living, y = ..density..),bins=50, fill="navyblue", color="white") +
  scale_x_continuous( limits = c( 6 , 9 ) ) +
  labs( x = "Hourly earnings in USD" , y = 'Relative Frequency', fill = "female" ) +
  geom_segment( aes(x = mean( df$ln_living , na.rm = T ), y = 0, xend = mean( df$ln_living , na.rm = T ), yend = 1.1) , color = 'red', size = 1.5 ) + 
  annotate( "text" , size=3, x = mean( df$ln_living , na.rm = T )+0.3 , y = 1.1 , label = 'Mean' , color = 'red') +
  annotate( "text" , size=3, x = mean( df$ln_living , na.rm = T )+0.3 , y = 1.05 , label = round(mean( df$ln_living , na.rm = T ),3) , color = 'red') +
  geom_segment( aes(x = median( df$ln_living , na.rm = T ), y = 0, xend = median( df$ln_living , na.rm = T ), yend = 1.1) , color = 'orange', size = 0.5 ) + 
  annotate( "text" , size=3, x = median( df$ln_living , na.rm = T )-0.3 , y = 1.1 , label = 'Median' , color = 'orange') +
  annotate( "text" , size=3, x = median( df$ln_living , na.rm = T )-0.3 , y = 1.05 , label = round(median( df$ln_living , na.rm = T ),3) , color = 'orange') +  theme_bw()


# distribution pairs
df %>%  keep(is.numeric) %>% select(ln_pps, ln_living, ln_lot, grade) %>% pairs(df)

####
# 4) Check the correlations
#
# 
numeric_df <- keep( df , is.numeric )
cT <- round( cor( numeric_df , use = "complete.obs") , 2 )
# create a lower triangular matrix
cT[ upper.tri( cT ) ] <- NA
# Put it into a tibble format
melted_format <- melt( cT , na.rm = TRUE)
# Now we can create a heat-map
ggplot( data = melted_format, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_bw()+ 
  theme( axis.text.x = element_text(angle = 45, vjust = 1, 
                                    size = 10, hjust = 1))+
  labs(y="",x="")+
  coord_fixed()

rm( cT , numeric_df, melted_format, numberif_df)


####
# 5) Checking scatter-plots to decide the functional form
#
#
chck_sp <- function( x_var , x_lab ){
  ggplot( df , aes(x = x_var, y = ln_pps)) +
    geom_point(color='red',size=0.1,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x ) +
    labs(x = x_lab, y = "Ln(Price per squarefoot [USD])") +
    theme_bw()
}

# Our main interest: houe size  
# ln_living -> inlude as lspline with a knot at 7.7
p1 <- chck_sp(df$ln_living,'Ln(Living area [sqft])')
library(segmented)
reglm <- lm( ln_pps ~ ln_living , data = df )
fit_seg <- segmented( reglm , seg.Z = ~ln_living, psi = list( ln_living=7 ) )
summary(fit_seg)
rm(reglm, fit_seg)

# ln_lot -> # inlude as lspline with a knot at 9.0
p2 <-chck_sp(df$ln_lot,'Ln(Lot area [sqft])')
reglm <- lm( ln_pps ~ ln_lot , data = df )
fit_seg <- segmented( reglm , seg.Z = ~ln_lot, psi = list( ln_lot=8 ) )
summary(fit_seg)
rm(reglm, fit_seg)

# figure <- ggarrange(p1, p2,
                    ncol = 1, nrow = 3)
# figure
rm(p1,p2,p3)

# grade -> include as dummies
gr <- df %>% dplyr::select(grade2, ln_pps) %>% group_by(grade2) %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=gr, aes(y=ln_pps, x=as.factor(grade2))) +
  geom_point( size=gr$weight/1000, color="red") + 
  theme_bw()
rm(gr)



# bedrooms --> include as dummies
bd <- df %>% dplyr::select(bedrooms2, ln_pps) %>%  group_by(bedrooms2)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=bd, aes(y=ln_pps, x=as.factor(bedrooms2))) +
  geom_point(size=bd$weight/1000, color="red") + 
  theme_bw()
rm(bd)


# bathrooms --> include as dummies
bt <- df %>% dplyr::select(bathrooms2, ln_pps) %>%  group_by(bathrooms2)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=bt, aes(y=ln_pps, x=as.factor(bathrooms2))) +
  geom_point(size=bt$weight/1000, color="red") + 
  theme_bw()
rm(bt)

# waterfront -> include as dummies
wf <- df %>% dplyr::select(waterfront, ln_pps) %>%  group_by(waterfront)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
wf
ggplot(data=wf, aes(y=ln_pps, x=as.factor(waterfront))) +
  geom_point(size=wf$weight/1000, color="red") + 
  labs(x = "Does real estate have direct waterfront access", y = "Ln(Price per squarefoot [USD])") +
  theme_bw()
rm(wf)

# view -> include as dummies
view <- df %>% dplyr::select(view2, ln_pps) %>%  group_by(view2)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=view, aes(y=ln_pps, x=as.factor(view2))) +
  geom_point(size=view$weight/1000, color="red") + 
  theme_bw()
rm(view)


# condition -> include as dummies
con <- df %>% dplyr::select(condition2, ln_pps) %>%  group_by(condition2)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=con, aes(y=con$ln_pps, x=as.factor(condition2))) +
  geom_point(size=con$weight/1000, color="red") + 
  theme_bw()
rm(con)

# age --> use quadratic form, but that would make interpretation difficult, so I'll use lspline with knot at 37
age <- df %>% dplyr::select(age, ln_pps) %>%  group_by(age)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=age, aes(y=ln_pps, x=age)) +
  geom_point(size=age$weight/100, color="red") +
  geom_smooth(method="loess") +
  theme_bw()
reglm <- lm( ln_pps ~ age , data = df )
fit_seg <- segmented( reglm , seg.Z = ~age, psi = list( age=20 ) )
summary(fit_seg)
rm(reglm, fit_seg)
rm(age)

# yr_renovated  -> create bins and include as dummies
yrr <- df %>% dplyr::select(renovation_bin, ln_pps) %>%   group_by(renovation_bin)  %>% summarise("ln_pps" = mean(ln_pps), weight=n())
ggplot(data=yrr, aes(y=ln_pps, x=factor(renovation_bin, levels=c("Not or before 1980", "1980-2000","Since 2000")))) +
  geom_point(size=1, color="red") + 
  theme_bw()
rm(yrr)

rm(reg_numberic,reg_factor)

# Possible  interactions:
#   living space and lot size interaction could be interesting to look at

  
#####
# 6) Modelling
#
#
# Main regression: score4 = b0 + b1*stratio
#   reg1: NO controls, simple linear
#   reg2: NO controls, use linear spline(L.S)  7.6
#   
# Use reg2 and control for lot size:
#   reg3: grade as control
#   reg4: lot size as control 
#   reg5: age as control
#   reg6: view as control
#   reg_all: control for all variables




#reg1
reg1 <- lm_robust(ln_pps ~ ln_living, data=df, se_type = "HC1")

df <- df %>% mutate(reg1_ln_pps=predict(reg1,df))
ggplot(df,aes(x=ln_living, y = ln_pps)) +
  geom_point( size=0.1) +
  geom_line(aes(y = reg1_ln_pps, color = "blue"))+
  labs(x = "Living area [sqf]",y = "Ln(Price per sqf [USD]")+
  theme_bw() 

#reg2
reg2 <- lm_robust(ln_pps ~ lspline(ln_living, c(7.7)), data=df, se_type = "HC1")

df <- df %>% mutate(reg2_ln_pps=predict(reg2,df))
ggplot(df,aes(x=ln_living, y = ln_pps)) +
  geom_point( size=0.1) +
  geom_line(aes(y = reg2_ln_pps, color = "blue"))+
  labs(x = "Living area [sqf]",y = "Ln(Price per sqf [USD]")+
  theme_bw() 

huxreg(reg1, reg2)

# control for grade and condition:
reg3.1 <- lm_robust(ln_pps ~  ln_living + as.factor(grade2) + as.factor(condition2) , data=df, se_type = "HC1")
reg3.2 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) , data=df, se_type = "HC1")
reg3.3 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(grade2)*lspline(ln_living, c(7.7)) + as.factor(condition2)  , data=df, se_type = "HC1")
huxreg(reg3.1, reg3.2, reg3.3)
# interaction between grade and ln_living dies not increase R2, but makes interpretation harder
# I would go for the simpler model of reg3.2
reg3 <- reg3.2
rm(reg3.1, reg3.2, reg3.3)
huxreg(reg1, reg2, reg3)

# control for lot size:
reg4.1 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + ln_lot , data=df, se_type = "HC1")
reg4.2 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) , data=df, se_type = "HC1")
reg4.3 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + lspline(ln_lot, c(9))*lspline(ln_living, c(7.7)), data=df, se_type = "HC1")
huxreg(reg4.1, reg4.2, reg4.3)
reg4 <- reg4.2
rm(reg4.1, reg4.2, reg4.3)
# additional complexity barely improves R2 and significance

huxreg(reg1, reg2, reg3, reg4)

# control for age:
reg5.1 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + age , data=df, se_type = "HC1")
reg5.2 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + lspline(age, c(25)) , data=df, se_type = "HC1")
huxreg(reg5.1, reg5.2)
# age is better described by 5.2
reg5 <- reg5.2
rm(reg5.1, reg5.2)
huxreg(reg1, reg2, reg3, reg4, reg5)

# control for view and waterfront:
reg6 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + lspline(age, c(25)) + as.factor(view2) + as.factor(waterfront) , data=df, se_type = "HC1")
huxreg(reg1, reg2, reg3, reg4, reg5, reg6, reg6)

# control for number of bathrooms
reg7 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + lspline(age, c(25)) + as.factor(view2) + as.factor(waterfront) + as.factor(bathrooms2)  , data=df, se_type = "HC1")

# adding all variables
reg_all <- lm_robust(ln_pps ~  
                       lspline(ln_living, c(7.7)) +
                       as.factor(grade2) + as.factor(condition2) + 
                       lspline(ln_lot, c(9)) + 
                       lspline(age, c(25)) + 
                       as.factor(view2) + as.factor(waterfront) +
                       as.factor(bathrooms2) +
                       as.factor(bedrooms2) +
                       as.factor(renovation_bin)
                     , data=df, se_type = "HC1")

huxreg(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg_all)


# creating nice output chart
msummary(models, title = 'Model summary')



msummary(list("reg2"=reg2, "reg3"=reg3, "reg4"=reg4, "reg5"=reg5, "reg6"=reg6, "reg7"=reg7, "reg_all"=reg_all),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename=c("(Intercept)" = "Constant",
                       "lspline(ln_living, c(7.7))1" = "ln_living<7.7",
                       "lspline(ln_living, c(7.7))2" = "ln_living>7.7",
                       "as.factor(grade2)2" = "f(grade=2)",
                       "as.factor(grade2)3" = "f(grade=3)",
                       "as.factor(grade2)4" = "f(grade=4)",
                       "as.factor(grade2)5" = "f(grade=5)",
                       "as.factor(grade2)6" = "f(grade=6)",
                       "as.factor(grade2)7" = "f(grade=7)",
                       "lspline(ln_lot, c(9))1" = "ln_lot<9",
                       "lspline(ln_lot, c(9))2" = "ln_lot>9",
                       "lspline(age, c(25))1" = "age<25",
                       "lspline(age, c(25))2" = "age>25",
                       "as.factor(view2)1" = "f(view=1)",
                       "as.factor(view2)2" = "f(view=2)",
                       "as.factor(bathrooms2)2" = "f(bathrooms=2)",
                       "as.factor(bathrooms2)3" = "f(bathrooms=3)",
                       "as.factor(bathrooms2)4" = "f(bathrooms=4)",
                       "as.factor(bathrooms2)5" = "f(bathrooms=5+)",
                       "as.factor(bedrooms2)2" = "f(bedrooms=2)",
                       "as.factor(bedrooms2)3" = "f(bedrooms=3)",
                       "as.factor(bedrooms2)4" = "f(bedrooms=4)",
                       "as.factor(bedrooms2)5" = "f(bedrooms=5)",
                       "as.factor(bedrooms2)6" = "f(bedrooms=6+)",
                       "as.factor(condition2)2" = "f(condition=2)",
                       "as.factor(condition2)3" = "f(condition=3)",
                       "as.factor(condition2)4" = "f(condition=4)",
                       "as.factor(waterfront)1" = "waterfront",
                       "as.factor(renovation_bin)1980-2000" = "Renovated: 1980-2000",
                       "as.factor(renovation_bin)Since 2000" = "Renovated since 2000")
         ) %>%
  column_spec(1:4, width = "7cm") %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"))

summary

# Checking external validity in time

# Lets see if we get similar results if we divide the dataset in time

df$date <- as.Date(df$date, "%Y-%m-%d")
typeof(df$date)
library(tidyr)

#four month from 2014-05 to 2014-07
df1 <- df %>% arrange(date) %>% head(5000)
#four month from 2015-02 to 2015-05
df2 <- df %>% arrange(date) %>% tail(5000)

reg7_1 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + lspline(age, c(25)) + as.factor(view2) + as.factor(waterfront) + as.factor(bathrooms2)  , data=df1, se_type = "HC1")

reg7_2 <- lm_robust(ln_pps ~  lspline(ln_living, c(7.7)) + as.factor(grade2) + as.factor(condition2) + lspline(ln_lot, c(9)) + lspline(age, c(25)) + as.factor(view2) + as.factor(waterfront) + as.factor(bathrooms2)  , data=df2, se_type = "HC1")

huxreg(reg7_1, reg7_2)
