## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Import Tidyverse.
library(tidyverse)

# Set working directory
setwd("~/OneDrive/LSE career accelerator/Assignments/Course 3")

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=T)

# Print the data frame.
View(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_clean <- select(sales, -Ranking,-Year,-Genre,-Publisher)

# View the data frame
sales_clean

# View the descriptive statistics.
summary(sales_clean)

# Return frequency table for Platforms
table(sales_clean$Platform)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

qplot(EU_Sales, NA_Sales, data=sales_clean)
qplot(Global_Sales, Platform, data=sales_clean)

## 2b) Histograms
# Create histograms.

qplot(EU_Sales, data=sales_clean)
qplot(NA_Sales, data=sales_clean)

## 2c) Boxplots
# Create boxplots.

qplot(Global_Sales, Platform, data=sales_clean, geom='boxplot')


###############################################################################
###############################################################################

# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_clean)

# Check output: Determine the min, max, and mean values.
# Calculate only for numerical variables for which it makes sense
select(sales_clean, -c(Platform, Product)) %>% apply(2,min)
select(sales_clean, -c(Platform, Product)) %>% apply(2,max)
select(sales_clean, -c(Platform, Product)) %>% apply(2,mean)

# View the descriptive statistics for numerical variables

select(sales_clean, -c(Platform, Product)) %>% summary()

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Platform.

sales_platform <- sales_clean %>% group_by(Platform) %>% summarise (sum_NA_Sales=sum(NA_Sales),
                             sum_EU_Sales=sum(EU_Sales),
                             sum_Global=sum(Global_Sales))

# View the data frame.
sales_platform
View(sales_platform)

# Explore the data frame.

qplot(x=Platform, y=sum_Global, data=sales_platform)
summary(sales_platform)

## 2b) Determine which plot is the best to compare game sales.
## Create scatterplots.

# EU Sales & Global Sales
ggplot(data = sales_clean,
       mapping = aes(x = Global_Sales, y = EU_Sales)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth()+
  labs(title = "Relationship between Global and EU sales",
       x = "Global Sales in £mil",
       y = "EU Sales in £mil")

# North American Sales & Global Sales
ggplot(data = sales_clean,
       mapping = aes(x = Global_Sales, y = NA_Sales)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth()+
  labs(title = "Relationship between Global and North American sales",
       x = "Global Sales in £mil",
       y = "North American Sales in £mil")

# North American Sales & European Sales
ggplot(data = sales_clean,
       mapping = aes(x = EU_Sales, y = NA_Sales)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth()+
  labs(title = "Relationship between Global and EU sales",
       x = "European Sales in £mil",
       y = "North American Sales in £mil")

# Create histograms.

ggplot(data=sales_clean,
       mapping=aes(x=Global_Sales))+
  geom_histogram(bins=25)+
  scale_x_continuous(breaks = seq(0, 70, 10))+
  labs(title="Global sales distribution per game",
       x = "Global Sales in £mil",
       y = "Count")

# Create boxplots.

ggplot(sales_clean, aes(y = Platform, x = Global_Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'red', outlier.color = 'red') +
  # Specify the titles.
  labs(title = "Global Sales by platform") +  
  # Add a 'minimal' theme.
  theme_minimal()  

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_clean$Global_Sales)
qqline(sales_clean$Global_Sales)

qqnorm(sales_clean$NA_Sales)
qqline(sales_clean$NA_Sales)

qqnorm(sales_clean$EU_Sales)
qqline(sales_clean$EU_Sales)

# Does not look like a normal distribution, skewewed to the right with heavy right tails

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

library(moments)

# Perform Shapiro-Wilk test - for each of the Sales data

shapiro.test(sales_clean$Global_Sales)
shapiro.test(sales_clean$NA_Sales)
shapiro.test(sales_clean$EU_Sales)

# P values super small - below 1% so we reject the null hypothesis that the data is normally 
# distributed and we can assume non-normality of the data

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sales_clean$Global_Sales) 
kurtosis(sales_clean$Global_Sales)

skewness(sales_clean$NA_Sales) 
kurtosis(sales_clean$NA_Sales)

skewness(sales_clean$EU_Sales) 
kurtosis(sales_clean$EU_Sales)

# Sales data highly skewed to the right with very high kurtosis indicating higher peak 
# and fatter and heavier tails than normal distribution (which has curtosis of 3)

## 3d) Determine correlation
# Determine correlation.

cor(sales_clean$Global_Sales, sales_clean$EU_Sales)
cor(sales_clean$Global_Sales, sales_clean$NA_Sales)
cor(sales_clean$EU_Sales, sales_clean$NA_Sales)

# Caution - data not normally distributed so Pearson correlation can be less reliable
# However from scatterplot we can see that Sales in different regions are indeed highly
# correlated. In subsequent code, Spearman correlation is also used, which confirms
# high correlation

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Number of games released per yaer

ggplot(sales, aes(x = Year)) +
  geom_bar()+
  scale_x_continuous(breaks = seq(1980, 2016, 5), "Year of release")+
  theme_minimal()+
  labs(title="Number of games released per year")

# Global sales of games per release year

ggplot(sales, aes(x = Year, y= Global_Sales)) +
  geom_bar(stat='sum')+
  scale_x_continuous(breaks = seq(1980, 2016, 5), "Year of release")+
  theme_minimal()+
  labs(title="Global sale of games per release year")+
  theme(legend.position="none")

# Different genres released over time - count

ggplot(sales, aes(y = Genre)) +
  geom_bar()+
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 70, 10), "Number of games")+
  labs(title="Most popular genres by title released")

# Different genres released over time - sales values

ggplot(sales, aes(y = Genre, x= Global_Sales)) +
  geom_bar(stat='sum')+
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 300, 50), 
                     "Global sales value in £mil")+
  labs(title="Most popular genres by sales")+
  theme(legend.position="none")


# Most popular platforms since 2010 among platforms releasing at least 3 games

# Create new dataframe grouping by Platform and Year and counting
sales_tally <- sales %>% group_by(Platform, Year) %>% tally(sort=TRUE)

# Pass the new dataframe and create graph for year above 2010 and n>3
sales_tally[sales_tally$Year>=2010 & sales_tally$n>3,] %>%
  ggplot(aes(x = Year, y=n, fill= Platform)) +
  geom_col()+
  scale_x_continuous(breaks = seq(2010, 2016, 1), "Year of release")+
  theme_minimal()+
  labs(title="Number of games released per year after 2010",
       subtitle="Main platforms only",
       caption="Includes Platforms which released >3 products")

# Number of distributors releasing on X360 & XOne

# Filter dataframe only on platform X360 & XOne
sales[sales$Platform %in% c("XOne","X360"),] %>%
  # Pass it to the graph
  ggplot(aes(x = Year, fill= Publisher)) +
  geom_bar()+
  # Define the axis breaks
  scale_x_continuous(breaks = seq(2005, 2015, 1), "Year of release")+
  scale_y_continuous(breaks = seq(0, 10, 1), "Number of games")+
  labs(title='Number of games released by each publisher on X360 & XOne')

# Number of distributors releasing on PS3 & PS4

# Filter dataframe only on platform PS3 & PS4
sales[sales$Platform %in% c("PS3","PS4"),] %>%
  # Pass it to the graph
  ggplot(aes(x = Year, fill= Publisher)) +
  geom_bar()+
  # Define the axis breaks
  scale_x_continuous(breaks = seq(2005, 2015, 1), "Year of release")+
  scale_y_continuous(breaks = seq(0, 12, 1), "Number of games")+
  labs(title='Number of games released by each publisher on PS3 & PS4')+
  theme(text = element_text(size = 9.5))

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View main data frame created in Week 4.
View(sales)

# Create dataframe with which count the products by year (used later)
sales_year <- sales %>% group_by(Year) %>% summarise (sum_NA_Sales=sum(NA_Sales),
                                                      sum_EU_Sales=sum(EU_Sales),
                                                      sum_Global=sum(Global_Sales))
# View the dataframe
View(sales_year)

# plot the dataframe (total sales)
plot(sales_year$Year, sales_year$sum_Global)

###############################################################################

# 2. Create a simple linear regression model

# 2a Create a linear regression model on the Global and European sales

model1 <- lm(Global_Sales~EU_Sales,
             data=sales)

# View model1
model1

# View more outputs for the model - the full regression table.
summary(model1)

# Create a plot (simple linear regression)
# Basic visualisation.

plot(sales$EU_Sales, sales$Global_Sales)
abline(coefficients(model1))

# 2b Create a linear regression model on the Global and North American sales

model2 <- lm(Global_Sales~NA_Sales,
             data=sales)

# View model2
model2

# View more outputs for the model - the full regression table.
summary(model2)

# Create a plot (simple linear regression)
# Basic visualisation.

plot(sales$NA_Sales, sales$Global_Sales)
abline(coefficients(model2))

# 2c Create a linear regression model on the yearly sales trend.

model3 <- lm(sum_Global~Year,
             data=sales_year)

# View model3
model3

# View more outputs for the model - the full regression table.
summary(model3)

# Create a plot (simple linear regression)
# Basic visualization

plot(sales_year$Year, sales_year$sum_Global)
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model - global sales predictions vs EU & NA

# Deterimine the correlation matrix (on numeric columns)
# We use Spearman correlation (rather than default Pearson) as data not normally
# distributed

select(sales,c(NA_Sales,EU_Sales,Global_Sales)) %>%
cor(method = "spearman")

# Multiple linear regression model.

model4 <- lm(Global_Sales~EU_Sales+NA_Sales,
             data=sales)

# View model3
model4

# View more outputs for the model - the full regression table.
summary(model4)

###############################################################################

# 4. Predictions based on given values - 

# 4a - Global Sales 

# Create new dataframe with sample of observations
global_forecast <-select(sales,c(NA_Sales,EU_Sales,Global_Sales))
global_forecast <-global_forecast[global_forecast$NA_Sales %in% 
                                    c(34.02,3.93,2.73,2.26,22.08),]

# View the dataframe
global_forecast

# Predict the Global sales based on multiple linear regression
predict(model4,newdata = global_forecast)

# Add the values to the dataframe
global_forecast$Predicted <- round(predict(model4,newdata = global_forecast),2)

# Inspect the dataframe
global_forecast

# The predictions are close to the real values as expected

# 4b - yearly sales of games
# Create new dataframe
sales_forecast <- data.frame(Year=2017:2020)

# Predict from 2017 to 2020.
predict(model3,
        newdata=sales_forecast)

# Add the values to the cpiForecast data frame.
sales_forecast$Global_sales <- predict(model3,
                                  newdata=sales_forecast)

# View the dataframe
sales_forecast


