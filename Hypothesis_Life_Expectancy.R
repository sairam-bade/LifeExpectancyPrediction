library(nortest)
library(caret)

dataset <- read.csv('Life Expectancy Data.csv')
View(dataset)

## Hypothesis - 1

# Ho : μ2000 - μ2015 = 0 (There is no difference in life expectancy of 2000 and 2015)
# Ha : μ2000 - μ2015 ≤ 0 (Life Expectancy in 2015 has increased over 2000)
# Where μ2000 and μ2015 are the population mean life expectancies of 2000 and 2005 respectively

df_2000 <- subset(dataset, Year == 2000)
df_2015 <- subset(dataset, Year == 2015)

# Perform Welch's two-sample t-test
t_test_result <- t.test(df_2000$Life_Expectancy, df_2015$Life_Expectancy, mu=0, alternative = 'less', conf.level = 0.95)

# Print the results
print(t_test_result)

# p-value is less than the significance level (0.05), we can reject the null hypothesis
# The mean life expectancy of the population in 2015 has increased over 2010.

# Plot Histograms
par(mfrow=c(1,2), mar=c(5,2,1,2)) 
hist(df_2000$Life_Expectancy,  font=1,main = "World Life Expectancy 2000", xlab="2000 life expectancy in years", ylab ="Number of Countries", plot = TRUE)
hist(df_2015$Life_Expectancy, main = "World Life Expectancy 2015", xlab="2015 life expectancy in years", ylab ="Number of Countries", plot = TRUE)
# Add labels to each plot
mtext("Number of Countries", side = 2, line = 3, cex.lab = 1.2)

# Calculating average life expectancy
avg_life_exp_2000 <- mean(df_2000$Life_Expectancy, na.rm = TRUE)
avg_life_exp_2015 <- mean(df_2015$Life_Expectancy, na.rm = TRUE)

# Creating a data frame for bar plot
data <- data.frame(
  Year = c("2000", "2015"),
  Avg_Life_Expectancy = c(avg_life_exp_2000, avg_life_exp_2015)
)

# Creating bar plot with ggplot
ggplot(data, aes(x = Year, y = Avg_Life_Expectancy)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Avg_Life_Expectancy, 2)), 
            vjust = -0.5, size = 4) +
  labs(x = "Year", y = "Average Life Expectancy", 
       title = "Average Life Expectancy in 2000 and 2015") +
  theme_minimal()

## Hypothesis - 2

# Ho : GDP has an impact on Life Expectancy
# Ha : GDP has no impact on Life Expectancy

# Perform normality test on GDP
p_val_GDP <- ad.test(dataset$GDP)$p.value

# Perform normality test on Life_Exp
p_val_Exp <- ad.test(dataset$Life_Expectancy)$p.value

# Create a dataframe to store the results
Norm_Test <- data.frame('p_value' = c(p_val_GDP, p_val_Exp),
                        'Variable' = c('GDP', 'Life Expectancy'))

# Set the significance level
alfa <- 0.05

# Categorize the normality based on the p-value
Norm_Test$Normality <- ifelse(Norm_Test$p_value < alfa, 'Rejected', 'Accepted')

# Print the results
print(Norm_Test)

# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

dataset <- na.omit(dataset)

# Create scatter plot with linear regression line
ggplot(dataset, aes(x = GDP, y = Life_Expectancy)) +
  geom_point(alpha = 0.5) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "GDP", y = "Life Expectancy") +
  ggtitle("GDP vs Life Expectancy") +
  theme_bw()

# Perform hypothesis test
lm_GDP_Life_Exp <- lm(Life_Expectancy ~ GDP, data = dataset)
summary(lm_GDP_Life_Exp)


## Hypothesis - 3

# We are testing that the average female lives longer than the average male. (μ1=female and μ2=male)
# H0: μ1 <= μ2
# H1: μ1 > μ2

male_data <- subset(dataset, Gender == "M", na.rm=TRUE)
female_data <- subset(dataset, Gender == "F", na.rm=TRUE)

t_result <- t.test(female_data$Life_Expectancy, male_data$Life_Expectancy, mu=0, alternative = 'less', conf.level = 0.95)

# Print the t-test result
print(t_result)

# Fail to reject the Ho since p-value is greater than alpha

library(ggplot2)

library(ggplot2)

# Calculate average life expectancy for female and male data
female_avg_life_exp <- mean(female_data$Life_Expectancy, na.rm = TRUE)
male_avg_life_exp <- mean(male_data$Life_Expectancy, na.rm = TRUE)

# Create a data frame for the bar plot
avg_life_exp <- data.frame(Gender = c("Female", "Male"),
                           Avg_Life_Expectancy = c(female_avg_life_exp, male_avg_life_exp))

# Create the bar plot
ggplot(avg_life_exp, aes(x = Gender, y = Avg_Life_Expectancy, fill = Gender)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Life Expectancy by Gender") +
  xlab("Gender") +
  ylab("Average Life Expectancy") +
  geom_text(aes(label = round(Avg_Life_Expectancy, 2)), 
            position = position_stack(vjust = 0.5), size = 4)

