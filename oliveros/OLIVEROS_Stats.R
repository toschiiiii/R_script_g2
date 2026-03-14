#----Necessary Libraries----
library(readxl) #read excel file
library(ggplot2) # for visuals
library(moments) # for skewness and kurtosis
library(dplyr) # for data manipulation
library(reshape2) # for heatmap

#----Data----
German_Credit_Risk <- read_excel("C:/Users/Mariel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk
attach(DATA)

#----Data Manipulating----
# [OLIVEROS, MARIEL M.]
Age_Purpose_data <- DATA %>%select(Age, Purpose, Sex)

#----Univariate Analysis----
##----Pie chart----
# 1. Create Frequency Table
purpose_counts <- table(Purpose)

# 2. calculate Percentages
prcnt <- round(purpose_counts / sum(purpose_counts) * 100)

# 3. Combine labels with percentage
labels <- paste(names(purpose_counts), prcnt, "%")

# 4. Pie chart with percentages and reduced white space
pie(purpose_counts, labels = labels, main = "Purpose Distribution",
    col = rainbow(length(purpose_counts)), radius = 1)


#----Bivariate Analysis----
##----Stacked Bar Plot----
ggplot(DATA, aes(x = Purpose, fill = Sex)) +
  geom_bar(position = "stack") +
  labs(title = "Loan Purpose by Sex", x = "Purpose", y = "Count") +
  theme_minimal()


