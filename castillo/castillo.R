#install.packages("here")
#library(here)

library(readxl) #Reads excel file
library(dplyr) #Data manipulation
library(ggplot2) #Data visualization


# 3.) Ang sequence lagi ng script natin is: 
#- Header Libraries
#- Custom Functions (if applicable)
#- Data Structure
#- Univariate Analysis [Data Manip, Data Visual]
#- Bi/Multivariate Analysis [Data Manip, Data Visual]

#4.) You guys can use custom functions, pero 
  #wag yung functions na inside other libs na di naturo




DATA <- read_excel("C:/Users/user/Documents/School/Stats For CS (2-2)/castillo/German_Credit_Risk.xlsx")
attach(DATA)


#Univariate
#What is the highest amount of checking account rating for loaners?
#Bar graph to show ranking
rating <- c("NA", "little", "moderate", "rich")
Checking_Order <- factor(DATA$Checking_account, levels = rating, ordered = TRUE)


Checking_Rank <- sort(table(Checking_Order), decreasing = TRUE)

#Base R
barplot(Checking_Rank,
        xlab = "Rating",
        ylab = "Count",
        col = c("black", "red", "yellow", "green"))

title("Checking Account Ratings")


#Bivariate
#Which loan purposes are associated with the highest average credit amount?
#Purposes
#Credit Amount Average
#Bar graph?

Average_Amount_Purpose <- DATA %>% 
  group_by(Purpose) %>% 
  summarize(Average_Credit = mean(Credit_amount, na.rm = TRUE)) %>% 
  arrange(desc(Average_Credit))


#Base R
barplot(Average_Amount_Purpose$Average_Credit,
        names.arg = Average_Amount_Purpose$Purpose,
        xlab = "Purpose",
        ylab = "Average Amount",
        col = ("Purple"))

title("Highest Average Credit Amount Per Purpose")


