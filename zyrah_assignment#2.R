# ============================================================
# Assignment#2: By Group
# Script by: Zyrah Mae M. Dotollo
# Group Number: Group 2
# Dataset: German Credit Risk
# ============================================================

# Load the libraries we need 
library(readxl)
library(dplyr)
library(ggplot2)


# ============================================================
# LOAD DATASET
# ============================================================

# Read the German Credit Risk data from the Excel file
credit_data <- read_excel("C:/Users/Zyrah/Downloads/German Credit Risk.xlsx")

# Removed the first column because it had no name and it was just an index column that we do not need
credit_data <- credit_data[ , !is.na(names(credit_data))]

# Looking at the dataset first so we can see what the data looks like before we start working with it
head(credit_data)
str(credit_data)


# ============================================================
# CUSTOM FUNCTION
# ============================================================

# This function looks at the credit amount and puts it in a group called Low, Medium, or High based on how big the number is
classify_credit <- function(amount) {
  if (amount < 2000) {
    return("Low")
  } else if (amount >= 2000 & amount <= 5000) {
    return("Medium")
  } else {
    return("High")
  }
}

# Function with example values
classify_credit(1500)   # Low
classify_credit(3000)   # Medium
classify_credit(8000)   # High

# Use the function on the whole **Credit_amount** column in the data
credit_data$Credit_Level <- sapply(credit_data$Credit_amount, classify_credit)

# Check the result
head(credit_data[ , c("Credit_amount", "Credit_Level")])


# ============================================================
# DATA STRUCTURES
# ============================================================

# Simple numeric vector of all loan durations in the dataset
duration_vector <- credit_data$Duration
head(duration_vector)

# UNORDERED FACTOR
# Job is stored as numbers (0-3). We convert it to a labeled factor.
# 0 = Unskilled (non-resident), 1 = Unskilled (resident), 2 = Skilled, 3 = Highly Skilled
job_factor <- factor(
  credit_data$Job,
  levels = c(0, 1, 2, 3),
  labels = c("Unskilled Non-Resident", "Unskilled Resident",
             "Skilled", "Highly Skilled")
)
# This is unordered because job types are categories, not ranks
head(job_factor)
levels(job_factor)

# ORDERED FACTOR
# Credit level has a clear low-to-high order, so we make it ordered
credit_level_factor <- factor(
  credit_data$Credit_Level,
  levels = c("Low", "Medium", "High"),
  ordered = TRUE
)
# Ordered means Low < Medium < High
head(credit_level_factor)
levels(credit_level_factor)

# TABLE
# Frequency table showing how many borrowers fall in each credit level
credit_level_table <- table(credit_data$Credit_Level)
credit_level_table

# DATA FRAME
# Data frame with only the columns we need for analysis
analysis_df <- data.frame(
  Age            = credit_data$Age,
  Sex            = credit_data$Sex,
  Job            = job_factor,
  Housing        = credit_data$Housing,
  Credit_Amount  = credit_data$Credit_amount,
  Credit_Level   = credit_level_factor,
  Duration       = credit_data$Duration,
  Purpose        = credit_data$Purpose,
  Risk           = credit_data$Risk
)

# Checking what the data frame looks like
head(analysis_df)
str(analysis_df)


# ============================================================
# DATA MANIPULATION
# ============================================================

# select() - Keeping the columns that actually matter for our questions
selected_df <- analysis_df %>%
  select(Job, Duration, Credit_Amount, Credit_Level, Purpose, Risk)

# filter() - Filter the data to keep only borrowers who have good credit
good_risk_df <- selected_df %>%
  filter(Risk == "good")

# mutate() - Made a new column to put Duration into Short, Medium, or Long loans.
manipulated_df <- good_risk_df %>%
  mutate(
    Duration_Group = case_when(
      Duration <= 12  ~ "Short (up to 12 months)",
      Duration <= 24  ~ "Medium (13 to 24 months)",
      TRUE            ~ "Long (over 24 months)"
    )
  )

# arrange() - Sort by Duration from shortest to longest
manipulated_df <- manipulated_df %>%
  arrange(Duration)

# group_by() and summarise() - Calculate the average credit and average duration for each job category
job_summary <- manipulated_df %>%
  group_by(Job) %>%
  summarise(
    Count            = n(),
    Avg_Duration     = round(mean(Duration), 2),
    Avg_Credit       = round(mean(Credit_Amount), 2)
  )

# rename() - Give the summary columns cleaner names
job_summary <- job_summary %>%
  rename(
    Job_Category        = Job,
    Number_of_Borrowers = Count,
    Average_Duration    = Avg_Duration,
    Average_Credit      = Avg_Credit
  )

# Display the final cleaned dataset
head(manipulated_df)
print(job_summary)


# ============================================================
# DATA VISUALIZATION
# ============================================================

# ----------------------------------------------------------
# UNIVARIATE ANALYSIS
# Question: What loan durations are least frequently chosen by borrowers?
# ----------------------------------------------------------

# Count how often each duration appears
duration_freq <- table(analysis_df$Duration)

# Base R Bar Chart - We use Base R graphics here to show duration frequency
barplot(
  duration_freq,
  main   = "Frequency of Loan Durations",
  xlab   = "Duration (Months)",
  ylab   = "Number of Borrowers",
  col    = c("lightblue", "lightgreen", "lightyellow", "salmon"),
  las    = 2,         # rotate x-axis labels so they fit
  cex.names = 0.7    # make x-axis text a bit smaller
)

# From the chart, the least frequently chosen loan durations are 5, 26, 40, 47, and 72 months, 
# each chosen by only 1 borrower


# ----------------------------------------------------------
# BIVARIATE ANALYSIS
# Question: Does job category have a relationship with loan duration?
# ----------------------------------------------------------

# ggplot2 Box Plot - Box plot is great here because it shows the spread and
#                    center of Duration for each Job category at the same time
ggplot(analysis_df, aes(x = Job, y = Duration, fill = Job)) +
  geom_boxplot(
    color        = "black",   # black border on each box
    linewidth    = 0.5,       # border thickness
    outlier.shape = 1,        # open circle for outliers
    outlier.size  = 1.5
  ) +
  scale_fill_manual(values = c(
    "lightblue", "lightgreen", "lightyellow", "salmon"
  )) +
  labs(
    title = "Loan Duration by Job Category",
    x     = "Job Category",
    y     = "Loan Duration (Months)"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title      = element_text(size = 11),
    axis.text.x     = element_text(size = 10, angle = 7, hjust = 0.6),
    legend.position = "none",    # no legend since x-axis already labels each box
    panel.grid      = element_blank()  # remove grid lines to match the plain style
  )

# From the box plot, As the job level goes up, so does the average loan duration:
# Job 0 (unskilled, non-resident): 17.4 months on average
# Job 1 (unskilled, resident): 16.5 months on average
# Job 2 (skilled): 21.4 months on average
# Job 3 (highly skilled): 25.2 months on average
