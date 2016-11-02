### Assessment 1.
### Predictive modelling of house prices.
require(ggplot2)
require(plotly)
require(corrplot)
require(PerformanceAnalytics)

## Exploratory data analysis.

# Set workign directory
setwd("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Applied Data Analysis/A1")


# Read in the data

raw_ <- read.csv("training.csv")

# Look at the structure.
dim(raw_)
str(raw_)

# Seems R loadin has recognised all features to be double or integer.

# Look at the head.
head(raw_)
# Hmm there are fractions of bathrooms. This is interesting.

# Look at the summary.
summary(raw_)

# Looks like ther are rows with no bedroom and bathrooms.
# Also it looks like waterfront is a factor rather than a numeric.
# Zipcode should als obe discrete rather than a number.
# We know that id's are not a variable, so we can dispose that.

# Visulise variable relationships.
chart.Correlation(raw_)


# We can see that the ID variable isn't correlated with anything. (Also identifiers in general are not required).
# It is also evident that there are a number of factor variables.
