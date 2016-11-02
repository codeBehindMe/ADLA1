### Assessment 1.
### Predictive modelling of house prices.
require(ggplot2)
require(plotly)
require(corrplot)

## Exploratory data analysis.

# Set workign directory
setwd("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Applied Data Analysis/A1")


# Read in the data

raw_ <- read.csv("training.csv")

# Look at the structure.
str(raw_)

# Seems R loadin has recognised all features to be double or integer.

# Look at the summary.
summary(raw_)

# Looks like ther are rows with no bedroom and bathrooms.
# Also it looks like waterfront is a factor rather than a numeric.
# Zipcode should als obe discrete rather than a number.
# We know that id's are not a variable, so we can dispose that.

# Visulise variable relationships.
plot(raw_)
corrplot(round(cor(raw_),2),method = "pie")

## So price is a continuous numerical variable, don't need to be a genius to figure that out.
## Bedrooms seem to be a discrete numerical variable. (From the plot anyway)
## Bathrooms are also a discrete numerical variable.
## SqrFootLiving seems to be a numerical continuous variable.
## SqrFootLot seems to be a numerical continuous variable.
## Waterfront seems to be a boolean factor.
## Condition seems to be a factor  variable with 5 levels.
## Grade also seems to be a factor variable with 12 levels.
## YrBuilt is a discrete numerical variable.
## Zipcode seems to be a factor variable as well.

## Let's remove the ID variable, since this is added arbitrarily to the dataset.
dt_ <- raw_[, !(names(raw_) %in% "id")]

# Let's also subset our features out from the target.
feat_ <- dt_[, !(names(dt_) %in% "price")]
lbl_ <- as.data.frame(dt_[, (names(dt_) %in% "price")])
names(lbl_) <- "price"

pcaMdl_ <- prcomp(feat_)
ggplot(as.data.frame(pcaMdl_$x), aes(pcaMdl_$x[, 1], pcaMdl_$x[, 2])) + geom_point(aes(color = lbl_$price)) + xlab("PC1") + ylab("PC2") + ggtitle(label = "Training Data (Principle Components vs Price)")

# It's hard to see like a immideate trend from just the principle components.

# Let's examine each of the features separately.

## Bedrooms
# Discrete
# Numerical
summary(feat_$bedrooms)
plot_ly(x = feat_$bedrooms, type = "histogram",bins=8) 

# Seems to be positively skewed.

ggplotly(ggplot(feat_,aes(feat_$bedrooms,lbl_$price)) +geom_point() + xlab("Number of Bedrooms") + ylab("Price") + ggtitle(label = "Price ~ # Bedrooms") + geom_smooth(method="lm"))

# Seems to have a positive gradient.

## Bathrooms
summary(feat_$bathrooms)
plot_ly(x = feat_$bathrooms, type = "histogram")
# Seems also to be positively skewed.
ggplotly(ggplot(feat_,aes(feat_$bathrooms,lbl_$price)) + geom_point() + xlab("Number of Bathrooms") + ylab("Price") + ggtitle(label = "Price ~ # Bathrooms") + geom_smooth(method = "lm"))
# Positive gradient with price.

## Square feet living (sqft_living)
summary(feat_$sqft_living)
plot_ly(x = feat_$sqft_living, type = 'histogram')

ggplotly(ggplot(feat_, aes(feat_$sqft_living, lbl_$price)) + geom_point() + xlab("Square ft living") + ylab("Price") + ggtitle(label = "Sqft Living ~ Price") + geom_smooth(method = "lm"))

## Square feet lot (sqft_lot)
summary(feat_$sqft_lot)
plot_ly(x = feat_$sqft_lot, type = 'histogram')
# Extremely right skewed.
ggplotly(ggplot(feat_, aes(feat_$sqft_lot, lbl_$price)) + geom_point() + xlab("Square ft lot") + ylab("Price") + ggtitle(label = "Sqft Lot ~ Price") + geom_smooth(method = "lm"))

## Waterfront
ggplot(feat_, aes(x = factor(1), fill = as.factor(feat_$waterfront))) + geom_bar(width = 1) + coord_polar(theta = "y")

ggplotly(ggplot(feat_, aes(x = pcaMdl_$x[, 1], y = lbl_$price)) + geom_point(aes(color = as.factor(feat_$waterfront))))

## Condition
summary(feat_$condition)
plot_ly(x = feat_$condition, type = 'histogram')

ggplotly(ggplot(feat_, aes(feat_$condition, lbl_$price)) + geom_point() + xlab("Condition") + ylab("Price") + ggtitle(label = "Condition ~ Price") + geom_smooth(method = "lm"))


## Grade
summary(feat_$grade)
plot_ly(x = feat_$grade, type = "histogram")

ggplotly(ggplot(feat_, aes(feat_$grade, lbl_$price)) + geom_point() + xlab("Grade") + ylab("Price") + ggtitle(label = "Grade ~ Price") + geom_smooth(method = "lm"))

## Yr Built
summary(feat_$yr_built)

plot_ly(x = feat_$yr_built, type = "histogram")

ggplotly(ggplot(feat_, aes(feat_$yr_built, lbl_$price)) + geom_point() + xlab("Year Built") + ylab("Price") + ggtitle(label = "Year Built ~ Price") + geom_smooth(method = "lm"))


## Zipcode
summary(feat_$zipcode)

plot_ly(x = feat_$zipcode, type = "histogram")
ggplotly(ggplot(feat_, aes(feat_$zipcode, lbl_$price)) + geom_point() + xlab("ZipCode") + ylab("Price") + ggtitle(label = "Zipcode ~ Price") + geom_smooth(method = "lm"))



# Let's make a baseline.

udf_utils_castFlexibleDataFrame <- function(object) {

    # Utility function that coerces vectors, dataframes, matrices and other enumerable types to data frame.

    cNames_ <- colnames(object) # get object column names.
    dfObj_ <- as.data.frame(object) # cast object as data frame.

    if (is.null(cNames_)) {
        # if no column names assign generic.
        for (i in 1:length(dfObj_)) {
            colnames(dfObj_)[i] <- paste0("c", i)
        }
    }
    return(dfObj_)
}

udf_utils_rootMeanSquareError <- function(predVals, actVals) {

    # this function returns teh root mean square error of predicted values and actual values. 

    predVals <- udf_utils_castFlexibleDataFrame(predVals)
    actVals <- udf_utils_castFlexibleDataFrame(actVals)

    if (nrow(predVals) != nrow(actVals)) {
        stop("differring predictions and actual values.")
    }

    eW_ <- (sum((predVals - actVals) ^ 2)) / 2

    eRMS_ <- sqrt((2 * eW_) / nrow(predVals))

    return(eRMS_)

}

# Let's do a mean prediction

meanPrediction <- rep(mean(lbl_$price), nrow(lbl_))

# Check the rmse
udf_utils_rootMeanSquareError(meanPrediction, lbl_)

# 368289.5 Which is prettey bad (68% of the mean)

# Let's do a simple LM with no search.
linearModel_ <- lm(price ~ .,data=dt_)

udf_utils_rootMeanSquareError(predict(linearModel_, feat_), lbl_$price)
# 219144.2  which is 40% of the mean.

dt_$bathrooms <- as.factor(dt_$bathrooms)
feat_$bathrooms <- as.factor(feat_$bathrooms)