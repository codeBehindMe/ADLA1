### Assessment 1.
### Predictive modelling of house prices.
require(ggplot2)
require(plotly)
require(corrplot)
require(PerformanceAnalytics)
require(scales)

################################
## Exploratory data analysis. ##
################################

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

# Remove ID
dt_ <- raw_[,-1]

# Revisualise relationships.
chart.Correlation(dt_)
corrplot(cor(dt_),method = "color")

# Let's investigate the target Variable.
# Plot a histogram.
ggplotly(ggplot(dt_,aes(dt_$price)) + geom_histogram(colour="black",aes(fill = ..count..),bins=50) + scale_fill_gradient(low="green",high="red") + scale_x_continuous(name = "House Price",labels = scales::comma) + scale_y_continuous(name = "Count",labels = scales::comma) + ggtitle("House Price Histogram"))



# Let's explore the variables one by one.

# Bedrooms #
############

summary(dt_$bedrooms)
ggplotly(ggplot(dt_,aes(dt_$bedrooms)) + geom_histogram(colour="black",aes(fill = ..count..),bins=50) + scale_fill_gradient(low="green",high="red") + scale_x_continuous(name = "Bedrooms",labels = scales::comma) + scale_y_continuous(name = "Count",labels = scales::comma) + ggtitle("Bedrooms Histogram"))
# Seems that the bedrooms is a factor variable. Look at the levels.
levels(as.factor(dt_$bedrooms))

# Revisualise
ggplot(dt_,aes(as.factor(dt_$bedrooms))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Number of Bedrooms") + ylab("Count") + ggtitle("Number of Bedrooms distribution")

# Let's look at how the price varies with the number of rooms.
ggplot(dt_,aes(as.factor(dt_$bedrooms),dt_$price)) + geom_boxplot(aes(fill=as.factor(dt_$bedrooms))) + guides(fill=FALSE) + xlab("Number of Rooms") + ylab("Price") + ggtitle("Price ~ Number of Bedrooms") + scale_y_continuous(labels = scales::comma)

# Seems like the mean price steadily increases from 1 to about 6 then starts dropping till 9 where it jumps up.
# No rooms seems to be quite high too. Maybe studio apartments ?
sum(dt_$bedrooms == 0)
# Seems to be only 6 observations.

# Let's feature engineer a new dataframe to keep these factors.
fe_ <- dt_
fe_$bedroomsFactor <- as.factor(fe_$bedrooms)

# Bathrooms #
#############

summary(dt_$bathrooms)
ggplot(dt_,aes(dt_$bathrooms)) + geom_histogram(colour="black",aes(fill = ..count..),bins=32) + scale_fill_gradient(low="green",high="red") + scale_x_continuous(name = "Bathrooms",labels = scales::comma) + scale_y_continuous(name = "Count",labels = scales::comma) + ggtitle("Bathrooms Histogram")
# Given that the max is 8 let's have a look at how many unique values are there.
dt_$bathrooms[unique(dt_$bathrooms)] # Looks like there are 26, but this could definitely be considered a factor.

# Revisualise as factor.
ggplot(dt_, aes(as.factor(dt_$bathrooms))) + geom_bar(colour = "black", aes(fill = ..count..)) + scale_fill_gradient(low = "green", high = "red") + xlab("Number of Bathrooms") + ylab("Count") + ggtitle("Number of Bathrooms distribution")
# Seems suprisingly that 2.5 bathrooms seem to be the most common.

# Let's see how this factored feature responds with price.
ggplot(dt_,aes(as.factor(dt_$bathrooms),dt_$price)) + geom_boxplot(aes(fill=as.factor(dt_$bathrooms))) + guides(fill=FALSE) + xlab("Number of Bathrooms") + ylab("Price") + ggtitle("Price ~ Number of Bathrooms") + scale_y_continuous(name = "Price",labels = scales::comma)
# The general trend seems to be up.


# It is also curious that there are fractional number of rooms. Let's assume that its always, full rooms and a partially finished room. So We can engineer 3 features. Full Bathrooms,Half Bathrooms and Quarter Bathrooms.

udf_fe_FractionRooms <- function(x,return=c("whole","fraction")){
    
    rem_ <- x %% 1
    
    whole_ <- x - rem_
  
    if(return[1] == "whole"){
        return(whole_)
    } else if(return[1] == "fraction"){
        return(rem_)
    } else{
        stop("return type not recognized!")
    }
    
}

udf_utils_MultiPlot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {


    #####################################################
    # Multiplot for ggplot2                             
    # REFERENCE: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/            
    #####################################################


    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                    ncol = cols, nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
        }
    }
}


fe_$fullBathroom <- apply(as.data.frame(dt_$bathrooms), 1, udf_fe_FractionRooms, return = "whole")

fe_$halfBathroom <- apply(as.data.frame(dt_$bathrooms), 1, function(x) {
         y = udf_fe_FractionRooms(x, return = "fraction") 
             if (y == 0.5) {
                 return(1)
             } else{
                 return(0)
             }
     })

fe_$quarterBathroom <- apply(as.data.frame(dt_$bathrooms), 1, function(x) {
    y = udf_fe_FractionRooms(x, return = "fraction")
        if (y == 0.25) {
            return(1)
        } else{
            return(0)
        }
})

fe_$threeQuarterBathroom <- apply(as.data.frame(dt_$bathrooms), 1, function(x) {
    y = udf_fe_FractionRooms(x, return = "fraction")
    if (y == 0.75) {
        return(1)
    } else {
        return(0)
    }
})



## Let's look at the full bathrooms
unique(fe_$fullBathroom)

# Barplot
# Full
p1_ <- ggplot(fe_,aes(as.factor(fe_$fullBathroom))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Number of Full Bathrooms") + ylab("Count") + ggtitle("Number of Full Bathrooms distribution")

# Half
p2_ <- ggplot(fe_,aes(as.factor(fe_$halfBathroom))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Half Bathrooms") + ylab("Count") + ggtitle("Half Bathrooms distribution") + scale_x_discrete(labels=c("No Half Bathrooms","Has Half Bathrooms"))

# Quarter
p3_ <- ggplot(fe_, aes(as.factor(fe_$quarterBathroom))) + geom_bar(colour = "black", aes(fill = ..count..)) + scale_fill_gradient(low = "green", high = "red") + xlab("Quarter Bathrooms") + ylab("Count") + ggtitle("Quarter Bathrooms distribution") + scale_x_discrete(labels=c("No Quarter Bathrooms","Has Quarter Bathrooms"))

# Three Quarter
p4_ <- ggplot(fe_, aes(as.factor(fe_$threeQuarterBathroom))) + geom_bar(colour = "black", aes(fill = ..count..)) + scale_fill_gradient(low = "green", high = "red") + xlab("Three Quarter Bathrooms") + ylab("Count") + ggtitle("Three Quarter Bathrooms distribution") + scale_x_discrete(labels = c("No Three Quarter Bathrooms", "Has Three Quarter Bathrooms"))

udf_utils_MultiPlot(p1_,p2_,p3_,p4_,cols = 2)

# Let's look with respect to price

ggplot(fe_,aes(as.factor(fe_$fullBathroom),fe_$price)) + geom_boxplot(aes(fill=as.factor(fe_$fullBathroom))) + guides(fill=FALSE) + xlab("Number of Full Bathrooms") + ylab("Price") + ggtitle("Price ~ Number of Full Bathrooms") + scale_y_continuous(labels=scales::comma)
# We can see a clear trend up except when it's 8 where it drops.


# It is unlikely that the boolean of has half,quarter,three quarter bathrooms will show a trend. Instead let's use the entire dataset split by full bathrooms and then color encoded by each of which.
# Half 
p1_ <- ggplot(fe_, aes(as.factor(fe_$fullBathroom), fe_$price, colour = as.factor(fe_$halfBathroom))) + geom_boxplot() + scale_colour_discrete(name = "Half Bathrooms", labels = c("No half bathrooms", "Has half Bathrooms")) + scale_y_continuous(name = "Price", labels = scales::comma) + xlab("Full Bathroom Count")

# Quarter
p2_ <- ggplot(fe_, aes(as.factor(fe_$fullBathroom), fe_$price, colour = as.factor(fe_$quarterBathroom))) + geom_boxplot() + scale_colour_discrete(name = "Quarter Bathrooms", labels = c("No Quarter bathrooms", "Has Quarter Bathrooms")) + scale_y_continuous(name = "Price", labels = scales::comma) + xlab("Full Bathroom Count")

# Three quarter
p3_ <- ggplot(fe_, aes(as.factor(fe_$fullBathroom), fe_$price, colour = as.factor(fe_$threeQuarterBathroom))) + geom_boxplot() + scale_colour_discrete(name = "Three Quarter Bathrooms", labels = c("No Three Quarter bathrooms", "Has Three Quarter Bathrooms")) + scale_y_continuous(name = "Price", labels = scales::comma) + xlab("Full Bathroom Count")

udf_utils_MultiPlot(p1_, p2_, p3_)

# Generally speaking it seems the mean price of the ones with a incomplete bathroom in addition to bathrooms is higher. Which is expected.


# Squarfoot living #
####################

summary(dt_$sqft_living)
ggplot(dt_, aes(dt_$sqft_living)) + geom_histogram(colour = "black", aes(fill = ..count..), bins = 50) + scale_fill_gradient(low = "green", high = "red") + scale_x_continuous(name = "Squarefoot Living", labels = scales::comma) + scale_y_continuous(name = "Count", labels = scales::comma) + ggtitle("Squarefoot living Histogram")

# Seems like it's heaily skewed to the right.


# Let's look at it's correlation to the price.
ggplot(dt_, aes(dt_$sqft_living, dt_$price)) + geom_point(aes(colour = dt_$price)) + scale_x_continuous(name = "Squarefoot Living", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot living") + scale_colour_gradient(low = "blue", high = "red") + guides(colour = FALSE) + geom_smooth(method = "lm")

# Seems like there is a positive trend with the increasing of square footage.

# Let's see if we can find some clear groups with the variables we have found so far.

# Full Bathrooms
p1_ <- ggplot(fe_, aes(fe_$sqft_living, fe_$price)) + geom_point(aes(colour = as.factor(fe_$fullBathroom))) + scale_x_continuous(name = "Squarefoot Living", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot living") + scale_color_discrete(guide = guide_legend(title = "Number of Full Bathrooms"))

# Half Bathrooms
p2_ <- ggplot(fe_, aes(fe_$sqft_living, fe_$price)) + geom_point(aes(colour = as.factor(fe_$halfBathroom))) + scale_x_continuous(name = "Squarefoot Living", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot living") + scale_color_discrete(guide = guide_legend(title = "Half Bathrooms"), labels = c("No Half Bathrooms", "Has Half Bathrooms"))

# Quarter Bathrooms

p3_ <- ggplot(fe_, aes(fe_$sqft_living, fe_$price)) + geom_point(aes(colour = as.factor(fe_$quarterBathroom))) + scale_x_continuous(name = "Squarefoot Living", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot living") + scale_color_discrete(guide = guide_legend(title = "Quarter Bathrooms"), labels = c("No Quarter Bathrooms", "Has Quarter Bathrooms"))


# Three Quarter Bathrooms

p4_ <- ggplot(fe_, aes(fe_$sqft_living, fe_$price)) + geom_point(aes(colour = as.factor(fe_$threeQuarterBathroom))) + scale_x_continuous(name = "Squarefoot Living", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot living") + scale_color_discrete(guide = guide_legend(title = "Three Quarter Bathrooms"), labels = c("No Three Quarter Bathrooms", "Has Three Quarter Bathrooms"))

udf_utils_MultiPlot(p1_, p2_, p3_, p4_, cols = 2)

# No clearly defined groups can be seen yet.

# Squarefoot lot # 
##################

summary(dt_$sqft_lot)
ggplot(dt_, aes(dt_$sqft_lot)) + geom_histogram(colour = "black", aes(fill = ..count..), bins = 50) + scale_fill_gradient(low = "green", high = "red") + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Count", labels = scales::comma) + ggtitle("Squarefoot Lot Histogram")
# Seems like there is a lot of skewness present. Try density.
ggplot(dt_, aes(dt_$sqft_lot)) + geom_density() + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Count", labels = scales::comma) + ggtitle("Squarefoot Lot Histogram")
# Seems to be extremely right skewed. 

# Let's check correlatiopn with the price
ggplot(dt_, aes(dt_$sqft_lot, dt_$price)) + geom_point(aes(colour = dt_$price)) + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot Lot") + scale_colour_gradient(low = "blue", high = "red") + guides(colour = FALSE) + geom_smooth(method = "lm")

# Seems like there are almost two correlations one very sharp and one very shallow.


# Let's do the same as we did before and check some groups with existing factors.


# Full Bathrooms
p1_ <- ggplot(fe_, aes(fe_$sqft_lot, fe_$price)) + geom_point(aes(colour = as.factor(fe_$fullBathroom))) + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot Lot") + scale_color_discrete(guide = guide_legend(title = "Number of Full Bathrooms"))

# Half Bathrooms
p2_ <- ggplot(fe_, aes(fe_$sqft_lot, fe_$price)) + geom_point(aes(colour = as.factor(fe_$halfBathroom))) + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot Lot") + scale_color_discrete(guide = guide_legend(title = "Half Bathrooms"), labels = c("No Half Bathrooms", "Has Half Bathrooms"))

# Quarter Bathrooms

p3_ <- ggplot(fe_, aes(fe_$sqft_lot, fe_$price)) + geom_point(aes(colour = as.factor(fe_$quarterBathroom))) + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot Lot") + scale_color_discrete(guide = guide_legend(title = "Quarter Bathrooms"), labels = c("No Quarter Bathrooms", "Has Quarter Bathrooms"))


# Three Quarter Bathrooms

p4_ <- ggplot(fe_, aes(fe_$sqft_lot, fe_$price)) + geom_point(aes(colour = as.factor(fe_$threeQuarterBathroom))) + scale_x_continuous(name = "Squarefoot Lot", labels = scales::comma) + scale_y_continuous(name = "Price", labels = scales::comma) + ggtitle("Price ~ Squarefoot Lot") + scale_color_discrete(guide = guide_legend(title = "Three Quarter Bathrooms"), labels = c("No Three Quarter Bathrooms", "Has Three Quarter Bathrooms"))

udf_utils_MultiPlot(p1_, p2_, p3_, p4_, cols = 2)

# As expected no real groupings.

# Waterfront #
##############
summary(dt_$waterfront)

# Seems to be a factor variable, let's plot it as factor.
ggplot(dt_, aes(as.factor(dt_$waterfront))) + geom_bar(colour = "black", aes(fill = ..count..)) + scale_fill_gradient(low = "green", high = "red") + ggtitle("Waterfront Properties Proportion") + scale_x_discrete(name = "Waterfront", labels = c("Not waterfront", "Is waterfront")) + scale_y_continuous(name = "Waterfront", labels = scales::comma)

# Let's look at correlation to price

ggplot(dt_, aes(y = dt_$price, x = as.factor(dt_$waterfront),fill = as.factor(dt_$waterfront))) + geom_boxplot() + ggtitle("Price ~ Waterfront property") + scale_y_continuous(name = "Price", labels = scales::comma) + scale_x_discrete(name = "Waterfront", labels = c("Not Waterfront", "Is Waterfront")) + guides(fill = FALSE)
# The mean of waterfront houses are clearlier higher.

fe_$waterfrontFactor <- as.factor(fe_$waterfront)

# Condition #
#############

summary(dt_$condition)

# Seems like this is also a factor variable.
ggplot(dt_, aes(as.factor(dt_$condition))) + geom_bar() + ggtitle("Proprtion of Condition in houses") + scale_x_discrete(name = "Condition") + scale_y_continuous(name = "Count", labels = scales::comma)

# Seems to be slightly skewed to the left.

# Let's look with respect to price.
ggplot(dt_, aes(as.factor(dt_$condition), dt_$price)) + geom_boxplot(aes(fill = as.factor(dt_$condition))) + ggtitle("Price ~ Condition") + scale_x_discrete(name = "Condition") + scale_y_continuous(name = "Price", labels = scales::comma) + guides(fill = FALSE)

# Seems higher as the grade improves, but many outliers in grade 3.


fe_$conditionFactor <- as.factor(fe_$condition)

# Grade # 
#########

summary(dt_$grade)

# This also seems to be like a factor variable. 
ggplot(dt_,aes(as.factor(dt_$grade),fill = ..count..)) + geom_bar() + ggtitle("Proportion of Grade") + scale_x_discrete(name = "Grade") + scale_y_continuous(name = "Count",labels = scales::comma) + scale_fill_continuous(low = "green",high = "red")
# Seems like a normal distribution of proportions.

# Let's look at its behaviour with respect to the target.

ggplot(dt_,aes(as.factor(dt_$grade),dt_$price)) + geom_boxplot(aes(fill=as.factor(dt_$grade))) + ggtitle("Price ~ Grade") + scale_x_discrete(name = "Grade") + scale_y_continuous(name= "Price",labels = scales::comma) + guides(fill = FALSE)
# Definitely increasing mean price with the grade. Non linearly but given that it's a factor we can't really force transform.

fe_$gradeFactor <- as.factor(fe_$grade)

# Year built #
##############

# This could be either factor or continuous, but given our data set this might fit a factor, let's try both.
summary(dt_$yr_built)

p1_ <- ggplot(dt_,aes(dt_$yr_built,fill = ..count..)) + geom_histogram(bins = 50) + scale_fill_continuous(low = "green",high="red") + guides(fill=FALSE) + scale_x_continuous(name = "Year Built") + ggtitle("Year Built Histogram")

p2_ <- ggplot(dt_,aes(as.factor(dt_$yr_built),fill = ..count..)) + geom_bar() + scale_fill_continuous(low = "green",high ="red") + guides(fill=FALSE) + scale_x_discrete("Year Built") + theme(axis.text.x = element_text(angle = 45, hjust= 0)) + ggtitle("Year Built Proportions")


udf_utils_MultiPlot(p1_,p2_)

# Barplot seems to be more suitable, however we cant really tell from the multiplot which year has the higest. Let's use our javascript based library to visualise the barplot.
ggplotly(p2_)

# Now we can see the largest number of houses were built in 2014, 250 houses. While the smalles number of houses were built in 1934, which was 9.

# Let's look at its correlation to the target variable. Let's use boxplots since it's better suited to be a factor.
ggplot(dt_,aes(as.factor(dt_$yr_built),dt_$price)) + geom_boxplot(aes(fill = as.factor(dt_$yr_built))) + guides(fill = FALSE) + ggtitle("Price ~ Year Built") + scale_x_discrete(name = "Year Bult") + theme(axis.text.x = element_text(angle = 45)) + scale_y_continuous(name = "Price", labels = scales::comma)

# The means seem to be bouncing up and down, but cant really distinguish a trend. 

fe_$yr_builtFactor <- as.factor(fe_$yr_built)


# Zipcode #
###########

summary(dt_$zipcode)
# Intuitively we know that zipcode is a factor.
length(unique(dt_$zipcode))
# There are 70 distinct zipcodes. 


# Let's plot their proportions.
ggplot(dt_,aes(as.factor(dt_$zipcode), fill = ..count..)) + geom_bar() + guides(fill=FALSE) + ggtitle("Zipcode Proportions") + scale_x_discrete(name = "Zipcode") + theme(axis.text.x = element_text(angle = 90)) + scale_y_continuous(name = "Count", labels = scales::comma) + scale_fill_continuous(low = "green" ,high = "red")

# No real distribution can be seen, seems somewhat scattered.

# Let's look in terms of price.
ggplot(dt_,aes(as.factor(dt_$zipcode),dt_$price)) + geom_boxplot(aes(fill=as.factor(dt_$zipcode))) + guides(fill=FALSE) + ggtitle("Price ~ Zipcode") + scale_x_discrete(name = "Zipcode") + theme(axis.text.x = element_text(angle = 90)) + scale_y_continuous(name = "Price", labels = scales::comma) 
# As expected, we can see mean prices for different zipcodes vary. But not in a trend, since the underlying distance between the zipcodes is hidden.

fe_$zipcodeFactor <- as.factor(fe_$zipcode)

###############
## MODELLING ##
###############

# Now we started with our original dataset with 9 features. And after casting to the data types and some minor feature engineering, we have 19 features.

# Now we can start with some models.

## Methodology ##
#################

# We will implement only linear models in this case, which is what is required by the assignment.
# We will use the minimisation of Root Mean Squared Error as the evaluation objective. 
# The best model performance will be the one that has the lowest Root Mean Squared Error on the test sample.
# Standard model comparison statistics will be utilised to compare the models.

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

# import our test dataset 
ts_ <- read.csv("dev.csv")
ts_ <- ts_[,-1] # Remove the ID column.

# Let's make a matrix to keep a track of the RMSEs
errMtx_ <- matrix(c("Model","TrainingError","TestError"),nrow = 1,ncol = 3)

# Let's set our baseline. Let's use mean prediction and calculate training and test RMSE.
 
mdlName_ <- "Mean Model"
mdlTrRmse <- udf_utils_rootMeanSquareError(rep(mean(dt_$price),times = length(dt_$price)),dt_$price)
mdlTsRmse <- udf_utils_rootMeanSquareError(rep(mean(dt_$price),times = length(ts_$price)),ts_$price)

# Add to error matrix.
errMtx_ <- rbind(errMtx_,c(mdlName_,mdlTrRmse,mdlTsRmse))


udf_utils_ModelScoreHelper <- function(model,modelName="MyModel",trainFeature,testFeature,trainLabel,testLabel,trackMatrix=NULL,showResult=TRUE){
    
    # This function helps generate RMSE from test and train and also storing it in teh matrix.
    trRMSE_ <- udf_utils_rootMeanSquareError(predict(model,trainFeature),trainLabel)
    tsRMSE_ <- udf_utils_rootMeanSquareError(predict(model,testFeature),testLabel)
    
    if(!is.null(trackMatrix)){
        trackMatrix <- rbind(trackMatrix,c(modelName,trRMSE_,tsRMSE_))
    }
    
    if(showResult){
        print(paste("Training Set RMSE:",trRMSE_,sep = " "))
        print(paste("Test Set RMSE:",tsRMSE_,sep = " "))
    }
    
    return(trackMatrix)
}

# Let's cut into our linear model with a simple model based of the original dataset.
mdl_1 <- lm(price ~ .,data = dt_)

# Get the objectives.
errMtx_ <- udf_utils_ModelScoreHelper(mdl_1,"Cut In",dt_[,-1],ts_[,-1],dt_$price,ts_$price,trackMatrix = errMtx_)  

# Not bad. 45 % imrpovement in RMSE. 

# Now let's use our feature engineered dataset to develop the model.
# We will need to develop a function to transform the base dataset to our feature engineered dataset.

# First let's do a transformation that's just a simple casting of known factor features. 
udf_fe_intern_FactorKnowFeatures <- function(dataSet){
    
    # Convert's known values to factors.
    
    dataSet[,'bedrooms'] <- as.factor(dataSet[,'bedrooms'])
    dataSet[,'bathrooms'] <- as.factor(dataSet[,'bathrooms'])
    dataSet[,'waterfront'] <- as.factor(dataSet[,'waterfront'])
    dataSet[,'condition'] <- as.factor(dataSet[,'condition'])
    dataSet[,'grade'] <- as.factor(dataSet[,'grade'])
    dataSet[,'yr_built'] <- as.factor(dataSet[,'yr_built'])
    dataSet[,'zipcode'] <- as.factor(dataSet[,'zipcode'])
    
    return(dataSet)
}

mdl_2 <- lm(price ~ .,data = udf_fe_intern_FactorKnowFeatures(dt_))

summary(mdl_2)

fe_tr <- udf_fe_intern_FactorKnowFeatures(dt_)
fe_ts <- udf_fe_intern_FactorKnowFeatures(ts_)

errMtx_ <- udf_utils_ModelScoreHelper(mdl_2,"Factorised",trainFeature = fe_tr[,-1],testFeature = fe_ts[,-1],trainLabel = fe_tr$price,testLabel = fe_ts$price,trackMatrix = errMtx_)

# The test set RMSE is down to 144xxx about a 30% improvement over our cut in model.
# Now we know that casting somerthing as a factor will transpose the matrix into a series of booleans for each factor level. 
# Let's see if we can augment and help the model out a bit by augmenting the bathrooms.


udf_fe_inter_DescribeBathRoomFeatures <- function(dataSet){
    
    dataSet[,'fullBathroom'] <- apply(as.data.frame(dataSet[,'bathrooms']), 1, udf_fe_FractionRooms, return = "whole")
    
    dataSet[,'halfBathroom'] <- apply(as.data.frame(dataSet[,'bathrooms']), 1, function(x) {
        y = udf_fe_FractionRooms(x, return = "fraction") 
        if (y == 0.5) {
            return(1)
        } else{
            return(0)
        }
    })
    
    dataSet[,'quarterBathroom'] <- apply(as.data.frame(dataSet[,'bathrooms']), 1, function(x) {
        y = udf_fe_FractionRooms(x, return = "fraction")
        if (y == 0.25) {
            return(1)
        } else{
            return(0)
        }
    })
    
    dataSet[,'threeQuarterBathroom'] <- apply(as.data.frame(dataSet[,'bathrooms']), 1, function(x) {
        y = udf_fe_FractionRooms(x, return = "fraction")
        if (y == 0.75) {
            return(1)
        } else {
            return(0)
        }
    })
    
    
    dataSet[,'bedrooms'] <- as.factor(dataSet[,'bedrooms'])
   # dataSet[,'bathrooms'] <- as.factor(dataSet[,'bathrooms'])
    dataSet[,'waterfront'] <- as.factor(dataSet[,'waterfront'])
    dataSet[,'condition'] <- as.factor(dataSet[,'condition'])
    dataSet[,'grade'] <- as.factor(dataSet[,'grade'])
    dataSet[,'yr_built'] <- as.factor(dataSet[,'yr_built'])
    dataSet[,'zipcode'] <- as.factor(dataSet[,'zipcode'])
    
    
     dataSet[,'fullBathroom'] <- as.factor(dataSet[,'fullBathroom'])
     dataSet[,'halfBathroom'] <- as.factor(dataSet[,'halfBathroom'])
     dataSet[,'quarterBathroom'] <- as.factor(dataSet[,'quarterBathroom'])
     dataSet[,'threeQuarterBathroom'] <- as.factor(dataSet[,'threeQuarterBathroom'])
    
    
    return(dataSet)
    
}


# Let's prepare the data
fe_tr <- udf_fe_inter_DescribeBathRoomFeatures(dt_)
fe_ts <- udf_fe_inter_DescribeBathRoomFeatures(ts_)

mdl_3 <- lm(price ~ .,data =fe_tr)


errMtx_ <- udf_utils_ModelScoreHelper(mdl_3,"BathroomSplit",trainFeature = fe_tr[,-1],testFeature = fe_ts[,-1],trainLabel = fe_tr$price,testLabel = fe_ts$price,trackMatrix = errMtx_)


summary(mdl_3)