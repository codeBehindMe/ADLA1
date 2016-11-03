### Assessment 1.
### Predictive modelling of house prices.
require(ggplot2)
require(plotly)
require(corrplot)
require(PerformanceAnalytics)
require(scales)

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
ggplot(dt_,aes(as.factor(dt_$bathrooms),dt_$price)) + geom_boxplot(aes(fill=as.factor(dt_$bathrooms))) + guides(fill=FALSE) + xlab("Number of Bathrooms") + ylab("Price") + ggtitle("Price ~ Number of Bathrooms")
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
p3_ <- p2_ <- ggplot(fe_, aes(as.factor(fe_$fullBathroom), fe_$price, colour = as.factor(fe_$threeQuarterBathroom))) + geom_boxplot() + scale_colour_discrete(name = "Three Quarter Bathrooms", labels = c("No Three Quarter bathrooms", "Has Three Quarter Bathrooms")) + scale_y_continuous(name = "Price", labels = scales::comma) + xlab("Full Bathroom Count")

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
ggplot(dt_, aes(as.factor(dt_$waterfront))) + geom_bar() + ggtitle("Waterfront Properties Proportion") + scale_x_discrete(name = "Waterfront", labels = c("Not waterfront", "Is waterfront")) + scale_y_continuous(name = "Waterfront", labels = scales::comma)

# Let's look at correlation to price

ggplot(dt_, aes(y = dt_$price, x = as.factor(dt_$waterfront))) + geom_boxplot() + ggtitle("Price ~ Waterfront property") + scale_y_continuous(name = "Price", labels = scales::comma) + scale_x_discrete(name = "Waterfront", labels = c("Not Waterfront", "Is Waterfront"))
# The mean of waterfront houses are clearlier higher.

# Condition #
#############

summary(dt_$condition)

# Seems like this is also a factor variable.
ggplot(dt_, aes(as.factor(dt_$condition))) + geom_bar() + ggtitle("Proprtion of Condition in houses") + scale_x_discrete(name = "Condition") + scale_y_continuous(name = "Count", labels = scales::comma)

# Seems to be slightly skewed to the left.

# Let's look with respect to price.
ggplot(dt_, aes(as.factor(dt_$condition), dt_$price)) + geom_boxplot(aes(fill = as.factor(dt_$condition))) + ggtitle("Price ~ Condition") + scale_x_discrete(name = "Condition") + scale_y_continuous(name = "Price", labels = scales::comma) + guides(fill = FALSE)

# Seems higher as the grade improves, but many outliers in grade 3.

# Grade # 
#########

summary(dt_$grade)

# This also seems to be like a factor variable. 
ggplot(dt_,aes(as.factor(dt_$grade))) + geom_bar()