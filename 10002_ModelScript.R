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
ggplot(dt_,aes(as.factor(dt_$bedrooms),dt_$price)) + geom_boxplot(aes(fill=as.factor(dt_$bedrooms))) + guides(fill=FALSE) + xlab("Number of Rooms") + ylab("Price") + ggtitle("Price ~ Number of Bedrooms")

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
ggplot(dt_,aes(as.factor(dt_$bathrooms))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Number of Bathrooms") + ylab("Count") + ggtitle("Number of Bathrooms distribution")

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


fe_$fullBathroom <- apply(as.data.frame(dt_$bathrooms),1,udf_fe_FractionRooms,return="whole")
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



## Let's look at the full bathrooms
unique(fe_$fullBathroom)

# Barplot
# Full
ggplot(fe_,aes(as.factor(fe_$fullBathroom))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Number of Full Bathrooms") + ylab("Count") + ggtitle("Number of Full Bathrooms distribution")

# Half
ggplot(fe_,aes(as.factor(fe_$halfBathroom))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Number of Half Bathrooms") + ylab("Count") + ggtitle("Number of Half Bathrooms distribution")

# Quarter
ggplot(fe_,aes(as.factor(fe_$quarterBathroom))) + geom_bar(colour="black",aes(fill = ..count..)) + scale_fill_gradient(low="green",high="red") + xlab("Number of Quarter Bathrooms") + ylab("Count") + ggtitle("Number of Quarter Bathrooms distribution")

# Let's look with respect to price

ggplot(fe_,aes(as.factor(fe_$fullBathroom),fe_$price)) + geom_boxplot(aes(fill=as.factor(fe_$fullBathroom))) + guides(fill=FALSE) + xlab("Number of Full Bathrooms") + ylab("Price") + ggtitle("Price ~ Number of Full Bathrooms") + scale_y_continuous(labels=scales::comma)
# We can see a clear trend up except when it's 8 where it drops.

ggplot(fe_,aes(as.factor(fe_$fullBathroom),fe_$price,colour = as.factor(fe_$halfBathroom))) + geom_point() + scale_colour_discrete(name="Half Bathrooms",labels=c("No half bathrooms","Has half Bathrooms")) + scale_y_continuous(name = "Price",labels = scales::comma) + xlab("Full Bathroom Count")



