# The following functions will build a prediction model for a large datasets using only the top 5 predictors.

# This is an example presented by Roger Peng in his Develpoing Data Products Class, part of Data Science Specialization offered by Coursera. 
# For more info go to "Building R Packages Demo" class.

# Here below is how the top 10 function works:

#For each predictor of your dataset of predictors, we fit a univariate regression model.

#For each individual predictor, we will calculate the p value associated with that given predictor, which will tell us how strong the association is.

#We will sort the p values from smallest to largest and we will take the top 10 smallest, that are the predictors with the strongest association.

#Finally we will fit a regression model with only those 10 top predictors.

top10<- function(x,y) {
  p<- ncol(x)
  if(p < 10)
    stop ("there are less than 10 predictors")

# Initialize the vector of p values
pvalues<- numeric(p)  # is going to be an ampty vector of zero

# Nww Loop through each of the predictors and fit a univariate regression model
for(i in seq_len(p)) {
  fit<- lm(y~x[, i])
  summ<- summary(fit)  # we need the summary of the fit model to get the p values
  pvalues[i]<- summ$coefficients[2, 4]  # get the p values
}

# Sort the p values
ord<- order(pvalues)
ord<- ord[1:10]  # we need to take just the top 10
x10<- x[, ord]  # we create a new dataset with the top 10 predictors

# Set the final linear model and grab the coefficients
fit<- lm(y~x10)
coef(fit)

}


