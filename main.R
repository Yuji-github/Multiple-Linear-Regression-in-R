# Multiple Linear REgression 

#import daata 
data = read.csv('50_Startups.csv')

# encoding categorical to numerical 
data$State = factor(data$State, 
                      levels = c('New York', 'California', 'Florida'), 
                      labels = c(1, 2, 3))

# split dataset into train and test *must have 'caTools' from library
set.seed(123)
split = sample.split(data$Profit, SplitRatio = 0.8)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

# Fitting multiple linear regression to Train models
# to make prediction from train data 
# Profit ~ R.D.Spend + Administration + MArketing.Spend + State -> . (dot) same equation 
regressor = lm(formula = Profit ~ ., data = train)
summary(regressor) # this is important as we have to know which varaibels are impacting 


# from the summary, we got only R.D.Spend has strong relation with Profit 
# So, we can rewrite this code into simple linear regression
# IF the results show 2 features are significant impact to the profit,
# do not need to create this 
regressor_SLR = lm(formula = Profit ~ R.D.Spend, data = train)

# prediction 
y_pred = predict(regressor_SLR, newdata = test)
print(y_pred)


# displaying the results: because this is a regualr simple linear regression 
# if multiple linear regression, do not need to plot the results
ggplot() +
  geom_point(aes(x = test$Profit, y = test$R.D.Spend), 
             color = 'red') +
  geom_line(aes(x = train$Profit, y = predict(regressor_SLR, newdata = train)), 
            color = 'blue') +
  ggtitle('R.D.Spend vs Profit (Test)') + 
  xlab('Profit') + 
  ylab('R.D.Spend')


# backward elminations 
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = data)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend+Marketing.Spend, data = train)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend, data = train)
summary(regressor)
