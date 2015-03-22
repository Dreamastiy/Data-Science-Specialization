library('dplyr')
library('GGally')
library('ggplot2')
library('knitr')

data('mtcars')
# some descriptions of data variables
str(mtcars)
# some information of what variables mean
?mtcars 

# let's look at scatterplots
ggpairs(mtcars, lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")), 
        upper=list(params=list(corSize=6)))
# One can see that there are some variables on the scatterplot which can be seemed as factors.
# Though there are some high correlation values between mpg and these variables, graphs show that the dependencies are not linear. 
# So let's make some factors variables. These variables are number of cylinders (cyl), type of engine(vs), 
# type of transmission (am), number of forward gears (gear), number of carburetors(carb).

mtcars <- mutate(mtcars, 
                 am = factor(am, labels = c('Auto','Man')), 
                 cyl = factor(cyl), 
                 vs = factor(vs), 
                 gear = factor(gear), 
                 carb = factor(carb))

# some descriptive information about "new" mtcars data
summary(mtcars)
# Lets make t.test for difference in mpg with different transmissions.
# Because we have not many points, lets assume that the distributions of mpg for different transmissions are normal.
par(mfrow = c(1,2))
qqnorm(as.numeric(mtcars$mpg[mtcars$am=='Man']), main = 'Manual')
qqnorm(as.numeric(mtcars$mpg[mtcars$am=='Auto']), main = 'Automatic')
# From the plots one can see that our assumption is not far from the truth
# now t.test

x.manual <- mtcars[(mtcars$am=='Man'),]$mpg
x.auto <- mtcars[(mtcars$am=='Auto'),]$mpg
tt <- t.test(x.manual, 
       x.auto,
       alternative = 'greater')

# t.test shows significant difference in mpg for auto and manual transmission. 
# lets save regression mpg ~ am. we will you it later
model.first <- lm(data = mtcars, mpg~am)

# but t.test assumes that all the other parameters are equal

# lets compare different sample populations across various factors parameters
ggplot(mtcars, aes(x = cyl, y = mpg, fill = am)) +
     geom_boxplot() +
     facet_wrap(~ vs + gear + carb, scales = 'free_x')

# From the plot one can see that only cars with straight type of engine (vs == 1), with 4 forwards gears (gear == 4) and 2 carburetors (carb == 2) have both automatic and manual transmissions
# And we see that for these cars mpg is larger for manual transmission. Let's examine these cars first.
mtcars[(mtcars$vs==1)&(mtcars$gear==4)&(mtcars$carb==2),]

# Too bad there are only 4 cars. But let's make a T-test for them
x.manual <- mtcars[(mtcars$vs==1)&(mtcars$gear==4)&(mtcars$carb==2)&(mtcars$am=='Man'),]$mpg
x.auto <- mtcars[(mtcars$vs==1)&(mtcars$gear==4)&(mtcars$carb==2)&(mtcars$am=='Auto'),]$mpg
t.test(x.manual, 
       x.auto,
       alternative = 'greater')

# now t.test shows that there are no significant difference between auto and manual transmissions. But the number of points is only 4.
# Time to build a regression!
# lm with all the variables 
model <- lm(data = mtcars, mpg~.) 
# see that in this case there are no 5%-significance variables
summary(model)$coefficients
# we see that there are no significant parameters.
# let's now exclude variables with step function
model.stepped <- step(model)
summary(model.stepped)$coefficients[,1]
summary(model.stepped)$coefficients[,4]

# One can see that changing transmission from automatic to manual increases mpg by 1.8. But this coefficient is 5%-unsignificant!
# lets exclude variables with the highest p-value from the regression

model.noam <- lm(data = mtcars, mpg ~ cyl + hp + wt)
summary(model.noam)$coefficients[,1]
summary(model.noam)$coefficients[,4]

# Horse power is also 5%-unsignificant. Lets exclude this parameter too

model.final <- lm(data = mtcars, mpg ~ cyl + wt)
summary(model.final)$coefficients[,1]
summary(model.final)$coefficients[,4]

par(mfrow = c(2,2))
plot(model.final)

# from the plots you can see that the residuals almost normally distributed (which is good) but there are also several outliers (which is bad).
# now see the difference between final model and the first one
anova(model.first, model.final)
# anova tes shows that the models are significantly not equal.


?ggplot
