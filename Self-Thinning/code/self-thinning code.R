#==================================================================================================================
# Albert Morera <morera.marra@gmail.com>
# Mediterranean Forestry (MedFor)
#==================================================================================================================


#------------------------------------------------------------------------------------------------------------------
## Reading and showing data
#------------------------------------------------------------------------------------------------------------------
# We use the 'read.table' function to read .txt files
# This dataset contains information of all the invetari plot
before <- read.table(file = "C:/Users/Albert/Desktop/MedFor/2021-2022/Data/Selfthinning_Before_Selection.txt", header = TRUE)
# Here we plot the relationsheep between the number of trees (n) and the diameter (dn)
plot(before$dn, before$n)

# This dataset contains information only of the selected plots to create the shelf-thinning models
# They are selected acording different criteria
already <- read.table(file = "C:/Users/Albert/Desktop/MedFor/2021-2022/Data/Selfthinnig_Already_Selected.txt", header = TRUE)
plot(already$dn, already$n)

# Here we can see which plots have been selected
plot(before$dn, before$n, col = "red") # Plots before selection
points(already$dn, already$n, col = "blue", cex = 1.5) # Plots already selected
#------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------------
## Fit a simple linear regression model
#------------------------------------------------------------------------------------------------------------------
# First we can check if a logarithmic transformation can help us
plot(x = log(already$dn), y = log(already$n))
# We can see that it approximates a linear relationship. Therefore we can use linear models

# Let's fit a simple linear regression model. with log tranformation on both sides of the equation
m1 <- lm(log(n) ~ log(dn), data = already)

# We must first check that the model fulfills various assumptions and criteria to consider it a good model
# Let's check the statistical significance of model coefficients
summary(m1) # We can see that the diameter is statistically significant to predict the number of trees

# Let's check the homocedasticity of the residuals
plot(fitted(m1), resid(m1))
abline(a=0, b=0, col="red") # We can see that there is no big problem of heteroscedasticity

# Let's check the normality of the residuals
qqnorm(resid(m1))
qqline(resid(m1), col="red") # Maybe there is a problem in the normality in the extreme values
# Let's assume that the different assumptions and criteria are satisfactory.

# Here we plot the line of the model. There are different way to do it
# 1st:
plot(x = log(already$dn), y = log(already$n)) # Here we plot the points with log transformation
abline(m1, col = "red") # 'abline' only works with straight lines

# 2nd:
plot(x = log(already$dn), y = log(already$n)) # Here we plot the points with log transformation
DN <- c(1:50) # We create a sequence of diameters on which the model predictions will be made.
lines(log(DN), coef(m1)[1] + coef(m1)[2]*log(DN), col = "blue")

# 3rd:
plot(x = log(already$dn), y = log(already$n)) # Here we plot the points with log transformation
#The same as the previous one, but setting the coefficients manually. They can be found by doing 'summary(m1)'.
summary(m1) # Summary of the model. Here we can find the coefficients of the model
lines(log(DN), (11.5920) + (-1.4883)*log(DN), col = "green")

# If we plot all the lines at the same time, we see that they are equal (they overlap).
plot(x = log(already$dn), y = log(already$n))
abline(m1, col = "red")
lines(log(DN), coef(m1)[1] + coef(m1)[2]*log(DN), col = "blue", lty="dashed")
lines(log(DN), (11.5920) + (-1.4883)*log(DN), col = "green", lty="dotted")

# Now let's do the logarithmic detransformation to see the result of our model in "normal" scale (without transformation).
plot(already$dn, already$n)
lines(DN, exp(coef(m1)[1] + coef(m1)[2]*log(DN)), col = "red")
# Remember that when we do the logarithmic detransformation we must add a detransformation ...
# ... coefficient. In this case we will use the Baskerville's detransformation coefficient.
lines(DN, exp(coef(m1)[1] + coef(m1)[2]*log(DN) + summary(m1)$sigma^2/2), col = "blue")
# We can see that there is a small difference between the two lines
#------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
## Comparing simple linear regression models
#------------------------------------------------------------------------------------------------------------------
# We are going to compare the first model (m1) with a new one
# We can see that just by doing the logarithmic transformation of the number of trees, ...
# ... the relationship between log(n) and dn, seems to have a linear relationship as well
plot(x = log(already$dn), y = already$n)

# Let's fit a new simple linear regression model with the same variables but with a different ...
# ... variable transformation
m2 <- lm(log(n) ~ dn, data = already)

# Let's check that the assumptions and criteria to consider it a good model
# Let's check the statistical significance of model coefficients
summary(m2) # We can see that the diameter is statistically significant to predict the number of trees

# Let's check the homocedasticity of the residuals
plot(fitted(m2), resid(m2))
abline(a=0, b=0, col="red") # We can see that there is no big problem of heteroscedasticity

# Let's check the normality of the residuals
qqnorm(resid(m2))
qqline(resid(m2), col="red") # Maybe there is a problem in the normality in the extreme values
# Let's assume that the different assumptions and criteria are satisfactory.

# Now let's compare the two adjusted models
# From the point of view of normality and heteroscedasticity of the residuals of the models we ...
# ... see that both models are similar. It does not help us to discard any of the two models.

# Let's compare the R-squared and the residual standard error and the significance of the parameters
# We can do this with the summary of each model
summary(m1)
summary(m2)
# We can see that in both cases, the coeficients are significant
# R-squared is lower in 'm1'
# Residual standard error is lower in 'm1'

# Now, let's see what Akaike Information Criterion (AIC) and Bayesian Inf. criterion (BIC) has to say
AIC(m1, m2)
BIC(m1, m2)
# In both cases the lower value is found in the m1. This means that m1 is better from the point of ...
# ... view of the predictive accuracy and complexity of the model

# Let's check the anova test
anova(m1, m2) # In this particular case, this test tells us that there is no statistically significant ...
# ... improvement between m2 and m1

# Now let's plot both lines of the models
plot(already$dn, already$n)
lines(DN, exp(coef(m1)[1] + coef(m1)[2]*log(DN) + summary(m1)$sigma^2/2), col = "blue")
lines(DN, exp(coef(m2)[1] + coef(m2)[2]*DN + summary(m2)$sigma^2/2), col = "red")
# Which on is better? We cannot say this just by looking at these lines. This is what the other criteria are for
#------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------------
## Fit a multiple linear regression model
#------------------------------------------------------------------------------------------------------------------
# Let's use the site index variable for example
m3 <- lm(log(n) ~ log(dn) + SI, data = already)

# Let's check that the assumptions and criteria to consider it a good model
# Let's check the statistical significance of model coefficients
summary(m3) # We can see that the diameter and site index are statistically significant to predict the number of trees

# Let's check the homocedasticity of the residuals
plot(fitted(m3), resid(m3))
abline(a=0, b=0, col="red") # We can see that there is no big problem of heteroscedasticity

# Let's check the normality of the residuals
qqnorm(resid(m3))
qqline(resid(m3), col="red") # Maybe there is a problem in the normality in the extreme values
# Let's assume that the different assumptions and criteria are satisfactory

# Now let's compare it with the best previous model (m1)
# Let's compare the R-squared and the residual standard error and the significance of the parameters
# We can do this with the summary of each model
summary(m1)
summary(m3)
# We can see that in both cases, the coeficients are significant
# R-squared is lower in 'm3'
# Residual standard error is lower in 'm3'

# Now, let's see what Akaike Information Criterion (AIC) and Bayesian Inf. criterion (BIC) has to say
AIC(m1, m3)
BIC(m1, m3)
# In both cases the lower value is found in the m3. This means that m1 is better from the point of ...
# ... view of the predictive accuracy and complexity of the model

# Let's check the anova test
anova(m1, m3) # In this particular case, this test tells us that there is  statistically significant ...
# ... improvement between m3 and m1. And m3 is better
#------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
## Fit a non-linear regression model
#------------------------------------------------------------------------------------------------------------------

# Fist of all let's try to fit a model equal to m1
m4 <- nls(log(n) ~ a0 + a1 * log(dn), data = already, start = list(a0 = 1, a1 = 1))

# We can see that both models are the same (same parameters)
summary(m1)
summary(m4)

# Let's fit another non-linear 
m5 <- nls(n ~ a1 * dn ^ a2, data = already, start = list(a1 = 1, a2 = 1))
# We see that there is an error due to the initialization parameters
# Let's try others
m5 <- nls(n ~ a1 * dn ^ a2, data = already, start = list(a1 = -1, a2 = 1))

# Let's compare both
summary(m1)
summary(m5) # We can no compare the R-squared or Residual standard error
# We need to use another way
AIC(m1, m5)
# Why this values are very different?? Because we can't compare both models. One (m1) in a log scale, while m5 does not. Very important!!

# But we can plot both. And ofcourse, we can see that both are the same model
plot(already$dn, already$n)
lines(x = DN, y = coef(m5)[1]*(DN^coef(m5)[2]), col = "red")
lines(DN, exp(coef(m1)[1] + coef(m1)[2]*log(DN) + summary(m1)$sigma^2/2), col = "blue")

# Let's fit another non-linear regressio model
m6 <- nls(log(n) ~ a0 + a1 ^ dn, data = already, start = list(a0 = 1, a1 = 1))
# When there is a problem with the number of interactions, we can do the following. Specify a greater number of interactions
m6 <- nls(log(n) ~ a0 + a1 ^ dn, data = already, start = list(a0 = 1, a1 = 1), control = nls.control(maxiter = 1000))
# Now it works

# These two models are on the same scale, so they can be compared using AIC, BIC or anova test
AIC(m1, m6) # We can see that this model is much worse

# Let's ajust a multiple non-linear regression model
m7 <- nls(log(n) ~ a0 + a1 * dn/age + a2 * SI, data = already, start = list(a0 = 1, a1 = 1, a2 = 1), control = nls.control(maxiter = 1000))
# And compare it with m1
AIC(m1, m7)

#------------------------------------------------------------------------------------------------------------------







#------------------------------------------------------------------------------------------------------------------
## Additional content
#------------------------------------------------------------------------------------------------------------------

# You don't have to know how to use it. If you don't want to, you don't need to look at it. It's just a ...
# ... material that I put here in case you want to try new things to plot.

# In this additional content I show another way to plot the data. This is done with a linearity called 'ggplot2'. 
# This library allows to create graphs usually more beautiful and with a great variety of possibilities. 
# When you get familiar with it, it is easier and more intuitive.

# Fist of all you need to download this library
install.packages("ggplot2")
# Then load the library
library(ggplot2)

# Now we need to create a data.frame with the data that we want to plot. In this case, wi will plot the resoults...
# ... of the predictions of the fist model (m1).
# This data.frame will have 2 columns. The fist on with the diameter and the second on with the predictions ...
# ... (number of trees) of each diametre
DN <- c(10:45) # Let's create a vector with diameter value between 10 and 45. predictions will be within this diametre range
df <- data.frame(dn = DN, n_pred = exp(coef(m1)[1] + coef(m1)[2]*log(DN) + summary(m1)$sigma^2/2))

# Now let's plot
ggplot()+ # Initializes a ggplot object
  geom_point(data = already, mapping = aes(x = dn, y = n)) # Add points. In this case, from the 'already' dataset

ggplot()+
  geom_point(data = already, mapping = aes(x = dn, y = n)) +
  geom_line(data = df, mapping = aes(x = dn, y = n_pred), col = "red") # Add a line with the predictions. It is in the 'df' object (where we make the predicitons)

ggplot()+
  geom_point(data = already, mapping = aes(x = dn, y = n)) +
  geom_line(data = df, mapping = aes(x = dn, y = n_pred), col = "red") + 
  coord_cartesian(x = c(11, 45), y = c(0,4000)) # Let's limit the plot

ggplot()+
  geom_point(data = already, mapping = aes(x = dn, y = n)) +
  geom_line(data = df, mapping = aes(x = dn, y = n_pred), col = "red") + 
  coord_cartesian(x = c(11, 45), y = c(0,4000)) +
  labs(x = "Diameter (cm)", y = "Number of trees", title = "Self-thinning", subtitle = "Simple linear regression models") # Here we can change the name axis, title, etc.

ggplot()+
  geom_point(data = already, mapping = aes(x = dn, y = n)) +
  geom_line(data = df, mapping = aes(x = dn, y = n_pred), col = "red") + 
  coord_cartesian(x = c(11, 45), y = c(0,4000)) +
  labs(x = "Diameter (cm)", y = "Number of trees", title = "Self-thinning", subtitle = "Simple linear regression models") +
  theme_bw() # Change the apparence of the plot. There are different default theme (theme_bw(), theme_minimal(), theme_light(), etc. )

ggplot()+
  geom_point(data = already, mapping = aes(x = dn, y = n)) +
  geom_line(data = df, mapping = aes(x = dn, y = n_pred), col = "red") + 
  coord_cartesian(x = c(11, 45), y = c(0,4000)) +
  labs(x = "Diameter (cm)", y = "Number of trees", title = "Self-thinning", subtitle = "Simple linear regression models") +
  theme_bw(base_size = 10) # Let's change the size of the plot labels (adding base_size)

ggplot()+
  geom_point(data = already, mapping = aes(x = dn, y = n)) +
  geom_line(data = df, mapping = aes(x = dn, y = n_pred), col = "red") + 
  coord_cartesian(x = c(11, 45), y = c(0,4000)) +
  labs(x = "Diameter (cm)", y = "Number of trees", title = "Self-thinning", subtitle = "Simple linear regression models") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) # In 'theme' you can change many things (many many things). Here we change the position of the title and subtitle

# There are may thing that you can do, and everithing is explained on internet


ggsave("Path of the directory to save plot to/plot_name.png", units = "cm", dpi = 300, width = 10, height = 6)# This function save the last plot that you made







