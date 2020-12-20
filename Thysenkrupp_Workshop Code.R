### Data Science for Business I
### Group  
### 

# -----------------------------------------------------------------------------------------------
# Case: Solid as Steel: Produktionsplanung bei ThyssenKrupp
# -----------------------------------------------------------------------------------------------

# Settings

# Remove all variables from the environment
rm(list =ls())

# Install readxl
install.packages("readxl")

# Run readxl
library("readxl")

# Load the data and call it database
database <- # complete this line

# Using summary to check the data for NA
summary(database)



# -----------------------------------------------------------------------------------------------
# Task 1: Perform a univariate analysis and answer the following questions: 
# -----------------------------------------------------------------------------------------------


# 1.a
# What is the average number of strips per shift? 
# Calcualte the average number of strips per shift
total_strips_per_shift <- database$`thickness 1` + database$`thickness 2` + database$`thickness 3`
avg_strips_per_shift <- mean(total_strips_per_shift) 


# 1.b
# Strips of which thickness cluster are the most common, and strips of which thickness cluster are the least common?
sum_t1 <- sum(database$`thickness 1`) 
sum_t2 <- sum(database$`thickness 2`)
sum_t3 <- sum(database$`thickness 3`) 

# Plot a barchart 
# las to rotate the names in the plot
barplot(cbind(sum_t1,sum_t2,sum_t3), 
        ylab = "No. of strips", main = "No. of strips per thickness cluster",
        names= c("Thickness 1","Thickness 2","Thickness 3"), las=1, col="#0099FF")



# 1.c
# What are the minimum, average, and maximum values of delta throughput and RTR? 

# Delta throughput
summary(database$`delta throughput`)




# RTR
summary(database$`run time ratio`)



# 1.d
# Are there shifts during which the PPL processes strips of only steel grade 1, or of only steel grade 2, etc.? 
# Shifts during which the PPL processes strips of only one steel grade
max(database$`grade 1`) 
max(database$`grade 2`) 
max(database$`grade 3`)
max(database$`grade 4`) 
max(database$`grade 5`) 
max(database$`grade rest`) 

# Calculating the number of shifts with only one steel grade produced
grade1only <- sum(database$`grade 1`==100) 
grade2only <- sum(database$`grade 2`==100) 
grade3only <- sum(database$`grade 3`==100) 
grade4only <- sum(database$`grade 4`==100) 
grade5only <- sum(database$`grade 5`==100) 
graderestonly <- sum(database$`grade rest`==100) 

# Creating a barplot to visualize 
barplot(cbind(grade1only,grade2only,grade3only,grade4only,grade5only, graderestonly), 
        ylab = "No. of shifts", main = "No. of shifts with only producing one grade",
        names= c("Grade 1","Grade 2","Grade 3","Grade 4","Grade 5", "GR"), las=2, col="#0099FF")


# -----------------------------------------------------------------------------------------------
# Task 2: Can the RTR theory adequately explain the deviations from the planned production 
#         figures? Explain why or why not.
# -----------------------------------------------------------------------------------------------

# Calculating the correlation between RTR and delta throughput
cor(database$`run time ratio`,database$`delta throughput`)

# Plot RTR against delta throughput with a smoothing line to see the trend 
library(ggplot2)
ggplot(database,aes(database$`run time ratio`,database$`delta throughput`))+
  geom_point()+
  geom_smooth(method = "lm", color = "#0099FF")+
  labs(x="RTR", y="delta throughput", title="What about correlation?")



# -----------------------------------------------------------------------------------------------
# Task 3: Is the MPT theory sufficient to explain the deviations? Explain why or why not.
# -----------------------------------------------------------------------------------------------

# Calculating the correlation of MPT and delta throughput
cor(database$`MPT`,database$`delta throughput`)

# Plot MPT against delta throughput with a smoothing line to see the trend 
ggplot(database,aes(database$`MPT`,database$`delta throughput`))+
  geom_point()+
  geom_smooth(method = "lm", color = "#0099FF")+
  labs(x="MPT", y="delta throughput", title="What about correlation?")

# -----------------------------------------------------------------------------------------------
# Task 4: Develop a sound regression model that can be used to predict delta throughput based on  
# the characteristics of the strips scheduled for production. Include only explanatory variables 
# that have a coefficient with a 10% level of significance.
# -----------------------------------------------------------------------------------------------

# We can not use all characteristics for the regression model because there would be perfect 

# find the best regression model
library(MASS)
optimal_lm <- lm(`delta throughput` ~ `thickness 1` + `thickness 2` + `thickness 3` + `width 1` +
            `width 2` + `width 3` + `grade 1` + `grade 2` + `grade 3` + `grade 4` + `grade 5` + `grade rest` + `run time ratio`, data=database)
step <- stepAIC(optimal_lm, direction="both")
step$anova 

#create optimal linear regression model to maximize adjusted R-squared 
lm_1 <- lm(`delta throughput` ~ `thickness 1` + `thickness 2` + `thickness 3` + `width 1` +
                     `width 2` + `grade 1` +`grade 3` + `grade 4` +`grade 5` + `run time ratio`, data=database)

summary(lm_1)

# Plot the residuals to see if they are random
plot(residuals(lm_1),lm_1$fitted.values)

# Breusch-Pagan-Test to check homoskedasticity
bptest(lm_1)

#Create heteroskedastiscity robust standard errors
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

coeftest(lm_1, vcov = vcovHC(lm_1, "HC1"))

# As a result of the coeftest we see that grade 4 is not significant for the regression model.
# We leave it out for the regression model lm2.
lm_2 <- lm(`delta throughput` ~ `thickness 1` + `thickness 2` + `thickness 3` + `width 1` +
             `width 2` + `grade 1` +`grade 3` +`grade 5` + `run time ratio`, data=database)
summary(lm_2)




# -----------------------------------------------------------------------------------------------
# Task 5: Interpret the coefficient of RTR for the PPL and provide a 90% confidence interval for  
# the value of the coefficient (in the population).
# -----------------------------------------------------------------------------------------------

# The RTR is significant at a 99% confidence level.
# An increase of the RTR by one percentage point, given that nothing else changed, explains an increase 
# in delta throughput by 5.40 units.

# Calculation the 90% confidence intervall for all coefficients
confint(lm_2, level=0.90)


# -----------------------------------------------------------------------------------------------
# Task 6: A strip of thickness 1 and width 1 is replaced by a strip of thickness 3 and width 3. 
# This change does not affect any other aspect of the production. Provide an estimate for the 
# change in delta throughput. 
# -----------------------------------------------------------------------------------------------

# delta throughput for thickness 1 and width 1
dt11<--881.13084627+8.77521279-10.91676181

# delta throughput for thickness 3 and width 3
dt33<--881.13084627+17.83759601

diff<-dt33-dt11
diff

# -----------------------------------------------------------------------------------------------
# Task 7: The table below shows the data provided by the production engineers. Because of major 
# upcoming maintenance on the PPL, only 84 shifts were planned for the month of May. Provide an 
# estimate for the average delta throughput per shift in May based on these estimated figures. 
# (The actual figures are, of course, still unknown.) 
# -----------------------------------------------------------------------------------------------

lm_database1 <- lm(`delta throughput` ~ `thickness 1` + `thickness 2` + `thickness 3` + `width 1` +
                    `width 2` + `grade 1` +`grade 3` +`grade 5` + `run time ratio`, data=database)

summary(lm_database1)

# Assumption: As there is no giving RTR we assumed the mean of the historical RTR
new <- data.frame("thickness 1" = c(996), "thickness 2" = c(1884), "thickness 3" = c(434), "width 1" = c(1242), "width 2" = c(1191), "grade 1" = c(109), "grade 2" = c(709),"grade 3" = c(167),"grade 4" = c(243),"grade 5" = c(121), "run time ratio" = c (85.77))
names(new)[1] <- "thickness 1"
names(new)[2] <- "thickness 2"
names(new)[3] <- "thickness 3"
names(new)[4] <- "width 1"
names(new)[5] <- "width 2"
names(new)[6] <- "grade 1"
names(new)[7] <- "grade 2"
names(new)[8] <- "grade 3"
names(new)[9] <- "grade 4"
names(new)[10] <- "grade 5"
names(new)[11] <- "run time ratio"


# Prediction Interval
predict_full_month <- predict(lm_database1, newdata = new, interval="prediction")
predict_per_shift <- predict_full_month/84
predict_per_shift


# -----------------------------------------------------------------------------------------------
# Task 8: Provide a 90% confidence interval for the average delta throughput per shift in May. 
# -----------------------------------------------------------------------------------------------


# Confidence Interval
predict(lm_database1, newdata = new, interval="confidence", level=0.9)

