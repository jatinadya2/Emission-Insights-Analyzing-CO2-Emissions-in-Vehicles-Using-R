# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(glmnet)
library(MASS)

# Load the dataset
data <- read.csv("CO2.csv")

# Structure of the dataset
str(data)

# View the first few rows of the dataset
head(data)

# Summary statistics
summary(data)

sum(is.na(data))

colSums(is.na(data))

# Plot for all continuous vars
ggplot(data, aes(x = CO2.Emissions.g.km.)) + 
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Frequency")

# Do this for all categorical vars to check the value counts
table(data$Make)

# Plotting the histogram
hist(data$Fuel.Consumption.Comb..mpg., 
     main = "Histogram of Data",  # Title of the histogram
     xlab = "Fuel Consumption",        # Label for the x-axis
     ylab = "Frequency",          # Label for the y-axis
     col = "lightgreen",          # Color of the bars
     border = "black")            # Color of the border of the bars

table(data$Vehicle.Class)


ggplot(data, aes(x = Vehicle.Class, y = CO2.Emissions.g.km.)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "CO2 Emissions by Vehicle Class", 
       x = "Vehicle Class", 
       y = "CO2 Emissions (g/km)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plotting the scatterplots for all continous vars with the DV
plot(data$CO2.Emissions.g.km., data$Fuel.Consumption.Comb..mpg., 
     main = "Scatterplot of CO2 Emissions vs Fuel Consumption",  # Title of the scatterplot
     xlab = "CO2 Emissions",          # Label for the x-axis
     ylab = "Fuel Consumption",       # Label for the y-axis
     pch = 19,                        # Type of point. 19 is solid circle
     col = "blue")                    # Color of the points

#train-trest-split
set.seed(123457)
train.prop <- 0.80
trnset <- sort(sample(1:nrow(data), ceiling(nrow(data)*train.prop)))
# create the training and test sets
train.set <- data[trnset, ]
test.set  <- data[-trnset, ]

# Null model
null_model <- lm('CO2.Emissions.g.km. ~ 1', data = train.set)
summary(null_model)

#raw model
rawmodel <- lm('CO2.Emissions.g.km.~. - Model', data = train.set)
summary(rawmodel)

# Diagnostics for raw model
par(mfrow=c(2,2))
plot(rawmodel)

# See how well data fits the reg line
library(ggplot2)
ggplot(train.set, aes(x=CO2.Emissions.g.km., y=predict(rawmodel, newdata = train.set))) +
  geom_point(color="blue") +
  geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
  ggtitle("Actual vs Predicted CO2 Emissions") +
  xlab("Actual CO2 Emissions (g/km)") +
  ylab("Predicted CO2 Emissions (g/km)") +
  theme_minimal()

# Check how strongly correlated these vars are
vif(rawmodel)

str(train.set)

# Select one var out of the 3. Fuel.Consumption.Comb..L.100.km. gives highest r2.
rawmodel1 <- lm('CO2.Emissions.g.km. ~ Make + Vehicle.Class + Engine.Size.L. + Cylinders + Transmission + Fuel.Type +Fuel.Consumption.Comb..L.100.km.', data = train.set)
summary(rawmodel1)

# Diagnostics for rawmodel1
par(mfrow=c(2,2))
plot(rawmodel1)

# See how well the data fits the reg line
if(requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot(train.set, aes(x=CO2.Emissions.g.km., y=predict(rawmodel1, newdata = train.set))) +
    geom_point(color="blue", size=3) +
    geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
    ggtitle("Comparison of Actual vs Predicted CO2 Emissions") +
    xlab("Actual CO2 Emissions (g/km)") +
    ylab("Predicted CO2 Emissions (g/km)") +
    theme_minimal()
}

# Identify and remove outliers using residual method and fit the same model again
extpts <- which(abs(residuals(rawmodel1)) > 3*sd(residuals(rawmodel1)))

data.train.2 <- train.set[-extpts,]
clean_mod <- lm('CO2.Emissions.g.km. ~ Make + Vehicle.Class + Engine.Size.L. + Cylinders + Transmission + Fuel.Type +Fuel.Consumption.Comb..L.100.km.', data = data.train.2)
summary(clean_mod)

# Diagnostics for clean model
par(mfrow=c(2,2))
plot(clean_mod)

if(requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot(data.train.2, aes(x=CO2.Emissions.g.km., y=predict(clean_mod, newdata = data.train.2))) +
    geom_point(color="blue", size=3) +
    geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
    ggtitle("Actual vs Predicted CO2 Emissions") +
    xlab("Actual CO2 Emissions (g/km)") +
    ylab("Predicted CO2 Emissions (g/km)") +
    theme_minimal()
}

# Clean the data again
extpts <- which(abs(residuals(clean_mod)) > 3*sd(residuals(clean_mod)))

data.train.3 <- data.train.2[-extpts,]
clean_mod2 <- lm('CO2.Emissions.g.km. ~ Make + Vehicle.Class + Engine.Size.L. + Cylinders + Transmission + Fuel.Type +Fuel.Consumption.Comb..L.100.km.', data = data.train.3)
summary(clean_mod2)

# Error is more ND than the previous model
par(mfrow=c(2,2))
plot(clean_mod2)

if(requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot(data.train.3, aes(x=CO2.Emissions.g.km., y=predict(clean_mod2, newdata = data.train.3))) +
    geom_point(color="blue", size=3) +
    geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
    ggtitle("Actual vs Predicted CO2 Emissions") +
    xlab("Actual CO2 Emissions (g/km)") +
    ylab("Predicted CO2 Emissions (g/km)") +
    theme_minimal()
}

# See how well the reg line fits on test data
plot(test.set$CO2.Emissions.g.km., predict(clean_mod2, newdata = test.set), 
     col="grey33", cex=0.3, xlab="Actual", ylab="Predicted")
abline(0,1)




