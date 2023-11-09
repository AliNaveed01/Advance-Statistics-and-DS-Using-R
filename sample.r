# we will first be installing the required packages

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("gridExtra")

library(gridExtra)
library(psych)
library(ggplot2)
library(readxl)
library(dplyr)

#---------------------------------------------------------------------

# reading the ames.xlsx file
ames <- read_excel("ames.xlsx")
print(head(ames))



# ----------------------------------------------------------------------
# --------------------Descriptive Statistics of data--------------------
col <- colnames(ames)
print(length(colnames(ames)))   # 78 columns in the data
num_col <- colnames(ames)[sapply(ames, is.numeric)]

print(summary(ames))


#--------------------------------------------------------------------------
#-------------------Ensuring Data quality by preprocessing --------------

#1) Missing value checking
#print(sum(is.na(ames)))
variables_with_missing_values <- colnames(ames)[colSums(is.na(ames)) > 0.5 * nrow(ames)] # removing columns with more missing values
ames <- ames[, !names(ames) %in% variables_with_missing_values]
#now check for missing again, it will be reduced since the numer of columns will be reduced too
print(sum(is.na(ames)))

# checkin duplicates
duplicate_rows <- ames[duplicated(ames), ]
print(length(duplicate_rows))

#4) recheck the structure of the dataset
structure <- str(ames)
print(structure)

#5) "frontage" column
missing_values <- sum(is.na(ames$frontage))
# Impute missing values with the median value
median_frontage <- median(ames$frontage, na.rm = TRUE)
ames$frontage[is.na(ames$frontage)] <- median_frontage
#print(sum(is.na(ames$frontage)))

#----------------------------------------------------------------------

#Pick 5 independent variables that you believe will be related to the dependent variable.
new_ames <- ames%>%
  select(ID,stories, house_quality, rooms_tot, bedroom, garage_cars, zone, sale_price)
# One-hot encode categorical variables
new_ames <- new_ames %>%
  mutate(zone = as.numeric(factor(zone)),
         stories = as.numeric(factor(stories)))
str(new_ames)

#------------------------------------------------------------------------------------------------

#4  Visualizations

plot1<-ggplot(new_ames, aes(x = house_quality, y = sale_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Histogram: House Quality vs. Sale Price", x = "House Quality", y = "Sale Price")

plot2<-ggplot(new_ames, aes(x = rooms_tot, y = sale_price)) +
  geom_point(color = "coral") +
  labs(title = "Scatterplot: Total Rooms vs. Sale Price", x = "Total Rooms", y = "Sale Price")


plot3<-ggplot(new_ames, aes(x = factor(bedroom), y = sale_price)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Bar Chart: Bedrooms vs. Sale Price", x = "Bedrooms", y = "Sale Price")


plot4<-ggplot(new_ames, aes(x = garage_cars, y = sale_price)) +
  geom_point(color = "purple") +
  labs(title = "Scatterplot: Garage Cars vs. Sale Price", x = "Garage Cars", y = "Sale Price")


plot5<-ggplot(new_ames, aes(x = garage_cars, y = sale_price, size = sale_price)) +
  geom_point(alpha = 0.7, color = "#ff0000") +
  labs(title = "Bubble Plot: Garage Cars vs. Sale Price", x = "Garage Cars", y = "Sale Price", size = "Sale Price")


plot6<-ggplot(new_ames, aes(x = house_quality, y = sale_price)) +
  geom_line(color = "blue") +
  labs(title = "Line Plot: House Quality vs. Sale Price", x = "House Quality", y = "Sale Price")


grid.arrange(plot1, plot2, plot3, plot4, plot5,plot6, ncol = 3)

#-----------------------------------------------------------------------------------------
#--------------------#5 Calculate appropriate measures of association ---------------------

#Pearson correlation matrix
cor_matrix <- cor(new_ames[, c("sale_price", "house_quality", "rooms_tot", "bedroom", "garage_cars")])
print(cor_matrix)

#Spearman rank correlation matrix
cor_matrix_spearman <- cor(new_ames[, c("sale_price", "house_quality", "rooms_tot", "bedroom", "garage_cars")], method = "spearman")
print(cor_matrix_spearman)

#----------------------------------------------------------------------------------------
#-------------------------------Data Splitting------------------------------

set.seed(123) # Put seed according to your roll number 
train_proportion <- 0.8
train_size <- round(nrow(new_ames) * train_proportion)
random_index <- sample(1:nrow(new_ames), nrow(new_ames))
train_data <- new_ames[random_index[1:train_size], ]
test_data <- new_ames[random_index[(train_size + 1):nrow(new_ames)], ]
print(nrow(train_data))
print(nrow(test_data))

#----------------------------------------------------------------------------------------
# -------------------Simple Linear Regression----------------------------------
model1 <- lm(sale_price ~ house_quality, data = train_data)
print(summary(model1)) # print the summary of the model you want.
test_data$predicted_sale_price <- predict(model1, test_data)
print(cor(test_data$sale_price, test_data$predicted_sale_price)^2) #0.65


model2 <- lm(sale_price ~ rooms_tot, data = train_data)
#print(summary(model2))
test_data$predicted_sale_price2 <- predict(model2, test_data)
print(cor(test_data$sale_price, test_data$predicted_sale_price2)^2) #0.39


model3 <- lm(sale_price ~ rooms_tot, data = train_data)
summary_model3 <- summary(model3)
r_squared <- summary_model3$r.squared
test_data$predicted_sale_price3 <- predict(model3, newdata = test_data)
r_squared_test <- 1 - mean((test_data$sale_price - test_data$predicted_sale_price3)^2) / var(test_data$sale_price)
cat("R-squared value for test data:", r_squared_test, "\n") # higher r_squared_test = better accuracy


model4 <- lm(sale_price ~ garage_cars, data = train_data)
#print(summary(model4))
test_data$predicted_sale_price4 <- predict(model4, test_data)
print(cor(test_data$sale_price, test_data$predicted_sale_price4)^2) #0.41

# -------------------Multiple Regression----------------------------------

model5 <- lm(sale_price ~ house_quality + rooms_tot + bedroom + garage_cars, data = train_data)
print(summary(model5))
test_data$predicted_sale_price5 <- predict(model5, test_data)
print(cor(test_data$sale_price, test_data$predicted_sale_price5)^2) #0.74


model6 <- lm(sale_price ~ house_quality + rooms_tot + bedroom, data = train_data)
#print(summary(model6))
test_data$predicted_sale_price6 <- predict(model6, test_data)
print(cor(test_data$sale_price, test_data$predicted_sale_price6)^2) #0.70


model7 <- lm(sale_price ~ house_quality + garage_cars, data = train_data)
#print(summary(model7))
test_data$predicted_sale_price7 <- predict(model7, test_data)

print(cor(test_data$sale_price, test_data$predicted_sale_price7)^2) #0.72


#--------result: Multiple Regression gave better output than simple linear regression--------------------------------
