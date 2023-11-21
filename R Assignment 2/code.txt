#Step1: Package Intallation

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("caret")
install.packages("lmtest")
install.packages("ggridges")


library(ggridges)
library(lmtest)
library(caret)
library(gridExtra)
library(ggplot2)
library(readxl)
library(dplyr)

#Step2: Load the dataset

# reading the ames.xlsx file
file <- read_excel("ames.xlsx")
print(head(file))


# Step 3) Pre-processing


print(nrow(file)) # total rows
print(ncol(file)) # total columns
print(summary(file)) # total DESCRIPTIVE SUMMARY.
print("----------------------------------------------------------------")
print(colnames(file)[sapply(file, is.numeric)]) # numeric columns
print("----------------------------------------------------------------")
print(colnames(file)[sapply(file, is.character)]) # character columns

 
# null values check
print(sum(is.na(file)))
# since "Alley", pool_qual , fence_qual and "features" column is mostly Null, we will remove 
# it since it is affecting other rows and making it null.
# print the columns with 50% null values()
missn_Val_Col <- colnames(file)[colSums(is.na(file)) > 0.5 * nrow(file)]
print(missn_Val_Col)
file <- file[, !names(file) %in% missn_Val_Col]


print(sum(is.na(file)))
print(nrow(file)) # total rows
print(ncol(file)) # total columns


# remove duplicate rows from the database
file <- file[!duplicated(file), ]

# Example: Identify and remove outliers in the 'sale_price' variable using IQR
Q1 <- quantile(file$sale_price, 0.25)
Q3 <- quantile(file$sale_price, 0.75)
IQR <- Q3 - Q1

# Remove outliers
file <- file[!(file$sale_price < Q1 - 1.5 * IQR | file$sale_price > Q3 + 1.5 * IQR), ]
# print the name of the columns which still have null values
print(colnames(file)[colSums(is.na(file)) > 0]) # only frontage, Garage_Area is the one with importance > 0
#impute frontage and garage_ area column with mean
file$frontage[is.na(file$frontage)] <- mean(file$frontage, na.rm = TRUE)
file$garage_area[is.na(file$garage_area)] <- mean(file$garage_area, na.rm = TRUE)

# Feature Engineering
#A feature called lawn of the area
file$lawn <- file$lot_area + file$garage_area * file$frontage


# Step 4: Choosing Important Variables for further analysis, processing and model Building

file_updated <- file%>%
  select(ID,d_type,stories, house_quality, shape, rooms_tot, bedroom, house_condition, garage_area, frontage, lawn, year_built, sale_price)

# describe the data
print(summary(file_updated))

# Convert the categorical columns into numeric columns for further use.
file_updated <- file_updated %>% mutate(stories = as.numeric(factor(stories)),shape = as.numeric(factor(shape)))

print(str(file_updated))


# Step 5: Visualizations
p1 <- ggplot(file_updated, aes(x = stories, y = sale_price)) +geom_bar(stat = "identity", fill = "#f60606") +labs(title = "Bar Chart", x = "Stories", y = "Sales Price")
ggsave("p1.png", p1, width = 15, height = 15, units = "cm")

p2 <- ggplot(file_updated, aes(x = garage_area, y = sale_price)) +geom_density_2d() +labs(title = "Density Plot: Garage Area vs. Sale Price", x = "Garage Area", y = "Sale Price")
ggsave("p2.png", p2, width = 15, height = 15, units = "cm")


p3 <- ggplot(file_updated, aes(x = sale_price, y = stories, fill = factor(stories))) +geom_density_ridges() +labs(title = "Ridgeline Plot: Sale Price Distribution Across Stories", x = "Sale Price", y = "Stories")
ggsave("p3.png", p3, width = 15, height = 15, units = "cm")

# Scatterplot Matrix of all the variables
p4 <- ggplot(file_updated, aes(x = frontage, y = lawn, color = factor(stories))) +geom_point() +labs(title = "Scatterplot Matrix", x = "Frontage", y = "Lawn")
ggsave("p4.png", p4, width = 15, height = 15, units = "cm")

# create area plot for year and sale price
p5 <- ggplot(file_updated, aes(x = year_built, y = sale_price)) +geom_area(fill = "lightblue") +labs(title = "Area Plot: Year Built vs. Sale Price", x = "Year Built", y = "Sale Price")
ggsave("p5.png", p5, width = 15, height = 15, units = "cm")

# Create pie chart for house quality
p6 <- ggplot(file_updated, aes(x = "", fill = factor(house_quality))) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart: House Quality", x = "", y = "") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

ggsave("p6.png", p6, width = 15, height = 15, units = "cm")



# Step 6: Measures of Association
# spearman rank correlation matrix
Spearman_Correlation <- cor(file_updated[, c("sale_price", "house_quality", "rooms_tot", "bedroom", "shape", "frontage","year_built")], method = "spearman")
print(Spearman_Correlation)

# third type of correlation matrix is the kendall rank correlation matrix
Kendall_Correlation <- cor(file_updated[, c("sale_price", "house_quality", "rooms_tot", "bedroom", "shape", "frontage","year_built")], method = "kendall")
print(Kendall_Correlation)


# Step 7: Split the data into training and testing sets
set.seed(62) # Put seed according to your roll number
CutOff <- 0.85
T_Size <- round(nrow(file_updated) * CutOff)
random_index <- sample(1:nrow(file_updated), nrow(file_updated))
Training_Set <- file_updated[random_index[1:T_Size], ]
Test_Set <- file_updated[random_index[(T_Size + 1):nrow(file_updated)], ]
print(nrow(Training_Set))
print(nrow(Test_Set))

# # Step 8: Model Building
# # Simple Linear Regression Model
# # Model between House condition and Sale Price
LinearM1 <- lm(sale_price ~ house_quality, data = Training_Set)
print(summary(LinearM1)) # print the summary of the model

# fit and predict the model
Test_Set$predicted_sale_price <- predict(LinearM1, Test_Set)

# print r squared value
summary_LinearM1 <- summary(LinearM1)
r_squared <- summary_LinearM1$r.squared
Test_Set$predicted_sale_price <- predict(LinearM1, newdata = Test_Set)
r_squared_test <- 1 - mean((Test_Set$sale_price - Test_Set$predicted_sale_price)^2) / var(Test_Set$sale_price)
print("R_squared value for test data:")
print(r_squared_test)
# print the accuracy of the model
print(cor(Test_Set$sale_price, Test_Set$predicted_sale_price)^2 * (100)) #0.65

# fit the regression line to the scatterplot
lmplot1 <- ggplot(Training_Set, aes(x = house_quality, y = sale_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#ff00c8") +
  labs(title = "Scatterplot: House Quality vs. Sale Price", x = "House Quality", y = "Sale Price")
ggsave("lmplot1.png", lmplot1, width = 15, height = 15, units = "cm")


# Multiple Linear Regression Models 
# Model between House quality, bedroom and garage area
LinearM3 <- lm(sale_price ~ house_quality + bedroom + garage_area + rooms_tot + year_built, data = Training_Set)
print(summary(LinearM3)) 

# fit and predict the model
Test_Set$predicted_sale_price3 <- predict(LinearM3, Test_Set)

# print r squared value
summary_LinearM3 <- summary(LinearM3)
r_squared <- summary_LinearM3$r.squared
Test_Set$predicted_sale_price3 <- predict(LinearM3, newdata = Test_Set)
r_squared_test <- 1 - mean((Test_Set$sale_price - Test_Set$predicted_sale_price3)^2) / var(Test_Set$sale_price)

print("Rsquared value for the test data : ")
print(r_squared_test)

# print the accuracy of the model
print(cor(Test_Set$sale_price, Test_Set$predicted_sale_price3)^2 * (100)) #0.76

# Plot the actual data points
plot <- ggplot(data = Test_Set, aes(x = house_quality, y = sale_price)) +
  geom_point(color = "blue") +
  labs(title = "Actual Data with Model Line", x = "House Quality", y = "Sale Price")
# Add the predicted values from the model as a line
plot <- plot + geom_line(data = Test_Set, aes(x = house_quality, y = predicted_sale_price3), color = "red")
# Add the R-squared value to the plot
plot <- plot + annotate("text", x = 5, y = 500000, label = paste("R-squared = ", round(r_squared_test, 2)))
ggsave("plot.png", plot, width = 15, height = 15, units = "cm")

