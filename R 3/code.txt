# Firstly, we aer going to install all the libraries which are necessary for this assignment
# for data exploration, and then we will load the data and perform pre-processing on it.

packages <- c("readxl", "dplyr", "ggplot2", "gridExtra", "caret", "lmtest", "ggridges")
install.packages(packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)



# Now, we will be reading the ames.xlsx data
data <- read_excel("ames.xlsx")
print(data)


#Now, lets look at the Data in more detail

print(head(data))
print("************************************************************************\n")
print(tail(data))

# print the number of rows and columns in the data 
print(str(data))

print("************************************************************************\n")
cat("Total Number of rows and columns" , data %>% dim())

# print the summary of the data
print(summary(data))

#######################################################################################

# Now we will be doing some preprocessing on the data
# print the highest value and the lowest value in the sales price column
print("************************************************************************\n")
print("Highest value in the sales price column")
print(max(data$sale_price))
print("Lowest value in the sales price column")
print(min(data$sale_price))

# print the column which contains null values 
print("************************************************************************\n")
print(colnames(data)[colSums(is.na(data)) > 0])
# print num of columns having null values
print("************************************************************************\n")
print(length(colnames(data)[colSums(is.na(data)) > 0]))

# Define a function to calculate mode
getMode <- function(x) {
  uniqX <- unique(x)
  uniqX[which.max(tabulate(match(x, uniqX)))]
}

# Impute all columns with mean values (numeric columns) or mode (categorical columns)
impute_data <- function(data) {
  data %>%
    mutate_all(
      funs(
        if (is.numeric(.)) {
          ifelse(is.na(.), mean(., na.rm = TRUE), .)
        } else {
          ifelse(is.na(.), getMode(.), .)
        }
      )
    )
}

# now call the function
data <- impute_data(data)

# now check again the number of columns having null values
print("************************************************************************\n")
print(length(colnames(data)[colSums(is.na(data)) > 0])) # the number is reduced from 25 to 5

# check which of the columns still have null values
print("************************************************************************\n")
print(colnames(data)[colSums(is.na(data)) > 0])

# drop these columns
data <- data[, !names(data) %in% colnames(data)[colSums(is.na(data)) > 0]]

# now check again the number of columns having null values
print("************************************************************************\n")
print(length(colnames(data)[colSums(is.na(data)) > 0])) # the number is reduced from 5 to 0.

print("************************************************************************\n")
# remove duplicates from the data
data <- data[!duplicated(data), ]
print(sum(duplicated(data))) # 0 rows are duplicated

# create a new feature "num_baths" which is sum of bsmt_full_bath, bsmt_half_bath and full_bath and half_bath
data <- data %>%
  mutate(num_baths = bsmt_full_bath + bsmt_half_bath + full_bath + half_bath)

print(str(data))

# Now we will choose atleast 5 variables which we are going to use in further processing and building of the data for 
# Regression analysis.
# we will be choosing ID, d_type, road, shape, building, house_condition, house_quality,kitchen, rooms_tot, garage_area, year_sold, sale_price
# we will be dropping all the other columns from the data
data_updated <- data %>%
  select(ID, d_type, road, shape, building,bedroom, num_baths, house_condition, house_quality, kitchen, rooms_tot, garage_area,month_sold, year_sold, sale_price)

# check number of columns which are numeric 
print("************************************************************************\n")
print(length(colnames(data_updated)[sapply(data_updated, is.numeric)]))
# now check the number of columns which are not numeric
print("************************************************************************\n")
print(length(colnames(data_updated)[!sapply(data_updated, is.numeric)]))
print(colnames(data_updated)[!sapply(data_updated, is.numeric)])

# convert road, shape, building columns using one-hot-encoded
data_updated <- data_updated %>%
  mutate(road = as.numeric(factor(road)), shape = as.numeric(factor(shape)), building = as.numeric(factor(building)))


print(str(data_updated))

# Now we will make 5 different visualizations containing relations between the data
# 1) bedrooms vs sale bar graph
vis1 <- ggplot(data_updated, aes(x = bedroom, y = sale_price)) +
  geom_bar(stat = "identity", fill = "#060af6") +
  labs(title = "Bar Chart", x = "Bedrooms", y = "Sales Price")
ggsave("vis1.png", vis1, width = 15, height = 15, units = "cm")

#2) num_baths vs sales price histogram
vis2 <- ggplot(data_updated, aes(x = num_baths, y = sale_price)) +
  geom_histogram(stat = "identity", fill = "#f2f606") +
  labs(title = "Histogram", x = "Number of Baths", y = "Sales Price")
ggsave("vis2.png", vis2, width = 15, height = 15, units = "cm")

#3) garage area vs sale line graph
vis3 <- ggplot(data_updated, aes(x = garage_area, y = sale_price)) +
  geom_line(color = "#2af606") +
  labs(title = "Line Graph", x = "Garage Area", y = "Sales Price")
ggsave("vis3.png", vis3, width = 15, height = 15, units = "cm")

#4) month sold vs sale price
data_updated$month_sold <- factor(data_updated$month_sold, levels = 1:12)
vis4 <- ggplot(data_updated, aes(x = month_sold, y = sale_price)) +
  geom_point(color = "#e606f6") +
  labs(title = "Scatterplot", x = "Month Sold", y = "Sales Price") +
  scale_x_discrete(labels = month.abb)  # To display month abbreviations on the x-axis

ggsave("vis4.png", vis4, width = 15, height = 15, units = "cm")

#5) number of houses sold in each year
vis5 <- ggplot(data_updated, aes(x = year_sold, fill = factor(year_sold))) +
  geom_bar() +
  labs(title = "Bar Chart", x = "Year Sold", y = "Number of Houses Sold")
ggsave("vis5.png", vis5, width = 15, height = 15, units = "cm")

#6) Avg price of house sold in each year
vis6 <- ggplot(data_updated, aes(x = year_sold, y = sale_price, fill = factor(year_sold))) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart", x = "Year Sold", y = "Average Price of Houses Sold")
ggsave("vis6.png", vis6, width = 15, height = 15, units = "cm")

#7) House quaality vs sales price
vis7 <- ggplot(data_updated, aes(x = house_quality, y = sale_price)) +
  geom_point(color = "#e606f6") +
  labs(title = "Scatterplot", x = "House Quality", y = "Sales Price")
ggsave("vis7.png", vis7, width = 15, height = 15, units = "cm")


# Now we have to do several measure of association methods
#1) Kendall rank correlation matrix
kendall_correlation <- cor(data_updated[, c("sale_price", "house_quality", "rooms_tot", "bedroom", "shape", "garage_area", "year_sold")], method = "kendall")
print(kendall_correlation)

print("************************************************************************\n")

#2) Correlation
pearson_correlation <- cor(data_updated[, c("sale_price", "house_quality", "rooms_tot", "bedroom", "shape", "garage_area", "year_sold")], method = "pearson")
print(pearson_correlation)

print("************************************************************************\n")

#3) durbin watson test
dwtest <- dwtest(lm(sale_price ~ house_quality, data = data_updated))
print(dwtest)

print("************************************************************************\n")

#Split the data into training and testing sets
set.seed(55) # Set the seed for reproducibility
train_indices <- createDataPartition(data_updated$sale_price, p = 0.90, list = FALSE)
Training_Set <- data_updated[train_indices, ]
Test_Set <- data_updated[-train_indices, ]

print(nrow(Training_Set))
print(nrow(Test_Set))

# Now we will create Linear Regression Models for the data

#1) house quality and sales price
model1 <- lm(sale_price ~ house_quality, data = Training_Set)
# Accuracy of the model
print(cor(Test_Set$sale_price, predict(model1, Test_Set))^2 * (100)) 
# rsquared value
print(summary(model1)$r.squared) 

print("--------------------------------")

#2) garage area and sales price
model2 <- lm(sale_price ~ garage_area, data = Training_Set)
# Accuracy of the model
print(cor(Test_Set$sale_price, predict(model2, Test_Set))^2 * (100)) 
# rsquared value
print(summary(model2)$r.squared) 

print("--------------------------------")

#3) num_baths and sales price
model3 <- lm(sale_price ~ num_baths, data = Training_Set)
# Accuracy of the model
print(cor(Test_Set$sale_price, predict(model3, Test_Set))^2 * (100)) 
# rsquared value
print(summary(model3)$r.squared) 

print("--------------------------------")


# Now we will do Multiple linear regression using all the variables

#1) house quality, bedroom and garage area
model4 <- lm(sale_price ~ house_quality + bedroom + garage_area + d_type, data = Training_Set)
# Accuracy of the model
print(cor(Test_Set$sale_price, predict(model4, Test_Set))^2 * (100)) 
# r squared value
print(summary(model4)$r.squared)

print("--------------------------------")

#2) house quality, bedroom, garage area, num_baths
model5 <- lm(sale_price ~ house_quality + bedroom + garage_area + num_baths + shape + d_type + house_condition, data = Training_Set)
# Accuracy of the model
print(cor(Test_Set$sale_price, predict(model5, Test_Set))^2 * (100)) 
# r squared value
print(summary(model5)$r.squared)

print("--------------------------------")