# Description:
# This script performs an in-depth analysis of product data, including exploratory data analysis (EDA),
# data cleaning, and visualization. The goal is to uncover trends and actionable insights about pricing,
# branding, gender segmentation, and product attributes.


# Load Required Libraries
library(dplyr)
library(ggplot2)
library(stringr)

# Datasets:
# 1. products_catalog.csv: Contains basic product attributes such as ID, brand, and price.
# 2. product_details.csv: Includes additional details like product descriptions, colors, and image counts.

# Key Variables:
# - ID: Unique identifier for each product (used for merging datasets).
# - PrimaryColor: The dominant color of the product.
# - ProductBrand: Brand name of the product.
# - Price (INR): Price of the product in Indian Rupees.
# - NumImages: Number of images available for the product.
# - Description: Textual description of the product.

# Purpose of Analysis:
# - Understand pricing trends across products and brands.
# - Analyze gender-based segmentation and its impact on product attributes.
# - Identify key product features (color, description length) that influence pricing.
# - Provide actionable insights for inventory management and marketing strategies.

# Load Datasets
df1 = read.csv("products_catalog.csv")
df2 = read.csv("product_details.csv")

# Preview and Explore Datasets
head(df1, 3)       # Preview first three rows of df1
head(df2, 3)       # Preview first three rows of df2
str(df1)           # Check structure of df1
str(df2)           # Check structure of df2
summary(df1)       # Summarize df1
summary(df2)       # Summarize df2

# Merge Datasets
df = merge(x = df1, y = df2, by.x = "ID", by.y = "ProductID", 
            all.x = FALSE, all.y = FALSE)

# Check Structure of Merged Dataset
head(df, 5)          # Preview first 5 rows
dim(df)              # Dimensions of the merged dataset
colnames(df)         # Column names of the merged dataset
str(df)              # Structure of the dataset

# Data Cleaning
# Remove duplicates
df = unique(df)

# Check for missing values
colSums(is.na(df))

'' %in% df[['PrimaryColor']]
'' %in% df[['ProductName']]

# Fill missing values for PrimaryColor with 'Others'
df$PrimaryColor[df$PrimaryColor == "" | is.na(df$PrimaryColor)] <- "Others"

# reading few rows in each column
for (cols in colnames(df)){
  if ( class(df[[cols]]) == "character" ){
    print( df[ 1 : 10 , cols]  )
    cat("\n\n")
  }
}
# Remove leading/trailing spaces in character columns
df = df %>%   mutate_if(is.character, str_trim)

# Rename Columns
df = rename(df, Price = Price..INR.)


#Exploratory Data Analysis

# Univariate Analysis --> Loop on numerical cols
for (cols in colnames(df)){
  if ( ( class(df[[cols]]) == "numeric" ) | ( class(df[[cols]]) == "integer" ) ){
    print(paste0("Column Name -> ", cols), quote = FALSE)
    cat("\n")
    print( paste0( "Minimum Value => ", min(df[[cols]], na.rm = TRUE)  ) , quote = F)  
    print( paste0( "Maximum Value => ", max(df[[cols]], na.rm = TRUE)  ) , quote = F)
    print( paste0( "Mean Value => ", mean(df[[cols]], na.rm = TRUE)  ) , quote = F)
    print( paste0( "Median Value => ", median(df[[cols]], na.rm = TRUE)  ) , quote = F)
    print( paste0( "25Percentile Value => ", quantile(df[[cols]] , probs = 0.25, na.rm = TRUE)  ) , quote = F)
    print( paste0( "75Percentile Value => ", quantile(df[[cols]] , probs = 0.75, na.rm = TRUE)  ) , quote = F)
    print( paste0( "Variance Value => ", var(df[[cols]], na.rm = TRUE)  )  , quote = F)
    print( paste0( "Standard Deviation Value => ", sd(df[[cols]], na.rm = TRUE)  )  , quote = F)
    print( paste0( "Total Sum Value => ", sum(df[[cols]], na.rm = TRUE)  )  , quote = F)
    
    print( paste0(rep("-", each = 10)), quote = F)
    cat("\n\n")
  }
}


# Univariate Analysis --> Loop on categorical cols
for (cols in colnames(df)){
  if ( class(df[[cols]]) == "character"  ){
    print(paste0("Column Name -> ", cols), quote = FALSE)
    cat("\n")
    mode_value = names(which.max(table(df[[cols]])))
    print( paste0( "Mode Value => ",  mode_value)    , quote = F)  
    
    total_values = length(unique( (df[[cols]]) ) )
    print( paste0( "Total Distinct Values => ",  total_values)    , quote = F)  
    
    print( paste0(rep("-", each = 10)), quote = F)
    cat("\n\n")
  }
}


###Univariate Analysis --> numerical cols
##Price

ggplot(data = df) + 
  geom_histogram( mapping = aes(x = Price), 
                  fill = 'orange', color = 'blue' , bins = 20 ) + 
  xlab(" -- Price --") + ylab("-- Distribution --") + 
  labs(title = " Price Histogram ") + 
  theme_classic()

# Identify outliers using IQR
Q1 = quantile(df$Price, 0.25, na.rm = TRUE)
Q3 = quantile(df$Price, 0.75, na.rm = TRUE)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
# Flag outliers
df = df %>%
  mutate(Outlier = ifelse(Price < lower_bound | Price > upper_bound, TRUE, FALSE))
# Summary of outliers
outlier_summary = df %>%
  group_by(Outlier) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), 
            Count = n())
print(outlier_summary)
 

# Since there is a lot of discrepancy in Price,dividing it into 2 regions based on the graphical representation
# Region 1 --> Price < 2000
ggplot(data = df %>% filter(Price < 2000)  ) + 
  geom_histogram( mapping = aes(x = Price), 
                  fill = 'red', color = 'green' , bins = 20 ) + 
  xlab(" -- Price --") + ylab("-- Distribution --") + 
  labs(title = " Price Histogram ") + 
  theme_classic()


# Region 2 --> Price > 2000
ggplot(data = df %>% filter(Price > 2000)  ) + 
  geom_histogram( mapping = aes(x = Price), 
                  fill = 'purple', color = 'skyblue' , bins = 20 ) + 
  xlab(" -- Price --") + ylab("-- Distribution --") + 
  labs(title = " Price Histogram ") + 
  theme_classic()


df = df %>% mutate( PriceRange =
                case_when(Price <= 2000 ~ "Lower Range", 
                         (Price <= 10000 ) & (Price >  2000) ~ "Middle Range", 
                          Price > 10000  ~ "Upper Range")) 
ggplot(data = df) + 
  geom_histogram( mapping = aes(x = Price, fill = PriceRange),  bins = 30 ) + 
  xlab(" -- Price --") + ylab("-- Distribution --") + 
  labs(title = " Price Histogram ") + 
  theme_classic() + 
  scale_fill_manual(values = c('green', 'yellow', 'red'), 
                             c("Lower Range", "Middle Range", "Upper Range"))


## Distribution of NumImages
ggplot(df, aes(x = NumImages)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Distribution of Number of Images", 
       x = "Number of Images", y = "Count")



### Univariate Analysis --> Categorical cols
## PrimaryColor Distribution

sort( table( df[['PrimaryColor']])  )

df %>%
  count(PrimaryColor, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(PrimaryColor, -Frequency), y = Frequency, 
             fill = PrimaryColor)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3) +
  labs(title = "PrimaryColor Bar Chart", 
       x = "-- PrimaryColor --", 
       y = "-- Distribution --") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ProductBrand Distribution

ggplot(df %>% group_by(ProductBrand) 
      %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% head(10),
      aes(x = reorder(ProductBrand, -Count), y = Count, fill = ProductBrand)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Product Brands", x = "Brand", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


## ProductName Distribution

df %>%
  group_by(ProductName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10)

ggplot(df %>% group_by(ProductName) 
       %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% head(10),
       aes(x = reorder(ProductName, -Count), y = Count, fill = ProductName)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 ProductName", x = "Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Gender Distribution
ggplot(df, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  geom_text(stat = "count", 
            aes(label = after_stat(count), vjust = -0.5), size = 3) +
  labs(title = "Distribution of Products by Gender", 
       x = "-- Gender --", 
       y = "-- Distribution --") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###Bivariate Analysis

## Price vs NumImages
ggplot(df, aes(x = NumImages, y = Price)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Price vs Number of Images", 
       x = "Number of Images", y = "Price")


## ProductBrand and Price
df %>%
  group_by(ProductBrand) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(AveragePrice)) %>%
  top_n(10, AveragePrice) %>%
  ggplot(aes(x = reorder(ProductBrand, -AveragePrice), y = AveragePrice, 
             fill = ProductBrand)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Product Brands by Average Price", 
       x = "Product Brand", y = "Average Price (INR)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


##ProductName and Price
df %>%
  group_by(ProductName) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(AveragePrice)) %>%
  top_n(10, AveragePrice) %>%
  ggplot(aes(x = reorder(ProductName, -AveragePrice), y = AveragePrice, 
             fill = ProductName)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Product Names by Average Price", 
       x = "Product Name", y = "Average Price (INR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Price vs Gender
df %>%
  group_by(Gender) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Gender, -AveragePrice), y = AveragePrice, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Price by Gender",
       x = "Gender",
       y = "Average Price (INR)") +
  theme_minimal() +
  theme(legend.position = "none")


## Price vs PrimaryColor
df %>%
  group_by(PrimaryColor) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(PrimaryColor, AveragePrice), y = AveragePrice, fill = PrimaryColor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Price by Primary Color",
       x = "Primary Color",
       y = "Average Price (INR)") +
  theme_minimal() +
  theme(legend.position = "none")



### Multivariate Analysis
## Gender, PrimaryColor, and Price
df %>%
  group_by(Gender, PrimaryColor) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  ggplot(aes(x = Gender, y = Avg_Price, fill = PrimaryColor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price by Gender and Primary Color", 
       x = "Gender", y = "Average Price")

## Gender, NumImages, and Price
ggplot(df, aes(x = Gender, y = Price, color = NumImages)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Price by Gender and Number of Images", 
       x = "Gender", y = "Price")


# Adding Derived Variables:
# 1. AgeGroup:
#    - Purpose: To group products based on the target age demographic.
#    - Categories:
#      - Kids: Includes "Boys", "Girls", and "Unisex Kids".
#      - Adults: Includes "Men", "Women", and "Unisex".

df = df %>%
  mutate(AgeGroup = case_when(
    Gender %in% c("Boys", "Girls", "Unisex Kids") ~ "Kids",
    Gender %in% c("Men", "Women", "Unisex") ~ "Adults"
  ))

# 2. NewGender:
#    - Purpose: To simplify gender categories for aggregated analysis.
#    - Categories:
#      - Men: Includes "Men" and "Boys".
#      - Women: Includes "Women" and "Girls".
#      - Unisex: Includes "Unisex" and "Unisex Kids".

df = df %>%
  mutate(NewGender = case_when(
    Gender %in% c("Men", "Boys") ~ "Men",
    Gender %in% c("Women", "Girls") ~ "Women",
    Gender %in% c("Unisex", "Unisex Kids") ~ "Unisex"
  ))


## AgeGroup and Gender Distributions
age_gender_dist = df %>%
  group_by(AgeGroup, Gender) %>%
  summarise(Count = n())
ggplot(age_gender_dist, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "AgeGroup and Gender Distribution", x = "AgeGroup", y = "Count") +
  theme_minimal()

## Distribution of NewGender
ggplot(df, aes(x = NewGender, fill = NewGender)) +
  geom_bar() +
  labs(title = "Distribution of Products by NewGender", x = "NewGender", y = "Count") +
  theme_minimal()

# Analysis on NewGender
print(table(df$NewGender))
df %>%
  group_by(NewGender) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), 
            Median_Price = median(Price, na.rm = TRUE)) %>%
  arrange(desc(Avg_Price))

df %>%
  group_by(NewGender, ProductBrand) %>%
  summarise(Count = n()) %>%
  arrange(NewGender, desc(Count))


df %>%
  group_by(AgeGroup) %>%
  summarise(Count = n(), Avg_Price = mean(Price, na.rm = TRUE))

df %>%
  group_by(AgeGroup, ProductBrand) %>%
  summarise(Count = n()) %>%
  arrange(AgeGroup, desc(Count))

df %>%
  group_by(AgeGroup, PrimaryColor) %>%
  summarise(Count = n(), Avg_Price = mean(Price, na.rm = TRUE)) %>%
  arrange(AgeGroup, desc(Count))


# Primary Color Information in Descriptions and ProductName:
# Purpose:
# - To determine whether the primary color of a product is explicitly mentioned in its `Description` or `ProductName`.
# - This analysis helps identify if product descriptions and names emphasize key visual attributes like color, which may influence customer preferences and searchability.

# Check if PrimaryColor is mentioned in the Description
df = df %>%
  mutate(PrimaryColorinDes = mapply(grepl, PrimaryColor, Description))
# PrimaryColorinDes and summarize the counts
AttributeDescription_Insights <- df %>%
  group_by(NewGender, PrimaryColorinDes) %>%
  summarise(Count = n())
# Print the insights
print(AttributeDescription_Insights)


# Check if PrimaryColor is mentioned in the ProductName
df = df %>%
  mutate(ColorInProductName = mapply(grepl, PrimaryColor, ProductName))
#ColorInProductName and summarize the counts
AttributeName_Insights = df %>%
  group_by(NewGender, ColorInProductName) %>%
  summarise(Count = n())
# Print the insights
print(AttributeName_Insights)

# Analyze impact of PrimaryColor presence in Description and ProductName on Price
color_impact = df %>%
  group_by(ColorInProductName, PrimaryColorinDes) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), 
            MedianPrice = median(Price, na.rm = TRUE), 
            Count = n()) %>%
  arrange(desc(AveragePrice))
# Print results
print(color_impact)


# Popular Colors by Price Range
popular_colors = c("blue", "black", "red")
df %>%
  filter(PrimaryColor %in% popular_colors) %>%
  group_by(PrimaryColor, NewGender) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), Count = n()) %>%
  ggplot(aes(x = PrimaryColor, y = Avg_Price, fill = NewGender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Price Distribution for Popular Colors by Gender", 
       x = "Primary Color", y = "Average Price")

# Correlation Analysis
desc_price_correlation = cor(df$Descriptionlen, df$Price, use = "complete.obs")
cat("Correlation between Description Length and Price:", 
    desc_price_correlation, "\n")

correlation = cor(df$Descriptionlen, df$NumImages, use = "complete.obs")
cat("Correlation between Description Length and NumImages:", correlation, "\n")

# Segment data by AgeGroup and PriceRange
age_price_segment = df %>%
  group_by(AgeGroup, PriceRange) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), 
            Count = n()) %>%
  arrange(AgeGroup, desc(AveragePrice))
print(age_price_segment)


ggplot(age_price_segment, aes(x = AgeGroup, y = AveragePrice, fill = PriceRange)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price by AgeGroup and PriceRange",
       x = "Age Group",
       y = "Average Price (INR)") +
  theme_minimal()

# Price Distribution by Gender and Price Range
df %>%
  mutate(PriceRange = case_when(
    Price < 5000 ~ "Low",
    Price >= 5000 & Price < 15000 ~ "Medium",
    Price >= 15000 ~ "High"
  )) %>%
  group_by(Gender, PriceRange) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Gender, y = Count, fill = PriceRange)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Price Range Distribution by Gender", x = "Gender", y = "Count")

# Comprehensive Brand Analysis
df %>%
  group_by(AgeGroup, ProductBrand) %>%
  summarise(Count = n()) %>%
  arrange(AgeGroup, desc(Count)) %>%
  group_by(AgeGroup) %>%
  slice_head(n = 5)

df %>%
  group_by(ProductBrand) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), Count = n()) %>%
  arrange(desc(Avg_Price)) %>%
  head(10)

# Final Summary
cat("Dataset insights and segmentation completed.")










