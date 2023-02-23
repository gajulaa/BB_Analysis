# CLEANING and EDA: WITH COMMENTS=======================================================

# the packages used for cleaning in this project
library(tidyverse)
library(readr)
library(formattable)
library(finalfit)
library(mice) # Multivariate Imputations By Chained Equations Package. Used for
# multiple imputation.

BB <- read_csv("BigBasket Products.csv")
View(BB)
attach(BB) # makes it easier to reference column names
summary(BB) # lists summary stats of data location & NA count

# verifying max and min values of market and sale prices
subset(BB, BB$sale_price == 2.45)
subset(BB, BB$sale_price == 12500.00)
subset(BB, BB$market_price == 3.0)
# no abnormalities detected

glimpse(BB) # glimpse of how data is recorded.

# detected inconsistent recording in sale price and market price columns.
# Sale price records values with 2 decimal points. Market price data will also
# be modified to reflect the same.

formattable(BB$market_price, format = "f", digits = 2)
# formatting market price to have 2 decimal points.

dupe_record <- duplicated(BB$index)
sum(dupe_record) # no duplicates exist
rm(dupe_record)

# confirming presence of duplicate data using a different approach

dupe_table <- duplicated(as.list(BB))
colnames(BB[dupe_table])
View(dupe_table)
rm(dupe_table)

# in the above chunk, df was converted to a list and passed through a base
# function duplicated() as it utilizes vectors. Colnames() lists columns that
# have duplicated values. No duplicates exits

colSums(is.na(BB)) # Lists NA value count in both numeric and character columns

# Rating has quite a bit of NAs. 8626 NA values to be exact. That amounts to a
# rounded estimate of 31.3 percent of all values in the column. Which inevitably
# affects all the rows in the df.

# NA count -> product: 1; brand: 1; rating: 8626, description: 115


BB[!complete.cases(BB), ] # lists the rows that have NA values

na_count_rating <- aggregate(rating ~ category,
                             BB,
                             function(rating) {
                               sum(is.na(rating))
                             },
                             na.action = NULL
)
rm(na_count_rating)
# the above function lists counts of NA values in the Rating column grouped by
# values in the Category column.

# Beauty and Hygiene have the highest number of NA values (2403) in the Rating
# column. While Baby Care has the least amount (114). It raises the question of
# how people are rating the products. To be more precise, Who are buying the
# products? parents? People who are part of a family? More data is needed to
# conduct further analysis in this aspect.

# investigating the rows in other columns which had NA values.

temp_tb_1 <- subset(BB, is.na(product)) # filtering the row that had NA in
# Product column

view(temp_tb_1) # NA value was recorded for a product
# in the Coffee sub-category.
rm(temp_tb_1)

temp_tb <- subset(BB, is.na(brand)) # filtering the row that had NA in Brand.

view(temp_tb) # NA values recorded in brand, rating, and
# description column. Type and Product columns reveal that it is a food package
# item that falls under same type as aluminium foil and cling wrap
rm(temp_tb)

# Further investigating missing data in Rating Column

BB %>%
  missing_plot() # It plots the missing values along the row numbers on the
# x axis. each line represents an NA value. Blanks indicate absence of NAs.

# However, this method is only useful to look at 'missingness' in numeric
# columns. Doesn't give the full picture.

# creating dummy variables (1 = missing, 0 = not missing) of the Rating and
# Description column. Running tests will confirm the pattern of missingness.

BB_dupe <- BB # duplicating BB dataset to prevent hardcoding of values.

BB_dupe$rating_na <- ifelse(is.na(BB_dupe$rating), 1, 0)
BB_dupe$desc_na <- ifelse(is.na(BB_dupe$description), 1, 0)

# created and added the dummy variables(rating_na, desc_na) into the duplicate
# data frame.

write.csv(BB_dupe, file = "Modified BigBasket.csv")
# saving the duplicate table with added columns as a csv file for further 
# reference and documentation. 

# commencing T- tests on continuous and Chi-squared tests on categorical
# variables to determine missingness pattern.

# Comparing rating_na against numeric columns
t.test(BB_dupe$sale_price ~ BB_dupe$rating_na, var.equal = TRUE)
# not significant

t.test(BB_dupe$market_price ~ BB_dupe$rating_na, var.equal = TRUE)
# not significant

# comparing desc_na against numeric columns
t.test(BB_dupe$sale_price ~ BB_dupe$desc_na, var.equal = TRUE)
# significant

t.test(BB_dupe$market_price ~ BB_dupe$desc_na, var.equal = TRUE)
# significant

chisq.test(table(BB_dupe$category, BB_dupe$rating_na))
# not significant

chisq.test(table(BB_dupe$sub_category, BB_dupe$rating_na))
# not significant

chisq.test(table(BB_dupe$brand, BB_dupe$rating_na))
# not significant

# Significance in t.tests indicate MAR (Missing at Random). It is possible that
# missingness of data is related to pricing of the product.

# Now that Missing pattern is established, missing data needs to be filled in.
# A common procedure is Multiple Imputation. The technique relies on the MAR
# assumption and does not introduce bias too.

# confirming MAR pattern in the data

tmp_tb <- subset(BB_dupe, select = c(sale_price, market_price, rating))
md.pattern(tmp_tb) # filtered the required columns and passed them through a
# md.pattern() function. It creates a matrix of the df to visualize missingness

imputed_data <- mice(tmp_tb, m = 5, maxit = 60, method = "pmm", seed = 500)
summary(imputed_data)
# using the same filtered data, ran it through a mice function to impute 5
# different datasets with 60 iterations, using a pmm method (predictive
# mean matching)

Impute_graph <- stripplot(imputed_data) # investigating differences in each
# imputation. no major differences detected between observed and imputed values.
rm(Impute_graph)

completeData <- complete(imputed_data, 2)
view(completeData)
rm(tmp_tb)
rm(imputed_data)
# Assigning second imputed data frame to the environment to view it
# (completeData).

BB_dupe$rating <- completeData$rating # replaced rating column in BB_dupe with
# the same column in completeData df.
summary(BB_dupe)
colSums(is.na(BB_dupe))
# verifying if data parsed over properly.
rm(completeData)

BB_final <- subset(BB_dupe, select = -c(description))
summary(BB_final)

BB_final <- BB_final[complete.cases(BB_final), ]
rm(BB_dupe)
colSums(is.na(BB_final))
# NA values removed, irrelevant column(s) (description) dropped. Assigned to a
# clean dataset named BB_final.

write.csv(BB_final, file = "BigBasketFinal.csv")
rm(BB)
# exported the Dataset for further use.

# final verification. will verify how average of overall combined rating differs
# from imputed and original dataset
temp <- BB_final %>%
  select(category, rating) %>%
  group_by(category) %>%
  summarise(Count = n(), avg_rating = mean(rating))
view(temp)

temp2 <- BB %>%
  select(category, rating) %>%
  group_by(category) %>%
  summarise(Count = n(), avg_rating = mean(rating, na.rm = TRUE))
# there seem to be slight changes in data. Will be better to visualize it

# visualizing the data for average overall rating across categories

ggplot(temp, aes(x = category, y = avg_rating)) +
  geom_bar(stat = "identity", aes(fill = avg_rating)) +
  scale_fill_distiller(palette = "Blues") +
  guides(x = guide_axis(angle = 45)) +
  labs(
    title =  "Average of Combined Overall Rating Across Categories",
    x = "Categories",
    y = "Average Rating"
  )

ggplot(temp2, aes(x = category, y = avg_rating)) +
  geom_bar(stat = "identity", aes(fill = avg_rating)) +
  scale_fill_distiller(palette = "Blues") +
  guides(x = guide_axis(angle = 45)) +
  labs(
    title = "Average of Combined Overall Rating Across Categories",
    x = "Categories",
    y = "Average Rating"
  )
rm(temp, temp2)

# visualized the differences in discrete values of combined overall product
# average. The color distillation provides a visual cue of how data changed post
# imputation. At a first glance, the values seem to have gone down the
# gradient slightly. There is always a degree of uncertainty for imputation.

# Missing bars displayed in the graph for the original dataset could indicate
# that satisfaction levels of the products in those categories depended on how
# they were stored and used by the customers post purchase. Satisfaction did not
# depend on the products themselves as these were raw items with very low
# shelf life.