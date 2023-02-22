# GENERAL INFORMATION ABOUT THE DATASET------------------

# R version: 4.1.2
# Packages are up-to date as of July 2nd, 2022
# Initial Dataset: BigBasket Products.csv
# Dataset after cleaning: BigBasketFinal.csv


# Data from BigBasket. An online grocery store and one of the biggest in India.
# Prices are in Indian rupees. Data contains product names, categories, prices
# product description and their overall rating. Market price is Wholesale price
# and Sale Price is Retail Price.

# Questions being answered in this project:

# 1. How are market prices and sale prices correlated? How about rating?

#  positive correlation. rating has no correlation.

# 2. Which categories of product have the highest prices?

# Kitchen, Garden & Pets

# 3. Are any products being sold at a lower sale price? if so, by how much?

# Yes. a combined average of 13.7 percent decrease from market price to sale
# price across the product catalog.s

# 4. Is the Brand considered expensive? How is the distribution of prices
# across categories? 

# No. the Brand is considered inexpensive. While prices can range from ₹3 to
# ₹12500, majority of the price points are clustered below ₹2000. Rating is far
# more nuanced and represents the complexity behind customer review behavior.
# Multiple factors affect the overall rating and these factors are not visible
# in the dataset. 


# Any insights on consumer behavior? Are ratings indicative of any patterns?

# Cleaning notes:
# No duplicates or human error was found. Dataset on Kaggle.com had a usability
# score of 10, indicating a well made dataset. No variable names were changed.
# However, the dataset consisted of at least 30 percent NA values. Out of which,
# majority came from the rating column. Since removal of non-complete cases or
# omission of NA values had a chance of skewing the data greatly, Imputation
# was conducted. Once imputed, filled-in data was parsed back into the original
# and assigned the new name mentioned above. data distributions of cleaned and
# original dataset(after omission of NA values) were compared to verify bias and
# skew. No bias and skew was found. Imputation was a success.


# CLEANING: WITHOUT COMMENTS===================================================
# Toggle this section by clicking the icon next to Line 22. This section is
# helpful when you need a simple glance-through without getting distorted from
# the multitude of comments.
library(tidyverse)
library(readr)
install.packages("formattable")
library(formattable)
install.packages("finalfit")
library(finalfit)
library(mice)

BB <- read_csv("BigBasket Products.csv")
View(BB)
attach(BB)
summary(BB)

subset(BB, BB$sale_price == 2.45)
subset(BB, BB$sale_price == 12500.00)
subset(BB, BB$market_price == 3.0)
glimpse(BB)

install.packages("formattable")
library(formattable)
formattable(BB$market_price, format = "f", digits = 2)
glimpse(BB)

dupe_record <- duplicated(BB$index)
sum(dupe_record)
rm(dupe_record)

dupe_table <- duplicated(as.list(BB))
colnames(BB[dupe_table])
rm(dupe_table)

colSums(is.na(BB))


BB[!complete.cases(BB), ]

na_count_rating <- aggregate(rating ~ category,
  BB,
  function(rating) {
    sum(is.na(rating))
  },
  na.action = NULL
)
rm(na_count_rating)

temp_tb_1 <- subset(BB, is.na(product))
view(temp_tb_1)
rm(temp_tb)

temp_tb <- subset(BB, is.na(brand))
view(temp_tb)
rm(temp_tb)

install.packages("finalfit")
library(finalfit)
BB %>%
  missing_plot()

BB_dupe <- BB
BB_dupe$rating_na <- ifelse(is.na(BB_dupe$rating), 1, 0)
BB_dupe$desc_na <- ifelse(is.na(BB_dupe$description), 1, 0)

write.csv(BB_dupe, file = "Modified BigBasket.csv")

t.test(BB_dupe$sale_price ~ BB_dupe$rating_na, var.equal = TRUE)
t.test(BB_dupe$market_price ~ BB_dupe$rating_na, var.equal = TRUE)
t.test(BB_dupe$sale_price ~ BB_dupe$desc_na, var.equal = TRUE)
t.test(BB_dupe$market_price ~ BB_dupe$desc_na, var.equal = TRUE)
chisq.test(table(BB_dupe$category, BB_dupe$rating_na))
chisq.test(table(BB_dupe$sub_category, BB_dupe$rating_na))
chisq.test(table(BB_dupe$brand, BB_dupe$rating_na))

library(mice)
tmp_tb <- subset(BB_dupe, select = c(sale_price, market_price, rating))
md.pattern(tmp_tb)

imputed_data <-
  mice(
    tmp_tb,
    m = 5,
    maxit = 60,
    method = "pmm",
    seed = 500
  )
summary(imputed_data)
Impute_graph <- stripplot(imputed_data)
rm(Impute_graph)

completeData <- complete(imputed_data, 2)
view(completeData)
rm(tmp_tb)
rm(imputed_data)

BB_dupe$rating <- completeData$rating
summary(BB_dupe)
colSums(is.na(BB_dupe))
rm(completeData)

BB_final <- subset(BB_dupe, select = -c(description))
summary(BB_final)

BB_final <- BB_final[complete.cases(BB_final), ]
rm(BB_dupe)
colSums(is.na(BB_final))

write.csv(BB_final, file = "BigBasketFinal.csv")

temp <- BB_final %>%
  select(category, rating) %>%
  group_by(category) %>%
  summarise(Count = n(), avg_rating = mean(rating))
view(temp)

temp2 <- BB %>%
  select(category, rating) %>%
  group_by(category) %>%
  summarise(Count = n(), avg_rating = mean(rating, na.rm = TRUE))

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

# CLEANING: WITH COMMENTS=======================================================
# Toggle this section by clicking the icon next to Line 113. This section is
# helpful when you need to walk yourself through the code and understand the
# rationale behind process.

library(tidyverse)
library(readr)
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

install.packages("formattable")
library(formattable)
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

install.packages("finalfit")
library(finalfit) # installed and loaded the final fit package
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

library(mice) # Multivariate Imputations By Chained Equations Package. Used for
# multiple imputation.

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
library(dplyr)
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





# ANALYSIS WITHOUT COMMENTS=====================================================

library(tidyverse)
BB_final <- read_csv("BigBasketFinal.csv")
View(BigBasketFinal)

ggplot(BB_final, aes(x = market_price, y = sale_price)) +
  geom_point(aes(color = category)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Market Price vs Sale Price",
    subtitle = "Postive correlation. Kitchen, Garden & Pets category have some
    of the highest prices in entire catalog",
    caption = "Data From Kaggle.com, Updated June 22nd, 2022",
    x = "Market Price in Indian Rupees",
    y = "Sale Price in Indian Rupees",
    color = "Category"
  )

ggplot(BB_final, aes(x = rating, y = sale_price)) +
  geom_point(aes(fill = sale_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Sale Price vs Rating",
    x = "Overall Rating",
    y = "Sale Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    sale price"
  )

ggplot(BB, aes(x = rating, y = sale_price)) +
  geom_point(aes(fill = sale_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Sale Price vs Rating (Original Dataset)",
    x = "Overall Rating",
    y = "Sale Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    sale price"
  )

ggplot(BB_final, aes(x = rating, y = market_price)) +
  geom_point(aes(fill = market_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Market Price vs Rating",
    x = "Overall Rating",
    y = "Market Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    market price"
  )

ggplot(BB, aes(x = rating, y = market_price)) +
  geom_point(aes(fill = market_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Market Price vs Rating (Original Dataset)",
    x = "Overall Rating",
    y = "Market Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    market price"
  )

ggplot(BB_final, aes(y = market_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  labs(
    title = "Market Price Data Distribution",
    subtitle = "Data points are overwhelmingly distributed towards the lower end 
    of the value range",
    caption = "Data from Kaggle.com",
    x = "Market Price",
    y = "Price in Rupees"
  )

ggplot(BB_final, aes(y = sale_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  labs(
    title = "Sale Price Data Distribution",
    subtitle = "Data points are overwhelmingly distributed towards the lower end 
    of the value range",
    caption = "Data from Kaggle.com",
    x = "Sale Price",
    y = "Price in Rupees"
  )

ggplot(BB_final, aes(y = rating, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  labs(
    title = "Data Distribution of Rating",
    subtitle = "Data point distribution is nuanced",
    caption = "Data from Kaggle.com",
    x = "Rating",
    y = "Rating Value"
  )

ggplot(BB_final, aes(y = sale_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  facet_wrap("category")

ggplot(BB_final, aes(y = market_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  facet_wrap("category")

ggplot(BB_final, aes(y = rating, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  facet_wrap("category")

avg_price_percent_change <- BB_final %>%
  group_by(category) %>%
  dplyr::summarize(
    avg_sale_price = mean(sale_price),
    avg_market_price = mean(market_price),
    percent_change = (avg_sale_price - avg_market_price) /
      avg_market_price * 100
  )

ggplot(avg_price_percent_change, aes(x = category, y = percent_change)) +
  geom_col(aes(fill = percent_change)) +
  scale_fill_distiller(palette = "Blues") +
  guides(x = guide_axis(angle = 45)) +
  labs(
    title = "Percentage Change From Market Price to Sale Price",
    y = "Percentage Change",
    x = "Category",
    fill = "Percent Gradient"
  )
# ANALYSIS: WITH COMMENTS=======================================================

library(tidyverse)
BB_final <- read_csv("Documents/BigBasketFinal.csv")
View(BigBasketFinal)
# importing cleaned dataset

ggplot(BB_final, aes(x = market_price, y = sale_price)) +
  geom_point(aes(color = category)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Market Price vs Sale Price",
    subtitle = "Postive correlation. Kitchen, Garden & Pets category have some
    of the highest prices in entire catalog",
    caption = "Data From Kaggle.com, Updated June 22nd, 2022",
    x = "Market Price in Indian Rupees",
    y = "Sale Price in Indian Rupees",
    color = "Category"
  )
# Plotted a scatter plot to understand the relationship between two different
# types of prices. Positive correlation on a whole. As market price goes up,
# sale price goes up too. Kitchen, Garden & Pets category have some of the
# most expensive items.

# investigating relationship between rating and sale price
ggplot(BB_final, aes(x = rating, y = sale_price)) +
  geom_point(aes(fill = sale_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Sale Price vs Rating",
    x = "Overall Rating",
    y = "Sale Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    sale price"
  )

# comparing data distribution with original dataset
ggplot(BB, aes(x = rating, y = sale_price)) +
  geom_point(aes(fill = sale_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Sale Price vs Rating (Original Dataset)",
    x = "Overall Rating",
    y = "Sale Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    sale price"
  )
# Plotted a scatter plot to understand the relationship between sale price and
# overall rating. No relationship found. This is consistent with findings on the
# original BigBasket dataset (NA values were omitted).

# investigating distribution of rating of against market price.
# imputed dataset
ggplot(BB_final, aes(x = rating, y = market_price)) +
  geom_point(aes(fill = market_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Market Price vs Rating",
    x = "Overall Rating",
    y = "Market Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    market price"
  )

# original dataset
ggplot(BB, aes(x = rating, y = market_price)) +
  geom_point(aes(fill = market_price), shape = 21) +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 6250
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Market Price vs Rating (Original Dataset)",
    x = "Overall Rating",
    y = "Market Price in Rupees",
    caption = "Data from Kaggle.com",
    subtitle = "No correlation found. Overall rating is not dependent on
    market price"
  )
# Plotted a scatter plot to understand the relationship between market price and
# overall rating. No relationship found. This is consistent with findings on the
# original BigBasket dataset (NA values were omitted).

# Therefore, imputation did not damage, skew or introduce bias. Nevertheless,
# deeper insights could be found if data has a column consisting of number of
# reviews. It could then be plotted against sale price and market price.


# investigating distribution of both sales prices and market prices

ggplot(BB_final, aes(y = market_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  labs(
    title = "Market Price Data Distribution",
    subtitle = "Data points are overwhelmingly distributed towards the lower end 
    of the value range",
    caption = "Data from Kaggle.com",
    x = "Market Price",
    y = "Price in Rupees"
  )
# data distribution shows that while prices can extend to over ₹12,0000, they
# concentrated to prices below ₹2,000. However, market prices only show the
# prices irrespective of grocery brand.

ggplot(BB_final, aes(y = sale_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  labs(
    title = "Sale Price Data Distribution",
    subtitle = "Data points are overwhelmingly distributed towards the lower end 
    of the value range",
    caption = "Data from Kaggle.com",
    x = "Sale Price",
    y = "Price in Rupees"
  )
# Sale price exhibits similar distribution.

# investigating distribution within rating.
ggplot(BB_final, aes(y = rating, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  labs(
    title = "Data Distribution of Rating",
    subtitle = "Data point distribution is nuanced",
    caption = "Data from Kaggle.com",
    x = "Rating",
    y = "Rating Value"
  )

# data distribution is much more nuanced for rating. The wide bottling of
# data at discrete points (whole numbers) instead of continuous points
# (decimal numbers) show that customers are unlikely to give ratings that fall
# between two numbers. Nevertheless, majority of the data tends to cluster
# around the median between 4 and 4.5.

ggplot(BB_final, aes(y = sale_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  facet_wrap("category")
# getting a deeper look at the sale price data across categories of product.
# The graphs are reflective of the trends discovered in the general population.

ggplot(BB_final, aes(y = market_price, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  facet_wrap("category")
# trends are similar to sale price trends for market price across categories.

ggplot(BB_final, aes(y = rating, x = "")) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red") +
  facet_wrap("category")
# reflective of the overall clustering of the data around the median witnessed
# in the general population. However tendencies of assigning a rating changes
# ever so slightly. To be more precise, there are smaller individual clusters
# of data points around whole numbers for some categories. For others, there
# are very long tails and a direct, large cluster of data points around a single
# region.

# this could indicate couple of things. It could indicate that people care about
# reviewing properly. Or perhaps, there are inconsistencies in product quality
# that cause the customers to have varying degrees of satisfaction. It could
# also indicate the nature of the product. A product whose usefulness depends
# on its quality itself rather than usage process will have very different
# review behavior than say, a product from Vegetable category. Vegetable
# satisfaction completely depends on the usage process rather than product
# quality. This is in contrast with Beauty and Hygiene, where satisfaction
# levels are fully dependent on quality rather than usage process.


# investigating avg sale and market prices across categories. Including
# percentage change between both the price parameters.

avg_price_percent_change <- BB_final %>%
  group_by(category) %>%
  dplyr::summarize(
    avg_sale_price = mean(sale_price),
    avg_market_price = mean(market_price),
    percent_change = (avg_sale_price - avg_market_price) /
      avg_market_price * 100
  )

# created a tibble listing average sale price, average market price, percent
# change and grouped by category. In general, sale prices are cheaper than
# market prices across categories.

# You can expect to pay 13.7 percent lower than
# market price on average if you were to buy from BigBasket.

ggplot(avg_price_percent_change, aes(x = category, y = percent_change)) +
  geom_col(aes(fill = percent_change)) +
  scale_fill_distiller(palette = "Blues") +
  guides(x = guide_axis(angle = 45)) +
  labs(
    title = "Percentage Change From Market Price to Sale Price",
    y = "Percentage Change",
    x = "Category",
    fill = "Percent Gradient"
  )

# plotted percentage change in a column chart to better visualize changes in
# market and sale price. Kitchen, Garden & Pets followed by Fruits & Vegetables
# display the highest decrease from market price to sale price

            
