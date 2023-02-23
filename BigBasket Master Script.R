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


# CLEANING:=====================================================================

library(tidyverse)
library(readr)
library(formattable)
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


# ANALYSIS =====================================================================

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
