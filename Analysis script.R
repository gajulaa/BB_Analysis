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