# Libraries .. Load the following libraries. 
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(plotly)
library(dplyr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(lubridate)

# SECTOR INFORMATION
sp_500_index_tbl <- readRDS("data/sp_500_index_tbl.rds")
sp_500_index_tbl
sp_500_prices_tbl <- readRDS("data/sp_500_prices_tbl.rds")
sp_500_prices_tbl


### <mark>Question: Which stock prices behave similarly?</mark> ###

## Step 1 - Convert stock prices to a standardized format (daily returns)

sp_500_prices_tbl %>% glimpse()

sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
  
  select(symbol, date, adjusted) %>%
  
  filter(date >= ymd("2018-01-01")) %>%
  
  group_by(symbol) %>%
  mutate(lag_1 = lag(adjusted)) %>%
  ungroup() %>%
  
  filter(!is.na(lag_1)) %>%
  
  mutate(diff = adjusted - lag_1) %>%
  mutate(pct_return = diff / lag_1) %>%
  
  select(symbol, date, pct_return)

# Apply your data transformation skills
sp_500_daily_returns_tbl
saveRDS(sp_500_daily_returns_tbl, file = "data/sp_500_daily_returns_tbl.RDS") 

## Step 2 - Convert to User-Item Format

stock_date_matrix_tbl <- readRDS("data/sp_500_daily_returns_tbl.rds") %>%
  spread(key = date, value = pct_return, fill = 0)

stock_date_matrix_tbl

## Step 3 - Perform K-Means Clustering
kmeans_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  kmeans(centers = 4, nstart = 20)

kmeans_obj %>% glance()

## Step 4 - Find the optimal value of K
# Lets use `purrr` to iterate over many values of "k" using the `centers` argument. 
kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

# Apply the `kmeans_mapper()` and `glance()` functions iteratively using `purrr`.

k_means_mapped_tbl <- tibble(centers = 1:30) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))

#Output: k_means_mapped_tbl
k_means_mapped_tbl

#Next, let's visualize the "tot.withinss" from the glance output as a Scree Plot. 

k_means_mapped_tbl %>%
  unnest(glance) %>%
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#3e502c") +
  geom_line(color = "#502c50") +
  labs(title = "Scree Plot") +
  theme_tq()

## Step 5 - Apply UMAP
# let's plot the `UMAP` 2D visualization to help us investigate cluster assignments. 
umap_results <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  umap()

# Next, we want to combine the `layout` from the `umap_results` with the `symbol` column from the `stock_date_matrix_tbl`. 
umap_results_tbl <- umap_results$layout %>%
  as_tibble() %>%
  bind_cols(stock_date_matrix_tbl %>% select(symbol)) 

umap_results_tbl

# Finally, let's make a quick visualization of the `umap_results_tbl`.

umap_results_tbl %>%
  ggplot(aes(V1, V2)) +
  geom_point(alpha = 0.5, color = "#2c3e50") +
  theme_tq() +
  labs(title = "UMAP Projection")

## Step 6 - Combine K-Means and UMAP

# Next, we combine the K-Means clusters and the UMAP 2D representation
# First, pull out the K-Means for 10 Centers. Use this since beyond this value the Scree Plot flattens. 

k_means_mapped_tbl <- read_rds("data/k_means_mapped_tbl.rds")
umap_results_tbl   <- read_rds("data/umap_results_tbl.rds")

k_means_obj <- k_means_mapped_tbl %>%
  filter(centers == 10) %>%
  pull(k_means) %>%
  pluck(1)

# Next, we'll combine the clusters from the `k_means_obj` with the `umap_results_tbl`.

umap_kmeans_results_tbl <- k_means_obj %>% 
  augment(stock_date_matrix_tbl) %>%
  select(symbol, .cluster) %>%
  left_join(umap_results_tbl, by = "symbol") %>%
  left_join(sp_500_index_tbl %>% select(symbol, company, sector),
            by = "symbol")

# Plot the K-Means and UMAP results.

umap_kmeans_results_tbl %>%
  ggplot(aes(V1, V2, color = .cluster)) +
  geom_point(alpha = 0.5) +
  theme_tq() +
  scale_color_tq()

#Congratulations! You are done with the 1st challenge! :) 

