treat_data2 <- function(line_item_clean){
  set.seed(1605)
  # function to compute total within-cluster sum of square 
  wss <- function(k) {
    kmeans(line_item_clean[, c("price", "unit_price")], k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:15
  
  # extract wss for 2-15 clusters
  wss_values <- map_dbl(k.values, wss)
  
  k4 <- kmeans(line_item_clean[, c("price", "unit_price")], centers = 4, nstart = 10)
  
  # Group 1
  line_item_group1 <- line_item_clean %>% filter(k4$cluster == 1)
  line_item_group1a <- line_item_group1 %>% filter(price/unit_price > 30000)
  line_item_group1a <- line_item_group1a %>% mutate(price = price/100000)
  line_item_group1b <- line_item_group1 %>% filter(price/unit_price < 30000)
  line_item_group1b <- line_item_group1b %>% mutate(price = price/10000)
  
  line_item_group1 <- line_item_group1a %>% bind_rows(line_item_group1b)
  rm(line_item_group1a, line_item_group1b)
  
  # Group 2
  line_item_group2 <- line_item_clean %>% filter(k4$cluster == 2)
  line_item_group2a <- line_item_group2 %>% filter(price/unit_price > 20000)
  line_item_group2a <- line_item_group2a %>% mutate(price = price/100000)
  line_item_group2b <- line_item_group2 %>% filter(price/unit_price < 20000)
  line_item_group2b <- line_item_group2b %>% mutate(price = price/10000)
  
  line_item_group2 <- line_item_group2a %>% bind_rows(line_item_group2b)
  rm(line_item_group2a, line_item_group2b)
  
  # Group 3
  line_item_group3 <- line_item_clean %>% filter(k4$cluster == 3)
  
  set.seed(1605)
  k2 <- kmeans(line_item_group3[, c("price", "unit_price")], centers = 2, nstart = 10)
  
  # Group 3a
  line_item_group3a <- line_item_group3 %>% filter(k2$cluster == 1)
  line_item_group3aSmall <- line_item_group3a %>% filter((price-5000)/unit_price < 50)
  
  line_item_group3aHigh <- line_item_group3a %>% filter((price-5000)/unit_price >= 50)
  line_item_higher <- line_item_group3aHigh %>% filter(price/unit_price > 4000)
  line_item_higher <- line_item_higher %>% mutate(price = price/10000)
  line_item_smaller <- line_item_group3aHigh %>% filter(price/unit_price < 4000)
  line_item_smaller <- line_item_smaller %>% mutate(price = price/1000)
  line_item_group3aHigh <- line_item_higher %>% bind_rows(line_item_smaller)
  line_item_group3a <- line_item_group3aSmall %>% bind_rows(line_item_group3aHigh)
  rm(line_item_higher, line_item_smaller, line_item_group3aSmall, line_item_group3aHigh)
  
  # Group 3b
  line_item_group3b <- line_item_group3 %>% filter(k2$cluster == 2)
  line_item_group3bSmall <- line_item_group3b %>% filter(unit_price < 400)
  line_item_group3bSmall <- line_item_group3bSmall %>% mutate(price = price/10000)
  line_item_group3bHigh <- line_item_group3b %>% filter(unit_price > 400)
  line_item_group3bHigh <- line_item_group3bHigh %>% mutate(price = price/1000)
  line_item_group3b <- line_item_group3bSmall %>% bind_rows(line_item_group3bHigh)
  
  line_item_group3 <- line_item_group3a %>% bind_rows(line_item_group3b)
  rm(line_item_group3bSmall, line_item_group3bHigh, line_item_group3a, line_item_group3b)
  
  # Group 4
  line_item_group4 <- line_item_clean %>% filter(k4$cluster == 4)
  line_item_group4 <- line_item_group4 %>% mutate(price = price/100000)
  
  # All groups
  line_item_clean <- line_item_group1 %>% bind_rows(line_item_group2) %>% 
    bind_rows(line_item_group3) %>% bind_rows(line_item_group4)
  rm(line_item_group1, line_item_group2, line_item_group3, line_item_group4)
  
  # Keep only the values which the absolute difference between unit price and price 
  # is smaller than the price multiplied by 75%
  line_item_clean <- line_item_clean %>% filter(abs(unit_price - price) < price*0.75)
  
  # Due to limitation process some sku-items with low frequency (< 3 times bought) 
  # were removed
  product_freq <- line_item_clean %>% group_by(sku) %>% 
    summarise(quantity = sum(product_quantity)) %>% filter(quantity >= 3)
  line_item_clean <- line_item_clean %>% filter(sku %in% product_freq$sku)
  
  line_item_clean
}