install.packages("tidyverse")
install.packages("tibbletime")
install.packages("lubridate")
install.packages("DT")

library(tidyverse)
library(tibbletime)
library(lubridate)
library(DT)
memory.limit(size = 50000)

lmeat_files <- untar("lunchmeat.tgz", list = TRUE)

untar("lunchmeat.tgz")

move_11 <- tibble()

for (i in seq_along(lmeat_files)) {
  
  if (substr(lmeat_files[i], 22, 40) == "2011/Movement_Files") {
    
    temp_move_11 <- read_tsv(lmeat_files[i])
    
    move_11 <- move_11 %>%
      
      bind_rows(temp_move_11)
    
    rm(temp_move_11)
  }
  
}

products <- read_tsv("nielsen_extracts/RMS/Master_Files/Latest/products.tsv", quote = "")

stores_11 <- read_tsv("nielsen_extracts/RMS/2011/Annual_Files/stores_2011.tsv")

full_11 <- move_11 %>%
  inner_join(stores_11, by ="store_code_uc") %>% 
  filter(channel_code == "F" & fips_state_descr %in% c("ND", "MN", "MO" ,"DC")) %>%
  inner_join(products, by ="upc")

rm(move_11, stores_11)

move_12 <- tibble()

for (i in seq_along(lmeat_files)) {
  
  if (substr(lmeat_files[i], 22, 40) == "2012/Movement_Files") {
    
    temp_move_12 <- read_tsv(lmeat_files[i])
    
    move_12 <- move_12 %>%
      
      bind_rows(temp_move_12)
    
    rm(temp_move_12)
  }
}

stores_12 <- read_tsv("nielsen_extracts/RMS/2012/Annual_Files/stores_2012.tsv")

full_12 <- move_12 %>%
  inner_join(stores_12, by ="store_code_uc") %>% 
  filter(channel_code == "F" & fips_state_descr %in% c("ND", "MN", "MO" ,"DC")) %>%
  inner_join(products, by ="upc")

rm(move_12, stores_12)

move_13 <- tibble()

for (i in seq_along(lmeat_files)) {
  
  if (substr(lmeat_files[i], 22, 40) == "2013/Movement_Files") {
    
    temp_move_13 <- read_tsv(lmeat_files[i])
    
    move_13 <- move_13 %>%
      
      bind_rows(temp_move_13)
    
    rm(temp_move_13)
  }
}

stores_13 <- read_tsv("nielsen_extracts/RMS/2013/Annual_Files/stores_2013.tsv", col_types = list(col_double(), col_double(),
                                                                                                 col_double(), col_double(), col_character(), col_character(), col_double(), col_character(), col_double(),
                                                                                                 col_character(), col_double(), col_character()))

full_13 <- move_13 %>%
  inner_join(stores_13, by ="store_code_uc") %>% 
  filter(channel_code == "F" & fips_state_descr %in% c("ND", "MN", "MO" ,"DC")) %>%
  inner_join(products, by ="upc")

rm(move_13, stores_13, products)

full_11_12_13 <- full_11 %>%
  bind_rows(full_12) %>%
  bind_rows(full_13) %>%
  mutate(week_end = ymd(week_end), sales = units * price)

store_id_11 <- distinct(full_11, store_code_uc)
store_id_12 <- distinct(full_12, store_code_uc)
store_id_13 <- distinct(full_13, store_code_uc)

in_11_not_12 <- anti_join(store_id_11, store_id_12, by = "store_code_uc")
in_11_not_13 <- anti_join(store_id_11, store_id_13, by = "store_code_uc")
in_12_not_11 <- anti_join(store_id_12, store_id_11, by = "store_code_uc")
in_12_not_13 <- anti_join(store_id_12, store_id_13, by = "store_code_uc")
in_13_not_11 <- anti_join(store_id_13, store_id_11, by = "store_code_uc")
in_13_not_12 <- anti_join(store_id_13, store_id_12, by = "store_code_uc")

full_11_12_13 <- full_11_12_13 %>%
  anti_join(in_11_not_12, by = "store_code_uc") %>%
  anti_join(in_11_not_13, by = "store_code_uc") %>%
  anti_join(in_12_not_11, by = "store_code_uc") %>%
  anti_join(in_12_not_13, by = "store_code_uc") %>%
  anti_join(in_13_not_11, by = "store_code_uc") %>%
  anti_join(in_13_not_12, by = "store_code_uc")

rm(full_11, full_12, full_13, in_11_not_12, in_11_not_13, in_12_not_11, in_12_not_13, in_13_not_11, in_13_not_12,
   store_id_11, store_id_12, store_id_13)

stores <- unique(full_11_12_13$store_code_uc)

retail_switchers <- vector("numeric", length = 0)

for (i in seq_along(stores)) {
  
  ind_store_sales <- full_11_12_13 %>%
    filter(store_code_uc == stores[i])
  
  switcher_test <- unique(ind_store_sales$retailer_code)
  
  if (length(switcher_test) > 1) {
    
    retail_switchers <- append(retail_switchers, stores[i])
    
  }
}

parent_switchers <- vector("numeric", length = 0)

for (i in seq_along(stores)) {
  
  ind_store_sales <- full_11_12_13 %>%
    filter(store_code_uc == stores[i])
  
  switcher_test <- unique(ind_store_sales$parent_code)
  
  if (length(switcher_test) > 1) {
    
    parent_switchers <- append(parent_switchers, stores[i])
    
  }
}

switchers <- full_11_12_13 %>%
  filter(store_code_uc %in% retail_switchers | store_code_uc %in% parent_switchers)

full_11_12_13 <- full_11_12_13 %>%
  anti_join(switchers, by = "store_code_uc")

save(full_11_12_13, file = "full_11_12_13.RData")

state_abbrevs <- c("MN", "MO", "ND", "DC")

for (i in seq_along(state_abbrevs)) {
  
  sales <- full_11_12_13 %>%
    filter(fips_state_descr == state_abbrevs[i])
  
  rets <- unique(sales$retailer_code)
  
  for (j in seq_along(rets)) {
    
    top_100 <- sales %>%
      filter(retailer_code == rets[j]) %>%
      group_by(upc) %>%
      summarize(sales = sum(sales)) %>%
      arrange(desc(sales)) %>%
      slice(1:100)
    
    ret_sales <- sales %>%
      filter(retailer_code == rets[j]) %>%
      semi_join(top_100, by = "upc") %>%
      group_by(retailer_code, upc, week_end) %>%
      summarize(total_sales = sum(sales))
    
    avg_sales <- ret_sales %>%
      group_by(retailer_code, upc) %>%
      summarize(avg_sales = mean(total_sales))
    
    ret_sales <- ret_sales %>%
      inner_join(avg_sales, by = c("upc", "retailer_code")) %>%
      mutate(scaled_sales = total_sales / avg_sales) %>%
      ungroup() %>%
      arrange(week_end)
    
    chart_data <- matrix(nrow = n_distinct(ret_sales$upc), ncol = n_distinct(ret_sales$week_end))
    
    colnames(chart_data) <- as.character(paste("week", unique(ret_sales$week_end)))
    
    rownames(chart_data) <- unique(ret_sales$upc)
    
    ind_upcs <- unique(ret_sales$upc)
    
    for (k in seq_along(ind_upcs)) {
      
      single_upc_sales <- ret_sales %>%
        select(upc, week_end, scaled_sales) %>%
        filter(upc == ind_upcs[k])
      
      no_sales <- ret_sales %>%
        distinct(week_end) %>%
        anti_join(single_upc_sales, by = "week_end") %>%
        mutate(upc = ind_upcs[k], scaled_sales = 0)
      
      single_upc_sales <- single_upc_sales %>%
        bind_rows(no_sales) %>%
        arrange(week_end)
      
      chart_data[ind_upcs[k],] <- single_upc_sales$scaled_sales
      
    }
    
    chart_data <- round(chart_data, 2)
    
    brks <- quantile(chart_data, probs = seq(.05, .95, .05), na.rm = TRUE)
    
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% 
    {paste0("rgb(255,", ., ",", ., ")")}
    
    chart_data <- as_tibble(chart_data, rownames = NA)
    
    sales_table <- datatable(chart_data, caption = paste("Lunchmeat Sales for Retailer", rets[j], "in", state_abbrevs[i])) %>% 
      formatStyle(names(chart_data), backgroundColor = styleInterval(brks, clrs))
    
    print(sales_table)
    
  }
  
}






