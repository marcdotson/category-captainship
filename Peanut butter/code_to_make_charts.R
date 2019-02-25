state_abbrevs <- c("MN", "MO", "ND", "DC")

for (i in seq_along(state_abbrevs)) {
  
  sales <- full_11_12_13 %>%
    filter(fips_state_descr == state_abbrevs[i])
  
  rets <- unique(sales$retailer_code)
  
  for (j in seq_along(rets)) {
    
    ret_sales <- sales %>%
      filter(retailer_code == rets[j]) %>%
      group_by(retailer_code, upc, week_end) %>%
      summarize(total_sales = sum(sales))
    
    avg_sales <- ret_sales %>%
      group_by(retailer_code, upc) %>%
      summarize(avg_sales = mean(total_sales))
    
    ret_sales <- ret_sales %>%
      inner_join(avg_sales, by = c("upc", "retailer_code")) %>%
      mutate(scaled_sales = total_sales / avg_sales) %>%
      ungroup()
    
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
    
    sales_table <- datatable(chart_data, caption = paste("Peanut Butter Sales for Retailer", rets[j], "in", state_abbrevs[i])) %>% 
      formatStyle(names(chart_data), backgroundColor = styleInterval(brks, clrs))
    
    print(sales_table)
    
  }
  
}






