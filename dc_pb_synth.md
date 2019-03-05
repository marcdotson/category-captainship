DC Peanut Butter Synthetic Control
================

Install packages and load library.

``` r
#install.packages('tidyverse')
#install.packages("Synth")
library(tidyverse)
library(lubridate)
library(Synth)
```

Unzip the 'peanutbutter.tgz' file. Read in 2011 movement and store files, merge them based on the store code, and filter for 'F' (food stores) and states North Dakota, Minnesota, Missouri, and D.C.

``` r
untar("peanutbutter.tgz")

move_11 <- read_tsv("nielsen_extracts/RMS/2011/Movement_Files/0506_2011/1421_2011.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   store_code_uc = col_double(),
    ##   upc = col_character(),
    ##   week_end = col_double(),
    ##   units = col_double(),
    ##   prmult = col_double(),
    ##   price = col_double(),
    ##   feature = col_double(),
    ##   display = col_double()
    ## )

``` r
stores_11 <- read_tsv("nielsen_extracts/RMS/2011/Annual_Files/stores_2011.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   store_code_uc = col_double(),
    ##   year = col_double(),
    ##   parent_code = col_double(),
    ##   retailer_code = col_double(),
    ##   channel_code = col_character(),
    ##   store_zip3 = col_character(),
    ##   fips_state_code = col_double(),
    ##   fips_state_descr = col_character(),
    ##   fips_county_code = col_double(),
    ##   fips_county_descr = col_character(),
    ##   dma_code = col_double(),
    ##   dma_descr = col_character()
    ## )

``` r
full_11 <- move_11 %>%
  inner_join(stores_11, by ="store_code_uc") %>% 
  filter(channel_code == "F", 
         fips_state_descr %in% c("ND", "MN", "MO" ,"DC"))

save(full_11, file = 'pb_full_11.RData')

rm(move_11, stores_11, full_11)
```

Perform the previous step for 2012 and 2013.

``` r
move_12 <- read_tsv("nielsen_extracts/RMS/2012/Movement_Files/0506_2012/1421_2012.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   store_code_uc = col_double(),
    ##   upc = col_character(),
    ##   week_end = col_double(),
    ##   units = col_double(),
    ##   prmult = col_double(),
    ##   price = col_double(),
    ##   feature = col_double(),
    ##   display = col_double()
    ## )

``` r
stores_12 <- read_tsv("nielsen_extracts/RMS/2012/Annual_Files/stores_2012.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   store_code_uc = col_double(),
    ##   year = col_double(),
    ##   parent_code = col_double(),
    ##   retailer_code = col_double(),
    ##   channel_code = col_character(),
    ##   store_zip3 = col_character(),
    ##   fips_state_code = col_double(),
    ##   fips_state_descr = col_character(),
    ##   fips_county_code = col_double(),
    ##   fips_county_descr = col_character(),
    ##   dma_code = col_double(),
    ##   dma_descr = col_character()
    ## )

``` r
full_12 <- move_12 %>%
  inner_join(stores_12, by ="store_code_uc") %>% 
  filter(channel_code == "F",
         fips_state_descr %in% c("ND", "MN", "MO" ,"DC"))

save(full_12, file = 'pb_full_12.RData')

rm(move_12, stores_12, full_12)

move_13 <- read_tsv("nielsen_extracts/RMS/2013/Movement_Files/0506_2013/1421_2013.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   store_code_uc = col_double(),
    ##   upc = col_character(),
    ##   week_end = col_double(),
    ##   units = col_double(),
    ##   prmult = col_double(),
    ##   price = col_double(),
    ##   feature = col_double(),
    ##   display = col_double()
    ## )

``` r
stores_13 <- read_tsv("nielsen_extracts/RMS/2013/Annual_Files/stores_2013.tsv", 
                      col_types = list(col_double(), col_double(),
                      col_double(), col_double(), col_character(),
                      col_character(), col_double(), col_character(), 
                      col_double(), col_character(), col_double(), col_character()))

full_13 <- move_13 %>%
  inner_join(stores_13, by = "store_code_uc") %>%
  filter(channel_code == "F",
         fips_state_descr %in% c("ND", "MN", "MO", "DC"))

save(full_13, file = 'pb_full_13.RData')

rm(move_13, stores_13)
```

Bind full\_11,12,13 into one tbl, 'full\_11\_12\_13'. Overwrite the variable week\_end to be in year-month-date format. Create sales variable, which is the number of units sold multiplied by the price.

``` r
load('pb_full_11.RData')
load('pb_full_12.RData')

full_11_12_13 <- full_11 %>%
  bind_rows(full_12) %>%
  bind_rows(full_13) %>%
  mutate(week_end = ymd(week_end), sales = units * price)

rm(full_11, full_12, full_13)
```

Read in products master file.

``` r
products <- read_tsv('products.tsv', quote = "")
```

    ## Parsed with column specification:
    ## cols(
    ##   upc = col_character(),
    ##   upc_ver_uc = col_double(),
    ##   upc_descr = col_character(),
    ##   product_module_code = col_double(),
    ##   product_module_descr = col_character(),
    ##   product_group_code = col_double(),
    ##   product_group_descr = col_character(),
    ##   department_code = col_double(),
    ##   department_descr = col_character(),
    ##   brand_code_uc = col_double(),
    ##   brand_descr = col_character(),
    ##   multi = col_double(),
    ##   size1_code_uc = col_double(),
    ##   size1_amount = col_double(),
    ##   size1_units = col_character(),
    ##   dataset_found_uc = col_character(),
    ##   size1_change_flag_uc = col_double()
    ## )

Merge products file and full\_11\_12\_13.

``` r
full_11_12_13 <- full_11_12_13 %>%
  left_join(products, by = 'upc')

rm(products)
```

Filter the full dataset for data pertaining to the retailers in DC. Display the retailer codes in DC.

``` r
dc_data <- full_11_12_13 %>%
  filter(fips_state_descr == 'DC')

treated_stores <- dc_data %>% filter(retailer_code==842) %>% distinct(store_code_uc)

control_stores <- dc_data %>% filter(retailer_code!=842) %>% distinct(store_code_uc)
```

Assign munfacturer for each brand. Organize data by stores and date, add sales across UPCs.

``` r
dc_data <- dc_data %>%
  mutate(manuf_name = substr(dc_data$brand_descr, 1, 3)) %>%
  filter(manuf_name %in% c('SKI', "JIF", "SMU", "SIM", "SAN", "ADA", "CTL")) %>%
  mutate(actual_manuf = if_else(manuf_name == 'CTL', 'CTL',
                        if_else(manuf_name == 'SKI', 'UNI', 'SMU'))) %>%
  mutate(store_code_uc=as.numeric(store_code_uc))

dc_data <- dc_data %>% group_by(store_code_uc, week_end) %>% summarize(sales=sum(sales)) %>%
  arrange(store_code_uc, week_end) 

dc_data <- as.data.frame(dc_data)
```

Check store codes to see how many weeks they have. Stores with less than 157 weeks of observations are dropped from the data. Then we arrange by 'store\_code\_uc' and 'week\_end', then create a 'n\_week\_end' variable to represent the date numerically.

``` r
weeks <- unique(dc_data$week_end)

codes <- unique(dc_data$store_code_uc)

drops <- c()

for (i in seq_along(codes)) {
  
  single_store <- dc_data %>% filter(store_code_uc==codes[i])
  
  single_store_vec <- unique(single_store$week_end)
  
  if (length(single_store_vec) < length(weeks)) {
    
    drops <- c(drops, codes[i])
    
  }

}

dc_data <- dc_data %>% filter(!store_code_uc %in% drops) %>% arrange(store_code_uc, week_end) %>%
  mutate(n_week_end=as.numeric(week_end))

control_stores <- control_stores %>% filter(!store_code_uc %in% drops)
```

Use data prep to create weights for synthetic control.

``` r
dataprep_out <- dataprep(foo=dc_data, predictors="sales", dependent="sales", unit.variable="store_code_uc",
                         time.variable="n_week_end", treatment.identifier=5133302,
                         controls.identifier=control_stores$store_code_uc,
                         time.predictors.prior=seq(14975, 15535, by = 7), time.optimize.ssr=seq(14975, 15535, by = 7),
                         time.plot=seq(14975, 16067, by = 7))

synth_out <-synth(dataprep_out)
```

    ## 
    ## X1, X0, Z1, Z0 all come directly from dataprep object.
    ## 
    ## 
    ## **************** 
    ##  optimization over w weights: computing synthtic control unit 
    ##  
    ## 
    ## 
    ## **************** 
    ## **************** 
    ## **************** 
    ## 
    ## MSPE (LOSS V): 24014.56 
    ## 
    ## solution.v:
    ##  1 
    ## 
    ## solution.w:
    ##  0.03670724 0.03469845 0.02984143 0.01159974 0.4612645 0.03238041 0.03246352 0.03670635 0.03585614 0.03173574 0.036639 0.02044926 0.02551085 0.03672516 0.03472481 0.03640007 0.0367203 0.02957704

Plot the synthetic control against the treated store over time.

``` r
path.plot(dataprep.res = dataprep_out, synth.res = synth_out, Xlab = c('Time'), Ylab = c('Sales'), tr.intake = 15535,
          Main = 'Synthetic Control and Treated Store Sales')
```

![](dc_pb_synth_files/figure-markdown_github/unnamed-chunk-11-1.png)

Plot the difference between the treated and synthetic control sales over time.

``` r
gaps.plot(dataprep.res = dataprep_out, synth.res = synth_out, Ylab = 'Difference', Xlab = 'Weeks', tr.intake = 15535,
          Main = 'Difference Between Treated and Synthetic Sales (Treated - Synthetic)')
```

![](dc_pb_synth_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
synth_tables <- synth.tab(synth.res = synth_out, dataprep.res = dataprep_out)
synth_tables$tab.w
```

    ##         w.weights unit.names unit.numbers
    ## 1804471     0.037    1804471      1804471
    ## 3949012     0.035    3949012      3949012
    ## 5148299     0.030    5148299      5148299
    ## 1802456     0.012    1802456      1802456
    ## 2078770     0.461    2078770      2078770
    ## 1808501     0.032    1808501      1808501
    ## 1028103     0.032    1028103      1028103
    ## 5319048     0.037    5319048      5319048
    ## 2319813     0.036    2319813      2319813
    ## 3521655     0.032    3521655      3521655
    ## 5524392     0.037    5524392      5524392
    ## 1860488     0.020    1860488      1860488
    ## 1657215     0.026    1657215      1657215
    ## 1788754     0.037    1788754      1788754
    ## 1807292     0.035    1807292      1807292
    ## 7962112     0.036    7962112      7962112
    ## 5164822     0.037    5164822      5164822
    ## 1809307     0.030    1809307      1809307

``` r
synth_tables$tab.pred
```

    ##       Treated Synthetic Sample Mean
    ## sales 897.081   897.082     1243.61

Create a vector of control sales and weights determined by the data prep. Creat w\_ctrl, which is a vector of the weighted control sales. Create a vector of treated sales. Create a tibble with the treated sales and weighted control sales for the pre period.

``` r
controls <- dataprep_out$Z0
weights <- synth_tables$tab.w$w.weights
colnames(controls) <- NULL
rownames(controls) <- NULL
w_ctrl <- as.vector(weights %*% t(controls), mode = 'numeric')
treat <- as.vector(dataprep_out$Z1)
weighted_data <- tibble(control = w_ctrl,
                           treated = treat,
                           time = 1:length(w_ctrl))
```

``` r
weighted_data <- weighted_data %>%
  gather(key = 'Store', value = 'Sales', -time)
```

Create a plot that compares the weighted control sales to the treated sales for the pre period.

``` r
weighted_data %>%
  ggplot(aes(x = time, y = Sales, col = Store)) +
  geom_line() +
  labs(x = 'Time',
       y = 'Total Sales',
       title = 'Sales for Treated Store and Synthetic Control Store Pre Treatment')
```

![](dc_pb_synth_files/figure-markdown_github/unnamed-chunk-17-1.png)

Add the weights for the controls to the dc data frame. Assign a weight of one to the treated store sales. Multiply the weights by the control store sales and sum the sales across the control stores by week. Create a dataframe that has the dates and sales by treated and control store. Add a variable "post" which is a binary variable reporting whether the sales occured pre or post treatment. The implementation date is 7/16/2012.

``` r
weight_tibble <- tibble(weight=synth_tables$tab.w$w.weights, store_code_uc=synth_tables$tab.w$unit.names)

dc_data <- dc_data %>% left_join(weight_tibble, "store_code_uc")

dc_data[is.na(dc_data)] <- 1

dc_data <- dc_data %>% mutate(w_sales=sales*weight) %>% 
  mutate(type=if_else(store_code_uc==5133302, 1, 0))

dc_controls <- filter(dc_data, type==0)

dc_controls <- aggregate(dc_controls$w_sales, by=list(dc_controls$week_end),sum)

dc_treats <- filter(dc_data, type==1)

names(dc_controls) <- c("week_end", "w_sales")

dc_controls <- dc_controls %>% mutate(type=0)

dc_treats <- subset(dc_treats, select=c("week_end", "w_sales", "type"))

reg_data <- bind_rows(dc_treats, dc_controls) %>% mutate(post=if_else(week_end > as.Date("2012-07-16"), 1, 0))

names(reg_data) <- c("week_end", "sales", "treat", "post")
```

Run the diff in diff regression. Create a plot for the synthetic control and treated store pre and post category captianship.

``` r
did_reg <- lm(sales ~ treat + post + treat * post, data = reg_data)

summary(did_reg)
```

    ## 
    ## Call:
    ## lm(formula = sales ~ treat + post + treat * post, data = reg_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -642.77 -142.96  -25.34  123.73  972.22 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1228.99      24.25  50.677  < 2e-16 ***
    ## treat        -331.91      34.30  -9.678  < 2e-16 ***
    ## post          260.52      34.86   7.474 8.01e-13 ***
    ## treat:post   -107.15      49.29  -2.174   0.0305 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 218.3 on 310 degrees of freedom
    ## Multiple R-squared:  0.5063, Adjusted R-squared:  0.5015 
    ## F-statistic:   106 on 3 and 310 DF,  p-value: < 2.2e-16

``` r
pre_cc <- reg_data %>%
        filter(week_end <= as.Date("2012-07-16"))
  
post_cc <- reg_data %>%
        filter(week_end > as.Date("2012-07-16"))

did_plot <- ggplot(reg_data, aes(week_end, sales, col = factor(treat))) + 
      geom_line() +
      geom_smooth(aes(week_end, sales), data = pre_cc, method = 'lm', se = FALSE) +
      geom_smooth(aes(week_end, sales), data = post_cc, method = 'lm', se = FALSE) +
      geom_vline(xintercept = as.numeric(as.Date("2012-07-16"))) +
      ggtitle("Synthetic control Diff in Diff for DC Peanut Butter") +labs(x="Time", y="Sales")

print(did_plot)
```

![](dc_pb_synth_files/figure-markdown_github/unnamed-chunk-19-1.png)
