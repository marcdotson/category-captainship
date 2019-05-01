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

control_stores <- control_stores %>% arrange(store_code_uc)
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
    ## 1028103     0.037    1028103      1028103
    ## 1657215     0.035    1657215      1657215
    ## 1788754     0.030    1788754      1788754
    ## 1802456     0.012    1802456      1802456
    ## 1804471     0.461    1804471      1804471
    ## 1807292     0.032    1807292      1807292
    ## 1808501     0.032    1808501      1808501
    ## 1809307     0.037    1809307      1809307
    ## 1860488     0.036    1860488      1860488
    ## 2078770     0.032    2078770      2078770
    ## 2319813     0.037    2319813      2319813
    ## 3521655     0.020    3521655      3521655
    ## 3949012     0.026    3949012      3949012
    ## 5148299     0.037    5148299      5148299
    ## 5164822     0.035    5164822      5164822
    ## 5319048     0.036    5319048      5319048
    ## 5524392     0.037    5524392      5524392
    ## 7962112     0.030    7962112      7962112

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
    ## -485.11 -123.47   -7.68  105.04  654.12 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  899.834     20.913  43.028  < 2e-16 ***
    ## treat         -2.753     29.575  -0.093    0.926    
    ## post         142.179     30.057   4.730 3.41e-06 ***
    ## treat:post    11.184     42.507   0.263    0.793    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 188.2 on 310 degrees of freedom
    ## Multiple R-squared:  0.1351, Adjusted R-squared:  0.1267 
    ## F-statistic: 16.14 on 3 and 310 DF,  p-value: 8.951e-10

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

Installl and load microsynth. Create the microsynth weights, plots and results. Save the output as msynth\_out. Create a vector of weights called m\_weights.

``` r
#install.packages("microsynth")
library(microsynth)

msynth <- microsynth(dc_data, 
                   idvar="store_code_uc", timevar="n_week_end", intvar="type", 
                   start.pre=14975, end.pre=15535, end.post=16067, 
                   match.out="sales", result.var="sales", omnibus.var="sales", plot.var="sales",
                   test="lower", sep = TRUE)
```

    ## match.covar = TRUE.  Resetting:

    ## match.covar = c("")

    ## WARNING: There is a low number (1) of cases in the treatment or intervention group.

    ## Setting use.survey = FALSE.

    ## Be cautious of results involving linearization or confidence intervals.

    ## Calculating weights...

    ## Warning in grake(mm, ww, calfun, bounds = bounds, population =
    ## population, : Failed to converge: eps=0.26064232322118 in 251 iterations

    ## First model is infeasible for primary weights.  Consider setting use.backup = TRUE.

    ## Created main weights for synthetic control: Time = 2.46

    ## Matching summary for main weights:

    ##             Targets Weighted.Control All.scaled
    ## Intercept      1.00           0.8565     1.0000
    ## sales.15535 1250.53        1202.0825  1449.8626
    ## sales.15528 1380.98        1211.0382  1488.2337
    ## sales.15521 1175.11         970.9799  1398.2116
    ## sales.15514  968.14         958.7122  1335.3526
    ## sales.15507 1080.81        1032.4709  1427.7116
    ## sales.15500 1515.34        1362.3237  1657.7568
    ## sales.15493  924.55         955.0616  1384.4079
    ## sales.15486 1019.68         888.5108  1328.2800
    ## sales.15479  806.70         847.5406  1308.9347
    ## sales.15472 1126.02        1084.0126  1387.2574
    ## sales.15465 1036.04         971.9941  1361.4068
    ## sales.15458  889.15         954.2908  1382.0647
    ## sales.15451  908.89         932.1755  1405.1489
    ## sales.15444 1045.30        1025.0157  1414.3116
    ## sales.15437 1255.18        1232.8817  1496.3737
    ## sales.15430  845.58         893.1102  1369.6005
    ## sales.15423  802.10         869.0017  1306.9989
    ## sales.15416  950.29         914.9937  1386.4537
    ## sales.15409 1271.72        1273.2558  1550.7358
    ## sales.15402  852.78         983.8318  1405.4716
    ## sales.15395  826.70         904.3253  1314.4079
    ## sales.15388  849.53         877.4019  1305.8095
    ## sales.15381 1232.58        1271.5235  1564.9937
    ## sales.15374  997.19        1173.0828  1609.4268
    ## sales.15367  773.61         912.9434  1360.7753
    ## sales.15360  868.84         977.6233  1431.4616
    ## sales.15353 1225.76        1099.4987  1428.3505
    ## sales.15346 1266.87        1082.8588  1390.3416
    ## sales.15339  730.09         672.8745   828.6668
    ## sales.15332  829.53         890.1670  1068.0237
    ## sales.15325  906.18         849.0437  1269.5668
    ## sales.15318 1085.07        1166.9022  1421.7900
    ## sales.15311  858.59         919.5953  1355.2126
    ## sales.15304  739.78         670.0513   995.9753
    ## sales.15297  894.17         820.6233  1268.9463
    ## sales.15290 1086.30        1187.4119  1453.2926
    ## sales.15283  928.94        1046.4028  1421.2405
    ## sales.15276  802.57         849.6878  1205.3305
    ## sales.15269  949.28         854.5951  1296.5658
    ## sales.15262 1026.55         936.6592  1314.5779
    ## sales.15255 1139.40        1020.2055  1259.0947
    ## sales.15248  712.40         762.2611  1128.2926
    ## sales.15241  742.68         804.7944  1170.4405
    ## sales.15234  746.53         816.7791  1098.7626
    ## sales.15227  999.53        1024.0666  1187.0405
    ## sales.15220  643.42         736.3729  1019.2742
    ## sales.15213 1239.72        1286.3911  1905.8647
    ## sales.15206  664.44         725.0794   951.2158
    ## sales.15199 1059.07         940.3445  1059.2958
    ## sales.15192 1156.60         969.4188  1148.4474
    ## sales.15185  725.09         679.2356   950.8332
    ## sales.15178  576.71         708.6251   990.6874
    ## sales.15171  825.22         863.3178  1108.9663
    ## sales.15164  931.21         947.9283  1158.9411
    ## sales.15157  747.25         724.7758  1010.9421
    ## sales.15150  611.76         719.4595  1003.0953
    ## sales.15143  627.44         757.4086  1037.9511
    ## sales.15136 1013.36         975.7782  1241.9174
    ## sales.15129  701.77         748.8181  1058.0832
    ## sales.15122  583.09         687.9163   951.5532
    ## sales.15115  699.38         645.0561   934.9226
    ## sales.15108  837.77         760.0145   978.8200
    ## sales.15101  883.52         896.9411  1089.6763
    ## sales.15094  490.78         618.9587   902.1137
    ## sales.15087  741.03         713.3093  1081.6568
    ## sales.15080  847.44         767.2511  1086.2374
    ## sales.15073 1057.05         971.5110  1188.6511
    ## sales.15066  750.94         759.8582  1100.0216
    ## sales.15059  789.88         681.8269  1054.3095
    ## sales.15052  577.97         682.5142  1074.9021
    ## sales.15045  871.62         897.0514  1099.0337
    ## sales.15038  706.55         803.3264  1117.9305
    ## sales.15031  749.78         670.5643  1013.2605
    ## sales.15024  574.88         662.9088   973.3011
    ## sales.15017  963.42         871.9331  1087.4284
    ## sales.15010  812.04         875.3667  1243.0121
    ## sales.15003  764.45         744.3031  1078.5405
    ## sales.14996  634.20         716.1392  1059.0000
    ## sales.14989  965.48         846.8373  1170.4200
    ## sales.14982  926.88        1052.9336  1202.8284
    ## sales.14975  588.81         614.5121   728.9905
    ## Calculation of weights complete: Total time = 2.49
    ## 
    ## Making graphs and calculating basic statistics for end.post = 16067...
    ## Completed graphs and calculation of basic statistics for end.post = 16067.  Time = 0.01
    ## 
    ## Calculating survey statistics for end.post = 16067...
    ## Completed survey statistics for main weights: Time = 1.02
    ## Completed calculation of survey statistics for end.post = 16067.  Time = 1.02
    ## 
    ## microsynth complete: Overall time = 3.6

![](dc_pb_synth_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
summary(msynth)
```

    ## Weight Balance Table: 
    ##             Targets Weighted.Control All.scaled
    ## Intercept      1.00        0.8565118     1.0000
    ## sales.15535 1250.53     1202.0825294  1449.8626
    ## sales.15528 1380.98     1211.0382435  1488.2337
    ## sales.15521 1175.11      970.9799287  1398.2116
    ## sales.15514  968.14      958.7122149  1335.3526
    ## sales.15507 1080.81     1032.4708727  1427.7116
    ## sales.15500 1515.34     1362.3237186  1657.7568
    ## sales.15493  924.55      955.0615517  1384.4079
    ## sales.15486 1019.68      888.5108081  1328.2800
    ## sales.15479  806.70      847.5406162  1308.9347
    ## sales.15472 1126.02     1084.0125605  1387.2574
    ## sales.15465 1036.04      971.9941316  1361.4068
    ## sales.15458  889.15      954.2908055  1382.0647
    ## sales.15451  908.89      932.1755458  1405.1489
    ## sales.15444 1045.30     1025.0157087  1414.3116
    ## sales.15437 1255.18     1232.8816708  1496.3737
    ## sales.15430  845.58      893.1101822  1369.6005
    ## sales.15423  802.10      869.0017343  1306.9989
    ## sales.15416  950.29      914.9936750  1386.4537
    ## sales.15409 1271.72     1273.2557758  1550.7358
    ## sales.15402  852.78      983.8318011  1405.4716
    ## sales.15395  826.70      904.3252997  1314.4079
    ## sales.15388  849.53      877.4019481  1305.8095
    ## sales.15381 1232.58     1271.5234865  1564.9937
    ## sales.15374  997.19     1173.0827524  1609.4268
    ## sales.15367  773.61      912.9433799  1360.7753
    ## sales.15360  868.84      977.6232775  1431.4616
    ## sales.15353 1225.76     1099.4986615  1428.3505
    ## sales.15346 1266.87     1082.8587807  1390.3416
    ## sales.15339  730.09      672.8744712   828.6668
    ## sales.15332  829.53      890.1669631  1068.0237
    ## sales.15325  906.18      849.0436972  1269.5668
    ## sales.15318 1085.07     1166.9021830  1421.7900
    ## sales.15311  858.59      919.5953158  1355.2126
    ## sales.15304  739.78      670.0513426   995.9753
    ## sales.15297  894.17      820.6233149  1268.9463
    ## sales.15290 1086.30     1187.4118848  1453.2926
    ## sales.15283  928.94     1046.4028486  1421.2405
    ## sales.15276  802.57      849.6878336  1205.3305
    ## sales.15269  949.28      854.5950799  1296.5658
    ## sales.15262 1026.55      936.6592172  1314.5779
    ## sales.15255 1139.40     1020.2055057  1259.0947
    ## sales.15248  712.40      762.2611123  1128.2926
    ## sales.15241  742.68      804.7943970  1170.4405
    ## sales.15234  746.53      816.7791135  1098.7626
    ## sales.15227  999.53     1024.0666018  1187.0405
    ## sales.15220  643.42      736.3729400  1019.2742
    ## sales.15213 1239.72     1286.3911209  1905.8647
    ## sales.15206  664.44      725.0793767   951.2158
    ## sales.15199 1059.07      940.3445365  1059.2958
    ## sales.15192 1156.60      969.4187969  1148.4474
    ## sales.15185  725.09      679.2356007   950.8332
    ## sales.15178  576.71      708.6250585   990.6874
    ## sales.15171  825.22      863.3177963  1108.9663
    ## sales.15164  931.21      947.9283410  1158.9411
    ## sales.15157  747.25      724.7758114  1010.9421
    ## sales.15150  611.76      719.4595148  1003.0953
    ## sales.15143  627.44      757.4085861  1037.9511
    ## sales.15136 1013.36      975.7782211  1241.9174
    ## sales.15129  701.77      748.8180757  1058.0832
    ## sales.15122  583.09      687.9162850   951.5532
    ## sales.15115  699.38      645.0560718   934.9226
    ## sales.15108  837.77      760.0144540   978.8200
    ## sales.15101  883.52      896.9411158  1089.6763
    ## sales.15094  490.78      618.9586817   902.1137
    ## sales.15087  741.03      713.3093043  1081.6568
    ## sales.15080  847.44      767.2510786  1086.2374
    ## sales.15073 1057.05      971.5109546  1188.6511
    ## sales.15066  750.94      759.8581995  1100.0216
    ## sales.15059  789.88      681.8268898  1054.3095
    ## sales.15052  577.97      682.5141530  1074.9021
    ## sales.15045  871.62      897.0513896  1099.0337
    ## sales.15038  706.55      803.3264142  1117.9305
    ## sales.15031  749.78      670.5643295  1013.2605
    ## sales.15024  574.88      662.9087923   973.3011
    ## sales.15017  963.42      871.9331384  1087.4284
    ## sales.15010  812.04      875.3667316  1243.0121
    ## sales.15003  764.45      744.3030810  1078.5405
    ## sales.14996  634.20      716.1392056  1059.0000
    ## sales.14989  965.48      846.8372772  1170.4200
    ## sales.14982  926.88     1052.9336049  1202.8284
    ## sales.14975  588.81      614.5121199   728.9905
    ## 
    ## Results: 
    ## 
    ## end.post = 16067
    ##              Trt      Con Pct.Chng Linear.pVal Linear.Lower Linear.Upper
    ## sales   79833.83 79594.97     0.3%      0.0000       -16.5%       -11.6%
    ## Omnibus       --       --       --      0.0000           --           --

``` r
msynth_out <- msynth$w

m_weights <- msynth_out$Weights
```
