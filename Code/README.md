Code
================
## Data Cleaning Markdowns
#### These files contain the code to clean and merge the data for each category. They are found in the respective category folders. 
The data cleaning markdown files downloads the movement and store files for 2011, 2012, and 2013 and the products file. The code combines all three years of data to create `full_11_12_13`. The combined data only includes food, it is filtered for channel "F" and the four states we are interested in (North Dakota, Washington DC, Minnesota, and Missouri). Any store codes that do not exist in all three years of data are removed from `full_11_12_13`.  Any stores that switch parent or retail codes within `full_11_12_13` are removed from the data. Retailer codes that are equal to NA are replaced with their parent code. `full_11_12_13` is then saved as "category_clean.RData". The data is then filtered for each state and saved by state as well. For example, Minnesota data would be saved as "mn_category_clean.RData". 

## Graph Analysis Markdowns
#### These files contain the code to create graphs of sales and price by manufacturer and retailer. They are found in the respective state folder within the category folder. 
The code loads in the cleaned data sets created by the data cleaning markdowns. `main_manuf_sales` is created by filtering for sales by brands in the captain, validator, and private label. Each category has a unique captain, validator, and private label, as well as implementation date (these can be found in the "Category Captainship Schedule" files). The code uses a `for` loop to create graphs for each manufacturer in each retailer. The graphs contain weekly average price and weekly sales, a vertical line at the implementation date, and regression lines pre and post implementation. 

## Retailer Test Markdowns
#### These files run a series of microsynth tests in hopes of uncovering the treated retailers. 
The retailer tests are done for sales and market share. Each file runs a series of synthetic controls, each retailer in a location is selected to be the treated retailer. The results for the true treated retailer should be positive and significant, allowing us to select out the treated retailer among the tested retailers. The synthetic controls were run at different levels (manufacturer, store). The most informative retailer test was the retailer combination test. This test paired each retailer with another retailer in the area. One was selected as the treated retailer and the other as the control. Note that retailers with only one store could not act as the synthetic control, and those combinations were dropped. We expected to see positive significant effects in the cases where the true treated retailer was selected as treated and negative significant effects when the true treated retailer was selected as the control.

## Store Location Analysis
### This file explains how we selected the treated retailers. 
This file goes through each state explaining why we believe the selected retailer is the actual treated retailer. The file does contain some code for analyzing store location and the number of stores in a state. 


## UPC Chart Markdowns
#### These files create heat charts for upc sales and prices across the four states for each category. 


## Errors
#### The following are errors we have run into while writing the code and potential ways to fix them. 
* "Error in value[[3L]](cond) : promise already under evaluation: recursive default argument reference or earlier problems?" : check the number of stores in each retailer. If there is only one store in a retailer, it may be causing this issue. Remove the cases where that retailer acts as a control. 
* "dum <= dum+1 : missing value where TRUE or FALSE needed" : make sure every unit in the data has an observation for every time period. 


