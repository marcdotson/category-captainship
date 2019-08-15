Paper
================

## Data
  Our scanner data comes from the Kilts Center for marketing. It contains data from approximately 35,000 drug, grocery, mass merchandiser, and other stores detailing weekly pricing, volume, and store merchandising conditions. For the purpose of our C.C. analysis, we isolate the data to grocery data only. If a retailer has elected to share data with the Kilts Center, typically all stores in the retail chain in the 48 contiguous states are included in the data. We leveraged this fact along with empirical evidence explained later to determine which retailers implemented category captainship. In rare cases a small number of stores were omitted from the sample due to privacy concerns. We believe this did not affect our analysis.
  
  Stores files contain information about each store in the data. These files contain the code used to identify the store, the year, the stores' retailer and parent company codes, the channel code (e.g. `F` for food), and variables for state, county, zip, and dma information.
  
  Movement files track upc sales volume and average weekly price. An individual observation records the upc code, the end of the week long measurement period, total units sold in the week, average price, whether the product was featured or on display, and the store code.
  
  The rms versions file tracks the upc codes, the upc code version, and what year that version of code is found in.
  
  The products file contains all upc codes found in the data with corresponding upc characteristics such as brand and size. The products file also tracks different versions of the same upc which are created when one or more of four core attributes (product module code, brand code, how many units appear in a given pack, and size code) changes.
  
  It is important to join the rms versions file to the movement file before merging the products file or else the merge results in millions of duplicated observations due to multiple upc versions from the products file being merged to the movement file.
  
  The time period we consider is from January of 2011 to December of 2013.
  
  Significant time was spent cleaning and merging the scanner data. We first join the movement, store, rms versions, and products files while filtering for the four states with supervalue retailers that we are analyzing: North Dakota, Minnesota, Missouri, and Illinois. We did this for the movement, store, rms versions, and products files on a yearly basis before combining the data for all of the years. We removed store codes that switch retailer and/or parent codes or that are missing from any of the years in the data. We replaced missing retailer code values with the parent code.
  
  Graph analysis files, e.g. `mn_cereal_graph_analysis.Rmd`, create plots of aggregated sales and average price at the manufacturer level. We plotted aggregate sales and average price for the category captain, validator, and private label for each retailer, giving a total of two plots for each retailer in a state. We expected to see _________________ (insert findings from 'On the Competitive and Collaborative Implications...).____________ Due to the large amount of weekly variation in sales and average price it was difficult to visually distinguish any effects at the CC implementation date.
  
  UPC chart code, e.g. `cereal_upc_chart.R`, creates tables containing the top 100 upc codes in terms of overall sales. Each cell contains the sales in dollars for a given week divided by the average sales over the entire time period. Cell values less than one indicate lower than average sales, while cell values greater than one indicate higher than average sales. The tables are color coded to help with spotting patterns in sales. Our goal was to use these tables to look for UPCs which had large changes in sales or were introduced or discontinued within a month before and after the CC implementation date. The charts were somewhat difficult to read and we weren't able to use them on their own to discover the treated retailers, although they helped to narrow the field of possible treated retailer codes.
  
  We used four different methods in order to discover the retailer who receieved category captainship. Two methods used a synthetic control approach, one method used regression discontinuity theory, and lastly, we looked at the number of stores in each retailer to verify our results. The first synthetic control method was done using sales as the matching and outcome variable.  The sales variable is calculated as price times the number of units sold, reported weekly for each UPC in each store and it is measured in dollars. In "Jeff's Other Paper", Dotson found that "sales increase for captain, validator, private label". By creating a synthetic control for each retailer in each category within each state and analyzing the significant effects, we can pick out the retailer with the most significant positive effects on sales. To further confirm, we ran the synthetic control on every combination of retailers, excluding retailers with only one store. First, we did the analysis on simulated data, in which we created three reatilers (A, B, and C). In our simulated data synthetic control, the stores in retailer A were treated and had a 25% increase on sales following our choosen implementation date. The retailer combinations were made by treating each retailer as the treated retailer and choosing one other retailer as a control. For example, when retailer B was choosen as the treated retailer, we ran one microsynth using the stores in retailer A as the control, and one using the stores in retailer C as the control. In this test, we saw positive effects on sales when retailer A was chosen as the treated retailer, and negative effects on sales when it was chosen as the control as we expected. This retailer combination synthetic control was done in each category within each state as the first step in finding the treated retailer. From this step, we could pick out the retailer (or two) that were most likely to have been treated. The next step was to perform the same synthetic control analysis using market share as the matching and outcome variable. The analysis was done at the manufacturer level for the captain, validator, and private label. Market share was calculated by dividing the selected manufacturer's weekly sales by the total weekly sales within that store within a category. As a third check, we created the captain's market share, defined as the total number of unique products sold within a week within a store for the captain, divided by the total number of unique products sold in that store in that week. This was plotted with a vertical line at the implementation date, for each retailer within a state by category. We expected to see a change in assortment or market share around the time of implementation. Finally, we looked at the number of stores the treated retailer reported on their website in 2012 for each area. We compared this with the number of stores that our assumed treated retailer contained in the data in order to verify. As the treated retailer would be unlikely to have one or only a couple of stores in a given area, or a number of stores significantly different from the number in the data.
  

















License: This work is licensed under a [Creative Commons
Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-sa/4.0/).
