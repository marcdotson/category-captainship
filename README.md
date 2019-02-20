An Empirical Generalization of the Effects of Category Captainship
================

## Abstract

This paper makes use of Nielsen data to investigate the generalizability
of category captainship and its impact on retailers. Category
captainship is when a retailer selects a category captain to manage the
product line assortment and optimization for all of the brands within a
category, including the retailer’s private label and captain’s own
brand. This has been shown to be optimal for the retailer and the
captain and not as detrimental to the other brands as might be expected.
But does this phenomenon generalize across many retailers? What
characteristics attenuate the success of the practice across retailers?

For general details on GitHub usage, project organization, and project
workflow, see [Research Assistant
Training](https://github.com/marcdotson/ra-training).

## Project Organization

### Project Description

  - general effects of category captainship
  - reference old paper: On the Competitive and Collaborative
    Implications of Category Captainship
  - extend and apply across multiple chains, markets, locations and
    categories to get better understanding of overall effects
  - Models:
      * Hierarchical Difference in Differences
      * Synthetic Control
      * Synthetic Difference in Differences

### Data

  - Nielsen data, retail scanner data (RMS), from the Kilts Center
    archive
  - weekly pricing, volume, store merchandising conditions
  - 35,000 grocery, drug, mass-merchandiser, and other stores (grocery
    for us)
  - stores are from approximately 90 retail chains
  - food stores, data represents 53% of all commodity volume (ACV)
  - 2011 to 2013
  - North Dakota, Washington DC, Minnesota, Missouri
  - Not every Nielsen retail cooperator has agreed to share their
    scanner data with the Kilts Center, but for retailers that do
    participate, typically all stores within the 48 contiguous states
    are included
  - 3 major file types:
      - stores: individual store locations  
      - products: UPC info  
      - movement: price and quantity of goods sold at specific stores on
        a specific week
  - since movement files are so large, there is one file for each
    product module code (category?) for each year

### Working with the Data

  - Data Merging/Cleaning: For each category and each year, store and
    movement files were merged based on store code and filtered for
    observations in the four areas where Supervalu Operates: North
    Dakota, Washington DC, Minnesota, and Missouri. Then this file was
    merged with the products master file based on UPC code. We then
    combined the merged store, movement, and product file for each year
    so that we had data spanning at least a year prior and a year post
    Category Captainship implementation. Next, we filtered out stores
    that switch parent or retailer codes.

### Discovering Supervalu

  - Graph method: There is a variable that identifies the brand for each
    product in our data, and we use the brand variable to tie category
    captains and validators to all of their products. We then filter for
    these products and create a new variable corresponding to each
    product’s manufacturer so that we can aggregate sales at the
    manufacturer level for each retailer. We plot aggregate sales for
    the category captain, validator, and store brand within each
    retailer. Similarly, we graph the average price level for category
    captain, validator, and store brand products. We expect to see
    increases in sales for captains and store brands post category
    captainship implementation, as well as significant changes in the
    average price level for captains and store brands. These graphs were
    created for the following categories for every retailer in the four
    Supervalu locations:
    
      - Ready-to-Eat Cereal
      - Spreads and Jams
      - Pickles and Olives
      - Peanut Butter
      - Novelties
      - Lunchmeat
      - Ice Cream
      - Frozen Dinners
      - Canned Soup

The graph method did not clearly indicate which retailer codes
correspond to the Supervalu chains we are interested in, so we tried
another method of identification.

  - Table method: To discover Supervalue, we created scaled sales charts
    for the top 100 upcs (in terms of sales) within each retailer in
    each category. These charts are at the weekly level such that each
    row represents a upc and each cell across the rows is that week’s
    sales amount divided by the average sale amount for that upc over
    the entire time period of the data. Additionally, the cells are
    filled with a red color gradient that gets darker when sales are
    higher, and lighter (or white) when sales are lower (or zero). We
    expect to see significant changes (products introduced,
    discontinued, or large shifts in the level of sales) within a month
    prior and post category captainship implementation and refresh for
    Supervalu retailers. We have been investigating each chart for these
    changes. When we see something significant, we track the upc code,
    what company manufactures the product, and what type of change
    occurred. We expect to see the most changes happen for captain,
    validator, and private label products. We track all changes for each
    retailer in each category, then compare the changes across retailer
    codes. We believe this method has been successful in identifying
    Supervalu retailer codes.

### Modeling

  - Currently in progress
