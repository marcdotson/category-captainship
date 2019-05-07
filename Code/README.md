Code
================

## Description

Each script should do something specific (like tidyverse functions),
have a descriptive name, and include number prefixes if they are meant
to be run in a certain order (e.g., `01_import_data.R`,
`02_clean_data.R`).

## Project Workflow

  - Use RStudio projects.
  - Use `here::here()` to specify files paths.
  - Use tidyverse functions as much as possible.
  - Try and follow the [tidyverse style
    guide](https://style.tidyverse.org).

## Data Cleaning Markdowns
# These files contain the code to clean and merge the data for each category. They are found in the respective category folders. 
The data cleaning markdown files downloads the movement and store files for 2011, 2012, and 2013 and the products file. The code combines all three years of data to create `full_11_12_13`. The combined data only includes food, it is filtered for channel "F" and the four states we are interested in (North Dakota, Washington DC, Minnesota, and Missouri). Any store codes that do not exist in all three years of data are removed from `full_11_12_13`.  Any stores that switch parent or retail codes within `full_11_12_13` are removed from the data. Retailer codes that are equal to NA are replaced with their parent code. `full_11_12_13` is then saved as "category_clean.RData". The data is then filtered for each state and saved by state as well. For example, Minnesota data would be saved as "mn_category_clean.RData". 









