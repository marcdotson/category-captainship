Data Cleaning and Preparation Process
================
Cameron Bale
8/11/2020

# Overview

This document outlines the process we use to clean and process the
original scanner data so that it is ready for modeling.

### Data Cleaning

There is one cleaning file for each category. These files will have
names with the following form: `XXXX_cleaning.Rmd`.

#### Getting Set Up

The first step is getting RStudio connected to the Github repository.
Marc Dotson has materials that can guide in this process. Cloning the
repository to your local machine will create a folder
`category-captainship`. This is where everything for the project will
live.

The Google Drive folder shared with the coauthors and RAs on this
project contains the original data files in the `Original Datasets`
folder. To work with a category, the corresponding (`XXXX.tgz`) file
must be downloaded to your local machine. Store this file in a folder
`category-captainship/Data/Category-Name/` on your local computer (where
you swap out Category-Name for whatever category you are working on).
Within `category-captainship/Data/Category-Name/` there should be a
folder for each state that we are analyzing: Minnesota, Missouri, North
Dakota, and Illinois, like this:
`category-captainship/Data/Category-Name/Illinois/`. Once you are done
running a cleaning file, the cleaned data for a given location will be
located in the corresponding folder.  

#### Cleaning Process

The process for cleaning the data for each category should generally be
the same. There may be some unique cases, and we can meet to discuss
these. The original cleaning file
`category-captainship/Code/Cereal/cereal_cleaning.Rmd` should serve as a
template for cleaning all other categories.

For now, there are five data files that will be use in the cleaning
process:

  - Stores files contain information about each individual store
    location.
  - Movement files contain price and quantity of goods sold at specific
    stores on specific weeks.
  - The products master file contains information on every upc in the
    movement files.
  - RMS versions files track different versions of upc codes over time.
    A new version of a upc code is generated whenever a core product
    attribute changes: Product module, brand, size, or multi.
  - `Manufacturer UPC Prefixes - From GS1 Company Database.csv` is a
    living file that we update whenever we clean a category with a new
    captain or validator (more on this later).

Generally speaking, the cleaning process involves combining the data
from all of these files such that we obtain weekly sales for every upc
in every store in every week, with associated upc and store
characteristics.

Finally, once the data for a category is cleaned, please remember to
upload the cleaned data to Google Drive. There is a `Cleaned Data`
folder, with sub-folders for each category. Please upload the full
cleaned data for a category, e.g., `cereal_clean.RData`, as well as the
subsets of this data corresponding to each location, e.g.,
`mn_cereal_clean.RData`.

#### Category Captainship Implementation

There are multiple years of data for each product category. We generally
would like to have at least a year of data before and after category
captainship implementation. Use the `Category Captainship Scheduled`
document in Google Drive to determine the `Target Reset Date` for a
given category. This is when category captainship was scheduled to be
implemented, and this is the treatment date we will use in our model. If
a treatment occurs at the very beginning of a year (January), use the
data for the year before and the year during which the treatment
occurred. If treatment occurs in the middle of a year, use the year
before treatment, the year during which treatment occurred, and the year
after treatment (e.g., if treatment occurs in May of 2012, combine data
for 2011, 2012, 2013).

In the `Category Captainship Scheduled` document, there is a variable
that indicates whether a category received a `Full` treatment on the
`Target Reset Date` or if it was an `Update`. Of main interest is the
`Full` treatment, but if a category received an `Update`, be sure the
time window of the data includes this.

The other thing to note in the `Category Captainship Scheduled` document
is the `Vendor Lead` (captain) and `Validator` (validator)
manufacturers. These are the manufacturers that we need to obtain UPC
prefixes for…

#### Manufacturer UPC Prefixes: GS1 Company Database

Use the [GS1 Company
Database](https://www.gs1us.org/tools/gs1-company-database-gepir) to
obtain all UPC prefixes for the captain and validator for all
categories. Right now, we don’t have a better way (e.g., API) to access
these prefixes other than searching manually based on the company name
and copy/paste the codes into the `Manufacturer UPC Prefixes - From GS1
Company Database.csv` file which  stored in the `Data` folder in Github.
We have already assembled quite a few prefixes. But, if you clean a
category and the captain or validator is not in this file already,
please use the database to add the new prefixes for the new
manufacturer(s).

### Questions

Any questions about the data structure or properties can probably be
answered by reading the `Retail Scanner Dataset Manual`, found in the
`Category Captainship - Empirical Generalization` folder in Google
Drive.

If any inconsistencies are uncovered in the data or in the data cleaning
process, please alert the authors and create an `Issue` on Github so we
can track any ongoing issues.
