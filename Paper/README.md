Paper
================

## Description

The manuscript of the paper, without any PDF knits.

## Finding the treated retailer

### North Dakota

Supervalu owns a chain called Hornbacher’s that has stores in both
Moorhead MN and Fargo ND. Hornbacher’s had 6 stores in 2012. We found
the unique dma descriptions in the Minnesota data and filtered for
“Minneapolis-St Paul MN” and “Fargo-Valley City ND” dma descriptions
since Cub foods is located in Minneapolis/St. Paul and Hornbacher’s is
located in Fargo/Moorhead. We were hoping to find an overlap between the
two locations, but the Minneapolis/St.Paul location is so large that it
includes all the retailer codes in the data. Next, we looked at the
number of stores that have a “Fargo-Valley City ND” dma description but
are located in MN. There are two retailers with stores in MN that have
Fargo dma descriptions. Both retailers have one store in the MN data.
Then we found the retailer codes with a “Fargo-Valley City ND” dma
description in the North Dakota data. There are two overlapping retailer
codes between North Dakota and Minnesota, one of them is likely to be
Hornbacher’s.

One of the retailers has 1 store in MN and 5 stores in ND that both have
dma description of Fargo-Valley City ND. Hornbacher’s had 5 stores in ND
in 2012 (Northport, Village West, Express, Southgate, and Osgood) and 1
store in MN in 2012 (Moorhead-101 11th st). Given this information the
selected retailer is Hornbacher’s and is our treated retailer. We used
the wayback machine to find Hornbacher’s and Supervalu’s sites in 2012,
which reports the number of stores and their locations.

### Minnesota

Supervalu’s retailer in Minnesota is Cub Foods, focused manily in the
Minneapolis market. To find the treated retailer in Minnesota, we
checked the number of stores in each MN retailer and location of those
stores in order to match with the wayback machine locations online. We
used unique county and zipcode descriptions to see how many stores are
in each location in attempt to match with Cub Foods. The wayback machine
does not have archived all the stores locations in 2012, so we had to
use the Cub Food stores current locations. The locations are likely
similar. When we compare the current location to the retailer options in
our data, it is clear who the treated retailer is. Five out of nine
zipcodes are an exact match between our picked retailer and Cub Foods
(558, 559, 560, 562, 565). Zipcode 550 has 11 stores in Cub Foods and 10
stores in our selected retailer. The last three zipcodes (551, 553, 554)
are close but missing a few stores in the actual data compared to the
stores online. The total number of stores under our selected retailer is
43. According to the supervalu website, there were 46 total stores in
2012. This retailer has the number of stores that is closest to matching
46.

### Missouri

The way back machine does not show the Missouri locations for
Shop’n’Save. All MO Shop’n’Save stores are closing or being sold. I
found a website that listed 6 stores in zip 631 and 5 stores in 630 that
are closing
(<https://www.kmov.com/news/these-shop-n-save-stores-are-on-closure-list-if/article_d4f853da-bb86-11e8-a565-635fd47aaf94.html>).
There is only one retailer with stores in zipcodes that start with 631
and 630. All St. Louis zipcodes start with 631, and as the Supervalu
website said their key Shop’n’Save market was in St. Louis, this is also
evidence for the selected retailer. We can also check the dma
descriptions for the retailers in Missouri. There are only two retailers
with dma descriptions in St. Louis. This combined with the other
retailer tests previously done, leads us to believe the selected
retailer is most likely Supervalu.

### Illinois

The supervalu website claims it has 182 stores in the Chicago market.
One retailer in the data has 161 stores. The next highest number of
stores for any retailer code is 78. Additionally, all 161 stores are
contained in the Chicago DMA description. This leads us to believe that
the selected retailer is the Supervalu retailer located in Chicago.

License: This work is licensed under a [Creative Commons
Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-sa/4.0/).
