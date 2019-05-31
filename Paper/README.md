Paper
================

## Data
We used four different methods in order to discover the retailer who receieved category captainship. Two methods used a synthetic control approach, one method used regression discontinuity theory, and lastly, we looked at the number of stores in each retailer to verify our results. The first synthetic control method was done using sales as the matching and outcome variable.  The sales variable is calculated as price times the number of units sold, reported weekly for each UPC in each store and it is measured in dollars. In "Jeff's Other Paper", Dotson found that "sales increase for captain, validator, private label". By creating a synthetic control for each retailer in each category within each state and analyzing the significant effects, we can pick out the retailer with the most significant positive effects on sales. To further confirm, we ran the synthetic control on every combination of retailers, excluding retailers with only one store. First, we did the analysis on simulated data, in which we created three reatilers (A, B, and C). In our simulated data synthetic control, the stores in retailer A were treated and had a 25% increase on sales following our choosen implementation date. The retailer combinations were made by treating each retailer as the treated retailer and choosing one other retailer as a control. For example, when retailer B was choosen as the treated retailer, we ran one microsynth using the stores in retailer A as the control, and one using the stores in retailer C as the control. In this test, we saw positive effects on sales when retailer A was chosen as the treated retailer, and negative effects on sales when it was chosen as the control as we expected. This retailer combination synthetic control was done in each category within each state as the first step in finding the treated retailer. From this step, we could pick out the retailer (or two) that were most likely to have been treated. The next step was to perform the same synthetic control analysis using market share as the matching and outcome variable. The analysis was done at the manufacturer level for the captain, validator, and private label. Market share was calculated by dividing the selected manufacturer's weekly sales by the total weekly sales within that store within a category. As a third check, we created the captain's market share, defined as the total number of unique products sold within a week within a store for the captain, divided by the total number of unique products sold in that store in that week. This was plotted with a vertical line at the implementation date, for each retailer within a state by category. We expected to see a change in assortment or market share around the time of implementation. Finally, we looked at the number of stores the treated retailer reported on their website in 2012 for each area. We compared this with our estimate of the treated retailer in order to verify. As the treated retailer would be unlikely to have one or only a couple of stores in a given area. 

















License: This work is licensed under a [Creative Commons
Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-sa/4.0/).
