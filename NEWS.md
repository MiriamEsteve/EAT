# eat 0.1.0

Bug fixes:

- In `rankingRFEAT()`, `digits` argument did not work. Now it is fixed.

- In `efficiencyEAT()` and `efficiencyCEAT()`, results were incorrect when setting `scores_model == "WAM.RAM'`. Now, it is fixed. 

- Now, the descriptive tables of scores are shown in a single table in `efficiencyEAT()`, `efficiencyCEAT()` and `efficiencyRFEAT()`. 

- `predictEAT()` and `predictRFEAT()` have been unified in the generic function `predict()`.

# eat 0.1.1

Bug fixes:

- `predict()` did not work properly when a `RFEAT` object was introduced. Now, it is fixed.

# eat 0.1.2

- Renamed `size()` as `EAT_size()`.

- Renamed `frontier.levels()` as `EAT_frontier_levels()`.

- Renamed `descrEAT` as `EAT_leaf_stats()`.

- Argument `print.table` included in `efficiencyEAT()`, `efficiencyCEAT()` and `efficiencyRFEAT`. Now, the summary descriptive table of the efficiency scores is optionally displayed.

- Now, `efficiencyEAT()`, `efficiencyCEAT()` and `efficiencyRFEAT` only return the efficiency scores.

- Now, `predict()` only returns predictions.