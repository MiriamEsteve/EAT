# eat 0.1.0

Bug fixes:

- In `rankingRFEAT()`, `digits` argument did not work. Now it is fixed.

- In `efficiencyEAT()` and `efficiencyCEAT()`, results were incorrect when setting `scores_model == "WAM.RAM'`. Now, it is fixed. 

- Now, the descriptive tables of scores are shown in a single table in `efficiencyEAT()`, `efficiencyCEAT()` and `efficiencyRFEAT()`. 

- `predictEAT()` and `predictRFEAT()` have been unified in the generic function `predict()`.

# eat 0.1.1

Bug fixes:

- `predict()` did not work properly when a `RFEAT` object was introduced. Now, it is fixed. 