# eat 0.1.0

Bug fixes:

- In `rankingRFEAT()`, `digits` argument did not work. Now it is fixed.

- In `efficiencyEAT()` and `efficiencyCEAT()`, results were incorrect when setting `scores_model == "WAM.RAM'`. Not it is fixed. 

- Now, in `efficiencyEAT()`, `efficiencyCEAT()` and `efficiencyRFEAT()` descriptive table of scores are shown in only one table.

- `predictEAT()` and `predictRFEAT()` varied their predictions depending on the position of the inputs. Now it is fixed.