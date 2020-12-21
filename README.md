---
output: 
  html_document:
    keep_md: true
---

# EAT: Efficiency Analysis Trees

[Efficiency Analysis Trees](https://www.sciencedirect.com/science/article/pii/S0957417420306072) is an alghoritm by which a production frontier is obtained through and adaptation of regression trees based on CART. The generation of production frontiers falls within the field of efficiency analysis, of which some concepts must be known:

* A **production frontier** is a boundary defined for those feasible combinations of input and output that are efficient.
* A **DMU** (**D**ecision **M**aking **U**nits) is an observation of the dataset whose efficiency is to be assessed.
* A specific DMU is **efficient** when is located at the production frontier and it has room for improvement regarding its inputs or outputs when it is in the area below the frontier.

The `EAT` algorithm must be conceived as a modeling of the response variable (output) in order to know its most efficient levels for each of the different regions of the input space that are generated. Thus, subspaces with homogeneous DMUs (since they must share the characteristics of said subspace) are delimited and the maximun expected output for that subspace is provided. In this way, the `EAT` predictor results in a monotonic increasing frontier with a stepped form where each of these steps corresponds to a node of the tree which contains observations with efficient  and non-efficient output levels.




```r
library(eat)
data("PISAindex")
```

* EAT model with 1 input (`NBMC`) and 1 output (`S_PISA`).


```r
single_model <- EAT(data = PISAindex, 
                    x = 6,
                    y = 4,
                    numStop = 1,
                    fold = 5,
                    na.rm = TRUE)


|id |  N| Prop| R_PISA|   MSE|
|:--|--:|----:|------:|-----:|
|3  | 32|   45|    549| 38.68|
|4  | 15|   21|    420| 18.56|
|5  | 24|   34|    479| 33.86|
```

* Plot the frontier


```r
frontier(single_model,
         train.data = TRUE,
         train.color = "black",
         pch = 19,
         size = 1,
         rwn = TRUE)
```

![](README_files/figure-html/frontier-1.png)<!-- -->

* EAT model with 18 inputs and 3 outputs


```r
multioutput <- EAT(data = PISAindex, 
                   x = 6:18,
                   y = 3:5,
                   numStop = 6,
                   fold = 5,
                   na.rm = TRUE)


|id |  N| Prop| S_PISA| R_PISA| M_PISA|  MSE|
|:--|--:|----:|------:|------:|------:|----:|
|6  |  5|    7|    496|    438|    523| 8.05|
|10 |  6|    8|    438|    427|    516| 6.27|
|11 |  2|    3|    432|    437|    530| 2.40|
|12 |  5|    7|    440|    438|    523| 5.80|
|14 |  6|    8|    479|    440|    511| 3.76|
|15 |  4|    6|    496|    469|    512| 3.25|
|16 |  5|    7|    396|    466|    516| 9.13|
|17 |  6|    8|    377|    454|    518| 6.55|
|19 |  3|    4|    438|    479|    516| 8.26|
|20 |  5|    7|    432|    496|    502| 7.40|
|22 |  3|    4|    429|    523|    508| 4.17|
|27 |  3|    4|    440|    492|    520| 2.85|
|28 |  3|    4|    398|    492|    526| 2.61|
|29 |  5|    7|    403|    496|    530| 5.32|
|30 |  9|   13|    423|    530|    523| 7.29|
|31 |  1|    1|    429|    523|    526| 0.21|
```

* Ranking of importance of variables for EAT


```r
ranking_EAT(object = multioutput,
            r = 2,
            threshold = 75,
            barplot = TRUE)
[[1]]
        Importance
AAE         100.00
WS           96.03
S            84.36
HW           83.25
NBMC         73.26
AIC          68.30
GDP_PPP      67.89
ABK          63.20
EQ           61.84
I            61.37
PR           60.11
PS           51.74
PFC          43.08

[[2]]
```

![](README_files/figure-html/ranking-1.png)<!-- -->

* Plot an EAT model


```r
EAT_plot(object = multioutput)
```

![](README_files/figure-html/plot-1.png)<!-- -->

* Efficiency scores EAT


```r
scores_EAT <- efficiency_EAT(data = PISAindex,
                             x = 6:18,
                             y = 3:5,
                             object = multioutput,
                             scores_model = "EAT_BCC_out",
                             r = 2,
                             na.rm = TRUE)
    EAT_BCC_out
SGP        1.00
JPN        1.04
KOR        1.00
EST        1.00
NLD        1.01
POL        1.00
CHE        1.10
CAN        1.00
DNK        1.03
SVN        1.01
BEL        1.00
FIN        1.00
SWE        1.03
GBR        1.03
NOR        1.04
DEU        1.03
IRL        1.00
AUT        1.03
CZE        1.03
LVA        1.00
FRA        1.02
ISL        1.04
NZL        1.03
PRT        1.00
AUS        1.03
RUS        1.01
ITA        1.02
SVK        1.02
LUX        1.03
HUN        1.01
LTU        1.01
USA        1.00
BLR        1.04
MLT        1.05
HRV        1.03
ISR        1.05
TUR        1.00
UKR        1.00
CYP        1.10
GRC        1.08
SRB        1.01
MYS        1.00
ALB        1.00
BGR        1.00
ARE        1.00
MNE        1.01
ROU        1.01
KAZ        1.00
MDA        1.00
AZE        1.00
THA        1.01
URY        1.00
CHL        1.03
QAT        1.05
MEX        1.00
BIH        1.00
CRI        1.00
JOR        1.00
PER        1.05
GEO        1.04
MKD        1.04
LBN        1.04
COL        1.02
BRA        1.02
ARG        1.16
IDN        1.00
SAU        1.01
MAR        1.03
PAN        1.00
PHL        1.07
DOM        1.10


| Mean| Std. Dev.| Min| Q1| Median|   Q3|  Max|
|----:|---------:|---:|--:|------:|----:|----:|
| 1.02|      0.03|   1|  1|   1.01| 1.01| 1.16|
```

* Efficiency scores FDH


```r
scores_FDH <- efficiency_FDH(data = PISAindex,
                             x = 6:18,
                             y = 3:5,
                             scores_model = "FDH_BCC_out",
                             r = 4,
                             na.rm = TRUE)
    FDH_BCC_out
SGP      1.0000
JPN      1.0000
KOR      1.0000
EST      1.0000
NLD      1.0000
POL      1.0000
CHE      1.0019
CAN      1.0000
DNK      1.0138
SVN      1.0000
BEL      1.0000
FIN      1.0000
SWE      1.0119
GBR      1.0000
NOR      1.0261
DEU      1.0159
IRL      1.0000
AUT      1.0341
CZE      1.0000
LVA      1.0000
FRA      1.0000
ISL      1.0424
NZL      1.0059
PRT      1.0000
AUS      1.0159
RUS      1.0000
ITA      1.0000
SVK      1.0000
LUX      1.0000
HUN      1.0000
LTU      1.0000
USA      1.0000
BLR      1.0000
MLT      1.0000
HRV      1.0000
ISR      1.0000
TUR      1.0000
UKR      1.0000
CYP      1.0000
GRC      1.0067
SRB      1.0000
MYS      1.0000
ALB      1.0000
BGR      1.0000
ARE      1.0000
MNE      1.0000
ROU      1.0000
KAZ      1.0000
MDA      1.0000
AZE      1.0000
THA      1.0000
URY      1.0000
CHL      1.0000
QAT      1.0000
MEX      1.0000
BIH      1.0000
CRI      1.0000
JOR      1.0000
PER      1.0000
GEO      1.0000
MKD      1.0000
LBN      1.0000
COL      1.0000
BRA      1.0000
ARG      1.0000
IDN      1.0000
SAU      1.0000
MAR      1.0000
PAN      1.0000
PHL      1.0000
DOM      1.0000


| Mean| Std. Dev.| Min| Q1| Median| Q3|  Max|
|----:|---------:|---:|--:|------:|--:|----:|
|    1|      0.01|   1|  1|      1|  1| 1.04|
```

* Efficiency jitter plot


```r
efficiency_jitter(object = multioutput,
                  scores_EAT = scores_EAT$EAT_BCC_out,
                  scores_model = "EAT_BCC_out",
                  upb = NULL,
                  lwb = NULL)
```

![](README_files/figure-html/jitter-1.png)<!-- -->

* Efficiency density plot


```r
efficiency_density(scores_EAT = scores_EAT$EAT_BCC_out,
                   scores_FDH = scores_FDH$FDH_BCC_out)
```

![](README_files/figure-html/density-1.png)<!-- -->

* EAT predict 


```r
predict_EAT(object = multioutput,
            newdata = PISAindex[, 6:18])
   S_PISA_pred R_PISA_pred M_PISA_pred
1          551         549         569
2          551         549         569
3          522         520         526
4          530         523         526
5          522         520         526
6          511         512         516
7          551         549         569
8          518         520         516
9          522         520         526
10         511         512         516
11         502         505         508
12         522         520         526
13         522         520         526
14         522         520         526
15         522         520         526
16         518         520         516
17         518         520         516
18         511         512         516
19         511         512         516
20         487         479         496
21         502         505         508
22         511         512         516
23         522         520         526
24         492         492         496
25         522         520         526
26         487         479         496
27         492         492         496
28         487         479         496
29         492         492         496
30         487         479         496
31         487         479         496
32         492         492         496
33         502         505         508
34         492         492         496
35         492         492         496
36         492         492         496
37         492         492         496
38         469         466         454
39         469         466         454
40         487         479         496
41         492         492         496
42         469         466         454
43         438         432         440
44         429         427         437
45         429         427         437
46         438         432         440
47         429         427         437
48         438         432         440
49         398         403         423
50         429         427         437
51         398         403         423
52         429         419         423
53         429         427         437
54         469         466         454
55         438         432         440
56         429         420         423
57         398         403         423
58         429         427         437
59         429         419         423
60         429         420         423
61         398         403         423
62         429         420         423
63         398         403         423
64         429         420         423
65         429         420         423
66         469         466         454
67         396         377         379
68         398         403         423
69         396         377         379
70         396         377         379
71         396         377         379
72         396         377         379
```

* RFEAT model


```r
RFEAT_model <- RFEAT(data = PISAindex,
                     x = 6:18,
                     y = 3:5,
                     numStop = 5,
                     m = 5,
                     s_mtry = "Breiman",
                     na.rm = TRUE)
```

* RFEAT ranking


```r
ranking_RFEAT(object = RFEAT_model,
              r = 4,
              barplot = TRUE)
[[1]]
         Importance
I        12.3179400
GDP_PPP  12.2329848
PFC       9.9112319
PR        8.9566402
AAE       5.3414444
AIC       2.7864351
NBMC      2.6985891
ABK       0.6837576
HW       -0.2568167
WS       -0.7562322
S        -1.4668550
PS       -2.9092153
EQ      -11.8324061

[[2]]
```

![](README_files/figure-html/RFEAT_ranking-1.png)<!-- -->

* RFEAT scores


```r
efficiency_RFEAT(data = PISAindex,
                 x = 6:18,
                 y = 3:5,
                 object = RFEAT_model)
     scoreRF
SGP 0.930756
JPN 0.985633
KOR 0.977947
EST 0.983174
NLD 0.995761
POL 0.971875
CHE 1.030680
CAN 0.999231
DNK 1.013360
SVN 1.000000
BEL 0.993701
FIN 1.002308
SWE 1.011858
GBR 1.003960
NOR 1.041283
DEU 1.012724
IRL 1.001931
AUT 1.024048
CZE 1.016499
LVA 1.002823
FRA 1.002424
ISL 1.056970
NZL 1.012205
PRT 0.990650
AUS 1.012326
RUS 0.965164
ITA 1.001232
SVK 0.970782
LUX 1.013836
HUN 0.977963
LTU 1.003734
USA 0.995248
BLR 1.001688
MLT 1.009091
HRV 0.979541
ISR 1.014468
TUR 0.954506
UKR 0.937768
CYP 1.065188
GRC 1.005689
SRB 1.019643
MYS 1.001826
ALB 0.989931
BGR 1.016514
ARE 0.981019
MNE 0.994299
ROU 1.005116
KAZ 1.039716
MDA 0.975943
AZE 0.990952
THA 0.984977
URY 1.064637
CHL 1.026106
QAT 1.036754
MEX 0.983333
BIH 1.002978
CRI 1.067136
JOR 1.003341
PER 0.994015
GEO 1.044221
MKD 0.994673
LBN 1.022901
COL 0.998058
BRA 0.977240
ARG 1.072637
IDN 0.980808
SAU 1.017043
MAR 1.011957
PAN 1.044032
PHL 1.054958
DOM 1.095322
```

* RFEAT predict


```r
predict_RFEAT(object = RFEAT_model,
              newdata = PISAindex[, 6:18])
   S_PISA_pred R_PISA_pred M_PISA_pred
1        518.0       517.8       529.6
2        521.4       514.8       527.0
3        508.2       506.4       514.4
4        521.4       514.2       519.2
5        509.0       510.6       516.8
6        499.2       497.6       503.4
7        526.6       523.8       530.8
8        522.2       519.6       520.8
9        509.4       509.4       515.8
10       507.0       503.2       509.0
11       504.2       502.0       504.8
12       525.8       521.2       523.0
13       509.2       512.0       515.6
14       507.0       506.4       509.4
15       524.4       519.6       523.0
16       509.4       509.2       513.8
17       514.2       519.0       520.6
18       507.0       506.4       511.0
19       505.2       505.0       507.6
20       491.6       486.6       497.4
21       499.2       499.6       496.2
22       516.2       515.0       523.2
23       514.2       516.6       520.0
24       489.6       489.2       487.4
25       509.2       512.0       515.0
26       467.8       464.2       471.0
27       475.6       476.6       487.6
28       461.4       462.6       471.8
29       483.6       479.0       492.0
30       470.4       467.6       478.6
31       483.8       477.8       489.8
32       481.2       482.0       481.4
33       500.2       502.6       496.2
34       477.2       474.8       478.2
35       466.2       465.6       480.8
36       472.4       469.2       478.4
37       473.8       476.8       480.8
38       450.2       444.8       446.4
39       440.0       437.0       441.4
40       476.2       475.2       480.4
41       461.6       459.6       464.6
42       450.6       448.2       456.8
43       438.8       430.8       443.2
44       433.2       423.0       432.6
45       433.8       429.4       443.2
46       427.4       423.8       428.8
47       420.6       418.6       430.8
48       434.4       431.2       432.2
49       432.4       424.4       439.8
50       422.6       413.8       423.2
51       410.8       400.6       416.2
52       419.6       407.4       415.6
53       454.8       454.6       455.0
54       461.4       463.8       458.4
55       434.4       432.8       431.6
56       421.4       413.0       414.4
57       410.8       404.2       416.2
58       454.8       454.6       455.0
59       430.8       420.4       419.8
60       410.4       398.6       407.6
61       413.0       408.8       415.6
62       410.8       404.2       416.2
63       403.0       394.0       402.0
64       424.6       411.2       415.6
65       417.6       403.6       413.2
66       437.4       431.2       432.4
67       388.4       374.6       374.6
68       412.2       405.8       407.8
69       384.6       371.0       372.4
70       403.0       393.6       389.6
71       384.6       371.0       372.4
72       388.4       374.6       374.6
```
