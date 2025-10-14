# Expected Threat (xT) in NBA Play-By-Play Data
## STAT 479 - Sports Analytics - UW-Madison
### Authors: Mason Langer, Rohan Ahuja, Hazim bin Mohd Izani

Based on derived metrics like [Expected Threat (xT)](https://www.hudl.com/blog/possession-value-models-explained) for play build-up in soccer game flow, we aimed to derive expected possession values based on start and end action types, location statistics, and conditional probabilities through NBA play-by-play data. With estimates for initial possession value based on how a play was initiated and where it started (iEPV), and expected ending value of a possession (xPts), modelled with glm and Random Forest regressors respectively, we derived xT as xT = iEPV+xPts. From our analysis, xT was found to be a quality estimator in regard to statistically modelling MVP-caliber players, as well as high-impact, high efficiency three point shooters. 

### Repository Overview
For an elaborate report on how data was gathered and processed, how models were produced and why, and detailed findings, please refer to the [report/](https://github.com/masonlanger/STAT-479-NBA-xThreat/tree/main/report) directory.

1. Data: data/ALL_SEQUENCES_2024.csv
    a. [hoopR](https://cran.r-project.org/web/packages/hoopR/index.html) was the primary source of play-by-play data, especially with detailed information regarding play X and Y coordinates, shot types, and possession results. For more information on the source of this data, please refer to the [notebooks/](https://github.com/masonlanger/STAT-479-NBA-xThreat/tree/main/notebooks) directory.
2. Scripts: scripts/GetSequenceData.R
    a. Multithreaded script to fetch, parse, and restructure play-by-play data into their relevant "sequences," where each row contains information related to how a play started and ended. **WARNING**: this script will take hours to run on most computers, due to throttling.
