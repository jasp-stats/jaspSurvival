<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspSurvival/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspSurvival/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspSurvival/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspSurvival)
<br>
<b>Maintainer:</b> <a href="https://github.com/FBartos/">František Bartoš</a>

</div>

# The Survival Module

## Overview

<img src='inst/icons/survival-analysis.svg' width='149' height='109' align='right'/>

**JASP Survival module** is an add-on module for JASP that provides tools for analyzing censored time-to-event data. The Survival module offers non-parametric Kaplan-Meier survival curves and life tables, semi-parametric Cox proportional hazards regression, and parametric survival models for several distributional families.

The module supports censoring summaries, group comparisons, model fit statistics, coefficient and hazard ratio estimates, diagnostic plots, and predictions for survival time, survival probability, hazard, cumulative hazard, and restricted mean survival time.


## R Packages

<img src='https://www.r-project.org/logo/Rlogo.svg' width='100' height='78' align='right'/>

The functionality is served by several R packages

- **survival** - Core survival analysis routines, including survival objects, Kaplan-Meier estimates, log-rank-style tests, and Cox proportional hazards models ([survival on CRAN](https://cran.r-project.org/package=survival))
- **flexsurv** - Flexible parametric survival modeling and prediction for a range of distributional families ([flexsurv on CRAN](https://cran.r-project.org/package=flexsurv))
- **ggsurvfit** - Survival curve visualizations based on ggplot2 ([ggsurvfit on CRAN](https://cran.r-project.org/package=ggsurvfit))
