# jaspSurvival Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---
# jaspSurvival 0.96.6
## Fixed
* Improved y-axis label and grid spacing for the exponential canvas in parametric survival probability plots.

# jaspSurvival 0.96.5
## Added
* Added an exponential canvas to the parametric survival probability plot.
* Added optional censoring marks to the parametric survival probability plot.

## Changed
* Enabled prediction and probability-plot legend/color-palette controls only when the plot can display multiple fitted curves, while keeping theme controls available for selected plots.
* Increased probability plot width when a side legend is shown.
* Reduced probability plot axis text and title sizing by 10% for the JASP theme to avoid label overlap.

# jaspSurvival 0.96.4
## Features
* Added probability plot to parametric survival

# jaspSurvival 0.96.3
## Changed
* Added unit tests
* Updated README
