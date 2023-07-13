## phenolocrop

### Overview

`phenolocrop` has been developed for the time-series analysis of crop traits. In time-series analysis, one data is composed of serial observation and interpreted by means of "time-series models." Therefore, `phenolocrop` is aimed at applying appropriate time-series model to the time-series observation of crop, which is called phenology.

### Installation

    install.packages("phenolocrop")

### Target traits and models

At the latest version (0.0.1), `phenolocrop` package offers a model for rice canopy height data.

-   **Trait**: Rice canopy height.

    -   **Model**: Three-parameter logistic with the decrease in the late growth phase. Use `logisLateDicr` function.
