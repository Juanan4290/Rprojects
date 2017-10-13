Overview
--------

This Concrete Compressive Strength application was built for the Coursera Course Developing Data Products as part of the Data Science Specilization.

The shiny application developed for this assignment is available at: <https://juanan4290.shinyapps.io/concretedataanalysis/>

The source codes of ui.R and server.R available on GitHub repo at: <https://github.com/Juanan4290/Rprojects/tree/master/DataProducts/ConcreteDataAnalysisApp/R>

Application Data
----------------

The application uses data from Concrete Compressive Strength dataset. These data were collected by I-Cheng Yeh to determine how the compressive strength of concrete is related to its ingredients (cement, blast furnace slag, fly ash, water, superplasticizer, coarse aggregate, and fine aggregate) and age.

``` r
library (fastR); data(concreteAll)
head(concreteAll)
```

    ##   cement  slag ash water superP coarseAg fineAg age strength
    ## 1  540.0   0.0   0   162    2.5   1040.0  676.0  28    79.99
    ## 2  540.0   0.0   0   162    2.5   1055.0  676.0  28    61.89
    ## 3  332.5 142.5   0   228    0.0    932.0  594.0 270    40.27
    ## 4  332.5 142.5   0   228    0.0    932.0  594.0 365    41.05
    ## 5  198.6 132.4   0   192    0.0    978.4  825.5 360    44.30
    ## 6  266.0 114.0   0   228    0.0    932.0  670.0  90    47.03

Application Funcionality
------------------------

-   The application represents an scatterplot with the two variables (X and Y variables) you select in the left side panel.

-   You can also group the data by the variable selected in the left side eyelash named "Grouping by".

-   The user can fit a linear model based on variables selected in the left side panel.
