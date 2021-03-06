[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

# About this story

**[Heart conditions drove spike in deaths beyond those attributed to covid-19, analysis shows](https://www.washingtonpost.com/graphics/2020/investigations/coronavirus-excess-deaths-heart/)**

# Files of interest within this folder

* `00_format_table.R` - Functions to extract data from modeled object
* `01_all_causes.R` - Pulls and formats data from the CDC on specific causes (2014-2020)
* `02_causes_modeling.R` - Creates a modeled baseline for six identified jurisdictions of note
* `04_specific_causes_six_states.Rmd` - Creates a report that shows excess deaths for specific causes in the jurisdictions we focused on

# Notebooks

* [Specific causes six states
](http://wpinvestigative.github.io/excess_deaths_covid19/specific_causes/04_specific_causes_six_states.html) - Exploratory visualization of data generated by models
* [Causes of death by U.S. and States
](http://wpinvestigative.github.io/excess_deaths_covid19/specific_causes/03_state_causes_of_death.html) - Comparing historical with current specific cause data trends (only through early May)

# Scripts

* `00_package_prep.R` - follow instructions to make sure correct packages are installed
* `01_xxx` - `04_xxx` - Run scripts in order to generate data used in analysis

# Methodology

The Washington Post gathered historical data on select causes of death by state between [2014](https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr) and early [2020](https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6), published by the National Center for Health Statistics (NCHS), and used a model previously developed by a research team led by the Yale School of Public Health to estimate the number of deaths that would normally be expected each week from March 1 to May 30. The estimate takes into account seasonal variations as well as year-to-year variations in mortality levels but did not adjust for the flu. The mortality data are collected from state health departments, which report deaths at different rates. It usually takes about three weeks for death data to stabilize, but even then, they are still not complete. As a result, it is expected that the numbers of deaths in the period The Post analyzed will increase as states continue reporting additional data to the NCHS. Details on the team’s statistical approach can be found on [GitHub](https://github.com/weinbergerlab/excess_pi_covid/blob/master/post%20methods.pdf).