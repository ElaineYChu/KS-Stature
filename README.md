# KidStats: Stature 
KidStats: Stature is a graphical user interface (GUI) to provide methods of subadult skeletal stature estimation using long bone measurements and linear and nonlinear regression. 

This application is one product of the research of Elaine Y. Chu, PhD, a Postdoctoral Research Fellow at the Center for Functional Anatomy and Evolution in the Johns Hopkins School of Medicine. Associated citations / articles are forthcoming.


Please cite this applicaiton as: 
> Chu, Elaine Y., & Stull, Kyra E. (2023). KidStats: Stature - A graphical user interface for subadult stature estimaiton. Version 1.00

The stable URL for this GUI can be found at: https://elaineychu.shinyapps.io/ks-stature

This GUI is part of a larger suite of applications for estimating the subadult biological profile, all of which can be found at: https://kyrastull.weebly.com/kidstats.html

# Installation
To access KidStats: Stature offline, copy (or "clone") this repository using one of the following two methods:  
1. Access the repository through a terminal system by typing the following code:
  
  ``` console
cd "file/path/to/desired/repository/location"
git clone https://github.com/ElaineYChu/ks-stature
```

2. Download the repository as a zipped file by clicking the green "CODE" button on the top-right side and select 'Download ZIP'

## Usage

**TO RUN KidStats: Stature IN RSTUDIO:**  
1. Locate the "ks-stature" folder in your system and the file called "ui.R".
2. Open the file with RStudio.
3. Locate and press the "Run App" button at the top-right corner of the "ui.R" script.

**TO RUN KidStats: Stature IN BASE R**  
1. Set the working directory  
``` r
setwd("file/path/to/ks-stature")  # set working directory
```
2. Run the application
``` r
shiny::runApp()  # run KidStats: Stature
```

KidStats: Stature requires R version **4.2.1** or higher and will require the following packages in your local R system:
* shiny
* shinydashboard
* shinyjs
* shinyBS
* shinyMatrix
* tidyverse
* rmarkdown
* stringr
* DT

Please make sure that all of these packages are installed in your local system before starting. 
