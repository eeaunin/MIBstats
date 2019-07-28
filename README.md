# MIBstats
<br /><br />
MIBstats is a biostatistics web app made made using R and [Shiny](https://shiny.rstudio.com/). The aim of this app is to provide a common graphical interface for several statistical methods that are used in metabolomics. The web app contains the [iterative group analysis (iGA)](https://www.ncbi.nlm.nih.gov/pubmed/15050037) and [rank product](https://www.ncbi.nlm.nih.gov/pubmed/28481966)
 functionalities and also includes other statistical methods, _e.g._, the t-test, F-test, Shapiro-Wilk test and principal component analysis (PCA). The app also has a data pre-processing section with many options. The code of the app has an object-oriented and modular design, which makes it easier to modify and expand the app.

### Prerequisites
The app can be run using R and RStudio.<br />
Instructions for installing R: https://cran.r-project.org/doc/manuals/R-admin.html
<br />
Installing RStudio: https://www.rstudio.com/products/rstudio/download
<br />
(The RStudio Desktop Open Source License can be used to run this app on a desktop computer).

### Installation
The code for installing the R packages required by MIBstats is in the [installing_packages.R](MIBstats_source/installing_packages.R) file. 
The use of the Rmpfr package may require the installation of the C library MPFR (https://cran.r-project.org/web/packages/Rmpfr/README.html).
The app has been tested on Linux operating system (Ubuntu).

### Running the app
The app can be run locally in RStudio by opening a "server.R" file in RStudio and clicking the RStudio ["Run app" icon](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/images/run-app.png). For the "Run app" icon to appear in RStudio, the Shiny package needs to be installed.

### Usage
The overview of the data processing pipeline in the app is shown in the diagram below.
<br />
![MIBstats data processing pipeline figure](diagrams/Figure_1_diagram_of_data_processing_pipeline.png?raw=true "MIBstats data processing pipeline figure")
<br />
<br />
For each individual step of the pipeline, the app has built-in help texts that are displayed when clicking on the [information icons](http://fontawesome.io/icon/info-circle/) in the app. 

#### Example screenshots
[PCA scores and loadings plots](screenshots/PCA_scores_and_loadings_plot.tif)<br />
[PCA k-means clustering and scree plot](screenshots/PCA_kmeans_and_scree_plot.tif)<br />
[Scaling, normalisation and variance stabilisation: mean centering](screenshots/preprocessing_mean_centering.tif)<br />
["Choose data for comparing two groups" page](screenshots/two_groups_test_queue.tif)<br />
[t-test results](screenshots/t_test_one_variable_statistics.tif)<br />
[Heat map of fold changes of paired samples](screenshots/fold_changes_heat_map.tif)<br />
[Rank product results](screenshots/rank_product_results.tif)<br />
[iGA results](screenshots/iGA_results.tif)<br />
[Volcano plot](screenshots/volcano_plot.tif)<br />

### Authors
The app was made by Eerik Aunin as a part of the bioinformatics MSc course in the University of Manchester. The project was supervised by Prof. Rainer Breitling and Francesco Del Carratore. The iGA calculation part of the app incorporates code written by Francesco Del Carratore. 
Various R packages were used (as listed in [list_of_R_packages.txt](list_of_R_packages.txt)). Code snippets that were adapted from various online sources have been referenced in the code.
