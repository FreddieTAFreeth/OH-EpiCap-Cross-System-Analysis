# OH-EpiCap Cross-System Analysis
Evaluating epidemiological systems under the OneHealth criterion, following the work of the OH-EpiCap Tool. This repository contains the code and data that generates the figures seen in the paper *Implementation of One Health surveillance systems, opportunities and challenges: Lessons learned from the OH-EpiCap application* by Henok Ayalew Tegegne<sup>1</sup>, **Frederick T. A. Freeth**<sup>2</sup> , Carlijn Bogaardt<sup>2</sup> , Emma Taylor<sup>2</sup>, Johana Reinhardt<sup>3</sup> , Lucie Collineau<sup>1</sup> , Joaquin M Prada<sup>2</sup>, and Viviane Hénaux<sup>1</sup>.

<sup>1</sup> University of Lyon - Agence Nationale de Sécurité Sanitaire de L&#39;Alimentation, de L&#39;Environnement et du Travail (ANSES), Laboratory of Lyon, Epidemiology and support to Surveillance Unit, 69007 Lyon, France

<sup>2</sup> University of Surrey, School of Veterinary Medicine, GU2 7XH Guildford, Surrey, United Kingdom

<sup>1</sup> Agence Nationale de Sécurité Sanitaire de L&#39;Alimentation, de L&#39;Environnement et du Travail (ANSES), Risk assessment department, Animal health, welfare, feed and vectors risk assessment Unit, 94700 Maisons-Alfort, France

The OH-EpiCap Tool ([GitHub](https://github.com/FreddieTAFreeth/OH-EpiCap), [web link to the tool](https://freddietafreeth.shinyapps.io/OH-EpiCap/)) was developed and published in *Frontiers*: Henok Ayalew Tegegne, Carlijn Bogaardt, Lucie Collineau, Géraldine Cazeau, Renaud Lailler, Johana Reinhardt, **Frederick T. A. Freeth**, Emma Taylor, Joaquin M. Prada, and Viviane Hénaux. (2023). OH-EpiCap: A semi-quantitative tool for the evaluation of one health epidemiological surveillance capacities and capabilities. Frontiers in Public Health, 11. [https://doi.org/10.3389/fpubh.2023.1053986](https://doi.org/10.3389/fpubh.2023.1053986).

## How-to Guide
Creating your own radar plots with your own datasets is fast and easy. A dataset with ```N``` epidemiological systems to be modelled under the OneHealth EpiCap system with this script should be a ```.csv``` file with the following columns: ```Dimension_Number```,	```Dimension_Label```,	```System 1```,	```System 2```, ..., ```System N```. The columns ```Dimension_Number``` and	```Dimension_Label``` should have the numbers and labels as seen in the OH-EpiCap tool, and the system scores should be scored with one of the following values: ```1```, ```2```, ```3```, ```4```, or ```NA```. See our data file ```System_Data.csv``` as an example.

## Figures
- ```EpiCap_Scores.svg``` is a barplot of the overall OH-EpiCap score along with the scores at the dimension level. 
- ```Indicators_All_Radar.svg``` is a plot of all the indicators across all dimensions.
- ```Indicators_Dimension_1_Radar.svg``` is a plot of the four targets in dimension 1.
- ```Indicators_Dimension_2_Radar.svg``` is a plot of the four targets in dimension 2.
- ```Indicators_Dimension_3_Radar.svg``` is a plot of the four targets in dimension 3.
- ```Indicators_Target_Radar.svg``` is a plot of all all dimensions at the target level.

## Session Info
The R version and packages used are given below:

```
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.4.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] rstudioapi_0.14 ggplot2_3.4.0   plotrix_3.8-2  

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.10         lattice_0.20-45     deldir_1.0-6        corpcor_1.6.10      gtools_3.9.4       
 [6] png_0.1-8           snow_0.4-4          digest_0.6.31       psych_2.2.9         foreach_1.5.2      
[11] utf8_1.2.2          R6_2.5.1            plyr_1.8.8          backports_1.4.1     stats4_4.2.2       
[16] pillar_1.8.1        rlang_1.0.6         data.table_1.14.6   rpart_4.1.19        Matrix_1.5-1       
[21] qgraph_1.9.3        pbivnorm_0.6.0      checkmate_2.1.0     glasso_1.11         splines_4.2.2      
[26] stringr_1.5.0       foreign_0.8-83      htmlwidgets_1.6.1   igraph_1.3.5        munsell_0.5.0      
[31] compiler_4.2.2      xfun_0.36           pkgconfig_2.0.3     base64enc_0.1-3     mnormt_2.1.1       
[36] htmltools_0.5.4     doSNOW_1.0.20       insight_0.19.0      nnet_7.3-18         tidyselect_1.2.0   
[41] tibble_3.1.8        gridExtra_2.3       htmlTable_2.4.1     Hmisc_4.7-2         codetools_0.2-18   
[46] fansi_1.0.4         viridisLite_0.4.1   withr_2.5.0         dplyr_1.1.0         grid_4.2.2         
[51] nlme_3.1-160        gtable_0.3.1        lifecycle_1.0.3     bayestestR_0.13.0   magrittr_2.0.3     
[56] scales_1.2.1        datawizard_0.6.5    pbapply_1.7-0       cli_3.6.0           stringi_1.7.12     
[61] reshape2_1.4.4      fdrtool_1.2.17      latticeExtra_0.6-30 generics_0.1.3      vctrs_0.5.2        
[66] cowplot_1.1.1       Formula_1.2-4       RColorBrewer_1.1-3  iterators_1.0.14    tools_4.2.2        
[71] interp_1.1-3        glue_1.6.2          jpeg_0.1-10         abind_1.4-5         parallel_4.2.2     
[76] fastmap_1.1.0       survival_3.4-0      colorspace_2.1-0    cluster_2.1.4       knitr_1.42         
[81] lavaan_0.6-13  
```
