# Televised Political Advertising
Replication data for [Politicizing Trade: How Economic Discontent andIdentity Politics Shape Anti-Trade Campaign Appeals](https://www.dropbox.com/s/jvh35k8l5hgrryf/Katitas_JMP.pdf?dl=0)

## Repository Layout 
This repository contains the folder ``code``, which includes all scripts needed to be run in order to generate all data.

## Installation 
Recreating all tables and figures in this repository requires working installations of [R]([Index of /src/base/R-4](https://cran.r-project.org/src/base/R-4/)  All this coded was tested R version 4.1.3 on a Mac Monterey. 

### R dependencies 
All R code uses the following packages: ``dplyr, tidyverse, lfe, stargazer, broom, marginaleffects, modelsummary, readstata13, scales, foreign, countrycode, quantmod, lmtest, sandwich, matchit, cobalt`` 

## Scripts
- [ ] Katitas_Ads_Merge_Data.R - Reads in data on televised political advertising from [Wisconsin Advertising Project](https://elections.wisc.edu/wisconsin-advertising-project/) and [Wesleyan Media Project](http://mediaproject.wesleyan.edu/), cleans and codes issues used in the paper. 
- [ ] Katitas_DMAanalysis.R - Merges ad data with other data used in the project, creates the final analysis file 
- [ ] Katitas_Figures.R - creates figures used in the paper 
- [ ] dma_analysis - STATA do file for the regression analyses at the media market level. 
- [ ] county_analysis - STATA do file for the regression analyses at the county level. 






