
# Rscript to load all the required packages for the ShinyApp
#====================================================================================================================

#Load the packages
library(spotifyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(highcharter)
library(viridis)
library(plotly)
library(dqshiny)

#====================================================================================================================

#Note that this script can be improved by:
# 1) writing a function that will automatically install packages which are not installed yet
# 2) store the packages in a seperate folder in the project and load them using .libPaths