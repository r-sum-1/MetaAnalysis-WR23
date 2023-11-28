# R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"


#remotes::install_github("MathiasHarrer/dmetar")


######    Load packages
# Install missing packages or load the package if already installed    
packages <- c("robumeta", "metafor", 
              "esc", "meta", 
              "ggplot2", "gridExtra", 
              "readxl", "rstudioapi", "clipr", 
              "weightr", "dplyr", "dmetar", "fastDummies")

# Install packages not yet installed

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



######    Import data ##

setwd(dirname(getSourceEditorContext()$path))


df_full <- read_excel("Datasets/Raw_Datasets/data amended.xlsx", sheet="data")
