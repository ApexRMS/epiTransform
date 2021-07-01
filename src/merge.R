library(rsyncrosim)
library(dplyr)
library(stringr)

# Read in run settings
settings <- datasheet(scenario(), "epiTransform_MergeInputs")

transformerName <- "Data Transformations: Merge Scenarios"

# Read in merged inputs
originalData <- datasheet(scenario(), "epi_DataSummary", optional = T, lookupsAsFactors = F)

# Choose if or how to modify the source transformer data
if(str_detect(settings$Keep, "Keep both")){
  saveDatasheet(scenario(), bind_rows(originalData, originalData %>% mutate(TransformerID = transformerName)), "epi_DataSummary")
  
} else if(str_detect(settings$Keep, "Only keep original")){
  saveDatasheet(scenario(), originalData, "epi_DataSummary")
  
} else # Only keep consolidated
  saveDatasheet(scenario(), originalData %>% mutate(TransformerID = transformerName), "epi_DataSummary")

