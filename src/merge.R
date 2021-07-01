library(rsyncrosim)
library(dplyr)

# Read in run settings
settings <- datasheet(scenario(), "epiTransform_MergeInputs")

transformerName <- "Data Transformations: Merge Scenarios"

# Read in merged inputs
originalData <- datasheet(scenario(), "epi_DataSummary", optional = T, lookupsAsFactors = F)

# Choose if or how to modify the source transformer data
if(settings$Keep == "Both"){
  saveDatasheet(scenario(), bind_rows(originalData, originalData %>% mutate(TransformerID = transformerName)), "epi_DataSummary")
  
} else if(settings$Keep == "Source"){
  saveDatasheet(scenario(), originalData, "epi_DataSummary")
  
} else # settings$Keep == "Merged"
  saveDatasheet(scenario(), originalData %>% mutate(TransformerID = transformerName), "epi_DataSummary")

