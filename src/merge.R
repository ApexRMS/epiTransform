library(rsyncrosim)

# Read in merged inputs and write back out
dataSummary <- datasheet(scenario(), "epi_DataSummary", optional = T, lookupsAsFactors = F)
saveDatasheet(scenario(), dataSummary, "epi_DataSummary")
