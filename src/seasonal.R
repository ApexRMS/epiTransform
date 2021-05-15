# seasonal.R
# Removes seasonal effects from daily and cumulative data using STL decompositions

library(rsyncrosim)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# Setup ----------
myScenario <- scenario()
transformerName <- "Data Transformations: Remove Seasonal Effects"

# Load data from SyncroSim
settings <- datasheet(myScenario, name = "epiTransform_STLInputs", lookupsAsFactors = F, optional = T)
data_in <- datasheet(myScenario, name = "epi_DataSummary", lookupsAsFactors = F, optional = T) %>%
  mutate(TransformerID = replace_na(TransformerID, "Placeholder Transformer"))

if(nrow(data_in) == 0)
  stop("No input data found, please check scenario dependencies!")

# Parse settings
s.window <- settings$SWindow[1]
t.window <- settings$TWindow[1]
transformVariable <- str_replace(settings$Variable[1], "Cumulative", "Daily")
logOffset <- settings$LogOffset[1]
dataSource <- ifelse(is.na(settings$Source), "Placeholder Transformer", settings$Source)

# If no variable to transform is provided, use the first variable in the data
if(is.na(transformVariable))
  transformVariable <- data_in %>%
    filter(TransformerID %in% dataSource) %>%
    pull(Variable) %>%
    head(1) %>%
    str_replace("Cumulative", "Daily")

# internal, names for new variables
new_variables <- c("Trend (STL)", "Adjusted (STL)")

# Function Definitions ------
add_stl_trend_m <- function(c,s.window=21,t.window=14){
  cc <- c %>%
    log() %>%
    ts(frequency = 7,start = as.numeric(format(Sys.Date(), "%j"))) %>% 
    stl(s.window=s.window,t.window=t.window) 
  
  as_tibble(cc$time.series) %>%
    mutate_all(exp)
}

# Transform Data -----------
data_out <- data_in %>%
  filter(
    Variable %in% transformVariable,
    TransformerID %in% dataSource) %>%
  group_by(Variable,Jurisdiction) %>%
  mutate(stl=add_stl_trend_m(.data$Value+logOffset,s.window=s.window,t.window=t.window)) %>%
  ungroup() %>%
  mutate(!!new_variables[1] := pmax(0,stl$trend-logOffset),
         Random=stl$remainder,
         Seasonal=stl$seasonal) %>% # Value = Trend * Seasonal * Random
  mutate(!!new_variables[2] := Value/Seasonal) %>%
  select(c(names(data_in),new_variables)) %>% # throw out unused variables
  select(-TransformerID,-Value) %>%
  pivot_longer(new_variables,names_to = "TransformerID",values_to = "Value") %>%
  bind_rows((.) %>%
            group_by(TransformerID,Jurisdiction) %>%
            arrange(Timestep) %>%
            mutate(Variable="Cases - Cumulative",
                   Value=cumsum(Value))) %>%
  arrange(Timestep,Variable) %>%
  # Temporarily just keep the trend
  filter(TransformerID == new_variables[1]) %>%
  mutate(
    TransformerID = transformerName) %>%
  as.data.frame

# Save output ------
saveDatasheet(myScenario, data_out, name = "epi_DataSummary", append = T)
