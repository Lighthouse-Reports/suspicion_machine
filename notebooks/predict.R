### Author: JB
### Date: 20221019

### Purpose: This script is adapted from Rotterdam's Scoring.R's script.
# Produces a file where variables are merged with predicted risk score.

### Modifications
# 20221025; Ted: Edited to a minimum script as example for partners.
###

library('dplyr')
library('caret')
# use excel because feature names contain characters that a text file has trouble
library('xlsx')

# load features table
abt <- read.xlsx(
  'data/03_experiment_input/sample_input.xlsx',
  sheetName = 'sample_input')
# remove na
abt <- na.omit(abt)
# load model
final_model <- readRDS('data/01_raw/20220929_finale_model.rds')

# make prediction
abt_prob <- predict(final_model$model[[1]], newdata = abt, type = 'prob')
# save predictions
write.csv(
  abt_prob['Ja'],
  'data/04_experiment_output/sample_input_scores_only.csv',
  row.names = F
)

# merge features and prediction tables and save
# this output is more heavy than previous one but contains details
abt_prob <- cbind(abt_prob, abt)
lijst_alle <- abt_prob %>%
  arrange(-Ja) %>%
  mutate(volgnummer = 1:nrow(.))
write.xlsx(
  lijst_alle,
  'data/04_experiment_output/sample_input_scored.xlsx',
  row.names = F
)
