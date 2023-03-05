### Ted & Justin
### 20221119
###
### Purpose: Predict and analyze data generate for a specific experiment. Conducts basic and conditional statistical parity tests.
###
### Usage: Load all the functions, the for-loop at the bottom of the script then calls all relevant functions to analyze data for 
### all config files in '../conf/YOUR_DIR' (You will need to specify YOUR_DIR below. 
### Make sure that you have run the data generator notebook previously.
### Saves files in 'results/statistical_parity/YOUR_DIR' locally. 
### Look for 'CHECK' comments to specify filepaths.

# Load packages
if (!require("caret")) install.packages("caret")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("glue")) install.packages("glue")
if (!require("ini")) install.packages("ini")  # init files
if (!require("RJSONIO")) install.packages("RJSONIO") # json string to r object #yes
if (!require("OneR")) install.packages("OneR") #bin
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("rstatix")) install.packages("rstatix")
if (!require("stringr")) install.packages("stringr")
if (!require("rstudioapi")) install.packages("rstudioapi")


# Predict 
#
# @param final_model: gbm model taken from Rotterdam's risk prediction system
# @param input_data: Each row is one instance to be predicted. Column names, except 'data_type', must comply with the model's feature names.
# @param output_filename: filename, input data + predictions are to be saved
#
# @return risk_scores: A dataframe containing all data from `input_file` plus `(Ja, Nee)`. `Ja` is the risk score. `Ja + Nee = 1`
final_model.predict <- function(final_model, input_data, output_filename) {
  # calculate risk scores
  risk_scores <- predict.train(final_model, newdata = select(input_data, -data_type), type = 'prob') #remove data_type from the prediction, so the model actually runs
  
  # concatenate input data and risk scores for comprehensive output
  risk_scores <- cbind(input_data, risk_scores)
  
  # write the risk scores as is
  write.csv(risk_scores, output_filename)
  print(glue('Risk scores have been written to {output_filename}'))
  
  return(risk_scores)
}

# Prepare for Analysis
# 
# @param variable_list: list of lists of lists with each var of interest and corresponding values specified
# @param df: scored dataframe 
# @param bins: list of column names to be binned
#
# @return df: returns dataframe where all variables of interest and their corresponding values have been collated in the single column 'IV'
# variables without any variation are not included in 'IV' to improve readability of the output graphs
#
# Purpose: This fn takes the dataset incl. risk scores and transforms all F_i into a single variable, which can then be used by the analysis scripts
final_model.prepare_for_analysis <- function(variable_list, df, bins, nbins = 8){
  new_var_list <- list()
  
  for (i in 1:length(variable_list)) {
    new_var_name <- NA #initialize new_var_name, to be used if variables need to be transformed
    nested_list <- variable_list[[i]]
    
    #One Hot Encoded case: combine all the dummies into a single new variable variable (reverse pivot)
    if (length(nested_list) > 1) { 
      var_name <- paste0('var_', toString(i)) #set up new variable
      df[var_name] <- NA
      for (j in 1:length(nested_list)){
        cur <- names(nested_list[[j]])
        if(nrow(df[df[cur] == 1,]) > 0){
          df[df[cur] == 1,][var_name] <- cur #set the new variable equal to the column name of the dummy that is equal to 1
        }
        
      }
      new_var_name <- var_name
    }
    
    cur <- names(nested_list[[1]]) #extract variable name
    
    #binning case: if there are variables to be binned, construct new variable with the binned values
    if (cur %in% bins){ 
      var_name <- paste0('var_', toString(i))
      df[, var_name] <- bin(df[[cur]], nbins = nbins, method = 'content') #bin by content, so outliers don't leave us with  of empty bins
      new_var_name <- var_name
    }
    
    #TODO: implement NA
    
    #if variable has not been changed (not OHE or binning), set new_var_name equal to cur
    if(is.na(new_var_name)){
      new_var_name <- cur
    }
    
    #add variable to variable_list
    new_var_list <- append(new_var_list, new_var_name)
  }
  
  #combine all variables of interest into 'IV' column
  df$IV <- 'IV: \n' #initialize IV
  for (var in new_var_list) {
    
    #if there is only one unique value for a variable, don't include it in 'IV'
    if (length(unique(df[[var]])) == 1) {
      next
    }
    
    #combine column name and variable value 
    df$IV <- paste(df$IV, var,': ' ,df[[var]],'\n', sep = '')
  }
  return (df)
}

# Visual Analysis
#
# @param sum_stats_list: list of data frames with summary statistics 
# @param decile_count_list: list of data frames containing number of observations per risk score decile
# @param plot_list: list of plots to put more plots into
#
# @return plotlist: return plot_list to which the newly generated plots have been added
final_model.visual_analysis <- function(sum_stats_list, decile_count_list, plot_list) {
  
  #iterate over all data frames in decile count list
  for(decile_df in decile_count_list){
    cur_dt <- decile_df[1,]$data_type
    
    #generate line graph
    p1 <- ggplot(decile_df, aes(x=decile, y=perc, color = as.factor(IV)))+
      geom_point(size = 2)+
      geom_line(size = 3)+
      ylim(0,NA)+
      scale_x_continuous(breaks = decile_df$decile)+
      xlab('Risk score decile')+
      ylab('Group percentage in risk score decile')+
      ggtitle(cur_dt)
    plot_list <- append(plot_list, list(p1))
  }
  
  #iterate over sum_stats list
  for(sum_stats in sum_stats_list){
    cur_dt <- sum_stats[1,]$data_type
    
    #generate over-representation shift plot
    p1 <- ggplot(sum_stats, aes(x=as.factor(IV), y = perc_misrep_td_threshold))+
      geom_col(fill = 'blue', width = 0.5) +
      geom_hline(yintercept = 0)+
      theme(axis.title.x = element_text(margin=margin(t = 3)))+
      coord_flip()+
      theme_bw() +
      labs(x = 'Excess share in highest risk decile', title = paste0(cur_dt, ': Excess share in highest risk decile'))
    
    plot_list <- append(plot_list, list(p1))
  }
  
  return (plot_list)
}


# Save Plots
#
# @param plot_list: list of plots to be saved
# @param config: configuration object used to extract output filename
final_model.save_plots <- function(plot_list, config){
  save_dir <- file.path('..', 'results', 'statistical_parity', 'archetypes', config$META$name) # CHECK: specify filepath for results
  dir.create(save_dir, showWarnings = FALSE) # creates the results dir
  output_filename <- file.path(save_dir, 'plot.pdf') #create output filename
  
  #save all plots in single pdf
  pdf(output_filename, width = 10, height =7) 
  print(plot_list)
  dev.off() 
}


# Numeric Analysis
# 
# @param df: scored dataframe to be analyzed
# @param dt: string with data type specified
# @param sum_stats_list: empty list of summary stats tables
# @param ttest_list: empty list of ttest tables
#
# @return: return both the sum_stats and the ttest_list in a list of lists, where the new tables have been added

final_model.numeric_analysis <- function(df, dt, sum_stats_list, ttest_list, decile_count_list){
  #synthetic data has a higher average risk score than real training data, so I use a separate cutoff point
  td_threshold <- ifelse(dt %in% c('real', 'real_conditional'), td_real_threshold, td_synth_threshold)
  
  #caculate share of risk scores above threshold
  df_threshold <- df %>%
    dplyr::mutate(above_td_threshold = ifelse(df$Ja > td_threshold, 1, 0),
           above_rw_threshold = ifelse(df$Ja > rw_threshold, 1, 0)) %>%
    dplyr::group_by(IV) %>%
    dplyr::summarise(share_above_td_threshold = mean(above_td_threshold),
              share_above_rw_threshold = mean(above_rw_threshold)) %>%
    dplyr::mutate(perc_misrep_td_threshold = (share_above_td_threshold - 0.1) * 100,
                  perc_misrep_rw_threshold = (share_above_rw_threshold - 0.1) * 100)
  
  #get mean and sd for each level of IV
  sum_stats <- df %>%
    dplyr::group_by(IV) %>%
    get_summary_stats(Ja, type = 'mean_sd')
  
  #merge over-representation with other summary stats  
  sum_stats <- dplyr::left_join(sum_stats, df_threshold, by = 'IV')
  
  sum_stats$data_type <- dt
  sum_stats_list <- append(sum_stats_list, list(sum_stats))
  
  
  #run pairwise ttests between each level of IV; tells us if there is a significant difference in the distribution between F_i = E_{i,j} and F_i = E_{i,k}
  if(length(unique(df$IV)) > 1){
    pwt <- df %>%
      pairwise_t_test(Ja~IV)
    pwt$data_type <- dt
    ttest_list <- append(ttest_list, list(pwt))
  }
  
  #calculate the risk score deciles
  df$decile <- ntile(df$Ja, 10)
  
  #count number of obersvations per decile
  df_decile_counts <- df %>%
    dplyr::group_by(decile, IV) %>%
    dplyr::count() %>%
    dplyr::group_by(IV) %>%
    dplyr::mutate(perc = (100 * n/sum(n))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(data_type = dt)
  
  #add decile count df to decile count list
  decile_count_list <- append(decile_count_list, list(df_decile_counts))
  
  return(list(sum_stats_list, ttest_list, decile_count_list))
  
}


# Save Tables
#
# @param sum_stats_list: list of summary stats tables
# @param ttest_list: list of ttest tables
# @param config: configuration object used to extract output filename
final_model.save_tables <- function(sum_stats_list, ttest_list, decile_count_list, config){
  save_dir <- file.path('..', 'results', 'statistical_parity', 'archetypes', config$META$name) #CHECK: specify destination directory
  output_sum_stats <- file.path(save_dir, 'sum_stats.csv') #summary stats file path
  output_ttest <- file.path(save_dir, 't_test.csv') #ttest file path
  output_decile <- file.path(save_dir, 'decile.csv') #Decile file path
  
  all_sum_stats <- as.data.frame(dplyr::bind_rows(sum_stats_list)) #combine all summary stats tables into single df
  all_ttest <- as.data.frame(dplyr::bind_rows(ttest_list))  #combine all ttest tables into single df
  all_decile <- as.data.frame(dplyr::bind_rows(decile_count_list))  #combine all decile tables into single df
  
  write.csv(all_sum_stats, output_sum_stats) #save summary stats
  write.csv(all_ttest, output_ttest) #save ttest tables
  write.csv(all_decile, output_decile) #save decile tables
  print('All tables have been saved.')
}


# Analyze
#
# @param ini: filepath to an ini config file
# @param final_model: gbm model taken from Rotterdam's risk prediction system
#
# Purpose: wrapper function to call all the load, predict, pre-processing, analysis, and save functions
final_model.analyze <- function(ini, final_model){
  # read ini file
  config_filename <- file.path('../conf/archetypes/', ini) #CHECK: specify filepath
  config <- read.ini(config_filename, encoding = 'utf8')
  input_filename <- file.path(
    '..','data','03_experiment_input',
    paste0(
      paste(config$META$date, config$META$user, config$META$name, sep = '_'),
      '.csv'
    )
  )
  output_filename <- file.path(
    '..','data','04_experiment_output',
    paste0(
      paste(config$META$date, config$META$user, config$META$name, sep = '_'),
      '.csv'
    )
  )
  var_list <- config$VARIABLES$variable_list
  var_list <- str_replace_all(var_list, '“', '\"')
  var_list <- str_replace_all(var_list, '”', '\"')
  variable_list <- fromJSON(var_list)
  print(variable_list)
  bin_list <- fromJSON(config$VARIABLES$bin)

  # Read data
  df <- read.csv(input_filename)
  print(glue('Shape of dataframe after loading {dim(df)}'))

  # Predict
  risk_scores <- final_model.predict(final_model, df, output_filename)
  print(glue('Shape of dataframe after prediction {dim(risk_scores)}'))

  # Bin and revert One Hot Encoding
  risk_scores <- final_model.prepare_for_analysis(variable_list, risk_scores, bin_list)
  
  #conduct seperate analysis on each data type. This allows us to differentiate in our analysis between simple and conditional statistical parity tests
  all_data_types <- unique(risk_scores$data_type)
  print(glue("All data types {all_data_types}"))
  
  #initialize analysis results lists
  plot_list <- list()
  sum_stats_list <- list()
  ttest_list <- list()
  decile_count_list <- list()
  
  #itereate over data types and conduct visual and numeric analysis
  for (dt in all_data_types){
    table_lists <- final_model.numeric_analysis(risk_scores[risk_scores$data_type == dt,], dt, sum_stats_list, ttest_list, decile_count_list)
    sum_stats_list <- table_lists[[1]]
    ttest_list <- table_lists[[2]]
    decile_count_list <- table_lists[[3]]
  }
  
  #take the visual analysis out of the loop and perform the visual analysis on the tables that were created inside the loop
  plot_list <- final_model.visual_analysis(sum_stats_list, decile_count_list, plot_list)

  #save numeric and visual analysis
  final_model.save_plots(plot_list, config)
  final_model.save_tables(sum_stats_list, ttest_list, decile_count_list, config)
}

# Set working dir
setwd(dirname(getActiveDocumentContext()$path))

# Load model
final_model <- readRDS(file.path('..', 'data', '01_raw', '20220929_finale_model.rds'))$model[[1]]
td_real_threshold <- 0.6970136 #@Htet threshold derived from training data risk score distribution
td_synth_threshold <- 0.7125668 #@Htet threshold derived from synthetic training data risk score distribution
rw_threshold <- 0.5912492 #@Htet threshold derived from normal approximation of real world thresholds, on the basis of mean and sd given to us by Rotterdam

# Run this loop
for (ini in list.files(path = '../conf/archetypes', pattern='*.ini')) { # CHECK: specify filepath
  print(glue('Analyzing {ini}'))
  final_model.analyze(ini, final_model)
  gc() #clean memory
}


