library('data.tree')
library('htmlwidgets')
library('webshot2')
library('rstudioapi')
library('gbm')

# Set working dir
setwd(dirname(getActiveDocumentContext()$path))

# Prepare Data ----
final_model <- readRDS(
  file.path('..', 'data', '01_raw', '20220929_finale_model.rds'))$model[[1]]$finalModel
vars <- final_model$var.names


get_tree_df <- function(i.tree) {
  df <- pretty.gbm.tree(final_model, i.tree = i.tree)
  # Add 1 to var indices because this is 0-based while r vectors are 1-based
  df$SplitVar <- 1 + df$SplitVar
  df$LeftNode <- 1 + df$LeftNode
  df$RightNode <- 1 + df$RightNode
  df$MissingNode <- 1 + df$MissingNode
  return (df)
}

construct_name <- function(df, row_number) {
  return (
    paste(
      paste(vars[df[row_number,]$SplitVar], row_number, sep='_'),
      df[row_number,]$SplitCodePred, sep='\n')
  )
}

# Set up directory to save charts
save_dir <- file.path('..','data','05_decision_trees')
html_dir <- file.path(save_dir, 'html')
png_dir <- file.path(save_dir, 'png')
tree_dir <- file.path(save_dir, 'trees')
dir.create(save_dir, showWarnings = FALSE)
dir.create(html_dir, showWarnings = FALSE)
dir.create(png_dir, showWarnings = FALSE)
dir.create(tree_dir, showWarnings = FALSE)

# Build Tree ----
# 
# Algorithm
# 
# 1 Create root node root
# 2 Initialize todo list with [1] where 1 is the row number of df
# 3 If there are items in todo , pop one as current_row and do the following:
#   Create current_node corresponding to current_row
#   Find and add left and right row numbers to todo
#   Add left and right nodes to current_node
#   Repeat 3 until todo is empty

# Loop through the trees
for (i.tree in seq(1:final_model$n.trees)) {
# for (i.tree in seq(1:1)) {
  df <- get_tree_df(i.tree)
  
  # 1 Create root node root
  tr <- Node$new(construct_name(df, 1))
  
  # 2 Initialize todo list with [1] where 1 is the row number of df
  todo <- list(list(i=1, n=construct_name(df, 1))) # Index and Name
  
  # 3 Set current_node to root
  while (length(todo) > 0) {
    # Pop 1st element
    current_row <- todo[[1]]
    todo <- todo[-1]
    
    # Continue next iteration if `current_row==0` because that's a leaf
    if (current_row[['i']] == 0) { next }
    
    
    # # Create current_node corresponding to current_row
    # # For node name, add the row number because same var can show up multiple times in the tree
    current_node_name <- construct_name(df, current_row[['i']])
    
    
    
    # Find and add left and right row numbers to todo
    ln_row <- df[current_row[['i']],]$LeftNode
    rn_row <- df[current_row[['i']],]$RightNode
    todo[[1+length(todo)]] <- list(i=ln_row, n=construct_name(df, ln_row))
    todo[[1+length(todo)]] <- list(i=rn_row, n=construct_name(df, rn_row))
    
    
    
    # Add left and right nodes to current_node
    ln_name <- construct_name(df, ln_row)
    rn_name <- construct_name(df, rn_row)
    
    # Find current node
    current_node <- FindNode(tr, current_node_name)
    current_node$AddChild(ln_name)
    current_node$AddChild(rn_name)
  }
  
  file_name <- paste0('/tree_', sprintf("%03d", i.tree))
  # Save current tree
  write.csv(df, file = paste0(tree_dir, file_name, '.csv'), row.names = T)
  
  
  # Chart the tree and save in html and png formats
  plt <- plot(tr)
  html_path <- paste0(html_dir, file_name, '.html')
  saveWidget(plt, html_path)
}
