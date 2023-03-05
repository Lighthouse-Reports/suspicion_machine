# How to Run a Statistical Parity Experiment

To understand the methodology, purpose and typical outputs of an experiment, see the [full article](???LinkToMethodology).

A typical experiment contains 4 stages. Detailed instructions are provided in the subsequent sections.
1. Deciding the experiment focus. An experiment must have one or more focus features.
2. Setting up config files. Each experiment is precisely defined by a config file in `ini` format.
3. Running the experiment. There are a few notebooks to run for data preparation, risk prediction and analysis.
4. Finding the Results and Interpreting them. All results are stored within the project folder.

## 1. Decide Features for the Experiment

Choose one feature or multiple related features for the experiment. See [full list and details of features](Link ToFeaturesHandbook???) utilised by the model.

## 2. Creating `ini` Config File

Each experiment must have a corresponding config file that defines the author, experiment name, features and their specific values, business rules, and bins.
1. Decide single or multiple feature experiment
2. Copy `conf/test_template.ini` to `conf/single_feature` or `conf/multi_feature` depending on the experiment type
3. Pay especially close attention to matching the pattern of `variable_list`: Each variable lives within a nested list. A variable itself is a dictionary with the variable name as the key and the values you want to compare as the value in list-form. Both the variable name and the individual values need to be in quotation marks. If you want to compare across all unique values, you can specify `"ALL"` in the values list.
4. If you are interested in conditional statistical parity and you are looking at a variable with 100s of possible values. It is almost always better to specify a couple of values rather than using the binning list.

## 3. Running the Experiment

1. Run data generator notebook `notebooks/20221119_jb_data-generator.ipynb`. This is Jupyter Notebook written in Python. The notebook creates the input data for the experiment in `data/03_experiment_input/` directory. Confirm that there is a new csv file in `data/03_experiment_input/`. Filename is based on the unique experiment name defined in the config file.
2. Run the script `notebooks/predict_analyze_jb.R`. This is an R script that uses the input data from the previous step, calculates risk scores using the Rotterdam's model, and saves them in `data/04_experiment_output/` directory. Confirm that there is a new csv file in `data/04_experiment_output/`. Again, the filename is based on the unique experiment name defined in the config file.
3. The script from previous step also outputs the analysis results of the experiment in a new folder, named based on the unique experiment name given in config file, inside `results/` directory.

## 4. Results

1. One folder for each experiment is created, containing:
   1. pairwise t-tests for statistical significance,
   2. summary stats for each level of the variables specified in `variable_list` in the config file, and
   3. Decile line graphs and shift bar graphs for intuitive interpretation
2. Note that all the variables in `variable_list` are combined into a single variable in string format. For instance, if you are interested in the impact of `var1` and `var2`, we might end up with four levels:
   - `var1 = 0, var2 = 0`
   - `var1 = 0, var2 = 1`
   - `var1 = 1, var2 = 0`
   - `var1 = 1, var2 = 1`
3. These variable combinations correspond to all possible value combinations of the variables in `variable_list`.

# Other Scripts
## [Htet] explain how tree scrip is used
