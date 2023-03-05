# Suspicion Machine

This project is part of the [Surveillance Newsroom](https://www.lighthousereports.nl/newsroom/surveillance/) at [Lighthouse Reports](https://www.lighthousereports.nl/). Other projects from Lighthouse Reports can be found at the [main GitHub repo](https://github.com/Lighthouse-Reports/).

## Project Overview
This project contains the source code for an investigation into the use of a welfare fraud prediction algorithm by the city of Rotterdam. The model used by Rotterdam is a [Gradient Boosting Machine (GBM)](https://deepai.org/machine-learning-glossary-and-terms/gradient-boosting) trained on a dataset of 12,707 past investigations into welfare fraud. This project contains source code for and results of the experiments conducted to explore and identify potential bias in the GBM model.

### Reporting Partners
- WIRED
- Vers Beton 
- Follow the Money
- Argos

### Methods Used
- Statistical Parity
- Inferential Statistics
- Data Wrangling
- Data Visualisation

### Technologies
- R
- Jupyter Notebook

## Project Description
This project explores the impact of beneficiaries' attributes on their risk scores. The GBM model evaluates the welfare beneficiaries based on 315 features and assigns each applicant a risk score from [0, 1], where higher risk score indicate greater risk of having committed some kind of 'illegality'. Illegality encompasses everything from simple mistakes on a form to serious fraud.
This project uses data obtained through press questions and public records requests from Rotterdam Municipality and employs the following approaches:
- Testing whether the fraud prediction system violates Statistical Parity and Conditional Statistical Parity
- Synthetic data generation for replication, since the original training data cannot be shared publicly due to GDPR concerns
- Extraction of the model's decision trees

## Repo Overview 

- Follow setup [instructions](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/HowToRun.md) to reproduce results and create your own experiments
- Raw Synthetic Data is kept [here](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/data/01_raw/synth_data.csv)
- Synthetic Data Generation model is kept [here](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/data/01_raw/GaussianCopula-generator.pkl)
- Experiment results are kept [here](https://github.com/Lighthouse-Reports/suspicion_machine/tree/main/results/statistical_parity)
- The trained model file is kept [here](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/data/01_raw/20220929_finale_model.rds)
- The source code used to train the model is [here](https://github.com/Lighthouse-Reports/suspicion_machine/tree/main/Source%20Code)

##  Notebooks
- [20221020_jb_synth-model.ipynb](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/notebooks/synth_model.ipynb): Synthetic data generation script
- [data-generator.ipynb](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/notebooks/data_generator.ipynb): Generates custom data to run experiment on one or more attributes.
- [analyze.R](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/notebooks/analyze.R): Script runs GBM model and tests for violations of statistical parity.
- [20221208_ted_build_trees.R](https://github.com/Lighthouse-Reports/suspicion_machine/blob/main/notebooks/build_trees.R): Extract and visualize decision tree

## Contributing Members

**Justin-Casimir Braun | https://github.com/jusbraun | justin-casimir@lighthouseports.com**

**Htet Aung | https://github.com/NecklessCage | htet@ligthhousereports.com**

**Gabriel Geiger | https://github.com/gheghi18 | gabriel@lighthousereports.com**

**Eva Constantares | eva@lighthousereports.com**

