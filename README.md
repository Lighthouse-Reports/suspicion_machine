# Suspicion Machines

This project is part of the [Surveillance Newsroom](https://www.lighthousereports.nl/newsroom/surveillance/) at [Lighthouse Reports](https://www.lighthousereports.nl/). Other projects from Lighthouse Reports can be found at the [main GitHub repo](https://github.com/Lighthouse-Reports/).

## Project Overview
This project contains the source code for an investigation into the use of a welfare fraud prediction algorithm by the city of Rotterdam. The model used by Rotterdam is a [Gradient Boosting Machine (GBM)](https://deepai.org/machine-learning-glossary-and-terms/gradient-boosting) trained on a dataset of 12,707 past investigations into welfare fraud. This project contains source code for and results of the experiments conducted to explore and identify potential bias in the GBM model.

### Partners [TODO:include story links once they are live]
- WIRED: 
- Vers Beton: 
- Follow the Money: 
- Argos: 

### Methods Used
- Statistical Parity
- Inferential Statistics
- Data Wrangling
- Data Visualisation

### Technologies [maybe programming languages]
- R and R Markdown
- Python and Jupyter Notebook

## Project Description
This project explores the AI model's fairness regarding the use of various vulnerable attributes of the applicants. The AI model evaluates the welfare applicants based on 315 features and assigns each applicant  a risk score in [0, 1], where higher  risk score indicate greater risk of having committed some kind of 'illegality'. Illegality encompasses everything from simple mistakes on a form to serious fraud.
This project uses data obtained, through press questions and FOIA and press requests from Rotterdam Municipality, and employs the following approaches:
- Testing whether the welfare illegality risk system violates Statistical Parity and Conditional Statistical Parity
- Synthetic data generation for replication, since the original training data cannot be shared publicly due to GDPR concerns.
- Extraction of the model's decision trees

## Repo Overview [TODO: when we have moved everything to public repo]

- Raw Synthetic Data is kept [here](Repo folder containing raw data???) within this repo.
- Data processing/transformation scripts are kept [here](Repo folder containing data processing scripts/notebooks???)
- Experiment results are kept [here](???).
- Follow setup [instructions](Link to file `HowToRun.MD`)

## Featured Notebooks
- [20221020_jb_synth-model.ipynb]
- [20221119_jb_data-generator.ipynb]
- [20230207_jb_predict_analyze.R]
- [20221208_ted_build_trees.R]

## Contributing Members

**Htet Aung | https://github.com/NecklessCage | htet@ligthhousereports.com**

**Justin-Casimir Braun | https://github.com/jusbraun/ | justin-casimir@lighthouseports.com**

**Gabriel Geiger | https://github.com/gheghi18 | gabriel@lighthousereports.nl**

**Eva Constantares | eva@lighthousereports.nl**

