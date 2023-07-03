# socialGeneralization - COSMOS

  This repo contains the code and sample data for a [COSMOS 2023](https://cosmos-konstanz.github.io/) group project. The code and data are from [Social learning with a grain of salt](https://psyarxiv.com/c3fuq/), which investigates how humans integrate social information that is not directly applicable to their own situation.
  
## ./data

has saved simulated data, as well as the behavioural and fitting data from 3 groups of participants
- ./recovery_data contains the raw fitting data
- ./evoSim contains the raw evolutionary simulation data

## ./environments

home to the environments used in the experiment

## ./simulation

has all code used for simulating and fitting data

- `modelSim.py` has simulation, parameter generation and necessary helper (GP,UCB) functions
- `modelFit.py` computes negative log likelihood for different models given the data
- `modelFitting.py` and `modelRecovery.py` are functions formatted to run the fitting routine on real and simulated data respectively
- `fitting_synthesis.py` and `mrecov_parameteradd.py` format the raw data for further analysis
- `recovery_analyses.R` plots model and parameter recovery plots

## ./analysis

has analysis-related code

- `CogSciAnalysis.R` has plotting for results plots and testing from the paper
- `behav_measures.py` adds previous rewards and search distances to data (already run on the saved data), and saves data as a long dataframe for regression (needs to be run locally)
- `evoSimSynthesis.py` turns evoSim cluster output into one dataframe (needs to be run locally)