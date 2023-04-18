# Bottle of Lies

Study of the effects of different generic manufacturers on health outcomes in Medicare Part D.

#### Code Contributors
* Evan Flack (evanjflack@gmail.com)
* Tyler Mansfield (tyler.mansfield96@gmail.com)

#### Additional Contributors
* Amitabh Chandra
* Ziad Obermeyer

### Directories
* `00_pre_process:` Scripts to set librefs in sas and load libraries in R
* `01_sample:` Initial subsetting of claims data (SAS), month-level sample generation (R), and subsetting of claims to classes of interest (R)
* `02_features:` Scripts to make health outcome and control variables
* `03_model:` Scripts to make final model data, initial summary statistics, model health outcomes on manufacturers, and estimate balance/falsification checks
* `supporting_code:` Scripts to load packages and define plot themes
