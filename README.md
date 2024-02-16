# RPackages


## What to find here

As I am currently writing my thesis about Survey Satisficing, I need to build several indicators to make that unpleasent response behavior visible. I try to automate as much as possible and intend to store my functions here.

## Current functions in package

### Cleaning Agents

- setna 
    - customized function to set certain values na, based on sjmisc::set_na
- recode_likert
    - recodes likert scales
    - optional: set missings NA


### Satisficing Agents

- calc_nondiff
    - calculates the mean root of pairs for item packages as a proxy for nondifferentiation, based on explanations of Kim et al., 2019

