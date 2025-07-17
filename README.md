# Multivariate-analysis-2025 - DAPS&CO 2024/2025 - Università degli Studi di Milano

This repository contains my final project for the Multivariate analysis 2025 course

My project aims to replicate Mancosu and Vassallo's original paper "The life cycle of conspiracy theories: evidence from a long-term panel survey on conspiracy beliefs in Italy".
The original paper is available at: https://www.cambridge.org/core/journals/italian-political-science-review-rivista-italiana-di-scienza-politica/article/life-cycle-of-conspiracy-theories-evidence-from-a-longterm-panel-survey-on-conspiracy-beliefs-in-italy/83FD3CD09161EA356FB8A1EDA8B5E84B
The original paper's DOI is:  https://doi.org/10.1017/ipo.2021.57

Data are available at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/A8ISOH

The original STATA code has been rewritten in R. During this process, LLMs were used to find the optimal translation of the original code.
Human supervision and revision has always been at the core of the entire process.

## Project structure
The project is structured as follows:
- `00-Data/`: Contains the data files used in the analysis.
- `01-Code/`: Contains the R scripts used to replicate the analysis. The code is directly commented in the script.
- `02-Plots/`: Contains the plots of the results.
- `03-Output/`: Contains the output files (tables) of the analysis.
- `04-Full_paper` : Contains the final full paper in pdf.
- `README.md`: This file, which provides an overview of the project.

## Requirements
The code has been written in R version 4.4.3
To run the code, you will need to install the following R packages:
`library(haven)`       
`library(dplyr)`       
`library(psych)`
`library(margins)`
`library(tidyr)`       
`library(ggplot2)` 
`library(stargazer)`
`library(modelsummary)`


The paper is structured as follows:

- **Abstract**
- **Introduction**
- **Concept definition**
- **Underlying individual factors, the media context, social outcomes**
- **Data, variables, method and extension**
- **Replication results**
- **Possible explanations of the drop in conspiracism: the role of the media**
- **Discussion and Conclusion**
- **References**

