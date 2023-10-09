# php1550_project1

The main objective of this project is to examine the effects of Smoke During Pregnancy (SDP) / Environmental Tobacco Smoke (ETS) on adolescent self-regulation, substance use, and externalizing through an Exploratory Data Analysis. The data has 98 adolescents and mothers randomly selected from a larger data set originally collected for a study on smoke avoidance intervention to reduce low-income women’s (N=738) smoking, and ETS exposure during pregnancy and children’s exposure to ETS in the immediate postpartum period. The data includes: some background information for both the adolescents and mothers (age, race, sex, etc), mother's smoking status, smoke exposures from mother to child, brief problem monitor, emotion regulation responses, parental knowledge responses, and SWAN ratings. 

* The `pda_project.R` file performs some pre-processing on the data
  * summarizes the `bpm` (brief problem monitor), `erq` (emotion regulation), `pmq` (parental knowledge) variables
  * includes relevant variables and drops repetitive or unuseful variables
* The `project1_EDA.R` file includes further pre-processing and exploratory data analysis
  * descriptive statistics of selected variables
  * missing patterns within data set
  * correlations among relevant variables
  * simple regressions for main effects of independent variables
* The `Tables and Figures` folder includes all tables and figures generated from the EDA
