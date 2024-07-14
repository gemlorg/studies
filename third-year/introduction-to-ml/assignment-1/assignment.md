# WUM 2024 - Project 1

The goal of this task is the statistical analysis of data contained in the file `dane_projekt1.csv`.

## Data Description

These are simulated data describing a fragment of survey results concerning the consumer habits of the residents of the fictional land of Bajtocja, conducted on a representative sample. The data may contain random errors. Below is an explanation of the variable names used in the study:

- **id**: Observation identifier, contains no additional information.
- **waga**: Respondent's weight (in kg).
- **wzrost**: Respondent's height (in cm).
- **plec**: Respondent's gender as per their ID document (1 – "female", 2 – "male").
- **dzieci**: Number of children dependent on the respondent.
- **wiek**: Respondent's age (in years).
- **dochod**: Declared income of the respondent for the surveyed month (in bajtalars).
- **oszczednosci**: Declared savings of the respondent for the surveyed month (in bajtalars, negative values indicate expenses exceeded income).
- **jednoos**: Household status (1 – "single-person household", 0 – "multi-person household").
- **miejsce**: Size of the place where the respondent lives (1 – "up to 10,000 inhabitants", 2 – "10,000 to 100,000 inhabitants", 3 – "over 100,000 inhabitants").
- **wydatki_zyw**: Declared food expenses of the respondent for the surveyed month (in bajtalars).

## Task Instructions

The result should be a report in a Jupyter notebook (`.ipynb`). The report and comments must be sufficient to understand and reproduce your steps without needing to read your code. Every significant modification to the dataset (e.g., deleting records, modifying, and introducing new variables) must be justified and described. You can use ready-made implementations for each task. A place to submit the report will be available in the course's Moodle. The report will be graded by your group instructor.

**Submission Deadline**: May 8, 2024, 23:59. Please refer to the course's Moodle for the late submission policy.

**Total Points Available**: 30

## Tasks

### Task 1: Load, Review, and Summarize the Data

- **Helper Tasks**:
  - How many observations are there? Discuss the structure of the dataset: how many quantitative and qualitative variables are there? Are there any missing data? (1 point)
  - Present and comment on relevant frequency tables or descriptive statistics for the variables in the dataset (consider the type of variables). (1 point)
  - Present and comment (where relevant) on the distributions of variables, particularly comparing them visually with the normal distribution (e.g., using histograms, quantile-quantile plots, etc.). (2 points)

### Task 2: Check for Dependencies Between Variables

- Calculate and display a heatmap of the relevant correlation coefficient between quantitative variables, and also examine the dependency of qualitative variables. Comment on the results, particularly focusing on statistical significance. (3 points)

### Task 3: Summarize the Data with at Least Three Different Plots

- **Basic Set of Plots**:
  - Scatter plots for all quantitative variables against the variable `wydatki_zyw`.
  - Boxplot for one selected quantitative variable, divided by the respondents' place of residence.
  - Stacked bar chart for respondents' gender and whether they run a single-person household.

(3 points, each basic plot is worth 1 point: 0.25 points for the plot itself, 0.75 points for comments in the context of exploratory analysis. Additional plots that complement the exploratory analysis are welcome, e.g., bar charts, line charts, pie charts... – up to 1 additional point may be added by the evaluator for interesting additional visualizations.)

### Task 4: Calculate Two-Sided Confidence Intervals at a Confidence Level of 1 − α = 0.99 for the Variable `wiek` for the Following Distribution Parameters:

- Mean and standard deviation;
- Quartiles 1, 2, and 3.

Provide the assumptions used and comment on whether you consider them justified. (2 points: 0.25 points for the mean, 0.25 points for the variance, 0.75 points for the quartiles, 0.75 points for stating and commenting on the assumptions)

### Task 5: Discuss and Compare the Variation in Food Expenses Across Wealth Classes

- **Wealth Classes**:
  - Lower class (income below the 25th percentile of the income distribution)
  - Middle class (income at or above the 25th percentile and below the 75th percentile of the income distribution)
  - Upper-middle class (income at or above the 75th percentile and below the 90th percentile of the income distribution)
  - Upper class (income at or above the 90th percentile of the income distribution)

(2 points: 0.5 points for conducting the division, 1 point for calculating the appropriate measure of variation, 0.5 points for comments and discussion of the results)

### Task 6: Answer the Following Research Questions Using the Best-Suited Statistical Tests at a Significance Level of α = 0.01:

- Do women have higher savings than men?
- Is a lower proportion of food expenses relative to income correlated with higher savings?
- Is the average weight of women in the sample higher than 56 kg?

Also:

- Verify an additional (reasonable) hypothesis about compliance with a specific parametric distribution for a selected variable (e.g., "Variable A follows a Poisson distribution with parameter 1").

Provide the assumptions used and comment on whether you consider them justified. Each statistical test is worth 1 point (total of 4 points). Points are awarded for formulating the null and alternative hypotheses (0.25 points), justification/appropriateness of the chosen test (0.25 points), conducting the test (0.25 points), and providing the test conclusion (0.25 points).

### Task 7: Investigate Food Expenses Using Variables from the Database

- **Assume a significance level of α = 0.01.** To do this:
  - Estimate a preliminary model containing all variables from the original database (excluding `id`) and a constant, where the variable `wydatki_zyw` is the dependent variable. Remember to decode qualitative variables. (0.5 points)
  - Comment on R², joint and individual significance tests in the preliminary model. (1 point)
  - Check if the preliminary model meets the assumptions of the Classical Linear Regression Model (CLRM). Pay particular attention to issues of linearity of the functional form, homoscedasticity, and lack of autocorrelation of the random component, as well as the distribution of the random component. (2 points)
  - Check if there is an issue of imperfect multicollinearity in the preliminary model (0.5 points)
  - Using outlier analysis for the preliminary model, check if the database contains errors. If you find suspicious observations, decide and justify what you will do with them. (1 point)
  - Improve the model to meet as many CLRM assumptions as possible. Describe the steps taken to obtain the "best" model (4 points).

**Hint**: Consider different functional forms and variable transformations.

- Provide a quantitative interpretation of two individually significant coefficients in the "best" model. Remember that the constant is not interpreted. It is recommended to choose non-transformed variables. (1 point)
- What are the descriptive characteristics of people who have food expenses in the top 10% of food expense predictions in your "best" model? Check and discuss. (2 points)
