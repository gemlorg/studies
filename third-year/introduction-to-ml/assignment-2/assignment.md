# Introduction to Machine Learning 2024

### Project II, deadline: June 10, 2024, 23:59

## Preliminary Information

### Data Description

Organs, such as the pancreas, consist of many types of tissues, and these tissues, in turn, consist of many types of cells. Within the pancreas, we can distinguish cells typical only for this organ, such as alpha or beta cells, but also cells associated with blood supply or the immune system.

The data in this task comes from multimodal single-cell RNA sequencing (scRNA-seq). Using scRNA-seq allows studying samples in high resolution and separating cells of different types. It is possible, among other things, to compare pathological cells, taken from cancer patients, with healthy cells. In multimodal scRNA-seq technology, for each cell, we receive two types of readings:

- RNA transcript counts corresponding to gene expression in a given cell;
- Protein abundance, which is directly related to the cell type.

The result of the scRNA-seq experiment is matrices where each cell is assigned an RNA signal from thousands of genes (in our task, X) and a signal from several dozen surface proteins (in our task, for simplicity, we chose a single protein, CD361, y).

According to the central dogma of biology, we know that genetic information flows from RNA to proteins. Therefore, we expect a correlation between the amount of protein and the expression of the gene encoding that protein. For technical and biological reasons, this dependence often degenerates. The problem in this task is to predict the protein abundance signal based on gene expression. Predicting the protein abundance signal is crucial for most publicly available datasets, which only contain an RNA matrix. Analyzing the gene expression and protein abundance signals significantly facilitates the process of identifying and naming cells in the sample.

The data was collected from the bone marrow of human donors. The collected cells are mostly immune system cells. Correctly identifying T lymphocytes based on both types of readings in such a dataset could be the basis for developing targeted cancer therapies (for those interested: CAR T cell therapy).

[More about CD36](https://en.wikipedia.org/wiki/CD36)

[More about CAR T cell therapy](https://en.wikipedia.org/wiki/Chimeric_antigen_receptor_T_cell)

### Data Download Instructions

On the course's Moodle page, there is a link to the folder containing the data for each lab group. Since each group works on data from a different experiment, the results between groups may vary. The data is compressed and saved in .csv format. Three files will be provided:

- `X_train.csv` and `X_test.csv`, containing RNA matrices. Each row corresponds to a cell, each column to a gene, and the values represent the expression level. The columns of these matrices are our explanatory variables.
- `y_train.csv`, corresponding to the amount of a certain type of surface protein in the cells (those corresponding to the data from `X_train.csv`). This is our response variable.

In the further description, the data from `X_train.csv` and `y_train.csv` will be referred to as training data, and the data from `X_test.csv` will be referred to as test data.

### Submission Instructions

Submit the following files on the course's Moodle page, in the dedicated area for Project II:

- A report in a Jupyter notebook (`.ipynb`), implementing the tasks described below (file name template: `[StudentID] report.ipynb`, e.g., `123456_report.ipynb`).
- The report should be structured to guide the reader naturally through the solutions to the tasks comprising this project.
- Predictions on the test data (see task 4) in a .csv file containing a column `Id` with observation numbers and a column `Expected` with predicted values (file name template: `[StudentID] prediction.csv`, e.g., `123456_prediction.csv`).

Please double-check before submitting to ensure the file names match the required templates and that the .csv file is correctly prepared (two appropriately named columns, the correct number of rows corresponding to the test dataset).

### Evaluation

The project can earn up to 30 points. The maximum points for each subtask are provided in parentheses. The evaluation of tasks 1 to 4 will consider:

- Execution of the outlined commands,
- Quality of the report, i.e., logical structure, visualizations, text readability, description of results, and explanations of actions taken,
- Quality of the code used for this purpose. Ensure it is readable and reproducible.

Additional details on the scoring can be obtained from your lab instructor.

**Note**: Tasks submitted after the deadline will not be graded and will receive 0 points.

## Tasks

### Task 1: Exploration (7 points)

1. Check the number of observations and variables in the loaded training and test data. Examine the types of variables and, if deemed appropriate, perform the necessary conversions before further analysis. Ensure the data is complete.
2. Examine the empirical distribution of the response variable (provide several basic statistics, include a histogram or density plot).
3. Select the 250 explanatory variables most correlated with the response variable. Calculate the correlation for each pair of these variables. Illustrate the result with a heatmap.
   **Note**: The variable selection described here is only for this subtask; the analysis described in the subsequent tasks should be conducted on the full training dataset.

### Task 2: ElasticNet (7 points)

The first model to be trained is ElasticNet, a hybrid of ridge regression and lasso.

1. Present information about the ElasticNet model in the report, explaining the parameters being estimated, the optimized function, and the hyperparameters on which it depends. For what values of the hyperparameters do we get ridge regression, and for what values lasso?
2. Define a grid of hyperparameters, based on at least three values for each hyperparameter. Ensure the grid includes hyperparameter configurations corresponding to ridge regression and lasso. Use cross-validation to select the appropriate hyperparameters (decide on the number of subsets used in cross-validation and justify your choice).
3. Provide the training and validation error of the model (average the result across all subsets distinguished in cross-validation).

### Task 3: Random Forests (8 points)

In this part of the project, train a Random Forests model and compare its performance with the previously created ElasticNet model.

1. From the many hyperparameters characterizing the Random Forests model, choose three different ones. Define a three-dimensional grid of searched hyperparameter combinations and, using cross-validation, select their optimal values (in the context of the prediction being performed). The data partition used for cross-validation should be the same as in the case of ElasticNet.
2. Summarize the cross-validation results in a table for both considered models. (This comparison is why we want to use the same partitions). Determine which model you consider the best (justify your choice). Include a basic reference model in the comparison, which assigns the arithmetic mean of the response variable to any values of the explanatory variables.

### Task 4: Prediction on the Test Set (8 points)

This part of the project is open-ended. Based on the training data, fit any chosen model, and then use it to predict the response variable values in the test dataset. Describe the method of selecting and building the model, as well as the motivations behind the choice, in the report. Send the generated predictions to the instructor in a separate file, as previously described. The number of points earned will depend on the quality of the predictions, measured by the root mean squared error (RMSE).

**Scoring Details**:

- (1 point) – for an error lower than that of the basic reference model described earlier.
- (2 points) – for an error lower than that of the ElasticNet model trained by the lab instructors.
- (5 points) – this bonus is calculated using the formula \(12⌊10F̂ (e)/3⌋\), where \(e\) is the student's test error, \(F̂\) is the empirical distribution of all submitted predictions' errors in the student's lab group, and \(⌊·⌋\) is the floor function.
