---
title: "README"
output: html_document
---

# **Introduction**

Several scientific studies have highlighted how Near-Infrared Reflectance (NIR) spectroscopy can serve
as a valuable alternative tool for assessing the freshness of foods of animal origin. This technique is 
non-invasive and non-destructive to samples and, if proven effective, could be deployed on a large scale 
to optimize food quality evaluation processes.

The present work represents the latest in a series of analyses previously conducted in collaboration with 
researchers from the Faculty of Veterinary Medicine in Milan, aimed at evaluating the practical applicability
of NIR spectroscopy for the identification and classification of perishable fish fillets.

This is a preliminary study whose primary objective is to investigate the nature of the spectral data and 
their classification capability.

The limited sample size (n = 49) and the strong class imbalance had a substantial impact on the results.
Nevertheless, the findings outline potential directions for future investigations on larger and more
balanced datasets.

# **Analysis**

The spectra were smoothed and normalized on a per-sample basis, without altering the independence of
individual observations. The dataset contained missing values in variables required for the computation of
the K-value index, which represents a key aggregated reference measure for freshness assessment. After
verifying the non-normal distribution of the variables involved, missing values were imputed using
predictive mean matching. Finally, the dataset whose imputed values exhibited the closest distributional
similarity to the observed data was selected for subsequent analyses.

![](plot/imputation_7_K%20value.png)

Two main analytical approaches were then pursued:
- A feature-selection-based approach aimed at identifying the most informative wavelengths.
- A dimensionality-reduction approach using Principal Component Analysis (PCA) to reduce the number of
  non-informative variables while concentrating information into a smaller set of highly informative
  components.

![](plot/spectral%20smoothing%20comparison.png)

Given the limited classification performance observed when using individual wavelengths or the first
two principal components, the analysis was further refined by applying feature selection to the first 10
principal components (accounting for approximately 99% of the total variance). This allowed the models to
select the components with the greatest discriminative power.

![](plot/pca%20scatterplot.png)

All analyses were conducted with careful attention to avoiding bias and data leakage. Data preprocessing
and feature selection were fully embedded within the training pipeline using nested cross-validation.
