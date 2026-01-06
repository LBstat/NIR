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

# **Analysis**

Two main analytical approaches were then pursued:
- A feature-selection-based approach aimed at identifying the most informative wavelengths.
- A dimensionality-reduction approach using Principal Component Analysis (PCA) to reduce the number of
  non-informative variables while concentrating information into a smaller set of highly informative
  components.





All analyses were conducted with careful attention to avoiding bias and data leakage. Data preprocessing
and feature selection were fully embedded within the training pipeline using nested cross-validation.
