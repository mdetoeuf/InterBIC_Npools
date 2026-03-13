# InterBIC_Npools
This is the repository containing all files necessary to run the analysis of Npools for the project called "InterBiomIC", greenhouse and field trial 2024. Research project ran in collaboration with the CRA-W and its platform SyCBio.

## Describe raw date sets here

--> Don't forget to explain variables: name, acronym description, unit



## Describe scripts here

Absorbance pipeline

- goes from raw absorbance data:
  - the values of absorbance in the shape of a table
  - the plate map, also in the same shape of a table
  - plate information such as plate id, where to find the extractant, the standard curve, etc
- to corrected absorbance data:
  - blanc-corrected values computed for samples and for the standard curve
- and the computation of regression equation for the standard curve using a linear model fitted to include the origin (intercept = 0)
