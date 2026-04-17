# InterBIC_Npools
This is the repository containing all files necessary to run the analysis of Npools for the project called "InterBiomIC", greenhouse and field trial 2024. Research project ran in collaboration with the CRA-W and its platform SyCBio.

## Describe raw date sets here

--> Don't forget to explain variables: name, acronym description, unit



## Describe scripts here

** 1_import_tidy.qmd **
Imports data and metadata.
Imported data is presented in the shape of a 96-well plate. Thse script transforms it into a vectorized format where 
- one line = 1 unique well of 1 plate
- columns characterize each well with well data (column, row within plate, plate id, N species), with the last 3 columns containing 
    - "plate map" information (sample id, blank or standard curve)
    - raw absorbance readings
    - name of the data set (this helps to filter for single data sets later on)
Metadata is imported from a file containing one row per 96-well plate that characterizes plates
Both types of data sets are exported again in a more convenient format than the original one

** 2_absorbance_pipeline.qmd **
Starts from the clean, vectorized data sets that were produced in the previous script (containing raw absorbances and sample id / standard curve / blank).
It operates a series of transformations and quality checks to transform the raw absorbance data into concentrations of N [mg N / L]. This dosage is in a raw form, it does not account for any dilution or correction.
Steps are:
- import data
- identify suspicious wells (raw absorbance out of user-defined range) and take exclusion decisions
- correct absorbance for blank (separate steps for the standard curve and the samples). This step includes a manual exclusion of outlier values for blanks before computing the average value
- compute the per-plate regression between concentration and absorbance 
    - check quality of all regression curves based on R-square and p-value 
    - possible outlier removal
    - check inter-plate variability (plot) to view batch effect
- re-compute per-plate linear model (regression) to derive final slope coefficient 
- apply regression equation to all blank-corrected absorbance values to transform data into concentration of N species
- convert concentrations in mg N / L (correction on molar masses of N species and N)
- export data

** 3_data_tidying.qmd **
Tidies data sets into the shape that we need for data transformation and following analysis and visualization
Transforms data to compute some of the new variables (those that are needed per sample accross data sets, as the next steps will then happen on a per data set basis)
- this last step not yet implemented... coming up! (especially: computation of dry matter)

Absorbance pipeline

- goes from raw absorbance data:
  - the values of absorbance in the shape of a table
  - the plate map, also in the same shape of a table
  - plate information such as plate id, where to find the extractant, the standard curve, etc
- to corrected absorbance data:
  - blanc-corrected values computed for samples and for the standard curve
- and the computation of regression equation for the standard curve using a linear model fitted to include the origin (intercept = 0)
