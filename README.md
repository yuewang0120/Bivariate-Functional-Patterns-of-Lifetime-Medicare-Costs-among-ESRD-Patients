# Bivariate-Functional-Patterns-of-Lifetime-Medicare-Costs-among-ESRD-Patients

# Author Contributions Checklist Form 

## Data

### Abstract

The dataset contains the Medicare claims from End-Stage Renal Disease (ESRD) patients, organized by the United States Renal Data System (USRDS). Our analysis only concerns patients that had their first ESRD service in the year of 2007 to 2011. According to the data use agreement, the data can not be made available to the public. Researchers who want to obtain access to the data should contact the USRDS for details on the procedure for requesting access to the data.

### Availability

According to the data use agreement, the data cannot be made public. Those who wish to get access to the dataset could visit [USRDS SAFs](https://www.usrds.org/for-researchers/standard-analysis-files/) and submit a request. For reproducibility, we have created datasets that mimics the real USRDS data set used in the paper. The pseudo datasets are publicly available under `data` folder at the time of submission. The pseudo dataset is generated from the models in the paper using coefficients estimates from the original datast and simulated  covariates and errors.

### Description

See `usrds_desc.txt` under `data` folder.

## Code

### Abstract

Researchers could use the R scripts and Shell scripts to reproduce the results in the paper, including Figures 1-2 and Tables 2. Since Figure 3-5 and Table 3-4 are generated using the original dataset, they are not reproducible. Readers can generate a pseudo version of Figure 3-5 and Table 3-4 using the pseudo datasets under `data` folder following the instructions provided below (Step 3 and 4). The pseudo Figure 3-5 are also provided under `pseudo_figures` folder for reference.

As requested by reviewers, we also provide 3D versions of the figures that appear in the paper, which can be found under `3d_figures` folder. Note Figure 3-5 in this folder are generated from the original dataset, which should not be confused with the figures under `pseudo_figures` folder. All the 3D figures are generated using [Plotly](https://github.com/plotly/plotly.R) and provided in the form of HTML files. Since they cannot be viewed directly on GitHub, we suggest downloading and opening them in a browser.

## Instructions for Use

### 1. Simulation for the Semi-Varying Coefficient Model (Section 4.1)

A bandwidth of 1.12 is used for simulation, which is an average of the cross-validated bandwidths on 10 simulated datasets. For replication of the 10 bandwidths readers can run the following code in the command line

```console
sh simu_semi_cv.sh
Rscript simu_semi_cv_merge.R
```

Run simulation with 500 replications using the undersmoothed bandwidth $h=1.12$

```console
sh simu_semi_fit.sh
```

This will generate intermediate results, stored under `code` folder. Once this is done, run

```console
Rscript simu_semi_plot.R
```

to generate Figure 1 (both 2D and 3D versions) under `code` folder and display Table 2.

### 2. Simulation for the Mixed-Time Varying-Coefficient Mode (Model 4.2)

Bandwidths of 2.1 and 1.1 are used for the first and second stages of the simulation, each of which is an average of the cross-validated bandwidths on 10 simulated datasets. For replication of the 10 bandwidths in the first stage, readers can run the following code 

```console
sh simu_mixed_cv1.sh
Rscript simu_mixed_cv1_merge.R
```

Then run first-stage simulation with 500 replications using the selected bandwidth $h_1=2.1$, this will create the response for the second stage

```console
sh simu_mixed_fit12.sh
```

After that, we can obtain the second-stage bandwidths on 10 simulated datasets by running

```console
sh simu_mixed_cv2.sh
Rscript simu_mixed_cv2_merge.R
```

Run second-stage simulation with 500 replications using the undersmoothed bandwidth $h_2=1.1$

```console
sh simu_mixed_fit2.sh
```

This will generate intermediate results, stored under `code` folder. Once this is done, run

```console
Rscript simu_mixed_plot.R
```

to generate Figure 2 (2D and 3D) under the same folder.

### 3. ESRD Data Analysis of Cost Difference Associated with Waitlisting (Section 5.1)

Replicate cross-validation

```console
sh real_semi_cv.sh
Rscript real_semi_cv_merge.R
```

Fit the model with the selected bandwidth

```console
sh real_semi_fit.sh
```

This will generate the intermediate result under `code` folder. Run

```r
Rscript real_semi_plot.R
```

to generate Figure 3 (2D and 3D) and display Table 3 and 4. 

### 4. ESRD Data Analysis of Cost Difference Associated with Transplant (Section 5.2)

Replicate first-stage cross-validation

```console
sh real_mixed_cv1.sh
Rscript real_mixed_cv1_merge.R
```

Create the response for the second stage

```console
sh real_mixed_fit12.sh
```

Replicate second-stage cross-validation

```console
sh real_mixed_cv2.sh
Rscript real_mixed_cv2_merge.R
```

Fit the second-stage model with the selected bandwidth

```console
sh real_mixed_fit2.sh
```

This will generate the intermediate result under `code` folder. Run

```console
Rscript real_mixed_plot.R
```

to generate Figure 4 and Figure 5 (both in 2D and 3D). 
