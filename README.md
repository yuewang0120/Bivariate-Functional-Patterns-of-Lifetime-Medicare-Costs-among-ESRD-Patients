# Bivariate-Functional-Patterns-of-Lifetime-Medicare-Costs-among-ESRD-Patients

# Author Contributions Checklist Form 

## Data

### Abstract

The dataset contains the Medicare claims from End-Stage Renal Disease (ESRD) patients, organized by the United States Renal Data System (USRDS). Our analysis only concerns patients that had their first ESRD service in the year of 2007 to 2011. According to the data use agreement, the data can not be made available to the public. Researchers who want to obtain access to the data should contact the USRDS for details on the procedure for requesting access to the data.

### Availability

According to the data use agreement, the data cannot be made public. Those who wish to get access to the dataset could visit [USRDS SAFs](https://www.usrds.org/for-researchers/standard-analysis-files/) and submit a request. For reproducibility, we have created datasets that mimics the real USRDS data set used in the paper. The pseudo datasets are publicly available under `data` folder at the time of submission.

### Description

See `usrds_desc.txt` under `data` folder.

## Code

### Abstract

Researchers could use the R scripts and Shell scripts to reproduce the results in the paper (including Figures 1-2, Tables 2). However, since Figure 3-5 are generated using the original dataset, they are not reproducible. Readers can generate a pseudo version of Figure 3-5 using the pseudo datasets under folder “data”. The pseudo Figure 3-5 are also provided under “pseudo_figures” folder.

## Instructions for Use

### 1. Simulation for the Semi-Varying Coefficient Model (Section 4.1)

A bandwidth of 1.12 is used for simulation, which is an average of the cross-validated bandwidths on 10 simulated datasets. For replication of the 10 bandwidths readers can run

```console
sh simu_semi_cv.sh
Rscript simu_semi_cv_merge.R
```

Run simulation with 1000 replications with the selected bandwidth $h=0.66$

```r
source('code/simu.R')
```

This will generate intermediate results, stored in `simu_result.RData` under `code` folder. Once this is done, run

```r
source('code/figure1-2.R')
```

to generate Figure 1 and 2 under the same folder.

### 2. A Separate Simulation with $\beta_3=0.5$ (Section 4)

A bandwidth of 0.68 is used for simulation, this can be validated by replicating the cross-validation procedure

```r
source('code/simu_cv_beta3=0.5.R')
```

Run simulation with 1000 replications with the selected bandwidth $h=0.68$

```r
source('code/simu_beta3=0.5.R')
```

This will generate intermediate results, stored in `simu_result_beta3=0.5.RData` under `code` folder. Once this is done, run

```r
source('code/table1-2.R')
```

to generate Table 1 and 2 under the same folder.

### 3. ESRD Application with A Reduced Model (Section 5, with Pseudo Dataset)

Replicate cross-validation

```r
source('code/pseudo_cv.R')
```

Fit the model with the selected bandwidth

```r
source('code/pseudo_fit.R')
```

This will generate the intermediate result `pseudo_fit.RData` under `code` folder. Run

```r
source('code/figure3.R')
```

to generate Figure 3. Run

```r
source('code/table3-4.R')
```

to generate Table 3 and 4.

### 4. ESRD Application with A Full Model (Section C in the Online Supplementary Material, with Pseudo Dataset)

Replicate cross-validation

```r
source('code/pseudo_full_cv.R')
```

Run

```r
source('code/pseudo_full_fit.R')
```

to generate intermediate result `pseudo_full_fit.RData` under `code` folder. Run

```r
source('code/figureS1.R')
```

to generate Figure S1. 

### 5. Simulation for the Locally Weighted Pseudo Likelihood Approach (Section D in the Online Supplementary Material)

Run

```r
source('code/likelihood_simu.R')
```

to generate intermediate result `likelihood_simu_result.RData` under `code` folder. Run

```r
source('code/figureS2.R')
```

to generate Figure S2.
