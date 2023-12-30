#!/bin/bash
########################################################
# job_builder.sh
# Job Arrays without the Resource Manager # Version 1.0.0 ########################################################
# James Joseph Balamuta
# balamut2@illinois.edu
########################################################
# ## Example
#
# # Allow the builder script to work on the file system # chmod +x job_builder.sh # # # Run the job builder # ./job_builder.sh ########################################################


### Builds the job index
# Create a sequential range
# hlist=`seq 8`


# Launch the job and then remove the temporarily created qsub file.
for i in 20 40 60 80 100 150 200 250 300 400 500 600 800 1000
# for i in 60 80 150 800
do
    for j in {1..5}
    do
        for k in {1..6}
        do
            sbatch --output=output/%j.log --error=error/%j.log real_mixed_cv1.sub $i $j $k
        done
    done
done
