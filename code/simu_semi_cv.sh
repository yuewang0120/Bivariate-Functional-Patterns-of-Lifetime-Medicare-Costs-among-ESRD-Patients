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
for i in {1..10}
do
    for j in 0.5 1 1.5 2 2.5 3
    do
        for k in {1..5}
        do
            sbatch --output=output/%j.log --error=error/%j.log simu_semi_cv.sub $i $j $k
        done
    done
done
