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
# for i in {1..10}
# do
    for j in 20 30 40 50 60
    do
        for k in {1..5}
        do
            # echo "$i+$j"
            sbatch real_semi_cv.sub $j $k
        done
    done
# done
# for i in {21311735..21311784}
# do
#     scancel $i
# done