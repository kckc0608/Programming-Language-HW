#!/bin/bash

# Script to process test files using yacc and compare the outputs

for i in {1..35}
do
    # Process test file using yacc and save output to a temporary file
    ./yacc < test/test$i.c > temp.out
    
    # Compare the new output file with the existing one
    echo "Comparing $i.out with new output:"
    diff test/$i.out temp.out
    #rm temp.out
done

# # Clean up the temporary file
# rm temp.out

echo "Script completed."