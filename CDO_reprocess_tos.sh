#!/bin/bash

# script to regrid tos

# Start with TOS
indir="/Users/jason/Nextcloud/MME1Data/ZooMSS_Climate_Change/CMIP5/raw/tos/"
outdir="/Users/jason/Nextcloud/MME1Data/ZooMSS_Climate_Change/CMIP5/regrid/tos/"

# Declare an array of string with type
declare -a ModelArray=("IPSL-CM5A-LR" "GFDL-ESM2M")
declare -a ExpArray=("historical" "rcp26" "rcp85")

# Iterate the string array using for loop
for m in ${ModelArray[@]}; do
   echo $m

	for e in ${ExpArray[@]}; do
		echo $e
		curr_files=($(ls ${indir}*$m*$e*))
		num_files=${#curr_files[@]}
   		echo $curr_files

		for ((i=0; i<=num_files-1; i++)); do
			curr_file=${curr_files[i]}
			cdo -sinfov $curr_file # Print the current details of the file

			out_name1="tempfile_tos.nc"
			cdo -L -remapbil,global_1 -selyear,1950/2100 -selname,tos $curr_file $out_name1 # Remap to 1 degree global on the half-degree

			annual_name=${curr_file/_r1i1p1_/_r1i1p1_onedeg_} # Add onedeg to filename
			annual_name=${annual_name/_Omon_/_Oyr_} # Replace Omon with Oyr to indicate annual average
			annual_name=${annual_name/raw/regrid} # change directory
			cdo -yearmean $out_name1 $annual_name

			#Clean up
			rm $out_name1
		done
		
		merged_name=${annual_name::${#annual_name}-16} # Remove the dates
		merged_name=${merged_name/_onedeg_/_onedeg_merged.nc} # Add merged tag
		merged_name=${merged_name/regrid/merged} # change directory
		cdo -O -mergetime $outdir*$m*$e* $merged_name

	done
done
