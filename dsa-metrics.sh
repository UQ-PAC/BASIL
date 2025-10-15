#!/bin/bash

# Check if directory argument is provided
if [ $# -eq 0 ]; then
	echo "Usage: $0 <directory>"
	exit 1
fi

DIR="$1"

# Check if directory exists
if [ ! -d "$DIR" ]; then
	echo "Error: Directory '$DIR' does not exist"
	exit 1
fi

# Output CSV file
OUTPUT_CSV="dsa-results.csv"

# Write CSV header
echo "Program,dsa-default Nodes,dsa-default Nodes Collapsed,dsa-default Max Node Density,dsa-default Cells,dsa-default Max Cell Density,dsa-default Running Time,dsa-split Nodes,dsa-split Nodes Collapsed,dsa-split Max Node Density,dsa-split Cells,dsa-split Max Cell Density,dsa-split Running Time,dsa-eqv Nodes,dsa-eqv Nodes Collapsed,dsa-eqv Max Node Density,dsa-eqv Cells,dsa-eqv Max Cell Density,dsa-eqv Running Time" > "$OUTPUT_CSV"

# Process each .gts file
for gts_file in "$DIR"/*.gts; do
	# Skip if no .gts files found
	[ -e "$gts_file" ] || continue
	
	# Get the base filename without extension
	base_name=$(basename "$gts_file" .gts)
	full_path="${gts_file%.gts}"
	
	echo "Processing $base_name..."
	
	# Run the three commands
	./mill run --input "$gts_file" --simplify --dsa=
	./mill run --input "$gts_file" --simplify --dsa= --dsa-split
	./mill run --input "$gts_file" --simplify --dsa= --dsa-split --dsa-eqv
	
	# Read the output files
	default_stats="${base_name}-dsa-stats.txt"
	split_stats="${base_name}-dsa-split-stats.txt"
	eqv_stats="${base_name}-dsa-split-eqv-stats.txt"
	
	# Check if all output files exist
	if [ ! -f "$default_stats" ] || [ ! -f "$split_stats" ] || [ ! -f "$eqv_stats" ]; then
		echo "Error: Output files not found for $base_name"
		exit 1
	fi
	
	# Read values from each file
	default_vals=$(<"$default_stats")
	split_vals=$(<"$split_stats")
	eqv_vals=$(<"$eqv_stats")
	
	# Write to CSV
	echo "$base_name,$default_vals,$split_vals,$eqv_vals" >> "$OUTPUT_CSV"
	
	# Clean up temporary files
	rm -f "$default_stats" "$split_stats" "$eqv_stats"
done

echo "Processing complete. Results saved to $OUTPUT_CSV"