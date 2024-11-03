#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <executable> <input_dir> <output_dir>"
  exit 1
fi

# Assign arguments to variables for readability
EXECUTABLE=$1
INPUT_DIR=$2
OUTPUT_DIR=$3

# Check if executable exists
if [ ! -x "$EXECUTABLE" ]; then
  echo "Error: Executable '$EXECUTABLE' not found or not executable."
  exit 1
fi

# Check if input and output directories exist
if [ ! -d "$INPUT_DIR" ]; then
  echo "Error: Input directory '$INPUT_DIR' does not exist."
  exit 1
fi

if [ ! -d "$OUTPUT_DIR" ]; then
  echo "Error: Output directory '$OUTPUT_DIR' does not exist."
  exit 1
fi

# Loop over all .input files in the input directory
for input_file in "$INPUT_DIR"/*.izly; do
  # Get the base filename without extension
  base_name=$(basename "$input_file" .izly)
  
  # Define the expected output file
  expected_output_file="$OUTPUT_DIR/$base_name.output"
  
  # Check if expected output file exists
  if [ ! -f "$expected_output_file" ]; then
    echo "Warning: Expected output file '$expected_output_file' not found. Skipping."
    continue
  fi
  
  # Run the executable with the input file and capture the output
  actual_output=$("$EXECUTABLE" "$input_file")
  
  # Read the expected output
  expected_output=$(cat "$expected_output_file")
  
  # Compare the actual output with the expected output
  if [ "$actual_output" == "$expected_output" ]; then
    echo "Test passed for '$base_name'"
  else
    echo "Test failed for '$base_name'"
    echo "Expected output:"
    echo "$expected_output"
    echo "Actual output:"
    echo "$actual_output"
  fi
done
