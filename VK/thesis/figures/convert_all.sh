#!/bin/bash

# Compile the .tex file
pdflatex convert_tikz_to_svg.tex

# Check if the compilation was successful
if [ $? -eq 0 ]; then
  echo "LaTeX compilation successful."

  # Make the .sh script executable
  chmod +x convert_to_svg.sh

  # Execute the .sh script
  ./convert_to_svg.sh
else
  echo "LaTeX compilation failed."
fi