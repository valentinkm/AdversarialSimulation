#!/bin/bash

pdf_file="convert_tikz_to_svg.pdf"
model_names=(
    "model0"
    "model0_1"
    "model1_1"
    "model1_2"
    "model1_3"
    "model1_4"
    "model2_1"
    "model2_2"
    "model3_1"
    "model3_2"
)

for i in "${!model_names[@]}"; do
    page_num=$((i + 1))
    output_file="${model_names[$i]}.svg"
    pdf2svg "$pdf_file" "$output_file" "$page_num"
    echo "Converted page $page_num to $output_file"
done