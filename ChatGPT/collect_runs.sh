#/bin/bash
for file in */run.txt; do
    echo "In file $file:"
    dir_name=$(dirname "$file")

    grep -i -E "CET|iterations|called|user|matrix|complex" "$file" > "../_Results/${dir_name}_results.txt"
    echo ""
done


