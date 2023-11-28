#/bin/bash
for file in */run.txt; do
    echo "In file $file:"
    grep -i -E "CET|iterations|called|user|matrix|complex" "$file"
    echo ""
done


