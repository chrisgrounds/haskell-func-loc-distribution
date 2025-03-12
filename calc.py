import re
import json
import matplotlib.pyplot as plt

def count_haskell_function_loc(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    
    function_locs = {}
    current_function = None
    loc_count = 0
    multi_line_comment = False
    module_declaration = False
    
    for line in lines:
        stripped = line.strip()
        
        # remove comments
        if "{-" in stripped:
            multi_line_comment = True
        if "-}" in stripped:
            multi_line_comment = False
            continue
        if multi_line_comment:
            continue

        if "{-#" in stripped and "#-}" in stripped:
            continue

        if "::" in stripped:
            continue

        # remove module declarations
        if "module " in stripped:
            module_declaration = True
        if ") where" in stripped:
            module_declaration = False
            continue
        if module_declaration:
            continue

        if stripped.startswith("--") or not stripped:
            continue
          
        # remove type definitions
        if stripped.startswith("data"):
            continue
          
        if stripped.startswith("type"):
            continue
          
        if stripped.startswith("newtype"):
            continue
          
        if stripped.startswith("let") and current_function:
            loc_count += 1
            continue
        
        if stripped.startswith("let"):
            continue
    
        if stripped.endswith(","):
            continue

        if stripped.startswith("instance"):
            continue

        match = re.match(r"^(\w+)(\s+[\w|']+)*\s*=[\r\n|\s]+", stripped)
        if match:
            if current_function:
                function_locs[current_function] = loc_count
            
            current_function = match.group(1)
            loc_count = 1
        elif current_function:
            loc_count += 1
    
    if current_function:
        function_locs[current_function] = loc_count  # Add last function

    return function_locs

# Example usage
file_path = "raw_lib_data.hs"

with open(file_path, 'r') as f:
    lines = f.readlines()

filtered_lines = [
    line for line in lines if not (
        line.strip().startswith("--") 
        or line.strip().startswith("{-")
        or line.strip().startswith("-}")
        or line.strip().startswith("#-}")
        or line.strip().endswith("-}")
        or line.strip().startswith("import")
        or line.strip().startswith("qualified")
        or line.strip().startswith("module")
        or line.strip().startswith(") where")
    )
]

with open(file_path, 'w') as f:
    f.writelines(filtered_lines)

loc_counts = dict(sorted(count_haskell_function_loc(file_path).items(), key=lambda x: x[1], reverse=True))

# with open('out_raw.json', 'w') as f:
#     json.dump(loc_counts, f, indent=2)

with open('out_raw.json', 'r') as f:
    loc_counts = json.load(f)

histogram = {}
for loc in sorted(loc_counts.values()):
    histogram[loc] = histogram.get(loc, 0) + 1

with open('out_histogram.json', 'w') as f:
    json.dump(histogram, f, indent=2)

plt.figure(figsize=(15, 8))
plt.hist(list(loc_counts.values()), bins=range(1, max(loc_counts.values()) + 2), align='left')
plt.xlabel('Lines of Code')
plt.ylabel('Number of Functions')
plt.title('Distribution of Function Length in Standard Haskell Libraries')
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
