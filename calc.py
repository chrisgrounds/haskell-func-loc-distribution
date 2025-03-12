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

        # Match function definitions (without indentation)
        match = re.match(r"^(\w+)(\s+[\w|']+)*\s*=\s", stripped)
        if match:
            if current_function:
                function_locs[current_function] = loc_count
            
            current_function = match.group(1)
            loc_count = 1  # Start new function count
        elif current_function:
            loc_count += 1  # Count continued lines within the function
    
    if current_function:
        function_locs[current_function] = loc_count  # Add last function

    return function_locs

# Example usage
file_path = "aeson.hs"
loc_counts = dict(sorted(count_haskell_function_loc(file_path).items(), key=lambda x: x[1], reverse=True))

with open('out.json', 'w') as f:
    json.dump(loc_counts, f, indent=2)

# Get top 20 functions by LOC for better readability
top_20_functions = dict(list(loc_counts.items())[:20])

plt.figure(figsize=(15, 8))
plt.hist(list(loc_counts.values()), bins=range(1, max(loc_counts.values()) + 2), align='left')
plt.xlabel('Lines of Code')
plt.ylabel('Number of Functions')
plt.title('Distribution of Function Sizes')
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# print(loc_counts)