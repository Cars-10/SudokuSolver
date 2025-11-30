import os
import re

def get_sorted_solvers(base_dir="AI-2025"):
    solver_times = []
    
    for entry in os.listdir(base_dir):
        dir_path = os.path.join(base_dir, entry)
        if os.path.isdir(dir_path):
            run_file = os.path.join(dir_path, "run.txt")
            total_time = float('inf') # Default to slow if no data
            
            if os.path.exists(run_file):
                try:
                    with open(run_file, 'r', errors='ignore') as f:
                        content = f.read()
                        times = re.findall(r'Seconds to process ([\d.]+)', content)
                        if times:
                            total_time = sum(map(float, times))
                except:
                    pass
            
            # If total_time is 0 (e.g. failed runs), treat as very slow? 
            # Or if it ran fast but failed? 
            # Let's trust the time. If 0.0, it's fast.
            
            solver_times.append((entry, total_time))
    
    # Sort by time (asc), then name
    solver_times.sort(key=lambda x: (x[1], x[0]))
    
    return [x[0] for x in solver_times]

if __name__ == "__main__":
    solvers = get_sorted_solvers()
    print("\n".join(solvers))
