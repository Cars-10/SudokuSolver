import sys
import re
import subprocess
import os

REF_FILE = "reference_output.txt"
MATRIX_1_ITERS = 656

def get_reference_content():
    if not os.path.exists(REF_FILE):
        print(f"Error: {REF_FILE} not found.")
        sys.exit(1)
    with open(REF_FILE, 'r') as f:
        return f.read()

def parse_iterations(output):
    match = re.search(r'Solved in Iterations=(\d+)', output)
    if match:
        return int(match.group(1))
    return None

def verify_solver(solver_dir):
    print(f"Verifying {solver_dir}...")
    
    # Setup command
    run_script = os.path.join(solver_dir, "RunMe.sh")
    if not os.path.exists(run_script):
        print(f"  [FAIL] No RunMe.sh found in {solver_dir}")
        return False

    # We only want to run on 1.matrix for verification
    # Create a temp RunMe that only runs 1.matrix? 
    # Or just run the existing RunMe and kill it after 1.matrix?
    # Existing RunMe loops over all matrices.
    # Let's try to run the command manually if possible, or modify RunMe temporarily.
    # Actually, most RunMe.sh iterate over ../Matrices/*.matrix.
    # We can temporarily point ../Matrices to a dir with only 1.matrix.
    
    # Better approach: The user wants us to fix the code.
    # Let's assume we are running this script *after* we fixed the code.
    # But for verification, we need to run the solver.
    
    # Let's use a temporary matrices directory strategy
    matrices_link = "AI-2025/Matrices"
    matrices_backup = "AI-2025/Matrices_Full"
    matrices_single = "AI-2025/Matrices_Single"
    
    os.makedirs(matrices_single, exist_ok=True)
    subprocess.run(["cp", "Matrices/1.matrix", matrices_single], check=True)
    
    # Swap link
    if os.path.islink(matrices_link):
        os.unlink(matrices_link)
    elif os.path.isdir(matrices_link):
        os.rename(matrices_link, matrices_backup)
        
    os.symlink(f"../{matrices_single}", matrices_link)
    
    try:
        # Run the solver
        result = subprocess.run(
            ["./RunMe.sh"], 
            cwd=solver_dir, 
            capture_output=True, 
            text=True, 
            timeout=30
        )
        output = result.stdout + result.stderr
        
        # Check Iterations
        iters = parse_iterations(output)
        
        if iters == MATRIX_1_ITERS:
            print(f"  [PASS] Iterations match ({iters})")
            return True
        else:
            print(f"  [FAIL] Iterations mismatch. Expected {MATRIX_1_ITERS}, got {iters}")
            return False
            
    except subprocess.TimeoutExpired:
        print("  [FAIL] Timeout")
        return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False
    finally:
        # Restore link
        if os.path.islink(matrices_link):
            os.unlink(matrices_link)
        
        # Restore backup if it was a dir, or link back to ../Matrices
        # We know originally it points to ../Matrices
        os.symlink("../Matrices", matrices_link)
        
        # Cleanup
        import shutil
        shutil.rmtree(matrices_single)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 verify_standard.py <AI-2025/Language>")
        sys.exit(1)
        
    solver_dir = sys.argv[1]
    verify_solver(solver_dir)
