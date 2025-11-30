import os
import re
import glob

def parse_run_file(filepath):
    with open(filepath, 'r', errors='ignore') as f:
        content = f.read()
    
    iterations = re.findall(r'Solved in Iterations=(\d+)', content)
    times = re.findall(r'Seconds to process ([\d.]+)', content)
    cpu_user = re.findall(r'([\d.]+)\s+user', content)
    cpu_sys = re.findall(r'([\d.]+)\s+sys', content)
    max_rss = re.findall(r'(\d+)\s+maximum resident set size', content)
    
    return iterations, times, cpu_user, cpu_sys, max_rss

def get_results():
    results = {}
    base_dir = "AI-2025"
    for entry in os.listdir(base_dir):
        dir_path = os.path.join(base_dir, entry)
        if os.path.isdir(dir_path):
            run_file = os.path.join(dir_path, "run.txt")
            if os.path.exists(run_file):
                iterations, times, cpu_user, cpu_sys, max_rss = parse_run_file(run_file)
                if times:
                    results[entry] = {
                        'iters': iterations,
                        'times': times,
                        'cpu_user': cpu_user,
                        'cpu_sys': cpu_sys,
                        'max_rss': max_rss
                    }
    return results

def generate_neon(results):
    # Determine max matrices
    max_matrices = 0
    for lang, data in results.items():
        max_matrices = max(max_matrices, len(data['times']))

    sorted_results = sorted(results.items(), key=lambda x: sum(map(float, x[1]['times'])) if x[1]['times'] else float('inf'))

    html = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Sudoku Benchmark - Neon</title>
    <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap" rel="stylesheet">
    <style>
        :root {
            --bg: #0d0d12;
            --surface: #16161e;
            --primary: #00ff9d;
            --secondary: #00b8ff;
            --text: #e0e0e0;
            --muted: #5c5c66;
            --border: #2a2a35;
        }
        body {
            font-family: 'JetBrains Mono', monospace;
            background-color: var(--bg);
            color: var(--text);
            margin: 0;
            padding: 40px;
        }
        h1 {
            text-align: center;
            color: var(--primary);
            text-transform: uppercase;
            letter-spacing: 2px;
            margin-bottom: 40px;
            text-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            overflow-x: auto;
        }
        table {
            width: 100%;
            border-collapse: separate;
            border-spacing: 0 4px;
        }
        th {
            text-align: left;
            padding: 15px;
            color: var(--secondary);
            font-size: 0.9em;
            text-transform: uppercase;
            border-bottom: 2px solid var(--border);
        }
        td {
            background-color: var(--surface);
            padding: 15px;
            border-top: 1px solid var(--border);
            border-bottom: 1px solid var(--border);
            transition: transform 0.2s, box-shadow 0.2s;
        }
        tr:hover td {
            background-color: #1c1c26;
            border-color: var(--secondary);
            transform: scale(1.01);
            box-shadow: 0 0 15px rgba(0, 184, 255, 0.1);
            z-index: 10;
            position: relative;
        }
        .lang-col {
            font-weight: bold;
            color: var(--primary);
            border-left: 3px solid transparent;
        }
        tr:hover .lang-col {
            border-left-color: var(--primary);
        }
        .cell-content {
            display: flex;
            flex-direction: column;
            gap: 4px;
        }
        .time {
            font-size: 1.1em;
            font-weight: bold;
            color: #fff;
        }
        .meta {
            font-size: 0.75em;
            color: var(--muted);
            display: flex;
            gap: 10px;
        }
        .meta span {
            display: inline-block;
        }
        .total-time {
            font-size: 1.2em;
            color: var(--secondary);
            font-weight: bold;
        }
        .badge {
            background: rgba(0, 255, 157, 0.1);
            color: var(--primary);
            padding: 2px 6px;
            border-radius: 4px;
            font-size: 0.7em;
        }
    </style>
</head>
<body>
    <h1>System Status: Benchmark Complete</h1>
    <div class="container">
        <table>
            <thead>
                <tr>
                    <th>Language</th>
"""
    for i in range(max_matrices):
        html += f"<th>Matrix {i+1}</th>"
    html += "<th>Total</th></tr></thead><tbody>"

    for lang, data in sorted_results:
        times = data['times']
        iters = data['iters']
        total_time = sum(map(float, times)) if times else 0
        
        html += f"<tr><td class='lang-col'>{lang}</td>"
        for i in range(max_matrices):
            if i < len(times):
                t = float(times[i])
                it = iters[i] if i < len(iters) else "?"
                c_user = float(data['cpu_user'][i]) if i < len(data['cpu_user']) else 0
                c_sys = float(data['cpu_sys'][i]) if i < len(data['cpu_sys']) else 0
                mem = int(data['max_rss'][i]) if i < len(data['max_rss']) else 0
                mem_mb = mem / 1024 / 1024
                
                html += f"""<td>
                    <div class="cell-content">
                        <div class="time">{t:.3f}s</div>
                        <div class="meta">
                            <span title="Iterations">#{it}</span>
                            <span title="CPU">{c_user+c_sys:.2f}s</span>
                            <span title="Memory">{mem_mb:.1f}M</span>
                        </div>
                    </div>
                </td>"""
            else:
                html += "<td><span style='color: #333'>-</span></td>"
        html += f"<td class='total-time'>{total_time:.2f}s</td></tr>"

    html += "</tbody></table></div></body></html>"
    return html

def generate_cards(results):
    max_matrices = 0
    for lang, data in results.items():
        max_matrices = max(max_matrices, len(data['times']))
    sorted_results = sorted(results.items(), key=lambda x: sum(map(float, x[1]['times'])) if x[1]['times'] else float('inf'))

    html = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Sudoku Benchmark - Cards</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600;800&display=swap" rel="stylesheet">
    <style>
        :root {
            --bg: #f3f4f6;
            --card-bg: #ffffff;
            --text: #1f2937;
            --text-light: #6b7280;
            --accent: #6366f1;
            --success: #10b981;
        }
        body {
            font-family: 'Inter', sans-serif;
            background-color: var(--bg);
            color: var(--text);
            padding: 40px;
        }
        h1 {
            text-align: center;
            font-weight: 800;
            color: #111827;
            margin-bottom: 40px;
        }
        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
            gap: 20px;
            max-width: 1600px;
            margin: 0 auto;
        }
        .card {
            background: var(--card-bg);
            border-radius: 12px;
            padding: 20px;
            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            transition: transform 0.2s;
            border: 1px solid #e5e7eb;
        }
        .card:hover {
            transform: translateY(-4px);
            box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
        }
        .header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 15px;
            border-bottom: 1px solid #f3f4f6;
            padding-bottom: 10px;
        }
        .lang-name {
            font-size: 1.25em;
            font-weight: 800;
            color: var(--accent);
        }
        .total-badge {
            background: #eef2ff;
            color: var(--accent);
            padding: 4px 8px;
            border-radius: 999px;
            font-weight: 600;
            font-size: 0.9em;
        }
        .matrix-row {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 8px;
            font-size: 0.9em;
        }
        .matrix-label {
            color: var(--text-light);
            width: 60px;
        }
        .matrix-time {
            font-weight: 600;
            width: 60px;
            text-align: right;
        }
        .matrix-meta {
            font-size: 0.75em;
            color: #9ca3af;
            text-align: right;
            flex-grow: 1;
            padding-right: 10px;
        }
    </style>
</head>
<body>
    <h1>Benchmark Results</h1>
    <div class="grid">
"""
    for lang, data in sorted_results:
        times = data['times']
        iters = data['iters']
        total_time = sum(map(float, times)) if times else 0
        
        html += f"""
        <div class="card">
            <div class="header">
                <span class="lang-name">{lang}</span>
                <span class="total-badge">{total_time:.2f}s</span>
            </div>
        """
        
        for i in range(max_matrices):
            if i < len(times):
                t = float(times[i])
                it = iters[i] if i < len(iters) else "?"
                mem = int(data['max_rss'][i]) if i < len(data['max_rss']) else 0
                mem_mb = mem / 1024 / 1024
                
                html += f"""
                <div class="matrix-row">
                    <span class="matrix-label">M{i+1}</span>
                    <span class="matrix-meta">{it} iter • {mem_mb:.0f}MB</span>
                    <span class="matrix-time">{t:.3f}s</span>
                </div>
                """
        html += "</div>"
    
    html += "</div></body></html>"
    return html

def generate_minimal(results):
    max_matrices = 0
    for lang, data in results.items():
        max_matrices = max(max_matrices, len(data['times']))
    sorted_results = sorted(results.items(), key=lambda x: sum(map(float, x[1]['times'])) if x[1]['times'] else float('inf'))

    html = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Sudoku Benchmark - Minimal</title>
    <link href="https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500&display=swap" rel="stylesheet">
    <style>
        body {
            font-family: 'Roboto', sans-serif;
            background-color: #202124;
            color: #e8eaed;
            margin: 0;
            padding: 40px;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
        }
        h1 {
            font-weight: 300;
            letter-spacing: 1px;
            margin-bottom: 40px;
            border-bottom: 1px solid #3c4043;
            padding-bottom: 20px;
        }
        table {
            width: 100%;
            border-collapse: collapse;
        }
        th {
            text-align: left;
            font-weight: 500;
            color: #9aa0a6;
            padding: 12px 20px;
            border-bottom: 1px solid #3c4043;
        }
        td {
            padding: 16px 20px;
            border-bottom: 1px solid #3c4043;
            vertical-align: top;
        }
        .lang {
            font-size: 1.1em;
            font-weight: 500;
            color: #8ab4f8;
        }
        .data-cell {
            display: flex;
            flex-direction: column;
        }
        .primary {
            font-size: 1.1em;
            color: #fff;
        }
        .secondary {
            font-size: 0.8em;
            color: #9aa0a6;
            margin-top: 4px;
        }
        .total {
            font-size: 1.2em;
            font-weight: 500;
            color: #fff;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Benchmark Performance</h1>
        <table>
            <thead>
                <tr>
                    <th>Language</th>
"""
    for i in range(max_matrices):
        html += f"<th>Matrix {i+1}</th>"
    html += "<th>Total Time</th></tr></thead><tbody>"

    for lang, data in sorted_results:
        times = data['times']
        iters = data['iters']
        total_time = sum(map(float, times)) if times else 0
        
        html += f"<tr><td class='lang'>{lang}</td>"
        for i in range(max_matrices):
            if i < len(times):
                t = float(times[i])
                it = iters[i] if i < len(iters) else "?"
                mem = int(data['max_rss'][i]) if i < len(data['max_rss']) else 0
                mem_mb = mem / 1024 / 1024
                
                html += f"""<td>
                    <div class="data-cell">
                        <span class="primary">{t:.3f}s</span>
                        <span class="secondary">{it} it • {mem_mb:.0f}MB</span>
                    </div>
                </td>"""
            else:
                html += "<td>-</td>"
        html += f"<td class='total'>{total_time:.2f}s</td></tr>"

    html += "</tbody></table></div></body></html>"
    return html

def main():
    results = get_results()
    
    with open("design_neon.html", "w") as f:
        f.write(generate_neon(results))
    
    with open("design_cards.html", "w") as f:
        f.write(generate_cards(results))
        
    with open("design_minimal.html", "w") as f:
        f.write(generate_minimal(results))
        
    print("Generated design_neon.html, design_cards.html, design_minimal.html")

if __name__ == "__main__":
    main()
