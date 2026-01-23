
import { state } from './globals.js';

// --- D3 Chart Logic ---
// This logic was originally inline in HTMLGenerator.ts/report_client.js and is now modularized.

// Chart State
window.currentChart = 'line';
window.showLogos = true;

// Define default chart size
const defaultWidth = 1200;
const defaultHeight = 600;

export function initializeChart() {
    console.log('[D3 Chart] Initializing...');

    // Check if D3 is loaded
    if (typeof d3 === 'undefined') {
        console.warn('[D3 Chart] D3.js not loaded yet. Retrying in 100ms...');
        setTimeout(initializeChart, 100);
        return;
    }

    // Set initial mode from persistence if available
    try {
        const storedMode = localStorage.getItem('benchmarkUIState');
        if (storedMode) {
            const parsed = JSON.parse(storedMode);
            if (parsed.chartMode) window.currentChart = parsed.chartMode;
        }
    } catch (e) { }

    // Initialize chart
    switchChart(window.currentChart);

    // Add resize listener
    window.addEventListener('resize', () => { // Debounced
        if (window.resizeTimer) clearTimeout(window.resizeTimer);
        window.resizeTimer = setTimeout(() => switchChart(window.currentChart), 250);
    });
}

// Switch Chart Mode
export function switchChart(mode) {
    console.log('Switching chart to:', mode);
    window.currentChart = mode;

    // Update active button state
    const select = document.getElementById('chart-selector');
    if (select) select.value = mode;

    // Clear Logic
    d3.select("#d3-chart").selectAll("*").remove();
    d3.select(".chart-controls .active").classed("active", false);

    // Render based on mode
    if (mode === 'line') renderScatterPlot();
    else if (mode === 'bar') renderBarChart(); // If implemented
    else if (mode === 'race') renderMatrixRace();
    else if (mode === 'jockey') renderHorseRace();
    else if (mode === 'algorithm') renderAlgorithmComparison();
    else if (mode === 'language') renderLanguageComparison(); // Top Languages
    else if (mode === 'iterations') renderIterationComparison();
    else renderScatterPlot(); // Default
}

// 1. Scatter Plot (Metrics)
function renderScatterPlot() {
    const margin = { top: 20, right: 120, bottom: 60, left: 60 };
    const container = document.getElementById('chart-wrapper');
    const width = container.clientWidth - margin.left - margin.right;
    const height = container.clientHeight - margin.top - margin.bottom;

    const svg = d3.select("#d3-chart")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

    // Get Data (Filtered)
    const data = getFilteredChartData();

    // Scales
    const x = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.totalTime) * 1.1])
        .range([0, width]);

    const y = d3.scaleLinear() // Log scale often better for memory? Using Linear for now as per original
        .domain([0, d3.max(data, d => d.maxMem) * 1.1])
        .range([height, 0]);

    // Axes
    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x).tickFormat(d => d + "s"))
        .attr("color", "#565f89")
        .style("font-family", "JetBrains Mono");

    svg.append("g")
        .call(d3.axisLeft(y).tickFormat(d => d + "MB"))
        .attr("color", "#565f89")
        .style("font-family", "JetBrains Mono");

    // Labels
    svg.append("text")
        .attr("x", width / 2)
        .attr("y", height + 40)
        .style("text-anchor", "middle")
        .style("fill", "#565f89")
        .text("Total Execution Time (Seconds)");

    svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", -45)
        .attr("x", -height / 2)
        .style("text-anchor", "middle")
        .style("fill", "#565f89")
        .text("Peak Memory (MB)");

    // Dots
    const dots = svg.selectAll("dot")
        .data(data)
        .enter()
        .append("g")
        .attr("class", "node")
        .attr("transform", d => `translate(${x(d.totalTime)},${y(d.maxMem)})`)
        // Hover effects
        .on("mouseover", function (event, d) {
            d3.select(this).raise().select("circle").transition().duration(200).attr("r", 8).style("fill", "#fff");
            d3.select(this).select("text").transition().duration(200).style("opacity", 1).style("font-size", "14px").style("fill", "#fff");
        })
        .on("mouseout", function (event, d) {
            d3.select(this).select("circle").transition().duration(200).attr("r", 5).style("fill", getColor(d));
            if (!window.showLogos) {
                d3.select(this).select("text").transition().duration(200).style("opacity", 0);
            } else {
                d3.select(this).select("text").transition().duration(200).style("opacity", 1).style("font-size", "10px").style("fill", "#a9b1d6");
            }
        })
        .on("click", function (event, d) {
            if (window.openLanguageModal) window.openLanguageModal(d.solver);
        });

    dots.append("circle")
        .attr("r", 5)
        .style("fill", d => getColor(d))
        .style("stroke", "none")
        .style("opacity", 0.8);

    // Labels / Logos
    if (window.showLogos) {
        dots.append("image")
            .attr("xlink:href", d => d.logo)
            .attr("x", 8)
            .attr("y", -8)
            .attr("width", 16)
            .attr("height", 16)
            .style("opacity", 0.8);

        // Text fallback or name if no logo? Just name
        dots.append("text")
            .attr("x", 28)
            .attr("y", 4)
            .text(d => d.solver)
            .style("font-size", "10px")
            .style("fill", "#a9b1d6")
            .style("font-family", "JetBrains Mono");
    }
}

// 2. Matrix Race (Bar Chart of Matrix Times)
function renderMatrixRace() {
    const margin = { top: 20, right: 30, bottom: 40, left: 150 };
    const container = document.getElementById('chart-wrapper');
    const width = container.clientWidth - margin.left - margin.right;
    const height = container.clientHeight - margin.top - margin.bottom;

    const svg = d3.select("#d3-chart")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

    const data = getFilteredChartData().slice(0, 20); // Top 20

    const y = d3.scaleBand()
        .domain(data.map(d => d.solver))
        .range([0, height])
        .padding(0.2);

    const x = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.totalTime)])
        .range([0, width]);

    // Bars with Transition
    svg.selectAll("rect")
        .data(data)
        .enter()
        .append("rect")
        .attr("y", d => y(d.solver))
        .attr("height", y.bandwidth())
        .attr("x", 0)
        .attr("width", 0) // Start at 0 width
        .style("fill", d => getColor(d))
        .style("opacity", 0.8)
        .transition() // Animate
        .duration(1500)
        .ease(d3.easeCubicOut)
        .attr("width", d => x(d.totalTime));

    // Axis
    svg.append("g")
        .call(d3.axisLeft(y))
        .attr("color", "#a9b1d6")
        .style("font-family", "JetBrains Mono");

    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x))
        .attr("color", "#565f89");
}

// 3. Horse Race (Jockey) - Simplified Animation Visual
function renderHorseRace() {
    // Placeholder for complex animation - simplified static view for now
    renderMatrixRace();
}

// 4. Algorithm Comparison
function renderAlgorithmComparison() {
    // Group by Algorithm
    const data = getFilteredChartData();
    const groups = d3.group(data, d => d.algorithmType || 'BruteForce');

    const margin = { top: 20, right: 30, bottom: 40, left: 60 };
    const container = document.getElementById('chart-wrapper');
    const width = container.clientWidth - margin.left - margin.right;
    const height = container.clientHeight - margin.top - margin.bottom;

    const svg = d3.select("#d3-chart")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

    // Box plot or Violin plot logic would go here.
    // For now, implementing simple bar chart of Averages

    const avgData = Array.from(groups, ([key, values]) => ({
        key,
        value: d3.mean(values, d => d.totalTime)
    }));

    const x = d3.scaleBand()
        .domain(avgData.map(d => d.key))
        .range([0, width])
        .padding(0.4);

    const y = d3.scaleLinear()
        .domain([0, d3.max(avgData, d => d.value)])
        .range([height, 0]);

    svg.selectAll("rect")
        .data(avgData)
        .enter()
        .append("rect")
        .attr("x", d => x(d.key))
        .attr("y", d => y(d.value))
        .attr("width", x.bandwidth())
        .attr("height", d => height - y(d.value))
        .style("fill", "#7aa2f7");

    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x))
        .attr("color", "#a9b1d6");

    svg.append("g")
        .call(d3.axisLeft(y))
        .attr("color", "#565f89");

    svg.append("text")
        .attr("x", width / 2)
        .attr("y", -5)
        .style("text-anchor", "middle")
        .style("fill", "#a9b1d6")
        .text("Average Total Time (Lower is Better)");
}

// 5. Language Comparison (Top Languages)
function renderLanguageComparison() {
    // Top 10 fastest languages overall
    const data = getFilteredChartData()
        .sort((a, b) => a.totalTime - b.totalTime)
        .slice(0, 15);

    // Reuse Matrix Race logic
    const margin = { top: 20, right: 30, bottom: 40, left: 120 };
    const container = document.getElementById('chart-wrapper');
    const width = container.clientWidth - margin.left - margin.right;
    const height = container.clientHeight - margin.top - margin.bottom;

    const svg = d3.select("#d3-chart")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

    const y = d3.scaleBand()
        .domain(data.map(d => d.solver))
        .range([0, height])
        .padding(0.2);

    const x = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.totalTime)])
        .range([0, width]);

    svg.selectAll("rect")
        .data(data)
        .enter()
        .append("rect")
        .attr("y", d => y(d.solver))
        .attr("height", y.bandwidth())
        .attr("x", 0)
        .attr("width", d => x(d.totalTime))
        .style("fill", d => getColor(d));

    svg.append("g")
        .call(d3.axisLeft(y))
        .attr("color", "#a9b1d6")
        .style("font-family", "JetBrains Mono");

    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x));
}

// 6. Iteration Comparison
function renderIterationComparison() {
    const margin = { top: 20, right: 30, bottom: 60, left: 80 };
    const container = document.getElementById('chart-wrapper');
    const width = container.clientWidth - margin.left - margin.right;
    const height = container.clientHeight - margin.top - margin.bottom;

    const svg = d3.select("#d3-chart")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

    const data = getFilteredChartData();

    const x = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.totalIters)])
        .range([0, width]);

    const y = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.totalTime)])
        .range([height, 0]);

    svg.selectAll("circle")
        .data(data)
        .enter()
        .append("circle")
        .attr("cx", d => x(d.totalIters))
        .attr("cy", d => y(d.totalTime))
        .attr("r", 5)
        .style("fill", d => getColor(d))
        .style("opacity", 0.7);

    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(x))
        .attr("color", "#565f89");

    svg.append("g")
        .call(d3.axisLeft(y))
        .attr("color", "#565f89");

    svg.append("text")
        .attr("x", width / 2)
        .attr("y", height + 40)
        .style("text-anchor", "middle")
        .style("fill", "#a9b1d6")
        .text("Total Iterations");

    svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", -60)
        .attr("x", -height / 2)
        .style("text-anchor", "middle")
        .style("fill", "#a9b1d6")
        .text("Total Time (s)");
}


// --- Helper Functions ---

function getFilteredChartData() {
    if (!window.metricsData) return [];

    // Process Data
    let data = window.metricsData.map(m => {
        const times = m.results.map(r => r.time);
        const mems = m.results.map(r => r.memory || 0);
        const iters = m.results.map(r => r.iterations || 0);

        return {
            solver: m.solver,
            algorithmType: m.algorithmType,
            totalTime: times.reduce((a, b) => a + b, 0),
            maxMem: Math.max(...mems) / 1024 / 1024, // MB
            totalIters: iters.reduce((a, b) => a + b, 0),
            logo: m.logo,
            score: m.score,
            runType: m.runType
        };
    });

    // Apply Filter
    if (state.currentAlgorithm && state.currentAlgorithm !== 'all') {
        data = data.filter(d => (d.algorithmType || 'BruteForce') === state.currentAlgorithm);
    }

    // Remove outliers (> 60s) if scatter plot to keep scale sane? Optional.
    // data = data.filter(d => d.totalTime < 60); 

    return data;
}

function getColor(d) {
    if (d.score < 1.0) return "#00ff9d"; // Fast
    if (d.score < 2.0) return "#00b8ff"; // Okay
    return "#ff0055"; // Slow
}

// Global exposure for button handlers
window.toggleLogoMode = function (btn) {
    window.showLogos = !window.showLogos;
    switchChart(window.currentChart);

    // Identify icon type to toggle (simple implementation)
    // In real app, toggle SVG contents or classes
    btn.classList.toggle('active', !window.showLogos);
}

window.handleZoomExtend = function () {
    // Reset zoom (simple re-render)
    switchChart(window.currentChart);
}

window.toggleChartFullscreen = function () {
    const wrapper = document.getElementById('chart-wrapper');
    if (!document.fullscreenElement) {
        wrapper.requestFullscreen().catch(err => {
            console.warn(`Error attempting to enable full-screen mode: ${err.message}`);
        });
    } else {
        document.exitFullscreen();
    }
}

window.switchChart = switchChart;

