/**
 * Iterations Chart - Shows iteration counts per matrix with log scale
 * Highlights mismatches vs reference values
 */
import { BaseChart } from './BaseChart';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';
import * as d3 from 'd3';

// Reference iteration counts for BruteForce algorithm
const REFERENCE_ITERATIONS: Record<string, number> = {
  '1': 656,
  '2': 439269,
  '3': 98847,
  '4': 9085,
  '5': 445778,
  '6': 622577597
};

interface IterationPoint {
  language: string;
  matrix: string;
  iterations: number;
  time: number;
  expected: number;
  isMismatch: boolean;
  score: number;
  algorithm: string;
}

export class IterationsChart extends BaseChart {
  private selectedMatrix: string = 'all';

  constructor(container: HTMLElement, config: ChartConfig = {}) {
    super(container, {
      ...config,
      margin: { top: 40, right: 30, bottom: 60, left: 80 }
    });
  }

  render(data: ChartDataPoint[]): void {
    this.clear();
    this.data = data;

    // Flatten data to per-matrix points
    const points: IterationPoint[] = [];

    data.forEach(d => {
      const lang = d.language || d.solver;
      const algo = d.algorithm || 'BruteForce';

      d.results?.forEach(r => {
        if (r.iterations > 0) {
          const expected = REFERENCE_ITERATIONS[r.matrix] || r.iterations;
          points.push({
            language: lang,
            matrix: r.matrix,
            iterations: r.iterations,
            time: r.time,
            expected,
            isMismatch: algo === 'BruteForce' && r.iterations !== expected,
            score: d.score || 10,
            algorithm: algo
          });
        }
      });
    });

    if (points.length === 0) {
      this.renderEmptyState('No iteration data available');
      return;
    }

    // Create controls
    this.createControls(points);

    const { innerWidth: width, innerHeight: height, margin } = this.dimensions;

    const svg = d3.select(this.container)
      .append('svg')
      .attr('width', this.dimensions.width)
      .attr('height', this.dimensions.height)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Store svg reference for updates
    (this as any)._svg = svg;
    (this as any)._points = points;
    (this as any)._dimensions = { width, height };

    this.renderPoints(svg, points, width, height);
  }

  private createControls(points: IterationPoint[]): void {
    const matrices = ['all', ...new Set(points.map(p => p.matrix))].sort((a, b) => {
      if (a === 'all') return -1;
      if (b === 'all') return 1;
      return parseInt(a) - parseInt(b);
    });

    const controls = document.createElement('div');
    controls.className = 'iterations-controls';
    controls.innerHTML = `
      <div class="iterations-controls-inner">
        <label>Matrix:</label>
        <select id="matrix-filter">
          ${matrices.map(m => `<option value="${m}"${m === 'all' ? ' selected' : ''}>${m === 'all' ? 'All Matrices' : 'Matrix ' + m}</option>`).join('')}
        </select>
        <span class="iterations-legend">
          <span class="legend-item"><span class="dot match"></span> Match</span>
          <span class="legend-item"><span class="dot mismatch"></span> Mismatch</span>
          <span class="legend-item"><span class="dot dlx"></span> DLX</span>
        </span>
      </div>
      <style>
        .iterations-controls { padding: 8px 0; }
        .iterations-controls-inner { display: flex; align-items: center; gap: 12px; flex-wrap: wrap; }
        .iterations-controls label { color: #565f89; font-size: 12px; }
        .iterations-controls select {
          background: #1a1b26;
          border: 1px solid #414868;
          border-radius: 4px;
          color: #a9b1d6;
          padding: 4px 8px;
          cursor: pointer;
        }
        .iterations-legend { display: flex; gap: 16px; margin-left: auto; }
        .legend-item { display: flex; align-items: center; gap: 4px; font-size: 11px; color: #565f89; }
        .dot { width: 10px; height: 10px; border-radius: 50%; }
        .dot.match { background: #00ff9d; }
        .dot.mismatch { background: #ff5555; }
        .dot.dlx { background: #9ece6a; }
      </style>
    `;

    this.container.insertBefore(controls, this.container.firstChild);

    // Wire up filter
    const select = controls.querySelector('#matrix-filter') as HTMLSelectElement;
    select?.addEventListener('change', (e) => {
      this.selectedMatrix = (e.target as HTMLSelectElement).value;
      this.updateChart();
    });
  }

  private updateChart(): void {
    const svg = (this as any)._svg;
    const points = (this as any)._points as IterationPoint[];
    const { width, height } = (this as any)._dimensions;

    if (!svg || !points) return;

    // Clear existing elements except axes groups
    svg.selectAll('circle').remove();
    svg.selectAll('.x-axis').remove();
    svg.selectAll('.y-axis').remove();
    svg.selectAll('.ref-line').remove();
    svg.selectAll('.chart-title').remove();
    svg.selectAll('.axis-label').remove();

    this.renderPoints(svg, points, width, height);
  }

  private renderPoints(
    svg: d3.Selection<SVGGElement, unknown, null, undefined>,
    allPoints: IterationPoint[],
    width: number,
    height: number
  ): void {
    // Filter points based on selection
    const points = this.selectedMatrix === 'all'
      ? allPoints
      : allPoints.filter(p => p.matrix === this.selectedMatrix);

    if (points.length === 0) return;

    // Use log scale for iterations (handles huge range)
    const minIter = d3.min(points, d => d.iterations) || 1;
    const maxIter = d3.max(points, d => d.iterations) || 1;

    const x = d3.scaleLog()
      .domain([Math.max(1, minIter * 0.5), maxIter * 2])
      .range([0, width])
      .nice();

    // Use log scale for time too (ms range varies widely)
    const minTime = d3.min(points, d => d.time) || 0.001;
    const maxTime = d3.max(points, d => d.time) || 1;

    const y = d3.scaleLog()
      .domain([Math.max(0.001, minTime * 0.5), maxTime * 2])
      .range([height, 0])
      .nice();

    // Draw reference lines for expected iterations
    if (this.selectedMatrix !== 'all') {
      const expected = REFERENCE_ITERATIONS[this.selectedMatrix];
      if (expected && x(expected) >= 0 && x(expected) <= width) {
        svg.append('line')
          .attr('class', 'ref-line')
          .attr('x1', x(expected))
          .attr('x2', x(expected))
          .attr('y1', 0)
          .attr('y2', height)
          .attr('stroke', '#7aa2f7')
          .attr('stroke-width', 2)
          .attr('stroke-dasharray', '5,5')
          .attr('opacity', 0.5);

        svg.append('text')
          .attr('class', 'ref-line')
          .attr('x', x(expected) + 5)
          .attr('y', 15)
          .attr('fill', '#7aa2f7')
          .attr('font-size', '10px')
          .text(`Expected: ${expected.toLocaleString()}`);
      }
    }

    // Create tooltip
    const tooltip = this.createTooltip();

    // Draw points
    svg.selectAll('circle')
      .data(points)
      .enter()
      .append('circle')
      .attr('cx', d => x(d.iterations))
      .attr('cy', d => y(d.time))
      .attr('r', 0)
      .attr('fill', d => this.getPointColor(d))
      .attr('stroke', d => d.isMismatch ? '#ff5555' : '#1a1b26')
      .attr('stroke-width', d => d.isMismatch ? 2 : 1)
      .style('cursor', 'pointer')
      .style('opacity', 0.85)
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget)
          .attr('r', 10)
          .style('opacity', 1);

        const mismatchText = d.isMismatch
          ? `<br/><span style="color:#ff5555">⚠ Expected: ${d.expected.toLocaleString()}</span>`
          : '';

        this.showTooltip(tooltip, `
          <strong>${d.language}</strong> (M${d.matrix})<br/>
          Iterations: ${d.iterations.toLocaleString()}${mismatchText}<br/>
          Time: ${this.formatTime(d.time)}<br/>
          Algorithm: ${d.algorithm}
        `, event);
      })
      .on('mouseout', (event) => {
        d3.select(event.currentTarget)
          .attr('r', 6)
          .style('opacity', 0.85);
        this.hideTooltip(tooltip);
      })
      .on('click', (_, d) => {
        if ((window as any).showLanguageDetails) {
          (window as any).showLanguageDetails(d.language);
        }
      })
      .transition()
      .duration(500)
      .attr('r', 6);

    // X axis (log scale)
    const xAxis = svg.append('g')
      .attr('class', 'x-axis')
      .attr('transform', `translate(0,${height})`)
      .call(d3.axisBottom(x).ticks(6, '~s'));

    xAxis.selectAll('text')
      .attr('fill', '#565f89')
      .attr('font-size', '11px');
    xAxis.select('.domain').attr('stroke', '#414868');
    xAxis.selectAll('.tick line').attr('stroke', '#414868');

    // Y axis (log scale)
    const yAxis = svg.append('g')
      .attr('class', 'y-axis')
      .call(d3.axisLeft(y).ticks(6).tickFormat(d => this.formatTime(d as number)));

    yAxis.selectAll('text')
      .attr('fill', '#565f89')
      .attr('font-size', '11px');
    yAxis.select('.domain').attr('stroke', '#414868');
    yAxis.selectAll('.tick line').attr('stroke', '#414868');

    // Axis labels
    svg.append('text')
      .attr('class', 'axis-label')
      .attr('x', width / 2)
      .attr('y', height + 45)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Iterations (log scale)');

    svg.append('text')
      .attr('class', 'axis-label')
      .attr('transform', 'rotate(-90)')
      .attr('x', -height / 2)
      .attr('y', -55)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Execution Time (log scale)');

    // Title
    const titleText = this.selectedMatrix === 'all'
      ? 'Iterations vs Time (All Matrices)'
      : `Iterations vs Time (Matrix ${this.selectedMatrix})`;

    svg.append('text')
      .attr('class', 'chart-title')
      .attr('x', width / 2)
      .attr('y', -15)
      .attr('text-anchor', 'middle')
      .attr('fill', '#7aa2f7')
      .attr('font-size', '14px')
      .attr('font-weight', 'bold')
      .text(titleText);

    // Stats
    const matches = points.filter(p => !p.isMismatch && p.algorithm === 'BruteForce').length;
    const mismatches = points.filter(p => p.isMismatch).length;
    const dlxCount = points.filter(p => p.algorithm === 'DLX').length;

    svg.append('text')
      .attr('class', 'chart-title')
      .attr('x', width)
      .attr('y', -15)
      .attr('text-anchor', 'end')
      .attr('fill', '#565f89')
      .attr('font-size', '11px')
      .text(`${points.length} points | ${matches} match | ${mismatches} mismatch | ${dlxCount} DLX`);
  }

  private getPointColor(point: IterationPoint): string {
    if (point.algorithm === 'DLX') return '#9ece6a';  // Green for DLX
    if (point.isMismatch) return '#ff5555';  // Red for mismatches

    // Color by score for matching BruteForce
    if (point.score < 1.0) return '#00ff9d';
    if (point.score < 1.5) return '#00b8ff';
    if (point.score < 3.0) return '#ffaa00';
    return '#bb9af7';  // Purple for slow but correct
  }

  private renderEmptyState(message: string): void {
    const div = document.createElement('div');
    div.style.cssText = 'display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);';
    div.innerHTML = `<p>${message}</p>`;
    this.container.appendChild(div);
  }
}
