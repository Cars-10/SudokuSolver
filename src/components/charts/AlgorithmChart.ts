/**
 * Algorithm Comparison Chart - Compare performance by algorithm type
 */
import { BaseChart } from './BaseChart';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';
import * as d3 from 'd3';

interface AlgorithmStats {
  algorithm: string;
  count: number;
  avgTime: number;
  avgScore: number;
  minTime: number;
  maxTime: number;
}

export class AlgorithmChart extends BaseChart {
  constructor(container: HTMLElement, config: ChartConfig = {}) {
    super(container, config);
  }

  render(data: ChartDataPoint[]): void {
    this.clear();
    this.data = data;

    // Group by algorithm
    const grouped = d3.group(data, d => d.algorithm || 'BruteForce');

    const stats: AlgorithmStats[] = Array.from(grouped, ([algorithm, values]) => ({
      algorithm,
      count: values.length,
      avgTime: d3.mean(values, d => d.totalTime) || 0,
      avgScore: d3.mean(values, d => d.score || 0) || 0,
      minTime: d3.min(values, d => d.totalTime) || 0,
      maxTime: d3.max(values, d => d.totalTime) || 0
    })).sort((a, b) => a.avgTime - b.avgTime);

    if (stats.length === 0) {
      this.renderEmptyState('No algorithm data available');
      return;
    }

    const { innerWidth: width, innerHeight: height, margin } = this.dimensions;

    const svg = d3.select(this.container)
      .append('svg')
      .attr('width', this.dimensions.width)
      .attr('height', this.dimensions.height)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // X scale (algorithms)
    const x = d3.scaleBand<string>()
      .domain(stats.map(d => d.algorithm))
      .range([0, width])
      .padding(0.3);

    // Y scale (time)
    const y = d3.scaleLinear()
      .domain([0, d3.max(stats, d => d.maxTime) || 1])
      .range([height, 0]);

    // Color scale
    const color = d3.scaleOrdinal<string>()
      .domain(['BruteForce', 'DLX', 'CP'])
      .range(['#7aa2f7', '#9ece6a', '#e0af68']);

    // Draw bars with error bars
    const barGroups = svg.selectAll('.bar-group')
      .data(stats)
      .enter()
      .append('g')
      .attr('class', 'bar-group');

    // Main bars (average time)
    barGroups.append('rect')
      .attr('x', d => x(d.algorithm) || 0)
      .attr('y', height)
      .attr('width', x.bandwidth())
      .attr('height', 0)
      .attr('fill', d => color(d.algorithm))
      .attr('rx', 4)
      .transition()
      .duration(800)
      .attr('y', d => y(d.avgTime))
      .attr('height', d => height - y(d.avgTime));

    // Error bars (min-max range)
    barGroups.append('line')
      .attr('x1', d => (x(d.algorithm) || 0) + x.bandwidth() / 2)
      .attr('x2', d => (x(d.algorithm) || 0) + x.bandwidth() / 2)
      .attr('y1', d => y(d.minTime))
      .attr('y2', d => y(d.maxTime))
      .attr('stroke', '#c0caf5')
      .attr('stroke-width', 2)
      .attr('opacity', 0)
      .transition()
      .delay(800)
      .duration(300)
      .attr('opacity', 0.7);

    // Min/max caps
    barGroups.selectAll('.cap')
      .data(d => [{ y: d.minTime, x: d.algorithm }, { y: d.maxTime, x: d.algorithm }])
      .enter()
      .append('line')
      .attr('class', 'cap')
      .attr('x1', d => (x(d.x) || 0) + x.bandwidth() / 2 - 8)
      .attr('x2', d => (x(d.x) || 0) + x.bandwidth() / 2 + 8)
      .attr('y1', d => y(d.y))
      .attr('y2', d => y(d.y))
      .attr('stroke', '#c0caf5')
      .attr('stroke-width', 2)
      .attr('opacity', 0)
      .transition()
      .delay(800)
      .duration(300)
      .attr('opacity', 0.7);

    // Count labels
    barGroups.append('text')
      .attr('x', d => (x(d.algorithm) || 0) + x.bandwidth() / 2)
      .attr('y', d => y(d.avgTime) - 8)
      .attr('text-anchor', 'middle')
      .attr('fill', '#c0caf5')
      .attr('font-size', '11px')
      .text(d => `n=${d.count}`);

    // X axis
    const xAxis = svg.append('g')
      .attr('transform', `translate(0,${height})`)
      .call(d3.axisBottom(x));

    xAxis.selectAll('text')
      .attr('fill', '#c0caf5')
      .attr('font-size', '12px');
    xAxis.select('.domain').attr('stroke', '#414868');

    // Y axis
    const yAxis = svg.append('g')
      .call(d3.axisLeft(y).ticks(6).tickFormat(d => this.formatTime(d as number)));

    yAxis.selectAll('text').attr('fill', '#565f89');
    yAxis.select('.domain').attr('stroke', '#414868');

    // Labels
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', height + 40)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Algorithm Type');

    svg.append('text')
      .attr('transform', 'rotate(-90)')
      .attr('x', -height / 2)
      .attr('y', -50)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Average Execution Time');

    // Title
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', -5)
      .attr('text-anchor', 'middle')
      .attr('fill', '#7aa2f7')
      .attr('font-size', '14px')
      .attr('font-weight', 'bold')
      .text('Algorithm Performance Comparison');

    // Legend
    const legend = svg.append('g')
      .attr('transform', `translate(${width - 120}, 10)`);

    legend.append('text')
      .attr('x', 0)
      .attr('y', 0)
      .attr('fill', '#565f89')
      .attr('font-size', '10px')
      .text('Error bars show min-max range');
  }

  private renderEmptyState(message: string): void {
    const div = document.createElement('div');
    div.style.cssText = 'display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);';
    div.innerHTML = `<p>${message}</p>`;
    this.container.appendChild(div);
  }
}
