/**
 * Scatter Plot Chart
 *
 * Visualizes performance metrics (time vs memory) as a scatter plot.
 * Migrated from d3-chart.js renderScatterPlot()
 */

import * as d3 from 'd3';
import { BaseChart } from './BaseChart';
import { componentRegistry } from '../../core/ComponentRegistry';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';

export class ScatterPlot extends BaseChart {
  constructor(containerElement: HTMLElement, config?: ChartConfig) {
    super(containerElement, config);

    componentRegistry.register({
      id: 'CHART-SCATTER',
      fullName: 'Scatter Plot Chart',
      type: 'chart',
      path: '/src/components/charts/ScatterPlot.ts'
    });
  }

  render(data: ChartDataPoint[]): void {
    this.data = data;
    this.createSVG();

    if (!this.g) return;

    const { innerWidth, innerHeight } = this.dimensions;

    // Filter valid data points
    const validData = data.filter(d => d.totalTime > 0 && d.maxMem > 0);

    if (validData.length === 0) {
      this.g.append('text')
        .attr('x', innerWidth / 2)
        .attr('y', innerHeight / 2)
        .attr('text-anchor', 'middle')
        .style('fill', '#565f89')
        .text('No valid data to display (need time and memory data)');
      return;
    }

    // Create tooltip
    const tooltip = this.createTooltip();

    // Calculate data ranges
    const times = validData.map(d => d.totalTime).filter(t => t > 0);
    const mems = validData.map(d => d.maxMem).filter(m => m > 0);

    const minTime = d3.min(times) || 0.001;
    const maxTime = d3.max(times) || 1;
    const minMem = d3.min(mems) || 0.1;
    const maxMem = d3.max(mems) || 1;

    // Use log scale if data spans more than 2 orders of magnitude
    const timeRange = maxTime / minTime;
    const memRange = maxMem / minMem;

    const useLogX = timeRange > 100;
    const useLogY = memRange > 100;

    const xScale = useLogX
      ? d3.scaleLog()
          .domain([Math.max(0.001, minTime * 0.5), maxTime * 1.5])
          .range([0, innerWidth])
          .clamp(true)
      : d3.scaleLinear()
          .domain([0, maxTime * 1.1])
          .range([0, innerWidth]);

    const yScale = useLogY
      ? d3.scaleLog()
          .domain([Math.max(0.1, minMem * 0.5), maxMem * 1.5])
          .range([innerHeight, 0])
          .clamp(true)
      : d3.scaleLinear()
          .domain([0, maxMem * 1.1])
          .range([innerHeight, 0]);

    // Axes - use minimal ticks to avoid crowding
    const xAxis = d3.axisBottom(xScale)
      .ticks(3)
      .tickFormat((d) => this.formatTimeAxis(d as number));

    const yAxis = d3.axisLeft(yScale)
      .ticks(3)
      .tickFormat((d) => this.formatMemory(d as number));

    this.g.append('g')
      .attr('transform', `translate(0,${innerHeight})`)
      .call(xAxis)
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)')
      .style('font-size', '10px');

    this.g.append('g')
      .call(yAxis)
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)')
      .style('font-size', '10px');

    // Axis labels
    this.g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', innerHeight + 40)
      .style('text-anchor', 'middle')
      .style('fill', '#565f89')
      .text('Total Execution Time (Seconds)');

    this.g.append('text')
      .attr('transform', 'rotate(-90)')
      .attr('y', -45)
      .attr('x', -innerHeight / 2)
      .style('text-anchor', 'middle')
      .style('fill', '#565f89')
      .text('Peak Memory (MB)');

    // Data points
    const nodes = this.g.selectAll('.node')
      .data(validData)
      .enter()
      .append('g')
      .attr('class', 'node')
      .attr('transform', d => `translate(${xScale(d.totalTime)},${yScale(d.maxMem)})`)
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget)
          .raise()
          .select('circle')
          .transition()
          .duration(200)
          .attr('r', 8)
          .style('fill', '#fff');

        const content = `
          <strong>${d.language || d.solver}</strong><br/>
          Algorithm: ${d.algorithm || 'BruteForce'}<br/>
          Time: ${this.formatTime(d.totalTime)}<br/>
          Memory: ${this.formatMemory(d.maxMem)}<br/>
          Iterations: ${d.avgIterations.toLocaleString()}<br/>
          Score: ${(d.score || 0).toFixed(2)}x<br/>
          Tier: ${d.tier || 'N/A'}
        `;
        this.showTooltip(tooltip, content, event);
      })
      .on('mouseout', (event, d) => {
        d3.select(event.currentTarget)
          .select('circle')
          .transition()
          .duration(200)
          .attr('r', 5)
          .style('fill', this.getColor(d));

        this.hideTooltip(tooltip);
      })
      .on('click', (event, d) => {
        // Trigger language modal
        if ((window as any).showLangModal) {
          (window as any).showLangModal(d.language || d.solver);
        }
      });

    // Circles
    nodes.append('circle')
      .attr('r', 5)
      .style('fill', d => this.getColor(d))
      .style('stroke', 'none')
      .style('opacity', 0.8)
      .style('cursor', 'pointer');

    // Labels (if enabled)
    if (this.config.showLabels) {
      nodes.append('text')
        .attr('x', 8)
        .attr('y', 4)
        .text(d => d.solver)
        .style('font-size', '10px')
        .style('fill', '#a9b1d6')
        .style('font-family', 'var(--font-family-mono)');
    }

    // Add zoom/pan
    if (this.config.interactive) {
      this.addZoomBehavior();
    }
  }
}
