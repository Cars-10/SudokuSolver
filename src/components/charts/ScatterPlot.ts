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

    // Create tooltip
    const tooltip = this.createTooltip();

    // Scales
    const xScale = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.totalTime)! * 1.1])
      .range([0, innerWidth]);

    const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.maxMem)! * 1.1])
      .range([innerHeight, 0]);

    // Axes
    this.g.append('g')
      .attr('transform', `translate(0,${innerHeight})`)
      .call(d3.axisBottom(xScale).tickFormat(d => `${d}s`))
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)');

    this.g.append('g')
      .call(d3.axisLeft(yScale).tickFormat(d => `${d}MB`))
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)');

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
      .data(data)
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
          <strong>${d.solver}</strong><br/>
          Time: ${this.formatTime(d.totalTime)}<br/>
          Memory: ${this.formatMemory(d.maxMem)}<br/>
          Iterations: ${d.avgIterations.toLocaleString()}
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
