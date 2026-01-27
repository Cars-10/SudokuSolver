/**
 * Bar Chart
 *
 * Visualizes performance rankings as horizontal bars.
 * Migrated from d3-chart.js renderMatrixRace()
 */

import * as d3 from 'd3';
import { BaseChart } from './BaseChart';
import { componentRegistry } from '../../core/ComponentRegistry';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';

export class BarChart extends BaseChart {
  constructor(containerElement: HTMLElement, config?: ChartConfig) {
    super(containerElement, {
      ...config,
      margin: config?.margin || { top: 20, right: 30, bottom: 40, left: 150 }
    });

    componentRegistry.register({
      id: 'CHART-BAR',
      fullName: 'Bar Chart',
      type: 'chart',
      path: '/src/components/charts/BarChart.ts'
    });
  }

  render(data: ChartDataPoint[]): void {
    this.data = data;
    this.createSVG();

    if (!this.g) return;

    const { innerWidth, innerHeight } = this.dimensions;

    // Filter out invalid data and sort
    const validData = data.filter(d => d.totalTime > 0 && !isNaN(d.totalTime));

    if (validData.length === 0) {
      this.g.append('text')
        .attr('x', innerWidth / 2)
        .attr('y', innerHeight / 2)
        .attr('text-anchor', 'middle')
        .style('fill', '#565f89')
        .text('No valid data to display');
      return;
    }

    // Sort and limit to top 20
    const sortedData = [...validData]
      .sort((a, b) => a.totalTime - b.totalTime)
      .slice(0, 20);

    console.log('[BarChart] Rendering', sortedData.length, 'bars');

    // Create tooltip
    const tooltip = this.createTooltip();

    // Scales
    const yScale = d3.scaleBand()
      .domain(sortedData.map(d => d.solver))
      .range([0, innerHeight])
      .padding(0.2);

    const maxTime = d3.max(sortedData, d => d.totalTime) || 1;
    const xScale = d3.scaleLinear()
      .domain([0, maxTime * 1.15]) // 15% padding
      .range([0, innerWidth])
      .nice();

    // Axes
    this.g.append('g')
      .call(d3.axisLeft(yScale))
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)')
      .style('font-size', '10px');

    this.g.append('g')
      .attr('transform', `translate(0,${innerHeight})`)
      .call(d3.axisBottom(xScale).ticks(8).tickFormat(d => this.formatTimeAxis(d as number)))
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)');

    // Bars with transition
    this.g.selectAll('.bar')
      .data(sortedData)
      .enter()
      .append('rect')
      .attr('class', 'bar')
      .attr('y', d => yScale(d.solver)!)
      .attr('height', yScale.bandwidth())
      .attr('x', 0)
      .attr('width', 0) // Start at 0 for animation
      .style('fill', d => this.getColor(d))
      .style('opacity', 0.8)
      .style('cursor', 'pointer')
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget)
          .style('opacity', 1)
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
          .style('opacity', 0.8)
          .style('fill', this.getColor(d));

        this.hideTooltip(tooltip);
      })
      .on('click', (event, d) => {
        if ((window as any).showLangModal) {
          (window as any).showLangModal(d.language || d.solver);
        }
      })
      .transition()
      .duration(800)
      .attr('width', d => xScale(d.totalTime));

    // Value labels
    this.g.selectAll('.bar-label')
      .data(sortedData)
      .enter()
      .append('text')
      .attr('class', 'bar-label')
      .attr('x', d => xScale(d.totalTime) + 5)
      .attr('y', d => yScale(d.solver)! + yScale.bandwidth() / 2)
      .attr('dy', '0.35em')
      .text(d => this.formatTime(d.totalTime))
      .style('font-size', '10px')
      .style('fill', '#a9b1d6')
      .style('font-family', 'var(--font-family-mono)')
      .style('opacity', 0)
      .transition()
      .delay(800)
      .duration(400)
      .style('opacity', 1);
  }
}
