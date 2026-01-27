/**
 * Heatmap Chart
 *
 * Visualizes performance across languages and matrices as a color-coded grid.
 * New chart type for v2.0
 */

import * as d3 from 'd3';
import { BaseChart } from './BaseChart';
import { componentRegistry } from '../../core/ComponentRegistry';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';

export class HeatmapChart extends BaseChart {
  constructor(containerElement: HTMLElement, config?: ChartConfig) {
    super(containerElement, config);

    componentRegistry.register({
      id: 'CHART-HEATMAP',
      fullName: 'Heatmap Chart',
      type: 'chart',
      path: '/src/components/charts/HeatmapChart.ts'
    });
  }

  render(data: ChartDataPoint[]): void {
    this.data = data;
    this.createSVG();

    if (!this.g) return;

    const { innerWidth, innerHeight } = this.dimensions;

    // Group data by language (limit to top 20)
    const topLanguages = data
      .sort((a, b) => a.totalTime - b.totalTime)
      .slice(0, 20)
      .map(d => d.language || d.solver);

    // Create mock matrix data (in real implementation, would use actual matrix results)
    const matrices = ['Matrix 1', 'Matrix 2', 'Matrix 3', 'Matrix 4', 'Matrix 5'];

    // Create tooltip
    const tooltip = this.createTooltip();

    // Scales
    const xScale = d3.scaleBand()
      .domain(matrices)
      .range([0, innerWidth])
      .padding(0.05);

    const yScale = d3.scaleBand()
      .domain(topLanguages)
      .range([0, innerHeight])
      .padding(0.05);

    // Color scale (green = fast, red = slow)
    const colorScale = d3.scaleSequential(d3.interpolateRdYlGn)
      .domain([d3.max(data, d => d.totalTime)!, 0]);

    // Axes
    this.g.append('g')
      .attr('transform', `translate(0,${innerHeight})`)
      .call(d3.axisBottom(xScale))
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)')
      .style('font-size', '10px');

    this.g.append('g')
      .call(d3.axisLeft(yScale))
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)')
      .style('font-size', '10px');

    // Create heatmap cells
    const cellData: Array<{ language: string; matrix: string; time: number }> = [];
    topLanguages.forEach(lang => {
      matrices.forEach(matrix => {
        const d = data.find(item => (item.language || item.solver) === lang);
        cellData.push({
          language: lang,
          matrix,
          time: d?.totalTime || 0
        });
      });
    });

    this.g.selectAll('.cell')
      .data(cellData)
      .enter()
      .append('rect')
      .attr('class', 'cell')
      .attr('x', d => xScale(d.matrix)!)
      .attr('y', d => yScale(d.language)!)
      .attr('width', xScale.bandwidth())
      .attr('height', yScale.bandwidth())
      .style('fill', d => colorScale(d.time))
      .style('stroke', '#1a1b26')
      .style('stroke-width', 1)
      .style('cursor', 'pointer')
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget).style('opacity', 0.7);

        const content = `
          <strong>${d.language}</strong><br/>
          ${d.matrix}<br/>
          Time: ${this.formatTime(d.time)}
        `;
        this.showTooltip(tooltip, content, event);
      })
      .on('mouseout', (event) => {
        d3.select(event.currentTarget).style('opacity', 1);
        this.hideTooltip(tooltip);
      })
      .on('click', (event, d) => {
        if ((window as any).showLangModal) {
          (window as any).showLangModal(d.language);
        }
      });

    // Add legend
    const legendWidth = 20;
    const legendHeight = innerHeight;

    const legendScale = d3.scaleLinear()
      .domain(colorScale.domain())
      .range([legendHeight, 0]);

    const legendAxis = d3.axisRight(legendScale)
      .tickFormat(d => this.formatTime(Number(d)));

    // Legend gradient
    const defs = this.svg!.append('defs');
    const gradient = defs.append('linearGradient')
      .attr('id', 'heatmap-gradient')
      .attr('x1', '0%')
      .attr('y1', '100%')
      .attr('x2', '0%')
      .attr('y2', '0%');

    gradient.selectAll('stop')
      .data(d3.range(0, 1.1, 0.1))
      .enter()
      .append('stop')
      .attr('offset', d => `${d * 100}%`)
      .attr('stop-color', d => colorScale(d3.max(data, d => d.totalTime)! * (1 - d)));

    this.g.append('rect')
      .attr('x', innerWidth + 10)
      .attr('y', 0)
      .attr('width', legendWidth)
      .attr('height', legendHeight)
      .style('fill', 'url(#heatmap-gradient)');

    this.g.append('g')
      .attr('transform', `translate(${innerWidth + 10 + legendWidth}, 0)`)
      .call(legendAxis)
      .attr('color', '#565f89')
      .style('font-family', 'var(--font-family-mono)')
      .style('font-size', '9px');
  }
}
