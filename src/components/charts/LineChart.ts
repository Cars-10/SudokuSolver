/**
 * Line Chart - Time series or connected scatter plot
 */
import { BaseChart } from './BaseChart';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';
import * as d3 from 'd3';

export class LineChart extends BaseChart {
  constructor(container: HTMLElement, config: ChartConfig = {}) {
    super(container, config);
  }

  render(data: ChartDataPoint[]): void {
    this.clear();
    this.data = data;

    // Sort by score for a meaningful line
    const sortedData = [...data]
      .filter(d => d.totalTime > 0 && d.maxMem > 0)
      .sort((a, b) => (a.score || 0) - (b.score || 0));

    if (sortedData.length === 0) {
      this.renderEmptyState('No data available');
      return;
    }

    const { innerWidth: width, innerHeight: height, margin } = this.dimensions;

    const svg = d3.select(this.container)
      .append('svg')
      .attr('width', this.dimensions.width)
      .attr('height', this.dimensions.height)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Calculate ranges for scale selection
    const times = sortedData.map(d => d.totalTime).filter(t => t > 0);
    const mems = sortedData.map(d => d.maxMem).filter(m => m > 0);

    const minTime = d3.min(times) || 0.001;
    const maxTime = d3.max(times) || 1;
    const minMem = d3.min(mems) || 0.1;
    const maxMem = d3.max(mems) || 1;

    // Use log scale if data spans more than 2 orders of magnitude
    const timeRange = maxTime / minTime;
    const memRange = maxMem / minMem;

    const useLogX = timeRange > 100;
    const useLogY = memRange > 100;

    // X scale (time)
    const x = useLogX
      ? d3.scaleLog()
          .domain([Math.max(0.001, minTime * 0.5), maxTime * 1.5])
          .range([0, width])
          .clamp(true)
      : d3.scaleLinear()
          .domain([0, maxTime * 1.1])
          .range([0, width]);

    // Y scale (memory)
    const y = useLogY
      ? d3.scaleLog()
          .domain([Math.max(0.1, minMem * 0.5), maxMem * 1.5])
          .range([height, 0])
          .clamp(true)
      : d3.scaleLinear()
          .domain([0, maxMem * 1.1])
          .range([height, 0]);

    // Create tooltip
    const tooltip = this.createTooltip();

    // Draw points
    svg.selectAll('circle')
      .data(sortedData)
      .enter()
      .append('circle')
      .attr('cx', d => x(d.totalTime))
      .attr('cy', d => y(d.maxMem))
      .attr('r', 5)
      .attr('fill', d => this.getColorByScore(d.score || 0))
      .attr('stroke', '#1a1b26')
      .attr('stroke-width', 2)
      .style('cursor', 'pointer')
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget).attr('r', 8);
        this.showTooltip(tooltip, `
          <strong>${d.language || d.solver}</strong><br/>
          Time: ${this.formatTime(d.totalTime)}<br/>
          Memory: ${this.formatMemory(d.maxMem)}<br/>
          Score: ${(d.score || 0).toFixed(2)}x
        `, event);
      })
      .on('mouseout', (event) => {
        d3.select(event.currentTarget).attr('r', 5);
        this.hideTooltip(tooltip);
      })
      .on('click', (event, d) => {
        if ((window as any).showLangModal) {
          (window as any).showLangModal(d.language || d.solver);
        }
      });

    // Draw connecting line (optional - connects points in order)
    const line = d3.line<ChartDataPoint>()
      .x(d => x(d.totalTime))
      .y(d => y(d.maxMem))
      .curve(d3.curveMonotoneX);

    svg.append('path')
      .datum(sortedData)
      .attr('fill', 'none')
      .attr('stroke', '#414868')
      .attr('stroke-width', 1)
      .attr('stroke-dasharray', '4,4')
      .attr('d', line);

    // Add labels for top performers
    const topPerformers = sortedData.slice(0, 5);
    svg.selectAll('.label')
      .data(topPerformers)
      .enter()
      .append('text')
      .attr('class', 'label')
      .attr('x', d => x(d.totalTime) + 8)
      .attr('y', d => y(d.maxMem) + 4)
      .attr('fill', '#a9b1d6')
      .attr('font-size', '10px')
      .text(d => d.language || d.solver);

    // X axis
    const xAxis = svg.append('g')
      .attr('transform', `translate(0,${height})`)
      .call(d3.axisBottom(x).ticks(8).tickFormat(d => this.formatTime(d as number)));

    xAxis.selectAll('text').attr('fill', '#565f89');
    xAxis.select('.domain').attr('stroke', '#414868');

    // Y axis
    const yAxis = svg.append('g')
      .call(d3.axisLeft(y).ticks(6).tickFormat(d => this.formatMemory(d as number)));

    yAxis.selectAll('text').attr('fill', '#565f89');
    yAxis.select('.domain').attr('stroke', '#414868');

    // Labels
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', height + 40)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Total Execution Time');

    svg.append('text')
      .attr('transform', 'rotate(-90)')
      .attr('x', -height / 2)
      .attr('y', -50)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Peak Memory Usage');

    // Title
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', -5)
      .attr('text-anchor', 'middle')
      .attr('fill', '#7aa2f7')
      .attr('font-size', '14px')
      .attr('font-weight', 'bold')
      .text('Time vs Memory (sorted by score)');
  }

  private getColorByScore(score: number): string {
    if (score < 1.0) return '#00ff9d';
    if (score < 1.5) return '#00b8ff';
    if (score < 3.0) return '#ffaa00';
    return '#ff5555';
  }

  private renderEmptyState(message: string): void {
    const div = document.createElement('div');
    div.style.cssText = 'display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);';
    div.innerHTML = `<p>${message}</p>`;
    this.container.appendChild(div);
  }
}
