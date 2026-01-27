/**
 * Histogram Chart - Score Distribution
 */
import { BaseChart } from './BaseChart';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';
import * as d3 from 'd3';

export class HistogramChart extends BaseChart {
  constructor(container: HTMLElement, config: ChartConfig = {}) {
    super(container, config);
  }

  render(data: ChartDataPoint[]): void {
    this.clear();
    this.data = data;

    const scores = data.map(d => d.score || 0).filter(s => s > 0 && s < 100);

    if (scores.length === 0) {
      this.renderEmptyState('No score data available');
      return;
    }

    const { innerWidth: width, innerHeight: height, margin } = this.dimensions;

    const svg = d3.select(this.container)
      .append('svg')
      .attr('width', this.dimensions.width)
      .attr('height', this.dimensions.height)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Create histogram bins
    const x = d3.scaleLinear()
      .domain([0, Math.min(d3.max(scores) || 10, 20)])
      .range([0, width]);

    const histogram = d3.bin()
      .domain(x.domain() as [number, number])
      .thresholds(x.ticks(20));

    const bins = histogram(scores);

    const y = d3.scaleLinear()
      .domain([0, d3.max(bins, d => d.length) || 1])
      .range([height, 0]);

    // Draw bars
    svg.selectAll('rect')
      .data(bins)
      .enter()
      .append('rect')
      .attr('x', d => x(d.x0 || 0) + 1)
      .attr('y', height)
      .attr('width', d => Math.max(0, x(d.x1 || 0) - x(d.x0 || 0) - 2))
      .attr('height', 0)
      .attr('fill', d => this.getBinColor((d.x0 || 0 + (d.x1 || 0)) / 2))
      .attr('rx', 2)
      .transition()
      .duration(800)
      .attr('y', d => y(d.length))
      .attr('height', d => height - y(d.length));

    // X axis
    const xAxis = svg.append('g')
      .attr('transform', `translate(0,${height})`)
      .call(d3.axisBottom(x).ticks(10).tickFormat(d => d + 'x'));

    xAxis.selectAll('text').attr('fill', '#565f89');
    xAxis.select('.domain').attr('stroke', '#414868');

    // Y axis
    const yAxis = svg.append('g')
      .call(d3.axisLeft(y).ticks(5));

    yAxis.selectAll('text').attr('fill', '#565f89');
    yAxis.select('.domain').attr('stroke', '#414868');

    // Labels
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', height + 35)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Score (times slower than C baseline)');

    svg.append('text')
      .attr('transform', 'rotate(-90)')
      .attr('x', -height / 2)
      .attr('y', -40)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text('Number of Languages');

    // Title
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', -5)
      .attr('text-anchor', 'middle')
      .attr('fill', '#7aa2f7')
      .attr('font-size', '14px')
      .attr('font-weight', 'bold')
      .text('Score Distribution');

    // Add tier markers
    this.addTierMarkers(svg, x, height);
  }

  private addTierMarkers(svg: d3.Selection<SVGGElement, unknown, null, undefined>, x: d3.ScaleLinear<number, number>, height: number): void {
    const tiers = [
      { score: 0.95, label: 'S', color: '#00ff9d' },
      { score: 1.05, label: 'A', color: '#00b8ff' },
      { score: 1.5, label: 'B', color: '#7aa2f7' },
      { score: 3, label: 'C', color: '#ffaa00' },
      { score: 10, label: 'D', color: '#ff5555' }
    ];

    tiers.forEach(tier => {
      if (x(tier.score) <= x.range()[1]) {
        svg.append('line')
          .attr('x1', x(tier.score))
          .attr('x2', x(tier.score))
          .attr('y1', 0)
          .attr('y2', height)
          .attr('stroke', tier.color)
          .attr('stroke-dasharray', '4,4')
          .attr('opacity', 0.5);

        svg.append('text')
          .attr('x', x(tier.score))
          .attr('y', -8)
          .attr('text-anchor', 'middle')
          .attr('fill', tier.color)
          .attr('font-size', '10px')
          .attr('font-weight', 'bold')
          .text(tier.label);
      }
    });
  }

  private getBinColor(score: number): string {
    if (score < 1) return '#00ff9d';
    if (score < 1.5) return '#00b8ff';
    if (score < 3) return '#7aa2f7';
    if (score < 10) return '#ffaa00';
    return '#ff5555';
  }

  private renderEmptyState(message: string): void {
    const div = document.createElement('div');
    div.style.cssText = 'display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);';
    div.innerHTML = `<p>${message}</p>`;
    this.container.appendChild(div);
  }
}
