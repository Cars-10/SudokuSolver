/**
 * Jockey Chart - Animated horse race visualization
 */
import { BaseChart } from './BaseChart';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';
import * as d3 from 'd3';

export class JockeyChart extends BaseChart {
  private animationTimer: number | null = null;

  constructor(container: HTMLElement, config: ChartConfig = {}) {
    super(container, {
      ...config,
      margin: { top: 40, right: 100, bottom: 40, left: 120 }
    });
  }

  render(data: ChartDataPoint[]): void {
    this.clear();
    this.data = data;

    // Sort by score (best performers first) and take top 15
    const sortedData = [...data]
      .filter(d => d.totalTime > 0)
      .sort((a, b) => (a.score || 999) - (b.score || 999))
      .slice(0, 15);

    if (sortedData.length === 0) {
      this.renderEmptyState('No data available for Horse Race');
      return;
    }

    const { innerWidth: width, innerHeight: height, margin } = this.dimensions;

    const svg = d3.select(this.container)
      .append('svg')
      .attr('width', this.dimensions.width)
      .attr('height', this.dimensions.height)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    const getName = (d: ChartDataPoint) => d.language || d.solver;

    // Y scale (lanes)
    const y = d3.scaleBand<string>()
      .domain(sortedData.map(d => getName(d)))
      .range([0, height])
      .padding(0.2);

    // X scale (progress - normalized by fastest time)
    const minTime = d3.min(sortedData, d => d.totalTime) || 1;
    const maxTime = d3.max(sortedData, d => d.totalTime) || 1;
    const x = d3.scaleLinear()
      .domain([0, 1])
      .range([0, width]);

    // Draw track lanes
    svg.selectAll('.lane')
      .data(sortedData)
      .enter()
      .append('rect')
      .attr('class', 'lane')
      .attr('x', 0)
      .attr('y', d => y(getName(d)) || 0)
      .attr('width', width)
      .attr('height', y.bandwidth())
      .attr('fill', (_, i) => i % 2 === 0 ? '#1a1b26' : '#16161e')
      .attr('stroke', '#2a2a35')
      .attr('stroke-width', 1);

    // Draw finish line
    svg.append('line')
      .attr('x1', width)
      .attr('x2', width)
      .attr('y1', 0)
      .attr('y2', height)
      .attr('stroke', '#ff5555')
      .attr('stroke-width', 3)
      .attr('stroke-dasharray', '8,4');

    svg.append('text')
      .attr('x', width + 5)
      .attr('y', -10)
      .attr('fill', '#ff5555')
      .attr('font-size', '12px')
      .attr('font-weight', 'bold')
      .text('FINISH');

    // Create horse markers
    const horses = svg.selectAll('.horse')
      .data(sortedData)
      .enter()
      .append('g')
      .attr('class', 'horse')
      .attr('transform', d => `translate(0, ${(y(getName(d)) || 0) + y.bandwidth() / 2})`)
      .style('cursor', 'pointer')
      .on('click', (event, d) => {
        if ((window as any).showLangModal) {
          (window as any).showLangModal(getName(d));
        }
      });

    // Horse body (circle)
    horses.append('circle')
      .attr('r', Math.min(y.bandwidth() / 3, 12))
      .attr('fill', d => this.getColorByScore(d.score || 0))
      .attr('stroke', '#fff')
      .attr('stroke-width', 2);

    // Horse number/rank
    horses.append('text')
      .attr('text-anchor', 'middle')
      .attr('dy', '0.35em')
      .attr('fill', '#1a1b26')
      .attr('font-size', '10px')
      .attr('font-weight', 'bold')
      .text((_, i) => i + 1);

    // Y axis (language names)
    const yAxis = svg.append('g')
      .attr('class', 'y-axis')
      .call(d3.axisLeft(y).tickSize(0));

    yAxis.selectAll('text')
      .attr('fill', '#c0caf5')
      .attr('font-size', '11px');
    yAxis.select('.domain').remove();

    // Title
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', -20)
      .attr('text-anchor', 'middle')
      .attr('fill', '#7aa2f7')
      .attr('font-size', '14px')
      .attr('font-weight', 'bold')
      .text('🏇 Performance Race');

    // Animate the race
    this.animateRace(horses, sortedData, x, minTime);
  }

  private animateRace(
    horses: d3.Selection<SVGGElement, ChartDataPoint, SVGGElement, unknown>,
    data: ChartDataPoint[],
    x: d3.ScaleLinear<number, number>,
    maxTime: number
  ): void {
    const duration = 3000; // 3 second race
    const getName = (d: ChartDataPoint) => d.language || d.solver;

    horses.transition()
      .duration(duration)
      .ease(d3.easeCubicOut)
      .attrTween('transform', (d) => {
        const yPos = this.getYPosition(d, data);
        const startX = 0;
        const endX = x(maxTime / d.totalTime); // Faster = further right

        return (t: number) => {
          const currentX = startX + (endX - startX) * t;
          return `translate(${currentX}, ${yPos})`;
        };
      });

    // Add time labels after race finishes
    setTimeout(() => {
      horses.append('text')
        .attr('x', 20)
        .attr('dy', '0.35em')
        .attr('fill', '#a9b1d6')
        .attr('font-size', '10px')
        .style('opacity', 0)
        .text(d => this.formatTime(d.totalTime))
        .transition()
        .duration(300)
        .style('opacity', 1);
    }, duration);
  }

  private getYPosition(d: ChartDataPoint, data: ChartDataPoint[]): number {
    const getName = (d: ChartDataPoint) => d.language || d.solver;
    const index = data.findIndex(item => getName(item) === getName(d));
    const bandHeight = this.dimensions.innerHeight / data.length;
    return index * bandHeight + bandHeight / 2 + bandHeight * 0.1;
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

  destroy(): void {
    if (this.animationTimer) {
      cancelAnimationFrame(this.animationTimer);
    }
    super.destroy();
  }
}
