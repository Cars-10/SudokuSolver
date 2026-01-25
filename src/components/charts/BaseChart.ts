/**
 * Abstract Base Chart Class
 *
 * Provides common functionality for all D3-based charts including:
 * - SVG creation and sizing
 * - Margin management
 * - Zoom/pan interactions
 * - Tooltip handling
 * - Export functionality
 */

import * as d3 from 'd3';
import { componentRegistry } from '../../core/ComponentRegistry';
import type { ChartDataPoint, ChartConfig, ChartDimensions } from '../../types/charts';

export abstract class BaseChart {
  protected container: HTMLElement;
  protected svg: d3.Selection<SVGSVGElement, unknown, null, undefined> | null = null;
  protected g: d3.Selection<SVGGElement, unknown, null, undefined> | null = null;
  protected dimensions: ChartDimensions;
  protected config: ChartConfig;
  protected data: ChartDataPoint[] = [];

  constructor(
    containerElement: HTMLElement,
    config: ChartConfig = {}
  ) {
    this.container = containerElement;

    // Default configuration
    this.config = {
      width: config.width || 1200,
      height: config.height || 600,
      margin: config.margin || { top: 20, right: 120, bottom: 60, left: 60 },
      showLogos: config.showLogos ?? true,
      showLabels: config.showLabels ?? true,
      interactive: config.interactive ?? true
    };

    // Calculate dimensions
    this.dimensions = this.calculateDimensions();
  }

  /**
   * Calculate chart dimensions based on container and margins
   */
  protected calculateDimensions(): ChartDimensions {
    const width = this.container.clientWidth || this.config.width!;
    const height = this.container.clientHeight || this.config.height!;
    const margin = this.config.margin!;

    return {
      width,
      height,
      margin,
      innerWidth: width - margin.left - margin.right,
      innerHeight: height - margin.top - margin.bottom
    };
  }

  /**
   * Create SVG element with proper dimensions
   */
  protected createSVG(): void {
    this.clear();

    const { width, height, margin } = this.dimensions;

    this.svg = d3.select(this.container)
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .attr('data-component-id', 'CHART-SVG');

    this.g = this.svg
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`)
      .attr('data-component-id', 'CHART-GROUP');
  }

  /**
   * Add zoom and pan behavior to chart
   */
  protected addZoomBehavior(): void {
    if (!this.svg || !this.g) return;

    const zoom = d3.zoom<SVGSVGElement, unknown>()
      .scaleExtent([0.5, 10])
      .on('zoom', (event) => {
        this.g!.attr('transform', event.transform);
      });

    this.svg.call(zoom);
  }

  /**
   * Create tooltip element
   */
  protected createTooltip(): d3.Selection<HTMLDivElement, unknown, null, undefined> {
    return d3.select(this.container)
      .append('div')
      .attr('class', 'chart-tooltip')
      .attr('data-component-id', 'CHART-TOOLTIP')
      .style('opacity', 0)
      .style('position', 'absolute')
      .style('pointer-events', 'none');
  }

  /**
   * Show tooltip
   */
  protected showTooltip(
    tooltip: d3.Selection<HTMLDivElement, unknown, null, undefined>,
    content: string,
    event: MouseEvent
  ): void {
    tooltip
      .html(content)
      .style('opacity', 1)
      .style('left', `${event.pageX + 10}px`)
      .style('top', `${event.pageY - 10}px`);
  }

  /**
   * Hide tooltip
   */
  protected hideTooltip(
    tooltip: d3.Selection<HTMLDivElement, unknown, null, undefined>
  ): void {
    tooltip.style('opacity', 0);
  }

  /**
   * Get color for data point based on algorithm or performance
   */
  protected getColor(d: ChartDataPoint): string {
    if (d.color) return d.color;

    // Color by algorithm type
    const algorithm = d.algorithm || d.solver.split('/')[1];
    const colors: Record<string, string> = {
      'BruteForce': '#7aa2f7',
      'DLX': '#9ece6a',
      'CP': '#e0af68'
    };

    return colors[algorithm] || '#565f89';
  }

  /**
   * Format time for display
   */
  protected formatTime(seconds: number): string {
    if (seconds < 1) {
      return `${(seconds * 1000).toFixed(0)}ms`;
    }
    return `${seconds.toFixed(3)}s`;
  }

  /**
   * Format memory for display
   */
  protected formatMemory(mb: number): string {
    if (mb < 1) {
      return `${(mb * 1024).toFixed(0)}KB`;
    }
    return `${mb.toFixed(2)}MB`;
  }

  /**
   * Clear chart
   */
  clear(): void {
    d3.select(this.container).selectAll('*').remove();
    this.svg = null;
    this.g = null;
  }

  /**
   * Render chart - must be implemented by subclasses
   */
  abstract render(data: ChartDataPoint[]): void;

  /**
   * Update chart with new data
   */
  update(data: ChartDataPoint[]): void {
    this.data = data;
    this.render(data);
  }

  /**
   * Resize chart
   */
  resize(): void {
    this.dimensions = this.calculateDimensions();
    this.render(this.data);
  }

  /**
   * Export chart as PNG
   */
  async exportPNG(filename: string = 'chart.png'): Promise<void> {
    if (!this.svg) return;

    const svgNode = this.svg.node();
    if (!svgNode) return;

    // Get SVG data
    const svgData = new XMLSerializer().serializeToString(svgNode);
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    canvas.width = this.dimensions.width;
    canvas.height = this.dimensions.height;

    const img = new Image();
    const svgBlob = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
    const url = URL.createObjectURL(svgBlob);

    img.onload = () => {
      ctx.drawImage(img, 0, 0);
      URL.revokeObjectURL(url);

      canvas.toBlob((blob) => {
        if (blob) {
          const link = document.createElement('a');
          link.download = filename;
          link.href = URL.createObjectURL(blob);
          link.click();
        }
      });
    };

    img.src = url;
  }

  /**
   * Cleanup
   */
  destroy(): void {
    this.clear();
  }
}
