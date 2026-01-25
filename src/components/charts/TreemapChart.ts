/**
 * Treemap Chart
 *
 * Visualizes hierarchical performance data (Algorithm > Language).
 * New chart type for v2.0
 */

import * as d3 from 'd3';
import { BaseChart } from './BaseChart';
import { componentRegistry } from '../../core/ComponentRegistry';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';

interface TreeNode {
  name: string;
  value?: number;
  children?: TreeNode[];
  data?: ChartDataPoint;
}

export class TreemapChart extends BaseChart {
  constructor(containerElement: HTMLElement, config?: ChartConfig) {
    super(containerElement, config);

    componentRegistry.register({
      id: 'CHART-TREEMAP',
      fullName: 'Treemap Chart',
      type: 'chart',
      path: '/src/components/charts/TreemapChart.ts'
    });
  }

  render(data: ChartDataPoint[]): void {
    this.data = data;
    this.createSVG();

    if (!this.g) return;

    const { innerWidth, innerHeight } = this.dimensions;

    // Create tooltip
    const tooltip = this.createTooltip();

    // Group data by algorithm
    const grouped = d3.group(data, d => d.algorithm || 'Unknown');

    // Build hierarchy
    const root: TreeNode = {
      name: 'root',
      children: Array.from(grouped, ([algorithm, items]) => ({
        name: algorithm,
        children: items.map(item => ({
          name: item.solver,
          value: item.totalTime,
          data: item
        }))
      }))
    };

    // Create hierarchy and treemap
    const hierarchy = d3.hierarchy(root)
      .sum(d => d.value || 0)
      .sort((a, b) => (b.value || 0) - (a.value || 0));

    const treemap = d3.treemap<TreeNode>()
      .size([innerWidth, innerHeight])
      .padding(2)
      .round(true);

    treemap(hierarchy as d3.HierarchyNode<TreeNode>);

    // Color scale by algorithm
    const colorScale = d3.scaleOrdinal<string>()
      .domain(['BruteForce', 'DLX', 'CP', 'Unknown'])
      .range(['#7aa2f7', '#9ece6a', '#e0af68', '#565f89']);

    // Render cells
    const cells = this.g.selectAll('.cell')
      .data(hierarchy.leaves() as d3.HierarchyRectangularNode<TreeNode>[])
      .enter()
      .append('g')
      .attr('class', 'cell')
      .attr('transform', d => `translate(${d.x0},${d.y0})`);

    cells.append('rect')
      .attr('width', d => d.x1 - d.x0)
      .attr('height', d => d.y1 - d.y0)
      .style('fill', d => {
        const algorithm = d.parent?.data.name || 'Unknown';
        return colorScale(algorithm);
      })
      .style('stroke', '#1a1b26')
      .style('stroke-width', 1)
      .style('opacity', 0.8)
      .style('cursor', 'pointer')
      .on('mouseover', (event, d) => {
        d3.select(event.currentTarget).style('opacity', 1);

        const data = d.data.data;
        if (data) {
          const content = `
            <strong>${data.solver}</strong><br/>
            Algorithm: ${data.algorithm || 'Unknown'}<br/>
            Time: ${this.formatTime(data.totalTime)}<br/>
            Memory: ${this.formatMemory(data.maxMem)}
          `;
          this.showTooltip(tooltip, content, event);
        }
      })
      .on('mouseout', (event) => {
        d3.select(event.currentTarget).style('opacity', 0.8);
        this.hideTooltip(tooltip);
      })
      .on('click', (event, d) => {
        const data = d.data.data;
        if (data && (window as any).showLangModal) {
          (window as any).showLangModal(data.language || data.solver);
        }
      });

    // Add labels (only if cell is large enough)
    cells.append('text')
      .attr('x', 4)
      .attr('y', 14)
      .text(d => {
        const width = d.x1 - d.x0;
        const height = d.y1 - d.y0;
        // Only show label if cell is large enough
        return (width > 50 && height > 20) ? d.data.name : '';
      })
      .style('font-size', '10px')
      .style('fill', '#fff')
      .style('font-family', 'var(--font-family-mono)')
      .style('pointer-events', 'none');

    // Add value labels for larger cells
    cells.append('text')
      .attr('x', 4)
      .attr('y', 28)
      .text(d => {
        const width = d.x1 - d.x0;
        const height = d.y1 - d.y0;
        const value = d.data.value;
        return (width > 50 && height > 35 && value) ? this.formatTime(value) : '';
      })
      .style('font-size', '9px')
      .style('fill', '#a9b1d6')
      .style('font-family', 'var(--font-family-mono)')
      .style('pointer-events', 'none');
  }
}
