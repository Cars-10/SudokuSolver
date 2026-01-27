/**
 * Chart Container Component
 *
 * Manages chart selection and rendering
 */

import { metricsService } from '../services/MetricsService';
import { BarChart } from './charts/BarChart';
import { ScatterPlot } from './charts/ScatterPlot';
import { HeatmapChart } from './charts/HeatmapChart';
import { RaceChart } from './charts/RaceChart';
import { HistogramChart } from './charts/HistogramChart';
import { IterationsChart } from './charts/IterationsChart';
import { AlgorithmChart } from './charts/AlgorithmChart';
import { JockeyChart } from './charts/JockeyChart';
import { LineChart } from './charts/LineChart';
import { BaseChart } from './charts/BaseChart';
import type { ChartDataPoint } from '../types/charts';

export type ChartType = 'bar' | 'scatter' | 'heatmap' | 'histogram' | 'line' | 'race' | 'jockey' | 'iterations' | 'algorithm';

export class ChartContainer {
  private container: HTMLElement | null = null;
  private currentChart: BaseChart | null = null;
  private currentType: ChartType = 'bar';
  private chartElement: HTMLElement | null = null;
  private showLogos: boolean = true;

  render(containerId: string): void {
    this.container = document.getElementById(containerId);
    if (!this.container) {
      console.error(`[ChartContainer] Container #${containerId} not found`);
      return;
    }

    // The container is now the chart-area div inside the chart-wrapper
    // The wrapper and controls are rendered in main.ts
    this.chartElement = this.container;
    this.renderChart('bar');
  }

  renderChart(type: ChartType): void {
    if (!this.chartElement) return;

    this.currentType = type;

    // Clear existing chart
    if (this.currentChart) {
      this.currentChart.destroy();
      this.currentChart = null;
    }

    // Clear container for placeholder charts
    this.chartElement.innerHTML = '';

    // Get metrics data and transform to chart format
    const metrics = metricsService.getAll();
    console.log('[ChartContainer] Raw metrics:', metrics.length, 'items');

    const chartData = this.transformMetricsToChartData(metrics);
    console.log('[ChartContainer] Chart data:', chartData.length, 'items');

    // Validate data
    if (chartData.length === 0) {
      this.chartElement.innerHTML = `
        <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; color: var(--muted);">
          <p>No chart data available</p>
          <p style="font-size: 0.8em;">Make sure the server is running and metrics are loaded</p>
        </div>
      `;
      return;
    }

    // Get dimensions from container
    const width = this.chartElement.clientWidth || 1200;
    const height = this.chartElement.clientHeight || 450;
    console.log('[ChartContainer] Rendering', type, 'chart at', width, 'x', height);

    // Common config
    const config = { width, height, showLogos: this.showLogos };

    // Create chart based on type
    switch (type) {
      case 'bar':
        this.currentChart = new BarChart(this.chartElement, config);
        break;
      case 'scatter':
        this.currentChart = new ScatterPlot(this.chartElement, config);
        break;
      case 'heatmap':
        this.currentChart = new HeatmapChart(this.chartElement, config);
        break;
      case 'race':
        this.currentChart = new RaceChart(this.chartElement, config);
        break;
      case 'histogram':
        this.currentChart = new HistogramChart(this.chartElement, config);
        break;
      case 'line':
        this.currentChart = new LineChart(this.chartElement, config);
        break;
      case 'jockey':
        this.currentChart = new JockeyChart(this.chartElement, config);
        break;
      case 'iterations':
        this.currentChart = new IterationsChart(this.chartElement, config);
        break;
      case 'algorithm':
        this.currentChart = new AlgorithmChart(this.chartElement, config);
        break;
      default:
        console.warn(`[ChartContainer] Unknown chart type: ${type}`);
        this.renderPlaceholder(type, height);
        return;
    }

    this.currentChart.render(chartData);
  }

  private renderPlaceholder(type: string, height: number): void {
    if (!this.chartElement) return;

    this.chartElement.innerHTML = `
      <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: ${height}px; color: var(--muted);">
        <svg width="64" height="64" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1" style="opacity: 0.5; margin-bottom: 16px;">
          <rect x="3" y="3" width="18" height="18" rx="2" ry="2"/>
          <line x1="3" y1="9" x2="21" y2="9"/>
          <line x1="9" y1="21" x2="9" y2="9"/>
        </svg>
        <h3 style="color: var(--primary); margin-bottom: 8px;">${type}</h3>
        <p style="font-size: 0.9em;">Chart type not available</p>
      </div>
    `;
  }

  private transformMetricsToChartData(metrics: any[]): ChartDataPoint[] {
    return metrics
      .filter(m => m && (m.language || m.solver)) // Filter out invalid entries
      .map(m => {
        // Determine if time is in ms or seconds based on magnitude
        // If totalTime > 100, assume it's in ms; otherwise assume seconds
        const totalTimeMs = m.totalTime || 0;
        const totalTimeSec = totalTimeMs > 100 ? totalTimeMs / 1000 : totalTimeMs;

        return {
          solver: m.language || m.solver,
          language: m.language || m.solver,
          algorithm: m.algorithm || 'BruteForce',
          totalTime: totalTimeSec,
          maxMem: (m.maxMemory || m.avgMemory || 0) / (1024 * 1024), // Convert to MB
          avgIterations: m.avgIterations || m.totalIterations || 0,
          score: m.score || 0,
          tier: m.tier || 'F',
          logo: m.logo,
          results: m.results || []
        };
      })
      .filter(d => d.totalTime > 0 || d.maxMem > 0); // Filter out entries with no data
  }

  resize(): void {
    if (this.currentChart && this.chartElement) {
      const width = this.chartElement.clientWidth || 1200;
      const height = this.chartElement.clientHeight || 450;
      this.renderChart(this.currentType);
    }
  }

  destroy(): void {
    if (this.currentChart) {
      this.currentChart.destroy();
      this.currentChart = null;
    }
  }

  toggleLogos(): boolean {
    this.showLogos = !this.showLogos;
    // Re-render current chart with new logo setting
    if (this.currentType && this.chartElement) {
      this.renderChart(this.currentType);
    }
    return this.showLogos;
  }

  getShowLogos(): boolean {
    return this.showLogos;
  }
}

export const chartContainer = new ChartContainer();
