// Scoring Insights Modal - Shows statistical analysis of benchmark data
import { BaseModal } from './BaseModal';
import { metricsService } from '../../services/MetricsService';
import type { SolverMetrics } from '../../types/metrics';

interface StabilityData {
  language: string;
  maxSwing: number;
}

interface OutlierData {
  language: string;
  metric: string;
  explanation: string;
}

interface InsightsData {
  correlation: {
    r2: number;
    interpretation: string;
  };
  stability: StabilityData[];
  outliers: OutlierData[];
}

export class ScoringInsightsModal extends BaseModal {
  private insightsData: InsightsData | null = null;

  constructor() {
    super('SIM', 'Scoring Insights Modal', {
      dependencies: ['MetricsService']
    });
  }

  protected getDomId(): string {
    return 'scoringInsightsModal';
  }

  open(): void {
    // Calculate insights before opening
    this.calculateInsights();
    super.open();

    // Draw stability visualization after modal is rendered
    setTimeout(() => this.drawStabilityVisualization(), 200);
  }

  private calculateInsights(): void {
    const metrics = metricsService.getAll().filter(m => !m.failed && (m.totalTime || 0) > 0);

    // Calculate Time vs Memory correlation (R²)
    const correlation = this.calculateCorrelation(metrics);

    // Calculate rank stability (simulate different weighting scenarios)
    const stability = this.calculateStability(metrics);

    // Find statistical outliers using IQR method
    const outliers = this.findOutliers(metrics);

    this.insightsData = {
      correlation,
      stability,
      outliers
    };
  }

  private calculateCorrelation(metrics: SolverMetrics[]): { r2: number; interpretation: string } {
    if (metrics.length < 3) {
      return { r2: 0, interpretation: 'Insufficient data for correlation analysis.' };
    }

    const times = metrics.map(m => m.totalTime || 0);
    const memories = metrics.map(m => m.maxMemory || m.avgMemory || 0);

    // Calculate Pearson correlation coefficient
    const n = times.length;
    const sumX = times.reduce((a, b) => a + b, 0);
    const sumY = memories.reduce((a, b) => a + b, 0);
    const sumXY = times.reduce((acc, t, i) => acc + t * memories[i], 0);
    const sumX2 = times.reduce((acc, t) => acc + t * t, 0);
    const sumY2 = memories.reduce((acc, m) => acc + m * m, 0);

    const numerator = n * sumXY - sumX * sumY;
    const denominator = Math.sqrt((n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY));

    const r = denominator !== 0 ? numerator / denominator : 0;
    const r2 = r * r;

    let interpretation: string;
    if (r2 > 0.7) {
      interpretation = 'Strong positive correlation: Languages that are fast tend to use less memory, and vice versa. This suggests optimization efforts benefit both metrics.';
    } else if (r2 > 0.4) {
      interpretation = 'Moderate correlation: There is a noticeable relationship between time and memory usage, but other factors play significant roles.';
    } else if (r2 > 0.1) {
      interpretation = 'Weak correlation: Time and memory are somewhat independent. Optimizing for one does not necessarily affect the other.';
    } else {
      interpretation = 'No significant correlation: Time and memory usage appear to be independent metrics. Languages can excel in one without affecting the other.';
    }

    return { r2, interpretation };
  }

  private calculateStability(metrics: SolverMetrics[]): StabilityData[] {
    // Simulate ranking under different weighting scenarios
    const weightings = [
      { time: 1.0, memory: 0.0 },  // Time only
      { time: 0.8, memory: 0.2 },  // Heavy time
      { time: 0.6, memory: 0.4 },  // Balanced
      { time: 0.4, memory: 0.6 },  // Heavy memory
      { time: 0.2, memory: 0.8 },  // Mostly memory
      { time: 0.0, memory: 1.0 }   // Memory only
    ];

    const rankHistory: Map<string, number[]> = new Map();

    weightings.forEach(w => {
      // Calculate weighted scores
      const scored = metrics.map(m => ({
        language: m.language || m.solver,
        score: (m.scoreBreakdown?.time || 1) * w.time + (m.scoreBreakdown?.memory || 1) * w.memory
      })).sort((a, b) => a.score - b.score);

      scored.forEach((entry, rank) => {
        const history = rankHistory.get(entry.language) || [];
        history.push(rank + 1);
        rankHistory.set(entry.language, history);
      });
    });

    // Calculate max swing for each language
    const stability: StabilityData[] = [];
    rankHistory.forEach((ranks, language) => {
      const maxSwing = Math.max(...ranks) - Math.min(...ranks);
      stability.push({ language, maxSwing });
    });

    return stability.sort((a, b) => a.maxSwing - b.maxSwing);
  }

  private findOutliers(metrics: SolverMetrics[]): OutlierData[] {
    const outliers: OutlierData[] = [];

    // Check time outliers
    const times = metrics.map(m => m.totalTime || 0).filter(t => t > 0);
    const timeOutliers = this.findIQROutliers(times);
    metrics.forEach(m => {
      const time = m.totalTime || 0;
      if (timeOutliers.upper.includes(time)) {
        outliers.push({
          language: m.language || m.solver,
          metric: 'Slow',
          explanation: `Execution time (${time.toFixed(2)}ms) is significantly higher than typical implementations.`
        });
      } else if (timeOutliers.lower.includes(time)) {
        outliers.push({
          language: m.language || m.solver,
          metric: 'Fast',
          explanation: `Execution time (${time.toFixed(2)}ms) is exceptionally low compared to other implementations.`
        });
      }
    });

    // Check memory outliers
    const memories = metrics.map(m => m.maxMemory || m.avgMemory || 0).filter(m => m > 0);
    const memOutliers = this.findIQROutliers(memories);
    metrics.forEach(m => {
      const mem = m.maxMemory || m.avgMemory || 0;
      if (memOutliers.upper.includes(mem) && !outliers.some(o => o.language === (m.language || m.solver))) {
        outliers.push({
          language: m.language || m.solver,
          metric: 'Memory Heavy',
          explanation: `Memory usage (${(mem / 1024 / 1024).toFixed(1)}MB) is significantly higher than typical implementations.`
        });
      }
    });

    return outliers.slice(0, 6); // Limit to 6 outliers for display
  }

  private findIQROutliers(values: number[]): { upper: number[]; lower: number[] } {
    if (values.length < 4) return { upper: [], lower: [] };

    const sorted = [...values].sort((a, b) => a - b);
    const q1Index = Math.floor(sorted.length * 0.25);
    const q3Index = Math.floor(sorted.length * 0.75);
    const q1 = sorted[q1Index];
    const q3 = sorted[q3Index];
    const iqr = q3 - q1;

    const lowerBound = q1 - 1.5 * iqr;
    const upperBound = q3 + 1.5 * iqr;

    return {
      upper: values.filter(v => v > upperBound),
      lower: values.filter(v => v < lowerBound)
    };
  }

  protected render(): HTMLElement {
    const modal = document.createElement('div');
    modal.id = this.getDomId();
    modal.className = 'modal insights-modal';
    modal.style.width = '900px';
    modal.style.maxHeight = '85vh';
    modal.style.overflow = 'hidden';

    const data = this.insightsData || { correlation: { r2: 0, interpretation: '' }, stability: [], outliers: [] };

    modal.innerHTML = `
      <div class="modal-header insights-hero">
        <div class="insights-hero-content">
          <div class="insights-icon">
            <svg width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
              <path d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
              <path d="M10 7v6l4 2"/>
            </svg>
          </div>
          <div>
            <h2 style="margin: 0; color: #7aa2f7;">Scoring Insights</h2>
            <p style="margin: 5px 0 0 0; color: #565f89; font-size: 0.9em;">Deep dive into performance metrics and ranking behavior</p>
          </div>
        </div>
        <button class="modal-close-btn" aria-label="Close modal">&times;</button>
      </div>

      <div class="modal-body insights-content" style="overflow-y: auto; max-height: calc(85vh - 120px); padding: 20px;">
        <!-- Correlation Section -->
        <section class="insights-section">
          <div class="section-header">
            <div class="section-icon" style="background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); width: 40px; height: 40px; border-radius: 10px; display: flex; align-items: center; justify-content: center;">
              <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2">
                <line x1="18" y1="20" x2="18" y2="10"/>
                <line x1="12" y1="20" x2="12" y2="4"/>
                <line x1="6" y1="20" x2="6" y2="14"/>
              </svg>
            </div>
            <div style="margin-left: 15px;">
              <h3 style="margin: 0; color: #c0caf5;">Time vs Memory Correlation</h3>
              <p style="margin: 5px 0 0 0; color: #565f89; font-size: 0.85em;">Statistical relationship between execution time and memory usage</p>
            </div>
          </div>
          <div class="insights-card" style="margin-top: 15px; background: rgba(26, 27, 38, 0.8); border: 1px solid #2a2a35; border-radius: 12px; padding: 20px;">
            <div class="correlation-display" style="display: flex; align-items: center; gap: 30px;">
              <div class="correlation-value" style="flex: 0 0 150px; text-align: center;">
                <div style="font-size: 0.8em; color: #565f89; margin-bottom: 5px;">R² Coefficient</div>
                <div style="font-size: 2.5em; font-weight: bold; color: ${this.getCorrelationColor(data.correlation.r2)};">${data.correlation.r2.toFixed(3)}</div>
                <div style="width: 100%; height: 6px; background: #1a1b26; border-radius: 3px; margin-top: 10px; overflow: hidden;">
                  <div style="width: ${data.correlation.r2 * 100}%; height: 100%; background: ${this.getCorrelationColor(data.correlation.r2)}; border-radius: 3px; transition: width 1s ease;"></div>
                </div>
              </div>
              <div class="correlation-interpretation" style="flex: 1; display: flex; align-items: flex-start; gap: 12px; background: rgba(0, 0, 0, 0.2); padding: 15px; border-radius: 8px;">
                <div style="font-size: 1.5em;">💡</div>
                <p style="margin: 0; color: #9aa5ce; line-height: 1.6;">${data.correlation.interpretation}</p>
              </div>
            </div>
          </div>
        </section>

        <!-- Rank Stability Section -->
        <section class="insights-section" style="margin-top: 30px;">
          <div class="section-header">
            <div class="section-icon" style="background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); width: 40px; height: 40px; border-radius: 10px; display: flex; align-items: center; justify-content: center;">
              <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2">
                <path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/>
              </svg>
            </div>
            <div style="margin-left: 15px;">
              <h3 style="margin: 0; color: #c0caf5;">Rank Stability Analysis</h3>
              <p style="margin: 5px 0 0 0; color: #565f89; font-size: 0.85em;">How language rankings shift across different weighting scenarios</p>
            </div>
          </div>
          <div class="insights-card" style="margin-top: 15px; background: rgba(26, 27, 38, 0.8); border: 1px solid #2a2a35; border-radius: 12px; padding: 20px;">
            <canvas id="stabilityVizCanvas" width="850" height="250" style="width: 100%; max-width: 850px; display: block; margin: 0 auto;"></canvas>
            <div class="stability-grid" style="display: grid; grid-template-columns: 1fr auto 1fr; gap: 20px; margin-top: 20px;">
              <div class="stability-column stable">
                <div style="display: flex; align-items: center; gap: 8px; margin-bottom: 15px; color: #00ff9d;">
                  <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <polyline points="20 6 9 17 4 12"/>
                  </svg>
                  <span style="font-weight: 600;">Most Stable</span>
                </div>
                <div id="stable-languages-enhanced" style="display: flex; flex-direction: column; gap: 8px;">
                  ${this.renderStabilityList(data.stability.slice(0, 5), 'stable')}
                </div>
              </div>
              <div class="stability-divider" style="width: 1px; background: #2a2a35;"></div>
              <div class="stability-column volatile">
                <div style="display: flex; align-items: center; gap: 8px; margin-bottom: 15px; color: #f5576c;">
                  <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/>
                  </svg>
                  <span style="font-weight: 600;">Most Volatile</span>
                </div>
                <div id="unstable-languages-enhanced" style="display: flex; flex-direction: column; gap: 8px;">
                  ${this.renderStabilityList(data.stability.slice(-5).reverse(), 'volatile')}
                </div>
              </div>
            </div>
          </div>
        </section>

        <!-- Outliers Section -->
        <section class="insights-section" style="margin-top: 30px;">
          <div class="section-header">
            <div class="section-icon" style="background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); width: 40px; height: 40px; border-radius: 10px; display: flex; align-items: center; justify-content: center;">
              <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2">
                <circle cx="12" cy="12" r="10"/>
                <path d="M12 16v-4"/>
                <path d="M12 8h.01"/>
              </svg>
            </div>
            <div style="margin-left: 15px;">
              <h3 style="margin: 0; color: #c0caf5;">Statistical Outliers</h3>
              <p style="margin: 5px 0 0 0; color: #565f89; font-size: 0.85em;">Languages with exceptional performance characteristics (IQR method)</p>
            </div>
          </div>
          <div class="insights-card" style="margin-top: 15px; background: rgba(26, 27, 38, 0.8); border: 1px solid #2a2a35; border-radius: 12px; padding: 20px;">
            ${data.outliers.length > 0 ? `
              <div class="outliers-list" style="display: grid; grid-template-columns: repeat(auto-fill, minmax(250px, 1fr)); gap: 15px;">
                ${this.renderOutliersList(data.outliers)}
              </div>
            ` : `
              <div class="no-outliers" style="display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 40px; color: #565f89;">
                <svg width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" style="opacity: 0.5; margin-bottom: 15px;">
                  <circle cx="12" cy="12" r="10"/>
                  <line x1="15" y1="9" x2="9" y2="15"/>
                  <line x1="9" y1="9" x2="15" y2="15"/>
                </svg>
                <p>No statistical outliers detected</p>
              </div>
            `}
          </div>
        </section>
      </div>
    `;

    // Attach close handler
    modal.querySelector('.modal-close-btn')?.addEventListener('click', () => this.close());

    return modal;
  }

  private getCorrelationColor(r2: number): string {
    if (r2 > 0.7) return '#00ff9d';
    if (r2 > 0.4) return '#00b8ff';
    if (r2 > 0.1) return '#ffaa00';
    return '#9aa5ce';
  }

  private renderStabilityList(items: StabilityData[], type: 'stable' | 'volatile'): string {
    return items.map((item, index) => `
      <div class="stability-item" style="display: flex; justify-content: space-between; align-items: center; padding: 8px 12px; background: rgba(0, 0, 0, 0.2); border-radius: 6px; animation: fadeInUp 0.3s ease forwards; animation-delay: ${index * 0.1}s; opacity: 0;">
        <span style="color: #c0caf5;">${item.language}</span>
        <span style="color: ${type === 'stable' ? '#00ff9d' : '#f5576c'}; font-weight: 600;">±${item.maxSwing}</span>
      </div>
    `).join('');
  }

  private renderOutliersList(outliers: OutlierData[]): string {
    const colors = ['#fa709a', '#fee140', '#30cfd0', '#a8edea', '#ff6b6b', '#4ecdc4'];

    return outliers.map((outlier, index) => `
      <div class="outlier-item" style="border-left: 3px solid ${colors[index % colors.length]}; background: rgba(0, 0, 0, 0.2); padding: 15px; border-radius: 8px; animation: fadeInUp 0.4s ease forwards; animation-delay: ${index * 0.1}s; opacity: 0;">
        <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;">
          <span style="color: #c0caf5; font-weight: 600;">${outlier.language}</span>
          <span style="background: ${colors[index % colors.length]}22; color: ${colors[index % colors.length]}; padding: 2px 8px; border-radius: 4px; font-size: 0.8em;">${outlier.metric}</span>
        </div>
        <p style="margin: 0; color: #9aa5ce; font-size: 0.85em; line-height: 1.5;">${outlier.explanation}</p>
      </div>
    `).join('');
  }

  private drawStabilityVisualization(): void {
    const canvas = document.getElementById('stabilityVizCanvas') as HTMLCanvasElement;
    if (!canvas || !this.insightsData) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    const data = this.insightsData.stability;
    if (data.length === 0) return;

    const width = canvas.width;
    const height = canvas.height;
    const padding = { top: 40, right: 80, bottom: 40, left: 100 };
    const chartWidth = width - padding.left - padding.right;
    const chartHeight = height - padding.top - padding.bottom;

    // Clear canvas
    ctx.clearRect(0, 0, width, height);

    // Get top 10 most volatile
    const sorted = [...data].sort((a, b) => b.maxSwing - a.maxSwing);
    const top10 = sorted.slice(0, 10);

    const barHeight = chartHeight / top10.length;
    const maxSwing = Math.max(...top10.map(d => d.maxSwing), 1);

    // Draw background grid
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.05)';
    ctx.lineWidth = 1;
    for (let i = 0; i <= 5; i++) {
      const x = padding.left + (chartWidth / 5) * i;
      ctx.beginPath();
      ctx.moveTo(x, padding.top);
      ctx.lineTo(x, height - padding.bottom);
      ctx.stroke();

      // Grid labels
      ctx.fillStyle = '#565f89';
      ctx.font = '10px JetBrains Mono, monospace';
      ctx.textAlign = 'center';
      ctx.fillText(Math.round((maxSwing / 5) * i).toString(), x, height - padding.bottom + 15);
    }

    // Draw bars
    top10.forEach((lang, index) => {
      const y = padding.top + index * barHeight;
      const barWidth = (lang.maxSwing / maxSwing) * chartWidth;

      // Gradient color based on volatility
      const gradient = ctx.createLinearGradient(padding.left, 0, padding.left + barWidth, 0);
      const hue = 120 - (lang.maxSwing / maxSwing) * 120; // Green to Red
      gradient.addColorStop(0, `hsla(${hue}, 70%, 50%, 0.3)`);
      gradient.addColorStop(1, `hsla(${hue}, 70%, 50%, 0.8)`);

      // Draw bar
      ctx.fillStyle = gradient;
      ctx.fillRect(padding.left, y + barHeight * 0.2, barWidth, barHeight * 0.6);

      // Draw border
      ctx.strokeStyle = `hsla(${hue}, 70%, 50%, 1)`;
      ctx.lineWidth = 2;
      ctx.strokeRect(padding.left, y + barHeight * 0.2, barWidth, barHeight * 0.6);

      // Draw language name
      ctx.fillStyle = '#c0caf5';
      ctx.font = '11px JetBrains Mono, monospace';
      ctx.textAlign = 'right';
      ctx.fillText(lang.language, padding.left - 10, y + barHeight * 0.5 + 4);

      // Draw value
      ctx.textAlign = 'left';
      ctx.fillStyle = '#fff';
      ctx.fillText(`±${lang.maxSwing}`, padding.left + barWidth + 10, y + barHeight * 0.5 + 4);
    });

    // Draw title
    ctx.fillStyle = '#565f89';
    ctx.font = '12px JetBrains Mono, monospace';
    ctx.textAlign = 'center';
    ctx.fillText('Rank Volatility (Max Position Swing)', width / 2, padding.top - 15);

    // Draw axis label
    ctx.fillText('Position Changes →', width / 2, height - 5);
  }
}

export const scoringInsightsModal = new ScoringInsightsModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).scoringInsightsModal = scoringInsightsModal;
  (window as any).showScoringInsights = () => {
    scoringInsightsModal.open();
  };
}
