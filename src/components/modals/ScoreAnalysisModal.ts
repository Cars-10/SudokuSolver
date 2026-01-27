// Score Analysis Modal - shows scoring breakdown with radar chart (legacy style)
import { BaseModal } from './BaseModal';
import { metricsService } from '../../services/MetricsService';
import type { SolverMetrics } from '../../types/metrics';

export class ScoreAnalysisModal extends BaseModal {
  private currentLanguage: string = '';
  private currentMetrics: SolverMetrics | undefined;
  private radarCanvas: HTMLCanvasElement | null = null;

  constructor() {
    super('SAM', 'Score Analysis Modal', {
      dependencies: ['MetricsService']
    });
  }

  protected getDomId(): string {
    return 'scoreModal';
  }

  /**
   * Show score analysis for a specific language
   */
  showForLanguage(language: string): void {
    this.currentLanguage = language;
    this.currentMetrics = metricsService.getByLanguage(language);
    this.open();
  }

  protected render(): HTMLElement {
    const modal = document.createElement('div');
    modal.id = this.getDomId();
    modal.className = 'modal score-modal';
    modal.style.width = '800px';

    if (!this.currentMetrics) {
      modal.innerHTML = `
        <div class="modal-header">
          <h2>Score Analysis</h2>
          <button class="modal-close-btn" aria-label="Close modal">&times;</button>
        </div>
        <div class="modal-body">
          <p>No metrics available for ${this.currentLanguage}</p>
        </div>
      `;
      // Attach close handler after innerHTML
      modal.querySelector('.modal-close-btn')?.addEventListener('click', () => this.close());
      return modal;
    }

    const metrics = this.currentMetrics;
    const score = metrics.score || 0;
    const tier = metrics.tier || 'F';
    const logo = metrics.logo || '';
    const cBaseline = metricsService.getByLanguage('C');

    modal.innerHTML = `
      <div class="modal-header score-modal-header">
        <div class="score-modal-title">
          <img class="score-modal-logo" src="${logo}" alt="${this.currentLanguage}" onerror="this.style.display='none'">
          <div>
            <h2 style="margin: 0; color: #7aa2f7;">${this.currentLanguage}</h2>
            <p style="margin: 5px 0 0 0; color: #565f89; font-size: 0.9em;">${metrics.algorithm || 'BruteForce'} Algorithm</p>
          </div>
        </div>
        <div class="tier-badge tier-${tier.toLowerCase()}" style="font-size: 1.2em; width: 40px; height: 40px;">${tier}</div>
        <button class="modal-close-btn" aria-label="Close modal">&times;</button>
      </div>

      <div class="modal-body score-modal-body">
        <!-- Left column: Score breakdown -->
        <div class="score-modal-left">
          <h3 style="color: #7aa2f7; margin: 0 0 15px 0; font-size: 1em; border-bottom: 1px solid #414868; padding-bottom: 8px;">Composite Score</h3>
          <div class="score-modal-value" style="font-size: 2.5em; font-weight: bold; color: ${this.getScoreColor(score)}; text-align: center; margin-bottom: 20px;">
            ${score > 0 ? score.toFixed(2) + 'x' : 'N/A'}
          </div>
          <div class="score-breakdown">
            <div class="breakdown-item">
              <span class="breakdown-label">Time Ratio</span>
              <span class="breakdown-value" style="color: ${this.getRatioColor(metrics.scoreBreakdown?.time || 1)}">${metrics.scoreBreakdown?.time?.toFixed(2) || 'N/A'}x</span>
            </div>
            <div class="breakdown-item">
              <span class="breakdown-label">Memory Ratio</span>
              <span class="breakdown-value" style="color: ${this.getRatioColor(metrics.scoreBreakdown?.memory || 1)}">${metrics.scoreBreakdown?.memory?.toFixed(2) || 'N/A'}x</span>
            </div>
            <div class="breakdown-item">
              <span class="breakdown-label">CPU Ratio</span>
              <span class="breakdown-value" style="color: ${this.getRatioColor(metrics.scoreBreakdown?.cpu || 1)}">${metrics.scoreBreakdown?.cpu?.toFixed(2) || 'N/A'}x</span>
            </div>
            <div class="breakdown-item">
              <span class="breakdown-label">Consistency</span>
              <span class="breakdown-value" style="color: ${metrics.hasMismatch ? '#ff5555' : '#00ff9d'}">${metrics.hasMismatch ? 'Mismatch ⚠' : 'Verified ✓'}</span>
            </div>
          </div>
        </div>

        <!-- Right column: Radar chart -->
        <div class="score-modal-right">
          <h3 style="color: #7aa2f7; margin: 0 0 15px 0; font-size: 1em; border-bottom: 1px solid #414868; padding-bottom: 8px;">Performance Profile</h3>
          <canvas id="scoreRadarChart" width="280" height="280"></canvas>
          <div class="radar-legend" style="display: flex; gap: 20px; margin-top: 15px; justify-content: center;">
            <div class="legend-item" style="display: flex; align-items: center; gap: 8px; font-size: 0.85em; color: #9aa5ce;">
              <span class="legend-swatch" style="width: 12px; height: 12px; border-radius: 3px; background: #00ff9d;"></span>
              <span>${this.currentLanguage}</span>
            </div>
            <div class="legend-item" style="display: flex; align-items: center; gap: 8px; font-size: 0.85em; color: #9aa5ce;">
              <span class="legend-swatch" style="width: 12px; height: 12px; border-radius: 3px; background: rgba(122, 162, 247, 0.5);"></span>
              <span>C Baseline</span>
            </div>
          </div>
        </div>
      </div>

      <!-- Matrix Results Table -->
      <div class="score-modal-matrix" style="padding: 0 20px 20px 20px;">
        <h3 style="color: #7aa2f7; margin: 0 0 15px 0; font-size: 1em; border-bottom: 1px solid #414868; padding-bottom: 8px;">Matrix Results</h3>
        <table class="score-matrix-table" style="width: 100%; border-collapse: collapse; font-size: 0.9em;">
          <thead>
            <tr style="color: #7aa2f7; text-transform: uppercase; font-size: 0.8em;">
              <th style="padding: 10px 12px; text-align: center; border-bottom: 1px solid #2a2a35;">Matrix</th>
              <th style="padding: 10px 12px; text-align: center; border-bottom: 1px solid #2a2a35;">Time</th>
              <th style="padding: 10px 12px; text-align: center; border-bottom: 1px solid #2a2a35;">C Time</th>
              <th style="padding: 10px 12px; text-align: center; border-bottom: 1px solid #2a2a35;">Ratio</th>
              <th style="padding: 10px 12px; text-align: center; border-bottom: 1px solid #2a2a35;">Iterations</th>
              <th style="padding: 10px 12px; text-align: center; border-bottom: 1px solid #2a2a35;">Memory</th>
            </tr>
          </thead>
          <tbody>
            ${this.renderMatrixRows(metrics, cBaseline)}
          </tbody>
        </table>
      </div>
    `;

    // Attach close handler after innerHTML
    modal.querySelector('.modal-close-btn')?.addEventListener('click', () => this.close());

    // Defer radar chart drawing
    setTimeout(() => this.drawRadarChart(metrics, cBaseline), 100);

    return modal;
  }

  private getScoreColor(score: number): string {
    if (score <= 1.0) return '#00ff9d';
    if (score <= 2.0) return '#00b8ff';
    if (score <= 5.0) return '#ffaa00';
    return '#ff5555';
  }

  private renderMatrixRows(metrics: SolverMetrics, cBaseline?: SolverMetrics): string {
    const results = metrics.results || [];
    const cResults = cBaseline?.results || [];

    if (results.length === 0) {
      return '<tr><td colspan="6" style="text-align: center; color: #565f89; padding: 20px;">No matrix results available</td></tr>';
    }

    return results.map(result => {
      const cResult = cResults.find(r => r.matrix === result.matrix);
      const cTime = cResult?.time || 0;
      const ratio = cTime > 0 ? (result.time / cTime) : 0;
      const isMismatch = metrics.iterationMismatches?.some(m => m.matrix === result.matrix);

      return `
        <tr style="color: #c0caf5; background: #16161e;">
          <td style="padding: 10px 12px; text-align: center;">Matrix ${result.matrix}</td>
          <td style="padding: 10px 12px; text-align: center;">${this.formatTime(result.time)}</td>
          <td style="padding: 10px 12px; text-align: center;">${cTime > 0 ? this.formatTime(cTime) : 'N/A'}</td>
          <td style="padding: 10px 12px; text-align: center; color: ${this.getRatioColor(ratio)};">${ratio > 0 ? ratio.toFixed(2) + 'x' : 'N/A'}</td>
          <td style="padding: 10px 12px; text-align: center; ${isMismatch ? 'color: #ff5555;' : ''}">${result.iterations?.toLocaleString() || 'N/A'}${isMismatch ? ' ⚠' : ''}</td>
          <td style="padding: 10px 12px; text-align: center;">${this.formatMemory(result.memory)}</td>
        </tr>
      `;
    }).join('');
  }

  private getRatioColor(ratio: number): string {
    if (ratio <= 1.0) return '#00ff9d';
    if (ratio <= 1.5) return '#00b8ff';
    if (ratio <= 3.0) return '#ffaa00';
    return '#ff5555';
  }

  private formatTime(ms: number): string {
    if (!ms) return 'N/A';
    if (ms >= 1000) return (ms / 1000).toFixed(3) + 's';
    return ms.toFixed(2) + 'ms';
  }

  private formatMemory(bytes: number): string {
    if (!bytes) return 'N/A';
    if (bytes >= 1024 * 1024 * 1024) return (bytes / (1024 * 1024 * 1024)).toFixed(1) + 'GB';
    if (bytes >= 1024 * 1024) return (bytes / (1024 * 1024)).toFixed(1) + 'MB';
    if (bytes >= 1024) return (bytes / 1024).toFixed(1) + 'KB';
    return bytes + 'B';
  }

  private drawRadarChart(metrics: SolverMetrics, cBaseline?: SolverMetrics): void {
    const canvas = document.getElementById('scoreRadarChart') as HTMLCanvasElement;
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    const width = canvas.width;
    const height = canvas.height;
    const centerX = width / 2;
    const centerY = height / 2;
    const radius = Math.min(width, height) / 2 - 40;

    // Clear canvas
    ctx.clearRect(0, 0, width, height);

    // Labels for the radar axes - match the breakdown section
    const labels = ['Time', 'Memory', 'CPU', 'Consistency'];
    const numAxes = labels.length;

    // Normalize values (0-1 scale, inverted so lower ratio = better = higher on chart)
    const maxRatio = 10; // 10x is max displayed

    const langValues = [
      Math.max(0, 1 - ((metrics.scoreBreakdown?.time || 1) - 1) / (maxRatio - 1)),
      Math.max(0, 1 - ((metrics.scoreBreakdown?.memory || 1) - 1) / (maxRatio - 1)),
      Math.max(0, 1 - ((metrics.scoreBreakdown?.cpu || 1) - 1) / (maxRatio - 1)),
      metrics.hasMismatch ? 0.3 : 1.0 // Consistency: 1.0 if verified, 0.3 if mismatch
    ];

    const cValues = [1, 1, 1, 1]; // C baseline is perfect (1x ratio = top of chart)

    // Draw grid circles
    ctx.strokeStyle = '#2a2a35';
    ctx.lineWidth = 1;
    for (let i = 1; i <= 4; i++) {
      ctx.beginPath();
      ctx.arc(centerX, centerY, (radius / 4) * i, 0, Math.PI * 2);
      ctx.stroke();
    }

    // Draw axis lines and labels
    ctx.fillStyle = '#9aa5ce';
    ctx.font = '11px JetBrains Mono';
    ctx.textAlign = 'center';

    for (let i = 0; i < numAxes; i++) {
      const angle = (Math.PI * 2 * i) / numAxes - Math.PI / 2;
      const x = centerX + radius * Math.cos(angle);
      const y = centerY + radius * Math.sin(angle);

      ctx.beginPath();
      ctx.moveTo(centerX, centerY);
      ctx.lineTo(x, y);
      ctx.strokeStyle = '#2a2a35';
      ctx.stroke();

      // Draw label
      const labelX = centerX + (radius + 20) * Math.cos(angle);
      const labelY = centerY + (radius + 20) * Math.sin(angle);
      ctx.fillText(labels[i], labelX, labelY + 4);
    }

    // Draw C baseline polygon
    this.drawRadarPolygon(ctx, centerX, centerY, radius, cValues, 'rgba(122, 162, 247, 0.3)', 'rgba(122, 162, 247, 0.8)');

    // Draw language polygon
    this.drawRadarPolygon(ctx, centerX, centerY, radius, langValues, 'rgba(0, 255, 157, 0.3)', '#00ff9d');
  }

  private drawRadarPolygon(
    ctx: CanvasRenderingContext2D,
    centerX: number,
    centerY: number,
    radius: number,
    values: number[],
    fillColor: string,
    strokeColor: string
  ): void {
    const numAxes = values.length;

    ctx.beginPath();
    for (let i = 0; i < numAxes; i++) {
      const angle = (Math.PI * 2 * i) / numAxes - Math.PI / 2;
      const value = Math.max(0, Math.min(1, values[i]));
      const x = centerX + radius * value * Math.cos(angle);
      const y = centerY + radius * value * Math.sin(angle);

      if (i === 0) {
        ctx.moveTo(x, y);
      } else {
        ctx.lineTo(x, y);
      }
    }
    ctx.closePath();

    ctx.fillStyle = fillColor;
    ctx.fill();

    ctx.strokeStyle = strokeColor;
    ctx.lineWidth = 2;
    ctx.stroke();

    // Draw points
    for (let i = 0; i < numAxes; i++) {
      const angle = (Math.PI * 2 * i) / numAxes - Math.PI / 2;
      const value = Math.max(0, Math.min(1, values[i]));
      const x = centerX + radius * value * Math.cos(angle);
      const y = centerY + radius * value * Math.sin(angle);

      ctx.beginPath();
      ctx.arc(x, y, 4, 0, Math.PI * 2);
      ctx.fillStyle = strokeColor;
      ctx.fill();
    }
  }
}

export const scoreAnalysisModal = new ScoreAnalysisModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).scoreAnalysisModal = scoreAnalysisModal;
  (window as any).showScoreAnalysis = (lang: string) => {
    scoreAnalysisModal.showForLanguage(lang);
  };
}
