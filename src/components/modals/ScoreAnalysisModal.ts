// Score Analysis Modal - explains scoring methodology and breakdown
import { BaseModal } from './BaseModal';
import { metricsService } from '../../services/MetricsService';
import type { SolverMetrics } from '../../types/metrics';

export class ScoreAnalysisModal extends BaseModal {
  private currentLanguage: string = '';
  private currentMetrics: SolverMetrics | undefined;

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
    const title = `Score Analysis: ${this.currentLanguage}`;
    const bodyContent = this.renderScoreAnalysis();

    return this.createModalElement(title, bodyContent);
  }

  private renderScoreAnalysis(): HTMLElement {
    const container = document.createElement('div');
    container.className = 'score-analysis-container';
    container.setAttribute('data-component-id', `${this.id}-CONTENT`);

    if (!this.currentMetrics) {
      container.innerHTML = '<p>No metrics available for score analysis.</p>';
      return container;
    }

    const metrics = this.currentMetrics;
    const score = metrics.score || 0;

    container.innerHTML = `
      <div class="score-overview">
        <h3>Overall Score</h3>
        <div class="score-display">
          <span class="score-value">${score > 0 ? score.toFixed(2) : 'N/A'}</span>
        </div>
        ${score > 0 ? '<p class="score-note">Lower scores indicate better performance</p>' : ''}
      </div>

      <div class="score-breakdown">
        <div class="score-component">
          <h4>⏱️ Time Component (80% weight)</h4>
          <p>Total execution time across all matrices: <strong>${(metrics.totalTime || 0).toFixed(3)}s</strong></p>
          <div class="score-formula">
            Time Score = (Your Time - Min Time) / (Max Time - Min Time)
          </div>
        </div>

        <div class="score-component">
          <h4>💾 Memory Component (20% weight)</h4>
          <p>Average memory usage: <strong>${(metrics.avgMemory || 0).toFixed(2)} MB</strong></p>
          <div class="score-formula">
            Memory Score = (Your Memory - Min Memory) / (Max Memory - Min Memory)
          </div>
        </div>
      </div>

      <div class="score-formula-section">
        <h3>Composite Score Formula</h3>
        <div class="score-formula">
          Final Score = 0.8 × Time Score + 0.2 × Memory Score
        </div>
        <p>This weighting emphasizes execution speed while still considering memory efficiency.</p>
      </div>

      <div class="score-interpretation">
        <h3>Interpretation</h3>
        <ul>
          <li><strong>0.00 - 0.20:</strong> Excellent performance (top tier)</li>
          <li><strong>0.21 - 0.40:</strong> Very good performance</li>
          <li><strong>0.41 - 0.60:</strong> Good performance</li>
          <li><strong>0.61 - 0.80:</strong> Average performance</li>
          <li><strong>0.81 - 1.00:</strong> Below average performance</li>
        </ul>
      </div>
    `;

    return container;
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
