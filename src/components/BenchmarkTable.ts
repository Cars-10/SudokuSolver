// Benchmark Table Component
import { metricsService } from '../services/MetricsService';
import { languageDetailsModal } from './modals/LanguageDetailsModal';
import { timeAgo } from '../utils/timeago';

export class BenchmarkTable {
  private container: HTMLElement | null = null;

  render(containerId: string): void {
    this.container = document.getElementById(containerId);
    if (!this.container) {
      console.error(`[BenchmarkTable] Container #${containerId} not found`);
      return;
    }

    const metrics = metricsService.getAll();

    if (metrics.length === 0) {
      this.container.innerHTML = `
        <div class="empty-state text-center p-xl text-muted">
          <p>No benchmark data available</p>
          <p class="text-sm mt-md">
            Start the server to load data: <code>cd server && npm start</code>
          </p>
          <p class="text-sm mt-sm">
            Or run benchmarks: <code>./runBenchmarks.sh --all</code>
          </p>
        </div>
      `;
      return;
    }

    // Sort by total time (fastest first)
    const sorted = [...metrics].sort((a, b) => {
      const aTime = a.totalTime || Infinity;
      const bTime = b.totalTime || Infinity;
      return aTime - bTime;
    });

    const tableHTML = `
      <div class="container">
        <table id="benchmark-table">
          <thead>
            <tr>
              <th>Rank</th>
              <th class="lang-col-header">Language</th>
              <th>Algorithm</th>
              <th>Updated</th>
              <th>Total Time (s)</th>
              <th>Avg Iterations</th>
              <th>Avg Memory (KB)</th>
              <th>Score</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            ${sorted.map((metric, index) => this.renderRow(metric, index + 1)).join('')}
          </tbody>
        </table>
      </div>
    `;

    this.container.innerHTML = tableHTML;
    this.attachEventListeners();
  }

  private renderRow(metric: any, rank: number): string {
    const lang = metric.language || metric.solver;
    const algo = metric.algorithm || metric.algorithmType || 'BruteForce';
    const updated = metric.timestamp ? timeAgo(metric.timestamp) : 'N/A';
    const totalTime = metric.totalTime ? metric.totalTime.toFixed(3) : 'N/A';
    const avgIters = metric.avgIterations ? Math.round(metric.avgIterations) : 'N/A';
    const avgMem = metric.avgMemory ? Math.round(metric.avgMemory) : 'N/A';
    const score = metric.score ? (metric.score * 100).toFixed(1) : 'N/A';
    const failed = metric.failed;

    const statusClass = failed ? 'status-fail' : 'status-success';
    const statusText = failed ? '✖ Failed' : '✓ Pass';

    const scoreClass = score !== 'N/A' ? 'score-col score-good' : 'score-col text-muted';

    return `
      <tr data-language="${lang}" data-algorithm="${algo}">
        <td>${rank}</td>
        <td class="lang-col" data-lang="${lang}">
          <span class="lang-name">${lang}</span>
        </td>
        <td>${algo}</td>
        <td class="text-muted text-sm">${updated}</td>
        <td>${totalTime}</td>
        <td>${avgIters}</td>
        <td>${avgMem}</td>
        <td class="${scoreClass}">
          ${score}${score !== 'N/A' ? '%' : ''}
        </td>
        <td>
          <span class="status-badge ${statusClass}">${statusText}</span>
        </td>
      </tr>
    `;
  }

  private attachEventListeners(): void {
    const langCells = document.querySelectorAll('.lang-col');
    langCells.forEach(cell => {
      cell.addEventListener('click', (e) => {
        const target = e.currentTarget as HTMLElement;
        const lang = target.dataset.lang;
        if (lang) {
          languageDetailsModal.showForLanguage(lang);
        }
      });
    });

    // Hover effects
    const rows = document.querySelectorAll('#benchmark-table tbody tr');
    rows.forEach(row => {
      row.addEventListener('mouseenter', () => {
        row.classList.add('active-row');
      });
      row.addEventListener('mouseleave', () => {
        row.classList.remove('active-row');
      });
    });
  }
}

export const benchmarkTable = new BenchmarkTable();
