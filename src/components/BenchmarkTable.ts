// Benchmark Table Component - Rich format matching legacy UI
import { metricsService } from '../services/MetricsService';
import { languageDetailsModal } from './modals/LanguageDetailsModal';
import { scoreAnalysisModal } from './modals/ScoreAnalysisModal';
import { timeAgo, formatTimestamp, elapsedTimeDetailed } from '../utils/timeago';
import type { SolverMetrics, MetricResult } from '../types/metrics';

// Matrix names for column headers
const MATRIX_NAMES = ['1', '2', '3', '4', '5', '6'];

interface FilterOptions {
  search?: string;
  algorithm?: string | null;
  showMismatches?: boolean;
  showFailed?: boolean;
}

interface SortState {
  key: string;
  direction: 'asc' | 'desc';
}

export class BenchmarkTable {
  private container: HTMLElement | null = null;
  private containerId: string = '';
  private expandedRows: Set<string> = new Set();
  private filterOptions: FilterOptions = {
    search: '',
    algorithm: null,
    showMismatches: true,
    showFailed: true
  };
  private sortState: SortState = { key: 'score', direction: 'asc' };
  private updateInterval: number | null = null;
  private tooltip: HTMLElement | null = null;

  render(containerId: string): void {
    this.containerId = containerId;
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

    // Debug: Count algorithms
    const algoCounts: Record<string, number> = {};
    metrics.forEach(m => {
      const algo = m.algorithm || m.algorithmType || 'BruteForce';
      algoCounts[algo] = (algoCounts[algo] || 0) + 1;
    });
    console.log('[BenchmarkTable] Algorithm distribution:', algoCounts);

    const tableHTML = `
      <div class="table-container">
        <table id="benchmark-table" class="benchmark-table">
          <thead>
            <tr>
              <th class="lang-col-header sortable-header" data-sort="lang">Language <span class="sort-arrow">▲</span></th>
              <th class="score-col-header sortable-header sorted asc" data-sort="score">Score <span class="sort-arrow">▲</span></th>
              <th class="updated-col-header sortable-header" data-sort="updated">Updated <span class="sort-arrow">▲</span></th>
              ${MATRIX_NAMES.map(m => `<th class="matrix-col-header sortable-header" data-sort="m${m}">M${m} <span class="sort-arrow">▲</span></th>`).join('')}
              <th class="total-col-header sortable-header" data-sort="total">Total <span class="sort-arrow">▲</span></th>
            </tr>
          </thead>
          <tbody>
            ${metrics.map((metric, index) => this.renderMainRow(metric, index)).join('')}
          </tbody>
        </table>
      </div>
    `;

    this.container.innerHTML = tableHTML;
    this.createTooltip();
    this.attachEventListeners();
    this.startLiveUpdates();
  }

  private renderMainRow(metric: SolverMetrics, index: number): string {
    const lang = metric.language || metric.solver;
    const displayLang = this.getDisplayName(lang);
    const algo = metric.algorithm || metric.algorithmType || 'BruteForce';

    // Debug: log first 5 rows' algorithm values
    if (index < 5) {
      console.log(`[BenchmarkTable] Row ${index}: ${lang} -> algo="${algo}" (algorithm=${metric.algorithm}, algorithmType=${metric.algorithmType})`);
    }
    const algoShort = this.getAlgoShort(algo);
    const algoClass = this.getAlgoClass(algo);
    const timestamp = metric.timestamp ? new Date(metric.timestamp).getTime() : 0;
    const tier = metric.tier || 'F';
    const score = metric.score?.toFixed(2) || 'N/A';
    const totalTime = metric.totalTime || 0;
    const totalIters = metric.totalIterations || 0;
    const hasMismatch = metric.hasMismatch;
    const isFailed = metric.failed;
    const rowClasses = ['main-row'];
    if (hasMismatch) rowClasses.push('mismatch-iterations');
    if (isFailed) rowClasses.push('failed');
    if (score !== 'N/A' && parseFloat(score) > 100) rowClasses.push('suspect');

    // Build matrix time data attributes for sorting
    const matrixTimeAttrs = MATRIX_NAMES.map((name, idx) => {
      const result = metric.results?.find(r => r.matrix === name);
      return `data-m${idx}-time="${result?.time || 999999}"`;
    }).join(' ');

    // Data attributes for sorting and filtering
    const dataAttrs = `
      data-lang="${lang}"
      data-algorithm-type="${algo}"
      data-timestamp="${timestamp}"
      data-time="${totalTime.toFixed(6)}"
      data-score="${metric.score || 0}"
      data-tier="${tier}"
      data-iters="${totalIters}"
      data-mem="${metric.maxMemory || 0}"
      ${matrixTimeAttrs}
    `;

    return `
      <tr class="${rowClasses.join(' ')}" ${dataAttrs} onclick="window.toggleBenchmarkRow('expand-${lang}')">
        <td class="lang-col" onclick="event.stopPropagation(); window.showLanguageDetails && window.showLanguageDetails('${lang}');">
          <img src="${metric.logo}" alt="${displayLang}" class="lang-logo" onerror="this.style.display='none'">
          <div class="lang-info">
            <div class="lang-name">
              ${displayLang}
              <span class="algo-badge ${algoClass}" title="${algo}">${algoShort}</span>
              ${hasMismatch ? '<span class="mismatch-badge" title="Iteration mismatch">⚠</span>' : ''}
            </div>
          </div>
        </td>
        <td class="score-col" onclick="event.stopPropagation(); window.showScoreModal && window.showScoreModal('${lang}');">
          <div class="score-container">
            <div class="tier-badge tier-${tier.toLowerCase()}" title="Tier ${tier}">${tier}</div>
            <div class="score-decomposition">
              ${this.renderScoreBar(metric)}
              <span class="score-value">${score}</span>
            </div>
          </div>
        </td>
        <td class="updated-cell">
          <span class="relative-time" data-ts="${timestamp}">${metric.timestamp ? timeAgo(metric.timestamp) : 'N/A'}</span>
        </td>
        ${this.renderMatrixCells(metric)}
        <td class="total-time ${hasMismatch ? 'has-mismatch' : ''}">
          <div class="total-container">
            <div class="total-value">${this.formatTime(totalTime)}</div>
            <div class="total-iters ${hasMismatch ? 'mismatch' : ''}">${this.formatNumber(totalIters)} iters</div>
          </div>
        </td>
      </tr>
      <tr class="expand-row" id="expand-${lang}" style="display: none;">
        <td colspan="${3 + MATRIX_NAMES.length + 1}">
          <div class="expand-content">
            ${this.renderExpandedContent(metric)}
          </div>
        </td>
      </tr>
    `;
  }

  private renderScoreBar(metric: SolverMetrics): string {
    if (!metric.scoreBreakdown) return '<div class="stacked-bar"></div>';

    // Normalize breakdown values to percentages
    const total = Math.max(metric.scoreBreakdown.time + metric.scoreBreakdown.memory, 0.01);
    const timePercent = Math.min(80, (metric.scoreBreakdown.time / total) * 100);
    const memPercent = 100 - timePercent;

    return `
      <div class="stacked-bar">
        <div class="bar-segment bar-time" style="width: ${timePercent}%"></div>
        <div class="bar-segment bar-memory" style="width: ${memPercent}%"></div>
      </div>
    `;
  }

  private renderMatrixCells(metric: SolverMetrics): string {
    return MATRIX_NAMES.map((matrixName, idx) => {
      const result = metric.results?.find(r => r.matrix === matrixName);
      if (!result) {
        return `<td class="matrix-cell" data-matrix-index="${idx}"><span class="no-data">-</span></td>`;
      }

      const timeStr = this.formatTime(result.time);
      const memStr = this.formatMemory(result.memory);
      const isMismatch = metric.iterationMismatches?.some(m => m.matrix === matrixName);

      return `
        <td class="matrix-cell" data-matrix-index="${idx}">
          <div class="cell-content">
            <div class="cell-header">
              <div class="time" title="Wall Clock Time">${timeStr}</div>
            </div>
            <div class="meta">
              <span class="${isMismatch ? 'mismatch' : ''}" title="Iterations">#${this.formatNumber(result.iterations)}</span>
              <span title="Memory">${memStr}</span>
            </div>
          </div>
        </td>
      `;
    }).join('');
  }

  private renderExpandedContent(metric: SolverMetrics): string {
    const lang = metric.language || metric.solver;

    return `
      <div class="expanded-details">
        <div class="detail-section">
          <h4>Performance Summary</h4>
          <div class="detail-grid">
            <div class="detail-item">
              <span class="detail-label">Total Time:</span>
              <span class="detail-value">${this.formatTime(metric.totalTime || 0)}</span>
            </div>
            <div class="detail-item">
              <span class="detail-label">Avg Memory:</span>
              <span class="detail-value">${this.formatMemory(metric.avgMemory || 0)}</span>
            </div>
            <div class="detail-item">
              <span class="detail-label">Score:</span>
              <span class="detail-value">${metric.score?.toFixed(2) || 'N/A'}x</span>
            </div>
            <div class="detail-item">
              <span class="detail-label">Tier:</span>
              <span class="tier-badge tier-${(metric.tier || 'F').toLowerCase()}">${metric.tier || 'F'}</span>
            </div>
          </div>
        </div>
        ${metric.scoreBreakdown ? `
        <div class="detail-section">
          <h4>Score Breakdown</h4>
          <div class="detail-grid">
            <div class="detail-item">
              <span class="detail-label">Time Ratio:</span>
              <span class="detail-value">${metric.scoreBreakdown.time.toFixed(2)}x</span>
            </div>
            <div class="detail-item">
              <span class="detail-label">Memory Ratio:</span>
              <span class="detail-value">${metric.scoreBreakdown.memory.toFixed(2)}x</span>
            </div>
            <div class="detail-item">
              <span class="detail-label">CPU Ratio:</span>
              <span class="detail-value">${metric.scoreBreakdown.cpu.toFixed(2)}x</span>
            </div>
          </div>
        </div>
        ` : ''}
        ${metric.hasMismatch ? `
        <div class="detail-section warning">
          <h4>⚠ Iteration Mismatches</h4>
          <p>The following matrices have incorrect iteration counts:</p>
          <ul>
            ${metric.iterationMismatches?.map(m => `
              <li>Matrix ${m.matrix}: Got ${this.formatNumber(m.actual)}, expected ${this.formatNumber(m.expected)}</li>
            `).join('')}
          </ul>
        </div>
        ` : ''}
        <div class="detail-actions">
          <button class="btn btn-primary" onclick="window.showLanguageDetails && window.showLanguageDetails('${lang}')">
            View Details
          </button>
          <button class="btn" onclick="window.showSourceCode && window.showSourceCode('${lang}')">
            View Source
          </button>
        </div>
      </div>
    `;
  }

  private getDisplayName(lang: string): string {
    if (lang === 'C_Sharp') return 'C#';
    if (lang === 'F_Sharp') return 'F#';
    return lang;
  }

  private getAlgoShort(algo: string): string {
    switch (algo) {
      case 'BruteForce': return 'BF';
      case 'DLX': return 'DLX';
      case 'CP': return 'CP';
      default: return algo.substring(0, 2).toUpperCase();
    }
  }

  private getAlgoClass(algo: string): string {
    switch (algo) {
      case 'BruteForce': return 'algo-bf';
      case 'DLX': return 'algo-dlx';
      case 'CP': return 'algo-cp';
      default: return 'algo-other';
    }
  }

  private getFileExtension(lang: string): string {
    const extensions: Record<string, string> = {
      'Python': 'py', 'JavaScript': 'js', 'TypeScript': 'ts',
      'C': 'c', 'C++': 'cpp', 'C_Sharp': 'cs', 'Java': 'java',
      'Go': 'go', 'Rust': 'rs', 'Ruby': 'rb', 'PHP': 'php',
      'Swift': 'swift', 'Kotlin': 'kt', 'Scala': 'scala'
    };
    return extensions[lang] || 'txt';
  }

  private formatTime(ms: number): string {
    if (ms >= 1000) {
      return (ms / 1000).toFixed(2) + ' s';
    }
    return ms.toFixed(2) + ' ms';
  }

  private formatMemory(bytes: number): string {
    if (bytes >= 1024 * 1024 * 1024) {
      return (bytes / (1024 * 1024 * 1024)).toFixed(1) + 'G';
    }
    if (bytes >= 1024 * 1024) {
      return (bytes / (1024 * 1024)).toFixed(1) + 'M';
    }
    if (bytes >= 1024) {
      return (bytes / 1024).toFixed(1) + 'K';
    }
    return bytes + 'B';
  }

  private formatNumber(num: number): string {
    return num.toLocaleString();
  }

  /**
   * Create the timestamp tooltip element
   */
  private createTooltip(): void {
    // Remove existing tooltip if any
    const existing = document.getElementById('updated-tooltip');
    if (existing) existing.remove();

    this.tooltip = document.createElement('div');
    this.tooltip.id = 'updated-tooltip';
    this.tooltip.className = 'updated-tooltip';
    this.tooltip.style.cssText = `
      position: fixed;
      display: none;
      background: #1e2030;
      border: 1px solid var(--border);
      border-radius: 6px;
      padding: 8px 10px;
      z-index: 1002;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.3);
      pointer-events: none;
      font-size: 0.8em;
    `;
    document.body.appendChild(this.tooltip);
  }

  /**
   * Show tooltip for updated cell
   */
  private showTooltip(cell: HTMLElement, timestamp: number): void {
    if (!this.tooltip || timestamp <= 0) return;

    const date = new Date(timestamp);
    const formattedTime = formatTimestamp(date);
    const elapsed = elapsedTimeDetailed(date);

    this.tooltip.innerHTML = `
      <div style="margin-bottom: 4px;">
        <span style="color: var(--muted); font-size: 0.85em;">Run:</span>
        <span style="color: var(--primary); font-weight: 500;">${formattedTime}</span>
      </div>
      <div>
        <span style="color: var(--muted); font-size: 0.85em;">Elapsed:</span>
        <span style="color: var(--text);">${elapsed}</span>
      </div>
    `;

    // Position tooltip near the cell
    const rect = cell.getBoundingClientRect();

    let left = rect.left;
    let top = rect.bottom + 4;

    // Keep within viewport
    if (left < 10) left = 10;
    if (top + 60 > window.innerHeight) top = rect.top - 60;

    this.tooltip.style.left = `${left}px`;
    this.tooltip.style.top = `${top}px`;
    this.tooltip.style.display = 'block';
  }

  /**
   * Hide the tooltip
   */
  private hideTooltip(): void {
    if (this.tooltip) {
      this.tooltip.style.display = 'none';
    }
  }

  private attachEventListeners(): void {
    // Language cell click -> open modal
    const langCells = document.querySelectorAll('.lang-col');
    langCells.forEach(cell => {
      cell.addEventListener('click', (e) => {
        e.stopPropagation();
        const row = (e.currentTarget as HTMLElement).closest('tr');
        const lang = row?.getAttribute('data-lang');
        if (lang) {
          languageDetailsModal.showForLanguage(lang);
        }
      });
    });

    // Global toggle function for expandable rows
    (window as any).toggleBenchmarkRow = (rowId: string) => {
      const expandRow = document.getElementById(rowId);
      if (expandRow) {
        const isHidden = expandRow.style.display === 'none';
        expandRow.style.display = isHidden ? 'table-row' : 'none';
      }
    };

    // Global function for score modal
    (window as any).showScoreModal = (lang: string) => {
      scoreAnalysisModal.showForLanguage(lang);
    };

    // Updated cell hover -> show tooltip
    const updatedCells = document.querySelectorAll('.updated-cell');
    updatedCells.forEach(cell => {
      cell.addEventListener('mouseenter', (e) => {
        const timeSpan = cell.querySelector('.relative-time');
        const ts = parseInt(timeSpan?.getAttribute('data-ts') || '0', 10);
        if (ts > 0) {
          this.showTooltip(cell as HTMLElement, ts);
        }
      });
      cell.addEventListener('mouseleave', () => {
        this.hideTooltip();
      });
    });

    // Header sorting
    const headers = document.querySelectorAll('[data-sort]');
    headers.forEach(header => {
      header.addEventListener('click', () => {
        const sortKey = header.getAttribute('data-sort');
        if (sortKey) this.sortTable(sortKey);
      });
    });
  }

  private sortTable(key: string): void {
    const table = document.getElementById('benchmark-table');
    if (!table) return;

    const tbody = table.querySelector('tbody');
    if (!tbody) return;

    // Toggle direction if same key
    if (this.sortState.key === key) {
      this.sortState.direction = this.sortState.direction === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortState.key = key;
      this.sortState.direction = 'asc';
    }

    // Get all main rows (not expand rows)
    const rows = Array.from(tbody.querySelectorAll('tr.main-row'));

    // Sort rows
    rows.sort((a, b) => {
      let aVal: any;
      let bVal: any;

      switch (key) {
        case 'lang':
          aVal = a.getAttribute('data-lang')?.toLowerCase() || '';
          bVal = b.getAttribute('data-lang')?.toLowerCase() || '';
          break;
        case 'score':
          aVal = parseFloat(a.getAttribute('data-score') || '999999');
          bVal = parseFloat(b.getAttribute('data-score') || '999999');
          break;
        case 'updated':
          aVal = parseInt(a.getAttribute('data-timestamp') || '0');
          bVal = parseInt(b.getAttribute('data-timestamp') || '0');
          break;
        case 'total':
          aVal = parseFloat(a.getAttribute('data-time') || '999999');
          bVal = parseFloat(b.getAttribute('data-time') || '999999');
          break;
        default:
          // Matrix columns (m1, m2, etc.)
          if (key.startsWith('m')) {
            const matrixIdx = parseInt(key.substring(1)) - 1;
            aVal = parseFloat(a.getAttribute(`data-m${matrixIdx}-time`) || '999999');
            bVal = parseFloat(b.getAttribute(`data-m${matrixIdx}-time`) || '999999');
          } else {
            aVal = 0;
            bVal = 0;
          }
      }

      if (typeof aVal === 'string') {
        return this.sortState.direction === 'asc'
          ? aVal.localeCompare(bVal)
          : bVal.localeCompare(aVal);
      }

      return this.sortState.direction === 'asc' ? aVal - bVal : bVal - aVal;
    });

    // Rebuild tbody with sorted rows (including expand rows)
    const fragment = document.createDocumentFragment();
    rows.forEach(row => {
      fragment.appendChild(row);
      // Also move the expand row that follows
      const lang = row.getAttribute('data-lang');
      const expandRow = document.getElementById(`expand-${lang}`);
      if (expandRow) {
        fragment.appendChild(expandRow);
      }
    });

    tbody.innerHTML = '';
    tbody.appendChild(fragment);

    // Update header sort indicators
    this.updateSortIndicators(key);
  }

  private updateSortIndicators(activeKey: string): void {
    const headers = document.querySelectorAll('[data-sort]');
    headers.forEach(header => {
      header.classList.remove('sorted', 'asc', 'desc');
      const arrow = header.querySelector('.sort-arrow');
      if (arrow) {
        arrow.textContent = '▲'; // Reset arrow
      }
      if (header.getAttribute('data-sort') === activeKey) {
        header.classList.add('sorted', this.sortState.direction);
        if (arrow) {
          arrow.textContent = this.sortState.direction === 'asc' ? '▲' : '▼';
        }
      }
    });
  }

  /**
   * Filter the table based on options
   */
  filter(options: Partial<FilterOptions>): void {
    console.log('[BenchmarkTable] filter() called with:', options);
    this.filterOptions = { ...this.filterOptions, ...options };
    console.log('[BenchmarkTable] filterOptions now:', this.filterOptions);
    this.applyFilters();
  }

  private applyFilters(): void {
    const table = document.getElementById('benchmark-table');
    if (!table) {
      console.error('[BenchmarkTable] applyFilters: table not found!');
      return;
    }

    const rows = table.querySelectorAll('tr.main-row');
    const { search, algorithm, showMismatches, showFailed } = this.filterOptions;
    const searchLower = search?.toLowerCase() || '';

    console.log(`[BenchmarkTable] applyFilters: ${rows.length} rows, algorithm filter = "${algorithm}"`);

    let hiddenCount = 0;
    let shownCount = 0;

    rows.forEach(row => {
      const lang = row.getAttribute('data-lang') || '';
      const algo = row.getAttribute('data-algorithm-type') || '';
      const hasMismatch = row.classList.contains('mismatch-iterations');
      const isFailed = row.classList.contains('failed');

      let visible = true;

      // Search filter
      if (searchLower && !lang.toLowerCase().includes(searchLower)) {
        visible = false;
      }

      // Algorithm filter
      if (algorithm && algo !== algorithm) {
        visible = false;
        // Debug first few filtered rows
        if (hiddenCount < 3) {
          console.log(`[BenchmarkTable] Hiding ${lang}: row algo="${algo}" !== filter="${algorithm}"`);
        }
      }

      // Mismatch filter
      if (!showMismatches && hasMismatch) {
        visible = false;
      }

      // Failed filter
      if (!showFailed && isFailed) {
        visible = false;
      }

      // Apply visibility
      (row as HTMLElement).style.display = visible ? '' : 'none';
      if (visible) shownCount++; else hiddenCount++;

      // Also hide expand row if main row is hidden
      const expandRow = document.getElementById(`expand-${lang}`);
      if (expandRow && !visible) {
        expandRow.style.display = 'none';
      }
    });

    console.log(`[BenchmarkTable] applyFilters result: ${shownCount} shown, ${hiddenCount} hidden`);

    // Update visible count
    this.updateVisibleCount();
  }

  private updateVisibleCount(): void {
    const table = document.getElementById('benchmark-table');
    if (!table) return;

    const visibleRows = table.querySelectorAll('tr.main-row:not([style*="display: none"])');
    console.log(`[BenchmarkTable] Showing ${visibleRows.length} rows`);
  }

  /**
   * Start interval to keep relative times updated
   */
  private startLiveUpdates(): void {
    // Clear any existing interval
    if (this.updateInterval) {
      window.clearInterval(this.updateInterval);
    }

    // Update every second for accurate timing
    this.updateInterval = window.setInterval(() => {
      this.updateRelativeTimes();
    }, 1000);

    // Also update immediately
    this.updateRelativeTimes();
  }

  /**
   * Update all relative time displays and their tooltips
   */
  private updateRelativeTimes(): void {
    const timeElements = document.querySelectorAll('.relative-time[data-ts]');
    timeElements.forEach(el => {
      const ts = parseInt(el.getAttribute('data-ts') || '0', 10);
      if (ts > 0) {
        const date = new Date(ts);
        // Update the relative time text
        el.textContent = timeAgo(date);
      }
    });
  }

  /**
   * Clean up interval when component is destroyed
   */
  destroy(): void {
    if (this.updateInterval) {
      window.clearInterval(this.updateInterval);
      this.updateInterval = null;
    }
    if (this.tooltip) {
      this.tooltip.remove();
      this.tooltip = null;
    }
  }
}

export const benchmarkTable = new BenchmarkTable();
