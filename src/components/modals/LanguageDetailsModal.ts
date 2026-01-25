// Language Details Modal - displays comprehensive information about a language
import { BaseModal } from './BaseModal';
import { metricsService } from '../../services/MetricsService';
import type { SolverMetrics } from '../../types/metrics';

export class LanguageDetailsModal extends BaseModal {
  private currentLanguage: string = '';
  private currentMetrics: SolverMetrics | undefined;

  constructor() {
    super('LDM', 'Language Details Modal', {
      dependencies: ['MetricsService', 'PersonalityService'],
      stateKeys: ['ui.currentPersona']
    });
  }

  protected getDomId(): string {
    return 'langModal';
  }

  /**
   * Show modal for a specific language
   */
  showForLanguage(language: string): void {
    this.currentLanguage = language;
    this.currentMetrics = metricsService.getByLanguage(language);

    if (!this.currentMetrics) {
      console.error(`[${this.id}] No metrics found for language:`, language);
      return;
    }

    this.open();
  }

  protected render(): HTMLElement {
    if (!this.currentMetrics) {
      return this.createModalElement(
        'Error',
        '<p>No data available for this language.</p>'
      );
    }

    const title = this.getPersonaText('languageDetails') + ': ' + this.currentLanguage;
    const bodyContent = this.renderBody();

    return this.createModalElement(title, bodyContent);
  }

  private renderBody(): HTMLElement {
    const body = document.createElement('div');
    body.setAttribute('data-component-id', `${this.id}-CONTENT`);

    // Main layout: sidebar + content
    const layout = document.createElement('div');
    layout.style.display = 'flex';
    layout.style.gap = '20px';

    // Sidebar (logo and metadata)
    const sidebar = this.renderSidebar();
    layout.appendChild(sidebar);

    // Main content (metrics and details)
    const mainContent = this.renderMainContent();
    layout.appendChild(mainContent);

    body.appendChild(layout);

    return body;
  }

  private renderSidebar(): HTMLElement {
    const sidebar = document.createElement('div');
    sidebar.style.width = '200px';
    sidebar.style.flexShrink = '0';
    sidebar.setAttribute('data-component-id', `${this.id}-SIDEBAR`);

    // Logo placeholder
    const logoBox = document.createElement('div');
    logoBox.className = 'language-logo-box';
    logoBox.setAttribute('data-component-id', `${this.id}-LOGO`);
    logoBox.innerHTML = `
      <div class="logo-placeholder">
        <span>${this.currentLanguage.charAt(0)}</span>
      </div>
    `;

    sidebar.appendChild(logoBox);

    // Metadata
    const metadata = this.getLanguageMetadata();
    if (metadata) {
      const metaBox = document.createElement('div');
      metaBox.className = 'language-metadata';
      metaBox.innerHTML = `
        <div class="meta-item">
          <span class="meta-label">Year:</span>
          <span class="meta-value">${metadata.year || 'Unknown'}</span>
        </div>
        <div class="meta-item">
          <span class="meta-label">Author:</span>
          <span class="meta-value">${metadata.author || 'Unknown'}</span>
        </div>
        <div class="meta-item">
          <span class="meta-label">Paradigm:</span>
          <span class="meta-value">${metadata.paradigm?.join(', ') || 'Unknown'}</span>
        </div>
      `;
      sidebar.appendChild(metaBox);
    }

    return sidebar;
  }

  private renderMainContent(): HTMLElement {
    const content = document.createElement('div');
    content.style.flex = '1';
    content.setAttribute('data-component-id', `${this.id}-MAIN`);

    // Performance metrics
    const metricsSection = this.renderMetricsSection();
    content.appendChild(metricsSection);

    // Description
    const metadata = this.getLanguageMetadata();
    if (metadata?.description) {
      const descSection = document.createElement('div');
      descSection.className = 'language-description';
      descSection.innerHTML = `
        <h3>About ${this.currentLanguage}</h3>
        <p>${metadata.description}</p>
      `;
      content.appendChild(descSection);
    }

    // Quote (if available)
    if (metadata?.quote) {
      const quoteSection = document.createElement('div');
      quoteSection.className = 'language-quote';
      quoteSection.innerHTML = `<blockquote>${metadata.quote}</blockquote>`;
      content.appendChild(quoteSection);
    }

    return content;
  }

  private renderMetricsSection(): HTMLElement {
    const section = document.createElement('div');
    section.className = 'metrics-section';
    section.setAttribute('data-component-id', `${this.id}-METRICS`);

    if (!this.currentMetrics) {
      section.innerHTML = '<p>No metrics available</p>';
      return section;
    }

    const metrics = this.currentMetrics;

    section.innerHTML = `
      <h3>Performance Metrics</h3>
      <div class="metrics-grid">
        <div class="metric-card">
          <span class="metric-label">Total Time</span>
          <span class="metric-value">${(metrics.totalTime || 0).toFixed(3)}s</span>
        </div>
        <div class="metric-card">
          <span class="metric-label">Avg Iterations</span>
          <span class="metric-value">${Math.round(metrics.avgIterations || 0).toLocaleString()}</span>
        </div>
        <div class="metric-card">
          <span class="metric-label">Avg Memory</span>
          <span class="metric-value">${(metrics.avgMemory || 0).toFixed(2)} MB</span>
        </div>
        <div class="metric-card">
          <span class="metric-label">Score</span>
          <span class="metric-value">${metrics.score ? metrics.score.toFixed(2) : 'N/A'}</span>
        </div>
      </div>

      <h4>Results by Matrix</h4>
      <table class="results-table">
        <thead>
          <tr>
            <th>Matrix</th>
            <th>Time (s)</th>
            <th>Iterations</th>
            <th>Memory (MB)</th>
            <th>Status</th>
          </tr>
        </thead>
        <tbody>
          ${metrics.results.map((result, idx) => `
            <tr>
              <td>Matrix ${idx + 1}</td>
              <td>${result.time.toFixed(3)}</td>
              <td>${result.iterations.toLocaleString()}</td>
              <td>${result.memory.toFixed(2)}</td>
              <td>
                <span class="status-badge status-${result.status.toLowerCase()}">
                  ${result.status}
                </span>
              </td>
            </tr>
          `).join('')}
        </tbody>
      </table>
    `;

    return section;
  }

  private getLanguageMetadata(): any {
    // This will be populated from window.languageMetadata injected by HTMLGenerator
    // For now, return a placeholder
    const metadata = (window as any).languageMetadata?.[this.currentLanguage];
    return metadata || {
      year: null,
      author: null,
      paradigm: null,
      description: `${this.currentLanguage} is a programming language used in this benchmark.`,
      quote: null
    };
  }

  /**
   * Update modal content when persona changes
   */
  adaptToPersona(persona: string): void {
    if (!this.isOpen || !this.container) return;

    // Update title
    const titleEl = this.container.querySelector('h2');
    if (titleEl) {
      titleEl.textContent = this.getPersonaText('languageDetails') + ': ' + this.currentLanguage;
    }

    console.debug(`[${this.id}] Adapted to persona: ${persona}`);
  }
}

// Create singleton instance
export const languageDetailsModal = new LanguageDetailsModal();

// Make available globally for compatibility with existing code
if (typeof window !== 'undefined') {
  (window as any).languageDetailsModal = languageDetailsModal;
  (window as any).showLangModal = (lang: string) => {
    languageDetailsModal.showForLanguage(lang);
  };
}
