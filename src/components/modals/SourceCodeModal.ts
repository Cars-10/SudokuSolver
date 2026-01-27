// Source Code Modal - displays source code for a language
import { BaseModal } from './BaseModal';

const ALGORITHMS = ['BruteForce', 'DLX', 'CP'];

export class SourceCodeModal extends BaseModal {
  private currentLanguage: string = '';
  private currentAlgorithm: string = 'BruteForce';
  private currentSourceCode: string = '';
  private currentFilename: string = '';
  private isLoading: boolean = false;
  private availableAlgorithms: string[] = [];

  constructor() {
    super('SCM', 'Source Code Modal', {
      dependencies: ['MetricsService']
    });
  }

  protected getDomId(): string {
    return 'sourceModal';
  }

  /**
   * Show source code for a specific language
   */
  showForLanguage(language: string, algorithm?: string): void {
    this.currentLanguage = language;
    this.currentAlgorithm = algorithm || 'BruteForce';

    // Show modal with loading state, then fetch
    this.isLoading = true;
    this.currentSourceCode = '// Loading...';
    this.currentFilename = '';
    this.availableAlgorithms = [];
    this.open();
    this.checkAvailableAlgorithms(language);
    this.fetchSourceCode(language, this.currentAlgorithm);
  }

  private async checkAvailableAlgorithms(language: string): Promise<void> {
    // Check which algorithms are available for this language
    const available: string[] = [];
    for (const algo of ALGORITHMS) {
      try {
        const response = await fetch(`/api/source/${encodeURIComponent(language)}?algorithm=${algo}`, {
          method: 'HEAD'
        });
        if (response.ok) {
          available.push(algo);
        }
      } catch {
        // Algorithm not available
      }
    }
    // If HEAD not supported, just try fetching and see what works
    if (available.length === 0) {
      available.push(this.currentAlgorithm);
    }
    this.availableAlgorithms = available.length > 0 ? available : [this.currentAlgorithm];
    this.updateAlgorithmDropdown();
  }

  private updateAlgorithmDropdown(): void {
    const select = this.container?.querySelector('#algo-select') as HTMLSelectElement;
    if (select) {
      select.innerHTML = this.availableAlgorithms
        .map(algo => `<option value="${algo}" ${algo === this.currentAlgorithm ? 'selected' : ''}>${algo}</option>`)
        .join('');
    }
  }

  private async fetchSourceCode(language: string, algorithm: string): Promise<void> {
    this.isLoading = true;
    this.updateLoadingState();

    try {
      const response = await fetch(`/api/source/${encodeURIComponent(language)}?algorithm=${algorithm}`);
      if (response.ok) {
        const data = await response.json();
        this.currentSourceCode = data.source || '// No source code found';
        this.currentFilename = data.filename || language;
        // Add to available if not already there
        if (!this.availableAlgorithms.includes(algorithm)) {
          this.availableAlgorithms.push(algorithm);
          this.updateAlgorithmDropdown();
        }
      } else {
        this.currentSourceCode = `// No ${algorithm} implementation found for ${language}`;
        this.currentFilename = 'Not Found';
      }
    } catch (err) {
      console.error('[SourceCodeModal] Fetch error:', err);
      this.currentSourceCode = '// Failed to load source code';
      this.currentFilename = 'Error';
    }

    this.isLoading = false;
    this.refreshContent();
  }

  private updateLoadingState(): void {
    const codeEl = this.container?.querySelector('code');
    if (codeEl) {
      codeEl.textContent = '// Loading...';
    }
  }

  private refreshContent(): void {
    if (!this.container) return;
    const codeEl = this.container.querySelector('code');
    const filenameEl = this.container.querySelector('.source-filename');
    const titleEl = this.container.querySelector('h2');

    if (codeEl) {
      codeEl.textContent = this.currentSourceCode;
    }
    if (filenameEl) {
      filenameEl.textContent = this.currentFilename;
    }
    if (titleEl) {
      titleEl.textContent = `Source: ${this.currentLanguage} (${this.currentAlgorithm})`;
    }
  }

  protected render(): HTMLElement {
    const title = `Source: ${this.currentLanguage} (${this.currentAlgorithm})`;
    const bodyContent = this.renderSourceCode();

    const modal = this.createModalElement(title, bodyContent);

    // Attach event handlers after a tick
    setTimeout(() => this.attachHandlers(), 0);

    return modal;
  }

  private attachHandlers(): void {
    // Algorithm dropdown change
    const select = this.container?.querySelector('#algo-select') as HTMLSelectElement;
    select?.addEventListener('change', (e) => {
      const newAlgo = (e.target as HTMLSelectElement).value;
      if (newAlgo !== this.currentAlgorithm) {
        this.currentAlgorithm = newAlgo;
        this.fetchSourceCode(this.currentLanguage, newAlgo);
      }
    });

    // Copy button
    const copyBtn = this.container?.querySelector('#copy-source-btn');
    copyBtn?.addEventListener('click', () => this.copyToClipboard());
  }

  private async copyToClipboard(): Promise<void> {
    try {
      await navigator.clipboard.writeText(this.currentSourceCode);
      const btn = this.container?.querySelector('#copy-source-btn');
      if (btn) {
        const originalText = btn.textContent;
        btn.textContent = '✓ Copied!';
        setTimeout(() => {
          btn.textContent = originalText;
        }, 2000);
      }
    } catch (err) {
      console.error('[SourceCodeModal] Copy failed:', err);
    }
  }

  private renderSourceCode(): HTMLElement {
    const container = document.createElement('div');
    container.className = 'source-code-container';
    container.setAttribute('data-component-id', `${this.id}-CODE`);

    // Toolbar with algorithm dropdown and copy button
    const toolbar = document.createElement('div');
    toolbar.style.cssText = 'display: flex; justify-content: space-between; align-items: center; padding: 10px 12px; background: var(--surface); border-bottom: 1px solid var(--border);';
    toolbar.innerHTML = `
      <div style="display: flex; align-items: center; gap: 12px;">
        <label style="font-size: 0.85em; color: var(--muted);">Algorithm:</label>
        <select id="algo-select" class="btn" style="padding: 4px 8px; font-size: 0.85em;">
          ${ALGORITHMS.map(algo =>
            `<option value="${algo}" ${algo === this.currentAlgorithm ? 'selected' : ''}>${algo}</option>`
          ).join('')}
        </select>
        <span class="source-filename" style="font-size: 0.85em; color: var(--secondary);">${this.currentFilename || 'Loading...'}</span>
      </div>
      <button id="copy-source-btn" class="btn" style="padding: 4px 12px; font-size: 0.85em;">
        Copy
      </button>
    `;
    container.appendChild(toolbar);

    // Code block
    const pre = document.createElement('pre');
    pre.style.cssText = 'margin: 0; padding: 15px; max-height: 500px; overflow: auto; background: var(--bg); font-size: 0.85em; line-height: 1.5;';
    const code = document.createElement('code');
    code.textContent = this.currentSourceCode;
    code.style.cssText = 'color: var(--text); white-space: pre;';
    pre.appendChild(code);
    container.appendChild(pre);

    return container;
  }
}

export const sourceCodeModal = new SourceCodeModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).sourceCodeModal = sourceCodeModal;
  (window as any).showSourceCode = (lang: string, algo?: string) => {
    sourceCodeModal.showForLanguage(lang, algo);
  };
}
