// Language Details Modal - displays comprehensive information about a language
import { BaseModal } from './BaseModal';
import { metricsService } from '../../services/MetricsService';
import { sourceCodeModal } from './SourceCodeModal';
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

    // Fetch metadata from API and update if needed
    this.fetchMetadata(language).then(metadata => {
      if (metadata && this.isOpen && this.currentLanguage === language) {
        // Re-render with fresh metadata
        this.refreshContent();
      }
    });
  }

  protected render(): HTMLElement {
    if (!this.currentMetrics) {
      return this.createModalElement(
        'Error',
        '<p>No data available for this language.</p>'
      );
    }

    const title = 'Language Details';
    const bodyContent = this.renderBody();

    const modal = this.createModalElement(title, bodyContent);

    // Add tier badge to header if available
    if (this.currentMetrics?.tier) {
      const header = modal.querySelector('.modal-header');
      const closeBtn = header?.querySelector('.modal-close-btn');
      if (header && closeBtn) {
        const tierBadge = document.createElement('span');
        tierBadge.className = `tier-badge tier-${this.currentMetrics.tier.toLowerCase()}`;
        tierBadge.textContent = this.currentMetrics.tier;
        tierBadge.style.cssText = 'margin-left: auto; margin-right: 12px; font-size: 1em; padding: 6px 12px;';
        header.insertBefore(tierBadge, closeBtn);
      }
    }

    return modal;
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

    // Attach handlers after rendering
    setTimeout(() => {
      this.attachSourceButtonHandler();
    }, 0);

    return body;
  }


  private refreshContent(): void {
    if (this.container) {
      const modalBody = this.container.querySelector('.modal-body');
      if (modalBody) {
        const newBody = this.renderBody();
        modalBody.innerHTML = '';
        modalBody.appendChild(newBody);
      }
    }
  }

  private renderSidebar(): HTMLElement {
    const sidebar = document.createElement('div');
    sidebar.style.width = '220px';
    sidebar.style.flexShrink = '0';
    sidebar.setAttribute('data-component-id', `${this.id}-SIDEBAR`);

    // Logo - use actual logo from metrics or fallback
    const logoPath = this.currentMetrics?.logo || `Algorithms/BruteForce/${this.currentLanguage}/Media/${this.currentLanguage}_logo.png`;
    const displayName = this.getDisplayName(this.currentLanguage);

    const logoBox = document.createElement('div');
    logoBox.className = 'language-logo-box';
    logoBox.setAttribute('data-component-id', `${this.id}-LOGO`);
    logoBox.innerHTML = `
      <div class="logo-container">
        <img src="${logoPath}" alt="${displayName}" class="modal-lang-logo"
             onerror="this.style.display='none'; this.nextElementSibling.style.display='flex';">
        <div class="logo-fallback" style="display: none;">
          <span>${this.currentLanguage.charAt(0)}</span>
        </div>
      </div>
    `;

    sidebar.appendChild(logoBox);

    // Authors section with large square images matching logo size
    const metadata = this.getLanguageMetadata();
    if (metadata?.authors && Array.isArray(metadata.authors) && metadata.authors.length > 0) {
      const authorsSection = document.createElement('div');
      authorsSection.className = 'language-authors';
      authorsSection.style.marginTop = '24px';

      // Calculate avatar size based on number of authors (larger for single, smaller for multiple)
      const avatarSize = metadata.authors.length === 1 ? 160 : (metadata.authors.length === 2 ? 100 : 85);
      const fontSize = metadata.authors.length === 1 ? 48 : (metadata.authors.length === 2 ? 32 : 24);

      authorsSection.innerHTML = `
        <h4 style="color: var(--secondary); margin: 0 0 16px 0; font-size: 0.85em; text-transform: uppercase; letter-spacing: 1px;">Creator${metadata.authors.length > 1 ? 's' : ''}</h4>
        <div class="authors-grid" style="display: flex; flex-wrap: wrap; gap: 12px; justify-content: center;">
          ${metadata.authors.map((author: any) => {
            const initials = this.getInitials(author.name);
            const avatarId = `avatar-${author.name.replace(/\s+/g, '-')}-${Date.now()}`;
            return `
            <div class="author-card" style="display: flex; flex-direction: column; align-items: center; gap: 8px;">
              <div class="author-avatar" style="position: relative; width: ${avatarSize}px; height: ${avatarSize}px; border-radius: 8px; overflow: hidden; border: 2px solid var(--border); background: var(--surface);">
                ${author.image ? `<img src="${author.image}" alt="${author.name}" style="width: 100%; height: 100%; object-fit: cover;" onerror="this.style.display='none'; document.getElementById('${avatarId}').style.display='flex';">` : ''}
                <div id="${avatarId}" class="avatar-fallback" style="display: ${author.image ? 'none' : 'flex'}; width: 100%; height: 100%; background: linear-gradient(135deg, #7aa2f7, #00ff9d); align-items: center; justify-content: center; font-weight: bold; font-size: ${fontSize}px; color: #1a1b26;">${initials}</div>
              </div>
              <span style="color: var(--text); font-size: 0.8em; text-align: center; line-height: 1.3; max-width: ${avatarSize + 20}px; word-wrap: break-word;">${author.name}</span>
            </div>
          `;}).join('')}
        </div>
      `;
      sidebar.appendChild(authorsSection);
    }

    return sidebar;
  }

  private renderMainContent(): HTMLElement {
    const content = document.createElement('div');
    content.style.flex = '1';
    content.setAttribute('data-component-id', `${this.id}-MAIN`);

    // Description
    const metadata = this.getLanguageMetadata();
    const displayName = this.getDisplayName(this.currentLanguage);

    // Always show description section
    const descSection = document.createElement('div');
    descSection.className = 'language-description';
    if (metadata?._loading) {
      descSection.innerHTML = `
        <h3>About ${displayName}</h3>
        <p style="opacity: 0.5;">Loading description...</p>
      `;
    } else if (metadata?.description) {
      descSection.innerHTML = `
        <h3>About ${displayName}</h3>
        <p>${metadata.description}</p>
        ${metadata.benefits ? `<p style="margin-top: 10px; color: var(--secondary);"><strong>Benefits:</strong> ${metadata.benefits}</p>` : ''}
      `;
    } else {
      descSection.innerHTML = `
        <h3>About ${displayName}</h3>
        <p style="color: var(--muted);">No description available.</p>
      `;
    }
    content.appendChild(descSection);

    // Metadata section (Year, Paradigm, Type System) - grid layout with labels above values
    const isLoading = metadata?._loading;
    const metaSection = document.createElement('div');
    metaSection.className = 'language-metadata-section';
    metaSection.style.cssText = 'margin-top: 20px; padding: 16px 20px; background: var(--surface); border-radius: 8px; border: 1px solid var(--border);';

    // Build metadata items array (only include non-empty values)
    const metaItems: string[] = [];

    // Year
    const year = isLoading ? '...' : (metadata?.year || null);
    if (year) {
      metaItems.push(`
        <div class="meta-item">
          <span class="meta-label">Year</span>
          <span class="meta-value" style="color: var(--primary); font-weight: 700;">${year}</span>
        </div>
      `);
    }

    // Paradigm
    const paradigm = isLoading ? '...' : (metadata?.paradigm?.join(', ') || null);
    if (paradigm) {
      metaItems.push(`
        <div class="meta-item">
          <span class="meta-label">Paradigm</span>
          <span class="meta-value">${paradigm}</span>
        </div>
      `);
    }

    // Type System
    if (metadata?.typeSystem) {
      metaItems.push(`
        <div class="meta-item">
          <span class="meta-label">Type System</span>
          <span class="meta-value">${metadata.typeSystem}</span>
        </div>
      `);
    }

    // Website
    if (metadata?.website) {
      metaItems.push(`
        <div class="meta-item">
          <span class="meta-label">Website</span>
          <a href="${metadata.website}" target="_blank" class="meta-value" style="color: var(--secondary); text-decoration: none;">Visit →</a>
        </div>
      `);
    }

    metaSection.innerHTML = `
      <div class="metadata-grid">
        ${metaItems.join('')}
      </div>
    `;
    content.appendChild(metaSection);

    // History section (if available)
    if (metadata?.history) {
      const historySection = document.createElement('div');
      historySection.className = 'language-history';
      historySection.innerHTML = `
        <h3>History</h3>
        <p>${metadata.history}</p>
        ${metadata.location ? `<p style="color: var(--muted); font-size: 0.9em; margin-top: 8px;"><em>Origin: ${metadata.location}</em></p>` : ''}
      `;
      content.appendChild(historySection);
    }

    // Quote (if available)
    if (metadata?.quote) {
      const quoteSection = document.createElement('div');
      quoteSection.className = 'language-quote';
      quoteSection.innerHTML = `<blockquote>"${metadata.quote}"</blockquote>`;
      content.appendChild(quoteSection);
    }

    // External links
    const linksSection = document.createElement('div');
    linksSection.innerHTML = this.renderExternalLinks();
    content.appendChild(linksSection);

    return content;
  }

  private getLanguageMetadata(): any {
    // Check window.languageMetadata first (legacy compatibility)
    const windowMeta = (window as any).languageMetadata?.[this.currentLanguage];

    if (windowMeta) {
      // Normalize paradigm field (could be string or array)
      let paradigm = windowMeta.paradigm;
      if (typeof paradigm === 'string') {
        paradigm = paradigm.split(',').map((s: string) => s.trim());
      } else if (!Array.isArray(paradigm)) {
        paradigm = null;
      }

      // Normalize field names for compatibility
      return {
        year: windowMeta.date || windowMeta.year,
        author: windowMeta.creator || windowMeta.author,
        paradigm,
        typeSystem: windowMeta.typeSystem,
        description: windowMeta.description,
        history: windowMeta.history,
        benefits: windowMeta.benefits,
        website: windowMeta.website,
        quote: windowMeta.quote,
        location: windowMeta.location,
        authors: windowMeta.authors,
        related: windowMeta.related
      };
    }

    // Check cached metadata from API
    const cached = (this as any)._cachedMetadata?.[this.currentLanguage];
    if (cached) {
      return cached;
    }

    // Return placeholder while fetching
    return {
      year: null,
      author: null,
      paradigm: null,
      description: null,
      history: null,
      quote: null,
      _loading: true
    };
  }

  /**
   * Fetch metadata from API
   */
  private async fetchMetadata(language: string): Promise<any> {
    try {
      const response = await fetch(`/api/metadata/${encodeURIComponent(language)}`);
      if (response.ok) {
        const metadata = await response.json();
        // Cache it
        if (!(this as any)._cachedMetadata) {
          (this as any)._cachedMetadata = {};
        }
        (this as any)._cachedMetadata[language] = metadata;
        return metadata;
      }
    } catch (err) {
      console.warn(`[LanguageDetailsModal] Failed to fetch metadata for ${language}:`, err);
    }
    return null;
  }

  private getDisplayName(lang: string): string {
    if (lang === 'C_Sharp') return 'C#';
    if (lang === 'F_Sharp') return 'F#';
    return lang;
  }

  private getInitials(name: string): string {
    if (!name) return '?';
    const parts = name.split(/\s+/).filter(p => p.length > 0);
    if (parts.length === 0) return '?';
    if (parts.length === 1) return parts[0].charAt(0).toUpperCase();
    return (parts[0].charAt(0) + parts[parts.length - 1].charAt(0)).toUpperCase();
  }

  private renderExternalLinks(): string {
    const displayName = this.getDisplayName(this.currentLanguage);
    const searchTerm = encodeURIComponent(displayName + ' programming language');

    return `
      <div class="external-links">
        <h4>Learn More</h4>
        <div class="link-buttons">
          <button id="view-source-btn" class="btn btn-link">
            View Source
          </button>
          <a href="https://en.wikipedia.org/wiki/${encodeURIComponent(displayName)}_programming_language"
             target="_blank" class="btn btn-link">
            Wikipedia
          </a>
          <a href="https://google.com/search?q=${searchTerm}"
             target="_blank" class="btn btn-link">
            Google
          </a>
          <a href="https://github.com/search?q=${encodeURIComponent(displayName + ' language')}&type=repositories"
             target="_blank" class="btn btn-link">
            GitHub
          </a>
        </div>
      </div>
    `;
  }

  private attachSourceButtonHandler(): void {
    const btn = document.getElementById('view-source-btn');
    if (btn) {
      btn.addEventListener('click', () => {
        // Get the algorithm from the current metrics if available
        const algo = this.currentMetrics?.algorithm || this.currentMetrics?.algorithmType || 'BruteForce';
        sourceCodeModal.showForLanguage(this.currentLanguage, algo);
      });
    }
  }

  /**
   * Update modal content when persona changes
   */
  adaptToPersona(persona: string): void {
    if (!this.isOpen || !this.container) return;

    // Update title
    const titleEl = this.container.querySelector('h2');
    if (titleEl) {
      titleEl.textContent = 'Language Details';
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
