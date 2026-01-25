// Source Code Modal - displays source code for a language
import { BaseModal } from './BaseModal';

export class SourceCodeModal extends BaseModal {
  private currentLanguage: string = '';
  private currentSourceCode: string = '';

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
  showForLanguage(language: string, sourceCode?: string): void {
    this.currentLanguage = language;
    this.currentSourceCode = sourceCode || '// Source code not available';
    this.open();
  }

  protected render(): HTMLElement {
    const title = `Source Code: ${this.currentLanguage}`;
    const bodyContent = this.renderSourceCode();

    return this.createModalElement(title, bodyContent);
  }

  private renderSourceCode(): HTMLElement {
    const container = document.createElement('div');
    container.className = 'source-code-container';
    container.setAttribute('data-component-id', `${this.id}-CODE`);

    const pre = document.createElement('pre');
    const code = document.createElement('code');
    code.textContent = this.currentSourceCode;
    pre.appendChild(code);
    container.appendChild(pre);

    return container;
  }
}

export const sourceCodeModal = new SourceCodeModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).sourceCodeModal = sourceCodeModal;
  (window as any).showSourceCode = (lang: string, code?: string) => {
    sourceCodeModal.showForLanguage(lang, code);
  };
}
