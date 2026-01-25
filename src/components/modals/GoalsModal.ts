// Goals Modal - explains the project goals
import { BaseModal } from './BaseModal';

export class GoalsModal extends BaseModal {
  constructor() {
    super('GM', 'Goals Modal', {
      dependencies: ['PersonalityService']
    });
  }

  protected getDomId(): string {
    return 'goalsModal';
  }

  protected render(): HTMLElement {
    const title = this.getPersonaText('projectGoals') || 'Project Goals';
    const bodyContent = this.renderGoalsContent();

    return this.createModalElement(title, bodyContent);
  }

  private renderGoalsContent(): string {
    return `
      <div class="goals-content">
        <h3>Primary Objectives</h3>
        <ol>
          <li>
            <strong>Fair Cross-Language Comparison</strong>
            <p>Implement the exact same algorithm in 88+ languages to enable apples-to-apples performance comparison without algorithmic bias.</p>
          </li>
          <li>
            <strong>Language Diversity Showcase</strong>
            <p>Demonstrate the breadth of programming paradigms - from Assembly to Haskell, from imperative to functional, from compiled to interpreted.</p>
          </li>
          <li>
            <strong>Educational Resource</strong>
            <p>Provide a comprehensive collection of sudoku solver implementations that developers can study and learn from.</p>
          </li>
          <li>
            <strong>Performance Insights</strong>
            <p>Reveal how language design choices (compilation, memory management, runtime overhead) impact real-world performance.</p>
          </li>
        </ol>

        <h3>Non-Goals</h3>
        <ul>
          <li>❌ Finding the "best" programming language (there isn't one)</li>
          <li>❌ Optimizing algorithms (all use identical brute-force)</li>
          <li>❌ Comparing different sudoku solving techniques</li>
          <li>❌ Production-grade sudoku solver (this is a benchmark)</li>
        </ul>

        <h3>Success Criteria</h3>
        <ul>
          <li>✅ Identical iteration counts across all languages (algorithm correctness)</li>
          <li>✅ Comprehensive language coverage (88+ implementations)</li>
          <li>✅ Reproducible results (Docker environment)</li>
          <li>✅ Clear performance metrics (time, memory, CPU)</li>
          <li>✅ Interactive visualization (charts, tables, modals)</li>
        </ul>
      </div>
    `;
  }

  adaptToPersona(persona: string): void {
    if (!this.isOpen || !this.container) return;

    const titleEl = this.container.querySelector('h2');
    if (titleEl) {
      titleEl.textContent = this.getPersonaText('projectGoals') || 'Project Goals';
    }
  }
}

export const goalsModal = new GoalsModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).goalsModal = goalsModal;
  (window as any).showGoals = () => goalsModal.open();
}
