// Why Modal - explains why this project exists
import { BaseModal } from './BaseModal';

export class WhyModal extends BaseModal {
  constructor() {
    super('WM', 'Why Modal', {
      dependencies: ['PersonalityService']
    });
  }

  protected getDomId(): string {
    return 'whyModal';
  }

  protected render(): HTMLElement {
    const title = this.getPersonaText('whyThis') || 'Why This Project?';
    const bodyContent = this.renderWhyContent();

    return this.createModalElement(title, bodyContent);
  }

  private renderWhyContent(): string {
    return `
      <div class="why-content">
        <h3>The Motivation</h3>
        <p>
          Programming language benchmarks are often criticized for comparing different algorithms
          or cherry-picking favorable use cases. This project takes a different approach:
          implement the <strong>exact same algorithm</strong> in every language to eliminate
          algorithmic bias.
        </p>

        <h3>Why Sudoku?</h3>
        <ul>
          <li>
            <strong>Simple to Understand:</strong> Everyone knows what sudoku is. The problem
            domain is accessible to all developers.
          </li>
          <li>
            <strong>Easy to Verify:</strong> The iteration count serves as an algorithmic fingerprint.
            If the counts match, you know the implementation is correct.
          </li>
          <li>
            <strong>Computationally Interesting:</strong> Backtracking with recursion exercises
            language features like function calls, array access, and control flow.
          </li>
          <li>
            <strong>Portable:</strong> Can be implemented in any language with basic data structures
            and control flow.
          </li>
        </ul>

        <h3>Why 88+ Languages?</h3>
        <p>
          Language diversity matters. By including everything from Assembly to Zig, from COBOL
          to Rust, we showcase:
        </p>
        <ul>
          <li>Historical evolution of programming languages</li>
          <li>Trade-offs between different paradigms (imperative, functional, logic)</li>
          <li>Impact of compilation strategies (AOT, JIT, interpreted)</li>
          <li>Memory management approaches (manual, GC, borrow checker)</li>
        </ul>

        <h3>What We Learn</h3>
        <p>This benchmark reveals:</p>
        <ul>
          <li>📊 Performance characteristics of different language implementations</li>
          <li>🎯 How language design choices impact runtime efficiency</li>
          <li>🔍 The overhead of abstraction layers and runtime environments</li>
          <li>⚖️ Trade-offs between developer productivity and execution speed</li>
        </ul>

        <blockquote class="language-quote">
          "The purpose of abstraction is not to be vague, but to create a new semantic level
          in which one can be absolutely precise." — Edsger W. Dijkstra
        </blockquote>
      </div>
    `;
  }

  adaptToPersona(persona: string): void {
    if (!this.isOpen || !this.container) return;

    const titleEl = this.container.querySelector('h2');
    if (titleEl) {
      titleEl.textContent = this.getPersonaText('whyThis') || 'Why This Project?';
    }
  }
}

export const whyModal = new WhyModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).whyModal = whyModal;
  (window as any).showWhy = () => whyModal.open();
}
