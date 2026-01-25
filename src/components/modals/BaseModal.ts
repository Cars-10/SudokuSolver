// Abstract base class for all modals
import { componentRegistry } from '../../core/ComponentRegistry';
import { eventBus, Events } from '../../core/EventBus';
import { personalityService } from '../../services/PersonalityService';
import type { ComponentMetadata } from '../../types/components';

export abstract class BaseModal {
  protected container: HTMLElement | null = null;
  protected backdrop: HTMLElement | null = null;
  protected isDragging = false;
  protected offset = { x: 0, y: 0 };
  protected isOpen = false;

  constructor(
    protected id: string,
    protected fullName: string,
    protected metadata?: Partial<ComponentMetadata>
  ) {
    // Auto-register with ComponentRegistry
    componentRegistry.register({
      id: this.id,
      fullName: this.fullName,
      type: 'modal',
      path: this.constructor.name,
      domId: this.getDomId(),
      ...metadata
    });
  }

  /**
   * Render the modal content - must be implemented by subclasses
   */
  protected abstract render(): HTMLElement;

  /**
   * Get the DOM ID for the modal - must be implemented by subclasses
   */
  protected abstract getDomId(): string;

  /**
   * Open the modal
   */
  open(): void {
    if (this.isOpen) {
      console.debug(`[${this.id}] Modal already open`);
      return;
    }

    this.container = this.render();
    this.container.setAttribute('data-component-id', this.id);
    this.container.setAttribute('aria-label', this.fullName);
    this.container.setAttribute('role', 'dialog');
    this.container.setAttribute('aria-modal', 'true');

    this.backdrop = this.createBackdrop();
    document.body.appendChild(this.backdrop);
    document.body.appendChild(this.container);

    this.makeDraggable();
    this.addKeyboardHandlers();
    this.centerModal();

    this.isOpen = true;
    eventBus.emit(Events.MODAL_OPENED, this.id);
    console.debug(`[${this.id}] Modal opened`);
  }

  /**
   * Close the modal
   */
  close(): void {
    if (!this.isOpen) return;

    this.container?.remove();
    this.backdrop?.remove();
    this.container = null;
    this.backdrop = null;

    this.isOpen = false;
    eventBus.emit(Events.MODAL_CLOSED, this.id);
    console.debug(`[${this.id}] Modal closed`);
  }

  /**
   * Toggle modal open/close
   */
  toggle(): void {
    if (this.isOpen) {
      this.close();
    } else {
      this.open();
    }
  }

  /**
   * Create backdrop overlay
   */
  private createBackdrop(): HTMLElement {
    const backdrop = document.createElement('div');
    backdrop.className = 'modal-backdrop';
    backdrop.setAttribute('data-component-id', `${this.id}-BACKDROP`);
    backdrop.addEventListener('click', () => this.close());
    return backdrop;
  }

  /**
   * Make modal draggable by header
   */
  private makeDraggable(): void {
    const header = this.container?.querySelector('.modal-header') as HTMLElement;
    if (!header) return;

    header.style.cursor = 'move';
    header.setAttribute('data-component-id', `${this.id}-HEADER`);

    const handleMouseDown = (e: MouseEvent) => {
      // Don't drag if clicking close button or other interactive elements
      const target = e.target as HTMLElement;
      if (target.tagName === 'BUTTON' || target.closest('button')) {
        return;
      }

      this.isDragging = true;
      const rect = this.container!.getBoundingClientRect();
      this.offset = {
        x: e.clientX - rect.left,
        y: e.clientY - rect.top
      };
      header.style.userSelect = 'none';
    };

    const handleMouseMove = (e: MouseEvent) => {
      if (!this.isDragging || !this.container) return;

      const x = e.clientX - this.offset.x;
      const y = e.clientY - this.offset.y;

      // Keep modal within viewport bounds
      const maxX = window.innerWidth - this.container.offsetWidth;
      const maxY = window.innerHeight - this.container.offsetHeight;

      this.container.style.left = `${Math.max(0, Math.min(x, maxX))}px`;
      this.container.style.top = `${Math.max(0, Math.min(y, maxY))}px`;
    };

    const handleMouseUp = () => {
      this.isDragging = false;
      header.style.userSelect = '';
    };

    header.addEventListener('mousedown', handleMouseDown);
    document.addEventListener('mousemove', handleMouseMove);
    document.addEventListener('mouseup', handleMouseUp);
  }

  /**
   * Add keyboard event handlers (ESC to close)
   */
  private addKeyboardHandlers(): void {
    const escHandler = (e: KeyboardEvent) => {
      if (e.key === 'Escape' && this.isOpen) {
        this.close();
      }
    };

    document.addEventListener('keydown', escHandler);

    // Store handler reference for cleanup
    (this as any)._escHandler = escHandler;
  }

  /**
   * Center modal in viewport
   */
  private centerModal(): void {
    if (!this.container) return;

    // Wait for render to get accurate dimensions
    requestAnimationFrame(() => {
      if (!this.container) return;

      const rect = this.container.getBoundingClientRect();
      const x = (window.innerWidth - rect.width) / 2;
      const y = (window.innerHeight - rect.height) / 2;

      this.container.style.left = `${Math.max(0, x)}px`;
      this.container.style.top = `${Math.max(20, y)}px`;
    });
  }

  /**
   * Get persona-specific text
   */
  protected getPersonaText(key: string): string {
    return personalityService.getLabel(key) || key;
  }

  /**
   * Called when persona changes - subclasses can override to update content
   */
  adaptToPersona(persona: string): void {
    console.debug(`[${this.id}] Adapting to persona:`, persona);
    // Default: no action, subclasses can override
  }

  /**
   * Create a close button for modal header
   */
  protected createCloseButton(): HTMLElement {
    const closeBtn = document.createElement('button');
    closeBtn.className = 'modal-close-btn';
    closeBtn.innerHTML = '&times;';
    closeBtn.setAttribute('aria-label', 'Close modal');
    closeBtn.setAttribute('data-component-id', `${this.id}-CLOSE-BTN`);
    closeBtn.addEventListener('click', () => this.close());
    return closeBtn;
  }

  /**
   * Create standard modal structure
   */
  protected createModalElement(title: string, bodyContent: string | HTMLElement): HTMLElement {
    const modal = document.createElement('div');
    modal.id = this.getDomId();
    modal.className = 'modal';

    const header = document.createElement('div');
    header.className = 'modal-header';

    const titleEl = document.createElement('h2');
    titleEl.textContent = title;
    titleEl.setAttribute('data-component-id', `${this.id}-TITLE`);

    header.appendChild(titleEl);
    header.appendChild(this.createCloseButton());

    const body = document.createElement('div');
    body.className = 'modal-body';
    body.setAttribute('data-component-id', `${this.id}-BODY`);

    if (typeof bodyContent === 'string') {
      body.innerHTML = bodyContent;
    } else {
      body.appendChild(bodyContent);
    }

    modal.appendChild(header);
    modal.appendChild(body);

    return modal;
  }

  /**
   * Cleanup method called when modal is destroyed
   */
  destroy(): void {
    this.close();
    if ((this as any)._escHandler) {
      document.removeEventListener('keydown', (this as any)._escHandler);
    }
  }
}
