// Migrated from Metrics/modules/personality.js
import { eventBus, Events } from '../core/EventBus';
import { appState } from '../core/StateManager';
import type { PersonaData } from '../types/components';

export class PersonalityService {
  private personas: Map<string, PersonaData>;

  constructor(personas: PersonaData[]) {
    this.personas = new Map(personas.map(p => [p.id, p]));
  }

  change(personaId: string): void {
    if (!this.personas.has(personaId)) {
      console.error('[PersonalityService] Unknown persona:', personaId);
      return;
    }

    appState.setSlice('ui', {
      ...appState.getSlice('ui'),
      currentPersona: personaId
    });

    eventBus.emit(Events.PERSONA_CHANGED, personaId);
    console.debug('[PersonalityService] Persona changed:', personaId);
  }

  getCurrent(): PersonaData {
    const id = appState.getSlice('ui').currentPersona;
    return this.personas.get(id) || this.personas.get('standard')!;
  }

  getById(id: string): PersonaData | undefined {
    return this.personas.get(id);
  }

  list(): PersonaData[] {
    return Array.from(this.personas.values());
  }

  getLabel(key: string): string {
    const persona = this.getCurrent();
    return persona.labels?.[key] || key;
  }

  transformText(text: string, context?: string): string {
    const persona = this.getCurrent();

    // If persona has a voice template, apply it
    if (persona.voice) {
      // Simple template replacement for now
      // Can be enhanced with more sophisticated transformations
      return text;
    }

    return text;
  }
}

// Initialize with empty array - will be populated from window after DOM loads
export const personalityService = new PersonalityService([
  {
    id: 'standard',
    name: 'Standard',
    intro: 'Clear and professional analysis',
    labels: {}
  }
]);

// Populate from window.personalities if available
if (typeof window !== 'undefined') {
  window.addEventListener('DOMContentLoaded', () => {
    const personalities = (window as any).personalities;
    if (personalities && Array.isArray(personalities)) {
      (personalityService as any).personas = new Map(
        personalities.map((p: PersonaData) => [p.id, p])
      );
      console.debug('[PersonalityService] Loaded', personalities.length, 'personas');
    }
  });
}
