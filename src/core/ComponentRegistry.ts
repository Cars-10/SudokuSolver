// Component catalog for debugging and manifest generation
import type { ComponentMetadata, ComponentManifest } from '../types/components';

class ComponentRegistry {
  private components: Map<string, ComponentMetadata> = new Map();

  register(metadata: ComponentMetadata): void {
    if (this.components.has(metadata.id)) {
      console.warn(`[ComponentRegistry] Component "${metadata.id}" already registered, overwriting`);
    }

    this.components.set(metadata.id, metadata);

    if (import.meta.env.DEV) {
      console.debug('[ComponentRegistry] Registered:', metadata.id, metadata.fullName);
    }
  }

  find(id: string): ComponentMetadata | undefined {
    return this.components.get(id);
  }

  findByDomId(domId: string): ComponentMetadata | undefined {
    return Array.from(this.components.values())
      .find(c => c.domId === domId);
  }

  listByType(type: ComponentMetadata['type']): ComponentMetadata[] {
    return Array.from(this.components.values())
      .filter(c => c.type === type);
  }

  getDependents(dependency: string): ComponentMetadata[] {
    return Array.from(this.components.values())
      .filter(c => c.dependencies?.includes(dependency));
  }

  list(): ComponentMetadata[] {
    return Array.from(this.components.values());
  }

  export(): ComponentManifest {
    const manifest: ComponentManifest = {
      version: '2.0.0',
      generated: new Date().toISOString(),
      components: Object.fromEntries(this.components)
    };
    return manifest;
  }

  exportJSON(): string {
    return JSON.stringify(this.export(), null, 2);
  }

  // Generate summary report
  summary(): string {
    const byType = new Map<string, number>();
    this.components.forEach(c => {
      byType.set(c.type, (byType.get(c.type) || 0) + 1);
    });

    const lines = [
      `Component Registry Summary`,
      `Total: ${this.components.size} components`,
      ``,
      `By Type:`,
      ...Array.from(byType.entries()).map(([type, count]) =>
        `  ${type}: ${count}`
      )
    ];

    return lines.join('\n');
  }
}

export const componentRegistry = new ComponentRegistry();

// Decorator for auto-registration
export function Component(metadata: Omit<ComponentMetadata, 'path'>) {
  return function<T extends { new(...args: any[]): {} }>(constructor: T) {
    const path = new Error().stack?.split('\n')[2]?.match(/\((.*?):\d+:\d+\)/)?.[1] || metadata.id;
    componentRegistry.register({ ...metadata, path });
    return constructor;
  };
}

// Debug helper
if (import.meta.env.DEV) {
  (window as any).componentRegistry = componentRegistry;
}
