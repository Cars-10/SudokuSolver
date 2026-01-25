// Component metadata and registry types

export interface ComponentMetadata {
  id: string;
  fullName: string;
  type: 'modal' | 'chart' | 'table' | 'solver' | 'effect';
  path: string;
  domId?: string;
  dependencies?: string[];
  stateKeys?: string[];
}

export interface ComponentManifest {
  version: string;
  generated: string;
  components: Record<string, ComponentMetadata>;
}

export interface PersonaData {
  id: string;
  name: string;
  intro: string;
  labels?: Record<string, string>;
  voice?: string;
  color?: string;
  icon?: string;
}

export interface LanguageMetadata {
  name: string;
  year?: number;
  author?: string;
  paradigm?: string[];
  typing?: string;
  website?: string;
  logo?: string;
  description?: string;
  quote?: string;
}
