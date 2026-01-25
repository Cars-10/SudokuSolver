/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_OPENAI_KEY?: string;
  // Add more env variables here
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
