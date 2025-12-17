#!/usr/bin/env node
/**
 * Database Initialization Script
 * Creates SQLite database and applies schema
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import Database from 'better-sqlite3';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const DB_PATH = path.join(__dirname, 'benchmarks.db');
const SCHEMA_PATH = path.join(__dirname, 'schema.sql');

function initDatabase() {
    console.log('🔧 Initializing Sudoku Benchmark Database...\n');

    // Check if schema file exists
    if (!fs.existsSync(SCHEMA_PATH)) {
        console.error(`❌ Error: Schema file not found at ${SCHEMA_PATH}`);
        process.exit(1);
    }

    // Check if database already exists
    const dbExists = fs.existsSync(DB_PATH);
    if (dbExists) {
        console.log(`ℹ️  Database already exists at ${DB_PATH}`);
        console.log('   Applying schema (idempotent - safe to run multiple times)\n');
    } else {
        console.log(`📁 Creating new database at ${DB_PATH}\n`);
    }

    try {
        // Create/open database
        const db = new Database(DB_PATH);

        // Enable foreign keys
        db.pragma('foreign_keys = ON');

        // Read schema file
        const schema = fs.readFileSync(SCHEMA_PATH, 'utf8');

        // Execute schema (all statements)
        console.log('📝 Executing schema...');
        db.exec(schema);

        // Verify tables were created
        const tables = db.prepare(`
            SELECT name FROM sqlite_master
            WHERE type='table' AND name NOT LIKE 'sqlite_%'
            ORDER BY name
        `).all();

        console.log(`\n✅ Database initialized successfully!`);
        console.log(`\n📊 Tables created:`);
        tables.forEach(table => {
            const count = db.prepare(`SELECT COUNT(*) as count FROM ${table.name}`).get();
            console.log(`   - ${table.name.padEnd(20)} (${count.count} rows)`);
        });

        // Verify views
        const views = db.prepare(`
            SELECT name FROM sqlite_master
            WHERE type='view'
            ORDER BY name
        `).all();

        if (views.length > 0) {
            console.log(`\n📈 Views created:`);
            views.forEach(view => {
                console.log(`   - ${view.name}`);
            });
        }

        // Verify indexes
        const indexes = db.prepare(`
            SELECT name FROM sqlite_master
            WHERE type='index' AND name NOT LIKE 'sqlite_%'
            ORDER BY name
        `).all();

        console.log(`\n🔍 Indexes created: ${indexes.length}`);

        // Display schema version
        const version = db.prepare('SELECT version, applied_at FROM schema_version ORDER BY version DESC LIMIT 1').get();
        console.log(`\n📌 Schema version: ${version.version} (applied ${version.applied_at})`);

        // Test query
        console.log(`\n🧪 Testing database connectivity...`);
        const languageCount = db.prepare('SELECT COUNT(*) as count FROM languages').get();
        console.log(`   Found ${languageCount.count} languages in Tier 1`);

        // Close database
        db.close();

        console.log(`\n✨ Database ready for use!`);
        console.log(`   Path: ${DB_PATH}`);
        console.log(`   Size: ${(fs.statSync(DB_PATH).size / 1024).toFixed(2)} KB`);

        return true;

    } catch (error) {
        console.error(`\n❌ Error initializing database:`);
        console.error(`   ${error.message}`);
        console.error(`\n   Stack trace:`);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
    initDatabase();
}

export { initDatabase };
