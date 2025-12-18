"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
Object.defineProperty(exports, "__esModule", { value: true });
var fs = __importStar(require("fs"));
var path = __importStar(require("path"));
var url_1 = require("url");
var glob_1 = require("glob");
var gather_metrics_ts_1 = require("./gather_metrics.ts");
var __filename = (0, url_1.fileURLToPath)(import.meta.url);
var __dirname = path.dirname(__filename);
var rootDir = path.resolve(__dirname, '..');
var outputDir = process.env.OUTPUT_DIR || rootDir;
var metricsFile = path.join(outputDir, process.env.METRICS_FILE || 'metrics.json');
var htmlFile = path.join(rootDir, '_report.html');
function run() {
    return __awaiter(this, void 0, void 0, function () {
        var historyMgr, history_1, mainMetrics, data, e_1, metricsPattern, metricFiles, aggregatedMetrics, knownSolvers, _i, metricFiles_1, file, content, parsedMetrics, metricsList, pathParts, langIndex, solverName, stats, runType, runTypePath, rt, e_2, _a, metricsList_1, m, e_3, _b, mainMetrics_1, m, metadataOverrides, metadataPath, allMetrics, allowedMatrices, benchmarkConfig, configPath, configData, e_4, metricsMap, finalMetrics, _c, orderedLanguages_1, lang, _d, allMetrics_1, m, htmlContent, historyHtml, historyHtmlFile, timestampFile, now, formatter, parts_1, getPart, timestamp, screenshotsDir, screenshotPath, e_5;
        return __generator(this, function (_e) {
            switch (_e.label) {
                case 0:
                    _e.trys.push([0, 34, , 35]);
                    historyMgr = new gather_metrics_ts_1.HistoryManager(rootDir);
                    return [4 /*yield*/, historyMgr.getHistory()];
                case 1:
                    history_1 = _e.sent();
                    console.log("Loaded ".concat(history_1.length, " historical records."));
                    mainMetrics = [];
                    _e.label = 2;
                case 2:
                    _e.trys.push([2, 4, , 5]);
                    return [4 /*yield*/, fs.promises.readFile(metricsFile, 'utf-8')];
                case 3:
                    data = _e.sent();
                    mainMetrics = JSON.parse(data);
                    return [3 /*break*/, 5];
                case 4:
                    e_1 = _e.sent();
                    console.warn("Could not read main metrics.json", e_1);
                    return [3 /*break*/, 5];
                case 5:
                    metricsPattern = path.join(rootDir, 'Languages', '*', 'metrics.json');
                    return [4 /*yield*/, (0, glob_1.glob)(metricsPattern)];
                case 6:
                    metricFiles = _e.sent();
                    console.log("Found ".concat(metricFiles.length, " additional metric files."));
                    aggregatedMetrics = [];
                    knownSolvers = new Set();
                    _i = 0, metricFiles_1 = metricFiles;
                    _e.label = 7;
                case 7:
                    if (!(_i < metricFiles_1.length)) return [3 /*break*/, 21];
                    file = metricFiles_1[_i];
                    _e.label = 8;
                case 8:
                    _e.trys.push([8, 19, , 20]);
                    return [4 /*yield*/, fs.promises.readFile(file, 'utf-8')];
                case 9:
                    content = _e.sent();
                    parsedMetrics = JSON.parse(content);
                    metricsList = [];
                    if (!Array.isArray(parsedMetrics)) return [3 /*break*/, 17];
                    if (!(parsedMetrics.length > 0 && parsedMetrics[0].results)) return [3 /*break*/, 10];
                    // Already wrapped format - use directly
                    metricsList = parsedMetrics;
                    return [3 /*break*/, 16];
                case 10:
                    pathParts = file.split(path.sep);
                    langIndex = pathParts.lastIndexOf('Languages');
                    solverName = "Unknown";
                    if (langIndex !== -1 && pathParts[langIndex + 1]) {
                        solverName = pathParts[langIndex + 1];
                    }
                    return [4 /*yield*/, fs.promises.stat(file)];
                case 11:
                    stats = _e.sent();
                    runType = 'Local';
                    _e.label = 12;
                case 12:
                    _e.trys.push([12, 14, , 15]);
                    runTypePath = path.join(path.dirname(file), 'run_type');
                    return [4 /*yield*/, fs.promises.readFile(runTypePath, 'utf-8')];
                case 13:
                    rt = _e.sent();
                    runType = rt.trim();
                    return [3 /*break*/, 15];
                case 14:
                    e_2 = _e.sent();
                    return [3 /*break*/, 15];
                case 15:
                    metricsList.push({
                        solver: solverName,
                        timestamp: stats.mtimeMs, // Use file modification time
                        runType: runType,
                        results: parsedMetrics
                    });
                    _e.label = 16;
                case 16: return [3 /*break*/, 18];
                case 17:
                    metricsList = [parsedMetrics];
                    _e.label = 18;
                case 18:
                    for (_a = 0, metricsList_1 = metricsList; _a < metricsList_1.length; _a++) {
                        m = metricsList_1[_a];
                        if (!knownSolvers.has(m.solver)) {
                            aggregatedMetrics.push(m);
                            knownSolvers.add(m.solver);
                        }
                    }
                    return [3 /*break*/, 20];
                case 19:
                    e_3 = _e.sent();
                    console.warn("Error reading ".concat(file, ":"), e_3);
                    return [3 /*break*/, 20];
                case 20:
                    _i++;
                    return [3 /*break*/, 7];
                case 21:
                    // 2. Load Root Metrics (Lower Priority - Backfill)
                    if (mainMetrics.length > 0) {
                        console.log("Backfilling with root metrics (".concat(mainMetrics.length, " found)..."));
                        for (_b = 0, mainMetrics_1 = mainMetrics; _b < mainMetrics_1.length; _b++) {
                            m = mainMetrics_1[_b];
                            if (!knownSolvers.has(m.solver)) {
                                console.log("Restoring legacy metric for: ".concat(m.solver));
                                aggregatedMetrics.push(m);
                                knownSolvers.add(m.solver);
                            }
                            else {
                                // console.log(`Skipping stale root metric for: ${m.solver}`);
                            }
                        }
                    }
                    metadataOverrides = {};
                    metadataPath = path.join(__dirname, '../Languages/metadata.json');
                    // fs.existsSync is synchronous, but this is fine for a startup config read
                    if (fs.existsSync(metadataPath)) {
                        console.log("Loading metadata overrides from " + metadataPath);
                        try {
                            // fs.readFileSync is synchronous, but this is fine for a startup config read
                            metadataOverrides = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
                        }
                        catch (e) {
                            console.error("Failed to load metadata overrides: " + e);
                        }
                    }
                    allMetrics = aggregatedMetrics;
                    console.log("Loaded ".concat(allMetrics.length, " total metrics."));
                    allowedMatrices = ['1.matrix', '2.matrix', '3.matrix', '4.matrix', '5.matrix', '6.matrix'];
                    console.log("Filtering report to standard suite: ".concat(allowedMatrices.join(', ')));
                    benchmarkConfig = {};
                    _e.label = 22;
                case 22:
                    _e.trys.push([22, 24, , 25]);
                    configPath = path.join(rootDir, 'benchmark_config.json');
                    return [4 /*yield*/, fs.promises.readFile(configPath, 'utf-8')];
                case 23:
                    configData = _e.sent();
                    benchmarkConfig = JSON.parse(configData);
                    console.log("Loaded benchmark config with ".concat(Object.keys(benchmarkConfig.languages || {}).length, " language configurations."));
                    return [3 /*break*/, 25];
                case 24:
                    e_4 = _e.sent();
                    console.warn("Could not read benchmark_config.json, using defaults");
                    return [3 /*break*/, 25];
                case 25:
                    metricsMap = new Map(allMetrics.map(function (m) { return [m.solver, m]; }));
                    finalMetrics = [];
                    for (_c = 0, orderedLanguages_1 = gather_metrics_ts_1.orderedLanguages; _c < orderedLanguages_1.length; _c++) {
                        lang = orderedLanguages_1[_c];
                        if (metricsMap.has(lang)) {
                            finalMetrics.push(metricsMap.get(lang));
                        }
                        else {
                            // Create Placeholder for Missing Language
                            finalMetrics.push({
                                solver: lang,
                                runType: 'Local',
                                timestamp: Date.now(),
                                results: [] // No results -> HTMLGenerator will render as Init/Empty
                            });
                        }
                    }
                    // Add any discovered languages NOT in the master list at the end
                    for (_d = 0, allMetrics_1 = allMetrics; _d < allMetrics_1.length; _d++) {
                        m = allMetrics_1[_d];
                        if (!gather_metrics_ts_1.orderedLanguages.includes(m.solver)) {
                            finalMetrics.push(m);
                        }
                    }
                    console.log("Final Report contains ".concat(finalMetrics.length, " languages."));
                    return [4 /*yield*/, (0, gather_metrics_ts_1.generateHtml)(finalMetrics, history_1, gather_metrics_ts_1.personalities, gather_metrics_ts_1.languageMetadata, gather_metrics_ts_1.methodologyTexts, {}, allowedMatrices, benchmarkConfig, metadataOverrides)];
                case 26:
                    htmlContent = _e.sent();
                    return [4 /*yield*/, fs.promises.writeFile(htmlFile, htmlContent)];
                case 27:
                    _e.sent();
                    return [4 /*yield*/, (0, gather_metrics_ts_1.generateHistoryHtml)(history_1)];
                case 28:
                    historyHtml = _e.sent();
                    historyHtmlFile = path.join(rootDir, 'benchmark_history.html');
                    return [4 /*yield*/, fs.promises.writeFile(historyHtmlFile, historyHtml)];
                case 29:
                    _e.sent();
                    console.log("Successfully generated ".concat(historyHtmlFile));
                    timestampFile = path.join(rootDir, 'timestamp.js');
                    return [4 /*yield*/, fs.promises.writeFile(timestampFile, "window.latestTimestamp = ".concat(Date.now(), ";"))];
                case 30:
                    _e.sent();
                    console.log("Successfully generated ".concat(htmlFile));
                    now = new Date();
                    formatter = new Intl.DateTimeFormat('en-SE', {
                        timeZone: 'CET',
                        year: 'numeric',
                        month: '2-digit',
                        day: '2-digit',
                        hour: '2-digit',
                        minute: '2-digit',
                        second: '2-digit',
                        hour12: false
                    });
                    parts_1 = formatter.formatToParts(now);
                    getPart = function (type) { var _a; return (_a = parts_1.find(function (p) { return p.type === type; })) === null || _a === void 0 ? void 0 : _a.value; };
                    timestamp = "".concat(getPart('year'), "-").concat(getPart('month'), "-").concat(getPart('day'), "_").concat(getPart('hour'), "-").concat(getPart('minute'), "-").concat(getPart('second'), "_CET");
                    screenshotsDir = path.join(rootDir, 'screenshots');
                    if (!!fs.existsSync(screenshotsDir)) return [3 /*break*/, 32];
                    return [4 /*yield*/, fs.promises.mkdir(screenshotsDir, { recursive: true })];
                case 31:
                    _e.sent();
                    _e.label = 32;
                case 32:
                    screenshotPath = path.join(screenshotsDir, "benchmark_report_".concat(timestamp, ".png"));
                    return [4 /*yield*/, (0, gather_metrics_ts_1.captureScreenshot)(htmlFile, screenshotPath)];
                case 33:
                    _e.sent();
                    console.log("Screenshot captured for ".concat(htmlFile, " at ").concat(screenshotPath));
                    return [3 /*break*/, 35];
                case 34:
                    e_5 = _e.sent();
                    console.error("Error:", e_5);
                    return [3 /*break*/, 35];
                case 35: return [2 /*return*/];
            }
        });
    });
}
run();
