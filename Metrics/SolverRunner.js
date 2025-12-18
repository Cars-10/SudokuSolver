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
exports.runSolver = runSolver;
exports.runDockerSolver = runDockerSolver;
var path = __importStar(require("path"));
var util = __importStar(require("util"));
var child_process_1 = require("child_process");
var fs = __importStar(require("fs/promises"));
var execPromise = util.promisify(child_process_1.exec);
function runSolver(scriptPath, matrixPattern) {
    return __awaiter(this, void 0, void 0, function () {
        var solverDir, langName, parentDir, solverName, runType, command, cwd, matrixName, globalScript, _a, stdout, stderr, results, e_1;
        return __generator(this, function (_b) {
            switch (_b.label) {
                case 0:
                    if (scriptPath.endsWith('.sh')) {
                        solverDir = path.dirname(scriptPath);
                        langName = path.basename(solverDir);
                        parentDir = path.basename(path.dirname(solverDir));
                    }
                    else {
                        // Assume it's a directory (Languages/Lang)
                        solverDir = scriptPath;
                        langName = path.basename(solverDir);
                        parentDir = path.basename(path.dirname(solverDir)); // Should be 'Languages'
                    }
                    solverName = langName;
                    runType = 'Local';
                    console.log("Running solver: ".concat(solverName, " on ").concat(matrixPattern));
                    _b.label = 1;
                case 1:
                    _b.trys.push([1, 3, , 4]);
                    command = void 0;
                    cwd = void 0;
                    matrixName = matrixPattern.replace('.matrix', '');
                    globalScript = path.resolve(solverDir, 'runMeGlobal.sh');
                    command = "".concat(globalScript, " ").concat(langName, " ").concat(matrixName);
                    cwd = path.dirname(globalScript); // Run from Languages dir
                    return [4 /*yield*/, execPromise(command, {
                            cwd: cwd,
                            timeout: 180000 // 3 minutes
                        })];
                case 2:
                    _a = _b.sent(), stdout = _a.stdout, stderr = _a.stderr;
                    try {
                        results = JSON.parse(stdout);
                        return [2 /*return*/, {
                                solver: solverName,
                                runType: runType,
                                timestamp: new Date().toISOString(),
                                results: results
                            }];
                    }
                    catch (e) {
                        console.error("Failed to parse JSON output from ".concat(solverName, ":"), e);
                        console.log("Stdout:", stdout);
                        return [2 /*return*/, null];
                    }
                    return [3 /*break*/, 4];
                case 3:
                    e_1 = _b.sent();
                    if (e_1.killed || e_1.signal === 'SIGTERM' || e_1.code === 143) {
                        console.error("Solver ".concat(solverName, " timed out after 3 minutes."));
                        return [2 /*return*/, {
                                solver: solverName,
                                runType: runType,
                                timestamp: new Date().toISOString(),
                                results: [{
                                        matrix: matrixPattern,
                                        time: 180, // Cap at timeout
                                        iterations: 0,
                                        memory: 0,
                                        cpu_user: 0,
                                        cpu_sys: 0,
                                        status: "timeout",
                                        output: "Solver timed out after 180s"
                                    }]
                            }];
                    }
                    console.error("Failed to run solver ".concat(solverName, ":"), e_1);
                    return [2 /*return*/, null];
                case 4: return [2 /*return*/];
            }
        });
    });
}
function runDockerSolver(scriptPath, matrixPattern) {
    return __awaiter(this, void 0, void 0, function () {
        var solverDir, langName, solverName, runType, dockerFile, _a, safeLangName, imageName, repoRoot, matricesDir, matrixName, containerMatrixPath, command, _b, stdout, stderr, jsonStart, jsonEnd, jsonStr, results, e_2;
        return __generator(this, function (_c) {
            switch (_c.label) {
                case 0:
                    if (scriptPath.endsWith('.sh')) {
                        solverDir = path.dirname(scriptPath);
                        langName = path.basename(solverDir);
                    }
                    else {
                        solverDir = scriptPath;
                        langName = path.basename(solverDir);
                    }
                    solverName = langName;
                    runType = 'Docker';
                    console.log("Running solver in Docker: ".concat(solverName, " on ").concat(matrixPattern));
                    dockerFile = path.join(solverDir, 'Dockerfile');
                    _c.label = 1;
                case 1:
                    _c.trys.push([1, 3, , 4]);
                    return [4 /*yield*/, fs.access(dockerFile)];
                case 2:
                    _c.sent();
                    return [3 /*break*/, 4];
                case 3:
                    _a = _c.sent();
                    console.log("Skipping ".concat(langName, ": No Dockerfile found."));
                    return [2 /*return*/, null];
                case 4:
                    _c.trys.push([4, 7, , 8]);
                    safeLangName = langName.toLowerCase().replace(/[^a-z0-9]/g, '');
                    imageName = "sudoku-".concat(safeLangName);
                    console.log("Building Docker image: ".concat(imageName, "..."));
                    return [4 /*yield*/, execPromise("docker build -t ".concat(imageName, " ."), { cwd: solverDir })];
                case 5:
                    _c.sent();
                    repoRoot = path.resolve(solverDir, '../../../');
                    matricesDir = path.join(repoRoot, 'Matrices');
                    matrixName = matrixPattern.replace('.matrix', '');
                    containerMatrixPath = "../Matrices/".concat(matrixPattern);
                    command = "docker run --rm -v \"".concat(matricesDir, ":/usr/src/Matrices\" ").concat(imageName, " \"").concat(containerMatrixPath, "\"");
                    console.log("Executing: ".concat(command));
                    return [4 /*yield*/, execPromise(command)];
                case 6:
                    _b = _c.sent(), stdout = _b.stdout, stderr = _b.stderr;
                    try {
                        jsonStart = stdout.indexOf('[');
                        jsonEnd = stdout.lastIndexOf(']');
                        if (jsonStart === -1 || jsonEnd === -1) {
                            throw new Error("No JSON array found in output");
                        }
                        jsonStr = stdout.substring(jsonStart, jsonEnd + 1);
                        results = JSON.parse(jsonStr);
                        return [2 /*return*/, {
                                solver: solverName,
                                runType: runType,
                                timestamp: new Date().toISOString(),
                                results: results
                            }];
                    }
                    catch (e) {
                        console.error("Failed to parse JSON output from Docker ".concat(solverName, ":"), e);
                        console.log("Stdout:", stdout);
                        return [2 /*return*/, null];
                    }
                    return [3 /*break*/, 8];
                case 7:
                    e_2 = _c.sent();
                    console.error("Failed to run Docker solver ".concat(solverName, ":"), e_2);
                    return [2 /*return*/, null];
                case 8: return [2 /*return*/];
            }
        });
    });
}
