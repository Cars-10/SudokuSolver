export const SharedStyles = `
        /* Pill UI */
        .pill-container {
            display: flex;
            align-items: center;
        }

        .pill-combined {
            display: flex;
            width: 70px;
            height: 24px;
            border-radius: 12px;
            overflow: hidden;
            border: 1px solid rgba(255, 255, 255, 0.3);
            backdrop-filter: blur(6px);
            background: rgba(255, 255, 255, 0.1);
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3), inset 0 1px 0 rgba(255, 255, 255, 0.2);
            transition: transform 0.2s ease;
        }

        .zoom-btn {
            background: transparent;
            border: none;
            color: var(--secondary);
            cursor: pointer;
            padding: 0;
            display: flex;
            align-items: center;
            justify-content: center;
            opacity: 0.7;
            transition: opacity 0.2s, color 0.2s;
        }
        .zoom-btn:hover {
            opacity: 1;
            color: var(--primary);
        }

        .pill-combined:hover {
            transform: scale(1.05);
        }

        .pill-half {
            flex: 1;
            height: 100%;
            cursor: pointer;
            transition: background 0.2s ease, box-shadow 0.2s ease;
            position: relative;
        }

        .pill-half.blue {
            background: linear-gradient(135deg, rgba(50, 50, 255, 0.4), rgba(0, 0, 180, 0.6));
            box-shadow: inset 2px 2px 4px rgba(255, 255, 255, 0.2);
        }

        .pill-half.blue:hover {
            background: linear-gradient(135deg, rgba(80, 80, 255, 0.6), rgba(20, 20, 200, 0.8));
            box-shadow: inset 2px 2px 6px rgba(255, 255, 255, 0.4);
        }

        .pill-half.red {
            background: linear-gradient(135deg, rgba(255, 50, 50, 0.4), rgba(180, 0, 0, 0.6));
            box-shadow: inset -2px -2px 4px rgba(0, 0, 0, 0.2); /* Subtle shadow on other side */
        }

        .pill-half.red:hover {
            background: linear-gradient(135deg, rgba(255, 80, 80, 0.6), rgba(200, 20, 20, 0.8));
            box-shadow: inset 2px 2px 6px rgba(255, 255, 255, 0.4);
        }

        /* Matrix Screensaver */
        #matrix-canvas {
            position: fixed; /* Changed to fixed for full screen overlay */
            top: 0;
            left: 0;
            width: 100vw;
            height: 100vh;
            z-index: 9999; /* Very high z-index */
            display: none; /* Hidden by default */
            pointer-events: auto; /* Capture clicks to exit */
        }

        /* Animations */
        @keyframes slideDownEnter {
            from { top: -100vh; }
            to { top: 0; }
        }

        @keyframes slideDownExit {
            from { transform: translateY(0); opacity: 1; }
            to { transform: translateY(100vh); opacity: 0; }
        }

        @keyframes slideUpEnter {
            from { transform: translateY(100vh); opacity: 0; }
            to { transform: translateY(0); opacity: 1; }
        }

        .slide-down-enter {
            animation: slideDownEnter 1.5s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }

        .slide-down-exit {
            animation: slideDownExit 1.5s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }
        
        .slide-up-enter {
             animation: slideUpEnter 1.0s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }

        /* Main content wrapper for sliding */
        .content-wrapper {
            transition: transform 1.5s cubic-bezier(0.25, 1, 0.5, 1), opacity 1.5s ease;
        }

        /* Cell Popup Menu */
        .popup-menu {
            display: none;
            position: absolute;
            background: var(--surface);
            border: 1px solid var(--primary);
            box-shadow: 0 0 15px rgba(0, 255, 157, 0.2);
            z-index: 10000;
            min-width: 150px;
            border-radius: 4px;
            padding: 5px 0;
        }
        .popup-item {
            padding: 8px 15px;
            cursor: pointer;
            color: var(--text);
            font-size: 0.9em;
            transition: background 0.2s;
        }
        .popup-item:hover {
            background: rgba(0, 255, 157, 0.1);
            color: var(--primary);
        }

        /* Dropdown Menu */
        .dropdown {
            position: relative;
            display: inline-block;
        }
        
        .dropdown-content {
            display: none;
            position: absolute;
            background-color: var(--surface); /* Fallback */
            background: rgba(22, 22, 30, 0.95);
            backdrop-filter: blur(10px);
            min-width: 160px;
            box-shadow: 0 8px 16px 0 rgba(0,0,0,0.5);
            z-index: 1000;
            border: 1px solid var(--border);
            border-radius: 4px;
            padding: 5px 0;
            top: 100%;
            left: 0;
        }
        
        .dropdown:hover .dropdown-content {
            display: block;
        }
        
        .dropdown-content a {
            color: var(--text);
            padding: 10px 15px;
            text-decoration: none;
            display: block;
            font-size: 0.9em;
            cursor: pointer;
            transition: background 0.2s, color 0.2s;
        }
        
        .dropdown-content a:hover {
            background-color: rgba(0, 255, 157, 0.1);
            color: var(--primary);
        }

        /* Log Modal */
        #logModal {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.85);
            justify-content: center;
            align-items: center;
            z-index: 10001;
        }
        
        #logModal .modal-content {
            background: #1e1e24;
            width: 80%;
            max-width: 900px;
            height: 80%;
            border-radius: 8px;
            border: 1px solid #444;
            display: flex;
            flex-direction: column;
            padding: 0;
            overflow: hidden;
            box-shadow: 0 10px 30px rgba(0,0,0,0.5);
        }
        
        #logHeader {
            padding: 15px;
            background: #2a2a35;
            border-bottom: 1px solid #444;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        
        #logOutput {
            flex: 1;
            padding: 15px;
            overflow: auto;
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            color: #d0d0d0;
            white-space: pre-wrap;
            background: #16161a;
        }

        /* ========================================
           MODAL SYSTEM - FLATTENED STRUCTURE
           ======================================== */

        /* Modal Backdrop - Transparent (no dark overlay) */
        .modal {
            display: none;
            position: fixed;
            z-index: 10000;
            left: 0;
            top: 0;
            width: 100vw;
            height: 100vh;
            background: transparent;
            pointer-events: none;
            overflow: hidden;
        }

        .modal.visible {
            display: block !important;
        }

        /* Modal Overlay for non-draggable modals */
        .modal-overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.85);
            display: none;
            align-items: center;
            justify-content: center;
            z-index: 10000;
            backdrop-filter: blur(8px);
            pointer-events: auto;
        }

        .modal-overlay.visible {
            display: flex !important;
        }

        /* Modal Content Container - Fixed Width for Consistency */
        .modal .modal-content {
            /* Box Model - Fixed sizing for width consistency */
            box-sizing: border-box;
            width: 800px;
            max-width: 90vw;
            max-height: 90vh;

            /* Layout - Simple flexbox, no nesting issues */
            display: flex;
            flex-direction: column;
            position: absolute;

            /* Styling */
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
            border: 2px solid #7aa2f7;
            border-radius: 12px;
            box-shadow: 0 8px 24px rgba(0, 0, 0, 0.5);

            /* Interaction */
            cursor: default;
            pointer-events: auto;
        }

        /* Centered positioning (fallback) */
        .modal.centered .modal-content {
            left: 50%;
            top: 50%;
            transform: translate(-50%, -50%);
        }

        /* Modal Animations */
        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }

        @keyframes slideUp {
            from {
                transform: translateY(50px);
                opacity: 0;
            }
            to {
                transform: translateY(0);
                opacity: 1;
            }
        }

        /* Prevent body scroll when modal open */
        body.modal-open {
            overflow: hidden;
        }

        /* Close button styling */
        .modal-close {
            position: absolute;
            right: 15px;
            top: 15px;
            font-size: 28px;
            font-weight: bold;
            color: #9aa5ce;
            cursor: pointer;
            z-index: 10;
            transition: color 0.2s, transform 0.2s;
            line-height: 1;
            width: 32px;
            height: 32px;
            display: flex;
            align-items: center;
            justify-content: center;
            border-radius: 4px;
        }

        .modal-close:hover {
            color: #f7768e;
            background: rgba(247, 118, 142, 0.1);
            transform: scale(1.1);
        }

        /* Modal Header - Flattened, Direct Child */
        .modal-header {
            /* Box Model - Guaranteed width match */
            box-sizing: border-box;
            width: 100%;
            padding: 20px;
            margin: 0;

            /* Layout - No flex grow/shrink */
            flex: 0 0 auto;
            display: flex;
            flex-direction: column;
            gap: 15px;

            /* Styling */
            border-bottom: 1px solid #414868;
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
            cursor: move;
        }

        /* Modal Header Top Row (logo + info) */
        .modal-header-top {
            display: flex;
            gap: 20px;
            align-items: flex-start;
        }

        /* Modal Header Bottom Row (buttons) */
        .modal-header-buttons {
            display: flex;
            gap: 10px;
            justify-content: flex-end;
        }

        /* Modal Body - Flattened, Direct Child, NO SCROLLBAR */
        .modal-body {
            /* Box Model - Guaranteed width match */
            box-sizing: border-box;
            width: 100%;
            padding: 20px;
            margin: 0;

            /* Layout - Auto height, no scrollbar */
            flex: 0 1 auto;
            overflow: visible;
            display: flex;
            flex-direction: column;
            gap: 20px;

            /* No scrollbar needed */
            cursor: auto;
        }

        /* Modal footer */
        .modal-footer {
            padding: 15px 20px;
            border-top: 1px solid #414868;
            background: #1a1b26;
            display: flex;
            justify-content: flex-end;
            gap: 10px;
        }

        /* Edit Mode Toggle */
        .edit-only {
            display: none !important;
        }

        .editing .edit-only {
            display: block !important;
        }

        .editing .view-only {
            display: none !important;
        }

        /* Modal Edit Inputs */
        .modal-edit-input,
        .modal-edit-textarea {
            width: 100%;
            background: #24283b;
            border: 1px solid #414868;
            border-radius: 4px;
            padding: 8px 12px;
            color: #c0caf5;
            font-size: 14px;
            font-family: inherit;
            margin-bottom: 10px;
        }

        .modal-edit-textarea {
            min-height: 100px;
            resize: vertical;
        }

        .modal-edit-input:focus,
        .modal-edit-textarea:focus {
            outline: none;
            border-color: #7aa2f7;
            background: #1a1b26;
        }

        /* Sidebar Box Styling (Logo & Authors) */
        .sidebar-box {
            display: flex;
            flex-direction: column;
            align-items: center;
            padding: 10px;
            background: rgba(0, 0, 0, 0.2);
            border: 1px solid #414868;
            border-radius: 12px;
            width: 170px; /* 150px image + 2*10px padding */
            box-sizing: border-box;
            gap: 10px;
            flex-shrink: 0;
        }

        .sidebar-img-container {
            width: 150px;
            height: 150px;
            position: relative;
            border-radius: 8px;
            overflow: hidden;
            background: rgba(0, 0, 0, 0.3);
            flex-shrink: 0;
        }

        .sidebar-img {
            width: 100%;
            height: 100%;
            object-fit: contain; /* Default for logos */
            border-radius: 4px;
        }

        .author-img {
            object-fit: cover; /* Authors look better covered/scaled */
        }

        .sidebar-box span {
            color: #c0caf5;
            font-weight: 500;
            text-align: center;
            font-size: 0.9em;
            word-break: break-word;
            width: 100%;
        }

        /* Author List Styling */
        .author-list {
            display: flex;
            flex-direction: column;
            gap: 15px;
            width: 100%;
            align-items: center;
        }

        .author-item {
            /* Inherits from .sidebar-box */
        }

        .author-item span {
            color: #c0caf5;
            font-weight: 500;
            text-align: center;
            font-size: 0.9em;
            word-break: break-word;
        }
        
        .author-item .modal-edit-input {
            font-size: 0.8em;
            padding: 4px;
            margin-bottom: 4px;
            text-align: center;
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
            .modal .modal-content {
                width: 95%;
                max-height: 95vh;
            }

            .modal-header {
                padding: 15px;
            }

            .modal-body {
                padding: 15px;
            }
        }

        /* Logo Fallback Styling */
        .lang-logo {
            width: 40px;
            height: 40px;
            object-fit: contain;
            background: rgba(122, 162, 247, 0.1);
            border-radius: 6px;
            padding: 4px;
            image-rendering: -webkit-optimize-contrast;
            image-rendering: crisp-edges;
            vertical-align: middle;
            margin-right: 8px;
        }

        /* Tooltip Styling */
        #tooltip {
            position: fixed;
            display: none;
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
            border: 1px solid #00ff9d;
            border-radius: 8px;
            padding: 12px 16px;
            color: #c0caf5;
            font-size: 14px;
            line-height: 1.6;
            box-shadow: 0 0 15px rgba(0, 255, 157, 0.2), 0 8px 32px rgba(0, 0, 0, 0.6);
            z-index: 9999;
            pointer-events: none;
            max-width: 300px;
            backdrop-filter: blur(8px);
            -webkit-backdrop-filter: blur(8px);
        }

        .run-btn {
            background: transparent;
            border: 1px solid rgba(255,255,255,0.1);
            color: #00ff9d;
            cursor: pointer;
            font-size: 10px;
            padding: 2px 6px;
            border-radius: 4px;
            margin-right: 5px;
            opacity: 0.6;
            transition: all 0.2s;
        }
        .run-btn:hover {
            opacity: 1;
            background: rgba(0, 255, 157, 0.1);
            transform: scale(1.1);
        }

        /* Matrix Cell Content Layout */
        .cell-content {
            display: flex;
            flex-direction: column;
            justify-content: center;
            height: 100%;
            position: relative;
        }
        
        /* Make Matrix Cell Relative for absolute positioning of play button if needed, 
           or use flex row for header */
        .cell-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 2px;
        }
        /* Fullscreen Wrapper Centering */
        #chart-wrapper.fullscreen-active {
            background: #000 !important;
            border: none !important;
            padding: 0 !important;
            width: 100% !important;
            height: 100% !important;
            display: flex !important;
            justify-content: center !important;
            align-items: center !important;
        }
        
        /* Node Label Toggling */
        .chart-node-label { display: none; fill: #e0e0e0; font-size: 10px; font-weight: normal; text-shadow: 0 0 2px #000; }
        .chart-label-end { display: block; } /* Always show end labels */
        .show-text-labels .chart-node-label { display: block; } /* Show intermediate on toggle */
        .show-text-labels .chart-node-image { display: none; }
        
        /* All columns equal width */
        .lang-col, .score-col, .matrix-cell, .total-time {
            overflow: hidden;
            white-space: nowrap;
            text-overflow: ellipsis;
        }

        /* Hide Chart Buttons (using Pulldown now) */
        .chart-options { display: none !important; }

        /* ========================================
           SCORING ANALYSIS UI COMPONENTS
           ======================================== */

        /* Score Decomposition - Mini Stacked Bar */
        .score-decomposition {
            display: inline-flex;
            align-items: center;
            gap: 8px;
            cursor: help;
        }

        .stacked-bar {
            display: flex;
            width: 60px;
            height: 16px;
            border-radius: 3px;
            overflow: hidden;
            border: 1px solid rgba(255,255,255,0.2);
            background: rgba(0,0,0,0.3);
        }

        .bar-segment {
            height: 100%;
            transition: opacity 0.2s ease;
        }

        .bar-time {
            background: linear-gradient(180deg, #4A90E2 0%, #357ABD 100%);
        }

        .bar-memory {
            background: linear-gradient(180deg, #F5A623 0%, #D4880F 100%);
        }

        .score-decomposition:hover .stacked-bar {
            transform: scale(1.1);
            box-shadow: 0 0 8px rgba(74,144,226,0.5);
        }

        .score-value {
            font-weight: 600;
            font-size: 14px;
            min-width: 50px;
        }

        /* Expandable Row Styles */
        .expandable-row {
            background: rgba(0,0,0,0.2);
        }

        .expandable-row td {
            padding: 0 !important;
            border-top: none;
        }

        .sensitivity-details {
            padding: 16px 24px;
            background: linear-gradient(180deg, rgba(0,0,0,0.3) 0%, rgba(0,0,0,0.1) 100%);
            border-left: 3px solid #4A90E2;
            overflow: hidden;
            transition: max-height 0.3s ease, opacity 0.3s ease;
        }

        .sensitivity-details h4 {
            margin: 0 0 12px 0;
            color: #4A90E2;
            font-size: 14px;
            text-transform: uppercase;
            letter-spacing: 1px;
        }

        .sensitivity-table {
            width: auto;
            margin-bottom: 12px;
            border-collapse: collapse;
        }

        .sensitivity-table th,
        .sensitivity-table td {
            padding: 6px 16px;
            text-align: left;
            border-bottom: 1px solid rgba(255,255,255,0.1);
            font-size: 13px;
        }

        .sensitivity-table th {
            color: #888;
            font-weight: 500;
            text-transform: uppercase;
            font-size: 11px;
            letter-spacing: 0.5px;
        }

        .sensitivity-table tr.current-scenario {
            background: rgba(74,144,226,0.15);
        }

        .rank-swing {
            margin: 12px 0 0 0;
            font-size: 13px;
            color: #aaa;
        }

        .rank-swing strong {
            color: #F5A623;
        }

        .expand-indicator {
            cursor: pointer;
            user-select: none;
            transition: transform 0.2s ease;
            font-size: 10px;
            color: #666;
            padding: 4px 8px;
        }

        .main-row:hover .expand-indicator {
            color: #4A90E2;
        }

        .main-row.expanded .expand-indicator {
            transform: rotate(180deg);
        }

        /* Scoring Insights Section */
        .scoring-insights {
            margin: 32px 0;
            padding: 24px;
            background: rgba(0,0,0,0.2);
            border-radius: 8px;
            border: 1px solid rgba(255,255,255,0.1);
        }

        .scoring-insights h3 {
            margin: 0 0 20px 0;
            color: #4A90E2;
            font-size: 18px;
            border-bottom: 1px solid rgba(74,144,226,0.3);
            padding-bottom: 12px;
        }

        .insight-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 24px;
        }

        .insight-card {
            background: rgba(0,0,0,0.2);
            border-radius: 6px;
            padding: 16px;
            border-left: 3px solid #4A90E2;
        }

        .insight-card.correlation {
            border-left-color: #9B59B6;
        }

        .insight-card.outliers {
            border-left-color: #E74C3C;
        }

        .insight-card.stability {
            border-left-color: #2ECC71;
        }

        .insight-card h4 {
            margin: 0 0 12px 0;
            font-size: 14px;
            color: #fff;
            display: flex;
            align-items: center;
            gap: 8px;
        }

        .insight-card p {
            margin: 0 0 8px 0;
            font-size: 13px;
            color: #aaa;
            line-height: 1.5;
        }

        .insight-card .metric-value {
            font-size: 24px;
            font-weight: 600;
            color: #4A90E2;
            margin-bottom: 8px;
        }

        .insight-card ul {
            margin: 8px 0 0 0;
            padding-left: 20px;
            font-size: 13px;
            color: #ccc;
        }

        .insight-card li {
            margin-bottom: 4px;
        }

        .stable-badge {
            display: inline-block;
            padding: 2px 8px;
            border-radius: 10px;
            font-size: 11px;
            font-weight: 500;
        }

        .stable-badge.stable {
            background: rgba(46,204,113,0.2);
            color: #2ECC71;
        }

        .stable-badge.unstable {
            background: rgba(231,76,60,0.2);
            color: #E74C3C;
        }

        /* Tooltip for stacked bar */
        .score-tooltip {
            position: absolute;
            background: rgba(0,0,0,0.9);
            border: 1px solid rgba(255,255,255,0.2);
            border-radius: 4px;
            padding: 8px 12px;
            font-size: 12px;
            color: #fff;
            pointer-events: none;
            z-index: 1000;
            white-space: nowrap;
            box-shadow: 0 4px 12px rgba(0,0,0,0.3);
        }

        .score-tooltip .tooltip-row {
            display: flex;
            justify-content: space-between;
            gap: 16px;
            margin-bottom: 4px;
        }

        .score-tooltip .tooltip-row:last-child {
            margin-bottom: 0;
            padding-top: 4px;
            border-top: 1px solid rgba(255,255,255,0.2);
        }

        .score-tooltip .label {
            color: #888;
        }

        .score-tooltip .value {
            font-weight: 600;
        }

        .score-tooltip .value.time {
            color: #4A90E2;
        }

        .score-tooltip .value.memory {
            color: #F5A623;
        }

        /* ========================================
           VALIDATION UI COMPONENTS
           ======================================== */

        /* Validation badge styles */
        .validation-badge {
            display: inline-flex;
            align-items: center;
            gap: 4px;
            padding: 2px 6px;
            border-radius: 4px;
            font-size: 0.75rem;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.2s ease;
            margin-left: 6px;
        }

        .validation-badge.critical {
            background: rgba(255, 68, 68, 0.2);
            border: 1px solid #ff4444;
            color: #ff4444;
        }

        .validation-badge.warning {
            background: rgba(255, 157, 0, 0.2);
            border: 1px solid #ff9d00;
            color: #ff9d00;
        }

        .validation-badge:hover {
            transform: scale(1.05);
            box-shadow: 0 0 10px currentColor;
        }

        .validation-badge svg {
            width: 12px;
            height: 12px;
        }

        /* Diagnostics modal overlay */
        .diagnostics-modal-overlay {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0, 0, 0, 0.8);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 10000;
            opacity: 0;
            visibility: hidden;
            transition: opacity 0.3s ease, visibility 0.3s ease;
        }

        .diagnostics-modal-overlay.visible {
            opacity: 1;
            visibility: visible;
        }

        .diagnostics-modal {
            background: var(--bg-dark, #0d0d12);
            border: 1px solid var(--primary, #00ff9d);
            border-radius: 8px;
            padding: 24px;
            max-width: 500px;
            width: 90%;
            box-shadow: 0 0 30px rgba(0, 255, 157, 0.2);
        }

        .diagnostics-modal h3 {
            color: var(--primary, #00ff9d);
            margin: 0 0 16px 0;
            font-family: 'JetBrains Mono', monospace;
        }

        .diagnostics-modal .issue-item {
            background: rgba(0, 0, 0, 0.3);
            border-radius: 4px;
            padding: 12px;
            margin-bottom: 12px;
        }

        .diagnostics-modal .issue-severity {
            display: inline-block;
            padding: 2px 8px;
            border-radius: 4px;
            font-size: 0.7rem;
            font-weight: 700;
            text-transform: uppercase;
        }

        .diagnostics-modal .issue-severity.critical {
            background: #ff4444;
            color: #000;
        }

        .diagnostics-modal .issue-severity.warning {
            background: #ff9d00;
            color: #000;
        }

        .diagnostics-modal .iteration-compare {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 16px;
            margin-top: 12px;
        }

        .diagnostics-modal .iteration-compare .expected,
        .diagnostics-modal .iteration-compare .actual {
            text-align: center;
        }

        .diagnostics-modal .iteration-compare label {
            display: block;
            font-size: 0.8rem;
            color: #888;
            margin-bottom: 4px;
        }

        .diagnostics-modal .iteration-compare .value {
            font-size: 1.2rem;
            font-family: 'JetBrains Mono', monospace;
        }

        .diagnostics-modal .iteration-compare .expected .value {
            color: var(--primary, #00ff9d);
        }

        .diagnostics-modal .iteration-compare .actual .value {
            color: #ff4444;
        }

        .diagnostics-modal .close-btn {
            margin-top: 16px;
            width: 100%;
            padding: 10px;
            background: transparent;
            border: 1px solid var(--primary, #00ff9d);
            color: var(--primary, #00ff9d);
            border-radius: 4px;
            cursor: pointer;
            font-family: 'JetBrains Mono', monospace;
            transition: all 0.2s ease;
        }

        .diagnostics-modal .close-btn:hover {
            background: var(--primary, #00ff9d);
            color: #000;
        }

        /* Invalid chart points */
        .scatter-point.invalid {
            stroke: #ff4444;
            stroke-width: 2;
            stroke-dasharray: 2,2;
        }
`;
