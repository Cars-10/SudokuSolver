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
            align-items: flex-start;
            justify-content: center;
            z-index: 10000;
            padding-top: 40px;
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

        /* Highlighted/Pinned Row State */
        .main-row.highlighted {
            background: linear-gradient(90deg, rgba(0, 255, 157, 0.15), rgba(0, 255, 157, 0.05)) !important;
            border-left: 4px solid var(--primary) !important;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.3);
            transform: scale(1.01);
            z-index: 10;
            position: relative;
        }

        .main-row.highlighted::before {
            content: '📍';
            position: absolute;
            left: 8px;
            top: 50%;
            transform: translateY(-50%);
            font-size: 16px;
            animation: pinBounce 1s ease-in-out;
        }

        @keyframes pinBounce {
            0%, 100% { transform: translateY(-50%) scale(1); }
            50% { transform: translateY(-50%) scale(1.3); }
        }

        .main-row.highlighted td:first-child {
            padding-left: 32px;
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

        /* ========================================
           ADVANCED VISUALIZATION STYLES (06-02)
           ======================================== */

        /* Scatter Plot - Time vs Memory */
        .scatter-point {
            fill: rgba(0, 184, 255, 0.6);
            stroke: #00b8ff;
            stroke-width: 1.5px;
            cursor: pointer;
            transition: all 0.2s ease;
        }

        .scatter-point:hover {
            fill: rgba(0, 255, 157, 0.8);
            stroke: #00ff9d;
            stroke-width: 2px;
            filter: drop-shadow(0 0 8px rgba(0, 255, 157, 0.6));
        }

        .scatter-point.outlier {
            fill: rgba(255, 68, 68, 0.6);
            stroke: #ff4444;
        }

        .scatter-point.outlier:hover {
            fill: rgba(255, 68, 68, 0.9);
            filter: drop-shadow(0 0 8px rgba(255, 68, 68, 0.8));
        }

        .scatter-point.highlighted {
            fill: rgba(0, 255, 157, 1) !important;
            stroke: #00ff9d !important;
            stroke-width: 3px !important;
            filter: drop-shadow(0 0 12px rgba(0, 255, 157, 1)) !important;
        }

        .scatter-tooltip {
            position: fixed;
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
            border: 1px solid #00ff9d;
            border-radius: 6px;
            padding: 10px 14px;
            color: #c0caf5;
            font-size: 13px;
            line-height: 1.6;
            box-shadow: 0 0 15px rgba(0, 255, 157, 0.3), 0 8px 32px rgba(0, 0, 0, 0.6);
            z-index: 9999;
            pointer-events: none;
            max-width: 250px;
            backdrop-filter: blur(8px);
            -webkit-backdrop-filter: blur(8px);
        }

        .log-scale-toggle {
            position: absolute;
            top: 10px;
            right: 10px;
            background: rgba(0, 184, 255, 0.2);
            border: 1px solid #00b8ff;
            color: #00b8ff;
            padding: 6px 12px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 12px;
            font-family: 'JetBrains Mono', monospace;
            transition: all 0.2s ease;
            z-index: 10;
        }

        .log-scale-toggle:hover {
            background: rgba(0, 184, 255, 0.4);
            border-color: #00ff9d;
            color: #00ff9d;
            box-shadow: 0 0 10px rgba(0, 184, 255, 0.5);
        }

        .log-scale-toggle.active {
            background: rgba(0, 255, 157, 0.3);
            border-color: #00ff9d;
            color: #00ff9d;
        }

        /* Heatmap - Language x Matrix */
        .heatmap-cell {
            stroke: rgba(255, 255, 255, 0.1);
            stroke-width: 1px;
            cursor: pointer;
            transition: stroke 0.2s ease;
        }

        .heatmap-cell:hover {
            stroke: #00ff9d;
            stroke-width: 2px;
        }

        .heatmap-axis-label {
            font-family: 'JetBrains Mono', monospace;
            font-size: 11px;
            fill: #c0caf5;
            user-select: none;
        }

        .heatmap-legend {
            position: absolute;
            top: 10px;
            right: 10px;
            background: rgba(0, 0, 0, 0.7);
            border: 1px solid rgba(255, 255, 255, 0.2);
            border-radius: 6px;
            padding: 10px;
            z-index: 10;
        }

        .heatmap-legend-title {
            font-family: 'JetBrains Mono', monospace;
            font-size: 11px;
            color: #c0caf5;
            margin-bottom: 8px;
            text-align: center;
        }

        .heatmap-gradient {
            width: 200px;
            height: 20px;
            border-radius: 3px;
            border: 1px solid rgba(255, 255, 255, 0.2);
        }

        .heatmap-legend-labels {
            display: flex;
            justify-content: space-between;
            margin-top: 4px;
            font-family: 'JetBrains Mono', monospace;
            font-size: 10px;
            color: #888;
        }

        /* Histogram - Score Distribution */
        .histogram-bar {
            fill: rgba(0, 184, 255, 0.4);
            stroke: rgba(0, 184, 255, 0.5);
            stroke-width: 1px;
            transition: fill 0.2s ease;
        }

        .histogram-bar:hover {
            fill: rgba(0, 255, 157, 0.4);
            stroke: #00ff9d;
        }

        .percentile-line {
            stroke-width: 2px;
            stroke-dasharray: none;
        }

        .percentile-line.q1 {
            stroke: #4A90E2;
        }

        .percentile-line.median {
            stroke: #2ECC71;
            stroke-width: 3px;
        }

        .percentile-line.q3 {
            stroke: #4A90E2;
        }

        .percentile-line.mean {
            stroke: #F5A623;
            stroke-dasharray: 5,5;
        }

        .percentile-label {
            font-family: 'JetBrains Mono', monospace;
            font-size: 11px;
            font-weight: 500;
            text-anchor: middle;
        }

        .percentile-label.q1 {
            fill: #4A90E2;
        }

        .percentile-label.median {
            fill: #2ECC71;
        }

        .percentile-label.q3 {
            fill: #4A90E2;
        }

        .percentile-label.mean {
            fill: #F5A623;
        }

        /* Chart Controls (Shared) */
        .chart-controls {
            position: absolute;
            top: 10px;
            left: 10px;
            display: flex;
            gap: 10px;
            align-items: center;
            z-index: 10;
        }

        .language-filter {
            background: rgba(0, 0, 0, 0.7);
            border: 1px solid rgba(0, 184, 255, 0.5);
            border-radius: 4px;
            padding: 6px 12px;
            color: #c0caf5;
            font-family: 'JetBrains Mono', monospace;
            font-size: 12px;
            outline: none;
            transition: border-color 0.2s ease;
        }

        .language-filter:focus {
            border-color: #00ff9d;
            box-shadow: 0 0 8px rgba(0, 255, 157, 0.3);
        }

        .language-filter::placeholder {
            color: #666;
        }

        /* ========================================
           INTERACTIVE SOLVER - 3D GRID
           ======================================== */

        /* Interactive Solver Section */
        .solver-section {
            background: #0a0a0f;
            padding: 2rem;
            padding-top: 3rem; /* Space for close button */
            border-radius: 8px;
            border: 1px solid rgba(0, 255, 157, 0.3);
            position: relative;
            z-index: 1;
            min-height: 400px;
        }

        /* Three-column header layout */
        .solver-header-grid {
            display: grid;
            grid-template-columns: 1fr 1.2fr 1fr;
            gap: 2rem;
            padding-bottom: 2rem;
            margin-bottom: 2rem;
            border-bottom: 1px solid rgba(0, 255, 157, 0.2);
        }

        .solver-header-title h2 {
            margin: 0 0 0.5rem 0;
            font-size: 2rem;
            color: #00ff9d;
            text-shadow: 0 0 10px rgba(0, 255, 157, 0.5);
        }

        .solver-header-title .solver-subtitle {
            margin: 0;
            font-size: 0.9rem;
            color: #888;
            font-style: italic;
        }

        .solver-header-controls {
            display: flex;
            flex-direction: column;
            gap: 0.75rem;
        }

        .solver-control-group {
            display: flex;
            align-items: center;
            gap: 0.75rem;
        }

        .solver-control-group label {
            min-width: 80px;
            font-size: 0.9rem;
            font-weight: 600;
            color: #00ff9d;
            text-shadow: 0 0 5px rgba(0, 255, 157, 0.3);
        }

        .solver-header-description h4 {
            margin: 0 0 0.75rem 0;
            font-size: 1rem;
            color: #00ff9d;
            text-shadow: 0 0 5px rgba(0, 255, 157, 0.3);
        }

        .solver-header-description p {
            margin: 0;
            font-size: 0.85rem;
            line-height: 1.6;
            color: #aaa;
        }

        /* Main content area: Grid on left, Controls on right */
        .solver-main-content {
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 2rem;
            min-height: 500px;
        }

        .solver-grid-section {
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: flex-start;
        }

        .solver-controls-section {
            display: flex;
            flex-direction: column;
            gap: 1rem;
        }

        /* Responsive layout */
        @media (max-width: 1200px) {
            .solver-header-grid {
                grid-template-columns: 1fr;
            }

            .solver-main-content {
                grid-template-columns: 1fr;
            }
        }

        #interactive-solver-section {
            position: relative;
            z-index: 1;
        }

        /* Modal centering for solver */
        #solver-modal .diagnostics-modal {
            margin: auto;
            position: relative;
        }

        /* Solver header */
        .solver-header {
            text-align: center;
            margin-bottom: 2rem;
        }

        .solver-header h2 {
            color: #00ff9d;
            font-size: 2rem;
            margin: 0;
            text-shadow: 0 0 20px rgba(0, 255, 157, 0.5);
        }

        .solver-subtitle {
            color: #888;
            font-style: italic;
            margin-top: 0.5rem;
        }

        /* Solver setup controls */
        .solver-setup {
            display: flex;
            flex-direction: column;
            align-items: center;
            gap: 1rem;
            margin-bottom: 2rem;
        }

        .solver-setup-row {
            display: flex;
            align-items: center;
            gap: 1rem;
        }

        .solver-setup-row label {
            font-family: 'JetBrains Mono', monospace;
            color: #888;
            min-width: 80px;
        }

        .solver-select {
            background: rgba(0, 255, 157, 0.1);
            border: 1px solid rgba(0, 255, 157, 0.3);
            color: #00ff9d;
            padding: 0.5rem 1rem;
            border-radius: 4px;
            font-family: 'JetBrains Mono', monospace;
            cursor: pointer;
            min-width: 250px;
        }

        .solver-select:focus {
            outline: none;
            border-color: #00ff9d;
            box-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }

        .solver-select option {
            background: #0a0a0f;
            color: #00ff9d;
        }

        .solver-select option:disabled {
            color: #666;
        }

        .solver-start-btn {
            margin-top: 1rem;
            padding: 0.75rem 2rem;
            font-size: 1rem;
        }

        /* Status display */
        .solver-status {
            text-align: center;
            padding: 1rem;
            margin-bottom: 1rem;
            border-radius: 4px;
            font-family: 'JetBrains Mono', monospace;
        }

        .status-info {
            color: #00b8ff;
            background: rgba(0, 184, 255, 0.1);
            border: 1px solid rgba(0, 184, 255, 0.3);
        }

        .status-success {
            color: #00ff9d;
            background: rgba(0, 255, 157, 0.1);
            border: 1px solid rgba(0, 255, 157, 0.3);
        }

        .status-error {
            color: #ff0064;
            background: rgba(255, 0, 100, 0.1);
            border: 1px solid rgba(255, 0, 100, 0.3);
        }

        /* Memory warning */
        .solver-memory-warning {
            display: flex;
            align-items: center;
            gap: 0.5rem;
            padding: 0.75rem 1rem;
            background: rgba(255, 157, 0, 0.1);
            border: 1px solid rgba(255, 157, 0, 0.3);
            border-radius: 4px;
            color: #ff9d00;
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.85rem;
            margin-top: 1rem;
        }

        .warning-icon {
            font-size: 1.2rem;
        }

        .solver-grid-container {
            perspective: 1000px;
            perspective-origin: center center;
            display: flex;
            justify-content: center;
            align-items: center;
            padding: 2rem;
        }

        .solver-grid {
            display: grid;
            grid-template-columns: repeat(9, 1fr);
            gap: 2px;
            background: rgba(0, 255, 157, 0.1);
            padding: 4px;
            border-radius: 4px;
            box-shadow:
                0 0 20px rgba(0, 255, 157, 0.3),
                inset 0 0 20px rgba(0, 255, 157, 0.1);
            transform-style: preserve-3d;
            transform: rotateX(5deg); /* Slight 3D tilt */
        }

        .solver-cell {
            width: 56px;
            height: 56px;
            display: flex;
            align-items: center;
            justify-content: center;
            background: rgba(10, 10, 20, 0.9);
            border: 1px solid rgba(0, 255, 157, 0.2);
            transform-style: preserve-3d;
            transition: transform 0.3s ease, background 0.3s ease;
            position: relative;
        }

        /* 3x3 box emphasis borders */
        .solver-cell[data-col="2"],
        .solver-cell[data-col="5"] {
            border-right: 2px solid rgba(0, 255, 157, 0.5);
        }

        .solver-cell[data-row="2"],
        .solver-cell[data-row="5"] {
            border-bottom: 2px solid rgba(0, 255, 157, 0.5);
        }

        .cell-value {
            font-family: 'JetBrains Mono', 'Fira Code', monospace;
            font-size: 1.75rem;
            font-weight: bold;
            color: #00ff9d;
            text-shadow:
                0 0 5px #fff,
                0 0 10px #00ff9d,
                0 0 20px #00ff9d;
            backface-visibility: hidden;
        }

        /* Initial puzzle values (fixed) */
        .solver-cell.fixed .cell-value {
            color: #888;
            text-shadow: none;
        }

        /* Active cell being tried */
        .solver-cell.active {
            background: rgba(0, 184, 255, 0.2);
            border-color: #00b8ff;
            box-shadow:
                0 0 10px rgba(0, 184, 255, 0.5),
                inset 0 0 10px rgba(0, 184, 255, 0.2);
        }

        .solver-cell.active .cell-value {
            color: #00b8ff;
            text-shadow:
                0 0 5px #fff,
                0 0 10px #00b8ff,
                0 0 20px #00b8ff;
        }

        /* Successfully placed value */
        .solver-cell.success {
            background: rgba(0, 255, 157, 0.2);
        }

        .solver-cell.success .cell-value {
            color: #00ff9d;
            text-shadow:
                0 0 5px #fff,
                0 0 10px #00ff9d,
                0 0 20px #00ff9d,
                0 0 40px #00ff9d;
        }

        /* Backtracking - error state */
        .solver-cell.backtrack {
            background: rgba(255, 0, 100, 0.2);
            border-color: #ff0064;
        }

        .solver-cell.backtrack .cell-value {
            color: #ff0064;
            text-shadow:
                0 0 5px #fff,
                0 0 10px #ff0064,
                0 0 20px #ff0064;
        }

        /* Spin animations */
        .solver-cell.spinning {
            animation: spinY var(--spin-duration, 0.6s) ease-in-out;
        }

        .solver-cell.spinning-reverse {
            animation: spinY-reverse var(--spin-duration, 0.6s) ease-in-out;
        }

        @keyframes spinY {
            0% { transform: rotateY(0deg); }
            50% { transform: rotateY(90deg); }
            100% { transform: rotateY(0deg); }
        }

        @keyframes spinY-reverse {
            0% { transform: rotateY(0deg); }
            50% { transform: rotateY(-90deg); }
            100% { transform: rotateY(0deg); }
        }

        /* Glitch effects */
        .solver-grid-container.glitch-screen-shake {
            animation: shake 0.2s ease-in-out;
        }

        @keyframes shake {
            0%, 100% { transform: translate(0, 0); }
            25% { transform: translate(-3px, 3px); }
            50% { transform: translate(3px, -3px); }
            75% { transform: translate(-3px, -3px); }
        }

        /* Chromatic aberration on intense moments */
        .solver-cell.chromatic .cell-value {
            position: relative;
        }

        .solver-cell.chromatic .cell-value::before,
        .solver-cell.chromatic .cell-value::after {
            content: attr(data-value);
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
        }

        .solver-cell.chromatic .cell-value::before {
            left: 2px;
            color: #f0f;
            opacity: 0.7;
        }

        .solver-cell.chromatic .cell-value::after {
            left: -2px;
            color: #0ff;
            opacity: 0.7;
        }

        /* Info panel */
        .solver-info {
            display: flex;
            gap: 2rem;
            margin-top: 1rem;
            padding: 1rem;
            background: rgba(0, 255, 157, 0.05);
            border-radius: 4px;
            font-family: 'JetBrains Mono', monospace;
        }

        .solver-info-item {
            display: flex;
            flex-direction: column;
            align-items: center;
        }

        .solver-info-label {
            font-size: 0.75rem;
            color: #888;
            text-transform: uppercase;
        }

        .solver-info-value {
            font-size: 1.25rem;
            color: #00ff9d;
            text-shadow: 0 0 10px rgba(0, 255, 157, 0.5);
        }

        /* Interactive Solver - Controls */
        .solver-controls {
            display: flex;
            flex-direction: column;
            gap: 1rem;
            padding: 1rem;
            background: rgba(0, 255, 157, 0.05);
            border-radius: 8px;
            border: 1px solid rgba(0, 255, 157, 0.2);
            margin-top: 1rem;
        }

        .solver-controls-row {
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 0.5rem;
            flex-wrap: wrap;
        }

        .solver-btn {
            background: rgba(0, 255, 157, 0.1);
            border: 1px solid rgba(0, 255, 157, 0.3);
            color: #00ff9d;
            padding: 0.5rem 1rem;
            border-radius: 4px;
            cursor: pointer;
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9rem;
            transition: all 0.2s ease;
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .solver-btn:hover {
            background: rgba(0, 255, 157, 0.2);
            border-color: #00ff9d;
            box-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }

        .solver-btn:disabled {
            opacity: 0.5;
            cursor: not-allowed;
        }

        .solver-btn.primary {
            background: rgba(0, 255, 157, 0.2);
            border-color: #00ff9d;
        }

        .solver-btn.primary:hover {
            background: rgba(0, 255, 157, 0.3);
        }

        /* Play/Pause icon */
        .solver-btn .icon {
            font-size: 1.2rem;
        }

        /* Speed slider container */
        .solver-speed-container {
            display: flex;
            align-items: center;
            gap: 1rem;
            width: 100%;
            max-width: 400px;
            margin: 0 auto;
        }

        .solver-speed-label {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.85rem;
            color: #888;
            min-width: 60px;
        }

        .solver-speed-value {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.85rem;
            color: #00ff9d;
            min-width: 50px;
            text-align: right;
        }

        /* Speed slider styling */
        .solver-speed-slider {
            -webkit-appearance: none;
            appearance: none;
            background: transparent;
            cursor: pointer;
            flex: 1;
            height: 20px;
        }

        .solver-speed-slider::-webkit-slider-runnable-track {
            background: linear-gradient(
                to right,
                #00ff9d 0%,
                #00ff9d var(--progress, 1%),
                rgba(255, 255, 255, 0.1) var(--progress, 1%),
                rgba(255, 255, 255, 0.1) 100%
            );
            height: 6px;
            border-radius: 3px;
            box-shadow: 0 0 5px rgba(0, 255, 157, 0.3);
        }

        .solver-speed-slider::-moz-range-track {
            background: rgba(255, 255, 255, 0.1);
            height: 6px;
            border-radius: 3px;
        }

        .solver-speed-slider::-moz-range-progress {
            background: #00ff9d;
            height: 6px;
            border-radius: 3px;
            box-shadow: 0 0 5px rgba(0, 255, 157, 0.3);
        }

        .solver-speed-slider::-webkit-slider-thumb {
            -webkit-appearance: none;
            appearance: none;
            height: 18px;
            width: 18px;
            background: #fff;
            border: 2px solid #00ff9d;
            border-radius: 50%;
            box-shadow: 0 0 5px #fff, 0 0 10px #00ff9d;
            margin-top: -6px;
            transition: transform 0.1s ease;
        }

        .solver-speed-slider::-webkit-slider-thumb:hover {
            transform: scale(1.2);
        }

        .solver-speed-slider::-moz-range-thumb {
            height: 18px;
            width: 18px;
            background: #fff;
            border: 2px solid #00ff9d;
            border-radius: 50%;
            box-shadow: 0 0 5px #fff, 0 0 10px #00ff9d;
            transition: transform 0.1s ease;
        }

        .solver-speed-slider::-moz-range-thumb:hover {
            transform: scale(1.2);
        }

        /* Speed presets */
        .solver-speed-presets {
            display: flex;
            justify-content: space-between;
            width: 100%;
            max-width: 400px;
            margin: 0 auto;
            padding: 0 10px;
        }

        .solver-speed-preset {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.7rem;
            color: #666;
            cursor: pointer;
            transition: color 0.2s;
        }

        .solver-speed-preset:hover {
            color: #00ff9d;
        }

        .solver-speed-preset.active {
            color: #00ff9d;
        }

        /* Progress bar */
        .solver-progress {
            width: 100%;
            height: 4px;
            background: rgba(255, 255, 255, 0.1);
            border-radius: 2px;
            overflow: hidden;
        }

        .solver-progress-bar {
            height: 100%;
            background: linear-gradient(90deg, #00ff9d, #00b8ff);
            width: 0%;
            transition: width 0.1s ease;
        }

        /* Responsive - smaller on mobile */
        @media (max-width: 600px) {
            .solver-cell {
                width: 32px;
                height: 32px;
            }
            .cell-value {
                font-size: 1rem;
            }
            .solver-controls-row {
                flex-direction: column;
            }
            .solver-btn {
                width: 100%;
                justify-content: center;
            }
            .solver-speed-container {
                flex-direction: column;
                align-items: stretch;
            }
        }

        /* Stability Visualization Canvas */
        .stability-viz-container {
            margin-bottom: 24px;
            background: rgba(0, 0, 0, 0.2);
            border-radius: 8px;
            padding: 20px;
            overflow-x: auto;
        }

        #stabilityVizCanvas {
            display: block;
            max-width: 100%;
            height: auto;
        }
`;
