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
            width: 900px;
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

        /* Author List Styling */
        .author-list {
            display: flex;
            flex-direction: row;
            flex-wrap: wrap;
            gap: 15px;
            margin-top: 10px;
        }

        .author-item {
            display: flex;
            flex-direction: column;
            align-items: center;
            gap: 8px;
            padding: 10px;
            background: #24283b;
            border-radius: 8px;
            border: 1px solid #414868;
            width: 140px; /* Fixed width for grid alignment */
        }

        .author-img {
            width: 80px;
            height: 80px;
            border-radius: 50%;
            object-fit: cover;
            background: #1a1b26;
            border: 2px solid #7aa2f7;
            flex-shrink: 0;
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
            border: 1px solid #414868;
            border-radius: 8px;
            padding: 12px 16px;
            color: #c0caf5;
            font-size: 14px;
            line-height: 1.6;
            box-shadow: 0 8px 32px rgba(0, 0, 0, 0.6);
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
        .chart-node-label { display: none; fill: #e0e0e0; font-size: 12px; font-weight: bold; text-shadow: 0 0 2px #000; }
        .show-text-labels .chart-node-label { display: block; }
        .show-text-labels .chart-node-image { display: none; }
        
        /* All columns equal width */
        .lang-col, .score-col, .matrix-cell, .total-time {
            overflow: hidden;
            white-space: nowrap;
            text-overflow: ellipsis;
        }

        /* Hide Chart Buttons (using Pulldown now) */
        .chart-options { display: none !important; }
`;
