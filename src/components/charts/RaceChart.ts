/**
 * Matrix Race Chart - Interactive animated bar chart racing through matrices
 * Shows how language rankings change as they progress through matrices 1-6
 */
import { BaseChart } from './BaseChart';
import type { ChartDataPoint, ChartConfig } from '../../types/charts';
import * as d3 from 'd3';

interface RaceFrame {
  matrixIndex: number;
  matrixName: string;
  rankings: { name: string; cumulativeTime: number; color: string; tier: string }[];
}

type RaceMode = 'matrix' | 'elapsed';

export class RaceChart extends BaseChart {
  private frames: RaceFrame[] = [];
  private currentFrame: number = 0;
  private isPlaying: boolean = false;
  private playInterval: number | null = null;
  private chartGroup: d3.Selection<SVGGElement, unknown, null, undefined> | null = null;
  private xScale: d3.ScaleLinear<number, number> | null = null;
  private yScale: d3.ScaleBand<string> | null = null;
  private controlsContainer: HTMLElement | null = null;
  private topN: number = 999; // Show all languages by default
  private raceMode: RaceMode = 'matrix';
  private elapsedTime: number = 0;
  private maxElapsedTime: number = 0;
  private elapsedFrameCount: number = 100; // Number of frames for elapsed time animation
  private isFullscreen: boolean = false;
  private algorithmFilter: string = 'all';

  constructor(container: HTMLElement, config: ChartConfig = {}) {
    super(container, {
      ...config,
      margin: { top: 30, right: 100, bottom: 80, left: 140 }
    });
  }

  render(data: ChartDataPoint[]): void {
    this.clear();
    this.data = data;
    this.currentFrame = 0;
    this.isPlaying = false;

    if (this.playInterval) {
      clearInterval(this.playInterval);
      this.playInterval = null;
    }

    // Build race frames from matrix results
    this.buildFrames(data);

    if (this.frames.length === 0) {
      this.renderEmptyState('No matrix data available for race');
      return;
    }

    const { innerWidth: width, innerHeight: height, margin } = this.dimensions;

    // Create controls container
    this.createControls();

    // Create SVG
    const svgEl = d3.select(this.container)
      .append('svg')
      .attr('width', this.dimensions.width)
      .attr('height', this.dimensions.height);

    this.chartGroup = svgEl
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Get all language names across all frames for consistent ordering
    const allNames = new Set<string>();
    this.frames.forEach(f => f.rankings.forEach(r => allNames.add(r.name)));

    // Y scale (languages) - will be updated each frame
    this.yScale = d3.scaleBand<string>()
      .range([0, height])
      .padding(0.15);

    // X scale (time) - max across all frames
    const maxTime = d3.max(this.frames, f => d3.max(f.rankings, r => r.cumulativeTime)) || 1;
    this.xScale = d3.scaleLinear()
      .domain([0, maxTime * 1.1])
      .range([0, width]);

    // Draw X axis
    const xAxis = this.chartGroup.append('g')
      .attr('class', 'x-axis')
      .attr('transform', `translate(0,${height})`)
      .call(d3.axisBottom(this.xScale).ticks(8).tickFormat(d => this.formatTime(d as number)));

    xAxis.selectAll('text')
      .attr('fill', '#565f89')
      .attr('font-size', '11px')
      .attr('font-family', 'JetBrains Mono, monospace');

    xAxis.select('.domain').attr('stroke', '#414868');
    xAxis.selectAll('.tick line').attr('stroke', '#414868');

    // X axis label
    const xLabelText = this.raceMode === 'elapsed'
      ? 'Elapsed Time (Real-Time Simulation)'
      : 'Cumulative Execution Time';
    this.chartGroup.append('text')
      .attr('class', 'x-label')
      .attr('x', width / 2)
      .attr('y', height + 35)
      .attr('text-anchor', 'middle')
      .attr('fill', '#565f89')
      .attr('font-size', '12px')
      .text(xLabelText);

    // Title (will be updated with current matrix)
    this.chartGroup.append('text')
      .attr('class', 'chart-title')
      .attr('x', width / 2)
      .attr('y', -10)
      .attr('text-anchor', 'middle')
      .attr('fill', '#7aa2f7')
      .attr('font-size', '16px')
      .attr('font-weight', 'bold')
      .text('Matrix Race: Starting...');

    // Y axis group (will be updated)
    this.chartGroup.append('g').attr('class', 'y-axis');

    // Bars group
    this.chartGroup.append('g').attr('class', 'bars');

    // Time labels group
    this.chartGroup.append('g').attr('class', 'time-labels');

    // Rank labels group
    this.chartGroup.append('g').attr('class', 'rank-labels');

    // Render initial frame
    this.renderFrame(0, false);
  }

  private buildFrames(data: ChartDataPoint[]): void {
    this.frames = [];

    // Filter to languages with results and by algorithm
    let validData = data.filter(d => d.results && d.results.length > 0);

    // Apply algorithm filter
    if (this.algorithmFilter !== 'all') {
      validData = validData.filter(d => d.algorithm === this.algorithmFilter);
    }

    if (validData.length === 0) return;

    // Get unique matrix names, sorted
    const matrixNames = new Set<string>();
    validData.forEach(d => d.results?.forEach(r => matrixNames.add(r.matrix)));
    const sortedMatrices = Array.from(matrixNames).sort((a, b) => parseInt(a) - parseInt(b));

    if (this.raceMode === 'elapsed') {
      this.buildElapsedFrames(validData, sortedMatrices);
    } else {
      this.buildMatrixFrames(validData, sortedMatrices);
    }
  }

  private buildMatrixFrames(validData: ChartDataPoint[], sortedMatrices: string[]): void {
    // Build cumulative frames - one per matrix
    sortedMatrices.forEach((matrixName, matrixIndex) => {
      const rankings: RaceFrame['rankings'] = [];

      validData.forEach(d => {
        const name = d.language || d.solver;
        let cumulativeTime = 0;

        // Sum time for all matrices up to and including this one
        for (let i = 0; i <= matrixIndex; i++) {
          const result = d.results?.find(r => r.matrix === sortedMatrices[i]);
          if (result) {
            cumulativeTime += result.time;
          }
        }

        if (cumulativeTime > 0) {
          rankings.push({
            name,
            cumulativeTime,
            color: this.getColorByScore(d.score || 10),
            tier: d.tier || 'F'
          });
        }
      });

      // Sort by cumulative time and take top N
      rankings.sort((a, b) => a.cumulativeTime - b.cumulativeTime);
      const topRankings = rankings.slice(0, this.topN);

      this.frames.push({
        matrixIndex,
        matrixName,
        rankings: topRankings
      });
    });
  }

  private buildElapsedFrames(validData: ChartDataPoint[], sortedMatrices: string[]): void {
    // Calculate total time for all languages to determine max elapsed time
    let maxTotal = 0;
    const languageTotals: { name: string; totalTime: number; color: string; tier: string; times: number[] }[] = [];

    validData.forEach(d => {
      const name = d.language || d.solver;
      const times: number[] = [];
      let totalTime = 0;

      sortedMatrices.forEach(matrixName => {
        const result = d.results?.find(r => r.matrix === matrixName);
        const time = result?.time || 0;
        times.push(time);
        totalTime += time;
      });

      if (totalTime > 0) {
        languageTotals.push({
          name,
          totalTime,
          color: this.getColorByScore(d.score || 10),
          tier: d.tier || 'F',
          times
        });
        maxTotal = Math.max(maxTotal, totalTime);
      }
    });

    this.maxElapsedTime = maxTotal;

    // Create frames at regular time intervals
    const timeStep = maxTotal / this.elapsedFrameCount;

    for (let frameIdx = 0; frameIdx <= this.elapsedFrameCount; frameIdx++) {
      const elapsedTime = frameIdx * timeStep;
      const rankings: RaceFrame['rankings'] = [];

      languageTotals.forEach(lang => {
        // Calculate how much time has elapsed for this language
        let cumulativeTime = 0;
        let currentMatrix = 0;

        for (let i = 0; i < lang.times.length; i++) {
          if (cumulativeTime + lang.times[i] <= elapsedTime) {
            cumulativeTime += lang.times[i];
            currentMatrix = i + 1;
          } else {
            // Partial progress through current matrix
            cumulativeTime = elapsedTime;
            break;
          }
        }

        // Cap at total time
        cumulativeTime = Math.min(cumulativeTime, lang.totalTime);

        rankings.push({
          name: lang.name,
          cumulativeTime,
          color: lang.color,
          tier: lang.tier
        });
      });

      // Sort by cumulative time and take top N
      rankings.sort((a, b) => a.cumulativeTime - b.cumulativeTime);
      const topRankings = rankings.slice(0, this.topN);

      // Determine which matrix we're currently showing
      const currentMatrixIdx = Math.min(
        Math.floor(frameIdx / (this.elapsedFrameCount / sortedMatrices.length)),
        sortedMatrices.length - 1
      );

      this.frames.push({
        matrixIndex: currentMatrixIdx,
        matrixName: `${this.formatTime(elapsedTime)}`,
        rankings: topRankings
      });
    }
  }

  private rebuildForMode(): void {
    if (!this.data) return;

    // Clear existing
    this.clear();
    this.currentFrame = 0;

    // Rebuild frames
    this.buildFrames(this.data);

    if (this.frames.length === 0) {
      this.renderEmptyState('No data available');
      return;
    }

    // Update slider max
    const slider = this.controlsContainer?.querySelector('#race-slider') as HTMLInputElement;
    if (slider) {
      slider.max = String(this.frames.length - 1);
      slider.value = '0';
    }

    // Update matrix ticks (hide in elapsed mode or show in matrix mode)
    const ticksContainer = this.controlsContainer?.querySelector('.race-matrix-labels');
    if (ticksContainer) {
      if (this.raceMode === 'elapsed') {
        (ticksContainer as HTMLElement).style.display = 'none';
      } else {
        (ticksContainer as HTMLElement).style.display = 'flex';
        ticksContainer.innerHTML = this.frames.map((f, i) =>
          `<span class="matrix-tick${i === 0 ? ' active' : ''}" data-index="${i}">M${f.matrixName}</span>`
        ).join('');
        // Re-wire tick clicks
        ticksContainer.querySelectorAll('.matrix-tick').forEach(tick => {
          tick.addEventListener('click', (e) => {
            const index = parseInt((e.target as HTMLElement).dataset.index || '0');
            this.goToFrame(index);
          });
        });
      }
    }

    // Re-render chart
    this.render(this.data);
  }

  private createControls(): void {
    this.controlsContainer = document.createElement('div');
    this.controlsContainer.className = 'race-controls';
    this.controlsContainer.innerHTML = `
      <div class="race-controls-inner">
        <button class="race-btn" id="race-restart" title="Restart">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            <path d="M1 4v6h6M23 20v-6h-6"/>
            <path d="M20.49 9A9 9 0 0 0 5.64 5.64L1 10m22 4l-4.64 4.36A9 9 0 0 1 3.51 15"/>
          </svg>
        </button>
        <button class="race-btn" id="race-prev" title="Previous Matrix">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            <polygon points="19 20 9 12 19 4 19 20"/>
            <line x1="5" y1="19" x2="5" y2="5"/>
          </svg>
        </button>
        <button class="race-btn race-btn-primary" id="race-play" title="Play/Pause">
          <svg width="20" height="20" viewBox="0 0 24 24" fill="currentColor" id="play-icon">
            <polygon points="5 3 19 12 5 21 5 3"/>
          </svg>
          <svg width="20" height="20" viewBox="0 0 24 24" fill="currentColor" id="pause-icon" style="display:none">
            <rect x="6" y="4" width="4" height="16"/>
            <rect x="14" y="4" width="4" height="16"/>
          </svg>
        </button>
        <button class="race-btn" id="race-next" title="Next Matrix">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            <polygon points="5 4 15 12 5 20 5 4"/>
            <line x1="19" y1="5" x2="19" y2="19"/>
          </svg>
        </button>
        <div class="race-slider-container">
          <input type="range" id="race-slider" min="0" max="${this.frames.length - 1}" value="0" class="race-slider">
          <div class="race-matrix-labels">
            ${this.frames.map((f, i) => `<span class="matrix-tick${i === 0 ? ' active' : ''}" data-index="${i}">M${f.matrixName}</span>`).join('')}
          </div>
        </div>
        <div class="race-mode">
          <label>Mode:</label>
          <select id="race-mode">
            <option value="matrix" selected>Matrix Steps</option>
            <option value="elapsed">Elapsed Time</option>
          </select>
        </div>
        <div class="race-speed">
          <label>Speed:</label>
          <select id="race-speed">
            <option value="2000">0.5x</option>
            <option value="1000" selected>1x</option>
            <option value="500">2x</option>
            <option value="250">4x</option>
          </select>
        </div>
        <div class="race-algo">
          <label>Algo:</label>
          <select id="race-algo">
            <option value="all" selected>All</option>
            <option value="BruteForce">Brute Force</option>
            <option value="DLX">DLX</option>
            <option value="CP">Constraint Prop</option>
          </select>
        </div>
        <button class="race-btn" id="race-fullscreen" title="Fullscreen">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            <path d="M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3"/>
          </svg>
        </button>
      </div>
    `;

    // Add styles
    const style = document.createElement('style');
    style.textContent = `
      .race-controls {
        padding: 12px 0;
        margin-bottom: 8px;
      }
      .race-controls-inner {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
      }
      .race-btn {
        background: #1a1b26;
        border: 1px solid #414868;
        border-radius: 6px;
        color: #a9b1d6;
        padding: 8px 12px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.2s;
      }
      .race-btn:hover {
        background: #24283b;
        border-color: #7aa2f7;
        color: #7aa2f7;
      }
      .race-btn-primary {
        background: #7aa2f7;
        border-color: #7aa2f7;
        color: #1a1b26;
      }
      .race-btn-primary:hover {
        background: #89b4fa;
        border-color: #89b4fa;
      }
      .race-slider-container {
        flex: 1;
        min-width: 200px;
        padding: 0 12px;
      }
      .race-slider {
        width: 100%;
        height: 6px;
        -webkit-appearance: none;
        appearance: none;
        background: #414868;
        border-radius: 3px;
        outline: none;
        cursor: pointer;
      }
      .race-slider::-webkit-slider-thumb {
        -webkit-appearance: none;
        appearance: none;
        width: 18px;
        height: 18px;
        background: #7aa2f7;
        border-radius: 50%;
        cursor: pointer;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      }
      .race-slider::-moz-range-thumb {
        width: 18px;
        height: 18px;
        background: #7aa2f7;
        border-radius: 50%;
        cursor: pointer;
        border: none;
      }
      .race-matrix-labels {
        display: flex;
        justify-content: space-between;
        margin-top: 4px;
        font-size: 10px;
        color: #565f89;
      }
      .matrix-tick {
        cursor: pointer;
        padding: 2px 4px;
        border-radius: 3px;
        transition: all 0.2s;
      }
      .matrix-tick:hover {
        color: #a9b1d6;
      }
      .matrix-tick.active {
        color: #7aa2f7;
        font-weight: bold;
      }
      .race-mode, .race-speed {
        display: flex;
        align-items: center;
        gap: 6px;
        font-size: 12px;
        color: #565f89;
      }
      .race-mode select, .race-speed select, .race-algo select {
        background: #1a1b26;
        border: 1px solid #414868;
        border-radius: 4px;
        color: #a9b1d6;
        padding: 4px 8px;
        cursor: pointer;
      }
      .race-mode select:focus, .race-speed select:focus, .race-algo select:focus {
        border-color: #7aa2f7;
        outline: none;
      }
      .race-algo {
        display: flex;
        align-items: center;
        gap: 6px;
        font-size: 12px;
        color: #565f89;
      }
    `;
    this.container.appendChild(style);
    this.container.insertBefore(this.controlsContainer, this.container.firstChild);

    // Wire up controls
    this.wireControls();
  }

  private wireControls(): void {
    const playBtn = this.controlsContainer?.querySelector('#race-play');
    const prevBtn = this.controlsContainer?.querySelector('#race-prev');
    const nextBtn = this.controlsContainer?.querySelector('#race-next');
    const restartBtn = this.controlsContainer?.querySelector('#race-restart');
    const slider = this.controlsContainer?.querySelector('#race-slider') as HTMLInputElement;
    const speedSelect = this.controlsContainer?.querySelector('#race-speed') as HTMLSelectElement;
    const modeSelect = this.controlsContainer?.querySelector('#race-mode') as HTMLSelectElement;
    const matrixTicks = this.controlsContainer?.querySelectorAll('.matrix-tick');

    playBtn?.addEventListener('click', () => this.togglePlay());
    prevBtn?.addEventListener('click', () => this.prevFrame());
    nextBtn?.addEventListener('click', () => this.nextFrame());
    restartBtn?.addEventListener('click', () => this.restart());

    slider?.addEventListener('input', (e) => {
      const value = parseInt((e.target as HTMLInputElement).value);
      this.goToFrame(value);
    });

    speedSelect?.addEventListener('change', () => {
      if (this.isPlaying) {
        this.pause();
        this.play();
      }
    });

    modeSelect?.addEventListener('change', (e) => {
      this.raceMode = (e.target as HTMLSelectElement).value as RaceMode;
      this.pause();
      this.rebuildForMode();
    });

    // Algorithm filter
    const algoSelect = this.controlsContainer?.querySelector('#race-algo') as HTMLSelectElement;
    algoSelect?.addEventListener('change', (e) => {
      this.algorithmFilter = (e.target as HTMLSelectElement).value;
      this.pause();
      this.rebuildForMode();
    });

    // Fullscreen button
    const fullscreenBtn = this.controlsContainer?.querySelector('#race-fullscreen');
    fullscreenBtn?.addEventListener('click', () => this.toggleFullscreen());

    matrixTicks?.forEach(tick => {
      tick.addEventListener('click', (e) => {
        const index = parseInt((e.target as HTMLElement).dataset.index || '0');
        this.goToFrame(index);
      });
    });
  }

  private togglePlay(): void {
    if (this.isPlaying) {
      this.pause();
    } else {
      this.play();
    }
  }

  private play(): void {
    if (this.currentFrame >= this.frames.length - 1) {
      this.currentFrame = 0;
    }

    this.isPlaying = true;
    this.updatePlayButton();

    const speedSelect = this.controlsContainer?.querySelector('#race-speed') as HTMLSelectElement;
    const speed = parseInt(speedSelect?.value || '1000');

    this.playInterval = window.setInterval(() => {
      if (this.currentFrame < this.frames.length - 1) {
        this.nextFrame();
      } else {
        this.pause();
      }
    }, speed);
  }

  private pause(): void {
    this.isPlaying = false;
    this.updatePlayButton();

    if (this.playInterval) {
      clearInterval(this.playInterval);
      this.playInterval = null;
    }
  }

  private restart(): void {
    this.pause();
    this.goToFrame(0);
  }

  private prevFrame(): void {
    if (this.currentFrame > 0) {
      this.goToFrame(this.currentFrame - 1);
    }
  }

  private nextFrame(): void {
    if (this.currentFrame < this.frames.length - 1) {
      this.goToFrame(this.currentFrame + 1);
    }
  }

  private goToFrame(index: number): void {
    this.currentFrame = Math.max(0, Math.min(index, this.frames.length - 1));
    this.renderFrame(this.currentFrame, true);
    this.updateSlider();
    this.updateMatrixTicks();
  }

  private updatePlayButton(): void {
    const playIcon = this.controlsContainer?.querySelector('#play-icon') as HTMLElement;
    const pauseIcon = this.controlsContainer?.querySelector('#pause-icon') as HTMLElement;

    if (playIcon && pauseIcon) {
      playIcon.style.display = this.isPlaying ? 'none' : 'block';
      pauseIcon.style.display = this.isPlaying ? 'block' : 'none';
    }
  }

  private updateSlider(): void {
    const slider = this.controlsContainer?.querySelector('#race-slider') as HTMLInputElement;
    if (slider) {
      slider.value = String(this.currentFrame);
    }
  }

  private updateMatrixTicks(): void {
    const ticks = this.controlsContainer?.querySelectorAll('.matrix-tick');
    ticks?.forEach((tick, i) => {
      tick.classList.toggle('active', i === this.currentFrame);
    });
  }

  private renderFrame(frameIndex: number, animate: boolean): void {
    if (!this.chartGroup || !this.xScale || !this.yScale) return;

    const frame = this.frames[frameIndex];
    if (!frame) return;

    const { innerWidth: width, innerHeight: height } = this.dimensions;
    const duration = animate ? 500 : 0;

    // Update Y scale domain based on current rankings
    this.yScale.domain(frame.rankings.map(r => r.name));

    // Update Y axis
    const yAxisGroup = this.chartGroup.select<SVGGElement>('.y-axis');
    yAxisGroup
      .transition()
      .duration(duration)
      .call(d3.axisLeft(this.yScale).tickSize(0) as any);

    yAxisGroup.selectAll('text')
      .attr('fill', '#c0caf5')
      .attr('font-size', '11px')
      .attr('font-family', 'JetBrains Mono, monospace');

    yAxisGroup.select('.domain').attr('stroke', '#414868');

    // Update title based on mode
    const titleText = this.raceMode === 'elapsed'
      ? `Matrix Race: Elapsed ${frame.matrixName}`
      : `Matrix Race: Through Matrix ${frame.matrixName} (${frameIndex + 1}/${this.frames.length})`;
    this.chartGroup.select('.chart-title').text(titleText);

    // Update bars
    const barsGroup = this.chartGroup.select('.bars');
    const bars = barsGroup.selectAll<SVGRectElement, typeof frame.rankings[0]>('rect.bar')
      .data(frame.rankings, d => d.name);

    // Enter
    bars.enter()
      .append('rect')
      .attr('class', 'bar')
      .attr('y', d => this.yScale!(d.name) || 0)
      .attr('height', this.yScale.bandwidth())
      .attr('x', 0)
      .attr('width', 0)
      .attr('fill', d => d.color)
      .attr('rx', 3)
      .attr('ry', 3)
      .style('opacity', 0.85)
      .merge(bars)
      .transition()
      .duration(duration)
      .attr('y', d => this.yScale!(d.name) || 0)
      .attr('height', this.yScale.bandwidth())
      .attr('width', d => this.xScale!(d.cumulativeTime))
      .attr('fill', d => d.color);

    // Exit
    bars.exit()
      .transition()
      .duration(duration)
      .attr('width', 0)
      .remove();

    // Time labels removed per user request

    // Update rank labels
    const ranksGroup = this.chartGroup.select('.rank-labels');
    const ranks = ranksGroup.selectAll<SVGTextElement, typeof frame.rankings[0]>('text.rank-label')
      .data(frame.rankings, d => d.name);

    ranks.enter()
      .append('text')
      .attr('class', 'rank-label')
      .attr('x', -5)
      .attr('y', d => (this.yScale!(d.name) || 0) + this.yScale!.bandwidth() / 2)
      .attr('dy', '0.35em')
      .attr('text-anchor', 'end')
      .attr('fill', d => this.getTierColor(d.tier))
      .attr('font-size', '10px')
      .attr('font-weight', 'bold')
      .text((_, i) => `#${i + 1}`)
      .merge(ranks)
      .transition()
      .duration(duration)
      .attr('y', d => (this.yScale!(d.name) || 0) + this.yScale!.bandwidth() / 2)
      .text((_, i) => `#${i + 1}`)
      .attr('fill', d => this.getTierColor(d.tier));

    ranks.exit().remove();
  }

  private renderEmptyState(message: string): void {
    const div = document.createElement('div');
    div.style.cssText = 'display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);';
    div.innerHTML = `<p>${message}</p>`;
    this.container.appendChild(div);
  }

  private getColorByScore(score: number): string {
    if (score < 1.0) return '#00ff9d';  // S tier - green
    if (score < 1.5) return '#00b8ff';  // A/B tier - blue
    if (score < 3.0) return '#ffaa00';  // C tier - yellow
    return '#ff5555';                    // D/F tier - red
  }

  private getTierColor(tier: string): string {
    switch (tier) {
      case 'S': return '#00ff9d';
      case 'A': return '#00b8ff';
      case 'B': return '#7aa2f7';
      case 'C': return '#ffaa00';
      case 'D': return '#ff8800';
      case 'F': return '#ff5555';
      default: return '#565f89';
    }
  }

  private toggleFullscreen(): void {
    const wrapper = this.container.closest('.chart-wrapper') || this.container;

    if (!document.fullscreenElement) {
      wrapper.requestFullscreen().catch(err => {
        console.warn('Fullscreen failed:', err);
      });
      this.isFullscreen = true;
    } else {
      document.exitFullscreen();
      this.isFullscreen = false;
    }

    // Re-render after fullscreen change
    setTimeout(() => {
      if (this.data) {
        this.dimensions = this.calculateDimensions();
        this.render(this.data);
      }
    }, 100);
  }

  // Clean up on destroy
  destroy(): void {
    this.pause();
    if (this.controlsContainer) {
      this.controlsContainer.remove();
    }
    super.clear();
  }
}
