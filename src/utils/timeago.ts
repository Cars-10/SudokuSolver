/**
 * Relative Time Formatting Utility
 * Converts timestamps to human-readable relative time ("3d ago", "2h ago")
 */

export function timeAgo(date: Date | string | number): string {
  const now = new Date();
  const then = new Date(date);
  const seconds = Math.floor((now.getTime() - then.getTime()) / 1000);

  if (seconds < 0) {
    return 'just now';
  }

  const intervals: [number, string, string][] = [
    [31536000, 'year', 'y'],
    [2592000, 'month', 'mo'],
    [86400, 'day', 'd'],
    [3600, 'hour', 'h'],
    [60, 'minute', 'm'],
    [1, 'second', 's']
  ];

  for (const [secondsInUnit, _, shortLabel] of intervals) {
    const count = Math.floor(seconds / secondsInUnit);
    if (count >= 1) {
      return `${count}${shortLabel} ago`;
    }
  }

  return 'just now';
}

export function timeAgoLong(date: Date | string | number): string {
  const now = new Date();
  const then = new Date(date);
  const seconds = Math.floor((now.getTime() - then.getTime()) / 1000);

  if (seconds < 0) {
    return 'just now';
  }

  const intervals: [number, string][] = [
    [31536000, 'year'],
    [2592000, 'month'],
    [86400, 'day'],
    [3600, 'hour'],
    [60, 'minute'],
    [1, 'second']
  ];

  for (const [secondsInUnit, label] of intervals) {
    const count = Math.floor(seconds / secondsInUnit);
    if (count >= 1) {
      return `${count} ${label}${count !== 1 ? 's' : ''} ago`;
    }
  }

  return 'just now';
}

export function formatTimestamp(date: Date | string | number): string {
  const d = new Date(date);
  return d.toLocaleDateString('en-US', {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit'
  });
}

/**
 * Format elapsed time with precision to the second
 * e.g., "2 days, 3 hours, 45 minutes, 12 seconds ago"
 */
export function elapsedTimeDetailed(date: Date | string | number): string {
  const now = new Date();
  const then = new Date(date);
  let seconds = Math.floor((now.getTime() - then.getTime()) / 1000);

  if (seconds < 0) {
    return 'just now';
  }

  if (seconds === 0) {
    return '0 seconds ago';
  }

  const parts: string[] = [];

  const days = Math.floor(seconds / 86400);
  if (days > 0) {
    parts.push(`${days} day${days !== 1 ? 's' : ''}`);
    seconds %= 86400;
  }

  const hours = Math.floor(seconds / 3600);
  if (hours > 0) {
    parts.push(`${hours} hour${hours !== 1 ? 's' : ''}`);
    seconds %= 3600;
  }

  const minutes = Math.floor(seconds / 60);
  if (minutes > 0) {
    parts.push(`${minutes} minute${minutes !== 1 ? 's' : ''}`);
    seconds %= 60;
  }

  if (seconds > 0) {
    parts.push(`${seconds} second${seconds !== 1 ? 's' : ''}`);
  }

  return parts.join(', ') + ' ago';
}
