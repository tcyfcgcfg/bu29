#!/usr/bin/env node

/**
 * Safe Prisma client generation script
 * Attempts to generate Prisma client, but exits successfully if it fails
 * This allows builds to continue using mock Prisma when generation is not possible
 */

const { exec } = require('child_process');

console.log('[PRISMA] Attempting to generate Prisma client...');

const generateProcess = exec('prisma generate', (error, stdout, stderr) => {
  if (error) {
    console.warn('[PRISMA] Failed to generate Prisma client, continuing with mock client');
    console.warn('[PRISMA] Error:', error.message);
    if (stderr) console.warn('[PRISMA] Stderr:', stderr);
    // Exit with success code to allow build to continue
    process.exit(0);
  } else {
    console.log('[PRISMA] Successfully generated Prisma client');
    if (stdout) console.log('[PRISMA] Stdout:', stdout);
    process.exit(0);
  }
});

// Set a timeout to prevent hanging
setTimeout(() => {
  console.warn('[PRISMA] Prisma generation timed out, continuing with mock client');
  generateProcess.kill();
  process.exit(0);
}, 60000); // 60 second timeout