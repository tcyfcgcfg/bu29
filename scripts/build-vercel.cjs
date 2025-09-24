#!/usr/bin/env node

/**
 * Safe build script for Vercel
 * Handles Prisma setup and gracefully falls back when it's not available
 */

const { exec } = require('child_process');

console.log('[BUILD] Starting safe build process...');

async function runCommand(command, description, allowFailure = false) {
  return new Promise((resolve, reject) => {
    console.log(`[BUILD] ${description}...`);
    
    const process = exec(command, (error, stdout, stderr) => {
      if (error) {
        if (allowFailure) {
          console.warn(`[BUILD] ${description} failed, continuing...`);
          console.warn(`[BUILD] Error: ${error.message}`);
          resolve(false);
        } else {
          console.error(`[BUILD] ${description} failed!`);
          console.error(`[BUILD] Error: ${error.message}`);
          reject(error);
        }
      } else {
        console.log(`[BUILD] ${description} completed successfully`);
        if (stdout) console.log(stdout);
        resolve(true);
      }
    });

    // Set timeout for each step
    setTimeout(() => {
      console.warn(`[BUILD] ${description} timed out`);
      process.kill();
      if (allowFailure) {
        resolve(false);
      } else {
        reject(new Error(`${description} timed out`));
      }
    }, 120000); // 2 minute timeout per step
  });
}

async function main() {
  try {
    // Step 1: Try to generate Prisma client (allow failure)
    const prismaGenerated = await runCommand(
      'prisma generate', 
      'Generating Prisma client', 
      true
    );

    // Step 2: Try to push database schema (only if Prisma was generated and DATABASE_URL exists)
    if (prismaGenerated && process.env.DATABASE_URL) {
      await runCommand(
        'prisma db push', 
        'Pushing database schema', 
        true
      );
    } else {
      console.warn('[BUILD] Skipping database push - Prisma not available or DATABASE_URL not set');
    }

    // Step 3: Build client (required)
    await runCommand(
      'npm run build:client', 
      'Building client application', 
      false
    );

    console.log('[BUILD] Build process completed successfully!');
    process.exit(0);

  } catch (error) {
    console.error('[BUILD] Build process failed:', error.message);
    process.exit(1);
  }
}

main();