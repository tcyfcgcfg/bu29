// Shared constants for escrow logic
export const N_PERCENT = 1; // commission percent used across app

// Escrow contract operation codes (must match contracts/escrow.fc)
export const ESCROW_OPS = {
  DEPOSIT_MAKER: 0x1001,
  TAKE: 0x1002,
  CONFIRM: 0x1003,
  CANCEL: 0x1004,
  ADMIN_FORCE: 0x1005,
} as const;

export type EscrowOpCode = typeof ESCROW_OPS[keyof typeof ESCROW_OPS];
