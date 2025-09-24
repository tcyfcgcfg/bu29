import { beginCell, toNano } from "@ton/core";
import { ESCROW_OPS } from "@shared/constants";

export function buildOpPayloadBase64(op: number, opts?: { role?: 1 | 2; what?: number }) {
  const role = opts?.role ?? 0;
  const what = opts?.what ?? 0;
  const cell = beginCell().storeUint(op, 32).storeUint(role, 8).storeUint(what, 8).endCell();
  // toBoc returns a Buffer (polyfilled in browser by @ton/core)
  return cell.toBoc().toString("base64");
}

export function tonToNanoStr(ton: number | string) {
  return toNano(String(ton)).toString();
}

export function makerDepositAmount(priceTon: number, nPercent: number) {
  return priceTon * (1 + nPercent / 100);
}

export function takerStakeAmount(priceTon: number) {
  return priceTon * 0.2;
}

export { ESCROW_OPS };
