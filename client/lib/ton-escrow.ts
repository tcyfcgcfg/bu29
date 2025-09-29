import { ESCROW_OPS } from "@shared/constants";

export async function buildOpPayloadBase64(
  op: number,
  opts?: { role?: 1 | 2; what?: number },
): Promise<string | undefined> {
  const role = opts?.role ?? 0;
  const what = opts?.what ?? 0;
  try {
    const mod: any = await import("@ton/core");
    const cell = mod
      .beginCell()
      .storeUint(op, 32)
      .storeUint(role, 8)
      .storeUint(what, 8)
      .endCell();
    return cell.toBoc().toString("base64");
  } catch (e) {
    console.warn("TON core import failed, skipping payload build:", e);
    return undefined;
  }
}

export function tonToNanoStr(val: number | string): string {
  const s = String(val);
  if (!/^[0-9]+(\.[0-9]+)?$/.test(s)) return "0";
  const [intPart, fracRaw = ""] = s.split(".");
  const frac = (fracRaw + "000000000").slice(0, 9);
  const nano = BigInt(intPart) * BigInt(1_000_000_000) + BigInt(frac || "0");
  return nano.toString();
}

export function makerDepositAmount(priceTon: number, nPercent: number) {
  return priceTon * (1 + nPercent / 100);
}

export function takerStakeAmount(priceTon: number) {
  return priceTon * 0.2;
}

export { ESCROW_OPS };
