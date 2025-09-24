import { prisma } from "../_prisma";

function allow(res: any) {
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "POST,OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type");
}

export default async function handler(req: any, res: any) {
  allow(res);
  if (req.method === "OPTIONS") return res.status(204).end();
  if (req.method !== "POST")
    return res.status(405).json({ error: "method_not_allowed" });

  try {
    const body =
      typeof req.body === "string" ? JSON.parse(req.body || "{}") : req.body || {};
    const address = String(body?.address || req.query?.address || "").trim();
    if (!address) return res.status(400).json({ error: "address_required" });

    let order = await prisma.order.findFirst({
      where: { makerAddress: address, takerAddress: address },
      orderBy: { createdAt: "desc" },
    });

    if (!order) {
      order = await prisma.order.create({
        data: {
          title: "Favorites",
          makerAddress: address,
          takerAddress: address,
          priceTON: 0,
          nPercent: 0,
          makerDeposit: 0,
          takerStake: 0,
          status: "created",
        },
      });
    }

    return res.status(200).json({ ok: true, order });
  } catch (e) {
    console.error("ensureSelfChat error:", e);
    return res.status(500).json({ error: "internal_error" });
  }
}
