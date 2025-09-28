import { prisma } from "./_prisma";

export default async function handler(req: any, res: any) {
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type");
  if (req.method === "OPTIONS") return res.status(204).end();

  try {
    if (req.method === "GET") {
      // Parse query params robustly (support frameworks that don't populate req.query reliably)
      let q = "";
      let stack = "";
      let minBudget: number | undefined = undefined;
      let maxBudget: number | undefined = undefined;

      try {
        const qObj = (req?.query || {}) as any;
        if (typeof qObj.q === "string") q = String(qObj.q || "").trim();
        if (typeof qObj.stack === "string")
          stack = String(qObj.stack || "").trim();
        if (typeof qObj.minBudget === "string" && qObj.minBudget !== "") {
          const n = Number(qObj.minBudget);
          if (Number.isFinite(n)) minBudget = n;
        }
        if (typeof qObj.maxBudget === "string" && qObj.maxBudget !== "") {
          const n = Number(qObj.maxBudget);
          if (Number.isFinite(n)) maxBudget = n;
        }
      } catch {}

      if (!q && !stack && minBudget === undefined && maxBudget === undefined) {
        try {
          const base = `${req.headers?.["x-forwarded-proto"] || "https"}://${req.headers?.host || "localhost"}`;
          const raw = (req as any).originalUrl || req.url || base;
          const url = new URL(raw, base);
          q = String(url.searchParams.get("q") || "").trim();
          stack = String(url.searchParams.get("stack") || "").trim();
          const min = url.searchParams.get("minBudget");
          const max = url.searchParams.get("maxBudget");
          if (min !== null && min !== "") {
            const n = Number(min);
            if (Number.isFinite(n)) minBudget = n;
          }
          if (max !== null && max !== "") {
            const n = Number(max);
            if (Number.isFinite(n)) maxBudget = n;
          }
        } catch {}
      }

      const tokens = [
        ...(q ? q.split(/\s+/).filter(Boolean) : []),
        ...(stack ? stack.split(/[\s,]+/).filter(Boolean) : []),
      ];

      const filters: any[] = [];
      if (tokens.length) {
        filters.push(
          ...tokens.map((t) => ({
            OR: [
              { title: { contains: t, mode: "insensitive" as const } },
              { description: { contains: t, mode: "insensitive" as const } },
            ],
          })),
        );
      }
      if (minBudget !== undefined && Number.isFinite(minBudget)) {
        filters.push({ budgetTON: { gte: minBudget } });
      }
      if (maxBudget !== undefined && Number.isFinite(maxBudget)) {
        filters.push({ budgetTON: { lte: maxBudget } });
      }

      const where = filters.length ? { AND: filters } : undefined;

      const items = await prisma.offer.findMany({
        where,
        select: {
          id: true,
          title: true,
          description: true,
          budgetTON: true,
          status: true,
          createdAt: true,
          creator: { select: { address: true } },
        },
        orderBy: { createdAt: "desc" },
      });
      const mapped = items.map((o) => ({
        id: String(o.id),
        title: String(o.title ?? ""),
        description: String(o.description ?? ""),
        budgetTON: Number(o.budgetTON ?? 0),
        status: String(o.status ?? "open"),
        createdAt: String(o.createdAt ?? new Date().toISOString()),
        makerAddress: o.creator?.address || null,
      }));
      return res.status(200).json({ items: mapped });
    }

    if (req.method === "POST") {
      const body =
        typeof req.body === "string"
          ? JSON.parse(req.body || "{}")
          : req.body || {};
      const { title, description = "", budgetTON, makerAddress = "" } = body;
      if (!title || typeof budgetTON !== "number" || budgetTON < 0) {
        return res.status(400).json({ error: "Invalid payload" });
      }
      let creatorId: string | undefined;
      if (makerAddress) {
        const user = await prisma.user.findUnique({
          where: { address: makerAddress },
        });
        if (user) creatorId = user.id;
      }
      const created = await prisma.offer.create({
        data: {
          title,
          description,
          budgetTON,
          status: "open",
          ...(creatorId ? { creatorId } : {}),
        },
      });
      return res.status(201).json(created);
    }

    return res.status(405).json({ error: "Method not allowed" });
  } catch (e: any) {
    console.error("/api/offers error:", e);
    return res.status(500).json({ error: e?.message || String(e) });
  }
}
