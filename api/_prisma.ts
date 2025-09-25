let prisma: any;

try {
  // Try to require Prisma client without breaking bundling if absent
  // Using eval('require') avoids static analysis bundling issues
  const req: any = (function () {
    try {
      // @ts-ignore
      return eval("require");
    } catch {
      return null;
    }
  })();
  const mod: any = req ? req("@prisma/client") : null;
  const PrismaClientCtor: any =
    mod?.PrismaClient || mod?.default?.PrismaClient || null;
  if (!PrismaClientCtor) throw new Error("Prisma client not generated");

  const g = globalThis as any;
  prisma = g.__prisma || new PrismaClientCtor();
  if (process.env.NODE_ENV !== "production") g.__prisma = prisma;
  // Optional log to understand path taken in serverless
  console.log("[PRISMA] Using real Prisma client");
} catch (_err) {
  // Fallback to in-memory mock when Prisma is not available (e.g. build scripts blocked)
  console.warn("[PRISMA] Real Prisma client not available, using mock client");

  const mockStorage = {
    users: new Map<string, any>(),
    offers: new Map<string, any>(),
    orders: new Map<string, any>(),
    messages: new Map<string, any>(),
    reviews: new Map<string, any>(),
    jobs: new Map<string, any>(),
  };

  const genId = (p: string) =>
    `${p}-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`;

  prisma = {
    user: {
      findUnique: async (opts: any) =>
        mockStorage.users.get(opts?.where?.address || opts?.where?.id) || null,
      upsert: async (opts: any) => {
        const addr = String(opts?.where?.address || "");
        const existing = mockStorage.users.get(addr);
        if (existing) {
          const updated = { ...existing, ...(opts?.update || {}) };
          mockStorage.users.set(addr, updated);
          return updated;
        }
        const created = {
          id: genId("user"),
          address: addr,
          nickname: addr,
          createdAt: new Date().toISOString(),
          ...(opts?.create || {}),
        };
        mockStorage.users.set(addr, created);
        return created;
      },
      deleteMany: async () => ({ count: (mockStorage.users.clear(), 0) }),
    },

    offer: {
      findMany: async (opts: any = {}) => {
        let arr = Array.from(mockStorage.offers.values());
        if (opts?.where?.AND?.length) {
          arr = arr.filter((o) =>
            opts.where.AND.every((f: any) => {
              const ors = f?.OR || [];
              return ors.some((cond: any) => {
                const key = Object.keys(cond || {})[0];
                const val = cond?.[key]?.contains || "";
                const hay = `${o.title}\n${o.description}`.toLowerCase();
                return hay.includes(String(val).toLowerCase());
              });
            }),
          );
        }
        if (opts?.orderBy?.createdAt === "desc") {
          arr.sort((a, b) => +new Date(b.createdAt) - +new Date(a.createdAt));
        }
        // Apply select projection if provided
        if (opts?.select && typeof opts.select === "object") {
          return arr.map((o) => {
            const sel: any = {};
            for (const k of Object.keys(opts.select)) {
              if (!opts.select[k]) continue;
              if (k === "creator")
                sel.creator = { address: o.makerAddress || null };
              else sel[k] = (o as any)[k];
            }
            return Object.keys(sel).length ? sel : o;
          });
        }
        return arr;
      },
      findUnique: async (opts: any) => {
        const o = mockStorage.offers.get(opts?.where?.id) || null;
        if (!o) return null;
        if (opts?.select && typeof opts.select === "object") {
          const sel: any = {};
          for (const k of Object.keys(opts.select)) {
            if (!opts.select[k]) continue;
            if (k === "creator")
              sel.creator = { address: o.makerAddress || null };
            else sel[k] = (o as any)[k];
          }
          return Object.keys(sel).length ? sel : o;
        }
        return o;
      },
      create: async (opts: any) => {
        const rec = {
          id: genId("offer"),
          title: opts?.data?.title || "Offer",
          description: opts?.data?.description || "",
          budgetTON: Number(opts?.data?.budgetTON || 0),
          status: opts?.data?.status || "open",
          createdAt: new Date().toISOString(),
          creatorId: opts?.data?.creatorId || undefined,
          makerAddress: undefined,
        };
        mockStorage.offers.set(rec.id, rec);
        return rec;
      },
      deleteMany: async () => ({ count: (mockStorage.offers.clear(), 0) }),
    },

    order: {
      findMany: async (opts: any = {}) => {
        let arr = Array.from(mockStorage.orders.values());
        const w = opts?.where || {};
        arr = arr.filter((o) => {
          let ok = true;
          if (w.status) ok &&= o.status === w.status;
          if (w.makerAddress) ok &&= o.makerAddress === w.makerAddress;
          if (w.takerAddress) ok &&= o.takerAddress === w.takerAddress;
          if (Array.isArray(w.OR))
            ok &&= w.OR.some(
              (r: any) =>
                (r.makerAddress && r.makerAddress === o.makerAddress) ||
                (r.takerAddress && r.takerAddress === o.takerAddress),
            );
          return ok;
        });
        if (opts?.orderBy?.createdAt === "desc") {
          arr.sort((a, b) => +new Date(b.createdAt) - +new Date(a.createdAt));
        }
        return arr;
      },
      findFirst: async (opts: any) => {
        const arr = await prisma.order.findMany({ where: opts?.where });
        return arr[0] || null;
      },
      findUnique: async (opts: any) =>
        mockStorage.orders.get(opts?.where?.id) || null,
      create: async (opts: any) => {
        const rec = {
          id: genId("order"),
          title: opts?.data?.title || "Order",
          offerId: opts?.data?.offerId || null,
          makerAddress: opts?.data?.makerAddress || "",
          takerAddress: null,
          priceTON: Number(opts?.data?.priceTON || 0),
          nPercent: Number(opts?.data?.nPercent || 1),
          makerDeposit: Number(opts?.data?.makerDeposit || 0),
          takerStake: Number(opts?.data?.takerStake || 0),
          status: "created",
          makerConfirmed: false,
          takerConfirmed: false,
          createdAt: new Date().toISOString(),
          updatedAt: new Date().toISOString(),
          completedAt: null,
          cancelledAt: null,
          contractAddr: null,
        };
        mockStorage.orders.set(rec.id, rec);
        return rec;
      },
      update: async (opts: any) => {
        const id = opts?.where?.id;
        const cur = mockStorage.orders.get(id);
        if (!cur) throw new Error("not_found");
        const next = { ...cur, ...(opts?.data || {}) };
        mockStorage.orders.set(id, next);
        return next;
      },
      deleteMany: async () => ({ count: (mockStorage.orders.clear(), 0) }),
    },

    message: {
      findMany: async (opts: any = {}) => {
        const orderId = opts?.where?.orderId;
        const arr = Array.from(mockStorage.messages.values()).filter(
          (m) => !orderId || m.orderId === orderId,
        );
        arr.sort((a, b) => +new Date(a.createdAt) - +new Date(b.createdAt));
        return arr;
      },
      create: async (opts: any) => {
        const rec = {
          id: genId("msg"),
          orderId: String(opts?.data?.orderId || ""),
          sender: String(opts?.data?.sender || ""),
          text: String(opts?.data?.text || ""),
          createdAt: new Date().toISOString(),
        };
        mockStorage.messages.set(rec.id, rec);
        return rec;
      },
      deleteMany: async () => ({ count: (mockStorage.messages.clear(), 0) }),
    },

    review: {
      deleteMany: async () => ({ count: (mockStorage.reviews.clear(), 0) }),
    },
    job: { deleteMany: async () => ({ count: (mockStorage.jobs.clear(), 0) }) },

    $transaction: async (ops: any[]) => Promise.all(ops),
  };
}

export { prisma };
export default prisma;
