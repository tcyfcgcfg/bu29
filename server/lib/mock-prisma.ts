const mockStorage = {
  offers: new Map<string, any>(),
  orders: new Map<string, MockOrder>(),
  messages: new Map<string, any>(),
  users: new Map<string, any>(),
};

type MockOrder = {
  id: string;
  title: string;
  makerAddress: string;
  takerAddress: string | null;
  status: string;
  priceTON: number;
  nPercent: number;
  makerDeposit: number;
  takerStake: number;
  offerId: string | null;
  createdAt: string;
  updatedAt: string;
  makerConfirmed?: boolean;
  takerConfirmed?: boolean;
  completedAt?: string | null;
  cancelledAt?: string | null;
};

function nowIso() {
  return new Date().toISOString();
}

function generateId(prefix: string) {
  return `${prefix}-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`;
}

interface SelectTree {
  [key: string]: boolean | SelectTree | { select?: SelectTree };
}

type SelectShape = SelectTree | undefined;

type OrderWhere = {
  id?: string;
  offerId?: string | null;
  status?: string;
  makerAddress?: string;
  takerAddress?: string | null;
  OR?: OrderWhere[];
  AND?: OrderWhere[];
};

function applySelect<T extends Record<string, any>>(
  item: T,
  select?: SelectShape,
) {
  if (!select) return { ...item };

  if (
    typeof select === "object" &&
    select !== null &&
    Object.prototype.hasOwnProperty.call(select, "select")
  ) {
    return applySelect(
      item,
      (select as Record<string, any>).select as SelectShape,
    );
  }

  const result: Record<string, any> = {};
  for (const [key, value] of Object.entries(select)) {
    if (!value) continue;
    if (typeof value === "object" && value) {
      const nested = item[key as keyof T];
      if (nested && typeof nested === "object") {
        result[key] = applySelect(
          nested as Record<string, any>,
          value as SelectShape,
        );
      }
      continue;
    }
    if (key in item) {
      result[key] = item[key as keyof T];
    }
  }
  return result;
}

function matchesWhere(order: MockOrder, where?: OrderWhere): boolean {
  if (!where) return true;

  if (where.OR && Array.isArray(where.OR)) {
    const orMatches = where.OR.some((clause) => matchesWhere(order, clause));
    if (!orMatches) return false;
  }

  if (where.AND && Array.isArray(where.AND)) {
    const andMatches = where.AND.every((clause) => matchesWhere(order, clause));
    if (!andMatches) return false;
  }

  if (
    Object.prototype.hasOwnProperty.call(where, "id") &&
    order.id !== where.id
  )
    return false;
  if (
    Object.prototype.hasOwnProperty.call(where, "offerId") &&
    order.offerId !== where.offerId
  )
    return false;
  if (
    Object.prototype.hasOwnProperty.call(where, "status") &&
    order.status !== where.status
  )
    return false;
  if (
    Object.prototype.hasOwnProperty.call(where, "makerAddress") &&
    order.makerAddress !== where.makerAddress
  )
    return false;
  if (
    Object.prototype.hasOwnProperty.call(where, "takerAddress") &&
    order.takerAddress !== where.takerAddress
  )
    return false;

  return true;
}

function sortOrders(
  orders: MockOrder[],
  orderBy?: { createdAt?: "asc" | "desc" },
) {
  if (!orderBy || !orderBy.createdAt) return orders;
  const direction = orderBy.createdAt === "desc" ? -1 : 1;
  return [...orders].sort((a, b) => {
    if (a.createdAt === b.createdAt) return 0;
    return a.createdAt > b.createdAt ? direction : -direction;
  });
}

export const mockPrisma = {
  order: {
    findMany: async (options?: {
      where?: OrderWhere;
      orderBy?: any;
      select?: SelectShape;
    }) => {
      console.log("[MOCK] order.findMany called with:", options);
      const where = options?.where;
      const orderBy = options?.orderBy;
      const select = options?.select;

      let orders = Array.from(mockStorage.orders.values());
      if (where) {
        orders = orders.filter((order) => matchesWhere(order, where));
      }
      orders = sortOrders(orders, orderBy);

      return orders.map((order) => applySelect(order, select));
    },
    findFirst: async (options?: {
      where?: OrderWhere;
      orderBy?: any;
      select?: SelectShape;
    }) => {
      console.log("[MOCK] order.findFirst called with:", options);
      const results = await mockPrisma.order.findMany(options);
      return results.length ? results[0] : null;
    },
    findUnique: async (options?: {
      where?: { id?: string };
      select?: SelectShape;
    }) => {
      console.log("[MOCK] order.findUnique called with:", options);
      const id = options?.where?.id;
      if (!id) return null;
      const order = mockStorage.orders.get(id) || null;
      return order ? applySelect(order, options?.select) : null;
    },
    create: async (options?: {
      data?: Partial<MockOrder>;
      select?: SelectShape;
    }) => {
      console.log("[MOCK] order.create called with:", options);
      const data = options?.data ?? {};
      const timestamp = nowIso();
      const order: MockOrder = {
        id: data.id || generateId("mock-order"),
        title: data.title || "Mock Order",
        makerAddress: data.makerAddress || "",
        takerAddress: Object.prototype.hasOwnProperty.call(data, "takerAddress")
          ? (data.takerAddress ?? null)
          : null,
        status: data.status || "created",
        priceTON: Number.isFinite(data.priceTON as number)
          ? (data.priceTON as number)
          : 0,
        nPercent: Number.isFinite(data.nPercent as number)
          ? (data.nPercent as number)
          : 0,
        makerDeposit: Number.isFinite(data.makerDeposit as number)
          ? (data.makerDeposit as number)
          : 0,
        takerStake: Number.isFinite(data.takerStake as number)
          ? (data.takerStake as number)
          : 0,
        offerId: data.offerId ?? null,
        createdAt: data.createdAt || timestamp,
        updatedAt: data.updatedAt || timestamp,
        makerConfirmed: Boolean(data.makerConfirmed),
        takerConfirmed: Boolean(data.takerConfirmed),
        completedAt: data.completedAt ?? null,
        cancelledAt: data.cancelledAt ?? null,
      };
      mockStorage.orders.set(order.id, order);
      return applySelect(order, options?.select);
    },
    update: async (options?: {
      where?: { id?: string };
      data?: Partial<MockOrder>;
      select?: SelectShape;
    }) => {
      console.log("[MOCK] order.update called with:", options);
      const id = options?.where?.id;
      if (!id) {
        throw new Error("[MOCK] order.update requires where.id");
      }
      const current = mockStorage.orders.get(id);
      if (!current) {
        throw new Error(`[MOCK] order.update failed, order ${id} not found`);
      }
      const data = options?.data ?? {};
      const updated: MockOrder = {
        ...current,
        ...data,
        takerAddress: Object.prototype.hasOwnProperty.call(data, "takerAddress")
          ? (data.takerAddress ?? null)
          : current.takerAddress,
        updatedAt: nowIso(),
      };
      mockStorage.orders.set(id, updated);
      return applySelect(updated, options?.select);
    },
  },
  message: {
    findMany: async (options?: any) => {
      console.log("[MOCK] message.findMany called with:", options);
      const messages = Array.from(mockStorage.messages.values());
      return messages.filter(
        (message) =>
          !options?.where?.orderId || message.orderId === options.where.orderId,
      );
    },
    create: async (options?: any) => {
      console.log("[MOCK] message.create called with:", options);
      const mockMessage = {
        id: "mock-message-" + Date.now(),
        orderId: options?.data?.orderId || "",
        sender: options?.data?.sender || "",
        text: options?.data?.text || "",
        createdAt: nowIso(),
      };
      mockStorage.messages.set(mockMessage.id, mockMessage);
      return mockMessage;
    },
  },
  offer: {
    findMany: async (options?: any) => {
      console.log("[MOCK] offer.findMany called with:", options);
      return Array.from(mockStorage.offers.values());
    },
    findUnique: async (options?: any) => {
      console.log("[MOCK] offer.findUnique called with:", options);
      const offer = mockStorage.offers.get(options?.where?.id);
      if (!offer) return null;

      const enrichedOffer = {
        ...offer,
        makerAddress: "mock-maker-address-" + offer.id.split("-").pop(),
        creator: {
          address: "mock-maker-address-" + offer.id.split("-").pop(),
        },
      };

      if (options?.select) {
        return applySelect(enrichedOffer, options.select);
      }

      return enrichedOffer;
    },
    create: async (options?: any) => {
      console.log("[MOCK] offer.create called with:", options);
      const mockOffer = {
        id: "mock-offer-" + Date.now(),
        title: options?.data?.title || "Mock Offer",
        description: options?.data?.description || "",
        budgetTON: options?.data?.budgetTON || 0,
        status: options?.data?.status || "open",
        createdAt: nowIso(),
      };
      mockStorage.offers.set(mockOffer.id, mockOffer);

      if (options?.select) {
        return applySelect(mockOffer, options.select);
      }

      return mockOffer;
    },
  },
  user: {
    findUnique: async (options?: any) => {
      console.log("[MOCK] user.findUnique called with:", options);
      const user = mockStorage.users.get(
        options?.where?.address || options?.where?.id,
      );
      return user || null;
    },
    upsert: async (options?: any) => {
      console.log("[MOCK] user.upsert called with:", options);
      const address = options?.where?.address;
      const existing = mockStorage.users.get(address);

      if (existing) {
        const updated = { ...existing, ...options?.update };
        mockStorage.users.set(address, updated);
        return updated;
      }

      const mockUser = {
        id: "mock-user-" + Date.now(),
        address: address || "",
        nickname: address || "",
        createdAt: nowIso(),
        ...options?.create,
      };
      mockStorage.users.set(address, mockUser);
      return mockUser;
    },
  },
};
