// Safe Prisma client import with fallback to mock
let prisma;

try {
  // Try to import the real Prisma client
  const pkg = await import("@prisma/client");
  const { PrismaClient } = pkg.default || pkg;
  
  const globalForPrisma = globalThis as unknown as {
    prisma?: InstanceType<typeof PrismaClient>;
  };
  
  prisma = globalForPrisma.prisma || new PrismaClient();
  if (process.env.NODE_ENV !== "production") globalForPrisma.prisma = prisma;
  
  console.log('[PRISMA] Using real Prisma client');
} catch (error) {
  // Fallback to mock Prisma client
  console.warn('[PRISMA] Real Prisma client not available, using mock client');
  
  // Create a minimal mock Prisma client
  const mockStorage = {
    offers: new Map(),
    orders: new Map(),
    messages: new Map(),
    users: new Map(),
  };

  prisma = {
    user: {
      findUnique: async () => null,
      upsert: async (options) => {
        const address = options?.where?.address;
        const existing = mockStorage.users.get(address);
        if (existing) {
          const updated = { ...existing, ...options?.update };
          mockStorage.users.set(address, updated);
          return updated;
        } else {
          const mockUser = {
            id: 'mock-user-' + Date.now(),
            address: address || '',
            nickname: address || '',
            createdAt: new Date().toISOString(),
            ...options?.create,
          };
          mockStorage.users.set(address, mockUser);
          return mockUser;
        }
      }
    },
    offer: {
      findMany: async () => Array.from(mockStorage.offers.values()),
      findUnique: async () => null,
      create: async (options) => {
        const mockOffer = {
          id: 'mock-offer-' + Date.now(),
          title: options?.data?.title || 'Mock Offer',
          description: options?.data?.description || '',
          budgetTON: options?.data?.budgetTON || 0,
          status: options?.data?.status || 'open',
          createdAt: new Date().toISOString(),
        };
        mockStorage.offers.set(mockOffer.id, mockOffer);
        return mockOffer;
      }
    },
    order: {
      findMany: async () => Array.from(mockStorage.orders.values()),
      findFirst: async () => null,
      create: async () => ({ id: 'mock-order-' + Date.now() })
    },
    message: {
      findMany: async () => Array.from(mockStorage.messages.values()),
      create: async () => ({ id: 'mock-message-' + Date.now() })
    },
    review: { deleteMany: async () => ({ count: 0 }) },
    job: { deleteMany: async () => ({ count: 0 }) },
    $transaction: async (operations) => {
      const results = [];
      for (const op of operations) {
        results.push(await op);
      }
      return results;
    },
  };
}

export { prisma };
export default prisma;
