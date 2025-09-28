import { useEffect, useState, useMemo } from "react";
import { useWalletAddress } from "@/hooks/useTon";
import { Link, useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";

interface Order {
  id: string;
  title: string;
  status: "created" | "in_progress" | "completed" | "cancelled";
  makerAddress: string;
  takerAddress?: string | null;
  createdAt: string;
}

export default function Chat() {
  const addr = useWalletAddress();
  const [items, setItems] = useState<Order[]>([]);
  const [loading, setLoading] = useState(false);
  const navigate = useNavigate();

  useEffect(() => {
    if (!addr) return;
    let mounted = true;
    async function run() {
      try {
        setLoading(true);
        await fetch(`/api/chat/self`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ address: addr }),
        }).catch((err) => {
          console.error("Failed to ensure self-chat:", err);
        });
        const r = await fetch(
          `/api/orders?address=${encodeURIComponent(addr)}&role=any`,
        );
        const j = await r.json();
        if (!mounted) return;
        const list = (j.items || []) as any[];
        setItems(
          list.map((o) => ({
            id: String(o.id),
            title: String(o.title || "Order"),
            status: o.status,
            makerAddress: String(o.makerAddress || ""),
            takerAddress: o.takerAddress || null,
            createdAt: String(o.createdAt || new Date().toISOString()),
          })),
        );
      } finally {
        if (mounted) setLoading(false);
      }
    }
    run();
    return () => {
      mounted = false;
    };
  }, [addr]);

  const sections = useMemo(() => {
    const inProgress = items.filter(
      (i) => i.status === "in_progress" || i.status === "created",
    );
    const completed = items.filter((i) => i.status === "completed");
    return { inProgress, completed };
  }, [items]);

  function getPeerForOrder(o: Order, me?: string | null) {
    const maker = o.makerAddress;
    const taker = o.takerAddress || "";
    if (!me) return maker || taker;
    return me === maker ? taker : maker;
  }

  async function openSelfChat() {
    if (!addr) return;
    const r = await fetch(`/api/chat/self`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ address: addr }),
    });
    const j = await r.json().catch(() => ({}));
    const id = String(j?.order?.id || "");
    if (id) navigate(`/chat/${id}?peer=${encodeURIComponent(addr)}`);
  }

  function openChat(o: Order) {
    const peer = getPeerForOrder(o, addr);
    const url = `/chat/${o.id}${peer ? `?peer=${encodeURIComponent(peer)}` : ""}`;
    navigate(url);
  }

  return (
    <div className="min-h-screen bg-[hsl(217,33%,9%)] text-white">
      <div className="mx-auto w-full max-w-2xl px-4 py-10">
        <h1 className="text-3xl font-bold">Chat</h1>
        {!addr && (
          <div className="mt-3 text-white/70">
            Connect wallet to see your threads.
          </div>
        )}

        {loading && <div className="mt-4 text-white/70">Loading…</div>}

        {!loading && (
          <>
            <h2 className="mt-6 text-sm font-semibold text-white/60">
              In Progress
            </h2>
            <div className="mt-2 space-y-2">
              {addr && (
                <div className="flex items-center justify-between rounded-lg border border-white/10 bg-white/10 p-3">
                  <div>
                    <div className="font-medium truncate">Favorites</div>
                    <div className="text-xs text-white/60 mt-1">Private notes</div>
                  </div>
                  <Button
                    onClick={openSelfChat}
                    className="bg-primary text-primary-foreground"
                  >
                    Favorites
                  </Button>
                </div>
              )}

              {sections.inProgress.length === 0 && (
                <div className="rounded-lg border border-white/10 bg-white/5 p-3 text-white/70">
                  No active threads.
                </div>
              )}
              {sections.inProgress.map((o) => (
                <div
                  key={o.id}
                  className="rounded-lg border border-white/10 bg-white/5 p-3 hover:bg-white/10"
                >
                  <div className="flex items-center justify-between gap-3">
                    <div className="min-w-0 flex-1">
                      <div className="font-medium truncate">{o.title}</div>
                      <div className="text-xs text-white/60 mt-1">
                        {o.status} • {new Date(o.createdAt).toLocaleString()}
                      </div>
                    </div>
                    <Button
                      size="sm"
                      onClick={() => openChat(o)}
                      className="bg-primary text-primary-foreground"
                    >
                      Chat
                    </Button>
                  </div>
                </div>
              ))}
            </div>

            <h2 className="mt-6 text-sm font-semibold text-white/60">
              Completed
            </h2>
            <div className="mt-2 space-y-2">
              {sections.completed.length === 0 && (
                <div className="rounded-lg border border-white/10 bg-white/5 p-3 text-white/70">
                  No completed orders yet.
                </div>
              )}
              {sections.completed.map((o) => (
                <div
                  key={o.id}
                  className="rounded-lg border border-white/10 bg-white/5 p-3 hover:bg-white/10"
                >
                  <div className="flex items-center justify-between gap-3">
                    <div className="min-w-0 flex-1">
                      <div className="font-medium truncate">{o.title}</div>
                      <div className="text-xs text-white/60 mt-1">
                        completed • {new Date(o.createdAt).toLocaleString()}
                      </div>
                    </div>
                    <Button
                      size="sm"
                      onClick={() => openChat(o)}
                      className="bg-primary text-primary-foreground"
                    >
                      Chat
                    </Button>
                  </div>
                </div>
              ))}
            </div>
          </>
        )}
      </div>
    </div>
  );
}
