import { useEffect, useRef, useState } from "react";
import { useParams } from "react-router-dom";
import { useWalletAddress } from "@/hooks/useTon";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { useTonConnectUI } from "@tonconnect/ui-react";
import { tonToNanoStr } from "@/lib/ton-escrow";

interface Message {
  id: string;
  sender: string;
  text: string;
  createdAt: string;
}

export default function ChatRoom() {
  const { id } = useParams<{ id: string }>();
  const me = useWalletAddress();
  const [messages, setMessages] = useState<Message[]>([]);
  const [text, setText] = useState("");
  const [loading, setLoading] = useState(true);
  const bottomRef = useRef<HTMLDivElement | null>(null);
  const [order, setOrder] = useState<any>(null);
  const [tonConnectUI] = useTonConnectUI();

  async function load() {
    if (!id) return;
    setLoading(true);
    try {
      const [rm, ro] = await Promise.all([
        fetch(`/api/messages?orderId=${encodeURIComponent(id)}`),
        fetch(`/api/orders/${encodeURIComponent(id)}`),
      ]);
      const jm = await rm.json();
      const jo = await ro.json();
      setMessages(
        (jm.items || []).map((m: any) => ({
          id: String(m.id),
          sender: String(m.sender),
          text: String(m.text),
          createdAt: String(m.createdAt),
        })),
      );
      setOrder(jo.order || null);
    } finally {
      setLoading(false);
      setTimeout(
        () => bottomRef.current?.scrollIntoView({ behavior: "smooth" }),
        50,
      );
    }
  }

  useEffect(() => {
    let tm: any;
    load();
    tm = setInterval(load, 4000);
    return () => clearInterval(tm);
  }, [id]);

  async function send() {
    if (!id || !me || !text.trim()) return;
    const payload = { orderId: id, sender: me, text };
    setText("");
    await fetch(`/api/messages`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    await load();
  }

  return (
    <div className="min-h-screen bg-[hsl(217,33%,9%)] text-white">
      <div className="mx-auto flex h-[calc(100vh-100px)] w-full max-w-2xl flex-col px-4 py-6">
        <div className="mb-3 text-lg font-semibold">Chat</div>

        {order && (
          <div className="mb-3 flex flex-wrap items-center gap-2 text-xs text-white/70">
            <div className="rounded border border-white/10 bg-white/5 px-2 py-1">Status: {order.status}</div>
            <div className="rounded border border-white/10 bg-white/5 px-2 py-1">Maker: {order.makerConfirmed ? "confirmed" : "pending"}</div>
            <div className="rounded border border-white/10 bg-white/5 px-2 py-1">Taker: {order.takerConfirmed ? "confirmed" : "pending"}</div>
          </div>
        )}

        <div className="flex-1 space-y-2 overflow-y-auto rounded-lg border border-white/10 bg-white/5 p-3">
          {loading && <div className="text-white/70">Loading…</div>}
          {!loading && messages.length === 0 && (
            <div className="text-white/70">No messages yet.</div>
          )}
          {messages.map((m) => {
            const mine = me && m.sender && me === m.sender;
            return (
              <div key={m.id} className={mine ? "text-right" : "text-left"}>
                <div className="inline-block max-w-[85%] rounded-lg bg-white/10 px-3 py-2 text-sm">
                  <div className="opacity-70 text-[11px]">
                    {mine ? "You" : m.sender.slice(0, 6) + "…"}
                  </div>
                  <div className="whitespace-pre-wrap">{m.text}</div>
                </div>
              </div>
            );
          })}
          <div ref={bottomRef} />
        </div>
        <div className="mt-3 flex gap-2">
          <Input
            value={text}
            onChange={(e) => setText(e.target.value)}
            placeholder="Write a message…"
            className="bg-white/5 text-white border-white/10"
          />
          <Button onClick={send} className="bg-primary text-primary-foreground">
            Send
          </Button>
        </div>

        {order && me && (
          <div className="mt-3 flex flex-wrap gap-2">
            {(() => {
              const isMaker = me === order.makerAddress;
              const isTaker = me === order.takerAddress;
              const canConfirm = (isMaker && !order.makerConfirmed) || (isTaker && !order.takerConfirmed);
              const bothConfirmed = Boolean(order.makerConfirmed && order.takerConfirmed);
              const contractAddr = String(order.contractAddr || "");
              return (
                <>
                  {canConfirm && (
                    <Button
                      variant="secondary"
                      onClick={async () => {
                        try {
                          if (contractAddr) {
                            await tonConnectUI.sendTransaction({
                              validUntil: Math.floor(Date.now() / 1000) + 300,
                              messages: [
                                {
                                  address: contractAddr,
                                  amount: tonToNanoStr(0),
                                },
                              ],
                            });
                          }
                          const r = await fetch(`/api/orders/${encodeURIComponent(order.id)}`, {
                            method: "PATCH",
                            headers: { "Content-Type": "application/json" },
                            body: JSON.stringify({ action: "confirm", actor: isMaker ? "maker" : "taker" }),
                          });
                          const j = await r.json();
                          if (!r.ok) throw new Error(j?.error || "failed");
                          await load();
                        } catch (e) {
                          alert("Failed to confirm completion");
                        }
                      }}
                    >
                      Confirm completion
                    </Button>
                  )}

                  {bothConfirmed && contractAddr && (
                    <Button
                      onClick={async () => {
                        try {
                          const isMakerNow = me === order.makerAddress;
                          const role = isMakerNow ? 1 : 2;
                          await tonConnectUI.sendTransaction({
                            validUntil: Math.floor(Date.now() / 1000) + 300,
                            messages: [
                              {
                                address: contractAddr,
                                amount: tonToNanoStr(0),
                                payload: buildOpPayloadBase64(ESCROW_OPS.CONFIRM, { role }),
                              },
                            ],
                          });
                          alert("Payout requested on-chain");
                        } catch (e) {
                          alert("Failed to trigger payout");
                        }
                      }}
                    >
                      Get payout
                    </Button>
                  )}
                </>
              );
            })()}
          </div>
        )}
      </div>
    </div>
  );
}
