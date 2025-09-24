import { useEffect, useState } from "react";
import { useParams, Link, useLocation, useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";
import { useWalletAddress } from "@/hooks/useTon";
import { useTonConnectUI } from "@tonconnect/ui-react";
import { tonToNanoStr } from "@/lib/ton-escrow";

export default function OfferPage() {
  const { id } = useParams<{ id: string }>();
  const location = useLocation();
  const seed = (location.state as any)?.offer;
  const [offer, setOffer] = useState<any>(seed || null);
  const me = useWalletAddress();
  const navigate = useNavigate();
  const [tonConnectUI] = (useTonConnectUI as any)
    ? (useTonConnectUI() as any)
    : [null];
  const [loading, setLoading] = useState(!seed);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (seed || !id) return;
    let mounted = true;
    async function load() {
      setLoading(true);
      setError(null);
      try {
        const r = await fetch(`/api/offers/${id}`);
        if (!mounted) return;
        if (!r.ok) throw new Error(`Failed: ${r.status}`);
        const ct = r.headers.get("content-type") || "";
        const data = ct.includes("application/json")
          ? await r.json()
          : { offer: null };
        if (!data.offer) throw new Error("No data");
        setOffer(data.offer);
      } catch (e: any) {
        if (!mounted) return;
        setError("Unable to load offer");
      } finally {
        setLoading(false);
      }
    }
    load();
    return () => {
      mounted = false;
    };
  }, [id, seed]);

  const minimal = offer
    ? {
        title: String(offer.title || "Offer"),
        budgetTON: Number(offer.budgetTON || 0),
        status: String(offer.status || "open"),
        createdAt: String(offer.createdAt || new Date().toISOString()),
      }
    : null;

  return (
    <div className="min-h-screen bg-[hsl(217,33%,9%)] text-white">
      <div className="mx-auto w-full max-w-2xl px-4 py-10">
        <div className="mb-4 text-sm text-white/60">
          <Link to="/take" className="hover:underline">
            ← Back to offers
          </Link>
        </div>
        {loading && (
          <div className="rounded-lg border border-white/10 bg-white/5 p-4 text-white/70">
            Loading…
          </div>
        )}
        {error && (
          <div className="rounded-lg border border-red-600 bg-red-900/30 p-4 text-red-200">
            {error}
          </div>
        )}
        {minimal && (
          <div className="rounded-xl border border-white/10 bg-white/5 p-4">
            <div className="mb-1 text-2xl font-semibold">{minimal.title}</div>
            <div className="text-sm text-white/60 mb-4">
              {new Date(minimal.createdAt).toLocaleString()}
            </div>
            <div className="text-primary text-lg font-medium">
              {minimal.budgetTON} TON
            </div>
            <div className="mt-1 text-xs text-white/60">
              Status: {minimal.status}
            </div>
            {offer?.description && (
              <div className="mt-3 whitespace-pre-wrap text-sm text-white/80">
                {String(offer.description)}
              </div>
            )}
            <div className="mt-4 flex gap-2">
              <Button
                className="bg-primary text-primary-foreground"
                onClick={async () => {
                  try {
                    const maker = String(offer?.makerAddress || "");
                    // If trying to message yourself -> open Favorites (self chat)
                    if (me && maker && me === maker) {
                      const rSelf = await fetch("/api/chat/self", {
                        method: "POST",
                        headers: { "Content-Type": "application/json" },
                        body: JSON.stringify({ address: me }),
                      });
                      const jSelf = await rSelf.json();
                      if (!rSelf.ok) throw new Error(jSelf?.error || "failed");
                      const idSelf = jSelf?.order?.id;
                      if (!idSelf) throw new Error("no_self_chat");
                      navigate(`/chat/${idSelf}`);
                      return;
                    }

                    const r = await fetch("/api/orders", {
                      method: "POST",
                      headers: { "Content-Type": "application/json" },
                      body: JSON.stringify({
                        title: String(offer?.title || "Order"),
                        makerAddress: maker,
                        priceTON: Number(offer?.budgetTON || 0),
                        offerId: String(offer?.id || id || ""),
                      }),
                    });
                    const j = await r.json();
                    if (!r.ok) throw new Error(j?.error || "failed");
                    navigate(`/chat/${j.id || j.order?.id}`);
                  } catch (e) {
                    alert("Unable to start chat");
                  }
                }}
              >
                Message Maker
              </Button>
              <Button
                variant="secondary"
                onClick={async () => {
                  try {
                    if (!me) {
                      alert("Connect wallet to take the offer");
                      return;
                    }
                    const maker = String(offer?.makerAddress || "");
                    const r = await fetch("/api/orders", {
                      method: "POST",
                      headers: { "Content-Type": "application/json" },
                      body: JSON.stringify({
                        title: String(offer?.title || "Order"),
                        makerAddress: maker,
                        priceTON: Number(offer?.budgetTON || 0),
                        offerId: String(offer?.id || id || ""),
                      }),
                    });
                    const j = await r.json();
                    if (!r.ok) throw new Error(j?.error || "failed");
                    const orderId = String(j.id || j.order?.id || "");
                    if (!orderId) throw new Error("no_order");

                    // Send taker stake (20%) via TON to escrow contract if available
                    const contractAddr = String(
                      j.contractAddr || j.order?.contractAddr || "",
                    );
                    const takerStake = Number(
                      j.takerStake || j.order?.takerStake || 0,
                    );
                    if (contractAddr && takerStake > 0 && tonConnectUI) {
                      try {
                        let payload: string | undefined;
                        try {
                          const pr = await fetch(`/api/ton/payload?op=4098`); // 0x1002
                          const pj = await pr.json().catch(() => ({}));
                          payload = pj?.base64;
                        } catch {}
                        await tonConnectUI.sendTransaction({
                          validUntil: Math.floor(Date.now() / 1000) + 300,
                          messages: [
                            (() => {
                              const msg: any = {
                                address: contractAddr,
                                amount: tonToNanoStr(takerStake),
                              };
                              if (payload) msg.payload = payload;
                              return msg;
                            })(),
                          ],
                        });
                      } catch (txErr) {
                        console.error("Taker stake tx failed", txErr);
                        alert("TON transaction failed. Stake not sent.");
                      }
                    } else if (!contractAddr) {
                      alert(
                        "Escrow contract address is not set. Contact admin to initialize escrow for this offer.",
                      );
                    }

                    const rp = await fetch(`/api/orders/${orderId}`, {
                      method: "PATCH",
                      headers: { "Content-Type": "application/json" },
                      body: JSON.stringify({
                        action: "take",
                        takerAddress: me,
                      }),
                    });
                    const jp = await rp.json();
                    if (!rp.ok) throw new Error(jp?.error || "failed");

                    navigate(`/chat/${orderId}`);
                  } catch (_e) {
                    alert("Unable to take offer");
                  }
                }}
              >
                Take Offer
              </Button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
