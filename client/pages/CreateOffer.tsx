import { useState, useEffect } from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { useNavigate } from "react-router-dom";
import WalletGate from "@/components/WalletGate";
import { useIsWalletConnected, useWalletAddress } from "@/hooks/useTon";
import { useTonConnectUI } from "@tonconnect/ui-react";
import { buildOpPayloadBase64, tonToNanoStr, ESCROW_OPS } from "@/lib/ton-escrow";

export default function CreateOffer() {
  const [title, setTitle] = useState("");
  const [budget, setBudget] = useState("0.1");
  const [description, setDescription] = useState("");
  const [loading, setLoading] = useState(false);
  const [stack, setStack] = useState("");
  const navigate = useNavigate();
  const connected = useIsWalletConnected();
  const address = useWalletAddress();
  const [tonConnectUI] = useTonConnectUI();

  useEffect(() => {
    if (connected && address) {
      fetch("/api/users/upsert", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ address }),
      }).catch(console.error);
    }
  }, [connected, address]);

  async function submit() {
    if (!connected) {
      alert("Please connect your TON wallet");
      return;
    }
    setLoading(true);
    try {
      const r = await fetch("/api/offers", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          title,
          description,
          budgetTON: Number(budget),
          stack,
          makerAddress: address,
        }),
      });
      if (!r.ok) throw new Error("Failed to create offer");
      const offer = await r.json();

      const ro = await fetch("/api/orders", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          title,
          makerAddress: address,
          priceTON: Number(budget),
          offerId: String(offer.id || ""),
        }),
      });
      const order = await ro.json();
      if (!ro.ok) throw new Error(order?.error || "Failed to create order");

      const contractAddr = String(order.contractAddr || "");
      const makerDeposit = Number(order.makerDeposit || Number(budget));
      if (contractAddr) {
        try {
          let payload: string | undefined;
          try {
            const pr = await fetch(`/api/ton/payload?op=4097`); // 0x1001
            const pj = await pr.json().catch(() => ({}));
            payload = pj?.base64;
          } catch {}
          await tonConnectUI.sendTransaction({
            validUntil: Math.floor(Date.now() / 1000) + 300,
            messages: [
              (() => {
                const msg: any = { address: contractAddr, amount: tonToNanoStr(makerDeposit) };
                if (payload) msg.payload = payload;
                return msg;
              })(),
            ],
          });
        } catch (txErr) {
          console.error("Maker deposit tx failed", txErr);
          alert("TON transaction failed. Offer created, but escrow deposit not sent.");
        }
      } else {
        alert(
          "Offer created. Escrow contract is not configured yet (no contract address). Please initialize escrow before accepting takers.",
        );
      }

      navigate("/take");
    } catch (e) {
      console.error(e);
      alert("Failed to create offer");
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="min-h-screen bg-[hsl(217,33%,9%)] text-white">
      <div className="mx-auto w-full max-w-2xl px-4 py-10">
        <h1 className="text-3xl font-bold">Create a New Offer</h1>
        <WalletGate>
          <p className="mt-2 text-white/70">
            Define the title and budget in TON. On-chain escrow will lock funds from your wallet.
          </p>

          <div className="mt-6 space-y-4">
            <div>
              <label className="mb-2 block text-sm text-white/70">Title</label>
              <Input
                value={title}
                onChange={(e) => setTitle(e.target.value)}
                placeholder="Landing page design"
                className="bg-white/5 text-white border-white/10"
              />
            </div>
            <div>
              <label className="mb-2 block text-sm text-white/70">
                Description
              </label>
              <textarea
                value={description}
                onChange={(e) => setDescription(e.target.value)}
                placeholder="Describe the scope, deliverables, and milestones"
                className="min-h-28 w-full rounded-md bg-white/5 text-white border border-white/10 px-3 py-2 text-sm outline-none focus:ring-2 focus:ring-primary/40"
              />
            </div>
            <div>
              <label className="mb-2 block text-sm text-white/70">Stack</label>
              <Input
                value={stack}
                onChange={(e) => setStack(e.target.value)}
                placeholder="e.g. React, Node.js, TON"
                className="bg-white/5 text-white border-white/10"
              />
            </div>
            <div>
              <label className="mb-2 block text-sm text-white/70">
                Budget (TON)
              </label>
              <Input
                type="number"
                step="0.01"
                value={budget}
                onChange={(e) => setBudget(e.target.value)}
                className="bg-white/5 text-white border-white/10"
              />
            </div>
            <Button
              onClick={submit}
              disabled={loading || !title}
              className="bg-primary text-primary-foreground"
            >
              {loading ? "Creating..." : "Create Offer"}
            </Button>
            <div className="text-xs text-white/50">
              All fields can be edited later.
            </div>
          </div>
        </WalletGate>
      </div>
    </div>
  );
}
