import type { RequestHandler } from "express";
import { beginCell } from "@ton/core";
import { APP_BASE_URL } from "../config";

export const buildPayload: RequestHandler = async (req, res) => {
  try {
    const opRaw = req.query.op as string;
    const roleRaw = (req.query.role as string) || "0";
    const whatRaw = (req.query.what as string) || "0";
    if (!opRaw) return res.status(400).json({ error: "op_required" });
    const op = Number(opRaw);
    const role = Number(roleRaw) | 0;
    const what = Number(whatRaw) | 0;
    if (!Number.isFinite(op)) return res.status(400).json({ error: "bad_op" });

    const cell = beginCell()
      .storeUint(op, 32)
      .storeUint(role, 8)
      .storeUint(what, 8)
      .endCell();
    const base64 = cell.toBoc().toString("base64");
    res.json({ base64 });
  } catch (e: any) {
    console.error("buildPayload error:", e);
    res.status(500).json({ error: "internal_error" });
  }
};

export const manifest: RequestHandler = async (req, res) => {
  try {
    const base = (APP_BASE_URL && APP_BASE_URL.trim()) || `${req.protocol}://${req.get("host")}`;
    const baseNoSlash = base.replace(/\/$/, "");
    const json = {
      url: baseNoSlash,
      name: "FreelTON",
      iconUrl: `${baseNoSlash}/placeholder.svg`,
      termsOfUseUrl: `${baseNoSlash}/terms`,
      privacyPolicyUrl: `${baseNoSlash}/privacy`,
    };
    res.setHeader("content-type", "application/json; charset=utf-8");
    res.setHeader("Access-Control-Allow-Origin", "*");
    res.status(200).send(JSON.stringify(json));
  } catch (e) {
    res.status(500).json({ error: "internal_error" });
  }
};
