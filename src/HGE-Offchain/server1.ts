import express from "express";
import dotenv from "dotenv";
dotenv.config();

import { initialSubmit } from "./initialSubmit"; // Step 1
import { fullReservation } from "./fullReservation"; // Step 2
import { fullIdentitySubmit } from "./fullIdentitySubmit"; // Step 3
import { generateAndValidateDigitalKey } from "./generateAndValidate"; // Step 4
import { checkOut } from "./checkOut"; // Step 5

const app = express();
app.use(express.json());

/**
 * 🟢 Step 1: Lock initial UTXO with guest and admin PkH
 */
app.post("/submit-initial", async (req, res) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await initialSubmit(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

/**
 * 🟢 Step 2: Full reservation update
 */
app.post("/full-reservation", async (req, res) => {
  const { guestAddress, roomId, checkInDate, checkOutDate, reservationId } =
    req.body;
  try {
    const txHash = await fullReservation(
      guestAddress,
      roomId,
      checkInDate,
      checkOutDate,
      reservationId
    );
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

/**
 * 🟢 Step 3: Submit full identity (name, passport, photoHash)
 */
app.post("/submit-full-identity", async (req, res) => {
  const { guestAddress, guestName, passportNumber, photoHash } = req.body;
  try {
    const txHash = await fullIdentitySubmit(
      guestAddress,
      guestName,
      passportNumber,
      photoHash
    );
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

/**
 * 🟢 Step 4: Generate and validate digital key
 */
app.post("/generate-validate-key", async (req, res) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await generateAndValidateDigitalKey(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

/**
 * 🟢 Step 5: Checkout to clear reservation & key fields
 */
app.post("/check-out", async (req, res) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await checkOut(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    console.error("❌ Error in /check-out:", err);
    res.status(500).json({
      success: false,
      error: err?.message || String(err) || "Unknown error",
    });
  }
});

const port = process.env.PORT || 5001;
app.listen(port, () => {
  console.log(`🚀 Unified HGE API server running on port ${port}`);
});
