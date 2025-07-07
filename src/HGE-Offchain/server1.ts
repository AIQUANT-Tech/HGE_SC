import express from "express";
import dotenv from "dotenv";
dotenv.config();

import { initialSubmit } from "./initialSubmit"; // Step 1
import { fullReservation } from "./fullReservation"; // Step 2
import { fullIdentitySubmit } from "./fullIdentitySubmit"; // Step 3
import { generateAndValidateDigitalKey } from "./generateAndValidate"; // Step 4
import { checkOut } from "./checkOut"; // Step 5\
import {updateGuestAddress} from "./changeAddress";

const app = express();
app.use(express.json());

/**
 * 🟢 Step 1: Lock initial UTXO with guest and admin PkH
 */
app.post("/submit-initial", async (req, res) => {
  const { guestAddress,userId } = req.body;
  try {
    const txHash = await initialSubmit(guestAddress,userId);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

/**
 * 🟢 Step 2: Full reservation update
 */
app.post("/full-reservation", async (req, res) => {
  const {
    guestAddress,
    roomId,
    checkInDate,
    checkOutDate,
    reservationId,
    userId,
  } = req.body;
  try {
    const txHash = await fullReservation(
      guestAddress,
      userId,
      roomId,
      checkInDate,
      checkOutDate,
      reservationId
    );
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    console.error("❌ Full reservation error:", err.message); // <== ADD THIS
    res.status(500).json({ success: false, error: err.message }); // <== ADD ERROR MESSAGE
  }
});


/**
 * 🟢 Step 3: Submit full identity (name, passport, photoHash)
 */
app.post("/submit-full-identity", async (req, res) => {
  const { guestAddress, guestName, passportNumber, photoHash,userId } = req.body;
  try {
    const txHash = await fullIdentitySubmit(
      guestAddress,
      userId,
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
  const { guestAddress,userId } = req.body;
  try {
    const txHash = await generateAndValidateDigitalKey(guestAddress,userId);
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

app.post("/update-guest-address", async (req, res) => {
  const { newGuestAddress, userId } = req.body;

  if (!newGuestAddress || !userId) {
    return res.status(400).json({
      success: false,
      error: "Missing 'newGuestAddress' or 'userId' in request body",
    });
  }

  try {
    const txHash = await updateGuestAddress(newGuestAddress, userId);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    console.error("❌ Error in /update-guest-address:", err);
    res.status(500).json({
      success: false,
      error: err?.message || String(err),
    });
  }
});


const port = process.env.PORT || 5001;
app.listen(port, () => {
  console.log(`🚀 Unified HGE API server running on port ${port}`);
});
