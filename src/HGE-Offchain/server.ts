import express from "express";
import { reserveRoom } from "./reserveRoom";
import dotenv from "dotenv";
import { intialSubmit } from "./intialSubmit";
import { confirmReservation } from "./confirmReservation";
import { submitIdentity } from "./submitIdentity";
import { verifyIdentity } from "./verifyIdentity";
import { confirmIdentity } from "./confirmIdentity";
import { processGuestCheckInFlow } from "./initiateCheckIn";
import { generateDigitalKey } from "./generateDigitalKey";
import { validateDigitalKey } from "./validateDigitalKey";
import { checkOutAll } from "./checkOut";
const app = express();
dotenv.config();

app.use(express.json());

//1.Initial Submit
app.post("/intial-submit", async (req: any, res: any) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await intialSubmit(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//2.Reserve Room
app.post("/reserve-room", async (req: any, res: any) => {
  const { guestAddress, roomId, checkInDate, checkOutDate } = req.body;

  try {
    const txHash = await reserveRoom(
      guestAddress,
      roomId,
      checkInDate,
      checkOutDate
    );
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//3.Confirm Reservation
app.post("/confirm-reservation", async (req: any, res: any) => {
  const { guestAddress, reservationId } = req.body;
  try {
    const txHash = await confirmReservation(guestAddress, reservationId);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//4.Submit Identity
app.post("/submit-identity", async (req: any, res: any) => {
  const { guestAddress, guestName, passportNumber, photoHash } = req.body;
  try {
    const txHash = await submitIdentity(
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

//5. Verify Identity
app.post("/verify-identity", async (req: any, res: any) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await verifyIdentity(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//6. Confirm Identity
app.post("/confirm-identity", async (req: any, res: any) => {
  const { guestAddress, isUserVerified } = req.body;
  try {
    const txHash = await confirmIdentity(guestAddress, isUserVerified);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//7. Initiate Check In
app.post("/initiate-check-in", async (req: any, res: any) => {
  const { guestAddress } = req.body;
  try {
    // const { txHash,updatedDatum } = await initiateCheckIn(
    //   guestAddress);

    const txHash = await processGuestCheckInFlow(guestAddress);

    if (typeof txHash === "string" && txHash.startsWith("Check-in failed:")) {
      throw new Error(txHash);
    }

    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//8. Generate Digital Key
app.post("/generate-digital-key", async (req: any, res: any) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await generateDigitalKey(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//9. Validate Digital Key
app.post("/validate-digital-key", async (req: any, res: any) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await validateDigitalKey(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

//10. checkOut
app.post("/check-out", async (req: any, res: any) => {
  const { guestAddress } = req.body;
  try {
    const txHash = await checkOutAll(guestAddress);
    res.status(200).json({ success: true, txHash });
  } catch (err: any) {
    res.status(500).json({ success: false, error: err.message });
  }
});

app.listen(process.env.PORT, () =>
  console.log(`API server running on port ${process.env.PORT}`)
);
