import {
  Lucid,
  Blockfrost,
  Script,
  Constr,
  fromText,
  Data,
  toText,
} from "lucid-cardano";
import dotenv from "dotenv";
dotenv.config();

export async function checkOut(guestAddress: string): Promise<string> {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      process.env.BLOCKFROST_API_KEY!
    ),
    "Preprod"
  );

  const hgeScript: Script = {
    type: "PlutusV2",
    script: process.env.CBOR1!,
  };

  const scriptAddress = lucid.utils.validatorToAddress(hgeScript);
  lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);
  const adminAddress = await lucid.wallet.address();

  const utxos = await lucid.utxosAt(scriptAddress);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guest");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const fields = [...oldDatum.fields];

  // ================================
  // Reset ReservationInfo (index 3)
  // ================================
  const clearedReservationInfo = new Constr(0, [
    new Constr(0, []), // isReserved = false
    new Constr(0, []), // reservationStatus = false
    fromText(""), // reservationId = ""
    fromText(""), // roomId = ""
    fromText(""), // checkInDate = ""
    fromText(""), // checkOutDate = ""
  ]);

  fields[3] = clearedReservationInfo;

  // ================================
  // Reset KeyInfo (index 4)
  // ================================
  const clearedKeyInfo = new Constr(0, [
    new Constr(0, []), // initiateCheckIn = false
    fromText(""), // digitalKey = ""
    new Constr(0, []), // isDigitalKeyValidated = false
  ]);

  fields[4] = clearedKeyInfo;

  const updatedDatum = new Constr(0, fields);
  const redeemer = Data.to(new Constr(3, [])); // CheckOut = 4

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(hgeScript)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress,
      { inline: Data.to(updatedDatum) },
      matchedUtxo.assets // preserving original funds
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`✅ Check-out submitted: ${txHash}`);
  return txHash;
}

// 🧪 Example usage:
// checkOut("ABCDEF")
//   .then((txHash) => {
//     console.log("🎉 Checkout TX Hash:", txHash);
//   })
//   .catch((err) => {
//     console.error("❌ Error in checkOut:", err);
//   });
