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

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_API_KEY!
  ),
  "Preprod"
);

const cbor = process.env.CBOR1!;
const guestScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(guestScript);
lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);

export async function fullReservation(
  guestAddress: string,
  roomId: string,
  checkInDate: string,
  checkOutDate: string,
  reservationId: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();
  const utxos = await lucid.utxosAt(scriptAddress);

  // Match UTXO with guest address (datum.fields[0])
  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guest");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;

  // Clone existing fields
  const fields = [...oldDatum.fields];

  // ================================
  // Update reservation field (index 3)
  // ================================
  const newReservation = new Constr(0, [
    new Constr(1, []), // isReserved = True
    new Constr(1, []), // reservationStatus = True
    fromText(reservationId),
    fromText(roomId),
    fromText(checkInDate),
    fromText(checkOutDate),
  ]);

  fields[3] = newReservation;

  // Create new datum
  const updatedDatum = new Constr(0, fields);
  

  // Correct redeemer index for FullReservation (0)
  const redeemer = Data.to(new Constr(0, []));

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(guestScript)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress,
      { inline: Data.to(updatedDatum) },
      matchedUtxo.assets
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(` Reservation submitted. TX Hash: ${txHash}`);
  return txHash;
}

// fullReservation(
//   "Zimba",
//   "Room101",
//   "2023-10-01",
//   "2023-10-05",
//   "Resv12345"
// ).then((txHash) => {
//   console.log("🎉 Full Reservation TX Hash:", txHash);
// }).catch((err) => {
//   console.error("❌ Error in fullReservation:", err.message);
// });
