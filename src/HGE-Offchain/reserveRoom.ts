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

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const cbor = process.env.CBOR1!;
const hgeScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(hgeScript);

// // === Utility to check if a Constr is `true` (index 1) ===
// function isTrue(constr: any): boolean {
//   return constr?.index === 1;
// }

// === Main Function ===
export async function reserveRoom(
  guestAddress: string,
  roomId: string,
  checkInDate: string,
  checkOutDate: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();

  const utxos = await lucid.utxosAt(scriptAddress);
  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guestAddress");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const updatedFields = [...oldDatum.fields]; // clone

  // Only update if isUserVerified and identityStatus are both true
  // if (isTrue(updatedFields[4]) && isTrue(updatedFields[5])) {
  updatedFields[6] = new Constr(1, []); // reservationStatus = true

  updatedFields[10] = fromText(roomId);
  updatedFields[11] = fromText(checkInDate);
  updatedFields[12] = fromText(checkOutDate);
  // } else {
  //   throw new Error("User is not verified or identity not approved");
  // }

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(4, [])); // custom redeemer index

  const amount = 10_000_000; // 10 ADA
  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(hgeScript)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress,
      { inline: Data.to(updatedDatum) },
      { lovelace: BigInt(amount) }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Transaction submitted for reservation room: ${txHash}`);
  return `Transaction submitted for reservation room: ${txHash}`;
}

//const testReserveRoom = await reserveRoom("New Town", "12345", "2023-10-01", "2023-10-05");
