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

export async function generateAndValidateKey(
  guestAddress: string,
  digitalKey: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();
  const utxos = await lucid.utxosAt(scriptAddress);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("❌ No matching UTXO found for guest");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const fields = [...oldDatum.fields];

  // ================================
  // Update KeyInfo field (index 4)
  // ================================
  const updatedKeyInfo = new Constr(0, [
    new Constr(1, []), // initiateCheckIn = True
    fromText(digitalKey), // digitalKey = yourKey
    new Constr(1, []), // isDigitalKeyValidated = True
  ]);

  fields[4] = updatedKeyInfo;

  const updatedDatum = new Constr(0, fields);

  // Correct redeemer index for GenerateAndValidateKey (assumed index 3)
  const redeemer = Data.to(new Constr(2, []));

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

  console.log(`🔑 Digital Key Submitted. TX Hash: ${txHash}`);
  return txHash;
}

// 🧪 Example Usage
// generateAndValidateKey(
//   "Zimba", // guestAddress (roomId)
//   "key-abc123" // digital key to be stored
// )
//   .then((txHash) => {
//     console.log("🎉 Generate & Validate Key TX Hash:", txHash);
//   })
//   .catch((err) => {
//     console.error("❌ Error in generateAndValidateKey:", err);
//   });
