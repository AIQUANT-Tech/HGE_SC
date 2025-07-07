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

// Lucid and script setup
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

export async function updateGuestAddress(
  newGuestAddress: string,
  userId: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();
  const utxos = await lucid.utxosAt(scriptAddress);

  // Find the UTXO for this guest using userId (index 5)
  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[5] as string) === userId;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for this userId");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const fields = [...oldDatum.fields];

  // Update guestAddress (index 0)
  fields[0] = fromText(newGuestAddress);

  const updatedDatum = new Constr(0, fields);

  // Redeemer index 4 corresponds to UpdateAddress
  const redeemer = Data.to(new Constr(4, []));

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

  console.log("✅ Guest address updated. TX Hash:", txHash);
  return txHash;
}

// 🧪 Example call:
// updateGuestAddress("NewZimbaAddress", "guest@example.com");
