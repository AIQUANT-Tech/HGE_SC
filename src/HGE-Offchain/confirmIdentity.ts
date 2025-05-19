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

// Initialize Lucid instance
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

// === Utility to check if a Constr is `true` (index 1) ===
function isTrue(constr: any): boolean {
  return constr?.index === 1;
}

// === Main Function ===
export async function confirmIdentity(
  guestAddress: string,
  isUserVerified: boolean
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

  // Only update identityStatus if isUserVerified is true
  if (isTrue(updatedFields[4]) && isUserVerified) {
    updatedFields[5] = new Constr(1, []); // identityStatus = true
  } else {
    throw new Error("Guest is not verified, cannot update identityStatus");
  }

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(3, []));

  const amount = 10_000_000;
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

  console.log(`Confirm Identity. Transaction submitted: ${txHash}`);
  const result = `Confirm Identity. Transaction submitted: ${txHash}`;
  return result;
}

//const testConfirmIdentity = await confirmIdentity("New Town", true);
