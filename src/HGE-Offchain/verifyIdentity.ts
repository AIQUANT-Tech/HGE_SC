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

export async function verifyIdentity(guestAddress: string): Promise<string> {
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

  const updatedFields = [...oldDatum.fields]; // clone all fields
  updatedFields[4] = new Constr(1, []); // set isUserVerified = true

  const updatedDatum = new Constr(0, updatedFields);

  const redeemer = Data.to(new Constr(2, [])); // dummy redeemer

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

  console.log(`Verify Identity.  Transaction submitted: ${txHash}`);
  return txHash;
}

//const testverifyIdentity =await verifyIdentity("New Town");
