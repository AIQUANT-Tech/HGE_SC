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

export async function validateDigitalKey(
  guestAddress: string
): Promise<string> {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      process.env.BLOCKFROST_API_KEY!
    ),
    "Preprod"
  );

  const hgeScript2: Script = {
    type: "PlutusV2",
    script: process.env.CBOR2!,
  };

  const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);
  const adminSeed = process.env.ADMIN_SEED!;
  lucid.selectWalletFromSeed(adminSeed);
  const adminAddress = await lucid.wallet.address();

  const utxos = await lucid.utxosAt(scriptAddress2);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for the guest address");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const updatedFields = [...oldDatum.fields];

  updatedFields[4] = new Constr(1, []); // Set "Digital Key Validated" flag

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(2, [])); // Use correct redeemer for validation

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(hgeScript2)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress2,
      { inline: Data.to(updatedDatum) },
      { lovelace: BigInt(10_000_000) } // Adjust amount as needed
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Digital Key validated. TxHash: ${txHash}`);
  const result = `Digital Key validated. TxHash: ${txHash}`;
  return result;
}

//const testGenerateDigitalKey = await validateDigitalKey("New Town");
