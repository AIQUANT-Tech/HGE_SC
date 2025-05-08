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
    "preprodaObP3ncIxrfcxDhiWCDVYdsV6974tS4z"
  ),
  "Preprod"
);


const cbor2 = process.env.CBOR2!;

const hgeScript2: Script = {
  type: "PlutusV2",
  script: cbor2
};

const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);

console.log(`HGE2 Address: ${scriptAddress2}`);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const adminAddress = await lucid.wallet.address();

const utxos = await lucid.utxosAt(scriptAddress2);
const matchedUtxo = utxos.find((utxo) => {
  if (!utxo.datum) return false;
  const datum = Data.from(utxo.datum) as Constr<Data>;
  return (
    // toText(datum.fields[0] as string) ===
    // "SaltLake Kolkata West Bengal India, 700135"
    toText(datum.fields[0] as string) ===
    "SaltLake Kolkata West Bengal India, 700135"
  );
});

if (!matchedUtxo) {
  console.log("No matching UTXO found");
  process.exit(1);
}

console.log(matchedUtxo);

const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;

const updatedFields = [...oldDatum.fields]; 
updatedFields[4] = new Constr(1, []); // Update the address
const updatedDatum = new Constr(0, updatedFields);

const redeemer = Data.to(new Constr(2, []));

const amount = 10_000_000; // 10 ADA
const tx = await lucid
  .newTx()
  .collectFrom([matchedUtxo], redeemer)
  .attachSpendingValidator(hgeScript2)
  .addSigner(adminAddress)
  .payToContract(
    scriptAddress2,
    { inline: Data.to(updatedDatum) },
    {
      lovelace: BigInt(amount),
    }
  )
  .complete();
const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log(`Transaction submitted: ${txHash}`);



