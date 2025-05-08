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


const cbor = process.env.CBOR1!;

const hgeScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(hgeScript);

console.log(`HGE Address: ${scriptAddress}`);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const adminAddress = await lucid.wallet.address();

const utxos = await lucid.utxosAt(scriptAddress);
const matchedUtxo = utxos.find((utxo) => {
  if (!utxo.datum) return false;
  const datum = Data.from(utxo.datum) as Constr<Data>;
  return (
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

const updatedFields = [...oldDatum.fields]; // Clone fields
//updatedFields[4] = new Constr(1, []); // isUserVerified = true
function isTrue(constr: any): boolean {
  return constr?.index === 1;
}

if (isTrue(updatedFields[4]) && isTrue(updatedFields[5])) {
  updatedFields[6] = new Constr(1, []);
   //updatedFields[9]=fromText("54321"); // reservationId
  updatedFields[10]=fromText("12345"); // roomId
  updatedFields[11]=fromText("12/1/2025"); // checkInDate
  updatedFields[12]=fromText("14/1/2025"); // checkOutDate
}


const updatedDatum = new Constr(0, updatedFields);

const redeemer = Data.to(new Constr(3, []));

const amount = 10_000_000; // 10 ADA
const tx = await lucid
  .newTx()
  .collectFrom([matchedUtxo], redeemer)
  .attachSpendingValidator(hgeScript)
  .addSigner(adminAddress)
  .payToContract(
    scriptAddress,
    { inline: Data.to(updatedDatum) },
    {
      lovelace: BigInt(amount),
    }
  )
  .complete();
const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log(`Transaction submitted: ${txHash}`);
