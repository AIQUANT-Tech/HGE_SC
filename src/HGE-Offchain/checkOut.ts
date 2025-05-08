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
import { decodeField } from "./deserializeDatum";
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

//console.log("Decode Datum:", decodeField(oldDatum));




const updatedFields = [...oldDatum.fields];


    updatedFields[6] = new Constr(0, []); // initiateCheckIn = false
    updatedFields[7] = new Constr(0, []); // reservationStatus = false
    updatedFields[8] = new Constr(0, []); // isReserved = false
    updatedFields[9] = fromText("");     // reservationId = ""
    updatedFields[10] = fromText("");     // roomId = ""
    updatedFields[11] = fromText("");     // checkInDate = ""
    updatedFields[12] = fromText("");     // checkOutDate = ""
    const updatedDatum = new Constr(0, updatedFields);
    // console.log("Updated Datum:", decodeField(updatedDatum));
    

const redeemer = Data.to(new Constr(6, []));

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
