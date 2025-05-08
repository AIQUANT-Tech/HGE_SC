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

const utxos = await lucid.utxosAt(scriptAddress);
console.log("UTXOs:", utxos);
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
console.log("Old Datum:", oldDatum);

export const decodeField = (field: any): any => {
  if (typeof field === "string") {
    // Try converting hex to text
    try {
      return toText(field);
    } catch {
      return field; // If not text, return raw
    }
  } else if (field instanceof Constr) {
    return {
      index: field.index,
      fields: field.fields.map(decodeField),
    };
  } else {
    return field; // For numbers, nulls, etc.
  }
};
const decodedDatum = decodeField(oldDatum);
console.log("Decoded Datum:", decodedDatum);
