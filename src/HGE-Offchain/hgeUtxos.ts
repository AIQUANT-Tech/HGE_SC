import {
  Lucid,
  Blockfrost,
  Script,
  Constr,
  fromText,
  Data,
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

const cbor1 = process.env.CBOR1!;

const hgeScript1: Script = {
  type: "PlutusV2",
  script: cbor1,
};

const scriptAddress1 = lucid.utils.validatorToAddress(hgeScript1);

console.log(`HGE Address: ${scriptAddress1}`);

const utxos1 = await lucid.utxosAt(scriptAddress1);
console.log("UTXOs:", utxos1);

const cbor2 = process.env.CBOR2!;

const hgeScript2: Script = {
  type: "PlutusV2",
  script: cbor2,
};

const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);

console.log(`HGE Address: ${scriptAddress2}`);

const utxos2 = await lucid.utxosAt(scriptAddress2);
console.log("UTXOs:", utxos2);
