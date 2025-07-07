import { Lucid, Blockfrost, Script } from "lucid-cardano";
import dotenv from "dotenv";
dotenv.config();

async function fetchUtxosAtScript(cbor: string, label: string) {
  const script: Script = { type: "PlutusV2", script: cbor };
  const address = lucid.utils.validatorToAddress(script);
  console.log(`📍 ${label} Address: ${address}`);
  const utxos = await lucid.utxosAt(address);
  console.log(`💾 ${label} UTXOs:`, utxos);
}

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_API_KEY!
  ),
  "Preprod"
);

await fetchUtxosAtScript(process.env.CBOR1!, "HGE Script 1");

