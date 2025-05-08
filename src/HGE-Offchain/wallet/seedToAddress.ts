import { Lucid, Blockfrost } from "lucid-cardano";
import { promises as fs } from "fs";

async function main() {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      "preprodaObP3ncIxrfcxDhiWCDVYdsV6974tS4z"
    ),
    "Preprod"
  );

  // read seed phrase from file and print address

  try {
    const seed = await fs.readFile("seed.txt");
    lucid.selectWalletFromSeed(seed.toString());
    const address = await lucid.wallet.address();
    const utxos = await lucid.utxosAt(address);
    console.log("UTXOs:", utxos);
    console.log(`Address: ${address}`);
  } catch (error) {
    console.error("Error writing seed phrase to file:", error);
  }
}

main();
