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

    const seed = lucid.utils.generateSeedPhrase();

    try {
        await fs.writeFile("seed.txt", seed);
        console.log("Seed phrase written to seed.txt");
    } catch (error) {
        console.error("Error writing seed phrase to file:", error);
    }
}

main();
