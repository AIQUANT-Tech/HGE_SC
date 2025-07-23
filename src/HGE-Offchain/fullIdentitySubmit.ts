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

const cbor = process.env.CBOR1!;
const guestScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(guestScript);
lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);

export async function fullIdentitySubmit(
  guestAddress: string,
  userId: string,
  name: string,
  passportNumber: string,
  photoHash: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();
  const utxos = await lucid.utxosAt(scriptAddress);

  // Find the correct UTXO (roomId == guestAddress)
  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return (
      toText(datum.fields[5] as string) === userId &&
      toText(datum.fields[0] as string) === guestAddress
    );
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guest identity update");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const fields = [...oldDatum.fields];


  // Update identity field (index 2)
 
  const identityInfo = new Constr(0, [
    fromText(name),
    fromText(passportNumber),
    fromText(photoHash),
    new Constr(1, []),
    new Constr(1, []),
  ]);

  fields[2] = identityInfo;

  const updatedDatum = new Constr(0, fields);

  // Correct redeemer index for FullIdentitySubmit (1)
  const redeemer = Data.to(new Constr(1, []));

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(guestScript)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress,
      { inline: Data.to(updatedDatum) },
      matchedUtxo.assets
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(` Identity info submitted. TX Hash: ${txHash}`);
  return txHash;
}

// 🧪 Run the function with sample data
// fullIdentitySubmit(
//   "Zimba", // roomId or unique guest key
//   "Alice Johnson", // name
//   "P123456789", // passport number
//   "f8d3b88101ae9b6e" // photo hash (SHA256 or IPFS hash)
// )
//   .then((txHash) => {
//     console.log("🎉 Full Identity Submit TX Hash:", txHash);
//   })
//   .catch((err) => {
//     console.error("❌ Error in fullIdentitySubmit:", err.message);
//   });
