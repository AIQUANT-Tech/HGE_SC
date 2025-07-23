import {
  Lucid,
  Blockfrost,
  Script,
  Constr,
  Data,
  fromText,
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

lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);
const adminAddress = await lucid.wallet.address();

const guestScript: Script = {
  type: "PlutusV2",
  script: process.env.CBOR1!,
};

const scriptAddress = lucid.utils.validatorToAddress(guestScript);

function decodeField(field: any): string {
  try {
    if (typeof field === "string") return toText(field);
    if (typeof field === "object" && field?.bytes) return toText(field.bytes);
    return "";
  } catch {
    return "";
  }
}

function generateDigitalKeyFromDatum(identity: Constr<Data>): string {
  const guestNameHex = identity.fields[0];
  const guestName = decodeField(guestNameHex);
  const randomCode = Math.random().toString(36).substring(2, 7).toUpperCase();
  return `${guestName}-${randomCode}`;
}

export async function generateAndValidateDigitalKey(
  guestAddress: string,
  userId: string
): Promise<{txHash:string,digitalKey:string}> {
  const utxos = await lucid.utxosAt(scriptAddress);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return (
      toText(datum.fields[5] as string) === userId &&
      toText(datum.fields[0] as string) === guestAddress
    );
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guest");
  }

  const datum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const identity = datum.fields[2] as Constr<Data>;
  const reservation = datum.fields[3] as Constr<Data>;

  const digitalKey = generateDigitalKeyFromDatum(identity);

  const updatedKeyInfo = new Constr(0, [
    new Constr(1, []), // initiateCheckIn = true
    fromText(digitalKey),
    new Constr(1, []), // isDigitalKeyValidated = true
  ]);

  const updatedDatum = new Constr(0, [
    datum.fields[0], // guestAddress
    datum.fields[1], // adminPKH
    identity,
    reservation,
    updatedKeyInfo,
    datum.fields[5]
  ]);

  const redeemer = Data.to(new Constr(2, [])); // GenerateAndValidateKey

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

  console.log("✅ Digital Key Generated:", digitalKey);
  console.log("✅ Transaction submitted:", txHash);

  return {
    txHash,
    digitalKey
  };
}


// generateAndValidateDigitalKey("ABCDEF");