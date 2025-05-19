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

const cbor1 = process.env.CBOR1!;
const cbor2 = process.env.CBOR2!;
const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);
const adminAddress = await lucid.wallet.address();

const hgeScript1: Script = {
  type: "PlutusV2",
  script: cbor1,
};

const hgeScript2: Script = {
  type: "PlutusV2",
  script: cbor2,
};

const scriptAddress1 = lucid.utils.validatorToAddress(hgeScript1);
const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);

function isTrue(constr: any): boolean {
  return constr?.index === 1;
}

export async function generateDigitalKey(
  guestAddress: string
): Promise<string> {
  const utxos1 = await lucid.utxosAt(scriptAddress1);
  const utxos2 = await lucid.utxosAt(scriptAddress2);

  const matchedUtxo1 = utxos1.find((utxo) => {
    if (!utxo.datum) return false;

    const datum = Data.from(utxo.datum) as Constr<Data>;
    const addressMatch = toText(datum.fields[0] as string) === guestAddress;
    const identityVerified = isTrue(datum.fields[4]); // isIdentityApproved
    const userVerified = isTrue(datum.fields[5]); // isUserVerified

    return addressMatch && identityVerified && userVerified;
  });

  if (!matchedUtxo1) {
    throw new Error("No matching UTXO1 found for the provided guest address");
  }

  const oldDatum1 = Data.from(matchedUtxo1.datum!) as Constr<Data>;
  //console.log("Old Datum1:", oldDatum1);
  const digitalKey: string = generateDigitalKeyFromDatum(oldDatum1);
  console.log("Digital key:", digitalKey);

  const matchedUtxo2 = utxos2.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo2) {
    throw new Error("No matching UTXO2 found for the provided guest address");
  }

  const oldDatum2 = Data.from(matchedUtxo2.datum!) as Constr<Data>;
  //console.log("Old Datum2:", oldDatum2);

  const updatedFields2 = [...oldDatum2.fields];

  //Update digital key field (index 3)
  updatedFields2[3] = fromText(digitalKey);

  const updatedDatum = new Constr(0, updatedFields2);
  const redeemer = Data.to(new Constr(1, []));

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo2], redeemer)
    .attachSpendingValidator(hgeScript2)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress2,
      { inline: Data.to(updatedDatum) },
      { lovelace: BigInt(10_000_000) }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Generate Digital Key. Transaction submitted: ${txHash}`);
  // const txHash = "dummy-tx-hash"; // Replace with actual txHash after signing and submitting
  const result: string = `Generate Digital Key. Transaction submitted: ${txHash}`;
  return result;
}

export const decodeField = (field: any): any => {
  if (typeof field === "string") {
    try {
      return toText(field); // decode hex to UTF-8 string
    } catch {
      return field;
    }
  } else if (field instanceof Constr) {
    return {
      index: field.index,
      fields: field.fields.map(decodeField),
    };
  } else {
    return field;
  }
};

export const generateDigitalKeyFromDatum = (datum: Constr<any>): string => {
  const decoded1 = decodeField(datum.fields[1]); // e.g., Name
  const decoded9 = decodeField(datum.fields[9]); // e.g., Passport No
  const decoded10 = decodeField(datum.fields[10]); // e.g., DOB

  console.log(`${decoded1}-${decoded9}-${decoded10}`);
  const digitalKey = `${decoded1}-${decoded9}-${decoded10}`;
  return digitalKey;
};

//const testGenerateDigitalKey = await generateDigitalKey("New Town");
