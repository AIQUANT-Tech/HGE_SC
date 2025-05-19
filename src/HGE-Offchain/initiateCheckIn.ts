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

// Shared Lucid + Wallet + Script Setup
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_API_KEY!
  ),
  "Preprod"
);

const cbor1 = process.env.CBOR1!;
const cbor2 = process.env.CBOR2!;
const hgeScript1: Script = { type: "PlutusV2", script: cbor1 };
const hgeScript2: Script = { type: "PlutusV2", script: cbor2 };

const scriptAddress1 = lucid.utils.validatorToAddress(hgeScript1);
const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const adminAddress = await lucid.wallet.address();

/**
 * Step 1: Update Smart Contract 1 with reservationStatus
 */
function isTrue(constr: any): boolean {
  return constr?.index === 1;
}

export async function initiateCheckIn(
  guestAddress: string
): Promise<{txHash:string, updatedDatum: Constr<Data> }> {
  const utxos = await lucid.utxosAt(scriptAddress1);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;

    const datum = Data.from(utxo.datum) as Constr<Data>;
    const addressMatch = toText(datum.fields[0] as string) === guestAddress;
    const identityVerified = isTrue(datum.fields[4]); // isIdentityApproved
    const userVerified = isTrue(datum.fields[5]); // isUserVerified

    return addressMatch && identityVerified && userVerified;
  });

  if (!matchedUtxo) {
    throw new Error("No verified UTXO found for the guest address");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const updatedFields = [...oldDatum.fields];

  updatedFields[7] = new Constr(1, []); // initiateCheckIn = true

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(6, []));

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(hgeScript1)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress1,
      { inline: Data.to(updatedDatum) },
      { lovelace: BigInt(10_000_000) }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Smart Contract 1 updated: ${txHash}`);
  return {txHash, updatedDatum };
}

/**
 * Step 2: Submit to Smart Contract 2 using data from updatedDatum
 */
export async function submitCheckInToContract2(
  updatedDatum: Constr<Data>
): Promise<string> {
  const guestAddress = updatedDatum.fields[0] as string;
  const initiateCheckIn = updatedDatum.fields[7] as Constr<Data>;

  lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);
  const { paymentCredential } = lucid.utils.getAddressDetails(adminAddress);
  const adminPKH = paymentCredential?.hash;

  const utxos = await lucid.utxosAt(scriptAddress2);

  // Check if a UTXO already exists with this guestAddress
  const existingUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    const existingGuestAddress = toText(datum.fields[0] as string);
    return existingGuestAddress === toText(guestAddress);
  });

  if (existingUtxo) {
    console.log(`Guest already checked in at SC-2: ${toText(guestAddress)}`);
    return `Guest already checked in at SC-2`;
  }

  const boolCheckIn =
    initiateCheckIn.index === 1 ? new Constr(1, []) : new Constr(0, []);
  const digitalKey = fromText(""); // Placeholder
  const isDigitalKeyValidated = new Constr(0, []);

  const guestDatum = new Constr(0, [
    fromText(toText(guestAddress)), // guestAddress
    boolCheckIn, // initiateCheckIn
    adminPKH!, // adminPKH
    digitalKey,
    isDigitalKeyValidated,
  ]);

  const tx = await lucid
    .newTx()
    .payToContract(
      scriptAddress2,
      { inline: Data.to(guestDatum) },
      { lovelace: BigInt(10_000_000) }
    )
    .addSigner(adminAddress)
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Smart Contract 2 submitted: ${txHash}`);
  return txHash;
}

export async function processGuestCheckInFlow(
  guestAddr: string
): Promise<string> {
  //console.log("Processing guest check-in flow...");

  let result = "";

  try {
    const {txHash, updatedDatum } = await initiateCheckIn(guestAddr);

    console.log("Waiting 40 seconds for SC1 confirmation...");
    await new Promise((res) => setTimeout(res, 40000));

    const txHash2 = await submitCheckInToContract2(updatedDatum);

    console.log("Smart Contract 1 TX:", txHash);
    console.log("Smart Contract 2 TX:", txHash2);
    result = `Smart Contract 1 TX: ${txHash}, Smart Contract 2 TX: ${txHash2}`;
    //result = `Smart Contract 2 TX: ${txHash2}`;
  } catch (err: any) {
    console.error("Check-in failed:", err.message);
    result = `Check-in failed: ${err.message}`;
  }

  return result;
}

//processGuestCheckInFlow("New Town")
