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

export async function checkOutSC1(guestAddress: string): Promise<string> {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      process.env.BLOCKFROST_API_KEY!
    ),
    "Preprod"
  );

  const hgeScript1: Script = {
    type: "PlutusV2",
    script: process.env.CBOR1!,
  };

  const scriptAddress1 = lucid.utils.validatorToAddress(hgeScript1);
  lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);
  const adminAddress = await lucid.wallet.address();

  const utxos = await lucid.utxosAt(scriptAddress1);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guest in SC-1");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const updatedFields = [...oldDatum.fields];

  updatedFields[6] = new Constr(0, []); // initiateCheckIn = false
  updatedFields[7] = new Constr(0, []); // reservationStatus = false
  updatedFields[8] = new Constr(0, []); // isReserved = false
  updatedFields[9] = fromText(""); // reservationId = ""
  updatedFields[10] = fromText(""); // roomId = ""
  updatedFields[11] = fromText(""); // checkInDate = ""
  updatedFields[12] = fromText(""); // checkOutDate = ""

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(7, []));

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
  console.log(`SC-1 Check-out submitted: ${txHash}`);
  const result = `SC-1 Check-out submitted: ${txHash}`;

  return result;
}

export async function checkOutSC2(guestAddress: string): Promise<string> {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      process.env.BLOCKFROST_API_KEY!
    ),
    "Preprod"
  );

  const hgeScript2: Script = {
    type: "PlutusV2",
    script: process.env.CBOR2!,
  };

  const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);
  lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);
  const adminAddress = await lucid.wallet.address();

  const utxos = await lucid.utxosAt(scriptAddress2);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for guest in SC-2");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const updatedFields = [...oldDatum.fields];

  updatedFields[3] = fromText(""); // digitalKeyHash = ""
  updatedFields[4] = new Constr(0, []); // isDigitalKeyValidate = false

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(3, []));

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
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
  console.log(`SC-2 Check-out submitted: ${txHash}`);
  const result = `SC-2 Check-out submitted: ${txHash}`;
  return result;
}

// export async function checkOutAll(guestAddress: string): Promise<string> {
//   // const tx1 = await checkOutSC1(guestAddress);
//   // console.log("Waiting 5 seconds for SC1 confirmation...");
//   // await new Promise((res) => setTimeout(res, 5000));
//   const tx2 = await checkOutSC2(guestAddress);
//   console.log("Waiting 5 seconds for SC1 confirmation...");
//   await new Promise((res) => setTimeout(res, 5000));
//   //console.log(`Check-out transactions submitted: SC-1: ${tx1}, SC-2: ${tx2}`);
//   return `Check-out transactions submitted: SC-2: ${tx2}`;
// }

//checkOutAll("New Town1")
