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


const cbor1 = process.env.CBOR1!;
const cbor2 = process.env.CBOR2!;

const hgeScript1: Script = {
  type: "PlutusV2",
  script: cbor1,
};

const scriptAddress1 = lucid.utils.validatorToAddress(hgeScript1);

console.log(`HGE Address: ${scriptAddress1}`);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const adminAddress = await lucid.wallet.address();

const utxos = await lucid.utxosAt(scriptAddress1);
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

const updatedFields = [...oldDatum.fields]; // Clone fields


updatedFields[7] = new Constr(1, []); // reservationStatus = true
// updatedFields[9]=fromText("54321"); // reservationId


const updatedDatum = new Constr(0, updatedFields);

const redeemer = Data.to(new Constr(5, []));

// const amount1 = 10_000_000; // 10 ADA
// const tx1 = await lucid
//   .newTx()
//   .collectFrom([matchedUtxo], redeemer)
//   .attachSpendingValidator(hgeScript1)
//   .addSigner(adminAddress)
//   .payToContract(
//     scriptAddress1,
//     { inline: Data.to(updatedDatum) },
//     {
//       lovelace: BigInt(amount1),
//     }
//   )
//   .complete();
// const signedTx1 = await tx1.sign().complete();
// const txHash1 = await signedTx1.submit();
// console.log(`Transaction submitted Smart Contract1: ${txHash1}`);


const hgeScript2: Script = {
  type: "PlutusV2",
  script: cbor2
};

const scriptAddress2 = lucid.utils.validatorToAddress(hgeScript2);

console.log(`HGE2 Address: ${scriptAddress2}`);



const { paymentCredential } = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const adminPKH = paymentCredential?.hash;

const guestAddress = oldDatum.fields[0] as string;
//console.log("Guest Address:", toText(guestAddress));
console.log("Guest Address:", guestAddress);

const stringifiedGuestAddress = toText(guestAddress);

console.log("Stringified Guest Address:", stringifiedGuestAddress);

const initiateCheckIn = oldDatum.fields[7] as Constr<Data>;
console.log("Initiate CheckIn:", (initiateCheckIn));
const boolCheckIn =
  initiateCheckIn.index === 1 ? new Constr(1, []) : new Constr(0, []);

const digitalKey = fromText(""); // You can replace this as needed
const isDigitalKeyValidated = new Constr(0, []); // false

console.log("adminPKH:", adminPKH);

// Construct GuestDatum for smart contract 2
const guestDatum = new Constr(0, [
  fromText(stringifiedGuestAddress), // guestAddress
  boolCheckIn, // initiateCheckIn
  adminPKH!, // adminPKH
  digitalKey,
  isDigitalKeyValidated,
]);

console.log("New GuestDatum to submit:", guestDatum);

//Submit to Smart Contract 2
const amount2 = 10_000_000; // 10 ADA
const tx = await lucid
  .newTx()
  .payToContract(
    scriptAddress2,
    { inline: Data.to(guestDatum) },
    { lovelace: BigInt(amount2) }
  )
  .addSigner(adminAddress)
  .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log(`Transaction submitted to Smart Contract 2: ${txHash}`);