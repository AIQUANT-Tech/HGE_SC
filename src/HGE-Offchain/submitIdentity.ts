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

const cbor = process.env.CBOR1!;

const hgeScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(hgeScript);

console.log(`HGE Address: ${scriptAddress}`);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const { paymentCredential } = lucid.utils.getAddressDetails(
  await lucid.wallet.address()
);
const adminPKH = paymentCredential?.hash;


// new Constr(0, []) // for false
// new Constr(1, []) // for true

const guestDatum = new Constr(0, [
  fromText("SaltLake Kolkata West Bengal India, 700135"), // guestAddress 0
  fromText("Alice"), // name 1
  fromText("P1123581321"), // passportNumber 2
  fromText("bafybeihash..."), // photoHash (IPFS or CID hash) 3
  new Constr(0, []), // isUserVerified 4
  new Constr(0, []), // identityStatus 5
  new Constr(0, []), // isReserved 6
  new Constr(0, []), // initiateCheckIn 7
  new Constr(0, []), // reservationStatus 8
  fromText(""), // reservationId 9
  fromText(""), // roomId 10
  fromText(""), // checkInDate 11
  fromText(""), // checkOutDate 12
  adminPKH || "", // adminPKH (hex-encoded PubKeyHash) 13
]);


const amount = 10_000_000; // 10 ADA
const tx = await lucid
  .newTx()
  .payToContract(
    scriptAddress,
    { inline: Data.to(guestDatum) },
    {
      lovelace: BigInt(amount),
    }
  )
  .complete();
const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log(`Transaction submitted: ${txHash}`);
