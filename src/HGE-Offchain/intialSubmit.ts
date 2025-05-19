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
    process.env.BLOCKFROST_API_KEY!
  ),
  "Preprod"
);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const cbor = process.env.CBOR1!;
const hgeScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(hgeScript);

export async function intialSubmit(
  guestAddress: string
): Promise<string> {
  const { paymentCredential } = lucid.utils.getAddressDetails(
    await lucid.wallet.address()
  );
  const adminPKH = paymentCredential?.hash || "";

  const guestDatum = new Constr(0, [
    fromText(guestAddress),
    fromText(""),
    fromText(""),
    fromText(""),
    new Constr(0, []), // isUserVerified
    new Constr(0, []), // identityStatus
    new Constr(0, []), // isReserved
    new Constr(0, []), // initiateCheckIn
    new Constr(0, []), // reservationStatus
    fromText(""), // reservationId
    fromText(""), // roomId
    fromText(""), // checkInDate
    fromText(""), // checkOutDate
    adminPKH,
  ]);

  const amount = 10_000_000; // 10 ADA

  const tx = await lucid
    .newTx()
    .payToContract(
      scriptAddress,
      { inline: Data.to(guestDatum) },
      { lovelace: BigInt(amount) }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Intial detail submitted: ${txHash}`);
  const result = `Intial detail submitted: ${txHash}`;
  return result;
}

//const testintialSubmit = await intialSubmit("New Town1");