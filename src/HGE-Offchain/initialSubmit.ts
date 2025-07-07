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

// Setup Lucid with Blockfrost

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_API_KEY!
  ),
  "Preprod"
);

// Select admin wallet
const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);
const address = await lucid.wallet.address();
console.log("Admin Address:", address);

// Load the Plutus V2 script from environment
const cbor = process.env.CBOR1!;
const guestScript: Script = {
  type: "PlutusV2",
  script: cbor,
};
const scriptAddress = lucid.utils.validatorToAddress(guestScript);

// Initial Submit Function

export async function initialSubmit(
  guestAddress: string,
  userId: string // 🛠️ should be lowercase 'string' (TS primitive)
): Promise<string> {
  const { paymentCredential } = lucid.utils.getAddressDetails(address);
  const adminPKH = paymentCredential?.hash!;

  // === IdentityInfo
  const identity = new Constr(0, [
    fromText(""), // name
    fromText(""), // passportNumber
    fromText(""), // photoHash
    new Constr(0, []), // isUserVerified = False
    new Constr(0, []), // identityStatus = False
  ]);

  // === ReservationInfo
  const reservation = new Constr(0, [
    new Constr(0, []), // isReserved = False
    new Constr(0, []), // reservationStatus = False
    fromText(""), // reservationId
    fromText(""), // roomId
    fromText(""), // checkInDate
    fromText(""), // checkOutDate
  ]);

  // === KeyInfo
  const keyInfo = new Constr(0, [
    new Constr(0, []), // initiateCheckIn = False
    fromText(""), // digitalKey
    new Constr(0, []), // isDigitalKeyValidated = False
  ]);

  // === GuestDatum
  const guestDatum = new Constr(0, [
    fromText(guestAddress), // guestAddress
    adminPKH, // adminPKH (already BuiltinByteString)
    identity,
    reservation,
    keyInfo,
    fromText(userId), // userId (email or guest id)
  ]);

  // === Submit Transaction
  const tx = await lucid
    .newTx()
    .payToContract(
      scriptAddress,
      { inline: Data.to(guestDatum) },
      { lovelace: 5_000_000n }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log("✅ InitialSubmit TX Hash:", txHash);
  return txHash;
}

// Optional auto-run
// initialSubmit("Zimba", "guest@example.com");
