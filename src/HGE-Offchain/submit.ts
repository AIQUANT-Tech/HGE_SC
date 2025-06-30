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

// Initialize Lucid
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_API_KEY!
  ),
  "Preprod"
);

// Load Admin Wallet
const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

const utxos = await lucid.wallet.getUtxos();
console.log("Wallet UTXOs:", utxos);

// Load Plutus Script CBOR (Digital Key Validator)
const cbor = process.env.CBOR2!;
const digitalKeyScript: Script = {
  type: "PlutusV2",
  script: cbor,
};
const scriptAddress = lucid.utils.validatorToAddress(digitalKeyScript);

// 🔐 Lock initial UTXO at Digital Key Validator
export async function lockDigitalKeyUTXO(
  guestAddress: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();
  const { paymentCredential } = lucid.utils.getAddressDetails(adminAddress);
  const adminPKH = paymentCredential?.hash || "";


 const datum = new Constr(0, [
  fromText(guestAddress), // guestAddress: BuiltinByteString
  new Constr(0,[]), // initiateCheckIn: Bool
  adminPKH, // adminPKH: PubKeyHash is already correct
  fromText(""), // digitalKey: BuiltinByteString
  new Constr(0,[]), // isDigitalKeyValidated: Bool
]);

  const tx = await lucid
    .newTx()
    .payToContract(
      scriptAddress,
      { inline: Data.to(datum) },
      { lovelace: BigInt(10_000_000) } // 10 ADA
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Initial Digital Key UTXO locked: ${txHash}`);
  return txHash;
}

// Optional: call this directly
// await lockDigitalKeyUTXO("New Town");

// lockDigitalKeyUTXO("Hello");
