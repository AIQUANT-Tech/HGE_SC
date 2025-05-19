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

const cbor = process.env.CBOR1!;

const hgeScript: Script = {
  type: "PlutusV2",
  script: cbor,
};

const scriptAddress = lucid.utils.validatorToAddress(hgeScript);

//console.log(`HGE Address: ${scriptAddress}`);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

//const adminAddress = await lucid.wallet.address();

export async function submitIdentity(
  guestAddress: string,
  guestName: string,
  passportNumber: string,
  photoHash: string
): Promise<string> {
  const adminAddress = await lucid.wallet.address();
  const utxos = await lucid.utxosAt(scriptAddress);

  const matchedUtxo = utxos.find((utxo) => {
    if (!utxo.datum) return false;
    const datum = Data.from(utxo.datum) as Constr<Data>;
    return toText(datum.fields[0] as string) === guestAddress;
  });

  if (!matchedUtxo) {
    throw new Error("No matching UTXO found for the guest address");
  }

  const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
  const updatedFields = [...oldDatum.fields];

  updatedFields[1] = fromText(guestName); // guestName
  updatedFields[2] = fromText(passportNumber); // passportNumber
  updatedFields[3] = fromText(photoHash); // photoHash

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(1, [])); // Custom redeemer

  const amount = 10_000_000; // 10 ADA

  const tx = await lucid
    .newTx()
    .collectFrom([matchedUtxo], redeemer)
    .attachSpendingValidator(hgeScript)
    .addSigner(adminAddress)
    .payToContract(
      scriptAddress,
      { inline: Data.to(updatedDatum) },
      { lovelace: BigInt(amount) }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log(`Submit Identity. Transaction submitted: ${txHash}`);
  const result = `Submit Identity. Transaction submitted: ${txHash}`;
  return result;
}

// const testConfirmReservation = await submitIdentity(
//   "New Town",
//   "rajesh",
//   "EXAMPLE12345678",
//   "photoHash12345678"
// );
