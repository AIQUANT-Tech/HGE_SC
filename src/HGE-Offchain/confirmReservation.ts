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

// console.log(`HGE Address: ${scriptAddress}`);

const adminSeed = process.env.ADMIN_SEED!;
lucid.selectWalletFromSeed(adminSeed);

export async function confirmReservation(
  guestAddress: string,
  reservationId: string
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

  updatedFields[8] = new Constr(1, []); // reservationStatus = true
  updatedFields[9] = fromText(reservationId); // reservationId

  const updatedDatum = new Constr(0, updatedFields);
  const redeemer = Data.to(new Constr(5, []));
  const amount = 10_000_000;
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

  console.log(`Reservation confirmed. Transaction submitted: ${txHash}`);
  const result = `Reservation confirmed. Transaction submitted: ${txHash}`;
  return result;
}

//const testConfirmReservation = await confirmReservation("New Town", "1234554321");
