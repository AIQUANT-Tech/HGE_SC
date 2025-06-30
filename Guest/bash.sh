# 1. Build the script address
cardano-cli address build \
  --payment-script-file IdentityContract.plutus \
  --out-file script.addr \
  --testnet-magic 1
 
# 2. Generate a new payment keypair
cardano-cli address key-gen \
  --verification-key-file guest.vkey \
  --signing-key-file guest.skey
 
# 3. Build your payment address
cardano-cli address build \
  --payment-verification-key-file guest.vkey \
  --out-file admin.addr \
  --testnet-magic 1
 
  ### Generate public key hash
  cardano-cli address key-hash \
  --payment-verification-key-file guest.vkey
 
# 4. Lock funds at the script address
cardano-cli conway transaction build \
  --tx-in $(cardano-cli query utxo \
      --address $(< admin.addr) \
      --testnet-magic 1 \
      --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
      --output-json \
    | jq -r 'keys[0]') \
  --tx-out $(< key-script.addr)+4000000 \
  --tx-out-inline-datum-file datum.json \
  --change-address $(< admin.addr) \
  --out-file lock.tx \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket
 
# 5. Sign the locking transaction
cardano-cli conway transaction sign \
  --tx-file lock.tx \
  --signing-key-file admin.skey \
  --testnet-magic 1 \
  --out-file lock.tx
 
# 6. Submit the locking transaction
cardano-cli conway transaction submit \
  --tx-file lock.tx \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket
 
# 7. Verify UTXO at the script address
cardano-cli query utxo \
  --address $(< eternl.addr) \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
  --output-json
 
# 8. Build the “guess” transaction
# cardano-cli conway transaction build \
#   --tx-in $(cardano-cli conway query utxo \
#       --address $(< script.addr) \
#       --testnet-magic 1 \
#       --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
#       --output-json \
#     | jq -r 'keys[0]') \
#   --tx-in-collateral $(cardano-cli conway query utxo \
#       --address $(< payment.addr) \
#       --testnet-magic 1 \
#       --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
#       --output-json \
#     | jq -r 'keys[0]') \
#   --tx-in-script-file guessingGame.plutus \
#   --tx-in-inline-datum-present \
#   --tx-in-redeemer-file redeemer.json \
#   --change-address $(< payment.addr) \
#   --tx-out $(< payment.addr)+9664370 \
#   --out-file unlock.tx \
#   --testnet-magic 1 \
#   --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket
 
 
cardano-cli conway transaction build \
  --tx-in $(cardano-cli conway query utxo \
      --address $(< script.addr) \
      --testnet-magic 1 \
      --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
      --output-json \
    | jq -r 'keys[0]') \
  --tx-in-script-file IdentityContract.plutus\
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in-collateral $(cardano-cli conway query utxo \
      --address $(< seller.addr) \
      --testnet-magic 1 \
      --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
      --output-json \
    | jq -r 'keys[0]') \
  --tx-out $(< script.addr)+1176630 \
  --tx-out-inline-datum-file datum.json \
  --change-address $(< seller.addr) \
  --required-signer-hash $(cardano-cli address key-hash --payment-verification-key-file seller.vkey) \
  --out-file unlock.tx \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket
 
 
# 9. Sign the guess transaction
cardano-cli conway transaction sign \
  --tx-file unlock.tx \
  --signing-key-file guest1.skey \
  --testnet-magic 1 \
  --out-file unlock.tx
 
# 10. Submit the guess transaction
cardano-cli conway transaction submit \
  --tx-file unlock.tx \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket
 
# 11. Confirm funds returned
cardano-cli query utxo \
  --address $(< payment.addr) \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
  --out-file NewDatum.json
 
 
 
###Include buyer and seller vkey
 
cardano-cli conway transaction build \
  --tx-in $(cardano-cli conway query utxo \
      --address $(< key-script.addr) \
      --testnet-magic 1 \
      --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
      --output-json \
    | jq -r 'keys[0]') \
  --tx-in-collateral $(cardano-cli conway query utxo \
      --address $(< admin.addr) \
      --testnet-magic 1 \
      --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket \
      --output-json \
    | jq -r 'keys[0]') \
  --tx-in-script-file KeyContract.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --change-address $(< admin.addr) \
  --required-signer-hash $(cardano-cli address key-hash --payment-verification-key-file guest1.vkey) \
  --out-file unlock.tx \
  --testnet-magic 1 \
  --socket-path /home/$USER/git/cardano-node/preprod/db/node.socket
 






 cardano-cli conway transaction build \
  --tx-in 2e18b8d050bffd1ac254211f740bb8f806ae0b2acec9f16221a07d22a227cde1#1 \
  --tx-out $(< payment2.addr)+5000000 \
  --change-address $(< eternl.addr) \
  --out-file tx.raw