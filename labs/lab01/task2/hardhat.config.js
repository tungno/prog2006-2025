// hardhat.config.js
require("@nomicfoundation/hardhat-toolbox");

module.exports = {
    solidity: "0.8.0",
    networks: {
        sepolia: {
            url: `https://sepolia.infura.io/v3/98271a9959184c19afb77a60e674f167`,
            accounts: [`b5921db0fe3a8069f993063bc6651a28e824b588e3ee77f3c5a711742401cfa4`]
        }
    }
};

/*
how to get this value: 
https://www.infura.io/
https://developer.metamask.io/key/98271a9959184c19afb77a60e674f167/all-endpoints
and get the api key: 
change https://sepolia.infura.io/v3/...
url: `https://sepolia.infura.io/v3/98271a9959184c19afb77a60e674f167`,


How to get private key from MetaMask ( foa test account)@
In MetaMask click the 3 dot => Account Ddetails => Show private key => enter password and hold to see it
accounts: [`b5921db0fe3a8069f993063bc6651a28e824b588e3ee77f3c5a711742401cfa4`]

*/