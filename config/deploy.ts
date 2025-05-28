import {
    credentialToRewardAddress,
    Lucid,
    Script,
    scriptFromNative,
    TxComplete,
    validatorToAddress
} from "npm:@lucid-evolution/lucid";
import {validatorToScriptHash} from "npm:@lucid-evolution/lucid";
import {BuiltValidators, DeployedValidators, ScriptNames} from "./types.ts";
import {getLucid} from "./lucid.ts";
import {generateConfigJson} from "./config.ts";
import {setupWallet} from "./wallet.ts";
import {
    InstantOrderInstantOrder,
    InstantOrderBatchWitness,
} from "../plutus.ts";

export class Deployment {
    lucid: Lucid;

    constructor(lucid: Lucid) {
        this.lucid = lucid;
    }

    build(): BuiltValidators {
        const witnessScript = new InstantOrderBatchWitness();
        const witnessScriptHash = validatorToScriptHash(witnessScript);
        const orderScript = new InstantOrderInstantOrder({
            Inline: [
                {
                    ScriptCredential: [witnessScriptHash],
                },
            ],
        });
        const orderScriptHash = validatorToScriptHash(orderScript);
        return {
            instantOrder: {
                script: orderScript,
                hash: orderScriptHash,
            },
            instantOrderWitness: {
                script: witnessScript,
                hash: witnessScriptHash,
            },
        }
    }

    async deploy(builtValidators: BuiltValidators): Promise<TxComplete> {
        const ns: Script = scriptFromNative({
            type: 'before',
            slot: 0,
        });
        const lockScript = validatorToAddress("Mainnet", ns);
        const instantOrderAddr = credentialToRewardAddress("Mainnet", {
            type: "Script",
            hash: builtValidators.instantOrder.hash
        });
        const instantOrderWitnessAddr = credentialToRewardAddress("Mainnet", {
            type: "Script",
            hash: builtValidators.instantOrderWitness.hash
        });
        const tx = await this.lucid
            .newTx()
            .pay.ToAddressWithData(
                lockScript,
                {kind: "inline", value: "00"},
                undefined,
                builtValidators.instantOrder.script,
            )
            .pay.ToAddressWithData(
                lockScript,
                {kind: "inline", value: "00"},
                undefined,
                builtValidators.instantOrderWitness.script,
            )
            // .registerStake(instantOrderAddr)
            // .registerStake(instantOrderWitnessAddr)
            .complete();

        return tx;
    }
}

async function getDeployedValidators(
    lucid: Lucid,
    builtValidators: BuiltValidators,
    deployedValidatorsTxId: string,
): Promise<DeployedValidators> {
    try {
        const builtValidatorsKeys = Object.keys(builtValidators) as ScriptNames[];
        const utxosByOutRefsRequest = builtValidatorsKeys.map((_, index) => ({
            txHash: deployedValidatorsTxId,
            outputIndex: index,
        }));

        const validatorsUtxos = await lucid.utxosByOutRef(utxosByOutRefsRequest);

        return builtValidatorsKeys.reduce((
            acc,
            key: ScriptNames,
            index,
        ) => {
            const {script, hash} = builtValidators[key];
            const referenceUtxo = validatorsUtxos[index];

            return {
                [key]: {
                    script,
                    hash,
                    referenceUtxo,
                },
                ...acc,
            };
        }, {} as DeployedValidators);
    } catch (error) {
        console.error('Failed to get deployed validators:', error);
        throw error;
    }
}

async function main() {
    const lucid = await getLucid();
    await setupWallet(lucid);
    const deployment = new Deployment(lucid);
    const builtValidators = deployment.build();
    const deployTx = await deployment.deploy(builtValidators);
    const deployTxId = await (await deployTx.sign.withWallet().complete()).submit();
    console.log('Deployment Tx ID:', deployTxId);
    // Here we need to wait until contracts are deployed
    await lucid.awaitTx(deployTxId);
    const deployedValidators = await getDeployedValidators(lucid, builtValidators, deployTxId);
    await generateConfigJson(deployedValidators);
}

main();