import { type NextRequest, NextResponse } from "next/server"

interface BlockchainConfig {
  name: string
  symbol: string
  initialSupply: string
  consensusType: "pow" | "pos"
  blockTime: string
  difficulty: string
  blockReward: string
  maxSupply: string
  decimals: string
  description: string
  features: string[]
}

export async function POST(request: NextRequest) {
  try {
    const config: BlockchainConfig = await request.json()

    // Gerar código COBOL baseado na configuração
    const cobolCode = generateCobolBlockchain(config)

    return NextResponse.json({
      success: true,
      code: cobolCode,
      config: config,
    })
  } catch (error) {
    console.error("Erro ao gerar blockchain:", error)
    return NextResponse.json({ success: false, error: "Erro interno do servidor" }, { status: 500 })
  }
}

function generateCobolBlockchain(config: BlockchainConfig): string {
  const consensusSection = config.consensusType === "pow" ? generatePoWConsensus(config) : generatePoSConsensus(config)

  const featuresSection = generateFeatures(config.features)

  return `IDENTIFICATION DIVISION.
PROGRAM-ID. ${config.name.toUpperCase().replace(/\s+/g, "-")}-BLOCKCHAIN.
AUTHOR. WebCOBOL Blockchain Generator.
DATE-WRITTEN. ${new Date().toISOString().split("T")[0]}.

*> ${config.description}
*> Blockchain: ${config.name}
*> Symbol: ${config.symbol}
*> Consensus: ${config.consensusType.toUpperCase()}

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. IBM-PC.
OBJECT-COMPUTER. IBM-PC.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> Blockchain Configuration
01 BLOCKCHAIN-CONFIG.
   05 BC-NAME              PIC X(50) VALUE "${config.name}".
   05 BC-SYMBOL            PIC X(10) VALUE "${config.symbol}".
   05 BC-INITIAL-SUPPLY    PIC 9(15) VALUE ${config.initialSupply}.
   05 BC-MAX-SUPPLY        PIC 9(15) VALUE ${config.maxSupply}.
   05 BC-DECIMALS          PIC 9(2) VALUE ${config.decimals}.
   05 BC-BLOCK-TIME        PIC 9(5) VALUE ${config.blockTime}.
   05 BC-DIFFICULTY        PIC 9(5) VALUE ${config.difficulty}.
   05 BC-BLOCK-REWARD      PIC 9(10) VALUE ${config.blockReward}.

*> Block Structure
01 BLOCK-RECORD.
   05 BLOCK-INDEX          PIC 9(10).
   05 BLOCK-TIMESTAMP      PIC 9(15).
   05 BLOCK-PREVIOUS-HASH  PIC X(64).
   05 BLOCK-MERKLE-ROOT    PIC X(64).
   05 BLOCK-NONCE          PIC 9(15).
   05 BLOCK-DIFFICULTY     PIC 9(5).
   05 BLOCK-HASH           PIC X(64).
   05 BLOCK-DATA           PIC X(1000).

*> Transaction Structure
01 TRANSACTION-RECORD.
   05 TX-ID                PIC X(64).
   05 TX-FROM              PIC X(64).
   05 TX-TO                PIC X(64).
   05 TX-AMOUNT            PIC 9(15)V9(${config.decimals}).
   05 TX-FEE               PIC 9(10)V9(${config.decimals}).
   05 TX-TIMESTAMP         PIC 9(15).
   05 TX-SIGNATURE         PIC X(128).

*> Blockchain State
01 BLOCKCHAIN-STATE.
   05 BS-CURRENT-BLOCK     PIC 9(10) VALUE 0.
   05 BS-TOTAL-SUPPLY      PIC 9(15) VALUE ${config.initialSupply}.
   05 BS-DIFFICULTY        PIC 9(5) VALUE ${config.difficulty}.
   05 BS-LAST-BLOCK-TIME   PIC 9(15).
   05 BS-HASH-RATE         PIC 9(15).

*> Mining Variables
01 MINING-VARS.
   05 MV-TARGET            PIC X(64).
   05 MV-ATTEMPTS          PIC 9(15).
   05 MV-START-TIME        PIC 9(15).
   05 MV-END-TIME          PIC 9(15).

*> Proof of Stake Variables
01 STAKE-VARS.
   05 SV-VALIDATOR-ADDRESS  PIC X(64).
   05 SV-STAKE-AMOUNT       PIC 9(15)V9(${config.decimals}).
   05 SV-SELECTION-HASH     PIC X(64).

${featuresSection}

*> Implementação inline das funções criptográficas
01 CRYPTO-VARS.
   05 HASH-INPUT           PIC X(2000).
   05 HASH-OUTPUT          PIC X(64).
   05 TEMP-STRING          PIC X(2000).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "Iniciando ${config.name} Blockchain...".
    DISPLAY "Símbolo: " BC-SYMBOL.
    DISPLAY "Supply Inicial: " BC-INITIAL-SUPPLY.
    DISPLAY "Consenso: ${config.consensusType.toUpperCase()}".
    
    PERFORM INITIALIZE-BLOCKCHAIN.
    PERFORM CREATE-GENESIS-BLOCK.
    PERFORM START-MINING-LOOP.
    
    STOP RUN.

INITIALIZE-BLOCKCHAIN.
    DISPLAY "Inicializando blockchain...".
    MOVE FUNCTION CURRENT-DATE TO BS-LAST-BLOCK-TIME.
    MOVE 0 TO BS-CURRENT-BLOCK.
    DISPLAY "Blockchain inicializado com sucesso.".

CREATE-GENESIS-BLOCK.
    DISPLAY "Criando bloco gênesis...".
    MOVE 0 TO BLOCK-INDEX.
    MOVE FUNCTION CURRENT-DATE TO BLOCK-TIMESTAMP.
    MOVE SPACES TO BLOCK-PREVIOUS-HASH.
    MOVE "Genesis Block - ${config.name}" TO BLOCK-DATA.
    
    PERFORM CALCULATE-MERKLE-ROOT.
    PERFORM MINE-BLOCK.
    
    ADD 1 TO BS-CURRENT-BLOCK.
    DISPLAY "Bloco gênesis criado: " BLOCK-HASH.

${consensusSection}

CALCULATE-MERKLE-ROOT.
    *> Implementação inline sem COPY
    STRING BLOCK-DATA DELIMITED BY SIZE
           BLOCK-TIMESTAMP DELIMITED BY SIZE
           INTO HASH-INPUT.
    
    PERFORM SIMPLE-HASH.
    MOVE HASH-OUTPUT TO BLOCK-MERKLE-ROOT.

*> Função de hash simplificada inline
SIMPLE-HASH.
    MOVE FUNCTION REVERSE(HASH-INPUT) TO HASH-OUTPUT.
    INSPECT HASH-OUTPUT REPLACING ALL " " BY "0".
    INSPECT HASH-OUTPUT REPLACING ALL LOW-VALUES BY "F".

VALIDATE-TRANSACTION.
    *> Validar assinatura digital
    *> Verificar saldo suficiente
    *> Verificar taxa de transação
    DISPLAY "Validando transação: " TX-ID.

ADD-TRANSACTION-TO-BLOCK.
    *> Adicionar transação ao bloco atual
    DISPLAY "Adicionando transação ao bloco: " TX-ID.

BROADCAST-BLOCK.
    *> Transmitir bloco para a rede
    DISPLAY "Transmitindo bloco: " BLOCK-INDEX.

START-MINING-LOOP.
    DISPLAY "Iniciando loop de mineração...".
    PERFORM UNTIL BS-CURRENT-BLOCK > 10
        PERFORM MINE-NEXT-BLOCK
        PERFORM ADJUST-DIFFICULTY
        PERFORM BROADCAST-BLOCK
    END-PERFORM.

MINE-NEXT-BLOCK.
    ADD 1 TO BLOCK-INDEX.
    MOVE FUNCTION CURRENT-DATE TO BLOCK-TIMESTAMP.
    MOVE BLOCK-HASH TO BLOCK-PREVIOUS-HASH.
    
    PERFORM MINE-BLOCK.
    ADD 1 TO BS-CURRENT-BLOCK.

ADJUST-DIFFICULTY.
    *> Ajustar dificuldade baseado no tempo do último bloco
    IF BLOCK-TIMESTAMP - BS-LAST-BLOCK-TIME > BC-BLOCK-TIME * 2
        SUBTRACT 1 FROM BS-DIFFICULTY
    ELSE
        IF BLOCK-TIMESTAMP - BS-LAST-BLOCK-TIME < BC-BLOCK-TIME / 2
            ADD 1 TO BS-DIFFICULTY
        END-IF
    END-IF.
    
    MOVE BLOCK-TIMESTAMP TO BS-LAST-BLOCK-TIME.

END PROGRAM ${config.name.toUpperCase().replace(/\s+/g, "-")}-BLOCKCHAIN.`
}

function generatePoWConsensus(config: BlockchainConfig): string {
  return `
MINE-BLOCK.
    DISPLAY "Minerando bloco " BLOCK-INDEX "...".
    MOVE 0 TO BLOCK-NONCE.
    MOVE BC-DIFFICULTY TO BLOCK-DIFFICULTY.
    
    PERFORM CALCULATE-TARGET.
    PERFORM UNTIL BLOCK-HASH < MV-TARGET OR BLOCK-NONCE > 100000
        ADD 1 TO BLOCK-NONCE
        ADD 1 TO MV-ATTEMPTS
        PERFORM CALCULATE-BLOCK-HASH
        
        *> Mostrar progresso a cada 1000 tentativas
        IF FUNCTION MOD(MV-ATTEMPTS, 1000) = 0
            DISPLAY "Tentativas: " MV-ATTEMPTS " Nonce: " BLOCK-NONCE
        END-IF
    END-PERFORM.
    
    DISPLAY "Bloco minerado! Hash: " BLOCK-HASH.
    DISPLAY "Nonce: " BLOCK-NONCE " Tentativas: " MV-ATTEMPTS.

CALCULATE-TARGET.
    *> Calcular target baseado na dificuldade
    MOVE ALL "F" TO MV-TARGET.
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > BLOCK-DIFFICULTY
        MOVE "0" TO MV-TARGET(I:1)
    END-PERFORM.

CALCULATE-BLOCK-HASH.
    *> Implementação inline sem COPY
    STRING BLOCK-INDEX DELIMITED BY SIZE
           BLOCK-TIMESTAMP DELIMITED BY SIZE
           BLOCK-PREVIOUS-HASH DELIMITED BY SIZE
           BLOCK-MERKLE-ROOT DELIMITED BY SIZE
           BLOCK-NONCE DELIMITED BY SIZE
           INTO HASH-INPUT.
    
    PERFORM SIMPLE-HASH.
    MOVE HASH-OUTPUT TO BLOCK-HASH.`
}

function generatePoSConsensus(config: BlockchainConfig): string {
  return `
MINE-BLOCK.
    DISPLAY "Validando bloco " BLOCK-INDEX " (PoS)...".
    
    PERFORM SELECT-VALIDATOR.
    PERFORM VALIDATE-STAKE.
    PERFORM CREATE-BLOCK-SIGNATURE.
    
    DISPLAY "Bloco validado por: " SV-VALIDATOR-ADDRESS.

SELECT-VALIDATOR.
    *> Seleção pseudo-aleatória baseada no stake
    PERFORM CALCULATE-SELECTION-HASH.
    *> Implementação simplificada - em produção seria mais complexa
    MOVE "VALIDATOR001" TO SV-VALIDATOR-ADDRESS.
    MOVE ${config.blockReward} TO SV-STAKE-AMOUNT.

VALIDATE-STAKE.
    *> Verificar se o validador tem stake suficiente
    IF SV-STAKE-AMOUNT >= ${config.blockReward}
        DISPLAY "Stake válido: " SV-STAKE-AMOUNT
    ELSE
        DISPLAY "Stake insuficiente!"
        STOP RUN
    END-IF.

CREATE-BLOCK-SIGNATURE.
    *> Implementação inline sem COPY
    STRING BLOCK-INDEX DELIMITED BY SIZE
           BLOCK-TIMESTAMP DELIMITED BY SIZE
           BLOCK-PREVIOUS-HASH DELIMITED BY SIZE
           BLOCK-MERKLE-ROOT DELIMITED BY SIZE
           SV-VALIDATOR-ADDRESS DELIMITED BY SIZE
           INTO HASH-INPUT.
    
    PERFORM SIMPLE-HASH.
    MOVE HASH-OUTPUT TO BLOCK-HASH.

CALCULATE-SELECTION-HASH.
    STRING FUNCTION CURRENT-DATE DELIMITED BY SIZE
           BLOCK-PREVIOUS-HASH DELIMITED BY SIZE
           INTO HASH-INPUT.
    
    PERFORM SIMPLE-HASH.
    MOVE HASH-OUTPUT TO SV-SELECTION-HASH.`
}

function generateFeatures(features: string[]): string {
  let featuresCode = ""

  if (features.includes("Smart Contracts")) {
    featuresCode += `
*> Smart Contracts Support
01 SMART-CONTRACT.
   05 SC-ADDRESS           PIC X(64).
   05 SC-CODE              PIC X(2000).
   05 SC-STATE             PIC X(1000).
   05 SC-OWNER             PIC X(64).`
  }

  if (features.includes("Multi-Signature")) {
    featuresCode += `
*> Multi-Signature Support
01 MULTISIG-WALLET.
   05 MS-REQUIRED-SIGS     PIC 9(2).
   05 MS-TOTAL-SIGNERS     PIC 9(2).
   05 MS-SIGNERS OCCURS 10 TIMES.
      10 MS-SIGNER-ADDRESS PIC X(64).
      10 MS-SIGNATURE      PIC X(128).`
  }

  if (features.includes("Staking Rewards")) {
    featuresCode += `
*> Staking Rewards
01 STAKING-POOL.
   05 SP-TOTAL-STAKED      PIC 9(15)V9(8).
   05 SP-REWARD-RATE       PIC 9(3)V9(4).
   05 SP-LAST-UPDATE       PIC 9(15).`
  }

  return featuresCode
}
