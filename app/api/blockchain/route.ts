import { type NextRequest, NextResponse } from "next/server"
import crypto from "crypto"

interface Block {
  index: number
  timestamp: number
  data: string
  previousHash: string
  hash: string
  nonce: number
}

class Blockchain {
  private chain: Block[] = []
  private difficulty = 2

  constructor() {
    this.chain = [this.createGenesisBlock()]
  }

  private createGenesisBlock(): Block {
    const block: Block = {
      index: 0,
      timestamp: Date.now(),
      data: "Genesis Block - WebCOBOL Blockchain",
      previousHash: "0",
      hash: "",
      nonce: 0,
    }
    block.hash = this.calculateHash(block)
    return block
  }

  private calculateHash(block: Block): string {
    return crypto
      .createHash("sha256")
      .update(block.index + block.timestamp + block.data + block.previousHash + block.nonce)
      .digest("hex")
  }

  private mineBlock(block: Block): Block {
    const target = Array(this.difficulty + 1).join("0")

    while (block.hash.substring(0, this.difficulty) !== target) {
      block.nonce++
      block.hash = this.calculateHash(block)
    }

    return block
  }

  addBlock(data: string): Block {
    const previousBlock = this.chain[this.chain.length - 1]
    const newBlock: Block = {
      index: previousBlock.index + 1,
      timestamp: Date.now(),
      data,
      previousHash: previousBlock.hash,
      hash: "",
      nonce: 0,
    }

    const minedBlock = this.mineBlock(newBlock)
    this.chain.push(minedBlock)
    return minedBlock
  }

  getChain(): Block[] {
    return this.chain
  }

  isChainValid(): boolean {
    for (let i = 1; i < this.chain.length; i++) {
      const currentBlock = this.chain[i]
      const previousBlock = this.chain[i - 1]

      if (currentBlock.hash !== this.calculateHash(currentBlock)) {
        return false
      }

      if (currentBlock.previousHash !== previousBlock.hash) {
        return false
      }
    }
    return true
  }
}

// Instância global do blockchain
let blockchain = new Blockchain()

export async function GET() {
  return NextResponse.json({
    chain: blockchain.getChain(),
    isValid: blockchain.isChainValid(),
    length: blockchain.getChain().length,
  })
}

export async function POST(request: NextRequest) {
  try {
    const { data, action } = await request.json()

    if (action === "add-block") {
      if (!data) {
        return NextResponse.json({ error: "Dados do bloco são obrigatórios" }, { status: 400 })
      }

      const newBlock = blockchain.addBlock(data)
      return NextResponse.json({
        success: true,
        block: newBlock,
        chain: blockchain.getChain(),
      })
    }

    if (action === "reset") {
      blockchain = new Blockchain()
      return NextResponse.json({
        success: true,
        message: "Blockchain resetado",
        chain: blockchain.getChain(),
      })
    }

    return NextResponse.json({ error: "Ação não reconhecida" }, { status: 400 })
  } catch (error: any) {
    console.error("Erro no blockchain:", error)
    return NextResponse.json({ error: "Erro interno do servidor" }, { status: 500 })
  }
}
