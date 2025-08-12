import { type NextRequest, NextResponse } from "next/server"
import crypto from "crypto"

interface Vote {
  id: string
  voter: string
  candidate: string
  timestamp: number
  signature: string
}

interface VotingContract {
  votes: Vote[]
  candidates: string[]
  isActive: boolean
}

// Estado global do contrato de votação
let votingContract: VotingContract = {
  votes: [],
  candidates: ["Alice", "Bob", "Charlie"],
  isActive: true,
}

function createVoteSignature(vote: Omit<Vote, "signature">): string {
  return crypto
    .createHash("sha256")
    .update(vote.id + vote.voter + vote.candidate + vote.timestamp)
    .digest("hex")
}

export async function GET() {
  const results = votingContract.candidates.map((candidate) => ({
    candidate,
    votes: votingContract.votes.filter((v) => v.candidate === candidate).length,
  }))

  return NextResponse.json({
    contract: votingContract,
    results,
    totalVotes: votingContract.votes.length,
  })
}

export async function POST(request: NextRequest) {
  try {
    const { action, voter, candidate } = await request.json()

    if (action === "vote") {
      if (!votingContract.isActive) {
        return NextResponse.json({ error: "Votação encerrada" }, { status: 400 })
      }

      if (!voter || !candidate) {
        return NextResponse.json({ error: "Eleitor e candidato são obrigatórios" }, { status: 400 })
      }

      if (!votingContract.candidates.includes(candidate)) {
        return NextResponse.json({ error: "Candidato inválido" }, { status: 400 })
      }

      // Verificar se o eleitor já votou
      const hasVoted = votingContract.votes.some((v) => v.voter === voter)
      if (hasVoted) {
        return NextResponse.json({ error: "Eleitor já votou" }, { status: 400 })
      }

      const vote: Omit<Vote, "signature"> = {
        id: crypto.randomUUID(),
        voter,
        candidate,
        timestamp: Date.now(),
      }

      const voteWithSignature: Vote = {
        ...vote,
        signature: createVoteSignature(vote),
      }

      votingContract.votes.push(voteWithSignature)

      return NextResponse.json({
        success: true,
        vote: voteWithSignature,
        message: "Voto registrado com sucesso",
      })
    }

    if (action === "reset") {
      votingContract = {
        votes: [],
        candidates: ["Alice", "Bob", "Charlie"],
        isActive: true,
      }

      return NextResponse.json({
        success: true,
        message: "Votação resetada",
      })
    }

    if (action === "close") {
      votingContract.isActive = false
      return NextResponse.json({
        success: true,
        message: "Votação encerrada",
      })
    }

    return NextResponse.json({ error: "Ação não reconhecida" }, { status: 400 })
  } catch (error: any) {
    console.error("Erro na votação:", error)
    return NextResponse.json({ error: "Erro interno do servidor" }, { status: 500 })
  }
}
