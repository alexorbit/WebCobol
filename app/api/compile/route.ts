import { type NextRequest, NextResponse } from "next/server"
import { exec } from "child_process"
import { promisify } from "util"
import fs from "fs/promises"
import path from "path"
import crypto from "crypto"
import { CobolInterpreter } from "@/lib/cobol-interpreter"

const execAsync = promisify(exec)

export async function POST(request: NextRequest) {
  try {
    const { code } = await request.json()

    if (!code) {
      return NextResponse.json({ error: "Código COBOL é obrigatório" }, { status: 400 })
    }

    const interpreterUsed = "GnuCOBOL"
    try {
      // Verificar se GnuCOBOL está disponível
      await execAsync("cobc --version")
    } catch (error) {
      console.log("GnuCOBOL não disponível, usando interpretador nativo...")

      const interpreter = new CobolInterpreter()
      const result = interpreter.interpret(code)

      return NextResponse.json({
        success: result.success,
        output: result.output,
        error: result.error,
        interpreter: "WebCOBOL Interpreter v1.0 (Serverless)",
        executionTime: Date.now(),
        fallback: false,
      })
    }

    // Criar arquivo temporário
    const tempId = crypto.randomUUID()
    const tempDir = path.join(process.cwd(), "temp")
    const cobolFile = path.join(tempDir, `${tempId}.cob`)
    const execFile = path.join(tempDir, `${tempId}`)

    // Garantir que o diretório temp existe
    await fs.mkdir(tempDir, { recursive: true })

    // Escrever código COBOL
    await fs.writeFile(cobolFile, code)

    try {
      // Compilar com GnuCOBOL
      const compileCommand = `cobc -x -o "${execFile}" "${cobolFile}"`
      const { stdout: compileOutput, stderr: compileError } = await execAsync(compileCommand)

      if (compileError && !compileError.includes("warning")) {
        return NextResponse.json({
          success: false,
          error: compileError,
          output: compileOutput,
        })
      }

      // Executar programa compilado
      const { stdout: execOutput, stderr: execError } = await execAsync(`"${execFile}"`)

      // Limpar arquivos temporários
      try {
        await fs.unlink(cobolFile)
        await fs.unlink(execFile)
      } catch (cleanupError) {
        console.warn("Erro ao limpar arquivos temporários:", cleanupError)
      }

      return NextResponse.json({
        success: true,
        output: execOutput,
        warnings: compileError || null,
        executionTime: Date.now(),
        interpreter: interpreterUsed,
      })
    } catch (error: any) {
      // Limpar arquivos em caso de erro
      try {
        await fs.unlink(cobolFile)
        await fs.unlink(execFile)
      } catch (cleanupError) {
        console.warn("Erro ao limpar arquivos temporários:", cleanupError)
      }

      return NextResponse.json({
        success: false,
        error: error.message,
        output: error.stdout || "",
      })
    }
  } catch (error: any) {
    console.error("Erro na compilação:", error)
    return NextResponse.json({ error: "Erro interno do servidor" }, { status: 500 })
  }
}

export async function GET() {
  try {
    await execAsync("cobc --version")
    return NextResponse.json({
      available: true,
      message: "GnuCOBOL está disponível",
      interpreter: "GnuCOBOL + WebCOBOL Interpreter",
    })
  } catch (error) {
    return NextResponse.json({
      available: true,
      message: "WebCOBOL Interpreter disponível (modo serverless)",
      interpreter: "WebCOBOL Interpreter v1.0",
      note: "Interpretador nativo funcionando em ambiente serverless",
    })
  }
}
