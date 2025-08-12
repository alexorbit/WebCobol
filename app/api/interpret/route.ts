import { type NextRequest, NextResponse } from "next/server"
import { CobolInterpreter } from "@/lib/cobol-interpreter"

export async function POST(request: NextRequest) {
  try {
    const { code } = await request.json()

    if (!code) {
      return NextResponse.json({ error: "Código COBOL é obrigatório" }, { status: 400 })
    }

    const interpreter = new CobolInterpreter()
    const result = interpreter.interpret(code)

    return NextResponse.json({
      success: result.success,
      output: result.output,
      error: result.error,
      interpreter: "WebCOBOL Interpreter v1.0",
      executionTime: Date.now(),
    })
  } catch (error: any) {
    console.error("Erro no interpretador:", error)
    return NextResponse.json(
      {
        success: false,
        error: "Erro interno do interpretador",
        details: error.message,
      },
      { status: 500 },
    )
  }
}

export async function GET() {
  return NextResponse.json({
    available: true,
    interpreter: "WebCOBOL Interpreter v1.0",
    message: "Interpretador COBOL nativo disponível",
    features: [
      "Parsing de sintaxe COBOL básica",
      "Execução de comandos DISPLAY, MOVE, ADD, SUBTRACT",
      "Suporte a variáveis PIC",
      "Execução de procedimentos PERFORM",
      "Compatível com ambiente serverless",
    ],
  })
}
