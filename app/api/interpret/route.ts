import { type NextRequest, NextResponse } from "next/server"
import { CobolInterpreter } from "../../../src/interpreter"

export async function POST(request: NextRequest) {
  try {
    const { code } = await request.json()

    if (!code) {
      return NextResponse.json({ success: false, error: "No code provided" }, { status: 400 })
    }

    const interpreter = new CobolInterpreter()
    const result = interpreter.interpret(code)

    return NextResponse.json(result)
  } catch (error: any) {
    return NextResponse.json({ success: false, error: error.message }, { status: 500 })
  }
}
