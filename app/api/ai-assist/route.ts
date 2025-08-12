import { type NextRequest, NextResponse } from "next/server"
import axios from "axios"

const DEEPSEEK_API_URL = "https://api.deepseek.com/v1/chat/completions"

export async function POST(request: NextRequest) {
  try {
    const { prompt, code, action } = await request.json()

    const apiKey = process.env.DEEPSEEK_API_KEY
    if (!apiKey || apiKey.trim() === "") {
      return NextResponse.json(
        {
          error: "DEEPSEEK_API_KEY não configurada ou vazia. Configure a variável de ambiente na Vercel.",
          debug: "Variável existe: " + !!process.env.DEEPSEEK_API_KEY,
        },
        { status: 500 },
      )
    }

    let systemPrompt = ""
    let userPrompt = ""

    if (action === "assist") {
      systemPrompt = `Você é um especialista em COBOL e blockchain. Ajude com código COBOL, 
      explicações técnicas e melhores práticas. Responda em português de forma clara e concisa.`
      userPrompt = `Código atual:\n${code || "Nenhum código fornecido"}\n\nPergunta: ${prompt}`
    } else if (action === "autocomplete") {
      systemPrompt = `Complete o código COBOL fornecido de forma lógica e seguindo as melhores práticas. 
      Retorne apenas o código completado, sem explicações.`
      userPrompt = code || prompt
    } else if (action === "explain") {
      systemPrompt = `Explique o código COBOL fornecido de forma didática em português.`
      userPrompt = code || prompt
    }

    const response = await axios.post(
      DEEPSEEK_API_URL,
      {
        model: "deepseek-chat",
        messages: [
          { role: "system", content: systemPrompt },
          { role: "user", content: userPrompt },
        ],
        max_tokens: 1000,
        temperature: 0.7,
      },
      {
        headers: {
          Authorization: `Bearer ${apiKey}`,
          "Content-Type": "application/json",
        },
        timeout: 30000,
      },
    )

    const aiResponse = response.data.choices[0]?.message?.content || "Sem resposta da IA"

    return NextResponse.json({
      success: true,
      response: aiResponse,
      usage: response.data.usage,
    })
  } catch (error: any) {
    console.error("Erro na IA:", error)

    if (error.code === "ECONNABORTED") {
      return NextResponse.json({ error: "Timeout na conexão com a API DeepSeek" }, { status: 408 })
    }

    if (error.response?.status === 401) {
      return NextResponse.json(
        {
          error: "Chave da API DeepSeek inválida. Verifique se a DEEPSEEK_API_KEY está correta.",
          hint: "Obtenha uma chave válida em https://platform.deepseek.com",
        },
        { status: 401 },
      )
    }

    if (error.response?.status === 429) {
      return NextResponse.json(
        {
          error: "Limite de requisições excedido. Tente novamente em alguns minutos.",
        },
        { status: 429 },
      )
    }

    return NextResponse.json(
      {
        error: "Erro ao comunicar com a IA",
        details: error.message,
      },
      { status: 500 },
    )
  }
}

export async function GET() {
  const apiKey = process.env.DEEPSEEK_API_KEY

  return NextResponse.json({
    configured: !!apiKey && apiKey.trim() !== "",
    keyLength: apiKey ? apiKey.length : 0,
    keyPreview: apiKey ? `${apiKey.substring(0, 8)}...` : "não configurada",
    timestamp: new Date().toISOString(),
  })
}
