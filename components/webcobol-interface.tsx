"use client"

import { useState, useEffect } from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"
import { Alert, AlertDescription } from "@/components/ui/alert"
import { Loader2, Play, Code, Blocks, Vote, Brain } from "lucide-react"
import dynamic from "next/dynamic"

const MonacoEditor = dynamic(() => import("@monaco-editor/react"), {
  ssr: false,
  loading: () => <div className="h-64 bg-gray-100 animate-pulse rounded" />,
})

interface CompileResult {
  success: boolean
  output: string
  error?: string
  warnings?: string
}

interface BlockchainData {
  chain: any[]
  isValid: boolean
  length: number
}

export function WebCOBOLInterface() {
  const [code, setCode] = useState(`IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
    DISPLAY "Hello, WebCOBOL-Blockchain!".
    STOP RUN.`)

  const [compileResult, setCompileResult] = useState<CompileResult | null>(null)
  const [isCompiling, setIsCompiling] = useState(false)
  const [blockchain, setBlockchain] = useState<BlockchainData | null>(null)
  const [isLoading, setIsLoading] = useState(false)
  const [aiResponse, setAiResponse] = useState<string>("")
  const [aiPrompt, setAiPrompt] = useState("")

  const compileCode = async () => {
    setIsCompiling(true)
    try {
      const response = await fetch("/api/compile", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code }),
      })

      const result = await response.json()
      setCompileResult(result)
    } catch (error) {
      setCompileResult({
        success: false,
        output: "",
        error: "Erro ao conectar com o servidor",
      })
    } finally {
      setIsCompiling(false)
    }
  }

  const loadBlockchain = async () => {
    setIsLoading(true)
    try {
      const response = await fetch("/api/blockchain")
      const data = await response.json()
      setBlockchain(data)
    } catch (error) {
      console.error("Erro ao carregar blockchain:", error)
    } finally {
      setIsLoading(false)
    }
  }

  const addBlock = async () => {
    try {
      const response = await fetch("/api/blockchain", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          action: "add-block",
          data: `COBOL Block - ${new Date().toISOString()}`,
        }),
      })

      if (response.ok) {
        loadBlockchain()
      }
    } catch (error) {
      console.error("Erro ao adicionar bloco:", error)
    }
  }

  const getAiAssist = async () => {
    if (!aiPrompt.trim()) return

    setIsLoading(true)
    try {
      const response = await fetch("/api/ai-assist", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          action: "assist",
          prompt: aiPrompt,
          code: code,
        }),
      })

      const result = await response.json()
      if (result.success) {
        setAiResponse(result.response)
      }
    } catch (error) {
      console.error("Erro na IA:", error)
    } finally {
      setIsLoading(false)
    }
  }

  useEffect(() => {
    loadBlockchain()
  }, [])

  return (
    <div className="max-w-7xl mx-auto space-y-6">
      <Tabs defaultValue="compiler" className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="compiler" className="flex items-center gap-2">
            <Code className="h-4 w-4" />
            Compilador
          </TabsTrigger>
          <TabsTrigger value="blockchain" className="flex items-center gap-2">
            <Blocks className="h-4 w-4" />
            Blockchain
          </TabsTrigger>
          <TabsTrigger value="voting" className="flex items-center gap-2">
            <Vote className="h-4 w-4" />
            Votação
          </TabsTrigger>
          <TabsTrigger value="ai" className="flex items-center gap-2">
            <Brain className="h-4 w-4" />
            IA Assistant
          </TabsTrigger>
        </TabsList>

        <TabsContent value="compiler" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Editor COBOL</CardTitle>
              <CardDescription>Escreva e compile código COBOL diretamente no navegador</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="h-64 border rounded">
                <MonacoEditor
                  height="100%"
                  defaultLanguage="cobol"
                  value={code}
                  onChange={(value) => setCode(value || "")}
                  options={{
                    minimap: { enabled: false },
                    fontSize: 14,
                    wordWrap: "on",
                  }}
                />
              </div>

              <Button onClick={compileCode} disabled={isCompiling} className="w-full">
                {isCompiling ? (
                  <>
                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                    Compilando...
                  </>
                ) : (
                  <>
                    <Play className="mr-2 h-4 w-4" />
                    Compilar e Executar
                  </>
                )}
              </Button>

              {compileResult && (
                <Alert className={compileResult.success ? "border-green-200" : "border-red-200"}>
                  <AlertDescription>
                    <div className="space-y-2">
                      <Badge variant={compileResult.success ? "default" : "destructive"}>
                        {compileResult.success ? "Sucesso" : "Erro"}
                      </Badge>
                      <pre className="text-sm bg-gray-50 p-2 rounded overflow-x-auto">
                        {compileResult.output || compileResult.error}
                      </pre>
                    </div>
                  </AlertDescription>
                </Alert>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="blockchain" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Blockchain WebCOBOL</CardTitle>
              <CardDescription>Visualize e interaja com o blockchain</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex gap-2">
                <Button onClick={loadBlockchain} disabled={isLoading}>
                  {isLoading ? <Loader2 className="mr-2 h-4 w-4 animate-spin" /> : null}
                  Atualizar
                </Button>
                <Button onClick={addBlock} variant="outline">
                  Adicionar Bloco
                </Button>
              </div>

              {blockchain && (
                <div className="space-y-2">
                  <div className="flex gap-4 text-sm">
                    <Badge>Blocos: {blockchain.length}</Badge>
                    <Badge variant={blockchain.isValid ? "default" : "destructive"}>
                      {blockchain.isValid ? "Válido" : "Inválido"}
                    </Badge>
                  </div>

                  <div className="max-h-64 overflow-y-auto space-y-2">
                    {blockchain.chain.map((block, index) => (
                      <div key={index} className="p-3 bg-gray-50 rounded text-sm">
                        <div className="font-mono">
                          <div>Bloco #{block.index}</div>
                          <div className="text-gray-600">Hash: {block.hash.substring(0, 16)}...</div>
                          <div className="text-gray-600">Data: {block.data}</div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="voting" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Sistema de Votação</CardTitle>
              <CardDescription>Smart contract de votação descentralizada</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="text-center py-8 text-gray-500">Sistema de votação em desenvolvimento...</div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="ai" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>IA Assistant</CardTitle>
              <CardDescription>Assistência inteligente para desenvolvimento COBOL</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex gap-2">
                <input
                  type="text"
                  placeholder="Faça uma pergunta sobre COBOL..."
                  value={aiPrompt}
                  onChange={(e) => setAiPrompt(e.target.value)}
                  className="flex-1 px-3 py-2 border rounded"
                  onKeyPress={(e) => e.key === "Enter" && getAiAssist()}
                />
                <Button onClick={getAiAssist} disabled={isLoading}>
                  {isLoading ? <Loader2 className="h-4 w-4 animate-spin" /> : <Brain className="h-4 w-4" />}
                </Button>
              </div>

              {aiResponse && (
                <Alert>
                  <AlertDescription>
                    <pre className="whitespace-pre-wrap text-sm">{aiResponse}</pre>
                  </AlertDescription>
                </Alert>
              )}
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
