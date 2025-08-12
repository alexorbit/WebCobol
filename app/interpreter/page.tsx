"use client"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { Alert, AlertDescription } from "@/components/ui/alert"
import { Play, Download, RotateCcw, Copy, CheckCircle, AlertCircle } from "lucide-react"
import dynamic from "next/dynamic"

const MonacoEditor = dynamic(() => import("@monaco-editor/react"), {
  ssr: false,
  loading: () => <div className="h-96 bg-gray-100 animate-pulse rounded" />,
})

interface ExecutionResult {
  success: boolean
  output: string[]
  errors: string[]
  warnings: string[]
  executionTime: number
  memoryUsage: number
  variables: Record<string, any>
}

export default function InterpreterPage() {
  const [code, setCode] = useState(`IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(30) VALUE "Hello, WebCOBOL Interpreter!".
01 WS-COUNTER PIC 9(2) VALUE 1.

PROCEDURE DIVISION.
MAIN-LOGIC.
    DISPLAY WS-MESSAGE.
    PERFORM UNTIL WS-COUNTER > 5
        DISPLAY "Count: " WS-COUNTER
        ADD 1 TO WS-COUNTER
    END-PERFORM.
    DISPLAY "Program completed successfully!".
    STOP RUN.`)

  const [result, setResult] = useState<ExecutionResult | null>(null)
  const [isExecuting, setIsExecuting] = useState(false)
  const [copied, setCopied] = useState(false)

  const executeCode = async () => {
    setIsExecuting(true)
    try {
      const response = await fetch("/api/interpret", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code }),
      })

      const executionResult = await response.json()
      setResult(executionResult)
    } catch (error) {
      setResult({
        success: false,
        output: [],
        errors: [`Erro de conexão: ${error}`],
        warnings: [],
        executionTime: 0,
        memoryUsage: 0,
        variables: {},
      })
    } finally {
      setIsExecuting(false)
    }
  }

  const clearOutput = () => {
    setResult(null)
  }

  const copyOutput = () => {
    if (result) {
      const outputText = result.output.join("\n")
      navigator.clipboard.writeText(outputText)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    }
  }

  const downloadCode = () => {
    const blob = new Blob([code], { type: "text/plain" })
    const url = URL.createObjectURL(blob)
    const a = document.createElement("a")
    a.href = url
    a.download = "program.cob"
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
    URL.revokeObjectURL(url)
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-7xl mx-auto space-y-6">
        {/* Header */}
        <div className="text-center space-y-2">
          <h1 className="text-3xl font-bold text-gray-800">WebCOBOL Interpreter</h1>
          <p className="text-gray-600">Execute COBOL code directly in your browser</p>
          <Badge className="bg-blue-600">Serverless • Real-time • Open Source</Badge>
        </div>

        {/* Deploy Button */}
        <div className="text-center">
          <a
            href="https://vercel.com/new/clone?repository-url=https%3A%2F%2Fgithub.com%2FAKowaa%2FWCB-COBOL&env=DEEPSEEK_API_KEY&envDescription=DeepSeek%20API%20key%20for%20AI%20assistance&envLink=https%3A%2F%2Fplatform.deepseek.com"
            target="_blank"
            rel="noopener noreferrer"
          >
            <img src="https://vercel.com/button" alt="Deploy with Vercel" className="inline-block" />
          </a>
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {/* Code Editor */}
          <Card>
            <CardHeader>
              <div className="flex items-center justify-between">
                <CardTitle className="text-gray-800">COBOL Editor</CardTitle>
                <div className="flex gap-2">
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={downloadCode}
                    className="text-gray-600 border-gray-300 bg-transparent"
                  >
                    <Download className="w-4 h-4 mr-2" />
                    Download
                  </Button>
                  <Button onClick={executeCode} disabled={isExecuting} className="bg-blue-600 hover:bg-blue-700">
                    {isExecuting ? (
                      <>
                        <div className="w-4 h-4 mr-2 border-2 border-white border-t-transparent rounded-full animate-spin" />
                        Running...
                      </>
                    ) : (
                      <>
                        <Play className="w-4 h-4 mr-2" />
                        Execute
                      </>
                    )}
                  </Button>
                </div>
              </div>
            </CardHeader>
            <CardContent>
              <div className="border border-gray-200 rounded-lg overflow-hidden">
                <MonacoEditor
                  height="400px"
                  defaultLanguage="cobol"
                  value={code}
                  onChange={(value) => setCode(value || "")}
                  theme="vs-light"
                  options={{
                    minimap: { enabled: false },
                    fontSize: 14,
                    wordWrap: "on",
                    lineNumbers: "on",
                    scrollBeyondLastLine: false,
                    automaticLayout: true,
                  }}
                />
              </div>
            </CardContent>
          </Card>

          {/* Output Window */}
          <Card>
            <CardHeader>
              <div className="flex items-center justify-between">
                <CardTitle className="text-gray-800">Output</CardTitle>
                <div className="flex gap-2">
                  {result && (
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={copyOutput}
                      className="text-gray-600 border-gray-300 bg-transparent"
                    >
                      {copied ? (
                        <CheckCircle className="w-4 h-4 mr-2 text-green-600" />
                      ) : (
                        <Copy className="w-4 h-4 mr-2" />
                      )}
                      {copied ? "Copied!" : "Copy"}
                    </Button>
                  )}
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={clearOutput}
                    className="text-gray-600 border-gray-300 bg-transparent"
                  >
                    <RotateCcw className="w-4 h-4 mr-2" />
                    Clear
                  </Button>
                </div>
              </div>
            </CardHeader>
            <CardContent>
              <div className="h-96 bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-sm overflow-auto">
                {!result ? (
                  <div className="text-gray-500">Click "Execute" to run your COBOL program...</div>
                ) : (
                  <div className="space-y-2">
                    {/* Execution Status */}
                    <div className="flex items-center gap-2 pb-2 border-b border-gray-700">
                      {result.success ? (
                        <CheckCircle className="w-4 h-4 text-green-400" />
                      ) : (
                        <AlertCircle className="w-4 h-4 text-red-400" />
                      )}
                      <span className={result.success ? "text-green-400" : "text-red-400"}>
                        {result.success ? "Execution completed" : "Execution failed"}
                      </span>
                      <span className="text-gray-500 text-xs ml-auto">
                        {result.executionTime}ms • {result.memoryUsage}MB
                      </span>
                    </div>

                    {/* Program Output */}
                    {result.output.length > 0 && (
                      <div>
                        <div className="text-blue-400 text-xs mb-1">PROGRAM OUTPUT:</div>
                        {result.output.map((line, index) => (
                          <div key={index} className="text-gray-100">
                            {line}
                          </div>
                        ))}
                      </div>
                    )}

                    {/* Errors */}
                    {result.errors.length > 0 && (
                      <div>
                        <div className="text-red-400 text-xs mb-1">ERRORS:</div>
                        {result.errors.map((error, index) => (
                          <div key={index} className="text-red-400">
                            {error}
                          </div>
                        ))}
                      </div>
                    )}

                    {/* Warnings */}
                    {result.warnings.length > 0 && (
                      <div>
                        <div className="text-yellow-400 text-xs mb-1">WARNINGS:</div>
                        {result.warnings.map((warning, index) => (
                          <div key={index} className="text-yellow-400">
                            {warning}
                          </div>
                        ))}
                      </div>
                    )}

                    {/* Variables */}
                    {Object.keys(result.variables).length > 0 && (
                      <div>
                        <div className="text-cyan-400 text-xs mb-1">VARIABLES:</div>
                        {Object.entries(result.variables).map(([name, value]) => (
                          <div key={name} className="text-gray-300">
                            {name}: {JSON.stringify(value)}
                          </div>
                        ))}
                      </div>
                    )}
                  </div>
                )}
              </div>
            </CardContent>
          </Card>
        </div>

        {/* Status and Info */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Alert>
            <CheckCircle className="h-4 w-4" />
            <AlertDescription>
              <strong>Serverless Ready</strong>
              <br />
              Runs on Vercel, AWS Lambda, and other platforms
            </AlertDescription>
          </Alert>

          <Alert>
            <CheckCircle className="h-4 w-4" />
            <AlertDescription>
              <strong>Pure JavaScript</strong>
              <br />
              No external dependencies or compilers needed
            </AlertDescription>
          </Alert>

          <Alert>
            <CheckCircle className="h-4 w-4" />
            <AlertDescription>
              <strong>Open Source</strong>
              <br />
              MIT licensed, contributions welcome
            </AlertDescription>
          </Alert>
        </div>
      </div>
    </div>
  )
}
