"use client"

import { useState } from "react"
import dynamic from "next/dynamic"

const MonacoEditor = dynamic(() => import("@monaco-editor/react"), { ssr: false })

const defaultCode = `IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(20) VALUE "Hello, COBOL World!".
01 WS-NUMBER PIC 9(3) VALUE 123.

PROCEDURE DIVISION.
DISPLAY WS-MESSAGE.
DISPLAY "Number: " WS-NUMBER.
STOP RUN.`

export default function InterpreterPage() {
  const [code, setCode] = useState(defaultCode)
  const [output, setOutput] = useState("")
  const [isRunning, setIsRunning] = useState(false)

  const runCode = async () => {
    setIsRunning(true)
    try {
      const response = await fetch("/api/interpret", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code }),
      })

      const result = await response.json()

      if (result.success) {
        setOutput(result.output || "Program executed successfully (no output)")
      } else {
        setOutput(`Error: ${result.error}`)
      }
    } catch (error) {
      setOutput(`Network error: ${error}`)
    }
    setIsRunning(false)
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <header className="bg-white border-b border-gray-200 px-6 py-4">
        <h1 className="text-2xl font-bold text-gray-800">WebCOBOL Interpreter</h1>
        <p className="text-gray-600">Online COBOL interpreter and IDE</p>
      </header>

      {/* Main Content */}
      <div className="flex h-[calc(100vh-80px)]">
        {/* Editor Panel */}
        <div className="flex-1 flex flex-col">
          <div className="bg-white border-b border-gray-200 px-4 py-2 flex justify-between items-center">
            <h2 className="font-semibold text-gray-700">COBOL Editor</h2>
            <button
              onClick={runCode}
              disabled={isRunning}
              className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50"
            >
              {isRunning ? "Running..." : "Run Code"}
            </button>
          </div>
          <div className="flex-1">
            <MonacoEditor
              height="100%"
              defaultLanguage="cobol"
              value={code}
              onChange={(value) => setCode(value || "")}
              theme="vs-light"
              options={{
                minimap: { enabled: false },
                fontSize: 14,
                lineNumbers: "on",
                wordWrap: "on",
              }}
            />
          </div>
        </div>

        {/* Output Panel */}
        <div className="w-96 bg-white border-l border-gray-200 flex flex-col">
          <div className="bg-gray-100 px-4 py-2 border-b border-gray-200">
            <h2 className="font-semibold text-gray-700">Output</h2>
          </div>
          <div className="flex-1 p-4 overflow-auto">
            <pre className="text-sm text-gray-800 whitespace-pre-wrap font-mono">
              {output || 'Click "Run Code" to see output here...'}
            </pre>
          </div>
        </div>
      </div>
    </div>
  )
}
