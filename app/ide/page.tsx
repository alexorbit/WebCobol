"use client"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { Badge } from "@/components/ui/badge"
import {
  FileText,
  Play,
  Download,
  Bot,
  Folder,
  FolderOpen,
  File,
  Plus,
  Trash2,
  Save,
  Upload,
  Terminal,
  AlertCircle,
  CheckCircle,
  Info,
  Zap,
} from "lucide-react"

interface FileNode {
  name: string
  type: "file" | "folder"
  content?: string
  children?: FileNode[]
  path: string
}

interface LogEntry {
  type: "info" | "error" | "success" | "warning"
  message: string
  timestamp: Date
}

export default function WebCOBOLIDE() {
  const [selectedFile, setSelectedFile] = useState<string>("")
  const [fileContent, setFileContent] = useState<string>("")
  const [files, setFiles] = useState<FileNode[]>([
    {
      name: "contracts",
      type: "folder",
      path: "contracts",
      children: [
        {
          name: "HelloWorld.wcb",
          type: "file",
          path: "contracts/HelloWorld.wcb",
          content: `IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD-CONTRACT.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MESSAGE-TEXT    PIC X(50) VALUE "Hello, WebCOBOL Blockchain!".
01 BLOCK-DATA      PIC X(100).
01 BLOCK-HASH      PIC X(64).

PROCEDURE DIVISION.
MAIN-LOGIC.
    DISPLAY "Deploying Hello World Contract...".
    MOVE MESSAGE-TEXT TO BLOCK-DATA.
    PERFORM HASH-BLOCK.
    DISPLAY "Contract deployed with hash: " BLOCK-HASH.
    STOP RUN.

HASH-BLOCK.
    COMPUTE BLOCK-HASH = FUNCTION SHA-256(BLOCK-DATA).`,
        },
      ],
    },
    {
      name: "examples",
      type: "folder",
      path: "examples",
      children: [
        {
          name: "voting.wcb",
          type: "file",
          path: "examples/voting.wcb",
          content: `IDENTIFICATION DIVISION.
PROGRAM-ID. VOTING-CONTRACT.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 VOTE-RECORD.
   05 VOTER-ID      PIC X(20).
   05 CANDIDATE     PIC X(50).
   05 VOTE-TIME     PIC 9(15).

PROCEDURE DIVISION.
CAST-VOTE.
    ACCEPT VOTER-ID FROM CONSOLE.
    ACCEPT CANDIDATE FROM CONSOLE.
    MOVE FUNCTION CURRENT-DATE TO VOTE-TIME.
    PERFORM ADD-TO-BLOCKCHAIN.
    DISPLAY "Vote registered successfully!".
    STOP RUN.

ADD-TO-BLOCKCHAIN.
    DISPLAY "Adding vote to blockchain...".`,
        },
      ],
    },
  ])
  const [logs, setLogs] = useState<LogEntry[]>([])
  const [activePanel, setActivePanel] = useState<"explorer" | "compiler" | "deploy" | "ai">("explorer")
  const [isCompiling, setIsCompiling] = useState(false)
  const [aiPrompt, setAiPrompt] = useState("")
  const [aiResponse, setAiResponse] = useState("")
  const [isAiLoading, setIsAiLoading] = useState(false)

  const addLog = (type: LogEntry["type"], message: string) => {
    setLogs((prev) => [...prev, { type, message, timestamp: new Date() }])
  }

  const handleFileSelect = (file: FileNode) => {
    if (file.type === "file") {
      setSelectedFile(file.path)
      setFileContent(file.content || "")
    }
  }

  const handleCompile = async () => {
    if (!selectedFile || !fileContent) {
      addLog("error", "No file selected for compilation")
      return
    }

    setIsCompiling(true)
    addLog("info", `Compiling ${selectedFile}...`)

    try {
      const response = await fetch("/api/compile", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code: fileContent, filename: selectedFile }),
      })

      const result = await response.json()

      if (result.success) {
        addLog("success", "Compilation successful!")
        addLog("info", result.output || "No output")
      } else {
        addLog("error", `Compilation failed: ${result.error}`)
      }
    } catch (error) {
      addLog("error", `Compilation error: ${error}`)
    } finally {
      setIsCompiling(false)
    }
  }

  const handleAiAssist = async () => {
    if (!aiPrompt.trim()) return

    setIsAiLoading(true)
    addLog("info", `AI Assistant: ${aiPrompt}`)

    try {
      const response = await fetch("/api/ai-assist", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          prompt: aiPrompt,
          code: fileContent,
          context: "WebCOBOL IDE",
        }),
      })

      const result = await response.json()

      if (result.success) {
        setAiResponse(result.response)
        addLog("success", "AI Assistant responded")
      } else {
        addLog("error", `AI Error: ${result.error}`)
      }
    } catch (error) {
      addLog("error", `AI Error: ${error}`)
    } finally {
      setIsAiLoading(false)
    }
  }

  const renderFileTree = (nodes: FileNode[], level = 0) => {
    return nodes.map((node) => (
      <div key={node.path} style={{ marginLeft: `${level * 16}px` }}>
        <div
          className={`flex items-center gap-2 p-2 hover:bg-slate-100 cursor-pointer rounded-md ${
            selectedFile === node.path ? "bg-blue-100 text-blue-700" : ""
          }`}
          onClick={() => handleFileSelect(node)}
        >
          {node.type === "folder" ? (
            <FolderOpen className="w-4 h-4 text-amber-600" />
          ) : (
            <File className="w-4 h-4 text-slate-600" />
          )}
          <span className="text-sm font-medium">{node.name}</span>
        </div>
        {node.children && renderFileTree(node.children, level + 1)}
      </div>
    ))
  }

  const getLogIcon = (type: LogEntry["type"]) => {
    switch (type) {
      case "error":
        return <AlertCircle className="w-4 h-4 text-red-500" />
      case "success":
        return <CheckCircle className="w-4 h-4 text-green-500" />
      case "warning":
        return <AlertCircle className="w-4 h-4 text-amber-500" />
      default:
        return <Info className="w-4 h-4 text-blue-500" />
    }
  }

  return (
    <div className="h-screen flex flex-col bg-slate-50">
      {/* Header */}
      <div className="h-12 bg-white border-b border-slate-200 flex items-center justify-between px-4">
        <div className="flex items-center gap-3">
          <div className="flex items-center gap-2">
            <div className="w-8 h-8 bg-gradient-to-br from-blue-600 to-purple-600 rounded-lg flex items-center justify-center">
              <span className="text-white font-bold text-sm">WCB</span>
            </div>
            <h1 className="text-lg font-bold text-slate-800">WebCOBOL IDE</h1>
          </div>
          <Badge variant="secondary" className="bg-green-100 text-green-700">
            v1.0.0
          </Badge>
        </div>
        <div className="flex items-center gap-2">
          <Button variant="outline" size="sm">
            <Save className="w-4 h-4 mr-2" />
            Save
          </Button>
          <Button variant="outline" size="sm">
            <Upload className="w-4 h-4 mr-2" />
            Import
          </Button>
        </div>
      </div>

      <div className="flex-1 flex">
        {/* Icon Panel */}
        <div className="w-12 bg-slate-800 flex flex-col items-center py-4 gap-2">
          <Button
            variant={activePanel === "explorer" ? "secondary" : "ghost"}
            size="sm"
            className={`w-8 h-8 p-0 ${
              activePanel === "explorer"
                ? "bg-blue-600 text-white hover:bg-blue-700"
                : "text-slate-400 hover:text-white hover:bg-slate-700"
            }`}
            onClick={() => setActivePanel("explorer")}
          >
            <Folder className="w-4 h-4" />
          </Button>
          <Button
            variant={activePanel === "compiler" ? "secondary" : "ghost"}
            size="sm"
            className={`w-8 h-8 p-0 ${
              activePanel === "compiler"
                ? "bg-green-600 text-white hover:bg-green-700"
                : "text-slate-400 hover:text-white hover:bg-slate-700"
            }`}
            onClick={() => setActivePanel("compiler")}
          >
            <Zap className="w-4 h-4" />
          </Button>
          <Button
            variant={activePanel === "deploy" ? "secondary" : "ghost"}
            size="sm"
            className={`w-8 h-8 p-0 ${
              activePanel === "deploy"
                ? "bg-purple-600 text-white hover:bg-purple-700"
                : "text-slate-400 hover:text-white hover:bg-slate-700"
            }`}
            onClick={() => setActivePanel("deploy")}
          >
            <Play className="w-4 h-4" />
          </Button>
          <Button
            variant={activePanel === "ai" ? "secondary" : "ghost"}
            size="sm"
            className={`w-8 h-8 p-0 ${
              activePanel === "ai"
                ? "bg-amber-600 text-white hover:bg-amber-700"
                : "text-slate-400 hover:text-white hover:bg-slate-700"
            }`}
            onClick={() => setActivePanel("ai")}
          >
            <Bot className="w-4 h-4" />
          </Button>
        </div>

        {/* Side Panel */}
        <div className="w-80 bg-white border-r border-slate-200 flex flex-col">
          <div className="p-4 border-b border-slate-200">
            <h2 className="font-semibold text-slate-800">
              {activePanel === "explorer" && "File Explorer"}
              {activePanel === "compiler" && "COBOL Compiler"}
              {activePanel === "deploy" && "Deploy & Run"}
              {activePanel === "ai" && "AI Assistant"}
            </h2>
          </div>

          <div className="flex-1 overflow-auto">
            {activePanel === "explorer" && (
              <div className="p-4">
                <div className="flex items-center justify-between mb-4">
                  <span className="text-sm font-medium text-slate-600">Workspace</span>
                  <div className="flex gap-1">
                    <Button variant="ghost" size="sm" className="w-6 h-6 p-0">
                      <Plus className="w-3 h-3" />
                    </Button>
                    <Button variant="ghost" size="sm" className="w-6 h-6 p-0">
                      <Trash2 className="w-3 h-3" />
                    </Button>
                  </div>
                </div>
                {renderFileTree(files)}
              </div>
            )}

            {activePanel === "compiler" && (
              <div className="p-4 space-y-4">
                <div>
                  <label className="text-sm font-medium text-slate-700 mb-2 block">Current File</label>
                  <div className="text-sm text-slate-600 bg-slate-50 p-2 rounded">
                    {selectedFile || "No file selected"}
                  </div>
                </div>
                <Button
                  onClick={handleCompile}
                  disabled={!selectedFile || isCompiling}
                  className="w-full bg-green-600 hover:bg-green-700"
                >
                  {isCompiling ? "Compiling..." : "Compile"}
                </Button>
                <div className="text-xs text-slate-500">Compile your WebCOBOL contracts to bytecode</div>
              </div>
            )}

            {activePanel === "deploy" && (
              <div className="p-4 space-y-4">
                <div>
                  <label className="text-sm font-medium text-slate-700 mb-2 block">Environment</label>
                  <select className="w-full p-2 border border-slate-300 rounded-md text-sm">
                    <option>Local Testnet</option>
                    <option>WebCOBOL Testnet</option>
                    <option>Mainnet</option>
                  </select>
                </div>
                <Button className="w-full bg-purple-600 hover:bg-purple-700">
                  <Play className="w-4 h-4 mr-2" />
                  Deploy Contract
                </Button>
                <Button variant="outline" className="w-full bg-transparent">
                  <Download className="w-4 h-4 mr-2" />
                  Download Artifacts
                </Button>
              </div>
            )}

            {activePanel === "ai" && (
              <div className="p-4 space-y-4">
                <div>
                  <label className="text-sm font-medium text-slate-700 mb-2 block">Ask AI Assistant</label>
                  <textarea
                    value={aiPrompt}
                    onChange={(e) => setAiPrompt(e.target.value)}
                    placeholder="How can I help you with WebCOBOL?"
                    className="w-full p-3 border border-slate-300 rounded-md text-sm resize-none"
                    rows={3}
                  />
                </div>
                <Button
                  onClick={handleAiAssist}
                  disabled={isAiLoading || !aiPrompt.trim()}
                  className="w-full bg-amber-600 hover:bg-amber-700"
                >
                  {isAiLoading ? "Thinking..." : "Ask AI"}
                </Button>
                {aiResponse && (
                  <div className="bg-slate-50 p-3 rounded-md">
                    <div className="text-xs font-medium text-slate-600 mb-2">AI Response:</div>
                    <div className="text-sm text-slate-800 whitespace-pre-wrap">{aiResponse}</div>
                  </div>
                )}
              </div>
            )}
          </div>
        </div>

        {/* Main Panel */}
        <div className="flex-1 flex flex-col">
          {/* Editor Tabs */}
          <div className="h-10 bg-slate-100 border-b border-slate-200 flex items-center px-4">
            {selectedFile && (
              <div className="flex items-center gap-2 bg-white px-3 py-1 rounded-t-md border-t border-l border-r border-slate-200">
                <File className="w-3 h-3 text-slate-500" />
                <span className="text-sm font-medium">{selectedFile.split("/").pop()}</span>
              </div>
            )}
          </div>

          {/* Code Editor */}
          <div className="flex-1">
            {selectedFile ? (
              <div className="h-full">
                <textarea
                  value={fileContent}
                  onChange={(e) => setFileContent(e.target.value)}
                  className="w-full h-full p-4 font-mono text-sm border-none outline-none resize-none"
                  placeholder="Start coding your WebCOBOL contract..."
                />
              </div>
            ) : (
              <div className="h-full flex items-center justify-center text-slate-500">
                <div className="text-center">
                  <FileText className="w-12 h-12 mx-auto mb-4 text-slate-300" />
                  <p className="text-lg font-medium mb-2">Welcome to WebCOBOL IDE</p>
                  <p className="text-sm">Select a file from the explorer to start coding</p>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Terminal */}
      <div className="h-48 bg-slate-900 text-white flex flex-col">
        <div className="h-8 bg-slate-800 flex items-center px-4 border-t border-slate-700">
          <Terminal className="w-4 h-4 mr-2 text-slate-400" />
          <span className="text-sm font-medium text-slate-300">Console</span>
        </div>
        <div className="flex-1 overflow-auto p-4 space-y-1">
          {logs.map((log, index) => (
            <div key={index} className="flex items-start gap-2 text-sm">
              <span className="text-slate-500 text-xs mt-0.5">{log.timestamp.toLocaleTimeString()}</span>
              {getLogIcon(log.type)}
              <span
                className={`${
                  log.type === "error"
                    ? "text-red-400"
                    : log.type === "success"
                      ? "text-green-400"
                      : log.type === "warning"
                        ? "text-amber-400"
                        : "text-slate-300"
                }`}
              >
                {log.message}
              </span>
            </div>
          ))}
          {logs.length === 0 && (
            <div className="text-slate-500 text-sm">Console ready. Compile or deploy contracts to see output.</div>
          )}
        </div>
      </div>
    </div>
  )
}
