"use client"

import { useState } from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { Textarea } from "@/components/ui/textarea"
import { Badge } from "@/components/ui/badge"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Alert, AlertDescription } from "@/components/ui/alert"
import { Download, Play, Settings, Zap } from "lucide-react"

interface BlockchainConfig {
  name: string
  symbol: string
  initialSupply: string
  consensusType: "pow" | "pos"
  blockTime: string
  difficulty: string
  blockReward: string
  maxSupply: string
  decimals: string
  description: string
  features: string[]
}

export default function GeneratorPage() {
  const [config, setConfig] = useState<BlockchainConfig>({
    name: "",
    symbol: "",
    initialSupply: "1000000",
    consensusType: "pow",
    blockTime: "10",
    difficulty: "4",
    blockReward: "50",
    maxSupply: "21000000",
    decimals: "8",
    description: "",
    features: [],
  })

  const [isGenerating, setIsGenerating] = useState(false)
  const [generatedCode, setGeneratedCode] = useState("")
  const [isRunning, setIsRunning] = useState(false)
  const [blockchainStatus, setBlockchainStatus] = useState<any>(null)

  const handleInputChange = (field: keyof BlockchainConfig, value: string) => {
    setConfig((prev) => ({ ...prev, [field]: value }))
  }

  const toggleFeature = (feature: string) => {
    setConfig((prev) => ({
      ...prev,
      features: prev.features.includes(feature)
        ? prev.features.filter((f) => f !== feature)
        : [...prev.features, feature],
    }))
  }

  const generateBlockchain = async () => {
    setIsGenerating(true)
    try {
      const response = await fetch("/api/generator", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(config),
      })

      const result = await response.json()
      setGeneratedCode(result.code)
    } catch (error) {
      console.error("Erro ao gerar blockchain:", error)
    } finally {
      setIsGenerating(false)
    }
  }

  const runBlockchain = async () => {
    setIsRunning(true)
    try {
      const response = await fetch("/api/generator/run", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code: generatedCode, config }),
      })

      const result = await response.json()
      setBlockchainStatus(result)
    } catch (error) {
      console.error("Erro ao executar blockchain:", error)
    } finally {
      setIsRunning(false)
    }
  }

  const downloadBlockchain = async () => {
    try {
      const response = await fetch("/api/generator/download", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ code: generatedCode, config }),
      })

      const blob = await response.blob()
      const url = window.URL.createObjectURL(blob)
      const a = document.createElement("a")
      a.href = url
      a.download = `${config.name.toLowerCase().replace(/\s+/g, "-")}-blockchain.zip`
      document.body.appendChild(a)
      a.click()
      window.URL.revokeObjectURL(url)
      document.body.removeChild(a)
    } catch (error) {
      console.error("Erro ao fazer download:", error)
    }
  }

  const availableFeatures = [
    "Smart Contracts",
    "Multi-Signature",
    "Atomic Swaps",
    "Lightning Network",
    "Privacy Coins",
    "Staking Rewards",
    "Governance",
    "NFT Support",
  ]

  return (
    <div className="container mx-auto px-4 py-8 max-w-6xl">
      <div className="text-center mb-8">
        <h1 className="text-4xl font-bold mb-4 bg-gradient-to-r from-blue-600 to-purple-600 bg-clip-text text-transparent">
          WebCOBOL Blockchain Generator
        </h1>
        <p className="text-lg text-slate-600 dark:text-slate-400">
          Crie seu próprio blockchain em COBOL com apenas alguns cliques
        </p>
      </div>

      <Tabs defaultValue="config" className="space-y-6">
        <TabsList className="grid w-full grid-cols-3 bg-slate-100 dark:bg-slate-800">
          <TabsTrigger value="config" className="data-[state=active]:bg-blue-600 data-[state=active]:text-white">
            Configuração
          </TabsTrigger>
          <TabsTrigger value="preview" className="data-[state=active]:bg-blue-600 data-[state=active]:text-white">
            Preview
          </TabsTrigger>
          <TabsTrigger value="deploy" className="data-[state=active]:bg-blue-600 data-[state=active]:text-white">
            Deploy
          </TabsTrigger>
        </TabsList>

        <TabsContent value="config" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Configurações Básicas */}
            <Card className="border-l-4 border-l-blue-500 shadow-lg">
              <CardHeader className="bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-950 dark:to-indigo-950">
                <CardTitle className="text-blue-900 dark:text-blue-100">Configurações Básicas</CardTitle>
                <CardDescription className="text-blue-700 dark:text-blue-300">
                  Defina as propriedades fundamentais do seu blockchain
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4 pt-6">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="name">Nome do Blockchain</Label>
                    <Input
                      id="name"
                      placeholder="MeuBlockchain"
                      value={config.name}
                      onChange={(e) => handleInputChange("name", e.target.value)}
                    />
                  </div>
                  <div>
                    <Label htmlFor="symbol">Símbolo da Moeda</Label>
                    <Input
                      id="symbol"
                      placeholder="MBC"
                      value={config.symbol}
                      onChange={(e) => handleInputChange("symbol", e.target.value)}
                    />
                  </div>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="initialSupply">Supply Inicial</Label>
                    <Input
                      id="initialSupply"
                      type="number"
                      value={config.initialSupply}
                      onChange={(e) => handleInputChange("initialSupply", e.target.value)}
                    />
                  </div>
                  <div>
                    <Label htmlFor="maxSupply">Supply Máximo</Label>
                    <Input
                      id="maxSupply"
                      type="number"
                      value={config.maxSupply}
                      onChange={(e) => handleInputChange("maxSupply", e.target.value)}
                    />
                  </div>
                </div>

                <div>
                  <Label htmlFor="description">Descrição</Label>
                  <Textarea
                    id="description"
                    placeholder="Descreva o propósito do seu blockchain..."
                    value={config.description}
                    onChange={(e) => handleInputChange("description", e.target.value)}
                  />
                </div>
              </CardContent>
            </Card>

            {/* Configurações de Consenso */}
            <Card className="border-l-4 border-l-green-500 shadow-lg">
              <CardHeader className="bg-gradient-to-r from-green-50 to-emerald-50 dark:from-green-950 dark:to-emerald-950">
                <CardTitle className="text-green-900 dark:text-green-100">Consenso e Mineração</CardTitle>
                <CardDescription className="text-green-700 dark:text-green-300">
                  Configure o algoritmo de consenso e parâmetros de mineração
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4 pt-6">
                <div>
                  <Label htmlFor="consensus">Tipo de Consenso</Label>
                  <Select
                    value={config.consensusType}
                    onValueChange={(value: "pow" | "pos") => handleInputChange("consensusType", value)}
                  >
                    <SelectTrigger>
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="pow">Proof of Work (PoW)</SelectItem>
                      <SelectItem value="pos">Proof of Stake (PoS)</SelectItem>
                    </SelectContent>
                  </Select>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="blockTime">Tempo do Bloco (segundos)</Label>
                    <Input
                      id="blockTime"
                      type="number"
                      value={config.blockTime}
                      onChange={(e) => handleInputChange("blockTime", e.target.value)}
                    />
                  </div>
                  <div>
                    <Label htmlFor="difficulty">Dificuldade Inicial</Label>
                    <Input
                      id="difficulty"
                      type="number"
                      value={config.difficulty}
                      onChange={(e) => handleInputChange("difficulty", e.target.value)}
                    />
                  </div>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="blockReward">Recompensa por Bloco</Label>
                    <Input
                      id="blockReward"
                      type="number"
                      value={config.blockReward}
                      onChange={(e) => handleInputChange("blockReward", e.target.value)}
                    />
                  </div>
                  <div>
                    <Label htmlFor="decimals">Casas Decimais</Label>
                    <Input
                      id="decimals"
                      type="number"
                      value={config.decimals}
                      onChange={(e) => handleInputChange("decimals", e.target.value)}
                    />
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Recursos Avançados */}
          <Card className="border-l-4 border-l-purple-500 shadow-lg">
            <CardHeader className="bg-gradient-to-r from-purple-50 to-violet-50 dark:from-purple-950 dark:to-violet-950">
              <CardTitle className="text-purple-900 dark:text-purple-100">Recursos Avançados</CardTitle>
              <CardDescription className="text-purple-700 dark:text-purple-300">
                Selecione os recursos que deseja incluir no seu blockchain
              </CardDescription>
            </CardHeader>
            <CardContent className="pt-6">
              <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
                {availableFeatures.map((feature) => (
                  <Badge
                    key={feature}
                    variant={config.features.includes(feature) ? "default" : "outline"}
                    className={`cursor-pointer p-3 text-center justify-center transition-all hover:scale-105 ${
                      config.features.includes(feature)
                        ? "bg-blue-600 hover:bg-blue-700 text-white shadow-md"
                        : "border-slate-300 hover:border-blue-400 hover:bg-blue-50 dark:hover:bg-blue-950"
                    }`}
                    onClick={() => toggleFeature(feature)}
                  >
                    {feature}
                  </Badge>
                ))}
              </div>
            </CardContent>
          </Card>

          <div className="flex justify-center">
            <Button
              onClick={generateBlockchain}
              disabled={isGenerating || !config.name || !config.symbol}
              size="lg"
              className="px-8 bg-gradient-to-r from-blue-600 to-purple-600 hover:from-blue-700 hover:to-purple-700 text-white shadow-lg"
            >
              {isGenerating ? (
                <>
                  <Settings className="mr-2 h-4 w-4 animate-spin" />
                  Gerando Blockchain...
                </>
              ) : (
                <>
                  <Zap className="mr-2 h-4 w-4" />
                  Gerar Blockchain
                </>
              )}
            </Button>
          </div>
        </TabsContent>

        <TabsContent value="preview" className="space-y-6">
          {generatedCode ? (
            <Card className="border-l-4 border-l-amber-500 shadow-lg">
              <CardHeader className="bg-gradient-to-r from-amber-50 to-yellow-50 dark:from-amber-950 dark:to-yellow-950">
                <CardTitle className="text-amber-900 dark:text-amber-100">Código Gerado</CardTitle>
                <CardDescription className="text-amber-700 dark:text-amber-300">
                  Seu blockchain em COBOL foi gerado com sucesso
                </CardDescription>
              </CardHeader>
              <CardContent className="pt-6">
                <pre className="bg-slate-900 text-green-400 p-4 rounded-lg overflow-x-auto text-sm border">
                  <code>{generatedCode}</code>
                </pre>
              </CardContent>
            </Card>
          ) : (
            <Alert className="border-l-4 border-l-blue-500 bg-blue-50 dark:bg-blue-950">
              <AlertDescription className="text-blue-800 dark:text-blue-200">
                Configure seu blockchain na aba "Configuração" e clique em "Gerar Blockchain" para ver o preview.
              </AlertDescription>
            </Alert>
          )}
        </TabsContent>

        <TabsContent value="deploy" className="space-y-6">
          {generatedCode ? (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <Card className="border-l-4 border-l-green-500 shadow-lg">
                <CardHeader className="bg-gradient-to-r from-green-50 to-emerald-50 dark:from-green-950 dark:to-emerald-950">
                  <CardTitle className="text-green-900 dark:text-green-100">Executar Agora</CardTitle>
                  <CardDescription className="text-green-700 dark:text-green-300">
                    Execute seu blockchain imediatamente na nuvem
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4 pt-6">
                  <Button
                    onClick={runBlockchain}
                    disabled={isRunning}
                    className="w-full bg-green-600 hover:bg-green-700 text-white shadow-md"
                    size="lg"
                  >
                    {isRunning ? (
                      <>
                        <Settings className="mr-2 h-4 w-4 animate-spin" />
                        Iniciando...
                      </>
                    ) : (
                      <>
                        <Play className="mr-2 h-4 w-4" />
                        Executar Blockchain
                      </>
                    )}
                  </Button>

                  {blockchainStatus && (
                    <div className="p-4 bg-green-50 dark:bg-green-950 rounded-lg border border-green-200 dark:border-green-800">
                      <h4 className="font-semibold mb-2">Status do Blockchain:</h4>
                      <p>Blocos: {blockchainStatus.blocks}</p>
                      <p>Hash Rate: {blockchainStatus.hashRate}</p>
                      <p>Status: {blockchainStatus.status}</p>
                    </div>
                  )}
                </CardContent>
              </Card>

              <Card className="border-l-4 border-l-indigo-500 shadow-lg">
                <CardHeader className="bg-gradient-to-r from-indigo-50 to-blue-50 dark:from-indigo-950 dark:to-blue-950">
                  <CardTitle className="text-indigo-900 dark:text-indigo-100">Download Local</CardTitle>
                  <CardDescription className="text-indigo-700 dark:text-indigo-300">
                    Baixe o código e execute em seu próprio servidor
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4 pt-6">
                  <Button
                    onClick={downloadBlockchain}
                    variant="outline"
                    className="w-full border-indigo-300 text-indigo-700 hover:bg-indigo-50 dark:border-indigo-600 dark:text-indigo-300 dark:hover:bg-indigo-950 bg-transparent"
                    size="lg"
                  >
                    <Download className="mr-2 h-4 w-4" />
                    Download ZIP
                  </Button>

                  <div className="text-sm text-slate-600 dark:text-slate-400">
                    <p>O arquivo ZIP inclui:</p>
                    <ul className="list-disc list-inside mt-2 space-y-1">
                      <li>Código COBOL do blockchain</li>
                      <li>Scripts de compilação</li>
                      <li>Documentação</li>
                      <li>Arquivos de configuração</li>
                    </ul>
                  </div>
                </CardContent>
              </Card>
            </div>
          ) : (
            <Alert className="border-l-4 border-l-amber-500 bg-amber-50 dark:bg-amber-950">
              <AlertDescription className="text-amber-800 dark:text-amber-200">
                Gere seu blockchain primeiro para poder executar ou fazer download.
              </AlertDescription>
            </Alert>
          )}
        </TabsContent>
      </Tabs>
    </div>
  )
}
