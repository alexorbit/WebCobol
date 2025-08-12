"use client"

import { useState, useEffect } from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Badge } from "@/components/ui/badge"
import { Alert, AlertDescription } from "@/components/ui/alert"
import { CheckCircle, XCircle, AlertCircle, RefreshCw } from "lucide-react"

export default function ConfigPage() {
  const [apiStatus, setApiStatus] = useState<any>(null)
  const [loading, setLoading] = useState(false)
  const [testResult, setTestResult] = useState<any>(null)

  const checkApiStatus = async () => {
    setLoading(true)
    try {
      const response = await fetch("/api/ai-assist")
      const data = await response.json()
      setApiStatus(data)
    } catch (error) {
      setApiStatus({ error: "Erro ao verificar status da API" })
    }
    setLoading(false)
  }

  const testApi = async () => {
    setLoading(true)
    try {
      const response = await fetch("/api/ai-assist", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          prompt: "Teste de conectividade",
          action: "assist",
        }),
      })
      const data = await response.json()
      setTestResult(data)
    } catch (error) {
      setTestResult({ error: "Erro no teste da API" })
    }
    setLoading(false)
  }

  useEffect(() => {
    checkApiStatus()
  }, [])

  return (
    <div className="container mx-auto p-6 max-w-4xl">
      <div className="mb-8">
        <h1 className="text-3xl font-bold mb-2">Configuração do Sistema</h1>
        <p className="text-muted-foreground">Verifique e teste as configurações da WebCOBOL-Blockchain</p>
      </div>

      <div className="grid gap-6">
        {/* Status da API DeepSeek */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <AlertCircle className="h-5 w-5" />
              Status da API DeepSeek
            </CardTitle>
            <CardDescription>Verificação da configuração da chave da API</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {apiStatus && (
              <div className="space-y-3">
                <div className="flex items-center gap-2">
                  {apiStatus.configured ? (
                    <CheckCircle className="h-5 w-5 text-green-500" />
                  ) : (
                    <XCircle className="h-5 w-5 text-red-500" />
                  )}
                  <span className="font-medium">
                    API Key: {apiStatus.configured ? "Configurada" : "Não configurada"}
                  </span>
                  <Badge variant={apiStatus.configured ? "default" : "destructive"}>{apiStatus.keyPreview}</Badge>
                </div>

                {!apiStatus.configured && (
                  <Alert>
                    <AlertCircle className="h-4 w-4" />
                    <AlertDescription>
                      Configure a variável de ambiente DEEPSEEK_API_KEY nas configurações do projeto Vercel.
                      <br />
                      Obtenha uma chave em:{" "}
                      <a href="https://platform.deepseek.com" target="_blank" className="underline" rel="noreferrer">
                        platform.deepseek.com
                      </a>
                    </AlertDescription>
                  </Alert>
                )}

                <div className="text-sm text-muted-foreground">
                  Última verificação: {new Date(apiStatus.timestamp).toLocaleString()}
                </div>
              </div>
            )}

            <div className="flex gap-2">
              <Button onClick={checkApiStatus} disabled={loading} variant="outline">
                <RefreshCw className={`h-4 w-4 mr-2 ${loading ? "animate-spin" : ""}`} />
                Verificar Status
              </Button>
              <Button onClick={testApi} disabled={loading || !apiStatus?.configured}>
                Testar API
              </Button>
            </div>
          </CardContent>
        </Card>

        {/* Resultado do Teste */}
        {testResult && (
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                {testResult.success ? (
                  <CheckCircle className="h-5 w-5 text-green-500" />
                ) : (
                  <XCircle className="h-5 w-5 text-red-500" />
                )}
                Resultado do Teste
              </CardTitle>
            </CardHeader>
            <CardContent>
              {testResult.success ? (
                <div className="space-y-2">
                  <Badge variant="default">Sucesso</Badge>
                  <p className="text-sm">A API está funcionando corretamente!</p>
                  {testResult.usage && (
                    <div className="text-xs text-muted-foreground">Tokens usados: {testResult.usage.total_tokens}</div>
                  )}
                </div>
              ) : (
                <div className="space-y-2">
                  <Badge variant="destructive">Erro</Badge>
                  <p className="text-sm text-red-600">{testResult.error}</p>
                  {testResult.hint && <p className="text-xs text-muted-foreground">{testResult.hint}</p>}
                </div>
              )}
            </CardContent>
          </Card>
        )}

        {/* Instruções de Configuração */}
        <Card>
          <CardHeader>
            <CardTitle>Como Configurar</CardTitle>
            <CardDescription>Passos para configurar a integração com DeepSeek</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <h4 className="font-medium">1. Obter Chave da API</h4>
              <p className="text-sm text-muted-foreground">
                Acesse{" "}
                <a href="https://platform.deepseek.com" target="_blank" className="underline" rel="noreferrer">
                  platform.deepseek.com
                </a>{" "}
                e crie uma conta
              </p>
            </div>
            <div className="space-y-2">
              <h4 className="font-medium">2. Configurar na Vercel</h4>
              <p className="text-sm text-muted-foreground">Vá em Project Settings → Environment Variables → Add New</p>
              <code className="block bg-muted p-2 rounded text-sm">
                Name: DEEPSEEK_API_KEY
                <br />
                Value: sk-xxxxxxxxxxxxxxxx
              </code>
            </div>
            <div className="space-y-2">
              <h4 className="font-medium">3. Redeploy</h4>
              <p className="text-sm text-muted-foreground">Faça um novo deploy para aplicar as mudanças</p>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  )
}
