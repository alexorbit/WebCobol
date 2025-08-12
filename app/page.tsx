import { Suspense } from "react"
import { WebCOBOLInterface } from "@/components/webcobol-interface"
import Link from "next/link"
import { Button } from "@/components/ui/button"

export default function Home() {
  return (
    <main className="min-h-screen bg-gray-50">
      <div className="container mx-auto px-4 py-8">
        <div className="text-center mb-8">
          <h1 className="text-4xl font-bold text-gray-800 mb-4">WebCOBOL-Blockchain</h1>
          <p className="text-xl text-gray-600 max-w-3xl mx-auto mb-6">
            Uma linguagem baseada em COBOL para desenvolvimento web e blockchain. Combine a robustez do COBOL com
            tecnologias modernas de blockchain.
          </p>

          <div className="flex gap-4 justify-center mb-8">
            <Link href="/interpreter">
              <Button
                size="lg"
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                🔧 COBOL Interpreter
              </Button>
            </Link>
            <Link href="/ide">
              <Button
                size="lg"
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                💻 WebCOBOL IDE
              </Button>
            </Link>
            <Link href="/generator">
              <Button size="lg" className="bg-blue-600 hover:bg-blue-700 text-white">
                🚀 Gerador de Blockchain
              </Button>
            </Link>
            <Link href="/config">
              <Button variant="outline" size="lg" className="text-gray-600 border-gray-300 bg-transparent">
                ⚙️ Configurações
              </Button>
            </Link>
          </div>

          {/* <CHANGE> Adicionado botão Deploy with Vercel */}
          <div className="mb-8">
            <a
              href="https://vercel.com/new/clone?repository-url=https%3A%2F%2Fgithub.com%2FAKowaa%2FWCB-COBOL&env=DEEPSEEK_API_KEY&envDescription=DeepSeek%20API%20key%20for%20AI%20assistance&envLink=https%3A%2F%2Fplatform.deepseek.com"
              target="_blank"
              rel="noopener noreferrer"
            >
              <img src="https://vercel.com/button" alt="Deploy with Vercel" className="inline-block" />
            </a>
          </div>
        </div>

        <Suspense fallback={<div className="text-center text-gray-600">Carregando interface...</div>}>
          <WebCOBOLInterface />
        </Suspense>
      </div>
    </main>
  )
}
