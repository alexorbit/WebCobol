# WebCOBOL-Blockchain

[![Deploy with Vercel](https://vercel.com/button)](https://vercel.com/new/clone?repository-url=https%3A%2F%2Fgithub.com%2FAKowaa%2FWCB-COBOL&env=DEEPSEEK_API_KEY&envDescription=DeepSeek%20API%20key%20for%20AI%20assistance&envLink=https%3A%2F%2Fplatform.deepseek.com)

[![Deployed on Vercel](https://img.shields.io/badge/Deployed%20on-Vercel-black?style=for-the-badge&logo=vercel)](https://vercel.com/akowaas-projects/v0-cobol-blockchain-project)
[![Built with v0](https://img.shields.io/badge/Built%20with-v0.app-black?style=for-the-badge)](https://v0.app/chat/projects/6MII8GMhoG3)

## Overview

WebCOBOL-Blockchain é uma plataforma inovadora que combina a robustez do COBOL com tecnologias modernas de blockchain e desenvolvimento web. Inclui um interpretador COBOL serverless, IDE completo e ferramentas de desenvolvimento blockchain.

## 🚀 Features

- **WebCOBOL Interpreter**: Interpretador COBOL puro em JavaScript/TypeScript
- **IDE Completo**: Interface de desenvolvimento similar ao Remix IDE
- **Gerador de Blockchain**: Crie blockchains personalizados com COBOL
- **AI Assistant**: Assistência inteligente com DeepSeek API
- **Serverless Ready**: Funciona na Vercel, AWS Lambda e outras plataformas

## 🛠️ Quick Start

### Deploy Instantâneo
Clique no botão "Deploy with Vercel" acima para fazer deploy em segundos.

### Desenvolvimento Local
\`\`\`bash
git clone https://github.com/AKowaa/WCB-COBOL.git
cd WCB-COBOL
npm install
npm run dev
\`\`\`

### Variáveis de Ambiente
\`\`\`bash
DEEPSEEK_API_KEY=sua-chave-deepseek  # Opcional para AI Assistant
\`\`\`

## 📁 Estrutura do Projeto

\`\`\`
webcobol-blockchain/
├── app/                    # Next.js App Router
│   ├── ide/               # WebCOBOL IDE
│   ├── interpreter/       # Interpretador COBOL
│   ├── generator/         # Gerador de Blockchain
│   └── api/              # API Routes
├── interpreter/           # Interpretador Open Source
│   ├── src/              # Código fonte
│   ├── tests/            # Testes
│   └── docs/             # Documentação
├── components/           # Componentes React
└── lib/                 # Utilitários
\`\`\`

## 🔧 Interpretador COBOL

O WebCOBOL Interpreter é um interpretador COBOL completo escrito em TypeScript que funciona em ambientes serverless:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(30) VALUE "Hello, WebCOBOL!".

PROCEDURE DIVISION.
    DISPLAY WS-MESSAGE.
    STOP RUN.
