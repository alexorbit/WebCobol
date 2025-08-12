
```markdown file="interpreter/README.md"
# WebCOBOL Interpreter

A modern, serverless-compatible COBOL interpreter written in TypeScript. Execute COBOL programs directly in web browsers, Node.js, and serverless environments without native compilation.

## 🚀 Features

- **Pure JavaScript/TypeScript**: No native dependencies required
- **Serverless Ready**: Works on Vercel, AWS Lambda, Cloudflare Workers
- **Real-time Execution**: Instant COBOL program execution
- **Modern Integration**: HTTP, JSON, Database support
- **Web Compatible**: Runs in browsers and Node.js
- **Open Source**: MIT licensed

## 📦 Installation

\`\`\`bash
npm install webcobol-interpreter
\`\`\`

## 🔧 Quick Start

\`\`\`typescript
import { CobolInterpreter } from 'webcobol-interpreter';

const cobolCode = `
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".

PROCEDURE DIVISION.
DISPLAY WS-MESSAGE.
STOP RUN.
`;

const interpreter = new CobolInterpreter();
const result = interpreter.execute(cobolCode);
console.log(result.output); // "Hello, World!"
\`\`\`

## 🏗️ Architecture

The interpreter consists of four main components:

1. **Tokenizer**: Converts COBOL source into tokens
2. **Parser**: Builds Abstract Syntax Tree (AST)
3. **Runtime**: Executes the parsed program
4. **Memory Manager**: Handles COBOL data types and storage

## 📚 Language Support

### Supported COBOL Features

#### Data Division
- ✅ Working-Storage Section
- ✅ PIC clauses (X, 9, A, S, V)
- ✅ VALUE clauses
- ✅ OCCURS clauses
- ✅ REDEFINES clauses
- ⏳ File Section (partial)
- ⏳ Linkage Section

#### Procedure Division
- ✅ DISPLAY statement
- ✅ MOVE statement
- ✅ ADD, SUBTRACT, MULTIPLY, DIVIDE
- ✅ IF-THEN-ELSE
- ✅ PERFORM loops
- ✅ ACCEPT statement
- ⏳ STRING/UNSTRING operations
- ⏳ CALL statement

### Web Extensions

```cobol
* HTTP Operations
HTTP-GET "https://api.example.com" GIVING WS-RESPONSE.

* JSON Handling
JSON-PARSE WS-JSON-STRING INTO WS-DATA-STRUCTURE.

* Database Operations
SQL-EXEC "SELECT * FROM users" GIVING WS-RESULT.