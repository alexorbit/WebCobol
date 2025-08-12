# WebCOBOL Interpreter

A pure JavaScript/TypeScript COBOL interpreter designed for modern web and serverless environments.

## Features

- Pure JavaScript implementation (no native dependencies)
- Serverless-compatible (Vercel, AWS Lambda, Cloudflare Workers)
- COBOL-74/85 language support
- Web browser compatibility
- TypeScript definitions included
- Comprehensive test suite

## Installation

\`\`\`bash
npm install webcobol-interpreter
\`\`\`

## Quick Start

\`\`\`javascript
const { CobolInterpreter } = require('webcobol-interpreter')

const interpreter = new CobolInterpreter()

const cobolCode = `
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(20) VALUE "Hello, COBOL!".

PROCEDURE DIVISION.
DISPLAY WS-MESSAGE.
STOP RUN.
`

const result = interpreter.interpret(cobolCode)
console.log(result.output) // "Hello, COBOL!"
\`\`\`

## Supported COBOL Features

### Data Types
- PIC X (alphanumeric)
- PIC 9 (numeric)
- PIC V (decimal point)
- VALUE clauses
- SPACES and ZEROS

### Statements
- DISPLAY
- MOVE
- ADD
- SUBTRACT
- PERFORM
- STOP RUN

### Structure
- IDENTIFICATION DIVISION
- DATA DIVISION
- WORKING-STORAGE SECTION
- PROCEDURE DIVISION
- Paragraph procedures

## API Reference

### CobolInterpreter

\`\`\`typescript
class CobolInterpreter {
  interpret(cobolCode: string): ExecutionResult
}

interface ExecutionResult {
  success: boolean
  output: string
  error?: string
}
\`\`\`

## Examples

See the `/examples` directory for more comprehensive examples.

## Testing

\`\`\`bash
npm test
\`\`\`

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

MIT License - see [LICENSE](LICENSE) file for details.
