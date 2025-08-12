# WebCOBOL Interpreter - Online IDE

[![Deploy with Vercel](https://vercel.com/button)](https://vercel.com/new/clone?repository-url=https%3A%2F%2Fgithub.com%2Fwebcobol%2Finterpreter-web)

A modern online IDE and API for executing COBOL code in the browser. Built with Next.js and designed for serverless deployment.

## 🚀 Quick Deploy

Click the button above to deploy your own instance on Vercel in seconds!

## 🌟 Features

- **Online IDE**: Monaco Editor with COBOL syntax highlighting
- **Real-time Execution**: Run COBOL code instantly in the browser
- **API Endpoint**: RESTful API for programmatic access
- **Serverless Ready**: Optimized for Vercel, AWS Lambda, and other platforms
- **No Dependencies**: Pure JavaScript COBOL interpreter
- **Modern UI**: Clean, responsive interface

## 🔧 API Usage

### Execute COBOL Code

**Endpoint**: `POST /api/interpret`

**Request Body**:
\`\`\`json
{
  "code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\n\nPROCEDURE DIVISION.\nDISPLAY \"Hello, World!\".\nSTOP RUN."
}
\`\`\`

**Response**:
\`\`\`json
{
  "success": true,
  "output": ["Hello, World!"],
  "errors": [],
  "warnings": [],
  "executionTime": 15,
  "variables": {}
}
\`\`\`

### Example with cURL

\`\`\`bash
curl -X POST https://your-deployment.vercel.app/api/interpret \
  -H "Content-Type: application/json" \
  -d '{
    "code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\n\nPROCEDURE DIVISION.\nDISPLAY \"Hello, World!\".\nSTOP RUN."
  }'
\`\`\`

### Example with JavaScript

\`\`\`javascript
const response = await fetch('/api/interpret', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    code: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. CALCULATOR.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-NUM1 PIC 9(3) VALUE 100.
      01 WS-NUM2 PIC 9(3) VALUE 50.
      01 WS-RESULT PIC 9(4).
      
      PROCEDURE DIVISION.
      ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
      DISPLAY "Result: " WS-RESULT.
      STOP RUN.
    `
  })
});

const result = await response.json();
console.log(result.output); // ["Result: 150"]
\`\`\`

## 🏗️ Local Development

### Prerequisites
- Node.js 18+
- npm or yarn

### Setup

\`\`\`bash
# Clone the repository
git clone https://github.com/webcobol/interpreter-web.git
cd interpreter-web

# Install dependencies
npm install

# Start development server
npm run dev
\`\`\`

Open [http://localhost:3000](http://localhost:3000) to see the IDE.

### Build for Production

\`\`\`bash
npm run build
npm start
\`\`\`

## 📚 COBOL Language Support

### Supported Features
- ✅ IDENTIFICATION DIVISION
- ✅ DATA DIVISION
- ✅ WORKING-STORAGE SECTION
- ✅ PROCEDURE DIVISION
- ✅ PIC clauses (X, 9, V)
- ✅ VALUE initialization
- ✅ DISPLAY statements
- ✅ MOVE operations
- ✅ Arithmetic (ADD, SUBTRACT)
- ✅ PERFORM loops
- ✅ IF-ELSE conditions
- ✅ Paragraph procedures

### Example Programs

#### Hello World
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.

PROCEDURE DIVISION.
DISPLAY "Hello, COBOL World!".
STOP RUN.
