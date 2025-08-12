const { CobolInterpreter } = require("../dist/interpreter")

describe("CobolInterpreter", () => {
  let interpreter

  beforeEach(() => {
    interpreter = new CobolInterpreter()
  })

  test("should execute simple DISPLAY statement", () => {
    const code = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO.
      
      PROCEDURE DIVISION.
      DISPLAY "Hello World".
      STOP RUN.
    `

    const result = interpreter.interpret(code)
    expect(result.success).toBe(true)
    expect(result.output).toBe("Hello World")
  })

  test("should handle variables and MOVE statement", () => {
    const code = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. VARIABLES.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-NAME PIC X(10) VALUE "John".
      01 WS-AGE PIC 9(2) VALUE 25.
      
      PROCEDURE DIVISION.
      DISPLAY WS-NAME.
      DISPLAY WS-AGE.
      MOVE "Jane" TO WS-NAME.
      DISPLAY WS-NAME.
      STOP RUN.
    `

    const result = interpreter.interpret(code)
    expect(result.success).toBe(true)
    expect(result.output).toContain("John")
    expect(result.output).toContain("25")
    expect(result.output).toContain("Jane")
  })

  test("should perform arithmetic operations", () => {
    const code = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. MATH.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-NUM1 PIC 9(3) VALUE 100.
      01 WS-NUM2 PIC 9(3) VALUE 50.
      
      PROCEDURE DIVISION.
      ADD WS-NUM2 TO WS-NUM1.
      DISPLAY WS-NUM1.
      SUBTRACT 25 FROM WS-NUM1.
      DISPLAY WS-NUM1.
      STOP RUN.
    `

    const result = interpreter.interpret(code)
    expect(result.success).toBe(true)
    expect(result.output).toContain("150")
    expect(result.output).toContain("125")
  })

  test("should handle PERFORM statements", () => {
    const code = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PERFORM-TEST.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-COUNTER PIC 9(2) VALUE 1.
      
      PROCEDURE DIVISION.
      PERFORM SHOW-MESSAGE.
      STOP RUN.
      
      SHOW-MESSAGE.
      DISPLAY "Message from procedure".
    `

    const result = interpreter.interpret(code)
    expect(result.success).toBe(true)
    expect(result.output).toBe("Message from procedure")
  })

  test("should handle syntax errors gracefully", () => {
    const code = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. ERROR-TEST.
      
      PROCEDURE DIVISION.
      INVALID-STATEMENT.
      STOP RUN.
    `

    const result = interpreter.interpret(code)
    expect(result.success).toBe(true) // Should not crash
  })
})
