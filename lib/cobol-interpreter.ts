interface CobolVariable {
  name: string
  type: "PIC" | "COMP" | "BINARY"
  size: number
  decimals?: number
  value: string | number
  level: number
}

interface CobolProgram {
  programId: string
  variables: Map<string, CobolVariable>
  procedures: Map<string, string[]>
  output: string[]
}

export class CobolInterpreter {
  private program: CobolProgram
  private currentLine = 0
  private lines: string[] = []

  constructor() {
    this.program = {
      programId: "",
      variables: new Map(),
      procedures: new Map(),
      output: [],
    }
  }

  public interpret(cobolCode: string): { success: boolean; output: string; error?: string } {
    try {
      this.lines = cobolCode
        .split("\n")
        .map((line) => line.trim())
        .filter((line) => line && !line.startsWith("*>"))
      this.currentLine = 0
      this.program.output = []

      // Parse das divisões
      this.parseIdentificationDivision()
      this.parseDataDivision()
      this.parseProcedureDivision()

      return {
        success: true,
        output: this.program.output.join("\n"),
      }
    } catch (error: any) {
      return {
        success: false,
        output: this.program.output.join("\n"),
        error: error.message,
      }
    }
  }

  private parseIdentificationDivision(): void {
    while (this.currentLine < this.lines.length) {
      const line = this.lines[this.currentLine]

      if (line.includes("PROGRAM-ID.")) {
        this.program.programId = line.split("PROGRAM-ID.")[1].trim().replace(".", "")
      }

      if (line.includes("DATA DIVISION") || line.includes("PROCEDURE DIVISION")) {
        break
      }

      this.currentLine++
    }
  }

  private parseDataDivision(): void {
    let inWorkingStorage = false

    while (this.currentLine < this.lines.length) {
      const line = this.lines[this.currentLine]

      if (line.includes("WORKING-STORAGE SECTION")) {
        inWorkingStorage = true
        this.currentLine++
        continue
      }

      if (line.includes("PROCEDURE DIVISION")) {
        break
      }

      if (inWorkingStorage && line.match(/^\d{2}\s+/)) {
        this.parseVariable(line)
      }

      this.currentLine++
    }
  }

  private parseVariable(line: string): void {
    const match = line.match(/^(\d{2})\s+([A-Z0-9-]+)(?:\s+PIC\s+([X9V$$$$]+))?(?:\s+VALUE\s+(.+))?\.?$/)

    if (match) {
      const [, level, name, pic, value] = match
      const variable: CobolVariable = {
        name: name,
        type: "PIC",
        size: this.calculateSize(pic || "X"),
        level: Number.parseInt(level),
        value: this.parseValue(value || "0"),
      }

      if (pic && pic.includes("V")) {
        variable.decimals = pic.split("V")[1].replace(/[()]/g, "").length
      }

      this.program.variables.set(name, variable)
    }
  }

  private calculateSize(pic: string): number {
    if (!pic) return 1

    const match = pic.match(/[X9]+|$$(\d+)$$/)
    if (match) {
      if (match[1]) return Number.parseInt(match[1])
      return match[0].length
    }
    return 1
  }

  private parseValue(value: string): string | number {
    if (!value) return ""

    value = value.replace(/['"]/g, "").trim()

    if (value === "SPACES") return ""
    if (value === "ZEROS" || value === "ZEROES") return 0
    if (!isNaN(Number(value))) return Number(value)

    return value
  }

  private parseProcedureDivision(): void {
    let currentProcedure = "MAIN"
    let procedureLines: string[] = []

    while (this.currentLine < this.lines.length) {
      const line = this.lines[this.currentLine]

      if (line.includes("PROCEDURE DIVISION")) {
        this.currentLine++
        continue
      }

      // Detectar novo parágrafo/procedimento
      if (line.match(/^[A-Z][A-Z0-9-]*\.$/) && !line.includes("STOP RUN")) {
        if (procedureLines.length > 0) {
          this.program.procedures.set(currentProcedure, [...procedureLines])
        }
        currentProcedure = line.replace(".", "")
        procedureLines = []
      } else {
        procedureLines.push(line)
      }

      this.currentLine++
    }

    // Adicionar último procedimento
    if (procedureLines.length > 0) {
      this.program.procedures.set(currentProcedure, procedureLines)
    }

    // Executar procedimento principal
    this.executeProcedure("MAIN")
  }

  private executeProcedure(procedureName: string): void {
    const lines = this.program.procedures.get(procedureName)
    if (!lines) return

    for (const line of lines) {
      this.executeLine(line)
    }
  }

  private executeLine(line: string): void {
    const trimmedLine = line.trim()

    if (trimmedLine.startsWith("DISPLAY")) {
      this.executeDisplay(trimmedLine)
    } else if (trimmedLine.startsWith("MOVE")) {
      this.executeMove(trimmedLine)
    } else if (trimmedLine.startsWith("ADD")) {
      this.executeAdd(trimmedLine)
    } else if (trimmedLine.startsWith("SUBTRACT")) {
      this.executeSubtract(trimmedLine)
    } else if (trimmedLine.startsWith("PERFORM")) {
      this.executePerform(trimmedLine)
    } else if (trimmedLine === "STOP RUN.") {
      return
    }
  }

  private executeDisplay(line: string): void {
    const match = line.match(/DISPLAY\s+(.+)\.?$/)
    if (!match) return

    const items = match[1].split(/\s+/)
    let output = ""

    for (const item of items) {
      const cleanItem = item.replace(/['"]/g, "")

      if (this.program.variables.has(cleanItem)) {
        const variable = this.program.variables.get(cleanItem)!
        output += variable.value.toString()
      } else {
        output += cleanItem
      }
      output += " "
    }

    this.program.output.push(output.trim())
  }

  private executeMove(line: string): void {
    const match = line.match(/MOVE\s+(.+)\s+TO\s+([A-Z0-9-]+)\.?$/)
    if (!match) return

    const [, source, target] = match
    const cleanSource = source.replace(/['"]/g, "")

    if (this.program.variables.has(target)) {
      let value: string | number

      if (this.program.variables.has(cleanSource)) {
        value = this.program.variables.get(cleanSource)!.value
      } else if (!isNaN(Number(cleanSource))) {
        value = Number(cleanSource)
      } else {
        value = cleanSource
      }

      const variable = this.program.variables.get(target)!
      variable.value = value
    }
  }

  private executeAdd(line: string): void {
    const match = line.match(/ADD\s+(.+)\s+TO\s+([A-Z0-9-]+)\.?$/)
    if (!match) return

    const [, source, target] = match

    if (this.program.variables.has(target)) {
      const targetVar = this.program.variables.get(target)!
      let addValue = 0

      if (this.program.variables.has(source)) {
        addValue = Number(this.program.variables.get(source)!.value)
      } else {
        addValue = Number(source)
      }

      targetVar.value = Number(targetVar.value) + addValue
    }
  }

  private executeSubtract(line: string): void {
    const match = line.match(/SUBTRACT\s+(.+)\s+FROM\s+([A-Z0-9-]+)\.?$/)
    if (!match) return

    const [, source, target] = match

    if (this.program.variables.has(target)) {
      const targetVar = this.program.variables.get(target)!
      let subtractValue = 0

      if (this.program.variables.has(source)) {
        subtractValue = Number(this.program.variables.get(source)!.value)
      } else {
        subtractValue = Number(source)
      }

      targetVar.value = Number(targetVar.value) - subtractValue
    }
  }

  private executePerform(line: string): void {
    const match = line.match(/PERFORM\s+([A-Z0-9-]+)\.?$/)
    if (!match) return

    const procedureName = match[1]
    this.executeProcedure(procedureName)
  }
}
