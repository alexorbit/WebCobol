export interface CobolVariable {
  name: string
  type: "PIC" | "COMP" | "BINARY"
  size: number
  decimals?: number
  value: string | number
  level: number
}

export interface CobolProgram {
  programId: string
  variables: Map<string, CobolVariable>
  procedures: Map<string, string[]>
  output: string[]
}

export interface ExecutionResult {
  success: boolean
  output: string
  error?: string
  executionTime?: number
  memoryUsage?: number
  variables?: Record<string, any>
}

export interface InterpreterOptions {
  maxExecutionTime?: number
  maxMemoryUsage?: number
  enableWebExtensions?: boolean
  enableSQLExtensions?: boolean
  debugMode?: boolean
}

export interface ValidationResult {
  isValid: boolean
  errors: string[]
  warnings: string[]
}

export interface AST {
  type: "program"
  programId: string
  divisions: {
    identification?: any
    environment?: any
    data?: any
    procedure?: any
  }
}
