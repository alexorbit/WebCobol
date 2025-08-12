import type React from "react"
import "./globals.css"

export const metadata = {
  title: "WebCOBOL Interpreter",
  description: "Online COBOL interpreter and IDE",
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className="bg-gray-50 text-gray-800">{children}</body>
    </html>
  )
}
