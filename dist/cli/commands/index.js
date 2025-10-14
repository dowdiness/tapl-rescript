import React from 'react';
import { Text, Box } from 'ink';
export default function Index() {
    return (React.createElement(Box, { flexDirection: "column", padding: 1 },
        React.createElement(Text, { bold: true, color: "cyan" }, "Lambda Calculus Compiler"),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, null, "A compiler for lambda calculus expressions that generates LLVM IR."),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, { bold: true }, "Usage:"),
        React.createElement(Text, null, "  lambda compile [options] <expression>"),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, { bold: true }, "Examples:"),
        React.createElement(Text, { color: "gray" }, "  # Compile to LLVM IR (Phase 3)"),
        React.createElement(Text, null, "  lambda compile \"((\u03BBx.x + 8) (12 - 5))\""),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, { color: "gray" }, "  # Show ANF representation"),
        React.createElement(Text, null, "  lambda compile --anf \"\u03BBx.x + 1\""),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, { color: "gray" }, "  # Show AST"),
        React.createElement(Text, null, "  lambda compile --ast \"1 + 2\""),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, { color: "gray" }, "  # Specify LLVM phase (1-4)"),
        React.createElement(Text, null, "  lambda compile --phase=2 \"\u03BBx.x\""),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, { bold: true }, "Commands:"),
        React.createElement(Text, null, "  compile    Compile lambda expression to LLVM IR"),
        React.createElement(Text, { dimColor: true }, " "),
        React.createElement(Text, null, "Run \"lambda compile --help\" for more information.")));
}
