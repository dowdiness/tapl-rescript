import React from 'react';
import { Text, Box } from 'ink';
import zod from 'zod';
import { createRequire } from 'module';
import * as Parser from '#parser';
import * as Compiler from '#compiler';
import * as Ast from "#ast";
import * as ANF from "#anf";
import * as ClosureConversion from "#closure-conversion";
const require = createRequire(import.meta.url);
export const options = zod.object({
    phase: zod
        .number()
        .min(1)
        .max(4)
        .default(3)
        .describe('LLVM lowering phase (1-4)'),
    anf: zod.boolean().default(false).describe('Show ANF output instead of LLVM'),
    ast: zod.boolean().default(false).describe('Show AST output instead of LLVM'),
});
export const args = zod.tuple([
    zod.string().describe('Lambda expression to compile'),
]);
export default function Compile({ options, args }) {
    const [expression] = args;
    try {
        const ast = Parser.parse(expression);
        if (options.ast) {
            return (React.createElement(Box, { flexDirection: "column" },
                React.createElement(Text, { color: "green" }, "AST:"),
                React.createElement(Text, null, Ast.printLam(ast))));
        }
        if (options.anf) {
            const renamed = Ast.rename(ast);
            const anf = ANF.convert(renamed);
            const closure = ClosureConversion.convert(anf);
            const hoisted = Compiler.Hoisting.hoist(closure);
            return (React.createElement(Box, { flexDirection: "column" },
                React.createElement(Text, { color: "green" }, "ANF:"),
                React.createElement(Text, null, ANF.printANF(hoisted))));
        }
        const llvm = Compiler.Compiler.compileToLLVM(ast, options.phase);
        return (React.createElement(Box, { flexDirection: "column" },
            React.createElement(Text, { color: "green" },
                "LLVM IR (Phase ",
                options.phase,
                "):"),
            React.createElement(Text, null, llvm)));
    }
    catch (error) {
        return (React.createElement(Box, { flexDirection: "column" },
            React.createElement(Text, { color: "red" }, "Error:"),
            React.createElement(Text, { color: "red" }, error instanceof Error ? error.message : String(error))));
    }
}
