import React from 'react';
import {Text, Box} from 'ink';
import zod from 'zod';
import {createRequire} from 'module';
import {dirname, resolve} from 'path';
import * as Parser from '#parser'
import * as Compiler from '#compiler'
import * as Ast from "#ast"
import * as ANF from "#anf"
import * as ClosureConversion from "#closure-conversion"

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

type Props = {
  options: zod.infer<typeof options>;
  args: zod.infer<typeof args>;
};

export default function Compile({options, args}: Props) {
  const [expression] = args;

  try {
    const ast = Parser.parse(expression);

    if (options.ast) {
      return (
        <Box flexDirection="column">
          <Text color="green">AST:</Text>
          <Text>{Ast.printLam(ast)}</Text>
        </Box>
      );
    }

    if (options.anf) {
      const renamed = Ast.rename(ast);
      const anf = ANF.convert(renamed);
      const closure = ClosureConversion.convert(anf);
      const hoisted = Compiler.Hoisting.hoist(closure);

      return (
        <Box flexDirection="column">
          <Text color="green">ANF:</Text>
          <Text>{ANF.printANF(hoisted)}</Text>
        </Box>
      );
    }

    const llvm = Compiler.Compiler.compileToLLVM(ast, options.phase);

    return (
      <Box flexDirection="column">
        <Text color="green">LLVM IR (Phase {options.phase}):</Text>
        <Text>{llvm}</Text>
      </Box>
    );
  } catch (error) {
    return (
      <Box flexDirection="column">
        <Text color="red">Error:</Text>
        <Text color="red">
          {error instanceof Error ? error.message : String(error)}
        </Text>
      </Box>
    );
  }
}
