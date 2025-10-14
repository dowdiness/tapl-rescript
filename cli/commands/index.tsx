import React from 'react';
import {Text, Box} from 'ink';

export default function Index() {
	return (
		<Box flexDirection="column" padding={1}>
			<Text bold color="cyan">
				Lambda Calculus Compiler
			</Text>
			<Text dimColor> </Text>
			<Text>
				A compiler for lambda calculus expressions that generates LLVM IR.
			</Text>
			<Text dimColor> </Text>
			<Text bold>Usage:</Text>
			<Text>  lambda compile [options] &lt;expression&gt;</Text>
			<Text dimColor> </Text>
			<Text bold>Examples:</Text>
			<Text color="gray">  # Compile to LLVM IR (Phase 3)</Text>
			<Text>  lambda compile "((λx.x + 8) (12 - 5))"</Text>
			<Text dimColor> </Text>
			<Text color="gray">  # Show ANF representation</Text>
			<Text>  lambda compile --anf "λx.x + 1"</Text>
			<Text dimColor> </Text>
			<Text color="gray">  # Show AST</Text>
			<Text>  lambda compile --ast "1 + 2"</Text>
			<Text dimColor> </Text>
			<Text color="gray">  # Specify LLVM phase (1-4)</Text>
			<Text>  lambda compile --phase=2 "λx.x"</Text>
			<Text dimColor> </Text>
			<Text bold>Commands:</Text>
			<Text>  compile    Compile lambda expression to LLVM IR</Text>
			<Text dimColor> </Text>
			<Text>Run "lambda compile --help" for more information.</Text>
		</Box>
	);
}
