import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    // ReScriptが生成する.bs.mjsファイルをテスト対象に含める
    include: [
      '**/*.test.{js,mjs,ts}',
      '**/*_test.{js,mjs,ts}',
      '**/*.test.bs.{js,mjs,ts}'
    ],
    globals: true,
    environment: 'node',
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: true
      }
    }
  }
})
