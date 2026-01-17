# LOS Effect System PoC

効果システム（Algebraic Effects）のProof of Concept。

## 概要

このPoCは、LOSの核となる効果システムを実装したものです。

### 効果システムとは

従来のプログラミングでは、副作用（I/O、状態変更など）は「実行」されます：

```haskell
-- 従来: 副作用が即座に実行される
main = do
    putStrLn "Hello"  -- ← ここでI/Oが発生
    name <- getLine   -- ← ここでI/Oが発生
    putStrLn name     -- ← ここでI/Oが発生
```

効果システムでは、副作用は「記述」されます：

```haskell
-- 効果システム: 副作用は記述のみ
program :: Member Console effs => Eff effs ()
program = do
    putLine "Hello"   -- ← I/Oの「記述」
    name <- getLine   -- ← I/Oの「記述」
    putLine name      -- ← I/Oの「記述」

-- ハンドラが実際の解釈を行う
-- 本番用: 実際にI/Oを実行
-- テスト用: モックで純粋に実行
```

### なぜOSに効果システムが重要か

| 従来のOS | 効果ベースのOS |
|---------|---------------|
| syscallは副作用を直接実行 | syscallは効果を「記述」 |
| テストが困難 | 純粋なテストが可能 |
| サンドボックスは実行時チェック | サンドボックスは型レベル |
| 動作が不透明 | 動作が型で明示 |

## 構成

```
src/LOS/Effect/
├── Core.hs       -- Eff型、Member制約、run関数
├── Console.hs    -- Console効果（入出力）
├── FileSystem.hs -- FileSystem効果（ファイル操作）
└── State.hs      -- State効果（可変状態）

poc/
├── README.md     -- このファイル
└── EffectDemo.hs -- デモプログラム

test/
└── Main.hs       -- テスト
```

## 実行方法

### 環境構築

```bash
# GHCup でHaskell環境をインストール
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# パスを通す（シェルを再起動するか以下を実行）
source ~/.ghcup/env
```

### ビルドと実行

```bash
# ビルド
cabal build

# デモ実行
cabal run effect-demo

# テスト実行
cabal test
```

## 主要な型

### `Eff effs a`

効果付き計算を表すモナド。

- `effs`: 使用する効果のリスト（型レベルリスト）
- `a`: 計算結果の型

```haskell
-- Console効果のみを使用し、()を返す
greet :: Member Console effs => Eff effs ()

-- Console効果とState Int効果を使用し、Stringを返す
complexProgram :: (Member Console effs, Member (State Int) effs)
               => Eff effs String
```

### `Member eff effs`

「効果effがリストeffsに含まれる」という制約。

### ハンドラ

効果を解釈する関数。同じプログラムに異なるハンドラを適用できる。

```haskell
-- 本番用: 実際のI/Oを実行
runConsoleIO :: Eff (Console ': effs) a -> Eff effs (IO a)

-- テスト用: 純粋に実行
runConsolePure :: [String] -> Eff (Console ': effs) a -> Eff effs ([String], a)
```

## LOSへの展望

このPoCを拡張し、以下を実現していく：

1. **プロセス効果**: fork, exec, wait
2. **メモリ効果**: allocate, free（線形型と組み合わせ）
3. **ネットワーク効果**: socket, connect, send, recv
4. **セッション型**: プロトコルを型で表現

最終的には、カーネル全体を「効果ハンドラの集合」として実装する。
