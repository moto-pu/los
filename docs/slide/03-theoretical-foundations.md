---
marp: true
theme: default
paginate: true
---

# 理論的基盤

HSOSを支える4つの核心概念

---

# 4つの核心概念

1. **Pure Functions（純粋関数）**
2. **Algebraic Effects（代数的効果）**
3. **Linear Types（線形型）**
4. **Session Types（セッション型）**

---

<!-- _class: lead -->

# Part 1: Pure Functions
# 純粋関数

---

# 純粋関数とは

**数学的な関数**のこと

中学で習った `f(x) = x + 1` を思い出す

- `f(3)` は常に `4`
- 何回計算しても `4`
- 世界の何かが変わることもない

---

# 定義: 2つの性質

## 1. 参照透過性（Referential Transparency）
同じ入力には常に同じ出力を返す

## 2. 副作用がない（No Side Effects）
外の世界に影響を与えない

---

# 純粋関数の例

```haskell
-- 純粋: 何度呼んでも同じ
add :: Int -> Int -> Int
add x y = x + y

add 2 3  -- 常に 5
add 2 3  -- 常に 5
add 2 3  -- 常に 5
```

---

# 非純粋関数の例

```python
# 非純粋: 呼ぶたびに結果が変わる
import random
def impure_add(x, y):
    return x + y + random.randint(0, 10)

impure_add(2, 3)  # 8 かもしれない
impure_add(2, 3)  # 12 かもしれない
```

---

# 副作用の例

| 副作用 | 説明 |
|--------|------|
| 変数の書き換え | グローバル変数やオブジェクトの状態変更 |
| I/O操作 | ファイル読み書き、画面出力 |
| 例外送出 | 通常の戻り値以外の経路で脱出 |
| 乱数生成 | 呼ぶたびに結果が変わる |
| 現在時刻取得 | 呼ぶたびに結果が変わる |

---

# なぜ重要か: 推論が容易

```haskell
let x = add 2 3
in x + x
```

純粋なら**必ず10**

`add 2 3` を `5` に置き換えられる（参照透過性）

---

# なぜ重要か: テストが容易

```haskell
-- テストは入出力の確認だけ
test_add = add 2 3 == 5
```

モックもスタブも不要

---

# なぜ重要か: 並行処理が安全

```haskell
-- 並列実行しても安全
parMap double [1, 2, 3, 4]
```

共有状態がないので競合状態が起きない

---

# なぜ重要か: 形式検証が可能

数学的証明が適用できる

```
証明: ∀x. double (double x) = x * 4

double (double x)
= double (x * 2)      -- doubleの定義
= (x * 2) * 2         -- doubleの定義
= x * 4               -- 算術
```

---

# Haskellでの扱い

純粋関数と副作用のある計算を**型で区別**

```haskell
-- 純粋関数（副作用なし）
add :: Int -> Int -> Int

-- 副作用あり（IOモナド）
printHello :: IO ()
printHello = putStrLn "Hello"
```

型を見れば副作用の有無がわかる

---

# OS設計への示唆

「純粋」と「実用」の両立

副作用を**排除**するのではなく**制御**する

Haskellは副作用を**型で追跡**し、純粋な部分と分離

---

<!-- _class: lead -->

# Part 2: Algebraic Effects
# 代数的効果

---

# 代数的効果とは

**副作用を操作として定義し、その解釈を後から与える**仕組み

- 従来: 「ファイルを読む」→ 実際にファイルを読む
- 代数的効果: 「ファイルを読みたい」という**要求**を出す → **ハンドラ**が解釈

---

# 例: 例外（最も単純な効果）

```haskell
-- 効果の定義: 「失敗」という操作
effect Fail where
  fail : String -> a

-- プログラム: 効果を使う
divide :: Int -> Int -> Fail Int
divide x 0 = fail "division by zero"
divide x y = x / y
```

---

# ハンドラによる解釈

```haskell
-- ハンドラ1: 例外をMaybeに変換
handleMaybe :: Fail a -> Maybe a
handleMaybe (fail _) = Nothing
handleMaybe x        = Just x

-- ハンドラ2: デフォルト値を返す
handleDefault :: a -> Fail a -> a
handleDefault def (fail _) = def
handleDefault _   x        = x
```

**同じプログラムに異なるハンドラを適用できる！**

---

# 例: 状態

```haskell
-- 効果の定義
effect State s where
  get : () -> s        -- 状態を取得
  put : s -> ()        -- 状態を設定

-- プログラム
increment :: State Int ()
increment = do
  x <- get ()
  put (x + 1)
```

---

# 核心: 継続（Continuation）

ハンドラが強力な理由は**継続**を操作できること

```
継続 = 「この操作の後、何をするか」
```

ハンドラは継続を...
- 実行する（普通の動作）
- 実行しない（例外のように中断）
- 複数回実行する（非決定性）
- 保存して後で実行（コルーチン）

---

# モナドとの比較: モナドの問題

```haskell
-- モナドは合成が面倒
type App a = StateT Int (ReaderT Config (ExceptT Error IO)) a

-- リフトが必要
example :: App ()
example = do
  x <- get                           -- State
  config <- lift ask                 -- Reader（1段リフト）
  lift $ lift $ throwError "oops"    -- Except（2段リフト）
```

---

# モナドとの比較: 代数的効果の解決

```haskell
-- 効果は直交的に合成
example :: {State Int, Reader Config, Except Error} ()
example = do
  x <- get ()          -- リフト不要
  config <- ask ()     -- リフト不要
  throw "oops"         -- リフト不要
```

---

# OS設計への応用

```
┌─────────────────────────────────────────────┐
│            User Program                      │
│  read(fd)   -- FileSystem効果を発行          │
│  yield()    -- Scheduler効果を発行           │
│  send(msg)  -- Network効果を発行             │
├─────────────────────────────────────────────┤
│            Effect Handlers (Kernel)          │
│  FileSystemHandler: read → ディスクアクセス  │
│  SchedulerHandler:  yield → コンテキスト切替│
│  NetworkHandler:    send → パケット送信      │
└─────────────────────────────────────────────┘
```

---

# カーネル = 効果ハンドラの集合体

```haskell
-- システムコールを効果として定義
effect Syscall where
  read  : FileDescriptor -> Bytes
  write : FileDescriptor -> Bytes -> ()
  fork  : () -> ProcessId
  exit  : Int -> Never
```

---

# テストが劇的に簡単に

- 本番: 実際のファイルシステムハンドラ
- テスト: メモリ上のモックハンドラ

同じプログラムコードで、ハンドラだけ差し替え可能

---

<!-- _class: lead -->

# Part 3: Linear Types
# 線形型

---

# 線形型とは

**「この値はちょうど1回だけ使わなければならない」**
という制約を型で表現

---

# 日常生活の例

- 映画チケット: 1回使ったら無効
- 現金: 使ったら手元からなくなる
- ホテルの鍵: チェックアウト時に返却必須

プログラミングでは**リソース**がこれに相当

---

# リソースの例

- ファイルハンドル: 開いたら閉じる
- メモリ: 確保したら解放する
- ロック: 取得したら解放する

---

# 型システムの分類

```
通常の型:    何回でも使える、使わなくてもOK
線形型:      ちょうど1回使う
アフィン型:  最大1回（使わなくてもOK）
関連型:      最低1回（複数回OK）
```

---

# 問題: C言語でのファイルハンドル誤用

```c
// バグ1: use-after-close
FILE *f = fopen("data.txt", "r");
fclose(f);
fread(buf, 1, 100, f);  // 💥 解放済みメモリへのアクセス

// バグ2: double-close
fclose(f);
fclose(f);  // 💥 二重解放

// バグ3: リソースリーク
FILE *f = fopen("data.txt", "r");
if (error) return;  // 💥 fがクローズされない
```

---

# 線形型による解決

```haskell
-- 線形矢印 (⊸) : 引数を「ちょうど1回」使う

-- ファイルを閉じる: ハンドルを消費する
close :: Handle ⊸ IO ()

-- 読み取り: ハンドルを使って新しいハンドルを返す
read :: Handle ⊸ IO (ByteString, Handle)
```

---

# コンパイル時のエラー検出

```haskell
-- ❌ コンパイルエラー: use-after-close
bad1 = do
  h <- open "data.txt"
  close h
  (content, h') <- read h  -- エラー! hは既に使われた

-- ❌ コンパイルエラー: double-close
bad2 = do
  close h
  close h  -- エラー! hは既に使われた
```

---

# コンパイル時のエラー検出（続き）

```haskell
-- ❌ コンパイルエラー: リソースリーク
bad3 = do
  h <- open "data.txt"
  return ()  -- エラー! hが使われていない

-- ✅ 正しいコード
good = do
  h <- open "data.txt"
  (content, h') <- read h   -- hを消費、h'を取得
  close h'                  -- h'を消費
  return content
```

---

# Haskellでの Linear Types（GHC 9.0+）

```haskell
{-# LANGUAGE LinearTypes #-}

-- 線形関数の定義
dup :: a %1 -> (a, a)  -- これは型エラー！

-- 正しい線形関数
id :: a %1 -> a
id x = x  -- xをちょうど1回使用
```

`%1` は「線形に使う」という意味

---

# OS設計への応用: メモリ管理

```haskell
-- メモリ確保: 線形なポインタを返す
malloc :: Size -> IO (Ptr a %1 -> IO ())

-- メモリ解放: ポインタを消費
free :: Ptr a %1 -> IO ()
```

---

# OS設計への応用: ロック管理

```haskell
-- ロック取得: 線形なガードを返す
acquire :: Lock -> IO (Guard %1 -> IO ())

-- ロック解放: ガードを消費
release :: Guard %1 -> IO ()
```

デッドロックフリーの順序付けを型で強制可能

---

# 線形型 vs Rust所有権

| 観点 | Rust | Linear Haskell |
|------|------|----------------|
| 型システム | アフィン型 | 線形型 |
| リソース解放 | drop自動 | 明示的消費 |
| 設計思想 | 便利さ優先 | 明示性優先 |

---

<!-- _class: lead -->

# Part 4: Session Types
# セッション型

---

# セッション型とは

**通信プロトコルを型として表現し、コンパイル時に正しさを検証**

---

# 日常の例

ATMの操作:
1. カード挿入
2. 暗証番号入力
3. 金額選択
4. 現金受取
5. カード返却

**順序が決まっている**
途中をスキップしたり、順序を変えたりできない

---

# ATMプロトコルの図

```
クライアント          サーバー
     |  ---- カード番号 ---->  |
     |  <--- 暗証番号要求 ---  |
     |  ---- 暗証番号 ---->    |
     |  <--- 残高 ---          |
     |  ---- 引き出し額 ---->  |
     |  <--- 現金 ---          |
```

---

# 型での表現

```haskell
type ATMClient =
  Send CardNumber       -- カード番号を送信
  (Recv PINRequest      -- 暗証番号要求を受信
  (Send PIN             -- 暗証番号を送信
  (Recv Balance         -- 残高を受信
  (Send Amount          -- 引き出し額を送信
  (Recv Cash            -- 現金を受信
  End)))))
```

---

# 基本操作

```haskell
Send t s  -- 型tのデータを送信し、セッションsを続ける
Recv t s  -- 型tのデータを受信し、セッションsを続ける
End       -- セッション終了
```

---

# 双対性（Duality）

**一方が送信するとき、他方は受信する**

```haskell
Dual (Send t s) = Recv t (Dual s)
Dual (Recv t s) = Send t (Dual s)
Dual End        = End
```

サーバー側は `Dual ATMClient`

---

# 選択と分岐

```haskell
-- クライアントが選択する
type ATMChoice = Select [
  "withdraw": Send Amount (Recv Cash End),
  "deposit":  Send Cash (Recv Receipt End),
  "balance":  Recv Balance End
]
```

---

# プロトコル違反の検出

```haskell
-- ❌ コンパイルエラー: 順序違反
badClient session = do
  send session (PIN "1234")  -- エラー! まずカード番号を送るべき

-- ❌ コンパイルエラー: 型の不一致
badClient2 session = do
  send session "hello"  -- エラー! CardNumberを送るべき
```

---

# デッドロックフリー

セッション型を適切に設計すると
**デッドロックが起きないことを証明**できる

一方がSendなら他方はRecv
→ 両方が同時にwaitすることがない

---

# OS設計への応用: システムコールプロトコル

```haskell
type FileReadProtocol =
  Send OpenRequest
  (Recv (Either Error FileHandle)
  (Rec x. Select [
    "read":  Send ReadRequest (Recv Bytes x),
    "close": Send CloseRequest End
  ]))
```

---

# OS設計への応用: IPC

```haskell
type WorkerProtocol = Rec x. Offer [
  "task":     Recv Task (Send Result x),
  "status":   Send Status x,
  "shutdown": End
]

-- カーネル側
type ManagerProtocol = Dual WorkerProtocol
```

---

# Session Types + Linear Types

- **Session Types**: プロトコルの**順序**を保証
- **Linear Types**: チャネルの**所有権**を保証

組み合わせると:
- プロトコル違反なし
- リソースリークなし
- デッドロックフリー

---

<!-- _class: lead -->

# まとめ

---

# 4つの概念の関係

```
Pure Functions
└── 基盤: 副作用のない計算、形式検証の土台
     │
     ▼
Algebraic Effects
└── 副作用の構造化: OSサービスを効果として抽象化
     │
     ▼
Linear Types
└── リソース管理: ハードウェアリソースの安全な操作
     │
     ▼
Session Types
└── 通信プロトコル: IPC/システムコールの正確性
```

---

# 各概念の役割

| 概念 | 何を保証するか | OS設計での役割 |
|------|---------------|---------------|
| Pure Functions | 副作用の分離 | カーネルロジックの検証可能性 |
| Algebraic Effects | 効果の合成可能性 | OSサービスの抽象化 |
| Linear Types | リソースの使用回数 | メモリ、ハンドル、ロックの安全性 |
| Session Types | プロトコルの遵守 | IPC、システムコール、ドライバ通信 |

---

# HSOSが目指す統合

```
┌─────────────────────────────────────────────┐
│            HSOS Architecture                 │
├─────────────────────────────────────────────┤
│  Algebraic Effects  → OSサービスの抽象化    │
│  Linear Types       → リソース管理の安全性  │
│  Session Types      → 通信プロトコルの正確性│
│  Pure Functions     → 形式検証の容易さ      │
└─────────────────────────────────────────────┘
```
