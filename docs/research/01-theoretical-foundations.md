# 理論的基盤: 型理論とOS設計

HSOSの設計を支える4つの核心概念について解説する。

---

## 目次

1. [Pure Functions（純粋関数）](#1-pure-functions純粋関数)
2. [Algebraic Effects（代数的効果）](#2-algebraic-effects代数的効果)
3. [Linear Types（線形型）](#3-linear-types線形型)
4. [Session Types（セッション型）](#4-session-typesセッション型)
5. [まとめ: 4つの概念の関係](#5-まとめ-4つの概念の関係)

---

## 1. Pure Functions（純粋関数）

### 直感的理解

**純粋関数とは「数学的な関数」のこと**。

中学で習った関数 `f(x) = x + 1` を思い出してほしい。

- `f(3)` は常に `4`
- 何回計算しても `4`
- 世界の何かが変わることもない

これが純粋関数の本質である。

### 定義

純粋関数は2つの性質を満たす：

#### 1. 参照透過性（Referential Transparency）

**同じ入力には常に同じ出力を返す**

```haskell
-- 純粋: 何度呼んでも同じ
add :: Int -> Int -> Int
add x y = x + y

add 2 3  -- 常に 5
add 2 3  -- 常に 5
add 2 3  -- 常に 5
```

```python
# 非純粋: 呼ぶたびに結果が変わる
import random
def impure_add(x, y):
    return x + y + random.randint(0, 10)

impure_add(2, 3)  # 8 かもしれない
impure_add(2, 3)  # 12 かもしれない
```

#### 2. 副作用がない（No Side Effects）

**外の世界に影響を与えない**

```haskell
-- 純粋: 世界は何も変わらない
double :: Int -> Int
double x = x * 2
```

```python
# 非純粋: グローバル変数を書き換える
counter = 0
def impure_increment():
    global counter
    counter += 1  # 外の世界が変わる！
    return counter
```

### 副作用の例

| 副作用 | 説明 |
|--------|------|
| 変数の書き換え | グローバル変数やオブジェクトの状態変更 |
| I/O操作 | ファイル読み書き、画面出力、ネットワーク |
| 例外送出 | 通常の戻り値以外の経路で脱出 |
| 乱数生成 | 呼ぶたびに結果が変わる |
| 現在時刻取得 | 呼ぶたびに結果が変わる |

### なぜ重要か

#### 1. 推論が容易

```haskell
-- この式の結果は？
let x = add 2 3
in x + x
```

純粋なら **必ず10**。`add 2 3` を `5` に置き換えられる（参照透過性）。

非純粋だと、`add` が呼ばれるたびに副作用が起きるかもしれない。

#### 2. テストが容易

```haskell
-- テストは入出力の確認だけ
test_add = add 2 3 == 5
```

モックもスタブも不要。

#### 3. 並行処理が安全

```haskell
-- 並列実行しても安全
parMap double [1, 2, 3, 4]
```

共有状態がないので競合状態が起きない。

#### 4. 形式検証が可能

数学的証明が適用できる。

```
証明: ∀x. double (double x) = x * 4

double (double x)
= double (x * 2)      -- doubleの定義
= (x * 2) * 2         -- doubleの定義
= x * 4               -- 算術
```

### Haskellでの扱い

Haskellでは**純粋関数と副作用のある計算を型で区別**する：

```haskell
-- 純粋関数（副作用なし）
add :: Int -> Int -> Int
add x y = x + y

-- 副作用あり（IOモナド）
printHello :: IO ()
printHello = putStrLn "Hello"

-- 型を見れば副作用の有無がわかる
pure1 :: Int -> Int           -- 純粋
pure2 :: String -> Bool       -- 純粋
impure1 :: IO String          -- 副作用あり
impure2 :: FilePath -> IO ()  -- 副作用あり
```

### OS設計への示唆

**「純粋」と「実用」の両立**:
「副作用なしでプログラムが書けるのか？」という疑問は自然。
答えは「副作用を**排除**するのではなく**制御**する」。
Haskellは副作用を**型で追跡**し、純粋な部分と分離する。

---

## 2. Algebraic Effects（代数的効果）

### 直感的理解

**「副作用を操作（オペレーション）として定義し、その解釈を後から与える」仕組み**。

例えるなら：
- 従来のプログラミング: 「ファイルを読む」→ 実際にファイルを読む
- 代数的効果: 「ファイルを読みたい」という**要求**を出す → **ハンドラ**が解釈

### 具体例で理解する

#### 例外（最も単純な効果）

```
プログラム: 「エラー "not found" を投げたい」
ハンドラ:   「エラーを受け取ったら Nothing を返す」
```

```haskell
-- 効果の定義: 「失敗」という操作
effect Fail where
  fail : String -> a

-- プログラム: 効果を使う
divide :: Int -> Int -> Fail Int
divide x 0 = fail "division by zero"
divide x y = x / y

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

#### 状態（もう少し複雑な効果）

```haskell
-- 効果の定義: 「状態」という操作群
effect State s where
  get : () -> s        -- 状態を取得
  put : s -> ()        -- 状態を設定

-- プログラム: カウンターをインクリメント
increment :: State Int ()
increment = do
  x <- get ()
  put (x + 1)

-- ハンドラ: 実際の状態を持つ実装
handleState :: s -> State s a -> (a, s)
handleState s (get ())   = (s, s)           -- 現在の状態を返す
handleState s (put s')   = ((), s')         -- 状態を更新
handleState s (return x) = (x, s)
```

### 核心概念: 継続（Continuation）

ハンドラが強力な理由は**継続**を操作できること。

```
継続 = 「この操作の後、何をするか」
```

```haskell
-- プログラム
example = do
  x <- get ()      -- ← ここで一時停止
  put (x + 1)      -- ← これが「継続」
  return x

-- ハンドラは継続を...
-- - 実行する（普通の動作）
-- - 実行しない（例外のように中断）
-- - 複数回実行する（非決定性）
-- - 保存して後で実行（コルーチン）
```

### モナドとの比較

#### モナドの問題

```haskell
-- モナドは合成が面倒
type App a = StateT Int (ReaderT Config (ExceptT Error IO)) a
--           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--           効果を重ねるたびに複雑になる

-- リフトが必要
example :: App ()
example = do
  x <- get                           -- State
  config <- lift ask                 -- Reader（1段リフト）
  lift $ lift $ throwError "oops"    -- Except（2段リフト）
```

#### 代数的効果の解決

```haskell
-- 効果は直交的に合成
example :: {State Int, Reader Config, Except Error} ()
example = do
  x <- get ()          -- リフト不要
  config <- ask ()     -- リフト不要
  throw "oops"         -- リフト不要
```

### OS設計への応用

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

**カーネル = 効果ハンドラの集合体**

```haskell
-- システムコールを効果として定義
effect Syscall where
  read  : FileDescriptor -> Bytes
  write : FileDescriptor -> Bytes -> ()
  fork  : () -> ProcessId
  exit  : Int -> Never

-- ユーザープログラム
userProgram :: Syscall ()
userProgram = do
  content <- read stdin
  write stdout content
  exit 0

-- カーネルがハンドラを提供
runWithKernel :: Syscall a -> IO a
runWithKernel = ...
```

### テストへの恩恵

**テストが劇的に簡単になる**:
- 本番: 実際のファイルシステムハンドラ
- テスト: メモリ上のモックハンドラ

同じプログラムコードで、ハンドラだけ差し替え可能。

---

## 3. Linear Types（線形型）

### 直感的理解

**「この値はちょうど1回だけ使わなければならない」という制約を型で表現**する。

日常生活の例：
- 映画チケット: 1回使ったら無効（コピーできない、使わないと無駄）
- 現金: 使ったら手元からなくなる
- ホテルの鍵: チェックアウト時に返却必須

プログラミングでは**リソース**がこれに相当：
- ファイルハンドル: 開いたら閉じる
- メモリ: 確保したら解放する
- ロック: 取得したら解放する

### 型システムの分類

```
┌─────────────────────────────────────────────────┐
│                 Structural Rules                │
├─────────────────┬───────────────────────────────┤
│ Contraction     │ x を複数回使える              │
│ (収縮)          │ f(x, x) OK                    │
├─────────────────┼───────────────────────────────┤
│ Weakening       │ x を使わなくてもよい          │
│ (弱化)          │ let x = ... in 42 OK          │
└─────────────────┴───────────────────────────────┘

通常の型:    Contraction ✓  Weakening ✓  (何回でも、使わなくてもOK)
線形型:      Contraction ✗  Weakening ✗  (ちょうど1回)
アフィン型:  Contraction ✗  Weakening ✓  (最大1回、使わなくてもOK)
関連型:      Contraction ✓  Weakening ✗  (最低1回、複数回OK)
```

### 具体例

#### 問題: ファイルハンドルの誤用

```c
// C言語での典型的なバグ

// バグ1: use-after-close
FILE *f = fopen("data.txt", "r");
fclose(f);
fread(buf, 1, 100, f);  // 解放済みメモリへのアクセス

// バグ2: double-close
FILE *f = fopen("data.txt", "r");
fclose(f);
fclose(f);  // 二重解放

// バグ3: リソースリーク
FILE *f = fopen("data.txt", "r");
if (error) return;  // fがクローズされない
fclose(f);
```

#### 線形型による解決

```haskell
-- 線形矢印 (⊸) : 引数を「ちょうど1回」使う
-- 通常矢印 (→) : 引数を「何回でも」使える

-- ファイルを開く: 線形なハンドルを返す
open :: FilePath -> IO (Handle ⊸ IO ())
--                      ^^^^^^^^^^^^^^
--                      「Handleを受け取り、ちょうど1回使う関数」を返す

-- ファイルを閉じる: ハンドルを消費する
close :: Handle ⊸ IO ()
--       ^^^^^^
--       Handleを「ちょうど1回」使う（消費する）

-- 読み取り: ハンドルを使って新しいハンドルを返す
read :: Handle ⊸ IO (ByteString, Handle)
--      ^^^^^^                   ^^^^^^
--      古いハンドルを消費       新しいハンドルを返す
```

#### コンパイル時のエラー検出

```haskell
-- ❌ コンパイルエラー: use-after-close
bad1 = do
  h <- open "data.txt"
  close h
  (content, h') <- read h  -- エラー! hは既に使われた

-- ❌ コンパイルエラー: double-close
bad2 = do
  h <- open "data.txt"
  close h
  close h  -- エラー! hは既に使われた

-- ❌ コンパイルエラー: リソースリーク
bad3 = do
  h <- open "data.txt"
  return ()  -- エラー! hが使われていない

-- ✅ 正しいコード
good = do
  h <- open "data.txt"
  (content, h') <- read h   -- hを消費、h'を取得
  (more, h'') <- read h'    -- h'を消費、h''を取得
  close h''                 -- h''を消費
  return (content <> more)
```

### Haskellでの Linear Types（GHC 9.0+）

```haskell
{-# LANGUAGE LinearTypes #-}

-- 線形関数の定義
dup :: a %1 -> (a, a)  -- これは型エラー！aを2回使っている

-- 正しい線形関数
id :: a %1 -> a
id x = x  -- xをちょうど1回使用

-- 線形なペア操作
swap :: (a, b) %1 -> (b, a)
swap (x, y) = (y, x)  -- x, y それぞれ1回使用
```

`%1` は「線形に使う」という意味（multiplicityが1）。

### OS設計への応用

#### メモリ管理

```haskell
-- メモリ確保: 線形なポインタを返す
malloc :: Size -> IO (Ptr a %1 -> IO ())
--                    ^^^^^^^^^^^^^^^^^
--                    「Ptrをちょうど1回使う関数」

-- メモリ解放: ポインタを消費
free :: Ptr a %1 -> IO ()

-- 使用例
example = do
  withMalloc 1024 $ \ptr -> do
    poke ptr 42      -- ptrを使用
    peek ptr         -- ...
    -- スコープを抜けると自動的にfreeが呼ばれる
```

#### ロック管理

```haskell
-- ロック取得: 線形なガードを返す
acquire :: Lock -> IO (Guard %1 -> IO ())

-- ロック解放: ガードを消費
release :: Guard %1 -> IO ()

-- デッドロックフリーの順序付け
-- 型レベルで「Lock Aを取ってからLock Bを取る」を強制できる
```

### 線形型 vs Rust所有権

| 観点 | Rust | Linear Haskell |
|------|------|----------------|
| 型システム | アフィン型（使わなくてもよい） | 線形型（必ず使う） |
| リソース解放 | dropが自動で呼ばれる | 明示的な消費が必要 |
| 設計思想 | 便利さを優先 | 明示性を優先 |

OS設計では、リソースの**明示的なライフサイクル管理**が重要な場面がある。

---

## 4. Session Types（セッション型）

### 直感的理解

**「通信プロトコルを型として表現し、コンパイル時に正しさを検証」する仕組み**。

日常の例：
- ATMの操作: カード挿入 → 暗証番号入力 → 金額選択 → 現金受取 → カード返却
- 電話: 発信 → 相手が出る → 会話 → 切る

**順序が決まっている**。途中をスキップしたり、順序を変えたりできない。

### 具体例: ATMプロトコル

```
クライアント（利用者）          サーバー（ATM）
        |                            |
        |  ---- カード番号 ---->     |
        |  <--- 暗証番号要求 ---     |
        |  ---- 暗証番号 ---->       |
        |  <--- 残高 ---             |
        |  ---- 引き出し額 ---->     |
        |  <--- 現金 ---             |
        |                            |
```

これを**型**で表現する：

```haskell
-- クライアント側のプロトコル型
type ATMClient =
  Send CardNumber       -- カード番号を送信
  (Recv PINRequest      -- 暗証番号要求を受信
  (Send PIN             -- 暗証番号を送信
  (Recv Balance         -- 残高を受信
  (Send Amount          -- 引き出し額を送信
  (Recv Cash            -- 現金を受信
  End)))))              -- 終了

-- サーバー側は「双対」（Dual）
type ATMServer = Dual ATMClient
-- = Recv CardNumber (Send PINRequest (Recv PIN ...))
```

### 核心概念

#### 1. 基本操作

```haskell
Send t s  -- 型tのデータを送信し、セッションsを続ける
Recv t s  -- 型tのデータを受信し、セッションsを続ける
End       -- セッション終了
```

#### 2. 双対性（Duality）

**一方が送信するとき、他方は受信する**。

```haskell
Dual (Send t s) = Recv t (Dual s)
Dual (Recv t s) = Send t (Dual s)
Dual End        = End
```

クライアントが `Send Int (Recv String End)` なら、
サーバーは `Recv Int (Send String End)` でなければならない。

#### 3. 選択と分岐

```haskell
-- クライアントが選択する
Select [l1: s1, l2: s2, ...]

-- サーバーが分岐を提供する
Offer [l1: s1, l2: s2, ...]
```

例: ATMでの操作選択

```haskell
type ATMChoice = Select [
  "withdraw": Send Amount (Recv Cash End),
  "deposit":  Send Cash (Recv Receipt End),
  "balance":  Recv Balance End
]
```

#### 4. 再帰（繰り返し）

```haskell
-- 繰り返しのあるプロトコル
type ChatProtocol = Rec x. Select [
  "send":  Send Message x,   -- メッセージ送信、続ける
  "quit":  End               -- 終了
]
```

### 型チェックの威力

#### プロトコル違反の検出

```haskell
-- ❌ コンパイルエラー: 順序違反
badClient :: ATMClient -> IO ()
badClient session = do
  send session (PIN "1234")  -- エラー! まずカード番号を送るべき

-- ❌ コンパイルエラー: 型の不一致
badClient2 :: ATMClient -> IO ()
badClient2 session = do
  send session "hello"  -- エラー! CardNumberを送るべき

-- ✅ 正しいクライアント
goodClient :: ATMClient -> IO ()
goodClient session = do
  session' <- send session (CardNumber "1234-5678")
  (pinReq, session'') <- recv session'
  session''' <- send session'' (PIN "0000")
  (balance, session'''') <- recv session'''
  session''''' <- send session'''' (Amount 10000)
  (cash, finalSession) <- recv session'''''
  close finalSession
```

#### デッドロックフリー

セッション型を適切に設計すると、**デッドロックが起きないことを証明**できる。

```haskell
-- 両方が同時にrecvするとデッドロック
-- Session Typesでは、一方がSendなら他方はRecv
-- よって構造的にデッドロックが起きない
```

### OS設計への応用

#### システムコールプロトコル

```haskell
-- ファイル読み取りのプロトコル
type FileReadProtocol =
  Send OpenRequest      -- オープン要求
  (Recv (Either Error FileHandle)  -- ハンドルまたはエラー
  (Rec x. Select [
    "read":  Send ReadRequest (Recv Bytes x),  -- 読み取り
    "close": Send CloseRequest End             -- クローズ
  ]))
```

#### IPC（プロセス間通信）

```haskell
-- プロセス間のメッセージパッシング
type WorkerProtocol = Rec x. Offer [
  "task":     Recv Task (Send Result x),  -- タスク処理
  "status":   Send Status x,               -- 状態報告
  "shutdown": End                          -- 終了
]

-- カーネルがワーカーに対して持つ型は Dual WorkerProtocol
type ManagerProtocol = Dual WorkerProtocol
```

#### デバイスドライバプロトコル

```haskell
-- ネットワークドライバのプロトコル
type NetworkDriverProtocol = Rec x. Offer [
  "send":    Recv Packet (Send Ack x),
  "recv":    Send Packet x,
  "config":  Recv Config (Send ConfigResult x),
  "reset":   Send ResetAck End
]
```

### Session Types + Linear Types の組み合わせ

- Session Types: プロトコルの**順序**を保証
- Linear Types: チャネルの**所有権**を保証

両方を組み合わせると：
- プロトコル違反なし（セッション型）
- リソースリークなし（線形型）
- デッドロックフリー（設計による）

---

## 5. まとめ: 4つの概念の関係

```
┌─────────────────────────────────────────────────────────────┐
│                        HSOS Type System                      │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Pure Functions                                              │
│  └── 基盤: 副作用のない計算、形式検証の土台                 │
│       │                                                      │
│       ▼                                                      │
│  Algebraic Effects                                           │
│  └── 副作用の構造化: OSサービスを効果として抽象化           │
│       │                                                      │
│       ▼                                                      │
│  Linear Types                                                │
│  └── リソース管理: ハードウェアリソースの安全な操作         │
│       │                                                      │
│       ▼                                                      │
│  Session Types                                               │
│  └── 通信プロトコル: IPC/システムコールの正確性             │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### 役割の整理

| 概念 | 何を保証するか | OS設計での役割 |
|------|---------------|---------------|
| Pure Functions | 副作用の分離、推論可能性 | カーネルロジックの検証可能性 |
| Algebraic Effects | 効果の合成可能性、解釈の差し替え | OSサービスの抽象化 |
| Linear Types | リソースの正確な使用回数 | メモリ、ハンドル、ロックの安全性 |
| Session Types | 通信プロトコルの遵守 | IPC、システムコール、ドライバ通信 |

### 統合的アプローチ

```
┌─────────────────────────────────────────────────────┐
│                    HSOS Architecture                 │
├─────────────────────────────────────────────────────┤
│  Algebraic Effects  → OSサービスの抽象化            │
│  Linear Types       → リソース管理の安全性          │
│  Session Types      → IPC/通信プロトコルの正確性    │
│  Pure Functions     → 形式検証の容易さ              │
├─────────────────────────────────────────────────────┤
│  Singularity的      → 型によるソフトウェア隔離      │
│  seL4的             → 証明可能な設計                │
└─────────────────────────────────────────────────────┘
```

---

## 参考文献

### Pure Functions
- [Real World Haskell](http://book.realworldhaskell.org/)

### Algebraic Effects
- [Koka Language](https://koka-lang.github.io/koka/doc/book.html)
- [Why Algebraic Effects? - Ante](https://antelang.org/blog/why_effects/)
- [Programming with Algebraic Effects and Handlers](https://www.researchgate.net/publication/221671686_Programming_with_Algebraic_Effects_and_Handlers)

### Linear Types
- [Linear Haskell - Tweag](https://www.tweag.io/blog/2017-03-13-linear-types/)
- [GHC Linear Types Proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst)
- [Retrofitting Linear Types (Microsoft Research)](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/03/haskell-linear-submitted.pdf)

### Session Types
- [sessions - Hackage](https://hackage.haskell.org/package/sessions)
- [Haskell Session Types with (Almost) No Class](https://users.cs.northwestern.edu/~jesse/pubs/haskell-session-types/)
- [Embedding Session Types in Haskell](https://homepages.inf.ed.ac.uk/slindley/papers/gvhs.pdf)

### 既存OS研究
- [Singularity - Microsoft Research](https://www.microsoft.com/en-us/research/project/singularity/)
- [seL4 Formal Verification](https://sel4.systems/)

---

*作成日: 2025-12-30*
*ステータス: 理論的基盤の整理完了*
