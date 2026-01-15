---
marp: true
theme: default
paginate: true
---

# λ計算を起点としたOS設計

なぜλ計算ベースなのか？

---

# 二つの等価な計算モデル

```
チューリングマシン ≡ λ計算（Church-Turing Thesis）
     ↓                    ↓
  命令型言語           関数型言語
   (C, Rust)          (Haskell, ML)
```

**計算能力は等価**
しかし「世界の見方」が根本的に異なる

---

# 世界観の違い

| 観点 | チューリングマシン的 | λ計算的 |
|------|---------------------|---------|
| 中心概念 | 状態遷移 | 関数適用 |
| データ | 変更可能 | 不変 |
| 副作用 | 暗黙的 | 明示的 |
| 合成 | 手続きの連結 | 関数合成 |
| 実行 | 逐次的な命令実行 | 式の評価・簡約 |

---

# 問い: 従来のOS概念は「自然」か？

現在のOS概念は**1960〜70年代のハードウェア制約**から生まれた

| 概念 | 起源 |
|------|------|
| プロセス | メモリ保護＋CPUの時分割 |
| スレッド | プロセス内の軽量実行単位 |
| ファイル | 永続化されたバイト列 |
| システムコール | ユーザー/カーネル境界の越境 |

---

# これらは本質的か？

λ計算的に見ると、**必ずしも自然な抽象ではない**

再検討の余地がある

---

# OSの本質的定義（再考）

```
OS = リソースを計算に割り当て、
     計算の合成を可能にするシステム
```

「プロセス管理」「メモリ管理」という機能分割ではなく
**「計算」と「リソース」と「効果」**を中心に据える

---

# 視点の転換: 状態 → 変換

## 命令型の視点
```
state₁ → mutation → state₂ → mutation → state₃
```

## λ計算的視点
```haskell
transform :: State → State
OS = fold transform initialState events
```

---

# OSを「イベントストリームの変換器」として捉える

```
割り込み、システムコール、タイマーイベント...
    ↓
「入力ストリーム」
    ↓
OSはその「変換器」
```

FRP (Functional Reactive Programming) のOS版

---

# メリット1: 参照透過性

```haskell
-- 同じ入力には常に同じ出力
schedule :: ProcessList → CPU → (Process, ProcessList)
```

---

# 参照透過性の恩恵

| メリット | 説明 |
|---------|------|
| 再現性 | 同じ初期状態とイベント列なら同じ結果 |
| テスタビリティ | カーネルロジックを純粋関数でテスト |
| 形式検証 | 数学的証明が適用可能 |
| デバッグ容易性 | 状態の追跡が容易 |

---

# メリット2: 合成可能性

λ計算の核心: **関数は合成できる**

```haskell
kernelLogic = memoryManager . scheduler . interruptHandler
```

- OSのサブシステムを「独立した変換関数」として設計
- モノリシックカーネルの複雑さを分解可能に
- 各コンポーネントを個別にテスト・検証可能

---

# メリット3: 副作用の明示的制御

```haskell
IO a ≠ a  -- 型レベルで区別
```

- **純粋な計算**: 数学的世界
- **効果を持つ計算**: 物理的世界との接点

型シグネチャを見れば副作用の有無がわかる

---

# 新パラダイムの方向性

1. Computation as First-Class Citizen
2. Effect System as Capability System
3. Linear Types for Resource Management
4. Reactive Streams as System Model
5. Persistent Data Structures for State

---

# 1. Computation as First-Class Citizen

「プロセス」ではなく「計算」を第一級市民に

```haskell
-- 計算は値として扱える
type Computation a = Resources → (a, Resources)

-- 計算の合成
(>>=) :: Computation a → (a → Computation b) → Computation b
```

「プロセスを起動する」→「計算を評価する」

---

# 2. Effect System as Capability System

## 従来のケーパビリティ
```
プロセスが持つ「権限トークン」のリスト（実行時チェック）
```

## λ計算的ケーパビリティ
```haskell
readFile  :: HasCapability FileRead  m => FilePath → m ByteString
writeNet  :: HasCapability NetWrite  m => Socket → ByteString → m ()
```

**コンパイル時に権限違反を検出**

---

# 3. Linear Types for Resource Management

リソースの「使い捨て」性を型で保証

```haskell
-- ファイルハンドルは一度しか閉じられない
close :: FileHandle ⊸ IO ()

-- メモリは明示的に解放
free :: Ptr a ⊸ IO ()
```

**Use-after-free、Double-freeがコンパイルエラー**

---

# 4. Reactive Streams as System Model

```
          ┌─────────────────────────┐
 Events   │                         │  Effects
─────────→│   OS = Stream → Stream  │─────────→
          │                         │
          └─────────────────────────┘

Events:  割り込み、システムコール、タイマー...
Effects: 画面出力、ディスク書き込み、ネットワーク送信...
```

---

# 5. Persistent Data Structures

「状態の変更」ではなく「新しいバージョンの作成」

```haskell
type FileSystem = PersistentTree Inode

writeFile :: FileSystem → Path → Data → FileSystem
```

- タイムトラベル: 任意の過去状態に戻れる
- 並行安全: 古いバージョンを読みながら新バージョンを書ける
- スナップショット: コストほぼゼロ

---

# 従来概念の再解釈

| 従来の概念 | λ計算的再解釈 |
|-----------|--------------|
| プロセス | 独立した計算 |
| スレッド | 並行な計算の合成 |
| ファイル | 永続化された値へのアクセス経路 |
| システムコール | 効果の発行 |
| 割り込み | イベントストリームへの入力 |
| スケジューリング | 計算の評価戦略の選択 |

---

# 根本的な問い

## 考えるべきこと

- 「時間」をどう扱うか？
- 「並行」と「並列」の区別は？
- 「障害」をどう表現するか？
- 「境界」はどこに引くか？

---

# なぜ既存OSはλ計算ベースでないのか？

| 理由 | 説明 |
|------|------|
| 歴史的経緯 | Cが先に普及 |
| ハードウェア親和性 | CPUは命令型マシン |
| パフォーマンス | 抽象化のコスト（当時は致命的） |
| GCの問題 | リアルタイム性との相性 |

---

# 2025年現在の状況

これらの障壁は低くなっている

- 型理論・プログラミング言語研究の進歩
- ハードウェア性能の向上
- GHCの最適化技術の進歩
- Linear Haskellの登場
- 形式検証への関心の高まり

---

# 結論

λ計算を起点としてOSを設計することで得られるもの

1. **理論的明晰さ**: 形式的意味論が明確
2. **型による保証**: 効果、リソース、プロトコルを型で表現
3. **合成可能性**: 独立したコンポーネントの自然な合成
4. **検証可能性**: 純粋関数は証明が容易
5. **新しい抽象**: 従来とは異なるOS概念の可能性
