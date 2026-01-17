# ADR-00: Effect System の選定

## ステータス

**決定済み** - effectful を採用

## 日付

2026-01-18

---

## コンテキスト

LOSの設計思想では「カーネル = 効果ハンドラの集合体」と定義している。この概念を実装するにあたり、以下の選択肢を検討した：

1. **Monad Transformer (mtl)** - 伝統的なアプローチ
2. **Effect System** - 代数的効果の実装
   - polysemy
   - effectful
   - fused-effects

## 検討した選択肢

### 1. Monad Transformer (mtl)

**メリット**:
- 30年以上の歴史、成熟した技術
- GHCの最適化が効きやすい（newtypeは消去される）
- 資料・事例が豊富
- デバッグしやすい

**デメリット**:
- lift地獄（N層でO(N)のlift）
- 効果の順序が意味を持つ場合がある
- ハンドラの差し替えが困難（テストしにくい）
- 「効果」という概念との対応が間接的

**コード例**:
```haskell
type App a = StateT AppState (ReaderT Config (ExceptT AppError IO)) a

example :: App ()
example = do
  config <- lift ask                    -- 1段lift
  state  <- get
  lift $ lift $ throwError SomeError    -- 2段lift
```

### 2. polysemy

**メリット**:
- lift不要、直交的な効果合成
- ハンドラの差し替えが容易
- 代数的効果の概念を直接表現

**デメリット**:
- **パフォーマンスが著しく悪い**（MTの20-30倍遅い）
- Free monadベースで中間構造を大量生成
- GC負荷が高い

**OS用途では非推奨**。

### 3. effectful

**メリット**:
- lift不要、polysemy同様のAPI
- **パフォーマンスがMTに近い**（1.1倍程度）
- ReaderT IOベースで最適化が効く
- ハンドラの差し替えが容易（テスト向き）
- 代数的効果の概念を直接表現

**デメリット**:
- polysemyより新しく、エコシステムがやや小さい
- MTほどの最適化は期待できない（微差）

**コード例**:
```haskell
example :: (State AppState :> es, Reader Config :> es, Error AppError :> es) => Eff es ()
example = do
  config <- ask        -- liftなし
  state  <- get
  throwError SomeError -- liftなし
```

### 4. fused-effects

**メリット**:
- Church-encodedで中間構造を削減
- polysemyより高速

**デメリット**:
- effectfulより遅い（2倍程度）
- APIがやや複雑

## GC観点での分析

LOSの技術的課題として「GC問題」が挙げられている（docs/design/00-philosophy.md）。リアルタイム性に影響するため、GC負荷の観点は重要。

| 実装 | 割り当て特性 | GC負荷 |
|------|-------------|--------|
| Monad Transformer | newtypeは消去される | **低** |
| polysemy | Free monadベース、中間構造を大量生成 | **高** |
| effectful | ReaderT IOベース | **低〜中** |
| fused-effects | Church-encoded | **中** |

### OS文脈での影響

| シナリオ | 影響度 | 備考 |
|----------|--------|------|
| スケジューラのホットパス | 致命的 | 頻繁に呼ばれる |
| システムコールハンドリング | 高 | レイテンシに直結 |
| 初期化・設定読み込み | 低 | 一度だけ |

## 決定

**effectful を採用する**

### 理由

1. **設計思想との整合性**: 「カーネル = 効果ハンドラの集合体」という概念を直接表現できる
2. **GC負荷**: polysemyと異なり、MTに近いパフォーマンス特性を持つ
3. **テスタビリティ**: ハンドラの差し替えでモック実装が容易
4. **開発体験**: lift不要で、効果の合成が直交的
5. **許容可能なオーバーヘッド**: MTの1.1倍程度であれば、GC自体を完全に排除できない以上、許容範囲

### 補足

GC負荷を完全に排除することは不可能であり、effectful程度のオーバーヘッドは許容することが妥当と判断した。将来的にホットパスで問題が発生した場合は、その部分のみを低レベルな実装に置き換えることを検討する。

## 影響

- GHC 9.0以上が必要（effectfulの要件）
- Linear Typesとの組み合わせも GHC 9.0+ で可能
- プロジェクトのcabal設定でeffectfulを依存関係に追加

## 参考文献

- [effectful - Hackage](https://hackage.haskell.org/package/effectful)
- [effectful vs polysemy benchmark](https://github.com/haskell-effectful/effectful#comparison-with-other-effect-libraries)
- [Why Algebraic Effects? - Ante](https://antelang.org/blog/why_effects/)
- docs/research/01-theoretical-foundations.md（Algebraic Effectsセクション）

---

*作成日: 2026-01-18*
