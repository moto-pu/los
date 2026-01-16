---
marp: true
theme: default
paginate: true
---

# LOS: Lambda Operating System

**λ計算を理論的基盤としたOS設計の探求**

λ Kansai LT

もと

---

# 自己紹介

- **年齢**: 0x33歳
- **所属**: 株式会社ポテンシャルユナイテッド
- **来歴**: TCP屋 → 組み込み屋 → Web屋
- **嗜好**: Haskell、PureScript が好き。数学わなびー。

**GitHub**:
- 個人: github.com/motoyak/
- 会社: github.com/moto-pu/

**近況**
- AIエージェント開発と静的型付け純粋型関数型言語は相性いい実感
- いまやCCがあるので、社内にHaskell物をちりばめても問題ない ←


---

# 今日話すこと

**「OSをλ計算から再設計したらどうなる？」**

- 思考実験 + 実現可能性調査のプロジェクト
- まだコードはない、設計フェーズ

---

# 問い

## 既存のOS概念は「自然」なのか？

- プロセス
- スレッド
- ファイル
- システムコール

---

# これらの起源

**1960〜70年代のハードウェア制約**から生まれた

- プロセス → メモリ保護 + CPU時分割
- スレッド → プロセス内の軽量実行単位
- ファイル → 永続化されたバイト列

**チューリングマシン的世界観**の産物

---

# 等価だが異なる世界観

```
チューリングマシン ≡ λ計算
     ↓                 ↓
  命令型             関数型
```

計算能力は同じ。でも**見方**が違う。

---

# 視点の違い

| 命令型 | λ計算的 |
|--------|---------|
| 状態遷移 | 関数適用 |
| mutation | 不変データ |
| 暗黙の副作用 | 明示的な効果 |

---

# λ計算的にOSを見ると

```haskell
OS = fold handleEvent initialState eventStream
```

OSは**イベントストリームの変換器**

- 割り込み、システムコール、タイマー → 入力
- 画面出力、ディスク書き込み → 出力

---

# 4つの理論的柱

1. **Pure Functions** → 検証可能性
2. **Algebraic Effects** → OSサービスの抽象化
3. **Linear Types** → リソース管理
4. **Session Types** → プロトコル検証

---

# Algebraic Effects でシステムコール

```haskell
effect Syscall where
  read  : FileDescriptor -> Bytes
  write : FileDescriptor -> Bytes -> ()
  fork  : () -> ProcessId

-- カーネル = 効果ハンドラの集合
runKernel :: Syscall a -> IO a
```

同じプログラム、ハンドラ差し替えでテスト可能

---

# Linear Types でリソース管理

```haskell
close :: Handle ⊸ IO ()
--              ^ 線形矢印: ちょうど1回使う

-- コンパイルエラー！
bad h = do
  close h
  read h   -- use-after-close
```

---

# Session Types でIPC

```haskell
type Protocol =
  Send Request
  (Recv Response
  End)

-- プロトコル違反はコンパイルエラー
```

---

# 先行研究との位置づけ

| プロジェクト | アプローチ |
|-------------|----------|
| Singularity | 型安全（Sing#） |
| seL4 | 形式検証（Isabelle/HOL） |
| **LOS** | 型安全 + 証明可能 + 効果 |

---

# seL4の興味深い事実

seL4は**Haskellで書かれた実行可能仕様**を持つ

```haskell
handleSyscall :: Syscall -> KernelState -> (Result, KernelState)
```

これを「仕様」ではなく「実装」にできないか？

---

# 現状と今後

**現状**: 設計・調査フェーズ
- 実現可能性: ✅ (House, HaLVM で実証済み)
- 理論的基盤: ✅ 整理完了

**次のステップ**:
- コア概念のプロトタイプ
- 効果システムの実装選定

---

# まとめ

**LOS = λ計算からOSを再設計する試み**

- 既存概念を翻訳するのではなく、根本から再考
- Algebraic Effects, Linear Types, Session Types
- 型システムで安全性を保証

**リポジトリ**: github.com/moto-pu/los

---

# 議論したいこと

- 効果システム: polysemy? effectful? 自作?
- GC問題へのアプローチ
- 「OSらしさ」をどこまで残すか

ご意見・アイデア歓迎！
