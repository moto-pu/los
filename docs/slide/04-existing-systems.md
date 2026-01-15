---
marp: true
theme: default
paginate: true
---

# 既存システム・研究の分析

型安全性・形式検証を重視したOS設計の先行研究

---

# 分析対象

1. **Singularity OS** (Microsoft Research)
2. **seL4** (Trustworthy Systems)
3. **代数的効果の実装言語** (Koka, Eff等)

---

<!-- _class: lead -->

# Singularity OS
## Microsoft Research (2003-2015)

---

# Singularity OS: 概要

| 項目 | 内容 |
|------|------|
| 開発元 | Microsoft Research |
| 期間 | 2003年7月〜2015年2月 |
| 言語 | Sing#（C#の拡張） |
| 目的 | 高信頼性OSの実験的研究 |

---

# Singularity: コアアイデア

**ハードウェア保護なしで、型安全性によるプロセス隔離を実現**

---

# 従来のOS vs Singularity

```
従来のOS:
┌─────────────┐ ┌─────────────┐
│  Process A  │ │  Process B  │
├─────────────┴─┴─────────────┤  ← MMU（ハードウェア境界）
│         Kernel              │
└─────────────────────────────┘

Singularity:
┌─────────────┐ ┌─────────────┐
│    SIP A    │ │    SIP B    │
├─────────────┴─┴─────────────┤  ← 型安全性（論理境界）
│     Kernel (Managed)        │
└─────────────────────────────┘
```

---

# SIPs (Software Isolated Processes)

型安全な言語による隔離

- 分離されたオブジェクト空間
- 個別のGC
- 個別のランタイム
- ハードウェア保護のオーバーヘッドなし

---

# 契約ベースのチャネル

プロセス間通信を**契約（Contract）**で定義

```csharp
contract FileContract {
    in message Open(string path);
    out message OpenResult(FileHandle handle);
    in message Read(int count);
    out message ReadResult(byte[] data);
    in message Close();
}
```

---

# マニフェストベースのプログラム

プログラムの性質を**マニフェスト**で宣言

- 必要なABIバージョン
- 必要なIPCインターフェース
- 依存する他のプロセス
- リソース要件

カーネルが起動前に検証

---

# Singularity: 検証方法

| 種類 | 詳細 |
|------|------|
| 検証済みコード | コンパイラが型・メモリ安全性をチェック |
| 信頼済みコード | HAL、カーネル、ランタイムの一部 |

---

# Singularity: 成果

- ✅ ハードウェア保護なしでプロセス隔離を実現
- ✅ デバイスドライバも同一言語で記述
- ✅ チャネル契約による型安全なIPC
- ✅ 起動時のマニフェスト検証

---

# Singularity: 限界

- ❌ 商用化には至らず
- ❌ **形式検証**はしていない（型安全のみ）
- ❌ 既存ソフトウェアとの互換性なし

---

# 後継: Midori

SingularityはMidoriプロジェクトへ発展

一時期、Microsoftの自然言語検索サービスで稼働していた

---

# Singularityからの学び

**学び:**
- 型安全性だけでソフトウェア隔離が可能
- チャネル契約 ≒ Session Types
- マニフェスト検証 ≒ 静的解析

**課題:**
- 「型安全だけでは十分でない」場合がある

---

<!-- _class: lead -->

# seL4
## Trustworthy Systems (2009-)

---

# seL4: 概要

| 項目 | 内容 |
|------|------|
| 開発元 | NICTA / Data61 |
| 初版 | 2009年 |
| 言語 | C（実装）、Isabelle/HOL（証明） |
| 目的 | 形式検証されたマイクロカーネル |

---

# seL4: 世界初の成果

**完全な機能的正確性証明を持つ汎用OSカーネル**

---

# 形式検証の範囲

```
┌─────────────────────────────────────────────┐
│           Abstract Specification            │
├─────────────────────────────────────────────┤
│         Executable Specification            │  (Haskell)
├─────────────────────────────────────────────┤
│              C Implementation               │
├─────────────────────────────────────────────┤
│              Binary Code                    │
└─────────────────────────────────────────────┘
        ↑↓ 各層間で Refinement Proof
```

---

# 証明される性質

| 性質 | 説明 |
|------|------|
| 機能的正確性 | 実装が抽象仕様に厳密に従う |
| 整合性 | 許可されていないメモリアクセスなし |
| 機密性 | 情報漏洩なし |
| 情報フロー非干渉 | セキュリティドメイン間の干渉なし |

---

# 機能的正確性とは

実装が抽象仕様に**厳密に**従う

- カーネルはクラッシュしない
- 不安全な操作を行わない
- 挙動がすべての状況で予測可能

---

# 証明手法: Refinement Proof

高レベル（抽象）と低レベル（具体）の対応を証明

```
Abstract Spec のすべての Hoare Logic 性質
    ↓ 保存される
Concrete Implementation
```

---

# 証明規模

- 約8,700行のC実装
- 約200,000行のIsabelle/HOL証明
- **実装の約20倍以上の証明コード**

---

# 実行可能仕様

seL4の興味深い点: **Haskellで書かれた実行可能仕様**

```haskell
handleSyscall :: Syscall -> KernelState -> (Result, KernelState)
handleSyscall (Send ep msg) state = ...
handleSyscall (Recv ep) state = ...
```

実行してテスト可能 & 証明の基礎

---

# seL4: 成果

- ✅ 世界初の完全な機能的正確性証明
- ✅ バイナリレベルまでの証明
- ✅ 実用的なマイクロカーネル
- ✅ ARM, RISC-V, x86対応

---

# seL4: 限界

- ❌ 証明コストが膨大（実装の20倍以上）
- ❌ 実装言語（C）と証明言語（Isabelle）の分離
- ❌ 仕様変更時の証明更新コスト

---

# seL4からの学び

**「証明可能 ≠ 証明しやすい」**

- Cコードの事後検証は非常に高コスト
- **Haskellの実行可能仕様**は非常に有用
- 最初から証明を意識した言語設計が重要

---

# seL4とLOSの関係

HaskellでLOSを書く = seL4の「実行可能仕様」に相当

- 純粋関数ベース → 証明が容易
- 証明ツール（LiquidHaskell等）との連携可能

---

<!-- _class: lead -->

# 代数的効果の実装言語

---

# Koka

| 項目 | 内容 |
|------|------|
| 開発元 | Microsoft Research |
| 最新版 | v3.2.0 (2025-07) |
| 特徴 | 代数的効果をコア言語機能として持つ |

---

# Kokaの特徴

- **Evidence Passing**: 効果をランタイムなしでCにコンパイル
- Tail-resumptive効果の最適化
- 高いパフォーマンス

---

# Kokaの最近の研究 (2024-2025)

- "First-order Laziness" (ICFP'25)
- "The Functional Essence of Binary Search Trees" (PLDI'24)

活発な研究開発が続いている

---

# その他の代数的効果言語

| 言語 | 特徴 |
|------|------|
| Eff | 研究用、最も純粋な実装 |
| OCaml 5.x | 主流言語への効果導入 |
| Ante | 効果を重視した新興言語 |

---

# Haskellでの効果システム

| ライブラリ | 特徴 |
|-----------|------|
| polysemy | 使いやすさ重視 |
| fused-effects | 柔軟性重視 |
| effectful | パフォーマンス重視 |

---

<!-- _class: lead -->

# まとめと比較

---

# 比較表

| システム | アプローチ | 保証 | 言語 |
|---------|----------|------|------|
| Singularity | 型安全 | メモリ安全、プロトコル遵守 | Sing# |
| seL4 | 形式検証 | 機能的正確性、情報フロー | C + Isabelle |
| Koka | 代数的効果 | 効果の追跡・合成 | Koka |

---

# LOSの位置づけ

```
                    証明の厳密さ
                         ↑
          seL4 ●         │
                         │        ● LOS (目標)
       Singularity ●     │
                         │
─────────────────────────┼─────────→ 型システムの表現力
              Koka ●     │
```

---

# LOSが目指すもの

1. **Singularityの型安全性** + **seL4の証明可能性**
2. **Kokaの代数的効果** + **Haskellのエコシステム**
3. 純粋関数ベースで**実行可能仕様 = 実装**

---

# 各システムからの学び

| システム | 学び |
|---------|------|
| Singularity | 型でソフトウェア隔離が可能 |
| seL4 | 形式検証は可能だがコストが高い |
| Koka | 代数的効果は実用的 |

---

# LOSの設計方針

- **最初から証明を意識**した言語選択（Haskell）
- **代数的効果**でOSサービスを抽象化
- **Linear Types**でリソース管理
- **Session Types**でプロトコル検証
- **純粋関数**ベースで検証可能性を確保
