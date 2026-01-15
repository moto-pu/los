# 既存システム・研究の分析

型安全性や形式検証を重視したOS設計の既存研究を分析し、LOSへの示唆を導く。

---

## 1. Singularity OS

### 概要

| 項目 | 内容 |
|------|------|
| 開発元 | Microsoft Research |
| 期間 | 2003年7月〜2015年2月 |
| 言語 | Sing#（C#の拡張） |
| 目的 | 高信頼性OSの実験的研究 |
| URL | https://www.microsoft.com/en-us/research/project/singularity/ |

### アーキテクチャ的特徴

#### Software Isolated Processes (SIPs)

**概念**: ハードウェア保護なしで、**型安全性によるプロセス隔離**を実現

```
従来のOS:
┌─────────────┐ ┌─────────────┐
│  Process A  │ │  Process B  │
│  (User)     │ │  (User)     │
├─────────────┴─┴─────────────┤  ← ハードウェア境界（MMU）
│         Kernel              │
└─────────────────────────────┘

Singularity:
┌─────────────┐ ┌─────────────┐
│    SIP A    │ │    SIP B    │
│  (Managed)  │ │  (Managed)  │
├─────────────┴─┴─────────────┤  ← 型安全性による論理境界
│     Kernel (Managed)        │
└─────────────────────────────┘
```

**特徴**:
- 分離されたオブジェクト空間
- 個別のGC
- 個別のランタイム
- ハードウェア保護ドメインのオーバーヘッドなし

#### 契約ベースのチャネル

**概念**: プロセス間通信を**契約（Contract）**で定義

```csharp
// チャネル契約の定義
contract FileContract {
    in message Open(string path);
    out message OpenResult(FileHandle handle);
    in message Read(int count);
    out message ReadResult(byte[] data);
    in message Close();
}
```

型システムがチャネルの使用を静的に検証。

#### マニフェストベースのプログラム

**概念**: プログラムの性質を**マニフェスト**で宣言し、カーネルが起動前に検証

マニフェストに含まれる情報:
- 必要なABIバージョン
- 必要なIPCインターフェース
- 依存する他のプロセス
- リソース要件

### 検証方法

| 種類 | 詳細 |
|------|------|
| 検証済みコード | コンパイラが型・メモリ安全性をチェック |
| 信頼済みコード | HAL、カーネル、ランタイムの一部（検証不可） |

### 成果と限界

**成果**:
- ✅ ハードウェア保護なしでプロセス隔離を実現
- ✅ デバイスドライバも同一言語で記述
- ✅ チャネル契約による型安全なIPC
- ✅ 起動時のマニフェスト検証

**限界**:
- ❌ 商用化には至らず
- ❌ **形式検証**はしていない（型安全のみ）
- ❌ 既存ソフトウェアとの互換性なし

### 後継: Midori

SingularityはMidoriプロジェクトへ発展。一時期、Microsoftの自然言語検索サービス（西海岸・アジア）で稼働していた。

### LOSへの示唆

**学び**:
- 型安全性だけでソフトウェア隔離が可能
- チャネル契約 ≒ Session Types
- マニフェスト検証 ≒ 静的解析

**課題**:
- 「型安全だけでは十分でない」場合がある
- LOSでは、型安全＋**形式検証可能性**を両立させたい

---

## 2. seL4

### 概要

| 項目 | 内容 |
|------|------|
| 開発元 | NICTA / Data61 / Trustworthy Systems |
| 初版 | 2009年 |
| 言語 | C（実装）、Isabelle/HOL（証明） |
| 目的 | 形式検証されたマイクロカーネル |
| URL | https://sel4.systems/ |

### 形式検証の範囲

```
┌─────────────────────────────────────────────┐
│           Abstract Specification            │  ← 抽象仕様
├─────────────────────────────────────────────┤
│         Executable Specification            │  ← 実行可能仕様（Haskell）
├─────────────────────────────────────────────┤
│              C Implementation               │  ← C実装
├─────────────────────────────────────────────┤
│              Binary Code                    │  ← バイナリ
└─────────────────────────────────────────────┘
        ↑↓ 各層間で Refinement Proof
```

### 証明される性質

| 性質 | 説明 |
|------|------|
| **機能的正確性** | 実装が抽象仕様に厳密に従う |
| **整合性（Integrity）** | 許可されていないメモリアクセスなし |
| **機密性（Confidentiality）** | 情報漏洩なし |
| **情報フロー非干渉** | 異なるセキュリティドメイン間の干渉なし |

### 証明手法

**Refinement Proof（詳細化証明）**:

高レベル（抽象）と低レベル（具体）の対応を証明。

```
Abstract Spec のすべての Hoare Logic 性質
    ↓ 保存される
Concrete Implementation
```

**証明規模**:
- 約8,700行のC実装
- 約200,000行のIsabelle/HOL証明
- **実装の約20倍以上の証明コード**

### 実行可能仕様

seL4の興味深い点：**Haskellで書かれた実行可能仕様**が存在。

```haskell
-- seL4の実行可能仕様（簡略化）
handleSyscall :: Syscall -> KernelState -> (Result, KernelState)
handleSyscall (Send ep msg) state = ...
handleSyscall (Recv ep) state = ...
```

この仕様を**実行してテスト**でき、かつ**証明の基礎**にもなる。

### 成果と限界

**成果**:
- ✅ 世界初の完全な機能的正確性証明
- ✅ バイナリレベルまでの証明
- ✅ 実用的なマイクロカーネル（実プロジェクトで使用）
- ✅ ARM、RISC-V、x86対応

**限界**:
- ❌ 証明コストが膨大（実装の20倍以上）
- ❌ 実装言語（C）と証明言語（Isabelle）の分離
- ❌ 仕様変更時の証明更新コスト

### LOSへの示唆

**学び**:
- 「証明可能 ≠ 証明しやすい」
- Cコードの事後検証は非常に高コスト
- **Haskellの実行可能仕様**は非常に有用
- 最初から証明を意識した言語設計が重要

**LOSでのアプローチ**:
- Haskellで書く = seL4の「実行可能仕様」に相当
- 純粋関数ベース → 証明が容易
- 証明ツール（LiquidHaskell等）との連携

---

## 3. 代数的効果の実装言語

### 3.1 Koka

| 項目 | 内容 |
|------|------|
| 開発元 | Microsoft Research（Daan Leijen） |
| 最新版 | v3.2.0 (2025-07) |
| URL | https://koka-lang.github.io/koka/doc/book.html |

**特徴**:
- 代数的効果をコア言語機能として持つ
- **Evidence Passing**: 効果をランタイムなしでCにコンパイル
- Tail-resumptive効果の最適化

**最近の研究** (2024-2025):
- "First-order Laziness" (ICFP'25)
- "The Functional Essence of Binary Search Trees" (PLDI'24)

**LOSへの示唆**:
- カーネルレベルでも代数的効果が使える可能性
- Cへのコンパイル技術は参考になる

### 3.2 Eff

| 項目 | 内容 |
|------|------|
| 目的 | 代数的効果の研究用言語 |
| URL | https://www.eff-lang.org/ |

**特徴**:
- 代数的効果・ハンドラの最も純粋な実装
- 研究用途に最適化

### 3.3 OCaml 5.x

| 項目 | 内容 |
|------|------|
| 変更点 | Effect handlersの導入 |
| 特徴 | 実用的な言語への効果導入 |

**意義**: 主流言語への代数的効果の浸透を示す。

### 3.4 Ante

| 項目 | 内容 |
|------|------|
| 目的 | 効果を重視した新興言語 |
| URL | https://antelang.org/ |

**予測** (2025年ブログより):
> 代数的効果は「明日のプログラミング言語で人気が急上昇する」非常に有用な機能

---

## 4. Haskellでの効果システム

### 4.1 polysemy

```haskell
-- 効果の定義
data FileSystem m a where
  ReadFile  :: FilePath -> FileSystem m ByteString
  WriteFile :: FilePath -> ByteString -> FileSystem m ()

-- 効果を使うプログラム
program :: Member FileSystem r => Sem r ()
program = do
  content <- readFile "input.txt"
  writeFile "output.txt" content

-- ハンドラ（本番用）
runFileSystemIO :: Sem (FileSystem ': r) a -> Sem r a

-- ハンドラ（テスト用）
runFileSystemPure :: Map FilePath ByteString
                  -> Sem (FileSystem ': r) a
                  -> Sem r a
```

### 4.2 fused-effects

```haskell
-- 効果の定義
data State s m k
  = Get (s -> m k)
  | Put s (m k)

-- Carrier（ハンドラ）
newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }
```

### 4.3 effectful

- 最近の高性能効果ライブラリ
- GHCの最適化と親和性が高い

### LOSへの示唆

Haskellでも代数的効果相当の機能は利用可能。
ライブラリ選定が重要（パフォーマンス、使いやすさのトレードオフ）。

---

## 5. 分析のまとめ

### 比較表

| システム | アプローチ | 保証 | 言語 |
|---------|----------|------|------|
| Singularity | 型安全 | メモリ安全、プロトコル遵守 | Sing# |
| seL4 | 形式検証 | 機能的正確性、情報フロー | C + Isabelle |
| Koka | 代数的効果 | 効果の追跡・合成 | Koka |
| LOS (目標) | 型安全 + 証明可能 | 上記すべて | Haskell |

### LOSの位置づけ

```
                    証明の厳密さ
                         ↑
                         │
          seL4 ●         │
                         │
                         │        ● LOS (目標)
                         │
       Singularity ●     │
                         │
─────────────────────────┼─────────────────→ 型システムの表現力
                         │
              Koka ●     │
                         │
```

### LOSが目指すもの

1. **Singularityの型安全性** + **seL4の証明可能性**
2. **Kokaの代数的効果** + **Haskellの成熟したエコシステム**
3. 純粋関数ベースで**実行可能仕様 = 実装**

---

## 参考文献

### Singularity
- [Singularity - Microsoft Research](https://www.microsoft.com/en-us/research/project/singularity/)
- [Singularity: Rethinking the Software Stack (SIGOPS '07)](https://dl.acm.org/doi/10.1145/1243418.1243424)
- [An Overview of the Singularity Project (TR-2005-135)](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/10/tr-2005-135.pdf)

### seL4
- [seL4 Official Site](https://sel4.systems/)
- [seL4: Formal Verification of an OS Kernel (SOSP '09)](https://www.sigops.org/s/conferences/sosp/2009/papers/klein-sosp09.pdf)
- [Comprehensive Formal Verification of an OS Microkernel (TOCS '14)](https://sel4.systems/Research/pdfs/comprehensive-formal-verification-os-microkernel.pdf)
- [L4.verified GitHub](https://github.com/seL4/l4v)

### 代数的効果
- [Koka Language](https://koka-lang.github.io/koka/doc/book.html)
- [Why Algebraic Effects? - Ante](https://antelang.org/blog/why_effects/)
- [Programming with Algebraic Effects and Handlers](https://www.researchgate.net/publication/221671686_Programming_with_Algebraic_Effects_and_Handlers)

### Haskell効果ライブラリ
- [polysemy](https://hackage.haskell.org/package/polysemy)
- [fused-effects](https://hackage.haskell.org/package/fused-effects)
- [effectful](https://hackage.haskell.org/package/effectful)

---

*作成日: 2025-12-30*
*ステータス: 既存研究の分析完了*
