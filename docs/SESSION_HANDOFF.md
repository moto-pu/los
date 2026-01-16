# セッション引き継ぎドキュメント

*作成日: 2026-01-17*

## プロジェクト概要

**LOS (Lambda Operating System)** - λ計算を理論的基盤とした新しいOS設計パラダイムを探求するプロジェクト。

## 現在のステータス

- **フェーズ**: 初期設計・概念検討（コードはまだない）
- **リポジトリ**: github.com/moto-pu/los (private)

## 作成済み成果物

### ドキュメント
- `docs/design/00-philosophy.md` - 設計哲学
- `docs/design/01-lambda-calculus-paradigm.md` - λ計算パラダイム
- `docs/research/00-feasibility-analysis.md` - Haskell OS実現可能性
- `docs/research/01-theoretical-foundations.md` - 4つの理論的柱の詳細
- `docs/research/02-existing-systems-analysis.md` - 既存システム分析

### スライド（Marp形式）
- `docs/slide/00-los-overview.md` - プロジェクト概要
- `docs/slide/01-feasibility.md` - 実現可能性分析
- `docs/slide/02-lambda-paradigm.md` - λ計算パラダイム
- `docs/slide/03-theoretical-foundations.md` - 理論的基盤
- `docs/slide/04-existing-systems.md` - 既存システム
- `docs/slide/lt-lambda-kansai.md` - **λ Kansai LT用（10分）**
- `docs/slide/lt-lambda-kansai.pptx` - PowerPoint版

### ツール
- `bin/marp2pptx` - Marp MD → PPTX変換スクリプト

## 4つの理論的柱

1. **Pure Functions** - 参照透過性、検証可能性
2. **Algebraic Effects** - 効果の抽象化、ハンドラによるテスト容易性
3. **Linear Types** - リソースの線形使用、use-after-free防止
4. **Session Types** - 通信プロトコルの型安全性

## 直近の作業

- λ Kansai LT用スライド作成（自己紹介スライド含む）
- Marp → PPTX変換スクリプト作成

## 次のステップ候補

- λ Kansai LTの発表準備
- コア概念のプロトタイプ実装
- 効果システムの実装選定（polysemy? effectful? 自作?）

## 新PCでの開始方法

```bash
git clone git@github.com:moto-pu/los.git
cd los
# Claude Codeを起動し、このファイルを読ませる
```
