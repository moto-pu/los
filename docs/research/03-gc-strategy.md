# GC戦略の研究ノート

## ステータス

**検討中** - 将来の研究テーマ

## 日付

2026-01-18

---

## 背景

LOSの設計において、GC（ガベージコレクション）は避けられない技術的課題である（docs/design/00-philosophy.md セクション5.1）。

従来のアプローチ:
- リアルタイムGCの導入
- 特定領域でのマニュアルメモリ管理
- リージョンベースのメモリ管理

本ドキュメントでは、**OS側からGCを制御する**という新しいアプローチを検討する。

---

## 問題の本質

### 従来のGCの問題

```
アプリケーション実行中に GC pause が発生
  → 全スレッドが停止（stop-the-world）
  → レイテンシが予測不能
  → リアルタイム性が損なわれる
```

### OSとしての特殊性

通常のアプリケーション:
- OSのスケジューリングに任せる
- GCのタイミングは言語ランタイム任せ

LOSの場合:
- **OS自身がGCを制御できる立場にある**
- スケジューリング、コア割り当て、メモリ配置を統合的に設計可能

---

## 提案: GC専用コア割り当て

### コンセプト

特定のCPUコア（複数も可）をGC専用に割り当て、GCを継続的に実行させる。

```
┌─────────────────────────────────────────────────┐
│  従来: 全コアがアプリ実行、GC時は全停止        │
│                                                  │
│  [Core0] [Core1] [Core2] [Core3]                │
│    App    App     App     App                   │
│    ↓      ↓       ↓       ↓                     │
│   STOP   STOP    STOP    STOP  ← GC pause      │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│  提案: GC専用コアを分離                         │
│                                                  │
│  [Core0] [Core1] [Core2] [Core3]                │
│    App    App     App     GC専用               │
│    ↓      ↓       ↓       ↓                     │
│   継続   継続    継続    常時GC                │
└─────────────────────────────────────────────────┘
```

### 期待される効果

| 観点 | 効果 |
|------|------|
| **レイテンシ** | アプリコアがGC pauseで止まらない |
| **予測可能性** | GCタイミングがアプリの実行に影響しない |
| **リアルタイム性** | ホットパスの応答時間が安定 |
| **問題の局所化** | GC問題をメモリバス競合問題に縮小 |

---

## 残る課題

### 1. メモリバス競合

GCコアがヒープをスキャンする際、メモリバス帯域を消費する。

```
        メモリバス
           │
    ┌──────┼──────┐
    │      │      │
 [App]  [App]  [GC] ← GCがヒープをスキャン
    │      │      │    → バス帯域を消費
    └──────┴──────┘
```

**緩和策の候補**:
- NUMA構成でGCコアに近いメモリにヒープを配置
- GCのスキャン頻度を調整（帯域使用率の制御）
- 優先度の低いタイミングでGCを積極実行

### 2. キャッシュ汚染

GCコアがヒープをスキャンすると、共有キャッシュ（L3）に大量のヒープデータが流入し、アプリコアのキャッシュがevictされる可能性がある。

**緩和策の候補**:
- キャッシュパーティショニング（Intel CAT等）
- GCコアのキャッシュ使用を制限
- NUMA構成でキャッシュドメインを分離

### 3. ミューテータとGCの同期

Concurrent GCを実現するには、アプリケーション（ミューテータ）とGCスレッド間の同期が必要。

**必要な仕組み**:
- Write barrier: オブジェクト書き換え時にGCに通知
- Read barrier: 一部のGCアルゴリズムで必要
- Safe point: GCが一貫した状態を見られるポイント

**オーバーヘッド**:
- Write barrierは各書き込みにコストを追加
- ただしstop-the-worldよりは予測可能

---

## 既存技術の分析

### Concurrent/Pauseless GCの事例

| 技術 | アプローチ | pause時間 | 備考 |
|------|-----------|-----------|------|
| **Java ZGC** | Concurrent、colored pointers | < 1ms | JDK 15+で本番利用可 |
| **Java Shenandoah** | Concurrent、Brooks pointers | < 10ms | Red Hat主導 |
| **Azul C4** | Pauseless | ほぼ0 | 商用、専用ハードウェア支援 |
| **Go GC** | Concurrent、増分的 | < 1ms | Write barrier方式 |
| **GHC** | Stop-the-world、世代別 | 可変 | **Concurrent GCなし** |

### GHCの現状

```haskell
-- GHC RTS の GC は stop-the-world
-- -threaded で並列GCは可能だが、並行GCではない

-- 並列GC: 複数コアでGCを実行（アプリは停止）
-- 並行GC: アプリと同時にGCを実行（アプリは継続）
```

GHCに並行GCを導入する研究はあるが、プロダクションレベルには至っていない。

---

## LOSとしてのアプローチ

### 発想の転換

通常のアプローチ:
```
言語ランタイム → OS にスケジューリングを依頼
                  （GCのタイミングはランタイム任せ）
```

LOSのアプローチ:
```
LOS = 言語ランタイム + OS が一体
  → GCをOSの一部として設計
  → スケジューリングとGCの統合的制御
```

### OSレベルでできること

| 制御対象 | 具体的な施策 |
|----------|-------------|
| **コア割り当て** | GC専用コアの確保、アプリコアとの分離 |
| **メモリ配置** | NUMAを活用したヒープの配置最適化 |
| **スケジューリング** | GCの実行タイミングをOS側で制御 |
| **優先度制御** | アイドル時にGCを積極実行 |
| **キャッシュ制御** | ハードウェア機能を活用した分離 |

---

## 段階的実装計画

| フェーズ | 内容 | 難易度 | 依存関係 |
|----------|------|--------|----------|
| **Phase 1** | effectfulでPoC、GHC標準GCのまま | 低 | なし |
| **Phase 2** | GHC RTS tuning (`+RTS -A -n -qg` など) | 低 | Phase 1 |
| **Phase 3** | NUMA対応でヒープ分離実験 | 中 | Phase 2 |
| **Phase 4** | GHC RTSの改造（Concurrent GC実験） | 高 | Phase 3 |
| **Phase 5** | 独自ランタイム or GHC fork | 非常に高 | Phase 4 |

### Phase 1-2 での測定項目

- GC pause時間の分布
- GC頻度とヒープサイズの関係
- RTSオプションによる改善度

### Phase 3 での実験

```bash
# NUMA構成の確認
numactl --hardware

# GCスレッドを特定ノードにバインド
numactl --cpunodebind=1 --membind=1 ./los-kernel +RTS -qg -N4
```

---

## Write Barrier 詳説

Concurrent GCを実現するための核心技術。

### 問題: ミューテータとGCの競合

Concurrent GCでは、アプリ（ミューテータ）とGCが同時に動く。アプリがオブジェクトを書き換えている最中にGCがスキャンすると、不整合が起きる。

```
時刻 T1: GC が オブジェクトA をスキャン済み（生存とマーク）
時刻 T2: アプリが A.ref = B を実行（BへのポインタをAに格納）
時刻 T3: GC が スキャン完了、B は未到達と判断 → 誤って回収！
```

**解決**: Write Barrier = 「ポインタを書き換える時にGCに通知する仕組み」

### Tri-color Marking

Concurrent GCは通常「三色マーキング」を使う：

| 色 | 意味 |
|----|------|
| **白** | 未スキャン（回収候補） |
| **灰** | スキャン中（子をまだ見ていない） |
| **黒** | スキャン完了（生存確定） |

**不変条件（Tri-color invariant）**:
> 黒オブジェクトから白オブジェクトへの直接参照があってはならない

Write Barrierはこの不変条件を維持する。

### Write Barrier の種類

#### 1. Snapshot-at-the-beginning (SATB)

GC開始時点のオブジェクトグラフを「スナップショット」として保持。**書き換え前の値を記録する**。

```c
// 擬似コード
void write_barrier_satb(Object* obj, Field field, Object* new_value) {
    Object* old_value = obj->field;
    if (gc_in_progress && old_value != NULL) {
        mark_grey(old_value);  // 旧値を生存候補に追加
    }
    obj->field = new_value;
}
```

**特徴**:
- GC開始時点で到達可能だったものは全て生存扱い
- 保守的（ゴミを残す可能性）だが安全
- G1GC、Shenandoahで採用

#### 2. Incremental Update

新しく作られた参照を記録する。**書き換え後の値を記録する**。

```c
// 擬似コード
void write_barrier_incremental(Object* obj, Field field, Object* new_value) {
    obj->field = new_value;
    if (gc_in_progress && new_value != NULL) {
        mark_grey(new_value);  // 新値を再スキャン対象に
    }
}
```

**特徴**:
- 新しい参照を追跡
- より正確だが再スキャンが必要
- CMSで採用

### 図解: なぜWrite Barrierが必要か

```
【Write Barrierなしの場合】

    GCスキャン済み        未スキャン
         ↓                  ↓
        [A] ─────────────→ [C]
                            ↑
                           [B] (未スキャン)

Step 1: GCがAをスキャン、Aは生存
Step 2: アプリが A.ref = B を実行（AがBを指すように変更）
Step 3: アプリが C.ref = null を実行（CからBへの参照を削除）
Step 4: GCがCをスキャン、Bへの参照なし
Step 5: GCがBを未到達と判断 → Bを回収（しかしAから参照されている！）

        [A] ────→ [B] ← 回収されてしまった！ダングリングポインタ


【Write Barrierありの場合】

Step 2: A.ref = B を実行
        → Write Barrier発動
        → 「Bが新しく参照された」をGCに通知
        → Bを再スキャン対象に追加
Step 5: GCがBをスキャン → 生存と判断 → 安全
```

### Write Barrierのオーバーヘッド

| 項目 | 影響 |
|------|------|
| **命令追加** | 各ポインタ書き込みに数命令追加 |
| **分岐** | GC中かどうかのチェック |
| **メモリ** | バリア情報の記録領域（card table等） |
| **典型的コスト** | 全体性能の1-5%程度 |

```c
// 最適化されたWrite Barrier（card marking方式）
void write_barrier_card(Object* obj, Object* new_value) {
    obj->field = new_value;
    card_table[obj >> CARD_SHIFT] = DIRTY;  // 1命令で済む
}
```

### Read Barrier との比較

| 方式 | タイミング | 用途 |
|------|-----------|------|
| **Write Barrier** | ポインタ書き込み時 | 参照の変更を追跡 |
| **Read Barrier** | ポインタ読み込み時 | オブジェクト移動に対応 |

Read Barrierはさらにコストが高い（読み込みは書き込みより頻繁）が、ZGCやShenandoahはコンパクション（移動）のために使用。

### LOSへの示唆

- GHCの現行GCはstop-the-worldなのでWrite Barrier不要
- LOSでConcurrent GCを目指すなら、Write Barrierの実装が必須
- **Haskellの特性**: 不変データが多いので、Write Barrierの発動頻度は低い可能性（要検証）
- 可変参照（IORef, STRef, MVar）のみにWrite Barrierを適用すれば十分かもしれない

---

## 未解決の問い

1. **GHC RTSの改造はどこまで現実的か？**
   - GHCコミュニティとの協調は可能か
   - forkして独自メンテナンスするか

2. **Linear Typesとの統合**
   - 線形型で管理されるリソースはGC対象外にできる
   - どこまでGC負荷を減らせるか

3. **世代別GCとの相性**
   - GHCは世代別GC（若い世代は頻繁にGC）
   - 専用コア方式との相性は？

4. **ベンチマーク設計**
   - OS的ワークロードでのGC影響の測定方法
   - リアルタイム性の定量的評価

---

## 参考文献

- [The Garbage Collection Handbook](https://gchandbook.org/)
- [ZGC - Oracle](https://docs.oracle.com/en/java/javase/17/gctuning/z-garbage-collector.html)
- [Shenandoah GC - Red Hat](https://wiki.openjdk.org/display/shenandoah)
- [GHC RTS Options](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html)
- [Non-stop Haskell (研究)](https://www.microsoft.com/en-us/research/publication/non-stop-haskell/)

---

*作成日: 2026-01-18*
*ステータス: 将来の研究テーマとして記録*
