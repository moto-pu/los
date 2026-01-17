# LOS Kernel

ベアメタルで動作するHaskellカーネル。

## アーキテクチャ

```
+------------------+
|    User Shell    |  ← Haskell (Kernel.Shell)
+------------------+
|  Login System    |  ← Haskell (Kernel.Login)
+------------------+
|  Device Drivers  |  ← Haskell (Kernel.VGA, Kernel.Keyboard)
|  (VGA, Keyboard) |
+------------------+
|   I/O Ports      |  ← Haskell FFI + Assembly (boot/io.S)
+------------------+
|  Boot Sequence   |  ← Assembly (boot/boot.S)
+------------------+
|   Multiboot2     |  ← GRUB
+------------------+
|    Hardware      |  ← x86 (QEMU)
+------------------+
```

## ディレクトリ構造

```
kernel/
├── boot/
│   ├── boot.S          # エントリーポイント（アセンブリ）
│   ├── io.S            # I/Oポートアクセス（アセンブリ）
│   └── grub/
│       └── grub.cfg    # GRUB設定
├── src/
│   └── Kernel/
│       ├── Boot.hs     # ブートシーケンス
│       ├── VGA.hs      # VGAテキストモードドライバ
│       ├── IO.hs       # I/Oポートアクセス（FFI）
│       ├── Keyboard.hs # キーボードドライバ
│       ├── Login.hs    # ログインシステム
│       └── Shell.hs    # シェル
├── linker.ld           # リンカスクリプト
├── Makefile            # ビルドシステム
└── README.md           # このファイル
```

## 起動シーケンス

1. **BIOS** → GRUB起動
2. **GRUB** → Multiboot2ヘッダーを読み、カーネルをロード
3. **boot.S** → スタック設定、`kernelMain`呼び出し
4. **Kernel.Boot** → VGA初期化、キーボード初期化
5. **Kernel.Login** → ユーザー認証
6. **Kernel.Shell** → コマンドシェル起動

## ビルド要件

ベアメタルでHaskellを動かすには以下が必要：

### 1. クロスコンパイラ

```bash
# macOS (Homebrew)
brew install i686-elf-gcc

# Ubuntu
apt install gcc-i686-linux-gnu
```

### 2. GHC ベアメタルRTS

**これが最大の課題**。GHCの標準RTSはOSの機能に依存するため、ベアメタルでは動作しない。

選択肢：
- **House OS方式**: カスタムRTSを実装
- **HaLVM方式**: Xen上で動かす（厳密にはベアメタルではない）
- **自前コンパイラ**: Haskellのサブセットを直接コンパイル

### 3. QEMU

```bash
brew install qemu
```

## 現在の状態

- [x] ブートストラップコード（アセンブリ）
- [x] Haskellカーネルモジュール（ソースコード）
- [ ] GHC ベアメタルRTS
- [ ] リンク可能なバイナリ
- [ ] QEMUでの動作確認

## 次のステップ

1. **House OSの調査**: https://programatica.cs.pdx.edu/House/
2. **カスタムRTSの実装**: メモリ管理、例外処理、GC
3. **リンカの調整**: GHCオブジェクトをベアメタル向けにリンク

## デフォルト認証情報（PoC）

- Username: `root`
- Password: `lambda`

## 参考資料

- [House OS](https://programatica.cs.pdx.edu/House/)
- [OSDev Wiki](https://wiki.osdev.org/)
- [GHC Commentary](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary)
- [Writing an OS in Rust](https://os.phil-opp.com/) - 参考になるアプローチ

---

*このカーネルは研究・教育目的のPoCです。*
