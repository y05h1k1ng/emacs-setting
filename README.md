# emacs-setting

## color theme
dracula-theme

## packages
### company 自動補完
### ivy
### counsel emacs上でのファイル移動強化
### swiper 検索強化
### flycheck　シンタックスチェック
### ace-window
### magit
### company-jedi ライブラリの補完
はじめにjediを入れる必要がある．`pip install jedi`でインストール可能
### jedi
初期起動時に`M-x jedi:install-server`で利用可能（install-serverにはpyenvが必要なので、インストールしていない場合は`pip install virtualenv`でインストールする）
### js2-mode javascriptの文法補完
### rjsx-mode
jsxファイルの補完諸々

.jsファイルは全てrjsx-modeになる（reactさわるときは便利？？）。
### tern(company-tern) javascriptの関数補完
はじめにnodeを入れる必要がある。

それから、`npm install -g tern`でternをインストールする。
あとは、homeディレクトリに.tern-configを作る。内容は以下
```
{
  "libs": [
    "browser",
    "jquery"
  ],
  "plugins": {
    "node": {}
  }
}
```

### golang 関連
goの方のパッケージをインストールしておく
```
go get github.com/rogpeppe/godef # 関数定義等の参照パッケージ
go get -u github.com/nsf/gocode # 補完パッケージ
go get github.com/golang/lint/golint # flycheckでシンタックスエラーを検知
go get github.com/kisielk/errcheck # flycheckでシンタックスエラーを検知
```
入っているのは、`company-go`, `go-mode`の2つ

### magit
gitつかえるよ
```
C-c g 起動
```
### wgrep
なんかわからん
### ace-window
画面分割の便利ショートカット
```
M-o (a, s, d, f, g...) 画面移動
M-o m 画面交換
M-o x 画面削除
M-o ? ヘルプ
```
### rjsx-mode
`.jsx`を認識させるために入れた

`.js`はすべてこのモードで開かれる

### multiple-cursors
- `C->`で次の行にカーソル追加
- `C-<`で前の行にカーソル追加
- 行選択後`C-C C-C`で選択行にカーソルを追加
- 単語選択後`C-c C-<`で画面内のその単語全てにカーソルを追加

## その他設定
- 行番号表示
- 警告音、フラッシュ無効
- バックアップファイルの作成無効
- スクロールバー、ツールバー、メニューバーを非表示
