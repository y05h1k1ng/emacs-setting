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
### irony
c++のシンタックスチェック

初回はirony serverをインストールしないと行けないので
`M-x irony-server-install`する。

多分怒られると思うので、<https://github.com/Sarcasm/irony-mode/issues/167>見て頑張る。
`~/.emacs.d/irony`とか`/tmp/build-irony-server`とか残ってたら一旦消したほうがよさげ

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

### yasnippet
```
C-x i i 既存スニペットを挿入(keywordでtabでもok)
C-x i n 新規作成
C-x i v 既存スニペットを閲覧・編集
C-c C-c スニペット保存
C-c C-l ロード
M-x yas-describe-tables 一覧表示
```

### nyan-mode
nyan-catがおおよその位置を表示してくれる**神**機能

### all-the-icons
`dired` と `ivy`に適応させている

はじめは、`M-x all-the-icons-install-fonts`でfontのインストールをしてから、ターミナル上で`fc-cache -f -v`でインストール

### spaceline
下のバーがかっこよくなる。ここでもall-the-iconsを使用(重すぎて使ってない)

### projectile
projectごとのファイル移動が楽になる
```
M-s p f プロフェクト内のファイル検索
M-s p p プロジェクトの移動
```

### redo-tree
```
M-/ redo
C-x u redo-treeの表示 （履歴みたいなやつ）
```
redo-tree内での操作
```
C-p 上
C-n 下
d diffの表示
```


## その他設定
- 行番号表示
- 警告音、フラッシュ無効
- バックアップファイルの作成無効
- スクロールバー、ツールバー、メニューバーを非表示
