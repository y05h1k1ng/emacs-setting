# emacs-setting
- [Color-Theme](##Color Theme)
- [Miscs](##Miscs)
- [Packages](##Packages)
    - [特定の言語に限らない、便利Packages](###特定の言語に限らない、便利Packages)
	    - [company:★★★★★](####company ★★★★★)
		- [ivy:★★★★★](####ivy ★★★★★)
		- [counsel:★★★★★](####counsel ★★★★★)
		- [swiper:★★★★★](####swiper ★★★★★)
		- [flycheck:★★★★★](####flycheck ★★★★★)
		- [ace-window:★★★★★](####ace-window ★★★★★)
		- [magit:★★★★☆](####magit ★★★★☆)
		- [wgrep:★★★☆☆](####wgrep ★★★☆☆)
		- [multiple-cursors:★★★★☆](####multiple-cursors ★★★★☆)
		- [yasnippet:★★★☆☆](####yasnippet ★★★☆☆)
		- [rainbow-delimiters:★★★☆☆](####rainbow-delimiters ★★★☆☆)
		- [nyan-mode:★★★★★(☆☆☆☆☆)](####nyan-mode ★★★★★(☆☆☆☆☆))
		- [all-the-icons:★★★☆☆](####all-the-icons ★★★☆☆)
		- [projectile:★★★☆☆](####projectile ★★★☆☆)
		- [redo-tree:★★★★★](####redo-tree ★★★★★)
		- [emoji-cheat-sheet-plus:★★★☆☆](####emoji-cheat-sheet-plus ★★★☆☆)
	- [python](###python)
	    - [jedi](####jedi)
		- [company-jedi](####company-jedi)
	- [sagemath](###sagemath)
	    - [sage-shell-mode](####sage-shell-mode)
	- [html,js,css等](###html, js, css等)
	    - [web-mode](####web-mode)
	- [C++](###C++)
	    - [irony](####irony)
	- [golang](###golang)
		- [company-go](####company-go)
		- [go-mode](####go-mode)
	- [org](###org)
	    - [org mode](####org mode)
	- [markdown](###markdown)
	    - [markdown mode](####markdown mode)
- [過去に使ってた](##過去に使ってた)
    - [spaceline](###spaceline)
	- [js系](###js系)
	    - [rjsx-mode](####rjsx-mode)
		- [tern(company-tern)](####tern(company-tern))

## Color Theme
dracula-theme

## Miscs
- 行番号表示
- 警告音、フラッシュ無効
- バックアップファイルの作成無効
- スクロールバー、ツールバー、メニューバーを非表示
- スタートメニュー非表示

## Packages
### 特定の言語に限らない、便利Packages
#### company ★★★★★
- 自動補完
- 以前に入力した文字を覚えておいてくれる

#### ivy ★★★★★
- Switch Bufferとか？で助けてくれてるっぽい

#### counsel ★★★★★
- emacs上でのファイル移動強化

#### swiper ★★★★★
- 検索強化
- `C-s`が使いやすくなるのは、swiperのおかげ

#### flycheck ★★★★★
- シンタックスチェック
- どのファイルをチェックしてくれてるかわかんない

```
C-c n: 次のエラー
C-c p: 前のエラー
C-c d: エラーのリスト表示
```

#### ace-window ★★★★★
- 画面分割の移動補助
- `M-o`を割り当て
- 画面が2つだけのときは、`M-o`で画面移動のみ（削除とかはデフォルトの使ってネ）

```
M-o [asdfghjkl]: 画面移動
M-o x []: 削除
M-o m []: 移動
M-o ?: ヘルプ
```

#### magit ★★★★☆
- git使う
- diff見やすいのが嬉しい
- `C-x g`を割り当て

#### wgrep ★★★☆☆
- grepで引っかかった単語等を一括編集できる
- multiple-cursorsでは一画面でしか一括で選択できないが、一つのファイルに限らずディレクトリ内に当てはまる単語を編集したいときに便利
- M-x grepでgrep実行（コマンド: `grep -nH --null [options] <string> <file>`）
- grep bufferにgrep結果が表示
- grep buffer上での書き込みは `e` を割り当て
- `C-c C-c` `C-c C-s`で保存
- `C-c C-k`で破棄

#### multiple-cursors ★★★★☆
- 複数カーソル

```
C->: 下の行にカーソル追加
C-<: 上の行にカーソル追加
C-C C-C: 選択範囲の行にカーソル一括追加
C-c C-<: 画面内の選択単語にカーソル一括追加
```

#### yasnippet ★★★☆☆
- snippet管理
- 使用用途はC++(競プロ)かpython, sagemath(CTF)
- キーバインド割り当て
```
C-x i i 既存スニペットを挿入(keywordでtabでもok)
C-x i n 新規作成
C-x i v 既存スニペットを閲覧・編集
M-x yas-describe-tables 一覧表示

---------- snippet上で ----------
C-c C-c スニペット保存
C-c C-l ロード
```

#### rainbow-delimiters ★★★☆☆
- 括弧に色をつける
- jsonとか見やすくなって便利

#### nyan-mode ★★★★★(☆☆☆☆☆)
- nyan-catが下のバーでおおよその位置を表示してくれる神機能:heart_eyes_cat:
- （あってもなくても...まあ少しだけテンションが上がるかな）

#### all-the-icons ★★★☆☆
- `dired` と `ivy`に適応させている
- はじめは、`M-x all-the-icons-install-fonts`でfontのインストールをしてから、ターミナル上で`fc-cache -f -v`でインストール

#### projectile ★★★☆☆
- projectごとのファイル移動が楽になる
- gitとか決められたファイル構造のディレクトリしかprojectとして認識してくれない
    - 任意のディレクトリも覚えてくれたらいいのに....:cry:
```
M-s p f: プロフェクト内のファイル検索
M-s p p: プロジェクトの移動
```

#### redo-tree ★★★★★
- redoができるようになる
```
M-/: redo
C-x u: redo-treeの表示 （履歴みたいなやつ）

---------- redo-tree内での操作 ----------
C-p: 上
C-n: 下
d: diffの表示
```

#### emoji-cheat-sheet-plus ★★★☆☆
- MELPAにないので、ここからcloneする(https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus)
- helmも入ったぽい（というか、内部で使うからインストールする:innocent:）
- `C-c C-e`でinsert (org内ではexportのキーバインドと同じでつかないので、`M-x emoji-cheat-sheet-plus-insert`する)
- フォントのインストールする `sudo apt install fonts-symbola`
- org, markdown, magit 以外でも表示させたかったら、display-modeをhookする

### python
#### jedi
- 初期起動時に`M-x jedi:install-server`で利用可能（install-serverにはpyenvが必要なので、インストールしていない場合は`pip install virtualenv`でインストールする）

#### company-jedi
- company強化
- はじめにjediを入れる必要がある．`pip install jedi`でインストール可能

### sagemath
#### sage-shell-mode
- companyがサポートされてなくてauto-completeが動くようにしたが、`.sage`のときにcompanyをoffにできない

### html, js, css等
#### web-mode
- html, js諸々見やすくしてくれる
- タグの補完便利

### C++
#### irony
- c系のシンタックスチェック
- c, c++, objectc, irony-mode?にhook済み
- companyにも入れてる(company-irony?入れないとだめかも)
- 初回はirony serverをインストールしないと行けないので`M-x irony-server-install`する。
    - 多分怒られると思うので、<https://github.com/Sarcasm/irony-mode/issues/167>見て頑張る。
    - `~/.emacs.d/irony`とか`/tmp/build-irony-server`とか残ってたら一旦消したほうがよさげ


### golang
goの方のパッケージをインストールしておく
```
go get github.com/rogpeppe/godef # 関数定義等の参照パッケージ
go get -u github.com/nsf/gocode # 補完パッケージ
go get github.com/golang/lint/golint # flycheckでシンタックスエラーを検知
go get github.com/kisielk/errcheck # flycheckでシンタックスエラーを検知
```
#### company-go
- TODO
#### go-mode
- TODO

### org
- emacsといえばのコレ
- デフォルトでも使えるが、いろいろカスタマイズ(Dropboxとか)したかったら:point_down:を入れる
#### org mode
- パッケージ追加しとく`(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))`
- `M-x install RET org`
    - buildでコケるけど、うまく動く
- memo取るのにめちゃくちゃ便利（Dropboxで自動同期）
- 設定はコレ見て(https://solist.work/blog/posts/information-organizize-method/)

```
C-c c: org-select表示
C-c a a: org-agenda表示

---------- org-select内 ----------
m: memo
t: task
表示されるからそれみればOK

---------- CAPTURE-*.org内 ----------
C-c C-c: 保存
C-c C-k: 削除
```

### markdown
#### markdown mode
- ここのquick referenceよく書かれているから見て(https://leanpub.com/markdown-mode/read#leanpub-auto-quick-reference)
- pandocのインストールが必要(`sudo apt install pandoc`)
- previewにはpandocを使うようにした
    - `(setq markdown-command "pandoc")`に引数渡したいけど, `hogehoge is not found`になるのどうにかする(TODO)
```
C-c C-c p: preview in browser (md -> htmlから/tmp/にhtmlを置く)
```

## 過去に使ってた
### spaceline
- 下のバーがかっこよくなる。ここでもall-the-iconsを使用
- ゲロおも:shit:

### js系
バイトでバリバリ使ってたけど`web-mode`に落ち着いてしまった

#### rjsx-mode
- jsxファイルの補完諸々
- .jsファイルは全てrjsx-modeになる（reactさわるときは便利？？）

#### tern(company-tern)
- javascriptの関数補完
- はじめにnodeを入れる必要がある
- `npm install -g tern`でternをインストールする。
- homeディレクトリに.tern-configを作る。内容は以下
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
