# emacs-setting

## color theme
solarized dark mode

## packages
### company 自動補完
### ivy
### counsel emacs上でのファイル移動強化
### swiper 検索強化
### flycheck　シンタックスチェック
### company-jedi ライブラリの補完
はじめにjediを入れる必要がある．`pip install jedi`でインストール可能
### jedi
初期起動時に`M-x jedi:install-server`で利用可能（install-serverにはpyenvが必要なので、インストールしていない場合は`pip install virtualenv`でインストールする）
### js2-mode javascriptの文法補完
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

## その他設定
- 行番号表示
- 警告音、フラッシュ無効
- バックアップファイルの作成無効
