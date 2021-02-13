---
title: Deploying your Webapp
published: 2021/02/13
---

他人のことは言えないが, 個人的にはシステム管理のためのプログラミングを好む. しかし実際のところは結局, アプリケーションは供給する必要があり, あなたもシステム管理者となりセットアップをする必要がある.

Haskellウェブコミュニティにはデプロイメントを容易にする有望な構想がある. 将来, 1つのコマンドでアプリケーションをデプロイできるようなサービスでさえ可能かもしれない. 

しかし今のところはそこまでは至っていない. そしてそうであったとしても, そのような解決策は全ての人にとって上手くいくものではない. この章ではデプロイメントのための様々な方法を挙げ, 異なる状況下においてどのような方法を取るかについての一般的な推奨について説明する. 

YesodチームはYesod開発のための[quick startガイド](http://www.yesodweb.com/page/quickstart)で述べられているように, `stack build`ツールを用いることを強く推奨する. まだの場合はquick startをチェックしてください.

## Keter

Yesod scaffoldingはKeter開発エンジンを組み込みでサポートしている. そして, それはHaskellで書かれており, WAIやhttp-clientのような多くの根本が同じ技術を用いている. Keterはアプリケーションに対し, リバースプロキシーとして機能し, 実行しているアプリケーションの立ち上げ, モニタリング, 再デプロイとしても機能する. Keterでデプロイしたい場合, 次のステップを実行してください.

1. scaffoldアプリケーションにおける`config/keter.yaml`を必要に応じ編集する.

2. アプリケーションをホストするための, サーバを設定する. 個人的にはAmazon EC2をUbuntuで実行することを推奨する.

3. Keterをそのマシンにインストールする. 最も最新であろう[Keterウェブサイト](https://github.com/snoyberg/keter/)を見てください.

4. `yesod keter`を実行し, Keterバンドルを生成する, 例えば`myapp.keter`のように.

5. `myapp.keter`をサーバの`/opt/keter/incoming`ディレクトリにコピーする.

もし正しく設定できていれば, 製品環境で実行されているウェブサイトを見れることでしょう! 将来アップグレードは`yesod keter`を実行し, `myapp.keter`バンドルをサーバに再コピーしさえすれば可能になるだろう. Keterは自動的に新しいファイルの存在を認知し, アプリケーションをリロードすることに留意してください.

この章の残りでは様々なステップにおける詳細について説明し, scaffoldやKeterを使わない代理案について説明する.

## コンパイル

与えることのできる最大のアドバイスとしては: サーバ上でコンパイルしないでください. そうしたい気持ちはわかる. ソースコードを移動したり, 混乱のもととなる依存関係の問題を避ける必要があるためである. しかし, YesodアプリケーションのコンパイルにはかなりのメモリとCPUを要する. これは次のことを意味する.

- アプリケーションを再コンパイルしている間, 既存アプリケーションはパフォーマンス上の影響を受ける.

- コンパイルをするためにずっと大きなマシーンが必要であり, その容量は大部分の時間は動いていない. なぜならば, YesodアプリケーションはGHC自身よりもずっと少ないCPUとメモリしか要しないためである. 

一度コンパイルする準備ができたら, 常に`stack clean`を新しい製品ビルドの前に行い, 古いファイルがないことを確実にすべきである. そして, `stack build`を実行することで実行ファイルを得, それは`dist/build/myapp/myapp`に配置される. 

## デプロイするファイル

Yesod scaffoldアプリケーションでは, 本質的には3セットのファイルをデプロイする必要がある:

1. 実行ファイル.

2. configフォルダ.

3. staticフォルダ.

その他, Shakespeareテンプレートなどは, 実行ファイル自身にコンパイルされる.

しかし, 1つ注意点がある: `config/client_session_key.aes`ファイルである. このファイルはサーバ側の暗号化を制御し, クライアント側のセッションクッキを安全なものとする. もし存在しない場合, Yesodは自動的にこれらのキーの新しいものを生成する. 実際的には, これは次のことを意味する. もしこのファイルをデプロイメントに含まない場合, 再デプロイした場合すべてのユーザが再ログインする必要がある. もし上のアドバイスに従い, `config`フォルダを含めば, この問題は部分的には解決される. 他の方法としては, セッションキーを環境変数に入れることである. 

残り半分の解決策としては, 一度`config/client_session_key.aes`ファイルを生成したら, ずっと将来の開発に渡りそれを持ち続けることである. これを確実にするための最も簡単な方法は, そのファイルをバージョンコントロールすることである. しかし, もしバージョンコントロールがオープンソースである場合, これは危険である: レポジトリへのアクセス可能な人なら誰でも認証情報を盗めてしまう!

ここで述べられた問題は本質的にはシステム管理に関するもので, プログラミングに関するものではない. Yesodは安全にクライアントのセッションキーを保管するための組み込みの機能を有していない. もしオープンソースレポジトリがあり, そのソースコードレポジトリにアクセス権がある人全員を信頼しているのでなければ, クライアントセッションキーを安全に保管するための解決策を探ることは非常に重要である. 

## SSLとstaticファイル

Yesodの世界には2つの一般的に用いられる機能がある: HTTPS上でサイトを立ち上げること, staticファイルを別々のドメイン名で保存すること. これら両者はよい慣習であるが, もし組み合わされると不注意な場合, 問題を引き起こす. 特に, 多くのウェブブラウザはHTMLがHTTPSドメイン名から提供された場合, JavascriptファイルをHTTPSでないドメインからはロードしない. この場合, 次のうち1つを行う必要がある:

- staticファイルもHTTPS上で提供する.

- staticファイルをメインサイトと同じドメイン名から提供する. 

もし前者を選択した場合(よりよい方であるが), 2つの異なるSSL証明書, あるいはワイルドカード証明書を用いる.

## Warp

以前述べたように, Yesodはウェブアプリケーションインターフェース(WAI)上に作られ, どのWAIバックエンドでも実行可能である. これを書いている時点では, 次のバックエンドが利用可能である.

- Warp

- FastCGI

- SCGI

- CGI

- Webkit

- Development server

最後の2つは製品デプロイメント向けではない. 残り4つに関し, 理論的には全て製品デプロイメントのために使用可能である. 特に, CGIバックエンドは非常に非効率的になりがちである. なぜならば, 新しいプロセスが各コネクションごとに生成される必要があるためである. そしてSCGIはWarp(リバースプロキシにより)やFastCGIほどにはフロントエンドウェブサーバによってサポートされていない.

残り2つのうち, Warpは最も強く推奨される. なぜならば:

- かなり早い.

- FastCGIのように, リバースHTTPプロキシを用いてNginxのようなフロントエンドサーバの裏で実行可能である.

- さらに, それ自身完全に有能なサーバであり, したがってフロントエンドサーバがなくても使うことができる. 

すると1つの疑問が残る: Warpはそれ自身で実行すべきか, それともフロントエンドサーバの裏でリバースプロキシを通して実行されるべきか? 大部分の使用において, 後者を推奨する. なぜならば:

- アプリケーションの前にリバースプロキシを用いることで, 新しいバージョンをデプロイするのが容易になる.

- また, もしアプリケーションにバグがある場合, リバースプロキシによりユーザに多少わかりやすいエラーメッセージを与えられる.

- バーチャルホスティングを用いることで, 1つのホストから複数のアプリケーションをホストできる.

- リバースプロキシはロードバランサとSSLプロキシとしての両方の機能を持ち, アプリケーションを単純化できる. 

上で議論したように, Keterは取りかかりに非常に適している. もしNginxのような既存のウェブサーバがあれば, Yesodは代わりにその裏側でちゃんと機能する. 

## Nginxの設定

Keterの設定は単純である. なぜならば, それはYesodアプリケーションと一緒に機能するように設計されているためである. しかしもし代わりにNginxを使いたい場合, どのようにセットアップすればよいであろうか?

一般的に, Nginxはポート80を利用し, Yesod/Warpアプリは他の特別でないポート(例えば4321)を利用する. 次のようなnginx.confファイルを与える必要がある:

```
daemon off; # Don't run nginx in the background, good for monitoring apps
events {
    worker_connections 4096;
}

http {
    server {
        listen 80; # Incoming port for Nginx
        server_name www.myserver.com;
        location / {
            proxy_pass http://127.0.0.1:4321; # Reverse proxy to your Yesod app
        }
    }
}
```

好きなだけサーバのブロックを付け足してよい. 一般的に追加するものとしては, ユーザがドメイン名にwww接頭辞を用いてページにアクセスすることを保証することである. これにより正規化URLにおけるRESTful原則が確実になる. (容易に反対のことをしがちであり, 常にwwwを除いてしまう. 変更がnginxとサイトのapproot両方に反映されているか確認してください.) この場合, 次のようなブロックを追加する.

```
server {
    listen 80;
    server_name myserver.com;
    rewrite ^/(.*) http://www.myserver.com/$1 permanent;
}
```

非常に推奨される最適化としては, staticファイルを別々のドメイン名から提供することであり, したがって, クッキー転送オーバーヘッドを避けることができる. staticファイルがサイトフォルダの`static`フォルダにあり, サイトフォルダが`/home/michael/sites/mysite`にあると仮定すれば, 次のようになる:

```
server {
    listen 80;
    server_name static.myserver.com;
    root /home/michael/sites/mysite/static;
    # Since yesod-static appends a content hash in the query string,
    # we are free to set expiration dates far in the future without
    # concerns of stale content.
    expires max;
}
```

これが機能するために, サイトがこの代わりのドメイン名に合わせて, 正しくstatic URLを書き直す必要がある. scaffoldedサイトはこれを`Settings.staticRoot`関数と`urlRenderOverride`の定義を用いて, かなり簡単にできるようにしてある. しかし, もし別々のドメイン名を用いずにnginxの速いstatic file提供を生かしたいのならば, 代わりにもともとのサーバブロックを次のように変更できる:

``` 
server {
    listen 80; # Incoming port for Nginx
    server_name www.myserver.com;
    location / {
        proxy_pass http://127.0.0.1:4321; # Reverse proxy to your Yesod app
    }
    location /static {
        root /home/michael/sites/mysite; # Notice that we do *not* include /static
        expires max;
    }
}
```

## サーバのプロセス

多くの人々はApache/mod_phpやLighttpd/FastCGiなどのセットアップに慣れ親しんでいる. それらにおいては, ウェブサーバが自動的にウェブアプリケーションを生成する. nginxでは, リバースプロキシであれFastCGIであれ, これは当てはまらない: 自分でプロセスを実行する必要がある. モニタリングユーティリティを用いて, クラッシュした際に自動的にアプリケーションを再スタートすることを強く推奨する. angelやdaemontoolのように多くの素晴らしい方法がある. 

具体例を挙げるために, ここにUpstart設定ファイルがある. ファイルは`etc/init/mysite.conf`に置く必要がある:

```
description "My awesome Yesod application"
start on runlevel [2345];
stop on runlevel [!2345];
respawn
chdir /home/michael/sites/mysite
exec /home/michael/sites/mysite/dist/build/mysite/mysite
```

一度これを正しく置けば, アプリケーションを進めるのは`sudo start mysite`と同じくらい容易である. 同じようにシステム設定ファイルは`/etc/systemd/system/yesod-sample.service`に置かれる.

```
[Service]
ExecStart=/home/sibi/.local/bin/my-yesod-executable
Restart=always
StandardOutput=syslog
StandardError=syslog
SyslogIdentifier=yesod-sample

[Install]
WantedBy=multi-user.target
```

するとサービスを次のように進めることができる:

```
systemctl enable yesod-sample
systemctl start yesod-sample
```

`systemctl status yesod-sample`を用いて, プロセスのステータスを見ることができる. 

## Nginx + FastCGI

中にはFastCGIをデプロイメントに用いたい人もいるかもしれない. この場合, さらにツールを追加する必要がある. FastCGIは新しいコネクションをファイルディスクリプタから受け取ることで機能する. Cライブラリはこのファイルディスクリプタが0(標準入力)であることを想定する. よって, spawn-fcgiプログラムを用いて, アプリケーションの標準入力を正しいソケットにバインドする必要がある. 

このために, ポートにバインドする代わりに, Unix namedソケットを用いる方が便利である. 特に複数のアプリケーションを1つのホストでホスティングする場合はそうである. アプリをロードするスクリプトとしては次のようになる:

``` 
spawn-fcgi \
    -d /home/michael/sites/mysite \
    -s /tmp/mysite.socket \
    -n \
    -M 511 \
    -u michael \
    -- /home/michael/sites/mysite/dist/build/mysite-fastcgi/mysite-fastcgi
```

フロントエンドサーバを設定し, アプリにFastCGIを通して通信できるようにする必要がある. これはNginxでは比較的容易である:

``` 
server {
    listen 80;
    server_name www.myserver.com;
    location / {
        fastcgi_pass unix:/tmp/mysite.socket;
    }
}
```
それはとても馴染み深いものであろう. 唯一の最後のトリックとしては, Nginxでは手動で全てのFastCGI変数を指定する必要があるということである. これらを異なるファイル(例えば, fastcgi.conf)に保存し, `include fastcgi.conf;`をhttpブロックの最後に追加することが推奨される. WAIと連携するために, ファイルのコンテンツは次のようになる:

```
fastcgi_param  QUERY_STRING       $query_string;
fastcgi_param  REQUEST_METHOD     $request_method;
fastcgi_param  CONTENT_TYPE       $content_type;
fastcgi_param  CONTENT_LENGTH     $content_length;
fastcgi_param  PATH_INFO          $fastcgi_script_name;
fastcgi_param  SERVER_PROTOCOL    $server_protocol;
fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;
fastcgi_param  REMOTE_ADDR        $remote_addr;
fastcgi_param  SERVER_ADDR        $server_addr;
fastcgi_param  SERVER_PORT        $server_port;
fastcgi_param  SERVER_NAME        $server_name;
```

## Desktop

他のしゃれたバックエンドにはwai-handler-webkitがある. このバックエンドはWarpとQtWebkitを組み合わせ, 実行ファイルを作り, ユーザは単にダブルクリックするだけでよい. これはアプリケーションのオフライン版を与えるための便利な方法である.

これにおいてYesodで最も便利な点としては, テンプレートは全て実行ファイルにコンパイルされ, アプリケーションで配布する必要がないことである. しかしながら, staticファイルもそれを行なっている. 

<div class="yesod-book-notice">
実際にstaticファイルを直接実行ファイルに埋め込むためのサポートも存在する. 詳細については, yesod-static文書を見てください.
</div> 

QtWebkitライブラリを必要としない同様の方法は, wai-handler-launchであり, これはWarpサーバを立ち上げ, そしてユーザのデフォルトウェブブラウザを起動する. ここでは多少のトリックがある: ユーザがまだサイトを用いていることを知るために, `wai-handler-launch`は"ping" Javascriptコードを与えられる全てのHTMLページに挿入する. もし`wai-handler-launch`がpingを2分間受け取らなかった場合, シャットダウンする. 

## Apache上でのCGI

CGIとFastCGIはApache上でほとんど同じように機能する. よって, この設定になるように変換することは非常に単純である. 本質的には2つの目標を達成する必要はある.

1. サーバにファイルを(Fast)CGIとして与えさせる.

2. サイトへの全てのリクエストをリライトし, (Fast)CGI実行ファイルを経由するようにする. 

ここではブログアプリケーションを与えるための設定ファイルを挙げる. 実行ファイルは"bloggy.cgi"という名前であり, ドキュメントルートの"blog"という名前のサブフォルダにある. この例は`/f5/snoyman/public/blog`のパスにあるアプリケーションから取られる. 

```
Options +ExecCGI
AddHandler cgi-script .cgi
Options +FollowSymlinks

RewriteEngine On
RewriteRule ^/f5/snoyman/public/blog$ /blog/ [R=301,S=1]
RewriteCond $1 !^bloggy.cgi
RewriteCond $1 !^static/
RewriteRule ^(.*) bloggy.cgi/$1 [L]
```

最初のRewriteRuleはサブフォルダを扱うためである. 特に, これは`/blog`へのリクエストを`/blog/`へリダイレクトする. 最初のRewriteCondは直接実行ファイルにリクエストするのを防ぎ, 2つ目はApacheがstaticファイルを与えるのを許可する. そして最後の行で実際のリライトを行う. 

## lighttpd上でのFastCGI

この例では, いくつかのmime-typeのような基本的なFastCGI設定は省略する. また, 製品版においてより複雑なファイルがあり, 存在しない場合"www\."を先頭に追加し異なるドメインからstaticファイルを与える. しかし, これは基本を知るのに役立つであろう. 

ここでは, "/home/michael/fastcgi"はfastcgiアプリケーションである. アイデアとしては"/app"で始まる全てのリクエストをリライトし, "/app"で始まる全てのページをFastCGI実行ファイルを経由して与えることである. 

``` 
server.port = 3000
server.document-root = "/home/michael"
server.modules = ("mod_fastcgi", "mod_rewrite")

url.rewrite-once = (
  "(.*)" => "/app/$1"
)

fastcgi.server = (
    "/app" => ((
        "socket" => "/tmp/test.fastcgi.socket",
        "check-local" => "disable",
        "bin-path" => "/home/michael/fastcgi", # full path to executable
        "min-procs" => 1,
        "max-procs" => 30,
        "idle-timeout" => 30
    ))
)
```

## lighttpd上でのCGI

これは基本的にはFastCGI版と同じである. しかし, lighttpdに".cgi"で終わるファイルをCGI実行ファイルとして実行するように指示する. この場合, ファイルは"/home/michael/myapp.cgi"にある.

```
server.port = 3000
server.document-root = "/home/michael"
server.modules = ("mod_cgi", "mod_rewrite")

url.rewrite-once = (
    "(.*)" => "/myapp.cgi/$1"
)

cgi.assign = (".cgi" => "")
```