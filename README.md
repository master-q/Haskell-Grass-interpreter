Haskellで作ったGrass言語インタープリタ
======================================

ParsecとStateモナドの練習に作ってみました。
もっとまともなのは
[@mr_konnさんの実装](https://github.com/konn/hagi)
を見た方がいいです。

Grass言語については↓を参照してください。

* [ちょっと草植えときますね型言語 Grass](http://www.blue.sky.or.jp/grass/doc_ja.html)
* [うはｗｗｗ Mosh で Grass 実装したｗｗｗｗ - ひげぽん OSとか作っちゃうかMona-](http://d.hatena.ne.jp/higepon/20080605/1212678422#c)
* [GrassのHello World - * *scrap*](http://d.hatena.ne.jp/youz/20080606/1212769842)

使い方
------

    $ make
    $ ./grass < sample/infw.in
    wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww...

でもまだutf8対応してないです。
それから文字入力もまだ実装されていません。

テスト
------

[stdiotest](https://github.com/yuzutechnology/stdiotest)
をインストールしてから↓でテスト実行します。

    $ make test
