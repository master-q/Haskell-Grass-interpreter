Haskellで作ったGrass言語インタープリタ
======================================

ParsecとStateモナドの練習に作ってみました。
もっとまともなのは
[@mr_konnさんの実装](https://github.com/konn/hagi)
を見た方がいいです。

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
