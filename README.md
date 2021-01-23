# AtGorilla
## 本ツールについて
---
HaskellでAtCoderに参加する際に、サンプルのテスト等を行うツールです。
  
現在、入力例のテスト機能、提出機能、提出ステータス確認機能があります。
  
  
Haskellの勉強をかねて、自分用にゆっくり作成中です。
  
    
バグ等いっぱいあるかとおもいます。
  
ご使用いただけたらうれしいですが、いかなる不利益、損害が発生しても一切責任を持てません。
  
ご了承ください。
  
  
ver.0.0.1.0 では、`stack build`でビルドするプロジェクトが対象でしたが、
  
ver.0.0.2.0 より、`ghc`でビルドするプロジェクトがデフォルトの対象になります。
(一応`stack build`プロジェクト用に、`atgs`バイナリを用意しましたが将来的に削除すると思います。)


---
## コマンド内容

### cc

参加コンテストを指定します。
本コマンドにて、プロジェクト内に、`.atg`フォルダ、`Archive`フォルダ、及び`Current`リンクが作成されます。
`Current`リンクは、`Archive/<コンテスト名>`フォルダに対して貼られているので、
エディタ等でコーディング対象の選択にご使用ください。
現在と別のコンテストに対して作業をしたい場合、'cc'コマンドでコンテスト名を指定してください。
```
 $ ./atg cc <コンテスト名>
```
※コンテスト名: コンテストトップページのURLの最終`\`以降の文字列 (例:abc188)

### login

AtCoderに指定したユーザ名とパスワードでログインを試み、成功した場合ログイン情報を保存します。
(Cookieを保存しています。パスワードの平文はたぶん含まれていないため、保存されないと思います。)
```
$ ./atg login
username: <ユーザ名>
password: <パスワード>
```

### logout

AtCoderからログアウトし、`login`時に保存したログイン情報を削除します。
```
$ ./atg logout
```

### test

AtCoderから、指定したプログラムIDのサンプル入力例と出力例を取得し、
テストを実施します。
次のコマンドで検査対象コードのビルドを行います。
`ghc -o ./archive/<コンテスト名>/<プログラムID>.out -O0 ./archive/<コンテスト名>/<プログラムID>.hs

```
$ ./atg test <プログラムID>
running 3 tests
入力例1---------> OK
入力例2---------> OK
入力例3---------> OK
```
※プログラムID: コンテスト問題ページのURLの最終\"_\"以降の文字列 (例:a)

### testw

AtCoderから、指定したプログラムIDのサンプル入力例と出力例を取得し、
テストを実施します。
`./archive/<コンテスト名>/<プログラムID>.hs`ファイルの変更を検知する度に、繰り返しテストを再実施します。
[a-z|1-9]エンターキーを押すと問題が切り替わります。
エンターキーのみを押すと終了します。

```
$ ./atg test <プログラムID>
running 3 tests
入力例1---------> OK
入力例2---------> OK
入力例3---------> OK
INFO-testwp-1:app/Main.hsファイルの変更確認中。変更確認されればa問題サンプルのテストを行います。
[a-z|1-9]エンターキーを押すと問題が切り替わります。
エンターキーのみを押すと終了します。
```
### submit

指定した問題の解答として、AtCoderに`./app/Main.hs`の内容を提出します。
(`data.LanguageId`には、`Haskell(GHC8.8.3)`を表す`4027`が固定値で指定されます。)
```
$ ./atg submit <プログラムID>
```
### archive
`./app/Main.hs`を、`./Archive/<コンテストID>/<プログラムID>.hs`ファイルにコピーします。
コピー先にすでにファイルがある場合は、`./Archive/<コンテストID>/<プログラムID>.bk.hs`ファイルに
バックアップをさぃ瀬尾します。
(２回め以降はバックアップファイルを上書きします。)
```
$ atg archive <プログラムID>
```
### extract
`./Archive/<コンテストID>/<プログラムID>.hs`を、`./app/Main.hs`ファイルにコピーします。
コピー先にすでにファイルがある場合は、`./atg/Main.bk.hs`ファイルに
バックアップを作成します。
(２回め以降はバックアップファイルを上書きします。)

```
$ atg extract <プログラムID>
```



### test (atgsバイナリ時)

AtCoderから、指定したプログラムIDのサンプル入力例と出力例を取得し、
テストを実施します。
次のコマンドで検査対象コードのビルドを行います。
`stack build`
`stack run`を使ってテストを実行しています。

```
$ ./atgs test <プログラムID>
running 3 tests
入力例1---------> OK
入力例2---------> OK
入力例3---------> OK
```

### testw (atgsバイナリ時)

AtCoderから、指定したプログラムIDのサンプル入力例と出力例を取得し、
テストを実施します。
`./app/Main.hs`ファイルの変更を検知する度に、繰り返しテストを再実施します。
(ファイルの場所を変えている場合は、本機能を使わないでください。)
[a-z|1-9]エンターキーを押すと問題が切り替わります。
エンターキーのみを押すと終了します。

```
$ ./atgs test <プログラムID>
running 3 tests
入力例1---------> OK
入力例2---------> OK
入力例3---------> OK
INFO-testwp-1:app/Main.hsファイルの変更確認中。変更確認されればa問題サンプルのテストを行います。
[a-z|1-9]エンターキーを押すと問題が切り替わります。
エンターキーのみを押すと終了します。
```
---
## インストール方法
### 1.atgのビルド
githubからクローンして `stack build`でビルドしてください。
```
cd <作業フォルダ>
git clone https://github.com/kuo3/AtGorilla.git
cd AtGorilla
stack build
```
### 2.コンテスト用プロジェクトの作成
適当な場所にコンテスト用プロジェクトを作成してください。
```
cd <コンテスト用プロジェクト配置フォルダ>
mkdir <コンテスト用プロジェクト名>
```
### 3.ビルドしたatgのバイナリを配置
1.でビルドした`atg`バイナリファイルを2.で作成したコンテスト用プロジェクトに配置してください。
または、pathが通っている場所に`atg`のバイナリファイルを置いてください。
(`stack`ビルドプロジェクトの場合は、`atgs`バイナリファイルを配置してください。)

---
## バージョンごとの変更点
- [ChangeLog.md](ChangeLog.md)を参照してください。
## 確認されているバグ
- 一部文字化けがある。
## TODO
- スケルトンファイルをコピーする機能を追加したい。
- 各コマンドの表示を見直したい。(リッチにする。)
    - testおよびtestwコマンドのテスト失敗時の表示内容を見直し。
    - statusコマンドのテスト失敗時の表示内容を見直し。
    - 各コマンドの表示内容のうち、重要な箇所を色付けして情報を見やすくする。
      (テスト結果のAC、WAなど色で判別したい。)
- バイナリ提出機能を追加したい。
    (実現性不明。)
- インタラクティブモードを追加したい。
    (注視中のコンテスト名を表示し、提出ステータスを監視し、すべての問題に関して、一度でもAC済みか、または未ACかを表示。
     testwで作成コードを監視し、サンプルのテストを自動実行。
     コマンド追加入力で提出を実行。)
- 例外処理を全部見直す。
- パーサを全部見直す。
- テストを書く。(IOのテストどうやればいんだろうか。)
