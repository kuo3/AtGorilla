# Changelog for AtGorilla

## Unreleased
### Fixed
### Added
### Changed
- `test`、`testw`実行時に、取得するテストデータの格納場所を変更しました。

## [0.0.3.0] - 2021-1-24
### Fixed
- `test`、`testw`実行時に、サンプル入力例の内容が一部正しく入力データとして使用されない問題を修正しました。
### Changed
- `test`、`testw`実行時に、WAになった場合、入力データ、期待値データ、出力データを表示するように変更しました。
- 
## [0.0.2.0] - 2021-1-23
### Changed
- ビルドに`stack build`を用いているプロジェクトのみ対応していましたが、
  ビルドに`ghc`を用いるプロジェクトにも対応しました。

## [0.0.1.0] - 2021-1-22
### Added
- ccコマンドを追加。
- loginコマンドを追加。
- logoutコマンドを追加。
- testコマンドを追加。
- testwコマンドを追加。
- submitコマンドを追加。
- statusコマンドを追加。
- archiveコマンドを追加。
- extractコマンドを追加。