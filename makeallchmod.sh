#!/bin/sh
#以下のディレクトリ内の全てのシェルスクリプトにchmodを適用する。
#シェルスクリプトのディレクトリ
SHDIR=~/workspace/github/compiler/*.sh
#コマンド
chmod 755 $SHDIR

