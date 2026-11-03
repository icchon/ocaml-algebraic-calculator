#!/bin/sh
# src/（本書のコード）のモジュールを依存の順にバイトコードへコンパイルし、_build/ に置く。
# このあと、このフォルダで ocaml を起動すると、.ocamlinit がすべて読み込む。
set -e
cd "$(dirname "$0")"
rm -rf _build && mkdir _build && cp src/*.ml _build/
cd _build
ORDER=$(ocamldep -sort *.ml)
for f in $ORDER; do ocamlc -c "$f"; done
for f in $ORDER; do echo "#load \"_build/${f%.ml}.cmo\";;"; done > load.ml
echo "完了しました。このフォルダで ocaml を起動してください。"
