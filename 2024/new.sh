#!/bin/bash

d2=$(printf "%02d" $1)
name=dec$d2

stack new --no-init $name ./template/template.hsfiles

sed -i "s/$name/Dec $1/" $name/test/LibSpec.hs
find $name -name '*.hs' -not -path '*.stack-work*' -print0 | xargs -0 sed -i "s/Lib/Dec$d2/g"
find $name -name 'Lib*.hs' | xargs rename Lib Dec$d2
sed -i "s/00.txt/$d2.txt/" $name/test/Dec${d2}Spec.hs

yq -i '.packages += ["'$name'"]' stack.yaml
