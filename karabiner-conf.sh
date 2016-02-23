#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set repeat.initial_wait 500
/bin/echo -n .
$cli set option.emacsmode_controlV 1
/bin/echo -n .
$cli set option.emacsmode_controlD_nomodifiers 1
/bin/echo -n .
$cli set option.emacsmode_controlPNBF_ex 1
/bin/echo -n .
$cli set option.emacsmode_controlY 1
/bin/echo -n .
$cli set option.emacsmode_optionV 1
/bin/echo -n .
$cli set option.emacsmode_controlAE_alternative 1
/bin/echo -n .
$cli set option.emacsmode_controlW 1
/bin/echo -n .
$cli set repeat.wait 33
/bin/echo -n .
$cli set option.emacsmode_controlH 1
/bin/echo -n .
$cli set option.emacsmode_controlK 1
/bin/echo -n .
$cli set remap.jis_shiftSpace2toggle_kana_eisuu_ex_emacs 1
/bin/echo -n .
/bin/echo
