(ns game.ns-without-deps
  (:require
    game.music
    game.serialization
    game.monster.monsters
    game.item.instance-impl
    game.player.skill.learnable-impl
    game.components.body-render))

; other solution: (initialize (require 'the-impl-ns)) dort wo benutzt.
; doesnt work with compile, so here.


