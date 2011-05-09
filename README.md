ScalaMidiKtl
=============

A set of classes for easy use of Midi controllers in scala.

MidiKtl -> A simple controller.

MidiPagedKtl -> A multi-page midi controller. Each page is a virtual scene, whose values are kept. This is recomended for use only with controllers that support incoming midi messages and can reassign the controls based on these messages, i.e. with motorized faders or encoders.

Controllers suported:

Behringer BCR 2000

* BCRKtl
* BCRPagedKtl

The controls are:

* GroupKn(group:Int,column:Int) - Upper row of encoders
* GroupTr(group:Int,column:Int) - Upper row of encoders when pressed
* Bt(row:Int,column:Int) - The two rows of buttons
* Kn(row:Int,column:Int) - The three rows of encoders
* Pr(row:Int,column:Int) - The 4 buttons at the bottom right
