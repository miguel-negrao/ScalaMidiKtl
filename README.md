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


Behringer BCF 2000

* BCFKtl
* BCFPagedKtl

The controls are:

* GroupKn(group:Int,column:Int) - Upper row of encoders
* GroupTr(group:Int,column:Int) - Upper row of encoders when pressed
* Bt(row:Int,column:Int) - The two rows of buttons
* Sl(column:Int) - The slider
* Pr(row:Int,column:Int) - The 4 buttons at the bottom right

Example

    val bcr = new BCRKtl("BCR2000 Port 1","BCR2000 Port1")
    bcr.init
    //add action for lower left knob
    bcr.addAction(Kn(2,0), { v:Double => println(v) })
    
    val bcr = new BCRPagedKtl("BCR2000 Port 1","BCR2000 Port1")
    bcr.init
    //add action for lower left knob on (virtual) scene 0
    bcr.addAction(0,Kn(2,0), { v:Double => println(v) })
    //to change scene use the two lower right buttons.