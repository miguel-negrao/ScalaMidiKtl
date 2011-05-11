/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/05
 * Time: 17:38
 * To change this template use File | Settings | File Templates.
 */

package org.friendlyvirus.mn.midi

import collection.immutable.HashMap

class MidiKtl[Ctrl](ccNameMap: Map[Ctrl,CC], InDescr:String, OutDescr:String) extends AbstractMidiKtl[Ctrl](ccNameMap, InDescr:String, OutDescr:String) {

  var actionMap:Map[Int,Double => Unit] = new HashMap

  def onCCIn(cc:Int, v:Double) { actionMap.get(cc).map( _(v) ) }

  def addAction (key:Ctrl, action:Double => Unit ) {
    nameMap.get(key) map { actionMap +=  _ -> action } orElse { error("no int for string: "+key) }
  }

  def removeAction (ctlKey:Ctrl) { nameMap.get(ctlKey) map { actionMap -= _ } }

  def sendCtl (ctlKey:Ctrl, v:Double){ nameMap.get(ctlKey) map { ccOut(_, (v*127).toInt ) } }
}

object MidiKtl {

  def apply[Ctlr](nameMap: Map[Ctlr,CC], InDescr:String, OutDescr:String) =
    new MidiKtl(nameMap, InDescr, OutDescr)

}
