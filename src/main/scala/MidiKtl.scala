/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/05
 * Time: 17:38
 * To change this template use File | Settings | File Templates.
 */

package org.friendlyvirus.mn.midi

import collection.immutable.HashMap

class MidiKtl(ccNameMap: Map[CC,String], IN_DESCR:String, OUT_DESCR:String) extends AbstractMidiKtl(ccNameMap, IN_DESCR:String, OUT_DESCR:String) {

  var actionMap:Map[Int,Double => Unit] = new HashMap

  def onCCIn(cc:Int, v:Double) { println( actionMap.get(cc) ); actionMap.get(cc).map( _(v) ) }

  def addAction (key:String, action:Double => Unit ) {
    nameMap.collect{ case (a,b) => (b,a) }.getOrElse(key,println("no int for string: "+key))
    nameMap.collect{ case (a,b) => (b,a) }.get(key) map { actionMap +=  _ -> action }
  }

  def removeAction (ctlKey:String) { nameMap.collect{ case (a,b) => (b,a) }.get(ctlKey) map { actionMap -= _ } }

  def sendCtl(ctlKey:String, v:Double){ nameMap.collect{ case (a,b) => (b,a) }.get(ctlKey) map { ccOut(_, (v*127).toInt ) } }
}
