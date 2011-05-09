package org.friendlyvirus.mn.midi

import collection.immutable.HashMap
import org.friendlyvirus.mn.midi.BCF._

/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/05
 * Time: 21:54
 * To change this template use File | Settings | File Templates.
 */

object BCF {

  sealed abstract class BCFControl
  case class GroupKn(group:Int,column:Int) extends BCFControl
  case class GroupTr(group:Int,column:Int) extends BCFControl
  case class Bt(row:Int,column:Int) extends BCFControl
  case class Sl(column:Int) extends BCFControl
  case class Pr(row:Int,column:Int) extends BCFControl

  def caseClassToString(BCFCtl: BCFControl) = {
    BCFCtl match {
      case GroupKn(group,coll) => "GrKn_"+group+"_"+coll
      case GroupTr(group,coll) => "GrTn_"+group+"_"+coll
      case Bt(row,coll) => "Bt_"+row+"_"+coll
      case Sl(coll) => "Sl_"+coll
      case Pr(row,coll) => "Pr_"+row+"_"+coll
    }
  }

  def getDefaults = (Seq.tabulate(8){ coll:Int =>

  //4 encoder groups
    (Seq.tabulate(4){ group:Int =>

    // top knob push mode
      HashMap(
        CC(0, 33 + 8*group + coll) -> ("GrTn_"+group+"_"+coll),
        // knobs (top row)
        CC(0,  1 + 8*group + coll) -> ("GrKn_"+group+"_"+coll)
      )
    }.reduceLeft( _ ++ _ )
    ++
    // buttons 1st row
    HashMap(
      CC(0,  65+coll) -> ("Bt_0_"+coll),
      // buttons 2nd row
      CC(0,  73+coll) -> ("Bt_1_"+coll),
      // knobs (lower 3 rows)
      CC(0,  81+coll) -> ("Sl_"+coll)
    ))

  }.reduceLeft( _ ++ _)
  ++
  // buttons (4 bottom right ones)
  HashMap(
    CC(0,89) -> "Pr_0_0",
    CC(0,90) -> "Pr_0_1",
    CC(0,91) -> "Pr_1_0",
    CC(0,92) -> "Pr_1_1"
  ))

}

class BCFKtl private (ccNameMap: Map[CC,String], IN_DESCR:String, OUT_DESCR:String) extends MidiKtl(ccNameMap, IN_DESCR, OUT_DESCR) {

  import BCF._

   def addAction (ctlKey:BCFControl, action:Double => Unit ) {
     super.addAction(BCF.caseClassToString(ctlKey),action)
  }

  def removeAction (ctlKey:BCFControl) {
    super.removeAction(BCF.caseClassToString(ctlKey))
  }

  def sendCtl(ctlKey: BCFControl, v: Double) {
    super.sendCtl(caseClassToString(ctlKey), v)
  }
}

object BCFKtl {

  def apply(IN_DESCR:String = "BCF2000 Porta 1", OUT_DESCR:String = "BCF2000 Porta 1") = new BCFKtl( BCF.getDefaults, IN_DESCR, OUT_DESCR)

}

class BCFPagedKtl private (ccNameMap: Map[CC,String], IN_DESCR:String, OUT_DESCR:String, numOfScenes:Int = 32) extends MidiPagedKtl(ccNameMap, IN_DESCR, OUT_DESCR, numOfScenes) {

  import BCF._

  def addAction (scene:Int, ctlKey:BCFControl, action:Double => Unit ) {
     super.addAction(scene, caseClassToString(ctlKey), action)
  }

  def addActionAll(ctlKey: BCFControl, action: Double => Unit) {
    super.addActionAll( caseClassToString(ctlKey), action )
  }

  def removeAction (scene:Int, ctlKey:BCFControl) {
    super.removeAction(scene, caseClassToString(ctlKey))
  }

  def sendCtl(scene:Int, ctlKey: BCFControl, v: Double) {
    super.sendCtl(scene, caseClassToString(ctlKey), v)
  }

}

object BCFPagedKtl {

  def apply(InDescr:String = "BCF2000 Porta 1", OutDescr:String = "BCF2000 Porta 1") = {
    val pagedKtl = new BCFPagedKtl(BCF.getDefaults, InDescr, OutDescr)
    pagedKtl.addActionAll( Pr(1,0), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,0), 0 ); pagedKtl.previousScene() } )
    pagedKtl.addActionAll( Pr(1,1), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,1), 0 ); pagedKtl.nextScene() } )
    pagedKtl
  }

}