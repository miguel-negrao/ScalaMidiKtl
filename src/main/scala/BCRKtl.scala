package org.friendlyvirus.mn.midi

import collection.immutable.HashMap
import org.friendlyvirus.mn.midi.BCR._

/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/05
 * Time: 21:54
 * To change this template use File | Settings | File Templates.
 */

object BCR {

  sealed abstract class BCRControl
  case class GroupKn(group:Int,column:Int) extends BCRControl
  case class GroupTr(group:Int,column:Int) extends BCRControl
  case class Bt(row:Int,column:Int) extends BCRControl
  case class Kn(row:Int,column:Int) extends BCRControl
  case class Pr(row:Int,column:Int) extends BCRControl

  def caseClassToString(bcrCtl: BCRControl) = {
    bcrCtl match {
      case GroupKn(group,coll) => "GrKn_"+group+"_"+coll
      case GroupTr(group,coll) => "GrTn_"+group+"_"+coll
      case Bt(row,coll) => "Bt_"+row+"_"+coll
      case Kn(row,coll) => "Kn_"+row+"_"+coll
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
      CC(0,  81+coll) -> ("Kn_0_"+coll),
      CC(0,  89+coll) -> ("Kn_1_"+coll),
      CC(0,  97+coll) -> ("Kn_2_"+coll)
    ))

  }.reduceLeft( _ ++ _)
  ++
  // buttons (4 bottom right ones)
  HashMap(
    CC(0,105) -> "Pr_0_0",
    CC(0,106) -> "Pr_0_1",
    CC(0,107) -> "Pr_1_0",
    CC(0,108) -> "Pr_1_1"
  ))

}

class BCRKtl private (ccNameMap: Map[CC,String], IN_DESCR:String, OUT_DESCR:String) extends MidiKtl(ccNameMap, IN_DESCR, OUT_DESCR) {

  import BCR._

   def addAction (ctlKey:BCRControl, action:Double => Unit ) {
     super.addAction(BCR.caseClassToString(ctlKey),action)
  }

  def removeAction (ctlKey:BCRControl) {
    super.removeAction(BCR.caseClassToString(ctlKey))
  }

  def sendCtl(ctlKey: BCRControl, v: Double) {
    super.sendCtl(caseClassToString(ctlKey), v)
  }
}

object BCRKtl {

  def apply(IN_DESCR:String = "BCR2000 Porta 1", OUT_DESCR:String = "BCR2000 Porta 1") = new BCRKtl( BCR.getDefaults, IN_DESCR, OUT_DESCR)

}

class BCRPagedKtl private (ccNameMap: Map[CC,String], IN_DESCR:String, OUT_DESCR:String, numOfScenes:Int = 32) extends MidiPagedKtl(ccNameMap, IN_DESCR, OUT_DESCR, numOfScenes) {

  import BCR._

  def addAction (scene:Int, ctlKey:BCRControl, action:Double => Unit ) {
     super.addAction(scene, caseClassToString(ctlKey), action)
  }

  def addActionAll(ctlKey: BCRControl, action: Double => Unit) {
    super.addActionAll( caseClassToString(ctlKey), action )
  }

  def removeAction (scene:Int, ctlKey:BCRControl) {
    super.removeAction(scene, caseClassToString(ctlKey))
  }

  def sendCtl(scene:Int, ctlKey: BCRControl, v: Double) {
    super.sendCtl(scene, caseClassToString(ctlKey), v)
  }

}

object BCRPagedKtl {

  def apply(InDescr:String = "BCR2000 Porta 1", OutDescr:String = "BCR2000 Porta 1") = {
    val pagedKtl = new BCRPagedKtl(BCR.getDefaults, InDescr, OutDescr)
    pagedKtl.addActionAll( Pr(1,0), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,0), 0 ); pagedKtl.previousScene() } )
    pagedKtl.addActionAll( Pr(1,1), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,1), 0 ); pagedKtl.nextScene() } )
    pagedKtl
  }

}