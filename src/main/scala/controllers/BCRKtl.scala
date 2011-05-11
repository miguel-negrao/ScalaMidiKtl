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

  def getDefaults = (Seq.tabulate(8){ coll:Int =>

  //4 encoder groups
    (Seq.tabulate(4){ group:Int =>

    // top knob push mode
      HashMap(
        GroupTr(group,coll) -> CC(0, 33 + 8*group + coll),
        // knobs (top row)
        GroupKn(group,coll) -> CC(0,  1 + 8*group + coll)
      )
    }.reduceLeft( _ ++ _ )
    ++
    // buttons 1st row
    HashMap(
      Bt(0,coll) -> CC(0,  65+coll) ,
      // buttons 2nd row
      Bt(1,coll) -> CC(0,  73+coll),
      // knobs (lower 3 rows)
      Kn(0,coll) -> CC(0,  81+coll),
      Kn(1,coll) -> CC(0,  89+coll),
      Kn(2,+coll) ->CC(0,  97+coll)
    ))

  }.reduceLeft( _ ++ _)
  ++
  // buttons (4 bottom right ones)
  HashMap(
    Pr(0,0) -> CC(0,105),
    Pr(0,1) -> CC(0,106),
    Pr(1,0) -> CC(0,107),
    Pr(1,1) -> CC(0,108)
  ))

  def ktl(IN_DESCR:String = "BCR2000 Porta 1", OUT_DESCR:String = "BCR2000 Porta 1") = new MidiKtl[BCRControl]( getDefaults, IN_DESCR, OUT_DESCR)

  def pagedKtl(InDescr:String = "BCR2000 Porta 1", OutDescr:String = "BCR2000 Porta 1") = {
    val pagedKtl = new MidiPagedKtl[BCRControl](BCR.getDefaults, InDescr, OutDescr)
    pagedKtl.addActionAll( Pr(1,0), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,0), 0 ); pagedKtl.previousScene() } )
    pagedKtl.addActionAll( Pr(1,1), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,1), 0 ); pagedKtl.nextScene() } )
    pagedKtl
  }

}
