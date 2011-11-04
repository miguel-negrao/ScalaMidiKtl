package org.friendlyvirus.mn.midi

import collection.immutable.HashMap

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

  def getDefaults: Map[BCRControl,CC] = {
    val res = Map.newBuilder[BCRControl, CC]
    for( coll <- 0 until  8) {


      //4 encoder groups
      for( group <- 0 until 4) {

        // top knob push mode
        res += GroupTr(group,coll) -> CC(0, 33 + 8*group + coll)
        // knobs (top row)
        res += GroupKn(group,coll) -> CC(0,  1 + 8*group + coll)
      }

      // buttons 1st row
      res += Bt(0,coll) -> CC(0,  65+coll)
      // buttons 2nd row
      res += Bt(1,coll) -> CC(0,  73+coll)
      // knobs (lower 3 rows)
      res += Kn(0,coll) -> CC(0,  81+coll)
      res += Kn(1,coll) -> CC(0,  89+coll)
      res += Kn(2,+coll) ->CC(0,  97+coll)

    }
    // buttons (4 bottom right ones)
    res += Pr(0,0) -> CC(0,105)
    res += Pr(0,1) -> CC(0,106)
    res += Pr(1,0) -> CC(0,107)
    res += Pr(1,1) -> CC(0,108)
    res.result()
  }

  def getLowerColl(coll: Int):Seq[ BCRControl ] = Seq.tabulate(3){ Kn(_,coll) }
  def getColl(coll:Int):Seq[ BCRControl ] = (Seq.tabulate(4){ GroupKn(_:Int,coll) }) ++ getLowerColl(coll)


  def ktl(IN_DESCR:String = "BCR2000 Porta 1", OUT_DESCR:String = "BCR2000 Porta 1") = new MidiKtl[BCRControl]( getDefaults, IN_DESCR, OUT_DESCR)

  def pagedKtl(InDescr:String = "BCR2000 Porta 1", OutDescr:String = "BCR2000 Porta 1", numOfScenes:Int = 32,
    nonPagedControls:IndexedSeq[BCRControl] = IndexedSeq.empty[BCRControl]) = {
    val pagedKtl = new MidiPagedKtl[BCRControl](BCR.getDefaults, InDescr, OutDescr, numOfScenes, nonPagedControls)
    pagedKtl.addActionAll( Pr(1,0), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,0), 0 ); pagedKtl.previousScene() } )
    pagedKtl.addActionAll( Pr(1,1), { v:Double => pagedKtl.sendCtl( pagedKtl.currentScene, Pr(1,1), 0 ); pagedKtl.nextScene() } )
    pagedKtl
  }

}
