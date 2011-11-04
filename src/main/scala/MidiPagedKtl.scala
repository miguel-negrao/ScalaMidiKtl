package org.friendlyvirus.mn.midi

import collection.immutable.{Map,HashMap}

class MidiPagedKtl[Ctrl](ccNameMap: Map[Ctrl,CC], InDescr:String, OutDescr:String, val numOfScenes:Int = 32,
  val nonPagedControls:IndexedSeq[Ctrl] = IndexedSeq.empty[Ctrl]) extends
  AbstractMidiKtl[Ctrl](ccNameMap,InDescr, OutDescr) {

  private val numOfScenesBitLength = BigInt(numOfScenes).bitLength
  private var _currentScene = 0

  def currentScene = _currentScene

  private def makeCCWithScene(cc:Int, scene:Int) = cc << numOfScenesBitLength | scene
  private def makeCCWithCurrentScene(cc:Int) = cc << numOfScenesBitLength | _currentScene
  private def getSceneFromInt(cc:Int) = cc & numOfScenesBitLength

  private val nonPagedControlsCCs = nonPagedControls map ( ccNameMap(_).encodeInt )
  private var actionMap:Map[Int,Double => Unit] = new HashMap()
  private var pageChangeFunc = Option.empty[Int=>Unit]
  private var valuesListMap = Seq.tabulate(numOfScenes) { i =>
    nameMap collect {
      case (ctrl, ccint) if ccIsPaged(ccint) => ( ccint << numOfScenesBitLength | i, 0.0)
    }
  }.reduceLeft(_++_)

  private var singleActionMap = Map.empty[Int,Double => Unit]
  private var valuesMap = nameMap collect {
    case (ctrl, ccint) if ccIsSingle(ccint) => (ccint, 0.0)
  }

  private def ccIsSingle(cc:Int) = nonPagedControlsCCs.contains(cc)
  private def ccIsPaged(cc:Int) = !nonPagedControlsCCs.contains(cc)

  def onCCIn(cc: Int, v: Double) {
    if( ccIsSingle(cc) ) {
      valuesMap += (cc -> v)
      singleActionMap.get(cc) map { _(v) }
    } else {
      val cc_scene = makeCCWithCurrentScene(cc)
      valuesListMap  += (cc_scene -> v)
      actionMap.get(cc_scene) map { _(v) }
    }
  }

  private def checkScene(scene:Int, func: => Unit) {
    if( scene < numOfScenes ) {
      func
    } else {
      println("Scene "+scene+" does not exist.")
    }
  }

  def addAction(scene: Int = 0, ctlKey: Ctrl, action: Double => Unit) {
    nameMap.get(ctlKey) map { cc =>
      if( ccIsPaged(cc) ) {
        checkScene(scene, actionMap += (makeCCWithScene(cc, scene) -> action) )
      }
    }
  }

  def addAction(ctlKey: Ctrl, action: Double => Unit) {
    nameMap.get(ctlKey) map { cc =>
      if( ccIsSingle(cc) ) {
        singleActionMap += (cc -> action)
       }
    }
   }

  def addActionAll(ctlKey: Ctrl, action: Double => Unit) {
    nameMap.get(ctlKey) map { cc =>
      if( ccIsPaged(cc) ) {
        for(i <- 0 until numOfScenes) {
          actionMap += (makeCCWithScene(cc, i) -> action)
        }
      } else {
        singleActionMap += (cc -> action)
      }
    }
  }

  def addPageChangeAction(func:Int=>Unit) {
    pageChangeFunc = Some(func)
  }

  def removePageChangeAction{
      pageChangeFunc = None
  }

  def removeAction(scene: Int = 0, ctlKey: Ctrl) {
    nameMap.get(ctlKey) map { cc =>
      if( ccIsPaged(cc) ) {
        checkScene(scene, actionMap -= makeCCWithScene(cc, scene) )
      }
    }
  }

  def removeAction(ctlKey: Ctrl) {
    nameMap.get(ctlKey) map { cc =>
     if( ccIsSingle(cc) ) {
       singleActionMap -= cc
     }
    }
  }

  def sendCtl(scene: Int = 0, ctlKey: Ctrl, v: Double) {
    nameMap.get(ctlKey) foreach { cc =>
      if( ccIsPaged(cc) ) {
        checkScene(scene,
          {
            valuesListMap += (makeCCWithScene(cc, scene) -> v)
            if (scene == _currentScene) {
              ccOut(cc, (v * 127).toInt)
            }
          }
        )
      }
    }
  }

  def sendCtl(ctlKey: Ctrl, v: Double) {
    nameMap.get(ctlKey) foreach { cc =>
      if( ccIsSingle(cc) ) {
        valuesMap += (cc -> v)
        ccOut(cc, (v * 127).toInt)
      }
    }
  }

  def changeScene(scene: Int) {

    if ( (scene >= 0) & (scene < numOfScenes) & (scene != _currentScene) ) {
      _currentScene = scene
      if( DUMP_OUT ) { println("MIDIPagedKtl: changed to scene " + (_currentScene+1) ) }
      ccOutMulti(nameMap collect {
        case (ctlr,cc) if ccIsPaged(cc) => (cc, (valuesListMap(makeCCWithScene(cc,scene)) * 127).toInt )
      })
      pageChangeFunc map ( _(scene) )

    }

  }

  def nextScene() {
    val nextScene = (_currentScene + 1).max(0).min(numOfScenes - 1)
    if (nextScene != _currentScene) {
      changeScene(nextScene)
    }
  }

  def previousScene() {
    var previousScene = (_currentScene - 1).max(0).min(numOfScenes - 1)
    if (previousScene != _currentScene) {
      changeScene(previousScene)
    }
  }

  def clear() {
    actionMap = new HashMap
  }
}

object MidiPagedKtl {

  def apply[Ctlr](nameMap: Map[Ctlr,CC], IN_DESCR:String, OUT_DESCR:String, numOfScenes:Int = 32) =
    new MidiPagedKtl(nameMap, IN_DESCR, OUT_DESCR, numOfScenes)

}