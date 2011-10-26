package org.friendlyvirus.mn.midi

import collection.immutable.{Map,HashMap}

class MidiPagedKtl[Ctrl](ccNameMap: Map[Ctrl,CC], InDescr:String, OutDescr:String, val numOfScenes:Int = 32) extends
  AbstractMidiKtl[Ctrl](ccNameMap,InDescr, OutDescr) {

  val numOfScenesBitLength = BigInt(numOfScenes).bitLength
  var currentScene = 0

  def makeCCWithScene(cc:Int, scene:Int) = cc << numOfScenesBitLength | scene
  def makeCCWithCurrentScene(cc:Int) = cc << numOfScenesBitLength | currentScene
  def getSceneFromInt(cc:Int) = cc & numOfScenesBitLength

  var actionMap:Map[Int,Double => Unit] = new HashMap()
  var valuesListMap = Seq.tabulate(numOfScenes) { i =>
    nameMap collect {
      case (ctrl, ccint) => ( ccint << numOfScenesBitLength | i, 0.0)
    }
  }.reduceLeft(_++_)

  def onCCIn(cc: Int, v: Double) {
    val cc_scene = makeCCWithCurrentScene(cc)
    valuesListMap  += (cc_scene -> v)
    actionMap.get(cc_scene) map { _(v) }
  }

  def checkScene(scene:Int, func: => Unit) {
    if( scene < numOfScenes ) {
      func
    } else {
      println("Scene "+scene+" does not exist.")
    }
  }

  def addAction(scene: Int = 0, ctlKey: Ctrl, action: Double => Unit) {
    checkScene(scene,
      nameMap.get(ctlKey) map { cc =>
        actionMap += (makeCCWithScene(cc, scene) -> action)
      }
    )
  }

  def addActionAll(ctlKey: Ctrl, action: Double => Unit) {
    nameMap.get(ctlKey) map { cc =>
      for(i <- 0 until numOfScenes) {
        actionMap += (makeCCWithScene(cc, i) -> action)
      }
    }

  }

  def removeAction(scene: Int = 0, ctlKey: Ctrl) {
    checkScene(scene,
      nameMap.get(ctlKey) map { actionMap -= makeCCWithScene(_, scene) }
    )
  }

  def sendCtl(scene: Int = 0, ctlKey: Ctrl, v: Double) {

    checkScene(scene,
      nameMap.get(ctlKey) foreach {
        cc =>
          valuesListMap += (makeCCWithScene(cc, scene) -> v)
          if (scene == currentScene) {
            ccOut(cc, (v * 127).toInt)
          }
      }
    )
  }

  def changeScene(scene: Int) {

    if ( (scene >= 0) & (scene < numOfScenes) & (scene != currentScene) ) {
      currentScene = scene
      println("MIDIPagedKtl: changed to scene " + (currentScene+1) )
      ccOutMulti(nameMap collect {
        case (ctlr,cc) =>
          val keyWithScene = cc << numOfScenesBitLength | scene
          (cc, (valuesListMap(keyWithScene) * 127).toInt )
      })

    }

  }

  def nextScene() {
    val nextScene = (currentScene + 1).max(0).min(numOfScenes - 1)
    if (nextScene != currentScene) {
      changeScene(nextScene)
    }
  }

  def previousScene() {
    var previousScene = (currentScene - 1).max(0).min(numOfScenes - 1)
    if (previousScene != currentScene) {
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