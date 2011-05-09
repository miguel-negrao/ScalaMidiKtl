/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/05
 * Time: 20:32
 * To change this template use File | Settings | File Templates.
 */

package org.friendlyvirus.mn.midi

import javax.sound.midi.{MidiMessage, MidiSystem, ShortMessage, Receiver}

case class CC( channel:Int, num:Int ) {

  def encodeInt = channel << 8 | num

}

abstract class AbstractMidiKtl(val ccNameMap: Map[CC,String], val IN_DESCR:String, val OUT_DESCR:String) {

  var DUMP_IN    = false
  var DUMP_OUT   = false
  val nameMap = ccNameMap collect { case (cc,string) => (cc.encodeInt,string) }

  protected def inform( what: String ) {
    println( "Midi : " + what )
  }

  protected val sync = new AnyRef
  protected var out: Option[Receiver] = None
  protected val outMsg = new ShortMessage()

  def onCCIn( cc:Int, v: Double)

  protected def getCC(key:String) = nameMap.collect{ case (a,b) => (b,a) }.get(key)

  def init() {
    try {
      val infos   = MidiSystem.getMidiDeviceInfo
      val inDevO  = infos.filter( _.getDescription == IN_DESCR  ).map( MidiSystem.getMidiDevice( _ )).find( _.getMaxTransmitters != 0 )
      val outDevO = infos.filter( _.getDescription == OUT_DESCR ).map( MidiSystem.getMidiDevice( _ )).find( _.getMaxReceivers != 0 )

      (inDevO, outDevO) match {
        case (Some( inDev ), Some( outDev )) =>
          inDev.open()
          outDev.open()
          val t = inDev.getTransmitter
          t.setReceiver( new Receiver {
            def close() { inform( "Input closed" )}
            def send( m: MidiMessage, time: Long ) {
              m match {
                case sm: ShortMessage if( sm.getCommand == ShortMessage.CONTROL_CHANGE ) =>
                  val ch   = sm.getChannel
                  val num  = sm.getData1
                  val v    = sm.getData2

                  if( DUMP_IN ) inform( "cc in,  ch " + ch + ", num " + num + ", v " + v )
                  onCCIn( ch << 8 | num, v.toDouble / 127 )

                case _ =>
              }
            }
          })
          sync.synchronized { out = Some(outDev.getReceiver) }
          nameMap foreach {
            case (key, value) => ccOut(key, 0.toInt)
          }


        case _ =>
          if( inDevO.isEmpty )  inform( "No input device '" +  IN_DESCR +  "' found!" )
          if( outDevO.isEmpty ) inform( "No output device '" + OUT_DESCR + "' found!" )
      }
    } catch {
      case e =>
        inform( "Error initializing MIDI: ")
        e.printStackTrace()
    }
  }

  def ccOut ( cc:Int, v: Int ) {

    sync.synchronized {
      out foreach {
        val num = cc & 127
        val ch = cc >> 8
        if( DUMP_OUT ) inform( "cc out, ch " + ch + ", num " + num + ", v " + v )
        outMsg.setMessage( ShortMessage.CONTROL_CHANGE, ch, num, v )
        _.send( outMsg, -1 )
      }
    }

  }

  def ccOutMulti ( values:Map[Int,Int] ) {

    sync.synchronized {
      out foreach { out =>
        values foreach {
          case (cc,v) =>
            val num = cc & 127
            val ch = cc >> 8
            if( DUMP_OUT ) inform( "cc out, ch " + ch + ", num " + num + ", v " + v )
            outMsg.setMessage( ShortMessage.CONTROL_CHANGE, ch, num, v )
            out.send( outMsg, -1 )
        }
      }
    }

  }

}