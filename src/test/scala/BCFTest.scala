import java.awt.{Dimension}
import org.friendlyvirus.mn.midi._
import BCF._
import swing._

/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/06
 * Time: 11:22
 * To change this template use File | Settings | File Templates.
 */

object BCFKtlTest extends SimpleSwingApplication {


  val ccTxt = new TextArea
  val valueTxt = new TextArea {
    minimumSize = new Dimension(40,100)
    maximumSize = new Dimension(40,100)
    size = new Dimension(40,100)
  }

  def top = new MainFrame {
    title = "Test BCFKtl"
    contents = new FlowPanel(new Label("cc num:"),ccTxt,new Label("value:"),valueTxt)
    size = new Dimension(400,50)

  }

  def allControls = ((Seq.tabulate(8){ coll:Int =>

    //4 encoder groups
      ((0 until 4).map({ group =>

      // top knob push mode
        List(
          GroupKn(group, coll),
          // knobs (top row)
          GroupTr(group, coll)
        )
      }).flatten
      ++
      // buttons 1st row
      (0 until 2).map( Bt(_,coll) )
      ++
      // knobs (lower 3 rows)
      List(Sl(coll))


    )}).flatten
    ++
    // buttons (4 bottom right ones)
    (for(
      i <- (0 until 2);
      j <- (0 until 2)
    ) yield { Pr(i,j) }))


  val bcf = BCF.ktl()
  bcf.DUMP_IN = false

  for( control <- allControls ) {
    bcf.addAction(control,{ v:Double =>
      ccTxt.text = control.toString; valueTxt.text =  "%3.2f" format v
      control match { case Sl(0) => bcf.sendCtl(Sl(7), v) }
    })
  }


}

object BCFPagedKtlTest extends SimpleSwingApplication {

  import BCF._

  def allControls = ((Seq.tabulate(8){ coll:Int =>

    //4 encoder groups
      ((0 until 4).map({ group =>

      // top knob push mode
        List(
          GroupKn(group, coll),
          // knobs (top row)
          GroupTr(group, coll)
        )
      }).flatten
      ++
      // buttons 1st row
      (0 until 2).map( Bt(_,coll) )
      ++
      // knobs (lower 3 rows)
      List( Sl(coll) )


    )}).flatten
    ++
    // buttons (4 bottom right ones)
    (for(
      i <- (0 until 2);
      j <- (0 until 2)
    ) yield { Pr(i,j) }))



  val ccTxt = new TextArea
  val sceneTxt = new TextArea
  val valueTxt = new TextArea {
    minimumSize = new Dimension(40,100)
    maximumSize = new Dimension(40,100)
    size = new Dimension(40,100)
  }

  def top = new MainFrame {
    title = "Test BCFPagedKtl"
    contents = new FlowPanel(new Label("cc num:"),ccTxt,new Label("scene:"),sceneTxt,new Label("value:"),valueTxt)
    size = new Dimension(400,50)

  }

  val bcf = BCF.pagedKtl()
  bcf.DUMP_IN = false
  for(
    control <- allControls if (control != Pr(1,0) & control != Pr(1,1));
    scene <- (0 until bcf.numOfScenes)
  ) {
    val prsc = scene.toString
    val sceneint = scene
    bcf.addAction(scene, control,{ v:Double =>
      ccTxt.text = control.toString; sceneTxt.text = prsc; valueTxt.text =  "%3.2f" format v
      control match { case Sl(0) => bcf.sendCtl(sceneint, Sl(7), v) }

    })
  }

}
