import java.awt.Dimension
import org.friendlyvirus.mn.midi._
import swing._

/**
 * Created by IntelliJ IDEA.
 * User: miguelnegrao
 * Date: 11/05/06
 * Time: 11:22
 * To change this template use File | Settings | File Templates.
 */

// the bottom left encoder should be independent of the pages.

object BCRPagedKtlTestWithSingleControls extends SimpleSwingApplication {

  import BCR._

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
    (0 until 3).map( Kn(_,coll) )


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
    title = "Test BCRPagedKtl"
    contents = new FlowPanel(new Label("cc num:"),ccTxt,new Label("scene:"),sceneTxt,new Label("value:"),valueTxt)
    size = new Dimension(400,50)

  }
  val nonPaged:IndexedSeq[BCRControl] = IndexedSeq(Kn(2,7):BCRControl)
  val bcr = BCR.pagedKtl(nonPagedControls = nonPaged)
  bcr.DUMP_IN = false
  for(
    control <- allControls if (control != Pr(1,0) & control != Pr(1,1));
    scene <- (0 until bcr.numOfScenes)
  ) {
    val prsc = scene.toString
    val sceneint = scene
    bcr.addAction(scene, control,{ v:Double =>
      ccTxt.text = control.toString; sceneTxt.text = prsc; valueTxt.text =  "%3.2f" format v
      control match {
        case Kn(0,0) => bcr.sendCtl(sceneint, Kn(0,7), v)
        case Kn(2,0) => bcr.sendCtl( Kn(2,7), v)
      }

    })
  }

  bcr.addPageChangeAction(i => println("the new page is "+i))

}