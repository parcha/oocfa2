package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._

class ExampleContext extends FunSuite with JeevesLib {

  case class Dummy(ID: BigInt) extends JeevesRecord
  case class DummyContext(id: BigInt, viewer: Dummy) extends JeevesRecord

  val defaultVal = Dummy(-1)

  def mkElt(x: Dummy): Symbolic = {
    val l = mkLevel();
    policy(l, !(CONTEXT.viewer.ID === IntVal(1)), LOW);
    mkSensitive(l, x, defaultVal)
  }

  val x: Dummy = Dummy(1);
  val highCtxt: DummyContext = DummyContext(1, Dummy(1))
  val lowCtxt: DummyContext = DummyContext(0, Dummy(0))
  val x_s: Symbolic = mkElt(x);
  val y: Dummy = Dummy(2);
  val y_s: Symbolic = mkElt(y); 
 
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)

  test ("comparing two symbolic fields") {
    expect(true) { concretize[Boolean](lowCtxt, x_s === y_s) }
    expect(true) { concretize[Boolean](lowCtxt, x_s.ID === y_s.ID) }
    expect(false) { concretize[Boolean](highCtxt, x_s === y_s) }
    expect(false) { concretize[Boolean](highCtxt, x_s.ID === y_s.ID) }
  }

  test ("context viewer values") {
    expect(1) { concretize[BigInt](highCtxt, CONTEXT.viewer.ID) }
    expect(0) { concretize[BigInt](lowCtxt, CONTEXT.viewer.ID) }
    expect(true) {
      concretize[Boolean](highCtxt, CONTEXT.viewer.ID === IntVal(1))
    }
    expect(false) {
      concretize[Boolean](highCtxt, CONTEXT.viewer.ID === IntVal(0))
    }
  }

  // TODO: Both of these tests seem to fail...
  test ("formula with context id and object field - false") {
    expect(0) { concretize[BigInt](lowCtxt, CONTEXT.id) }
    expect(-1) { concretize[BigInt](lowCtxt, x_s.ID) }
    expect(false) {
      concretize[Boolean](lowCtxt, CONTEXT.id === x_s.ID)
    }
  }

  test ("formula with context viewer and object field - true") {
    expect(true) {
      concretize[Boolean](highCtxt, CONTEXT.viewer.ID === x_s.ID)
    }
  }

  test ("formula with context viewer and object field - false") {
    expect(0) { concretize[BigInt](lowCtxt, CONTEXT.viewer.ID) }
    expect(-1) { concretize[BigInt](lowCtxt, x_s.ID) }
    expect(false) {
      concretize[Boolean](lowCtxt, CONTEXT.viewer.ID === x_s.ID)
    }
  }

  test ("high confidentiality context") {
    expect (x) { concretize(highCtxt, x_s) }
  }

  test ("low confidentiality context") {
    expect (defaultVal) { concretize(NULL, x_s) }
  }

  test ("context field") {
    expect (x.ID) { concretize[BigInt](highCtxt, x_s.ID) }
    expect (true) { concretize(highCtxt, x_s.ID === IntVal(x.ID)) }
  }

  /* Lists. */
  test ("low confidentiality context - list") {
    expect(true) { concretize(NULL, s.has(defaultVal)) }
  }

  test ("high confidentiality context - list") {
    expect(true) { concretize[Boolean](highCtxt, s.has(Dummy(1))) }
  }

  /*
  test ("hasFormula") {
    expect(true) {
      concretize[Boolean](highCtxt, s.hasFormula(x =>
        x.ID === CONTEXT.viewer.ID))
    }
    expect(false) {
      concretize[Boolean](lowCtxt, s.hasFormula(x =>
        x.ID === CONTEXT.viewer.ID))
    }
  }
  */
}
