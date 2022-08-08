import scala.annotation.nowarn
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Main._

@nowarn
class MainSpec extends AnyFreeSpec with Matchers {

  "Area of" - {
    "Circle(5.0,6.0,2.0) is 12.566370614359172" in {
      Circle(5.0,6.0,2.0).area shouldBe 12.566370614359172
    }

    "Rectangle(0.0,0.0,10.0,5.0) is 50.0" in {
      Rectangle(0.0,0.0,10.0,5.0).area shouldBe 50
    }

    "Square(0.0,0.0,5.0) is 25.0" in {
      Square(0.0,0.0,5.0).area shouldBe 25
    }

    "Triangle(-2.0,-2.0,1.0,2.0,3.0,-1.0) is 8.5" in {
      Triangle(-2.0,-2.0,1.0,2.0,3.0,-1.0).area shouldBe 8.5
    }

    "Sphere(5.0,6.0,0.0,5.0) is 314.1592653589793" in {
      Sphere(5.0,6.0,0.0,5.0).area shouldBe 314.1592653589793
    }

    "Cuboid(1.0,1.0,1.0,3.0,4.0,5.0) is 94.0" in {
      Cuboid(1.0,1.0,1.0,3.0,4.0,5.0).area shouldBe 94
    }

    "Cube(1.0,1.0,1.0,3.0) is 54.0" in {
      Cube(1.0,1.0,1.0,3.0).area shouldBe 54
    }

    "Triangle3d(-2.0,-2.0,-2.0,1.0,2.0,4.0,3.0,-1.0,-5.0) is 23.097618924902193" in {
      Triangle3d(-2.0,-2.0,-2.0,1.0,2.0,4.0,3.0,-1.0,-5.0,Point3d(0.0,0.0,0.0)).area shouldBe 23.097618924902193
    }
  }

  "Volume of" - {
    "Sphere(5.0,6.0,0.0,5.0) is 523.5987755982989" in {
      Sphere(5.0,6.0,0.0,5.0).volume shouldBe 523.5987755982989
    }

    "Cuboid(1.0,1.0,1.0,3.0,4.0,5.0) is 60.0" in {
      Cuboid(1.0,1.0,1.0,3.0,4.0,5.0).volume shouldBe 60
    }

    "Cube(1.0,1.0,1.0,3.0) is 27.0" in {
      Cube(1.0,1.0,1.0,3.0).volume shouldBe 27
    }
  }
}