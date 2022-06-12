import scala.util.Random

class Vec(xi: Double = 0, yi: Double = 0, zi: Double = 0){
  var x: Double = xi
  var y: Double = yi
  var z: Double = zi

  def length_squared(): Double = x*x + y*y + z*z
  def length(): Double = math.sqrt(length_squared())
  def toArray: Array[Double] = Array(x, y, z)

  def -[T](that: T ): Vec = {
    that match {
      case that: Double => new Vec(x-that, y-that, z-that)
      case that: Int => new Vec(x-that, y-that, z-that)
      case that: Vec => new Vec(x-that.x, y-that.y, z-that.z)
    }
  }


  def +[T](that: T ): Vec = {
    that match {
      case that: Double => new Vec(x+that, y+that, z+that)
      case that: Int => new Vec(x+that, y+that, z+that)
      case that: Vec => new Vec(x+that.x, y+that.y, z+that.z)
    }
  }



  def /(that: Vec): Vec = {
    new Vec(
      x / that.x,
      y / that.y,
      z / that.z
    )
  }

  def /(that: Int ): Vec = {
    new Vec(x/that, y/that, z/that)
  }
  def /(that: Double ): Vec = {
    new Vec(x/that, y/that, z/that)
  }

  def dot(that: Vec): Double = {
    x*that.x + y*that.y + z*that.z
  }

  def *[T](that: T ): Vec = {
    that match {
      case that: Double => new Vec(x*that, y*that, z*that)
      case that: Int => new Vec(x*that, y*that, z*that)
      case that: Vec => new Vec(x*that.x, y*that.y, z*that.z)
      case _: Any => throw new IllegalArgumentException
    }
  }

  def unary_- = new Vec(-x, -y, -z)

  def near_zero(): Boolean = {
    val s = 1e-8
    (math.abs(x) < s) && (math.abs(y) < s) && (math.abs(z) < s)
  }

  //  def -(that: Vec): Vec = {
  //    x -= that.x
  //    y -= that.y
  //    z -= that.z
  //    this
  //  }
  //  def *(that: Vec): Vec = {
  //    new Vec(x*that.x, y*that.y, z*that.z)
  //  }
  //  def *(that: Double): Vec = {
  //    new Vec(x*that, y*that, z*that)
  //  }


}

object Vec{
    def random(rng: Random): Vec ={
      new Vec(rng.nextDouble(), rng.nextDouble(), rng.nextDouble())
    }

  def unit_vec(v: Vec): Vec = v / v.length()
  def random(minv: Double, maxv: Double, rng: Random): Vec= {
    new Vec(rng.between(minv, maxv), rng.between(minv, maxv),rng.between(minv, maxv))
  }

  def random_in_unit_sphere(rng: Random): Vec ={
    var out = Vec.random(-1, 1, rng)
    while
      out = Vec.random(-1, 1, rng)
      out.length_squared() >= 1
    do ()
    out
  }

  def random_in_unit_disk(rng: Random): Vec ={
    var out = new Vec(rng.between(-1, 1), rng.between(-1, 1), 0)
    while
      out = new Vec(rng.between(-1, 1), rng.between(-1, 1), 0)
      out.length_squared() >= 1
    do ()
    out
  }

  def random_unit_vector(rng: Random): Vec = unit_vec(random_in_unit_sphere(rng))

  def reflect(v: Vec, n: Vec): Vec = v - n * v.dot(n) * 2

  def refract(uv: Vec, n: Vec, etai_over_etat: Double): Vec = {
    val cos_theta = math.min(n.dot(-uv), 1)
    val r_out_perp = (uv + n*cos_theta) * etai_over_etat
    val r_out_parallel = n * -math.sqrt(math.abs(1 - r_out_perp.length_squared()))
    r_out_parallel + r_out_perp
  }
  def cross(u: Vec, v: Vec): Vec = {
    new Vec(u.y * v.z - u.z * v.y,
            u.z * v.x - u.x * v.z,
            u.x * v.y - u.y * v.x)
  }
}
