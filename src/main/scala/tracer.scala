import java.io._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

trait Material {
  val Albedo: Vec
  // (scattered, attentuation)
  def scatter(r: Ray, rec: HitRec, rng: Random): Option[(Ray, Vec)]
}

case class Lambertian(albedoi: Vec) extends Material {
  override val Albedo: Vec = albedoi
  override def scatter(r: Ray, rec: HitRec, rng: Random): Option[(Ray, Vec)] = {
    var scatter_dir = rec.normal + Vec.random_unit_vector(rng)
    if (scatter_dir.near_zero())
      scatter_dir = rec.normal

    Some(Ray(scatter_dir, rec.p), Albedo)
  }
}

case class Metal(albedoi: Vec, fuzz: Double) extends Material {
  override val Albedo: Vec = albedoi

  override def scatter(r: Ray, rec: HitRec, rng: Random): Option[(Ray, Vec)] = {
    val reflected = Vec.reflect(Vec.unit_vec(r.dir), rec.normal)
    val scattered = Ray(reflected + Vec.random_in_unit_sphere(rng)*fuzz, rec.p)

    if (scattered.dir.dot(rec.normal) > 0)
      Some(scattered, Albedo)
    else
      None
  }
}

case class Dielectric(ir: Double) extends Material {
  val Albedo: Vec = new Vec(1,1,1)

  override def scatter(r: Ray, rec: HitRec, rng: Random): Option[(Ray, Vec)] = {
    val refraction_ratio = if (rec.front_face) 1/ir else ir
    val unit_dir = Vec.unit_vec(r.dir)
    val cos_theta = math.min(rec.normal.dot(-unit_dir), 1)
    val sin_theta = math.sqrt(1 - cos_theta*cos_theta)

    val cant_refct = refraction_ratio * sin_theta > 1
    val dir = if (cant_refct || reflectance(cos_theta, refraction_ratio) > rng.nextDouble())
                Vec.reflect(unit_dir, rec.normal)
              else
                Vec.refract(unit_dir, rec.normal, refraction_ratio)

    Some(Ray(dir, rec.p) , Albedo)
  }

  private def reflectance(cosine: Double, ref_idx: Double): Double = {
    var r0 = (1-ref_idx)/(1+ref_idx)
    r0 = r0 * r0
    r0 + (1-r0)*math.pow((1-cosine), 5)
  }
}


trait Hittable extends Product{
  def hit(r: Ray, t_min: Double, t_max: Double): Option[HitRec]
}

case class Cube() extends Hittable {
  override def hit(r: Ray, t_min: Double, t_max: Double): Option[HitRec] = ???
}

case class Sphere(center: Vec, radius: Double, mat: Material) extends Hittable {
  override def hit(r: Ray, t_min: Double, t_max: Double): Option[HitRec] ={
    val oc = r.origin - center
    val a = r.dir.length_squared()
    val half_b =  oc.dot(r.dir)
    val c = oc.length_squared() - radius * radius
    val discriminant = half_b*half_b - a*c

    if (discriminant < 0) return None

    val sqrtd = math.sqrt(discriminant)
    var root = (-half_b - sqrtd)/a

    if (root < t_min || t_max < root){
      root = (-half_b + sqrtd)/a
      if (root < t_min || t_max < root) return None
    }

    val p = r.at(root)
    val outward_normal = (p - center) / radius
    val rec = new HitRec(p, root, mat)
    rec.set_face_normal(r, outward_normal)
    Some(rec)
  }
}


class HitRec(pi: Vec, ti: Double, mati: Material){
  val p: Vec = pi
  val t: Double = ti
  var normal: Vec = new Vec()
  var front_face: Boolean = true
  var mat: Material = mati

  def set_face_normal(r: Ray, outward_normal: Vec): Unit = {
    front_face = r.dir.dot(outward_normal) < 0
    normal = if (front_face) outward_normal else -outward_normal
  }

}

case class Ray(dir: Vec, origin: Vec){
  def at(t: Double): Vec = origin + (dir*t)
}



class RayTracer(cami: Camera, worldi: Array[Hittable], samples_pp: Int, max_dep: Int, iw: Int = 400, ih: Int = 300){
  val cam: Camera = cami
  val world: Array[Hittable] = worldi
  val samples_per_pixel: Int = samples_pp
  val max_depth: Int = max_dep

  val img_w: Int = iw
  val img_h: Int = (img_w / cam.aspect_ratio).toInt


  val rend_start: Long = System.nanoTime()
  val thds_idx = new AtomicInteger(0)



  // creates the threads that render the image for each vector
  // returns an array of rgb values
  def render(): Array[Array[Int]] ={
    val pixels = for {
      j <- (0 until img_h).reverse
      i <- 0 until img_w
    } yield render_vec(i, j)
    pixels.map(v => Await.result(v, Duration.Inf)).toArray
  }


  // creates a thread safe rng, samples a pixel the given amount of times, then turns it into an rgb value
  // rng instance is passed to other functions for threadsafeness
  def render_vec(i: Double, j: Double): Future[Array[Int]] = Future{
    val idx = thds_idx.getAndIncrement()
    val rng = new Random(rend_start + idx)

    var color = new Vec()
    for (_ <- 0 to samples_per_pixel){
      val r = cam.get_ray((i.toDouble + rng.nextDouble())/(img_w-1),
                          (j.toDouble + rng.nextDouble())/(img_h-1),
                          rng)
      color += get_color(r, max_depth, rng)
    }
    convert_to_rgb(color)
  }

  // checks if a ray hit an object in the world
  def hit_world(r: Ray, t_min: Double, t_max: Double): Option[HitRec] = {
    var rec: Option[HitRec] = None
    var closest = t_max

    for (obj <- world){
      obj.hit(r, t_min, closest) match {
        case Some(arec) =>
          closest = arec.t
          rec = Some(arec)
        case None =>
      }
    }
    rec
  }




  def get_color(r: Ray, depth: Int, rng: Random): Vec = {
    if (depth <= 0) return new Vec()

    hit_world(r, 0.001, Double.MaxValue) match {
      case Some(rec) => // the ray hit an object so scatter it depending on the material
        rec.mat.scatter(r, rec, rng) match {
          case Some((scattered, attentuation)) => attentuation * get_color(scattered, depth - 1, rng)
          case None => new Vec()
        }

      case None => // the ray didnt hit an object so show the background color
        val unitv = Vec.unit_vec(r.dir)
        val t = 0.5*(unitv.y + 1)
        (new Vec(1,1,1) * (1-t)) +  (new Vec(0.5,0.7,1) * t)
    }
  }

  // clamp a val, curryed for piping
  def clamp(minval: Double, maxval: Double)(x: Double): Double = {
    if (x < minval) return minval
    if (x > maxval) return maxval
    x
  }


  // convert the double values to ints between 0 and 255
  def convert_to_rgb(vec: Vec): Array[Int] = {
    vec.toArray.map(_.pipe((_: Double) /samples_per_pixel)
                      .pipe(math.sqrt)
                      .pipe(clamp(0.0, 0.999))
                      .pipe((_: Double) *255.999)
                      .pipe((_: Double) toInt))
  }


}

object tracer {
  @main def main(): Unit ={
    def random_scene(): Array[Hittable] = {
      val rng = new Random(System.nanoTime())
      var objs  = collection.mutable.Buffer[Hittable]()
      val mat_ground = Lambertian(new Vec(0.5,0.5,0.5))
      objs += Sphere( new Vec(0,-1000,0), 1000, mat_ground)

      val area_size = 6

      for (a <- -area_size until area_size){
        for (b <- -area_size until area_size){
          val choose_mat = rng.nextDouble()
          val center = new Vec(a + 0.9*rng.nextDouble(), 0.2, b + 0.9 * rng.nextDouble())

          if ((center - new Vec(4,0.2,0)).length() > 0.9){
            if (choose_mat < 0.8){
              val albedo = Vec.random(rng) * Vec.random(rng)
              val mat = Lambertian(albedo)
              objs += Sphere( center, 0.2, mat)
            } else if (choose_mat < 0.95) {
              val albedo = Vec.random(0.5,1, rng)
              val fuzz = rng.between(0, 0.5)
              val mat = Metal(albedo, fuzz)
              objs += Sphere( center, 0.2, mat)
            } else {
              val mat = Dielectric(1.5)
              objs += Sphere( center, 0.2, mat)
            }

          }
        }
      }

      val mat1 = Dielectric(1.5)
      val mat2 = Lambertian(new Vec(0.4,0.2,0.1))
      val mat3 = Metal(new Vec(0.7,0.6,0.5), 0)

      objs += Sphere( new Vec(0,1,0), 1, mat1)
      objs += Sphere( new Vec(-4,1,0), 1, mat2)
      objs += Sphere( new Vec(4,1,0), 1, mat3)

      objs.toArray
    }

//    val mat_ground = Lambertian(new Vec(0.8,0.8,0.0))
//    val mat_center = Lambertian(new Vec(0.7,0.3,0.3))
//    val mat_left = Dielectric(1.5)
//    val mat_right = Metal(new Vec(0.8,0.6,0.2), 0.1)
//
//
//
//
//    objs += Sphere( new Vec(0,-100.5,-1), 100, mat_ground)
//    objs += Sphere( new Vec(0,0,-1), 0.5, mat_center)
//    objs += Sphere( new Vec(-1,0,-1), 0.5, mat_left)
//    objs += Sphere( new Vec(-1,0,-1), -0.4, mat_left)
//    objs += Sphere( new Vec(1,0,-1), 0.5, mat_right)


    var world: Array[Hittable] = random_scene()

    val lookfrom =new Vec(13,2,3)
    val lookat =new Vec(0,0,0)

    val cam = new Camera(
      lookfrom,
      lookat,
      new Vec(0,1,0),
      20,
      0.1,
      10,
      3.0/2.0
    )

//    val raytracer = new RayTracer(cam, world, 50, 15, 1920)
    val raytracer = new RayTracer(cam, world, 30, 10, 400)


    val rendfile = new PrintWriter(new File("render_sc.ppm"))
    rendfile.write(f"P3\n${raytracer.img_w} ${raytracer.img_h} \n255\n")

    raytracer.render().foreach(a => {
      val pix = a.mkString(" ")
      rendfile.write(f"$pix\n")
    })

    rendfile.close()
  }



}
