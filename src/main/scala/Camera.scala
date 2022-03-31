import scala.util.Random

class Camera (lookfrom: Vec,
              lookat: Vec,
              vup: Vec,
              vfov: Double,
              aperture: Double,
              focus_dist: Double,
              aspect_ratioi: Double = 16.0 / 9.0) {
  val theta: Double = math.toRadians(vfov)
  val h: Double = math.tan(theta/2)

  val aspect_ratio: Double = aspect_ratioi
  val viewport_height: Double = 2.0 * h
  val viewport_width: Double = aspect_ratio * viewport_height
  val focal_len = 1.0

  val w = Vec.unit_vec(lookfrom - lookat)
  val u = Vec.unit_vec(Vec.cross(vup, w))
  val v = Vec.cross(w, u)


  val origin: Vec = lookfrom
  val horizontal: Vec = u * focus_dist * viewport_width
  val vertical: Vec = v * focus_dist * viewport_height
  val llc: Vec = origin - horizontal/2 - vertical/2 - w * focus_dist
  val lens_rad = aperture/2

  def get_ray(s: Double, t: Double, rng: Random): Ray = {
    val rd = Vec.random_in_unit_disk(rng) * lens_rad
    val offset = u * rd.x + v * rd.y


    Ray(
      llc + (horizontal * s) +  vertical * t - origin,
      origin + offset
    )
  }
}

/*
            point3 lookfrom,
            point3 lookat,
            vec3   vup,
            double vfov, // vertical field-of-view in degrees
            double aspect_ratio,
            double aperture,
            double focus_dist
 */
