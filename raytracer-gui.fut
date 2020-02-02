import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"

module raytracer = import "raytracer"
module vec3 = raytracer.vec3

type text_content = (f32, i32, i32)

let target_fps : f32 = 24

type~ sized_state [h] [w] =
  { world: []raytracer.obj
  , lookfrom: raytracer.vec3
  , lookat: raytracer.vec3
  , rngs: [h][w]raytracer.rng
  , samples: [h][w]i32
  , image: [h][w]argb.colour
  , fraction: f32
  , steps: i32
  , scanline: i32
  }

let shoot h w nss world cam rngs =
  raytracer.render 10 w h nss world cam rngs
  |> map unzip |> unzip

let step [h] [w] (td: f32) (s: sized_state [h] [w]) : sized_state [h] [w] =
  let fps = 1/td
  let fraction = (if fps > target_fps
                  then s.fraction * 1.1
                  else s.fraction * 0.9)
                 |> f32.max 0.01 |> f32.min 1
  let chunk_size = t32 (r32 h * fraction)
  let chunk_start = s.scanline
  let chunk_end = (chunk_start + chunk_size) % h

  let in_chunk j =
    if chunk_start < chunk_end
    then j >= chunk_start && j < chunk_end
    else j >= chunk_start || j < chunk_end

  let samples j _ = if in_chunk j then 1 else 0

  let dist_to_focus = 10
  let aperture = 0.1
  let cam = raytracer.camera
            s.lookfrom s.lookat (raytracer.vec(0,1,0)) 20
            (r32 w / r32 h) aperture dist_to_focus

  let nss = tabulate_2d h w samples
  let (rngs, image) = shoot h w nss s.world cam s.rngs
  let comb cur_ns cur new_ns new =
    if new_ns == 0
    then cur
    else argb.mix (r32 cur_ns) cur (r32 new_ns) new
  in s with rngs = rngs
       with image = map4 (map4 comb) s.samples s.image nss image
       with fraction = fraction
       with steps = s.steps + 1
       with samples = map2 (map2 (+)) s.samples nss
       with scanline = chunk_end

module lys : lys with text_content = text_content = {
  type~ state = sized_state [] []

  let init seed h w : state =
    let (rng, world) = raytracer.random_world (i32.u32 seed) 11
    let lookfrom = raytracer.vec(13,2,3)
    let lookat = raytracer.vec(0,0,0)
    let rngs = raytracer.rnge.split_rng (h*w) rng |> unflatten h w
    let (rngs, image) = (rngs, tabulate_2d h w (\_ _ -> argb.black))
    let samples = tabulate_2d h w (\_ _ -> 0)
    in {world, lookfrom, lookat, rngs, image,
        fraction = 0.1, steps = 0, samples, scanline = 0}

  let event [h] [w] (e: event) (s: sized_state [h] [w]) : state =
    match e
    case #step td ->
      step td s

    case #keydown {key} ->
      let ahead = vec3.normalise (s.lookat vec3.- s.lookfrom)
      in
      if key == SDLK_DOWN
      then init 123 h w
           with lookfrom = s.lookfrom vec3.+ ahead
           with world = s.world
      else if key == SDLK_UP
      then init 123 h w
           with lookfrom = s.lookfrom vec3.- ahead
           with world = s.world
      else s

    case _ -> s

  let resize h w (s: state) =
    init (u32.i32 s.steps) h w with lookfrom = s.lookfrom
                               with lookat = s.lookat

  let render (s: state) = s.image

  type text_content = (f32, i32, i32)
  let grab_mouse = false
  let text_format () = "Fraction: %f (%d pixels per frame)\nFPS: %d"
  let text_content [h] [w] (fps: f32) (s: sized_state [h] [w]) =
    (s.fraction,
     t32 (r32 (h * w) * s.fraction),
     t32 fps)
  let text_colour _ = argb.black
}
