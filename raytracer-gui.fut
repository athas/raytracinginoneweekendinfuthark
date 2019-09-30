import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"

module raytracer = import "raytracer"
module vec3 = raytracer.vec3

type text_content = (f32, i32, i32)

let target_fps : f32 = 24

module lys : lys with text_content = text_content = {
  type state = { h: i32
               , w: i32
               , world: []raytracer.obj
               , lookfrom: raytracer.vec3
               , lookat: raytracer.vec3
               , rngs: [][]raytracer.rng
               , samples: [][]i32
               , image: [][]argb.colour
               , fraction: f32
               , steps: i32
               , scanline: i32
               }

  let shoot h w nss world cam rngs =
    raytracer.render 10 w h nss world cam rngs
    |> map unzip |> unzip

  let init seed h w : state =
    let (rng, world) = raytracer.random_world seed
    let lookfrom = raytracer.vec(13,2,3)
    let lookat = raytracer.vec(0,0,0)
    let rngs = raytracer.rnge.split_rng (h*w) rng |> unflatten h w
    let (rngs, image) = (rngs, tabulate_2d h w (\_ _ -> argb.black))
    let samples = tabulate_2d h w (\_ _ -> 0)
    in {h, w, world, lookfrom, lookat, rngs, image,
        fraction = 0.1, steps = 0, samples, scanline = 0}

  let event (e: event) s : state =
    match e
    case #step td ->
      let fps = 1/td
      let fraction = (if fps > target_fps
                      then s.fraction * 1.1
                      else s.fraction * 0.9)
                     |> f32.max 0.01 |> f32.min 1
      let chunk_size = t32 (r32 s.h * fraction)
      let chunk_start = s.scanline
      let chunk_end = (chunk_start + chunk_size) % s.h

      let in_chunk j =
        if chunk_start < chunk_end
        then j >= chunk_start && j < chunk_end
        else j >= chunk_start || j < chunk_end

      let samples j =
        replicate s.w (if in_chunk j then 1 else 0)

      let dist_to_focus = 10
      let aperture = 0.1
      let cam = raytracer.camera
                s.lookfrom s.lookat (raytracer.vec(0,1,0)) 20
                (r32 s.w / r32 s.h) aperture dist_to_focus

      let nss = tabulate s.h samples
      let (rngs, image) = shoot s.h s.w nss s.world cam s.rngs
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

    case #keydown {key} ->
      let ahead = vec3.normalise (s.lookat vec3.- s.lookfrom)
      in
      if key == SDLK_DOWN
      then init 123 s.h s.w
           with lookfrom = s.lookfrom vec3.+ ahead
           with world = s.world
      else if key == SDLK_UP
      then init 123 s.h s.w
           with lookfrom = s.lookfrom vec3.- ahead
           with world = s.world
      else s

    case _ -> s

  let resize h w (s: state) =
    init s.steps h w with lookfrom = s.lookfrom
                     with lookat = s.lookat

  let render (s: state) = s.image

  type text_content = (f32, i32, i32)
  let grab_mouse = false
  let text_format = "Fraction: %f (%d pixels per frame)\nFPS: %d"
  let text_content (fps: f32) (s: state) =
    (s.fraction,
     t32 (r32 (s.h * s.w) * s.fraction),
     t32 fps)
  let text_colour _ = argb.black
}
