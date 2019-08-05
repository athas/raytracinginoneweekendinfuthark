import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"

module raytracer = import "raytracer"

type text_content = i32

module lys : lys with text_content = text_content = {
  type state = { h: i32
               , w: i32
               , world: []raytracer.obj
               , cam: raytracer.camera
               , rngs: [][]raytracer.rng.rng
               , image: [][]argb.colour
               , steps: i32
               }

  let shoot h w rays world cam rngs =
    raytracer.render w h rays world cam rngs
    |> map unzip |> unzip

  let init seed h w : state =
    let (rng, world) = raytracer.random_world seed
    let lookfrom = raytracer.vec(13,2,3)
    let lookat = raytracer.vec(0,0,0)
    let dist_to_focus = 10
    let aperture = 0.1
    let cam = raytracer.camera lookfrom lookat (raytracer.vec(0,1,0)) 20 (r32 w / r32 h)
                               aperture dist_to_focus
    let rngs = raytracer.rng.split_rng (h*w) rng |> unflatten h w
    let (rngs, image) = (rngs, tabulate_2d h w (\_ _ -> argb.black))
    in {h, w, world, cam, rngs, image, steps = 0}

  let event (e: event) s : state =
    match e
    case #keydown {key} ->
      let rays = if key == SDLK_1 then 1
                 else if key == SDLK_2 then 2
                 else if key == SDLK_3 then 3
                 else if key == SDLK_4 then 4
                 else if key == SDLK_5 then 5
                 else if key == SDLK_6 then 6
                 else if key == SDLK_7 then 7
                 else if key == SDLK_8 then 8
                 else if key == SDLK_9 then 9
                 else 0
      let (rngs, image) = shoot s.h s.w rays s.world s.cam s.rngs
      let comb cur new = argb.mix (r32 s.steps) cur 1 new
      in s with rngs = rngs
           with image = map2 (map2 comb) s.image image
           with steps = s.steps + rays
    case _ -> s

  let resize h w s : state =
    let rngs = flatten s.rngs
               |> raytracer.rng.join_rng
               |> raytracer.rng.split_rng (h*w)
               |> unflatten h w
    in s with h = h
         with w = w
         with rngs = rngs
         with image = tabulate_2d h w (\_ _ -> argb.black)

  let render (s: state) = s.image

  type text_content = i32
  let grab_mouse = false
  let text_format = "Rays per pixel: %d"
  let text_content _ (s: state) = s.steps
  let text_colour _ = argb.white
}
