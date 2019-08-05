import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

-- A convenient alias so we don't have to indicate the fields all the
-- time.
let vec (x, y, z) : vec3 = {x,y,z}

type ray = {origin: vec3, direction: vec3}

let point_at_parameter (r: ray) (t: f32) =
  vec3.(r.origin + scale t r.direction)

let reflect (v: vec3) (n: vec3) : vec3 =
  v vec3.- (2 * vec3.dot v n `vec3.scale` n)

type refraction = #no_refract | #refract vec3

let refract (v: vec3) (n: vec3) (ni_over_nt: f32) : refraction =
  let uv = vec3.normalise v
  let dt = vec3.dot uv n
  let discriminant = 1 - ni_over_nt*ni_over_nt*(1-dt*dt)
  in if discriminant > 0
     then #refract ((ni_over_nt `vec3.scale` (uv vec3.- (dt `vec3.scale` n)))
                      vec3.- (f32.sqrt discriminant `vec3.scale` n))
     else #no_refract

let schlick (cosine: f32) (ref_idx: f32) =
  let r0 = (1-ref_idx) / (1+ref_idx)
  let r0 = r0*r0
  in r0 + (1-r0)*(1-cosine)**5

import "lib/github.com/diku-dk/cpprandom/random"

module rng = pcg32
module dist = uniform_real_distribution f32 rng

let rand : rng.rng -> (rng.rng, f32) = dist.rand (0,1)

let random_in_unit_sphere rng =
  let new rng = let (rng, x) = rand rng
                let (rng, y) = rand rng
                let (rng, z) = rand rng
                in (rng, vec(x,y,z))
  let outside_sphere = vec3.quadrance >-> (>=1)
  in iterate_while ((.2) >-> outside_sphere) ((.1) >-> new) (new rng)

type camera = { origin: vec3
              , lower_left_corner: vec3
              , horizontal: vec3
              , vertical: vec3
              , u: vec3, v: vec3, w: vec3
              , lens_radius: f32}

let camera (lookfrom: vec3) (lookat: vec3) (vup: vec3) (vfov: f32) (aspect: f32)
           (aperture: f32) (focus_dist: f32) : camera =
  let theta = vfov * f32.pi / 180
  let half_height = f32.tan (theta / 2)
  let half_width = aspect * half_height
  let origin = lookfrom
  let w = vec3.normalise (lookfrom vec3.- lookat)
  let u = vec3.normalise (vec3.cross vup w)
  let v = vec3.cross w u
  in { lower_left_corner = origin vec3.-
                           (half_width * focus_dist `vec3.scale` u) vec3.-
                           (half_height * focus_dist `vec3.scale` v) vec3.-
                           (focus_dist `vec3.scale` w)
     , horizontal = (2*half_width*focus_dist) `vec3.scale` u
     , vertical = (2*half_height*focus_dist) `vec3.scale` v
     , origin, u, v, w
     , lens_radius = aperture / 2}

let get_ray (c: camera) (s: f32) (t: f32) (rng: rng.rng) : (rng.rng, ray) =
  let {origin, lower_left_corner, horizontal, vertical, u, v, w=_, lens_radius} = c
  let (rng, p) = random_in_unit_sphere rng
  let rd = lens_radius `vec3.scale` p
  let offset = vec3.((rd.x `scale` u) + (rd.y `scale` v))
  in (rng,
      { origin = offset vec3.+ c.origin
      , direction = vec3.(lower_left_corner +
                          (s `scale` horizontal) +
                          (t `scale` vertical) -
                          origin -
                         offset)})

type material = #lambertian {albedo: vec3}
              | #metal {albedo: vec3, fuzz: f32}
              | #dielectric {ref_idx: f32}

type hit_info = {t: f32, p: vec3, normal: vec3, material: material}

type hit = #no_hit | #hit hit_info

type sphere = {center: vec3, radius: f32, material: material}

let sphere_hit {center, radius, material} (r: ray) (t_min: f32) (t_max: f32) : hit =
  let oc = vec3.(r.origin - center)
  let a = vec3.dot r.direction r.direction
  let b = vec3.dot oc r.direction
  let c = vec3.dot oc oc - radius*radius
  let discriminant = b*b - a*c
  let try_hit (temp: f32) =
    if temp < t_max && temp > t_min
    then (#hit { t = temp
               , p = point_at_parameter r temp
               , normal = (1/radius) `vec3.scale` (point_at_parameter r temp vec3.- center)
               , material
               })
    else #no_hit
  in if discriminant <= 0
     then #no_hit
     else match try_hit ((-b - f32.sqrt(b*b-a*c))/a)
          case #hit h -> #hit h
          case #no_hit -> try_hit ((-b + f32.sqrt(b*b-a*c))/a)

type obj = #sphere sphere

let hit [n] (objs: [n]obj) (r: ray) (t_min: f32) (t_max: f32) : hit =
  (loop (hit, closest_so_far) = (#no_hit, t_max) for obj in objs do
   let hit' = match obj
              case #sphere s -> sphere_hit s r t_min closest_so_far
   in match hit'
      case #no_hit -> (hit, closest_so_far)
      case #hit h -> (#hit h, h.t)).1

type scatter = #scatter {attenuation: vec3, scattered: ray}
             | #no_scatter

let scattering (r: ray) (h: hit_info) (rng: rng.rng) : (rng.rng, scatter) =
  match h.material

  case #lambertian {albedo} ->
    let (rng, bounce) = random_in_unit_sphere rng
    let target = vec3.(h.p + h.normal + bounce)
    in (rng, #scatter {attenuation=albedo,
                       scattered={origin = h.p, direction = target vec3.- h.p}})

  case #metal {albedo, fuzz} ->
    let reflected = reflect (vec3.normalise r.direction) h.normal
    let (rng, bounce) = random_in_unit_sphere rng
    let scattered = {origin = h.p, direction = reflected vec3.+ (fuzz `vec3.scale` bounce)}
    in if vec3.dot scattered.direction h.normal > 0
       then (rng, #scatter {attenuation=albedo,
                            scattered})
       else (rng, #no_scatter)

  case #dielectric {ref_idx} ->
    let reflected = reflect r.direction h.normal
    let attenuation = vec(1, 1, 1)
    let (outward_normal, ni_over_nt, cosine) =
      if vec3.dot r.direction h.normal > 0
      then (vec3.map f32.negate h.normal,
            ref_idx,
            ref_idx * vec3.dot r.direction h.normal / vec3.norm r.direction)
      else (h.normal,
            1/ref_idx,
            -vec3.dot r.direction h.normal / vec3.norm r.direction)
    in match refract r.direction outward_normal ni_over_nt
       case #refract refracted ->
         let reflect_prob = schlick cosine ref_idx
         let (rng, x) = rand rng
         let direction = if x < reflect_prob then reflected else refracted
         in (rng, #scatter {attenuation, scattered={origin=h.p, direction}})
       case #no_refract ->
         (rng, #scatter {attenuation, scattered={origin=h.p, direction=reflected}})

let color (objs: []obj) (r: ray) (rng: rng.rng) : (rng.rng, vec3) =
  let max_depth = 50i32
  let ((rng, _), (_, color)) =
    loop ((rng, r), (depth, color)) = ((rng, r), (0, vec(1,1,1))) while depth < max_depth
    do match hit objs r 0.00001 f32.highest
       case #hit h ->
         (match scattering r h rng
          case (rng, #scatter {attenuation, scattered}) ->
            ((rng, scattered), (depth+1, attenuation vec3.* color))
          case (rng, #no_scatter) ->
            ((rng, r), (max_depth, vec(0,0,0))))
       case #no_hit ->
         let unit_direction = vec3.normalise r.direction
         let t = 0.5 * (unit_direction.y + 1)
         let color' = color vec3.*
                      (((1-t) `vec3.scale` vec(1, 1, 1)) vec3.+
                       (t `vec3.scale` vec(0.5, 0.7, 1.0)))
         in ((rng, r), (max_depth, color'))
  in (rng, color)

let random_object_at (a: f32) (b: f32) (rng: rng.rng) : (rng.rng, obj) =
  let (rng, center) = let (rng, xd) = rand rng
                      let (rng, yd) = rand rng
                      in (rng, vec(a+0.9*xd, 0.2, b+0.9*yd))
  let randp rng = let (rng, x) = rand rng
                  let (rng, y) = rand rng
                  in (rng, x * y)
  let (rng, x) = randp rng
  let (rng, y) = randp rng
  let (rng, z) = randp rng
  let albedo = vec(x,y,z)
  let (rng, fuzz) = rand rng
  let (rng, choose_mat) = rand rng
  let material =
    if choose_mat < 0.8
    then #lambertian {albedo}
    else if choose_mat < 0.95
    then #metal {albedo, fuzz}
    else #dielectric {ref_idx=1.5}
  in (rng,
      #sphere {center, radius=0.2, material})

-- From http://stackoverflow.com/a/12996028
let hash (x: i32): i32 =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x

import "lib/github.com/athas/matte/colour"

let random_world (seed: i32) =
  let mk_obj a b = let rng = rng.rng_from_seed [seed, a ^ b]
                   in random_object_at (r32 a) (r32 b) rng
  let (rngs, objs) = map (\a -> map (mk_obj a) (-11..<11)) (-11..<11)
                     |> map unzip |> unzip
  let rng = rng.join_rng (flatten rngs)

  let fixed_objs = [ #sphere {center=vec(0,-1000,0), radius=1000, material=#lambertian {albedo=vec(0.5,0.5,0.5)}}
                   , #sphere {center=vec(0,1,0), radius=1, material=#dielectric {ref_idx=1.5}}
                   , #sphere {center=vec(-4,1,0), radius=1, material=#lambertian {albedo=vec(0.4,0.2,0.1)}}
                   , #sphere {center=vec(4,1,0), radius=1, material=#metal {albedo=vec(0.6,0.6,0.5), fuzz=0}}
                   ]

  let world = flatten objs ++ fixed_objs

  in (rng, world)

let render (nx: i32) (ny: i32) (ns: i32) (world: []obj) (cam: camera) (rngs: [ny][nx]rng.rng) =
  let sample j i (rng, acc) = let (rng, ud) = rand rng
                              let (rng, vd) = rand rng
                              let u = (r32(i) + ud) / r32(nx)
                              let v = (r32(j) + vd) / r32(ny)
                              let (rng, r) = get_ray cam u v rng
                              let (rng, col) = color world r rng
                              in (rng, acc vec3.+ col)
  let pixel j i = let rng = rngs[j,i]
                  let (rng, col) = iterate ns (sample j i) (rng, vec(0,0,0))
                  let col = ((1/r32 ns) `vec3.scale` col) |> vec3.map f32.sqrt
                  in (rng, argb.from_rgba col.x col.y col.z 0)
  in tabulate_2d ny nx pixel |> reverse

let main (nx: i32) (ny: i32) (ns: i32): [ny][nx]argb.colour =
  let lookfrom = vec(13,2,3)
  let lookat = vec(0,0,0)
  let dist_to_focus = 10
  let aperture = 0.1
  let cam = camera lookfrom lookat (vec(0,1,0)) 20 (r32 nx / r32 ny)
                   aperture dist_to_focus
  let (rng, world) = random_world (nx ^ ny ^ ns)
  let rngs = rng.split_rng (nx*ny) rng |> unflatten ny nx
  in render nx ny ns world cam rngs |> map (map (.2))
