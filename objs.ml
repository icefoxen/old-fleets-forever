(* There are three general types of objects: Features, Weapons and Ships.
 * Features collide with weapons and ships.
 * Weapons collide with weapons and ships.
 * Ships collide with weapons and features.
 * Effects collide with nothing.
 *)
open Vector2d
open Drawing



(* Features are ambient things lying around fairly passively, that you can
 * run into and so on.
 *)
class virtual feature =
object (self)
   method calc = self
   method collideWeapon (other:weapon) = Some self
   method collideShip (other:ship) = Some self
   method draw (old:feature) (now:int) = [(0, (fun () -> ()))]
end

(* Weapons are things that blow up ships. *)
and weapon =
object (self)
end

(* Ships are things that fly around and fire weapons at other ships. *)
and ship =
object (self)
end


(* Features. *)
class rock loc vel =
object (self)
   inherit feature
   val loc = loc
   val vel = vel
   method calc = {< loc = loc ^+ vel >}
   method collideWeapon other = None
   method collideShip other = None
   method draw old now =
      [(-100, withMatrix (translated loc (scaled 0.02 triangle)))]
end

class cloud loc radius =
object (self)
   inherit feature
   val loc = loc
   val rad = radius
   method calc = self
   method collideWeapon other = None
   method collideShip other = None
   method draw old now =
      [(-100, withMatrix (translated loc (scaled 0.02 triangle)))]
end

class debris loc vel =
object (self)
   inherit feature
   val loc = loc
   val vel = vel
   method calc = {< loc = loc ^+ vel >}
   method collideWeapon other = None
   method collideShip other = None
   method draw old now =
      [(-100, withMatrix (translated loc (scaled 0.02 triangle)))]
end
