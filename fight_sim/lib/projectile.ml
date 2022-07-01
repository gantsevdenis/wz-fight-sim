(* calculates ballistic trajectory

  case MM_INDIRECT:         // Ballistic trajectory.
  {
    Vector3i delta = psProj->dst - psProj->src;
    delta.z = (psProj->vZ - (timeSoFar * ACC_GRAVITY / (GAME_TICKS_PER_SEC * 2))) * timeSoFar / GAME_TICKS_PER_SEC; // '2' because we reach our highest point in the mid of flight, when "vZ is 0".
    int targetDistance = std::max(iHypot(delta.xy()), 1);
    currentDistance = timeSoFar * psProj->vXY / GAME_TICKS_PER_SEC;
    psProj->pos = psProj->src + delta * currentDistance / targetDistance;
    psProj->pos.z = psProj->src.z + delta.z;  // Use raw z value.
    psProj->rot.pitch = iAtan2(psProj->vZ - (timeSoFar * ACC_GRAVITY / GAME_TICKS_PER_SEC), psProj->vXY);
    break;
  }
    *)

(* How long an object burns for after leaving a fire*)
let burn_time = 10000

(* How much damage per second an object takes when it is burning *)
let burn_damage = 15

(* Least percentage of damage an object takes when burning *)
let burn_min_damage = Pct.of_int 30

(* Downward force against projectiles *)
let acc_gravity = 1000
let proj_InFlightFunc () = ()
