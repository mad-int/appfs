-- pragma Optimize (time);
package body misc is

-- returns time in seconds
function GET_SEC (a, b : Time) return Duration is (b - a);

end misc;
