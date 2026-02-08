with Ada.Containers.Indefinite_Vectors;

package Tada.CL_Arguments is
   use Ada;

   package Argument_List is new Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => String);

   function Consume return Argument_List.Vector;
end Tada.CL_Arguments;
