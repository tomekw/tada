with Ada.Containers.Indefinite_Vectors;

--  Command line argument consumption.
--  Wraps Ada.Command_Line into an indefinite vector
--  of strings.
package Tada.CL_Arguments is
   use Ada;

   --  Positional string vector for command line arguments.
   package Argument_List is new Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => String);

   --  Consume all command line arguments into a vector.
   function Consume return Argument_List.Vector;
end Tada.CL_Arguments;
