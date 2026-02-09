with Tada.CL_Arguments;
with Ada.Strings.Unbounded;

package Tada.Commands is
   use Ada.Strings.Unbounded;

   type Command_Kind is (Build,
                         Clean,
                         Help,
                         Invalid_Command,
                         Invalid_Profile);

   type Profile_Kind is (Debug,
                         Release);

   type Command (Kind : Command_Kind := Help) is record
      case Kind is
         when Build =>
            Profile : Profile_Kind;
         when Clean | Help =>
            null;
         when Invalid_Command | Invalid_Profile =>
            Unknown_Name : Unbounded_String;
      end case;
   end record;

   function From (Arguments : CL_Arguments.Argument_List.Vector) return Command;

   procedure Execute (Cmd : Command);
end Tada.Commands;
