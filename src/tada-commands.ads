with Tada.CL_Arguments;

package Tada.Commands is
   type Command_Kind is (Build,
                         Clean,
                         Help);

   type Profile_Kind is (Debug,
                         Release);

   type Command (Kind : Command_Kind := Help) is record
      case Kind is
         when Build =>
            Profile : Profile_Kind;
         when Clean | Help =>
            null;
      end case;
   end record;

   function From (Arguments : CL_Arguments.Argument_List.Vector) return Command;

   procedure Execute (Cmd : Command);
end Tada.Commands;
