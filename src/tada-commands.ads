with Tada.CL_Arguments;
with Tada.Results;

package Tada.Commands is
   type Command_Kind is (Build,
                         Test,
                         Clean,
                         Help);

   type Profile_Kind is (Debug,
                         Release);

   type Command (Kind : Command_Kind := Help) is record
      case Kind is
         when Build | Test =>
            Profile : Profile_Kind;
         when Clean | Help =>
            null;
      end case;
   end record;

   package Command_Results is new Results (Command);

   function Parse (Arguments : CL_Arguments.Argument_List.Vector)
     return Command_Results.Result;

   procedure Execute (Cmd : Command);
end Tada.Commands;
