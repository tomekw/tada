with Ada.Strings.Unbounded;

with Tada.CL_Arguments;

package Tada.Commands is
   use Ada.Strings.Unbounded;

   Parse_Error : exception;
   Execute_Error : exception;

   type Command_Kind is (Build,
                         Test,
                         Run,
                         Init,
                         Clean,
                         Help,
                         Version);

   type Profile_Kind is (Debug,
                         Release);

   type Command (Kind : Command_Kind := Help) is record
      case Kind is
         when Build =>
            Build_Profile : Profile_Kind;
         when Init =>
            Package_Name : Unbounded_String;
            Package_Type : Package_Kind;
         when Run =>
            Run_Profile : Profile_Kind;
            Args : CL_Arguments.Argument_List.Vector;
         when Test =>
            Test_Profile : Profile_Kind;

         when Clean | Help | Version =>
            null;
      end case;
   end record;

   function Parse (Arguments : CL_Arguments.Argument_List.Vector) return Command;

   procedure Execute (Cmd : Command);
end Tada.Commands;
