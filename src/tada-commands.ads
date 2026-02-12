with Ada.Strings.Unbounded;

with Tada.CL_Arguments;
with Tada.Results;

--  Command parsing and execution.
--  Translates command line arguments into a typed Command
--  value and dispatches execution.
package Tada.Commands is
   use Ada.Strings.Unbounded;

   --  Available CLI commands.
   type Command_Kind is (Build,
                         Test,
                         Run,
                         Init,
                         Clean,
                         Help);

   --  Build optimization profile.
   type Profile_Kind is (Debug,
                         Release);

   --  Project scaffold kind for the init command.
   type Project_Kind is (Exe,
                         Lib);

   --  Parsed command with kind-specific fields.
   type Command (Kind : Command_Kind := Help) is record
      case Kind is
         when Build | Test =>
            Profile : Profile_Kind;
         when Run =>
            Run_Profile : Profile_Kind;
            Args : CL_Arguments.Argument_List.Vector;
         when Init =>
            Project_Name : Unbounded_String;
            Project_Type : Project_Kind;
         when Clean | Help =>
            null;
      end case;
   end record;

   --  Result type specialized for Command values.
   package Command_Results is new Results (Command);

   --  Parse command line arguments into a Command.
   function Parse (Arguments : CL_Arguments.Argument_List.Vector)
     return Command_Results.Result;

   --  Execute a parsed command.
   procedure Execute (Cmd : Command);
end Tada.Commands;
