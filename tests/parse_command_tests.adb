with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Tada.Commands; use Tada.Commands;
with Test_Helpers; use Test_Helpers;

package body Parse_Command_Tests is

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Parse_Command");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Empty_Is_Help'Access,
         "Empty is help");
      Registration.Register_Routine
        (T, Test_Help_Command'Access,
         "Help command");
      Registration.Register_Routine
        (T, Test_Clean_Command'Access,
         "Clean command");
      Registration.Register_Routine
        (T, Test_Unknown_Command'Access,
         "Unknown command");
      Registration.Register_Routine
        (T, Test_Command_Case_Insensitive'Access,
         "Command case insensitive");
      Registration.Register_Routine
        (T, Test_Init_Missing_Name'Access,
         "Init missing name");
   end Register_Tests;

   procedure Test_Empty_Is_Help
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Arguments : CL.Argument_List.Vector;
      Cmd : constant Command :=
        Parse_Ok (Arguments, "empty");
   begin
      Assert (Cmd.Kind = Help,
              "empty: expected Help");
   end Test_Empty_Is_Help;

   procedure Test_Help_Command
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok (["help"], "help");
   begin
      Assert (Cmd.Kind = Help,
              "help: expected Help");
   end Test_Help_Command;

   procedure Test_Clean_Command
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok (["clean"], "clean");
   begin
      Assert (Cmd.Kind = Clean,
              "clean: expected Clean");
   end Test_Clean_Command;

   procedure Test_Unknown_Command
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["unknown"], "unknown command");
   end Test_Unknown_Command;

   procedure Test_Command_Case_Insensitive
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd_1 : constant Command :=
        Parse_Ok (["HELP"], "uppercase");
      Cmd_2 : constant Command :=
        Parse_Ok (["Build"], "mixed case");
   begin
      Assert (Cmd_1.Kind = Help,
              "uppercase: expected Help");
      Assert (Cmd_2.Kind = Build,
              "mixed case: expected Build");
   end Test_Command_Case_Insensitive;

   procedure Test_Init_Missing_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["init"], "init missing name");
   end Test_Init_Missing_Name;

end Parse_Command_Tests;
