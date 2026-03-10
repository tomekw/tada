with Ada.Environment_Variables;

with GNAT.OS_Lib;

with Tada.CL_Arguments;
with Tada.Environments;

package body Tada.Runners is
   package OS renames GNAT.OS_Lib;

   function Spawn (Executable : String; Arguments : CL_Arguments.Argument_List.Vector) return Boolean is
      use type OS.String_Access;

      Env : constant Environments.Environment := Environments.Init;

      System_PATH : constant String := Environment_Variables.Value ("PATH");
      PATH_Separator : constant Character := (if Environments.Is_Windows then ';' else ':');

      Arguments_Count : constant Natural := Natural (Arguments.Length);
      Args : OS.Argument_List (1 .. Arguments_Count);

      Result : Boolean := False;
   begin
      for I in 1 .. Arguments_Count loop
         Args (I) := new String'(Arguments (I));
      end loop;

      Environment_Variables.Set ("PATH", Env.GNAT_Path & PATH_Separator & Env.GPRBuild_Path & PATH_Separator & System_PATH);

      OS.Spawn (Environments.Exec_Path (Executable), Args, Result);

      Environment_Variables.Set ("PATH", System_PATH);

      for Arg of Args loop
         OS.Free (Arg);
      end loop;

      return Result;
   exception
      when Environments.Environment_Error =>
         Environment_Variables.Set ("PATH", System_PATH);

         for Arg of Args loop
            OS.Free (Arg);
         end loop;

         raise;
      when others =>
         Environment_Variables.Set ("PATH", System_PATH);

         for Arg of Args loop
            OS.Free (Arg);
         end loop;

         return False;
   end Spawn;

   function Run_CURL (Source : String; Target : String) return Boolean is
   begin
      return Spawn ("curl", ["-fsSL", "-o", Target, Source]);
   end Run_CURL;

   function Run_GPRBuild (Project : String; Profile : String) return Boolean is
   begin
      return Spawn ("gprbuild", ["-P", Project & ".gpr", "-XBUILD_PROFILE=" & Profile, "-p"]);
   end Run_GPRBuild;

   function Run_Tar (Source : String; Target : String) return Boolean is
   begin
      return Spawn ("tar", ["xzf", Source, "-C", Target]);
   end Run_Tar;
end Tada.Runners;
