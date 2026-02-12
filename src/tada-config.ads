with Ada.Strings.Unbounded;

with Tada.Results;

package Tada.Config is
   use Ada.Strings.Unbounded;

   package Project_Name_Results is new Results (Unbounded_String);
   package Project_Version_Results is new Results (Unbounded_String);

   type Manifest is record
      Name : Project_Name_Results.Result;
      Version : Project_Version_Results.Result;
   end record;

   package Manifest_Results is new Results (Manifest);

   function Valid_Project_Name (Name : String) return Boolean;

   function Parse (Manifest_Path : String) return Manifest_Results.Result;
end Tada.Config;
