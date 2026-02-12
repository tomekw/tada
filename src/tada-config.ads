with Ada.Strings.Unbounded;

with Tada.Results;

--  Manifest parsing and project name validation.
--  Reads tada.toml files and validates that project names
--  are legal Ada identifiers.
package Tada.Config is
   use Ada.Strings.Unbounded;

   --  Result type for project name values.
   package Project_Name_Results is new Results (Unbounded_String);

   --  Result type for project version values.
   package Project_Version_Results is new Results (Unbounded_String);

   --  Parsed manifest contents. Each field carries its own
   --  Ok/Error status since both keys are optional in the file.
   type Manifest is record
      Name : Project_Name_Results.Result;
      Version : Project_Version_Results.Result;
   end record;

   --  Result type for the overall Parse operation.
   package Manifest_Results is new Results (Manifest);

   --  Check whether Name is a valid Ada identifier suitable
   --  for use as a project name: starts with a letter,
   --  contains only letters, digits, and single underscores,
   --  does not end with an underscore, and is not a
   --  reserved word.
   function Valid_Project_Name (Name : String) return Boolean;

   --  Parse a tada.toml manifest file at the given path.
   --  Returns Ok with the parsed Manifest on success, or
   --  Error with a diagnostic message on failure (file not
   --  found, malformed values, unknown keys).
   function Parse (Manifest_Path : String) return Manifest_Results.Result;
end Tada.Config;
