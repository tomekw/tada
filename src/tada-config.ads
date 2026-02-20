with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Tada.Config is
   use Ada;

   Manifest_Error : exception;

   package String_Maps is new Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => String,
      Hash => Strings.Hash,
      Equivalent_Keys => "=");

   package Section_Maps is new Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => String_Maps.Map,
      Hash => Strings.Hash,
      Equivalent_Keys => "=",
      "=" => String_Maps."=");

   type Manifest is record
      Sections : Section_Maps.Map;
   end record;

   function Read (Manifest_Path : String) return Manifest;

   function Valid_Package_Name (Name : String) return Boolean;
end Tada.Config;
