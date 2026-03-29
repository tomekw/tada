with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Holders;

package Tada is
   use Ada;

   Version : constant String := "0.10.0";

   Package_Index_Url : constant String :=
     "https://raw.githubusercontent.com/tadapm/tada-packages/refs/heads/main/index";

   type Package_Kind is (Exe, Lib);

   package String_Holders is new Ada.Containers.Indefinite_Holders (String);

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
end Tada;
