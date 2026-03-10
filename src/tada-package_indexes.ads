with Ada.Containers.Vectors;

with Tada.Packages;

package Tada.Package_Indexes is
   Package_Index_Error : exception;

   type Package_Index_Entry is record
      Name : String_Holders.Holder;
      Version : String_Holders.Holder;
      Url : String_Holders.Holder;
      Checksum : String_Holders.Holder;
   end record;

   package Package_Index_Entry_Vectors is new Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Package_Index_Entry);

   type Package_Index is record
      Packages : Package_Index_Entry_Vectors.Vector;
   end record;

   function Read (Package_Index_Url : String) return Package_Index;

   function Find (Index : Package_Index; P : Packages.Package_Info) return Package_Index_Entry;

   procedure Download (P : Package_Index_Entry);

   procedure Verify_Checksum (P : Package_Index_Entry);

   procedure Extract (P : Package_Index_Entry);

   function Package_Tmp_Path (P : Package_Index_Entry) return String;

   procedure Cleanup;
end Tada.Package_Indexes;
