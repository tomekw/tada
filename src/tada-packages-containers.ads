with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

package Tada.Packages.Containers is
   package Package_Info_Queues is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Packages.Package_Info,
                                                                          "=" => Packages."=");

   package Package_Info_Vectors is new Ada.Containers.Vectors (Index_Type => Positive,
                                                               Element_Type => Packages.Package_Info,
                                                               "=" => Packages."=");

   package Package_Info_Maps is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type => String,
                                                                           Element_Type => Packages.Package_Info,
                                                                           Hash => Ada.Strings.Hash,
                                                                           Equivalent_Keys => "=",
                                                                           "=" => Packages."=");
end Tada.Packages.Containers;
