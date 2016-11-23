with Ada.Text_IO;
use  Ada.Text_IO;

procedure Main is 
   type Ptr is access Integer;
   X : Ptr := new Integer;
   function F return Ptr is
   begin
      return X;
   end;
   procedure Print_Int(X : integer) is
   begin
      Put(Character'Val(X+64));
      New_Line;
   end;
begin
        Print_Int(X.all);
        F.all := 3;
        Print_Int(F.all);
end;
