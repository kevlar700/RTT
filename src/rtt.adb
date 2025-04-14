package body RTT is

   procedure Put
     (Value : Integer;
      Block : not null access Control_Block;
      Index : Index_Up_Max := 1)
   is
      Copy : aliased Integer := Value;
      subtype UInt8_Array is Elansys.Arrays.Natural_8_Array (1 .. 4);
      Data : UInt8_Array with
        Import, Address => Copy'Address;
   begin
      Write (Block.all, Index, Data);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Text  : String;
      Block : not null access Control_Block;
      Index : Index_Up_Max := 1)
   is
      subtype UInt8_Array is Elansys.Arrays.Natural_8_Array (Text'Range);
      Data : UInt8_Array with
        Import, Address => Text'Address;
   begin
      Write (Block.all, Index, Data);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Text  : String;
      Block : not null access Control_Block;
      Index : Index_Up_Max := 1)
   is
      subtype UInt8_Array is Elansys.Arrays.Natural_8_Array (Text'Range);
      Data : UInt8_Array with
        Import, Address => Text'Address;
   begin
      Write (Block.all, Index, Data);
      Write
        (Block.all,
         Index,
        (16#0D#,
          16#0A#));
   end Put_Line;

   -----------
   -- Write --
   -----------

   procedure Write
     (Block : in out Control_Block;
      Index :        Index_Up_Max;
      Data  :        Elansys.Arrays.Natural_8_Array)
   is
      use type Interfaces.C.unsigned;

      type Unbounded_UInt8_Array is
        array (0 .. Interfaces.C.unsigned'Last) of Interfaces.Unsigned_8;

      --  Buffer : RTT.Buffer renames Block.Up (Index);

      Target : Unbounded_UInt8_Array with
        Import, Address => Block.Up (Index).Buffer;

      Left   : Interfaces.C.unsigned;
      From   : Natural := Data'First;
      Length : Natural;

      Write_Offset : Interfaces.C.unsigned := Block.Up (Index).Write_Offset;
   begin
      while From <= Data'Last loop
         Left   := Block.Up (Index).Size - Block.Up (Index).Write_Offset;
         Length := Natural'Min (Data'Last - From + 1, Natural (Left));

         for J in 1 .. Length loop
            Target (Write_Offset) := Data (From);
            Write_Offset          := Write_Offset + 1;
            From                  := From + 1;
         end loop;

         if Write_Offset >= Block.Up (Index).Size
         then
            Write_Offset := 0;
         end if;

         Block.Up (Index).Write_Offset := Write_Offset;
      end loop;
   end Write;

end RTT;

--  BSD 3-Clause License
--
--   Copyright (c) 2023, Maxim Reznik
--   Copyright (c) 2025, Kevin Chadwick
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--  this
--     list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form must reproduce the above copyright
--  notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--
--  3. Neither the name of the copyright holder nor the names of its
--     contributors may be used to endorse or promote products derived from
--     this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
--  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
--  THE POSSIBILITY OF SUCH DAMAGE.
