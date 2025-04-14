with Interfaces.C;

with Elansys.Arrays;

with RTT;

package RTT_IO is
   use type Interfaces.C.char_array;

   Terminal : constant Interfaces.C.char_array :=
     ("Terminal" & Interfaces.C.nul);

   Graph : constant Interfaces.C.char_array := ("Graph" & Interfaces.C.nul);

   Terminal_Output : Elansys.Arrays.Natural_8_Array (1 .. 256);
   Graph_Output    : Elansys.Arrays.Natural_8_Array (1 .. 16);

   Control_Block : aliased RTT.Control_Block :=
     (Max_Up_Buffers   => RTT.Up_Buffers,
      Max_Down_Buffers => RTT.Down_Buffers,
      Up               =>
        (1 =>
           (Name   => Terminal'Address,
            Buffer => Terminal_Output'Address,
            Size   => Terminal_Output'Length,
            others => <>)
            --  2 =>
            --    (Name   => Graph'Address,
            --     Buffer => Graph_Output'Address,
            --     Size   => Graph_Output'Length,
            --     others => <>)
            ),
      others           => <>) with
     Export, External_Name => "_SEGGER_RTT";

   procedure Put
     (Text  : String;
      CB    : access RTT.Control_Block := Control_Block'Access;
      Index : Positive                 := 1) renames RTT.Put;
   --  Put Text.

   procedure Put_Line
     (Text  : String;
      CB    : access RTT.Control_Block := Control_Block'Access;
      Index : Positive                 := 1) renames RTT.Put_Line;
   --  Put Text and CR, LF.

   procedure Put
     (Value : Integer;
      CB    : access RTT.Control_Block := Control_Block'Access;
      Index : Positive                 := 1) renames RTT.Put;
   --  Dump Value in binary format. Could be used for plotting graphs with
   --  Cortex Debug.

end RTT_IO;

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
