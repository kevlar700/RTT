--  This package provides Real Time Transfer implementation on a target side.
--
--  Real Time Transfer (RTT) is an interface specified by SEGGER based on basic
--  memory reads and writes to transfer data bidirectionally between target and
--  host. The specification is independent of the target architecture. Every
--  target that supports so called "background memory access", which means
--  that the target memory can be accessed by the debugger while the target
--  is running, can be used.

with System;
with Elansys.Arrays;
with Interfaces.C;
with Rtt_Config;

package RTT is

   Up_Buffers   : constant Natural := Rtt_Config.Up_Buffers;
   Down_Buffers : constant Natural := Rtt_Config.Down_Buffers;
   subtype Index_Up_Max is Positive range 1 .. Up_Buffers;
   --  subtype Index_Down_Max is Positive range 1 .. Down_Buffers;

   type Operating_Mode is
     (
      --  if the up buffer (target to host) has not enough data to hold all the
      --  incoming data then all data is discarded
      No_Block_Skip,
      --  if the up buffer (target to host) has not enough data to hold all
      --  the incoming data then the available space is filled and the rest
      --  discarded
      No_Block_Trim,
      --  The application will wait when the buffer is full resulting in a
      --  blocked application state but preventing data from being lost
      Block_If_FIFO_Full);

   for Operating_Mode use
     (No_Block_Skip      => 0,
      No_Block_Trim      => 1,
      Block_If_FIFO_Full => 2);

   type Buffer_Flags is record
      Reserved : Natural range 0 .. 0 := 0;
      Mode     : Operating_Mode       := No_Block_Trim;
   end record with
     Size => 32;

   for Buffer_Flags use record
      Reserved at 0 range 2 .. 31;
      Mode     at 0 range 0 ..  1;
   end record;

   type Buffer is limited record
      --  Buffer's name such as "Terminal" or "SysView".
      Name         : System.Address        := System.Null_Address;
      --  Buffer pointer.
      Buffer       : System.Address        := System.Null_Address;
      --  Size of the buffer in bytes.
      Size         : Interfaces.C.unsigned := 0;
      --  Next byte to be written
      Write_Offset : Interfaces.C.unsigned := 0 with
        Atomic;
      --  Next byte to be read
      Read_Offset  : Interfaces.C.unsigned := 0 with
        Atomic;
      Flags        : Buffer_Flags;
   end record;
   --  Ring buffer for target<-->host transfers

   type Buffer_Array is array (Positive range <>) of Buffer;

   use type Interfaces.C.char_array;

   --  Control block for RTT with number of buffers and their configuration
   type Control_Block
     (Max_Up_Buffers   : Natural := Up_Buffers;
      Max_Down_Buffers : Natural := Down_Buffers)
   is
   limited record
      --  Predefined control block identifier value
      ID   : Interfaces.C.char_array (1 .. 16) :=
        "SEGGER RTT" & (1 .. 6 => Interfaces.C.nul);
      Up   : Buffer_Array (1 .. Max_Up_Buffers);
      Down : Buffer_Array (1 .. Max_Down_Buffers);
   end record;

   for Control_Block use record
      ID               at  0 range 0 .. 128 - 1;
      Max_Up_Buffers   at 16 range 0 ..  32 - 1;
      Max_Down_Buffers at 20 range 0 ..  32 - 1;
   end record;

   --  Write Data into Up buffer with given Index of the control block.
   procedure Write
     (Block : in out Control_Block;
      Index :        Index_Up_Max;
      Data  :        Elansys.Arrays.Natural_8_Array) with
     Pre => Index <= Block.Max_Up_Buffers;

   --  Put Text into Up buffer with given Index of the control block.
   procedure Put
     (Text  :        String;
      Index :        Index_Up_Max := 1;
      Block : in out Control_Block);

   --  Put Text and CR, LF into Up buffer with given Index of the control
   --  block.
   procedure Put_Line
     (Text  :        String;
      Index :        Index_Up_Max := 1;
      Block : in out Control_Block);

   --  Dump Value in binary format. Could be used for plotting graphs with
   --  Cortex Debug.
   procedure Put
     (Value :        Integer;
      Index :        Index_Up_Max := 1;
      Block : in out Control_Block);

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
