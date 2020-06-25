      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
          01  HC26.
              03  HC26-PRTYM         PIC  X(16).
              03  HC26-HOSPCD        PIC  X(20).
              03  HC26-KENSU-G       OCCURS  3. 
                  05  HC26-KENSU     PIC  X(5)     OCCURS  2.
              03  HC26-TENSU-G       OCCURS  3. 
                  05  HC26-TENSU     PIC  X(11)    OCCURS  2.
              03  HC26-KTENSU-G      OCCURS  3. 
                  05  HC26-KTENSU    PIC  X(11)    OCCURS  2.
              03  HC26-YKZFTN-G      OCCURS  3. 
                  05  HC26-YKZFTN    PIC  X(11)    OCCURS  2.
              03  HC26-ITBFTN-G      OCCURS  3. 
                  05  HC26-ITBFTN    PIC  X(11)    OCCURS  2.
              03  HC26-KOHNUM        PIC  X(2)     OCCURS  27.
              03  HC26-KOHKBN        PIC  X(8)     OCCURS  27.
              03  HC26-KOHKENSU      PIC  X(5)     OCCURS  27.
              03  HC26-KOHTENSU      PIC  X(11)    OCCURS  27.
              03  HC26-KOHKTENSU     PIC  X(11)    OCCURS  27.
              03  HC26-KOHYKZFTN     PIC  X(11)    OCCURS  27.
              03  HC26-KOHFTNGAK     PIC  X(11)    OCCURS  27.
              03  HC26-SYOKENSU      PIC  X(5)     OCCURS  3.
              03  HC26-SYOKINGAK     PIC  X(11)    OCCURS  3.
              03  HC26-SYOFTNGAK     PIC  X(11)    OCCURS  3.
