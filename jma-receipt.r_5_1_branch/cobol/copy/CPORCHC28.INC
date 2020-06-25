      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      *----------------------------------------------------------*
      *    ´µ¼Ô°ìÍ÷°õºþ           COPY (CPORCHC28.INC)
      *                                (ORCHC28 ¤Ç»ÈÍÑ¡Ë
      *----------------------------------------------------------*
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ************************************************************
       01  ORCHC28AREA.
      *É½Âê
           03  ORCHC28-HYODAI              PIC X(100).
      *¾ò·ï£±
           03  ORCHC28-JYOKEN1             PIC X(100).
      *¾ò·ï£²
           03  ORCHC28-JYOKEN2             PIC X(100).
      *´µ¼ÔIDÎÎ°è
           03  ORCHC28-PTID-AREA.
      *´µ¼ÔID·ï¿ô
               05  ORCHC28-PTID-COUNT      PIC 9(05).
      *
               05  ORCHC28-PTID-OCC        OCCURS  200.
      *Ï¢ÈÖ
                   07  ORCHC28-PTID-SEQ    PIC X(5).
      *´µ¼ÔID
                   07  ORCHC28-PTID        PIC 9(10).
      *
