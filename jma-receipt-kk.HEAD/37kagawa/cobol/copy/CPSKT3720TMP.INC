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
      ************************************************************
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT3720TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    µÈÀî         10/09/24  ¼ç²ÊÂÐ±þ
      *  01.00.02    ÇëÌî         19/01/11  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT3720TMP-REC.
           03  SKT3720TMP-KEY.
      *¸©Æâ¡¦¸©³°
               05  SKT3720TMP-PREFKBN           PIC  X(01).
      *¿ÇÎÅ²Ê
               05  SKT3720TMP-RECEKA            PIC  X(02).
      *
      *ÀÁµá¥Æ¡¼¥Ö¥ë
           03  SKT3720TMP-HKN-AREA  OCCURS  11.
      *·ï¿ô
               05  SKT3720TMP-KENSU        PIC 9(05).
      *¶â³Û
               05  SKT3720TMP-KINGK        PIC 9(09).
      *É¸½àÉéÃ´³Û
               05  SKT3720TMP-FTNGK        PIC 9(09).
      *
      *·ë³Ë¡¦Àº¿À
           03  SKT3720TMP-35-AREA.
      *·ë³Ë¡¿·ï¿ô
               05  SKT3720TMP-35-KENSU     PIC 9(05).
      *·ë³Ë¡¿¶â³Û
               05  SKT3720TMP-35-KINGK     PIC 9(09).
      *·ë³Ë¡¿É¸½àÉéÃ´³Û
               05  SKT3720TMP-35-FTNGK     PIC 9(09).
      *
      *¸øÈñ¥Æ¡¼¥Ö¥ë
           03  SKT3720TMP-KOH-AREA  OCCURS  20.
      *¸øÈñÈÖ¹æ
               05  SKT3720TMP-KOHNUM      PIC X(03).
      *¸øÈñÌ¾¾Î
               05  SKT3720TMP-KOHNAME     PIC X(10).
      *¸øÈñ¡¿·ï¿ô
               05  SKT3720TMP-KKENSU      PIC 9(05).
      *¸øÈñ¡¿¶â³Û
               05  SKT3720TMP-KKINGK      PIC 9(09).
      *¸øÈñ¡¿É¸½àÉéÃ´³Û
               05  SKT3720TMP-KFTNGK      PIC 9(09).
