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
      *****************************************************************
      *  Í½Ìó¥Þ¥¹¥¿¡¼       (CPYYK.INC) 
      *****************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ************************************************************
      *01  YYK-REC.
           03  YYK-KEY.
      *°åÎÅµ¡´Ø£É£Ä
               05  YYK-HOSPNUM          PIC 9(2).
      *¿ÇÎÅÆâÍÆ
               05  YYK-SRYNAIYO        PIC X(02).
      *¥É¥¯¥¿¡¼¥³¡¼¥É
               05  YYK-DRCD            PIC X(05).
      *Í½ÌóÇ¯·îÆü
               05  YYK-YYKYMD          PIC X(08).
      *¥­¡¼Í½Ìó»þ´Ö
               05  YYK-KEYYYKTIME      PIC 9(04).
      *Í½Ìó£É£Ä
               05  YYK-YYKID           PIC 9(02).
      *Í½Ìó·ï¿ô¡Ê£É£Ä¡á£°£°¡Ë
           03  YYK-NAIYOU1.
      *Í½ÌóºÇÂç·ï¿ô
               05  YYK-YYKMAXCNT       PIC 9(03).
      *Í½Ìó·ï¿ô
               05  YYK-YYKCNT          PIC 9(03).
      *Í½Ìó»þ´Ö
           03  YYK-YYKTIME             PIC 9(04).
      *´µ¼Ô£É£Ä
           03  YYK-PTID                PIC 9(10).
      *´µ¼Ô»áÌ¾
           03  YYK-NAME                PIC X(100).
      *¿ÇÎÅ²Ê
           03  YYK-SRYKA               PIC X(02).
      *Í½Ìó¶èÊ¬
           03  YYK-YYKKBN              PIC X(02).
      *³ÎÇ§Ï¢Íí
           03  YYK-KAKREN              PIC X(02).
      *Íè±¡ºÑ
           03  YYK-RAIINZUMI           PIC X(01).
      *Ã¼Ëö£É£Ä
           03  YYK-TERMID              PIC  X(16).
      *¥ª¥Ú¡¼¥ì¡¼¥¿£É£Ä
           03  YYK-OPID                PIC  X(16).
      *ºîÀ®Ç¯·îÆü
           03  YYK-CREYMD              PIC  X(08).
      *¹¹¿·Ç¯·îÆü
           03  YYK-UPYMD               PIC  X(08).
      *¹¹¿·»þ´Ö
           03  YYK-UPHMS               PIC  X(06).
