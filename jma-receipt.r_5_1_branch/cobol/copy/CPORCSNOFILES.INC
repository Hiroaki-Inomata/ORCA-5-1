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
      *    未配置ファイルチェックサブ(COBOL)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SNOFILES-AREA.
           03  SNOFILES-MAX                    PIC 9(05).
           03  SNOFILES-OCC                    OCCURS  200.
      *    01  01:1031（外来)／02:1031（入院)/03:日報/04:月報
               05  SNOFILES-TYPE               PIC X(02).
               05  SNOFILES-ID-G.
                   07  SNOFILES-ID             PIC 9(08).
                   07  SNOFILES-ID-X   REDEFINES   SNOFILES-ID
                                               PIC X(08).
               05  SNOFILES-PGID               PIC X(50).
               05  SNOFILES-PGNAME             PIC X(100).
      *    ONPG-GAIから順に01,02,03,04...
           03  SNOFILES-ONPG-GAI               PIC X(02).
           03  SNOFILES-ONPG-NYU               PIC X(02).
           03  SNOFILES-NIPPOU                 PIC X(02).
           03  SNOFILES-GEPPOU                 PIC X(02).
