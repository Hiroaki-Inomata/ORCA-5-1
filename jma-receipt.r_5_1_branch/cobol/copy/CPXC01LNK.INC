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
      *****************************************************************
      * プレビュー用情報領域      CPXC01LNK.INC                       *
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  LNK-XC01-AREA.
           03  LNK-XC01-KEY.
      *    ＫＥＹ
               05  LNK-XC01-TBL-KEY    PIC X(8).
      *    連番
               05  LNK-XC01-RENNUM     PIC 9(4).
      *    グループ
               05  LNK-XC01-TBL-GROUP  PIC X(14).
      *    UUID
               05  LNK-XC01-TBL-UUID   PIC X(36).
      *    処理内連番
               05  LNK-XC01-SHORI-RENNUM
                                       PIC 9(4).
           03  LNK-XC01-ST-PAGE        PIC 9(5).
           03  LNK-XC01-ED-PAGE        PIC 9(5).
      *
           03  LNK-XC01-HKNKBN         PIC X(1).
      *
           03  LNK-XC01-MOTOPG         PIC X(08).
      *
           03  LNK-XC01-GYOUMU-CD      PIC X(02).
      *
           03  LNK-XC01-YMD            PIC X(08).
      *
           03  LNK-XC01-MODE           PIC X(01).
