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
      ************************************************************
      *    負担区分コード取得　　　　　COPY (CPORCSGETFTNKBN.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSGETFTNKBNAREA.
      *
      *医療機関ＩＤ
           03  ORCSGETFK-HOSPNUM       PIC 9(02).
      *患者ＩＤ
           03  ORCSGETFK-PTID          PIC 9(10).
      *診療年月
           03  ORCSGETFK-SRYYM         PIC X(06).
      *保険組合せ番号
           03  ORCSGETFK-HKNCOMBINUM   PIC 9(04).
      *負担区分コード
           03  ORCSGETFK-FTNKBN        PIC X(01).
      *保険者番号
           03  ORCSGETFK-HKNJANUM      PIC X(08).
      *負担者番号
           03  ORCSGETFK-FTNJANUM-G.
               05  ORCSGETFK-FTNJANUM  PIC X(08)
                                       OCCURS  4.
      *
