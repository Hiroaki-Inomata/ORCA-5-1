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
      *    オーダーサブ引数    COPY (CPORCSODR.INC)                   *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  SODR-AREA.
           03  SODR-IN.
               05  SODR-HOSPNUM            PIC 9(2).
               05  SODR-KARTE-KEY          PIC X(36).
               05  SODR-ORDERNUM           PIC 9(02).
               05  SODR-NYUGAIKBN          PIC X(01).
               05  SODR-PTNUM              PIC X(20).
               05  SODR-PTID               PIC 9(10).
               05  SODR-SRYKA              PIC X(02).
               05  SODR-SRYYMD             PIC X(08).
               05  SODR-HKNCOMBI           PIC 9(04).
               05  SODR-DRCD               PIC X(05).
               05  SODR-ORDERID            PIC X(04).
               05  SODR-UKEYMD             PIC X(08).
               05  SODR-UKEHMS             PIC X(08).
               05  SODR-PROCKBN            PIC X(01).
               05  SODR-KEYGENERATE        PIC 9(01).
               05  SODR-INF-DIALOG         PIC 9(02).
               05  SODR-OPID               PIC X(16).
               05  SODR-UPYMD              PIC X(08).
               05  SODR-UPHMS              PIC X(06).
               05  SODR-KHNSRYCD           PIC X(09).
      *        呼び出し元 0:CLAIM/1:API:/2:画面
               05  SODR-SRCKBN             PIC X(01).
           03  SODR-OT.
               05  SODR-RRKNUM             PIC 9(03).
               05  SODR-RRKEDANUM          PIC 9(03).
               05  SODR-SHONUM             PIC 9(03).
               05  SODR-SKYSTYMD           PIC X(08).
               05  SODR-UNSKYSTYMD         PIC X(08).
               05  SODR-UNSKYEDYMD         PIC X(08).
               05  SODR-DENPNUM-MAX        PIC 9(03).
               05  SODR-DENPNUM-G.
                   07  SODR-DENPNUM-OCC    OCCURS  10.
                       09  SODR-DENPNUM    PIC 9(07).
               05  SODR-ERRFLG             PIC 9(01).
               05  SODR-ERRCD              PIC X(04).
               05  SODR-DIALOG             PIC 9(02).
               05  SODR-RC                 PIC S9(02).
