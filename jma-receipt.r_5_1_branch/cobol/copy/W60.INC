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
        02  W60.
      *     02  W60-FIXED1.
              03  W60-B03-STATE PIC S9(9)   BINARY.
              03  W60-STYUKYMD              PIC X(10).
              03  W60-EDYUKYMD              PIC X(10).
              03  W60-PAGENO                PIC S9(9)   BINARY.
              03  W60-CMB-SEIHOJNUMCHKKBN.
                04  W60-SEIHOJNUMCHKKBN        PIC X(22).
                04  W60-SEIHOJNUMCHKKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-SEIHOJNUMCHKKBN-ITEM   PIC X(22)   OCCURS  5.
              03  W60-CMB-BYOSRYKAKBN.
                04  W60-BYOSRYKAKBN        PIC X(10).
                04  W60-BYOSRYKAKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-BYOSRYKAKBN-ITEM   PIC X(10)   OCCURS  2.
              03  W60-CMB-SYUBYOCHK.
                04  W60-SYUBYOCHK        PIC X(10).
                04  W60-SYUBYOCHK-COUNT  PIC S9(9)   BINARY.
                04  W60-SYUBYOCHK-ITEM   PIC X(10)   OCCURS  2.
              03  W60-UTAGAIKKN          PIC S9(1). 
              03  W60-CMB-KYUSEIKKNKBN.
                04  W60-KYUSEIKKNKBN       PIC X(10).
                04  W60-KYUSEIKKNKBN-COUNT PIC S9(9)   BINARY.
                04  W60-KYUSEIKKNKBN-ITEM  PIC X(10)   OCCURS  15.
              03  W60-CMB-COMPBYONISSUKBN.
                04  W60-COMPBYONISSUKBN       PIC X(22).
                04  W60-COMPBYONISSUKBN-COUNT PIC S9(9)   BINARY.
                04  W60-COMPBYONISSUKBN-ITEM  PIC X(22)   OCCURS  5.
              03  W60-CMB-SRYSTTENKIKBN.
                04  W60-SRYSTTENKIKBN       PIC X(60).
                04  W60-SRYSTTENKIKBN-COUNT PIC S9(9)   BINARY.
                04  W60-SRYSTTENKIKBN-ITEM  PIC X(60)   OCCURS  5.
              03  W60-CHKMTRXNIL-G.
                  05  W60-CHKMTRXNIL-OCC   OCCURS 4.
                   06 W60-CHKMTRXNIL-STATE PIC S9(9)   BINARY.
                   06 W60-CHKMTRXNIL-STYLE PIC X(20).
              03  W60-CHKMTRX-G.
                04  W60-CHKMTRX-OCC    OCCURS 5.
                  05  W60-CHKMTRX-STYLE    PIC X(20).
                  05  W60-MTRX         PIC X(02).
              03  W60-CMB-TENKITEKBYOKBN.
                04  W60-TENKITEKBYOKBN        PIC X(20).
                04  W60-TENKITEKBYOKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-TENKITEKBYOKBN-ITEM   PIC X(20)   OCCURS  5.
              03  W60-CMB-TEKBYOYKZKBN.
                04  W60-TEKBYOYKZKBN        PIC X(60).
                04  W60-TEKBYOYKZKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-TEKBYOYKZKBN-ITEM   PIC X(60)   OCCURS  5.
              03  W60-CMB-TEKBYOSRYKBN.
                04  W60-TEKBYOSRYKBN        PIC X(60).
                04  W60-TEKBYOSRYKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-TEKBYOSRYKBN-ITEM   PIC X(60)   OCCURS  5.
              03  W60-CMB-YBKENSAYAKUKBN.
                04  W60-YBKENSAYAKUKBN        PIC X(32).
                04  W60-YBKENSAYAKUKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-YBKENSAYAKUKBN-ITEM   PIC X(32)   OCCURS  5.
              03  W60-CMB-YBSJTYKZKBN.
                04  W60-YBSJTYKZKBN           PIC X(32).
                04  W60-YBSJTYKZKBN-COUNT     PIC S9(9)   BINARY.
                04  W60-YBSJTYKZKBN-ITEM      PIC X(32)   OCCURS  5.
              03  W60-CMB-YBMSIYKZKBN.
                04  W60-YBMSIYKZKBN           PIC X(32).
                04  W60-YBMSIYKZKBN-COUNT     PIC S9(9)   BINARY.
                04  W60-YBMSIYKZKBN-ITEM      PIC X(32)   OCCURS  5.
              03  W60-CMB-YBSHOHOYKZKBN.
                04  W60-YBSHOHOYKZKBN         PIC X(32).
                04  W60-YBSHOHOYKZKBN-COUNT   PIC S9(9)   BINARY.
                04  W60-YBSHOHOYKZKBN-ITEM    PIC X(32)   OCCURS  5.
              03  W60-CMB-YBUTAGAIKBN.
                04  W60-YBUTAGAIKBN        PIC X(32).
                04  W60-YBUTAGAIKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-YBUTAGAIKBN-ITEM   PIC X(32)   OCCURS  5.
              03  W60-CMB-YKZHEIMSTKBN.
                04  W60-YKZHEIMSTKBN        PIC X(32).
                04  W60-YKZHEIMSTKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-YKZHEIMSTKBN-ITEM   PIC X(32)   OCCURS  5.
              03  W60-CMB-SRYHEIWRNKBN.
                04  W60-SRYHEIWRNKBN        PIC X(32).
                04  W60-SRYHEIWRNKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-SRYHEIWRNKBN-ITEM   PIC X(32)   OCCURS  5.
              03  W60-CMB-SRYHEIMSTKBN.
                04  W60-SRYHEIMSTKBN        PIC X(32).
                04  W60-SRYHEIMSTKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-SRYHEIMSTKBN-ITEM   PIC X(32)   OCCURS  5.
              03  W60-CMB-SRYHEIEXPKBN.
                04  W60-SRYHEIEXPKBN       PIC X(40).
                04  W60-SRYHEIEXPKBN-STATE PIC S9(9)   BINARY.
                04  W60-SRYHEIEXPKBN-COUNT PIC S9(9)   BINARY.
                04  W60-SRYHEIEXPKBN-ITEM  PIC X(40)   OCCURS  5.
                04  W60-SRYHEIEXPKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  W60-CMB-SRYMATERKKBN.
                04  W60-SRYMATERKKBN       PIC X(22).
                04  W60-SRYMATERKKBN-COUNT PIC S9(9)   BINARY.
                04  W60-SRYMATERKKBN-ITEM  PIC X(22)   OCCURS  5.
              03  W60-CMB-SRYKAISUMSTKBN.
                04  W60-SRYKAISUMSTKBN        PIC X(22).
                04  W60-SRYKAISUMSTKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-SRYKAISUMSTKBN-ITEM   PIC X(22)  OCCURS  5.
              03  W60-CMB-SRYKAISUWRNKBN.
                04  W60-SRYKAISUWRNKBN        PIC X(22).
                04  W60-SRYKAISUWRNKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-SRYKAISUWRNKBN-ITEM   PIC X(22)   OCCURS  5.
              03  W60-CMB-REIGAICHKKBN.
                04  W60-REIGAICHKKBN        PIC X(20).
                04  W60-REIGAICHKKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-REIGAICHKKBN-ITEM   PIC X(20)   OCCURS  5.
              03  W60-CMB-PRTSRTKBN.
                04  W60-PRTSRTKBN          PIC X(22).
                04  W60-PRTSRTKBN-COUNT    PIC S9(9)   BINARY.
                04  W60-PRTSRTKBN-ITEM     PIC X(22)   OCCURS  5.
              03  W60-CMB-PRTPTLSTKBN.
                04  W60-PRTPTLSTKBN        PIC X(22).
                04  W60-PRTPTLSTKBN-COUNT  PIC S9(9)   BINARY.
                04  W60-PRTPTLSTKBN-ITEM   PIC X(22)   OCCURS  2.
              03  W60-CMB-ERR990009LSTKBN.
                04  W60-ERR990009KBN       PIC X(22).
                04  W60-ERR990009KBN-COUNT PIC S9(9)   BINARY.
                04  W60-ERR990009KBN-ITEM  PIC X(22)   OCCURS  5.
              03  W60-CMB-ERRA10001LSTKBN.
                04  W60-ERRA10001KBN       PIC X(22).
                04  W60-ERRA10001KBN-COUNT PIC S9(9)   BINARY.
                04  W60-ERRA10001KBN-ITEM  PIC X(22)   OCCURS  5.
              03  W60-INGAITEN             PIC S9(4)   OCCURS  7.
              03  W60-CMB-INGAIPROCKBN.
                04  W60-INGAIPROCKBN       PIC X(20).
                04  W60-INGAIPROCKBN-COUNT PIC S9(9)   BINARY.
                04  W60-INGAIPROCKBN-ITEM  PIC X(20)   OCCURS  5.

