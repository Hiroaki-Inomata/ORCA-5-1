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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGW60.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : データチェック機能情報２
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/09    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
       01  SPA-W60.
           03  SPA-GMN-CUR                     PIC 9(03).
           03  SPA-GMN-PAGENO                  PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-SEIHOJNUMCHKKBN-G.
                   07  SPA-GMN-SEIHOJNUMCHKKBN     PIC X(01).
                   07  SPA-GMN-SEIHOJNUMCHKKBNFL   PIC X(01).
                   07  SPA-GMN-SEIHOJNUMCHKKBNMEI  PIC X(20).
               05  SPA-GMN-BYOSRYKAKBN-G.
                   07  SPA-GMN-BYOSRYKAKBN     PIC X(01).
                   07  SPA-GMN-BYOSRYKAKBNFL   PIC X(01).
                   07  SPA-GMN-BYOSRYKAKBNMEI  PIC X(08).
               05  SPA-GMN-SYUBYOCHK-G.
                   07  SPA-GMN-SYUBYOCHK       PIC X(01).
                   07  SPA-GMN-SYUBYOCHKFL     PIC X(01).
                   07  SPA-GMN-SYUBYOCHKMEI    PIC X(08).
               05  SPA-GMN-UTAGAIKKN           PIC 9(01).
               05  SPA-GMN-KYUSEIKKNKBN-G.
                   07  SPA-GMN-KYUSEIKKNKBN    PIC X(01).
                   07  SPA-GMN-KYUSEIKKNKBNFL  PIC X(01).
                   07  SPA-GMN-KYUSEIKKNKBNMEI PIC X(08).
               05  SPA-GMN-COMPBYONISSUKBN-G.
                   07  SPA-GMN-COMPBYONISSUKBN    PIC X(01).
                   07  SPA-GMN-COMPBYONISSUKBNFL  PIC X(01).
                   07  SPA-GMN-COMPBYONISSUKBNMEI PIC X(20).
               05  SPA-GMN-SRYSTTENKIKBN-G.
                   07  SPA-GMN-SRYSTTENKIKBN     PIC X(01).
                   07  SPA-GMN-SRYSTTENKIKBNFL   PIC X(01).
                   07  SPA-GMN-SRYSTTENKIKBNMEI  PIC X(58).
               05  SPA-GMN-CHKMTRX-G.
                   07  SPA-GMN-CHKMTRX         PIC X(01)
                                               OCCURS  5.
               05  SPA-GMN-TENKITEKBYOKBN-G.
                   07  SPA-GMN-TENKITEKBYOKBN  PIC X(01).
                   07  SPA-GMN-TENKITEKBYOKBNFL  PIC X(01).
                   07  SPA-GMN-TENKITEKBYOKBNMEI PIC X(18).
               05  SPA-GMN-TEKBYOYKZKBN-G.
                   07  SPA-GMN-TEKBYOYKZKBN    PIC X(01).
                   07  SPA-GMN-TEKBYOYKZKBNFL  PIC X(01).
                   07  SPA-GMN-TEKBYOYKZKBNMEI PIC X(58).
               05  SPA-GMN-TEKBYOSRYKBN-G.
                   07  SPA-GMN-TEKBYOSRYKBN    PIC X(01).
                   07  SPA-GMN-TEKBYOSRYKBNFL  PIC X(01).
                   07  SPA-GMN-TEKBYOSRYKBNMEI PIC X(58).
               05  SPA-GMN-YBKENSAYAKUKBN-G.
                   07  SPA-GMN-YBKENSAYAKUKBN  PIC X(01).
                   07  SPA-GMN-YBKENSAYAKUKBNFL PIC X(01).
                   07  SPA-GMN-YBKENSAYAKUKBNMEI PIC X(30).
               05  SPA-GMN-YBSJTYKZKBN-G.
                   07  SPA-GMN-YBSJTYKZKBN  PIC X(01).
                   07  SPA-GMN-YBSJTYKZKBNFL PIC X(01).
                   07  SPA-GMN-YBSJTYKZKBNMEI PIC X(30).
               05  SPA-GMN-YBMSIYKZKBN-G.
                   07  SPA-GMN-YBMSIYKZKBN  PIC X(01).
                   07  SPA-GMN-YBMSIYKZKBNFL PIC X(01).
                   07  SPA-GMN-YBMSIYKZKBNMEI PIC X(30).
               05  SPA-GMN-YBSHOHOYKZKBN-G.
                   07  SPA-GMN-YBSHOHOYKZKBN  PIC X(01).
                   07  SPA-GMN-YBSHOHOYKZKBNFL PIC X(01).
                   07  SPA-GMN-YBSHOHOYKZKBNMEI PIC X(30).
               05  SPA-GMN-YBUTAGAIKBN-G.
                   07  SPA-GMN-YBUTAGAIKBN     PIC X(01).
                   07  SPA-GMN-YBUTAGAIKBNFL   PIC X(01).
                   07  SPA-GMN-YBUTAGAIKBNMEI  PIC X(30).
               05  SPA-GMN-YKZHEIMSTKBN-G.
                   07  SPA-GMN-YKZHEIMSTKBN    PIC X(01).
                   07  SPA-GMN-YKZHEIMSTKBNFL  PIC X(01).
                   07  SPA-GMN-YKZHEIMSTKBNMEI PIC X(30).
               05  SPA-GMN-SRYHEIWRNKBN-G.
                   07  SPA-GMN-SRYHEIWRNKBN    PIC X(01).
                   07  SPA-GMN-SRYHEIWRNKBNFL  PIC X(01).
                   07  SPA-GMN-SRYHEIWRNKBNMEI PIC X(30).
               05  SPA-GMN-SRYHEIMSTKBN-G.
                   07  SPA-GMN-SRYHEIMSTKBN    PIC X(01).
                   07  SPA-GMN-SRYHEIMSTKBNFL  PIC X(01).
                   07  SPA-GMN-SRYHEIMSTKBNMEI PIC X(30).
               05  SPA-GMN-SRYHEIEXPKBN-G.
                   07  SPA-GMN-SRYHEIEXPKBN    PIC X(01).
                   07  SPA-GMN-SRYHEIEXPKBNFL  PIC X(01).
                   07  SPA-GMN-SRYHEIEXPKBNMEI PIC X(30).
               05  SPA-GMN-SRYMATERKKBN-G.
                   07  SPA-GMN-SRYMATERKKBN    PIC X(01).
                   07  SPA-GMN-SRYMATERKKBNFL  PIC X(01).
                   07  SPA-GMN-SRYMATERKKBNMEI PIC X(30).
               05  SPA-GMN-SRYKAISUMSTKBN-G.
                   07  SPA-GMN-SRYKAISUMSTKBN    PIC X(01).
                   07  SPA-GMN-SRYKAISUMSTKBNFL  PIC X(01).
                   07  SPA-GMN-SRYKAISUMSTKBNMEI PIC X(20).
               05  SPA-GMN-SRYKAISUWRNKBN-G.
                   07  SPA-GMN-SRYKAISUWRNKBN    PIC X(01).
                   07  SPA-GMN-SRYKAISUWRNKBNFL  PIC X(01).
                   07  SPA-GMN-SRYKAISUWRNKBNMEI PIC X(20).
               05  SPA-GMN-INGAIPROCKBN-G.
                   07  SPA-GMN-INGAIPROCKBN      PIC X(01).
                   07  SPA-GMN-INGAIPROCKBNFL    PIC X(01).
                   07  SPA-GMN-INGAIPROCKBNMEI   PIC X(20).
               05  SPA-GMN-REIGAICHKKBN-G.
                   07  SPA-GMN-REIGAICHKKBN    PIC X(01).
                   07  SPA-GMN-REIGAICHKKBNFL  PIC X(01).
                   07  SPA-GMN-REIGAICHKKBNMEI PIC X(18).
               05  SPA-GMN-PRTSRTKBN-G.
                   07  SPA-GMN-PRTSRTKBN       PIC X(01).
                   07  SPA-GMN-PRTSRTKBNFL     PIC X(01).
                   07  SPA-GMN-PRTSRTKBNMEI    PIC X(20).
               05  SPA-GMN-PRTPTLSTKBN-G.
                   07  SPA-GMN-PRTPTLSTKBN     PIC X(01).
                   07  SPA-GMN-PRTPTLSTKBNFL   PIC X(01).
                   07  SPA-GMN-PRTPTLSTKBNMEI  PIC X(20).
               05  SPA-GMN-ERR990009KBN-G.
                   07  SPA-GMN-ERR990009KBN    PIC X(01).
                   07  SPA-GMN-ERR990009KBNFL  PIC X(01).
                   07  SPA-GMN-ERR990009KBNMEI PIC X(20).
               05  SPA-GMN-ERRA10001KBN-G.
                   07  SPA-GMN-ERRA10001KBN    PIC X(01).
                   07  SPA-GMN-ERRA10001KBNFL  PIC X(01).
                   07  SPA-GMN-ERRA10001KBNMEI PIC X(20).
               05  SPA-GMN-INGAITEN-G.
                   07  SPA-GMN-SYA-INGAITEN    PIC 9(04).
                   07  SPA-GMN-KOK-INGAITEN    PIC 9(04).
                   07  SPA-GMN-KOU-INGAITEN    PIC 9(04).
                   07  SPA-GMN-RSI-INGAITEN    PIC 9(04).
                   07  SPA-GMN-JBI-INGAITEN    PIC 9(04).
                   07  SPA-GMN-JIH-INGAITEN    PIC 9(04).
                   07  SPA-GMN-KGI-INGAITEN    PIC 9(04).
               05  SPA-GMN-INGAITEN-R REDEFINES SPA-GMN-INGAITEN-G.
                   07  SPA-GMN-INGAITEN        PIC 9(04)
                                               OCCURS  7.
           03  SPA-GMN-CMB-AREA.
               05  SPA-GMN-SEIHOJNUMCHKKBNLST-G.
                   07  SPA-GMN-SEIHOJNUMCHKKBN-MAX     PIC 9(03).
                   07  SPA-GMN-SEIHOJNUMCHKKBNLST      OCCURS   2.
                       09  SPA-GMN-SEIHOJNUMCHKKBNL    PIC X(01).
                       09  SPA-GMN-SEIHOJNUMCHKKBNFLL  PIC X(01).
                       09  SPA-GMN-SEIHOJNUMCHKKBNMEIL PIC X(20).
               05  SPA-GMN-BYOSRYKAKBNLST-G.
                   07  SPA-GMN-BYOSRYKAKBN-MAX     PIC 9(03).
                   07  SPA-GMN-BYOSRYKAKBNLST      OCCURS   2.
                       09  SPA-GMN-BYOSRYKAKBNL    PIC X(01).
                       09  SPA-GMN-BYOSRYKAKBNFLL  PIC X(01).
                       09  SPA-GMN-BYOSRYKAKBNMEIL PIC X(08).
               05  SPA-GMN-SYUBYOCHKLST-G.
                   07  SPA-GMN-SYUBYOCHK-MAX       PIC 9(03).
                   07  SPA-GMN-SYUBYOCHKLST        OCCURS   2.
                       09  SPA-GMN-SYUBYOCHKL      PIC X(01).
                       09  SPA-GMN-SYUBYOCHKFLL    PIC X(01).
                       09  SPA-GMN-SYUBYOCHKMEIL   PIC X(08).
               05  SPA-GMN-KYUSEIKKNKBNLST-G.
                   07  SPA-GMN-KYUSEIKKNKBN-MAX    PIC 9(03).
                   07  SPA-GMN-KYUSEIKKNKBNLST     OCCURS   15.
                       09  SPA-GMN-KYUSEIKKNKBNL   PIC X(01).
                       09  SPA-GMN-KYUSEIKKNKBNFLL PIC X(01).
                       09  SPA-GMN-KYUSEIKKNKBNMEIL PIC X(08).
               05  SPA-GMN-COMPBYONISSUKBNLST-G.
                   07  SPA-GMN-COMPBYONISSUKBN-MAX    PIC 9(03).
                   07  SPA-GMN-COMPBYONISSUKBNLST     OCCURS   5.
                       09  SPA-GMN-COMPBYONISSUKBNL   PIC X(01).
                       09  SPA-GMN-COMPBYONISSUKBNFLL PIC X(01).
                       09  SPA-GMN-COMPBYONISSUKBNMEIL PIC X(20).
               05  SPA-GMN-SRYSTTENKIKBNLST-G.
                   07  SPA-GMN-SRYSTTENKIKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYSTTENKIKBNLST     OCCURS   5.
                       09  SPA-GMN-SRYSTTENKIKBNL   PIC X(01).
                       09  SPA-GMN-SRYSTTENKIKBNFLL PIC X(01).
                       09  SPA-GMN-SRYSTTENKIKBNMEIL PIC X(58).
               05  SPA-GMN-TENKITEKBYOKBNLST-G.
                   07  SPA-GMN-TENKITEKBYOKBN-MAX  PIC 9(03).
                   07  SPA-GMN-TENKITEKBYOKBNLST   OCCURS   2.
                       09  SPA-GMN-TENKITEKBYOKBNL PIC X(01).
                       09  SPA-GMN-TENKITEKBYOKBNFLL PIC X(01).
                       09  SPA-GMN-TENKITEKBYOKBNMEIL PIC X(18).
               05  SPA-GMN-TEKBYOYKZKBNLST-G.
                   07  SPA-GMN-TEKBYOYKZKBN-MAX  PIC 9(03).
                   07  SPA-GMN-TEKBYOYKZKBNLST   OCCURS   3.
                       09  SPA-GMN-TEKBYOYKZKBNL PIC X(01).
                       09  SPA-GMN-TEKBYOYKZKBNFLL PIC X(01).
                       09  SPA-GMN-TEKBYOYKZKBNMEIL PIC X(58).
               05  SPA-GMN-TEKBYOSRYKBNLST-G.
                   07  SPA-GMN-TEKBYOSRYKBN-MAX  PIC 9(03).
                   07  SPA-GMN-TEKBYOSRYKBNLST   OCCURS   3.
                       09  SPA-GMN-TEKBYOSRYKBNL PIC X(01).
                       09  SPA-GMN-TEKBYOSRYKBNFLL PIC X(01).
                       09  SPA-GMN-TEKBYOSRYKBNMEIL PIC X(58).
               05  SPA-GMN-YBKENSAYAKUKBNLST-G.
                   07  SPA-GMN-YBKENSAYAKUKBN-MAX  PIC 9(03).
                   07  SPA-GMN-YBKENSAYAKUKBNLST   OCCURS   2.
                       09  SPA-GMN-YBKENSAYAKUKBNL PIC X(01).
                       09  SPA-GMN-YBKENSAYAKUKBNFLL  PIC X(01).
                       09  SPA-GMN-YBKENSAYAKUKBNMEIL PIC X(30).
               05  SPA-GMN-YBSJTYKZKBNLST-G.
                   07  SPA-GMN-YBSJTYKZKBN-MAX  PIC 9(03).
                   07  SPA-GMN-YBSJTYKZKBNLST   OCCURS   2.
                       09  SPA-GMN-YBSJTYKZKBNL PIC X(01).
                       09  SPA-GMN-YBSJTYKZKBNFLL  PIC X(01).
                       09  SPA-GMN-YBSJTYKZKBNMEIL PIC X(30).
               05  SPA-GMN-YBMSIYKZKBNLST-G.
                   07  SPA-GMN-YBMSIYKZKBN-MAX  PIC 9(03).
                   07  SPA-GMN-YBMSIYKZKBNLST   OCCURS   2.
                       09  SPA-GMN-YBMSIYKZKBNL PIC X(01).
                       09  SPA-GMN-YBMSIYKZKBNFLL  PIC X(01).
                       09  SPA-GMN-YBMSIYKZKBNMEIL PIC X(30).
               05  SPA-GMN-YBSHOHOYKZKBNLST-G.
                   07  SPA-GMN-YBSHOHOYKZKBN-MAX  PIC 9(03).
                   07  SPA-GMN-YBSHOHOYKZKBNLST   OCCURS   2.
                       09  SPA-GMN-YBSHOHOYKZKBNL PIC X(01).
                       09  SPA-GMN-YBSHOHOYKZKBNFLL  PIC X(01).
                       09  SPA-GMN-YBSHOHOYKZKBNMEIL PIC X(30).
               05  SPA-GMN-YBUTAGAIKBNLST-G.
                   07  SPA-GMN-YBUTAGAIKBN-MAX     PIC 9(03).
                   07  SPA-GMN-YBUTAGAIKBNLST      OCCURS   2.
                       09  SPA-GMN-YBUTAGAIKBNL    PIC X(01).
                       09  SPA-GMN-YBUTAGAIKBNFLL  PIC X(01).
                       09  SPA-GMN-YBUTAGAIKBNMEIL PIC X(30).
               05  SPA-GMN-YKZHEIMSTKBNLST-G.
                   07  SPA-GMN-YKZHEIMSTKBN-MAX    PIC 9(03).
                   07  SPA-GMN-YKZHEIMSTKBNLST     OCCURS   2.
                       09  SPA-GMN-YKZHEIMSTKBNL   PIC X(01).
                       09  SPA-GMN-YKZHEIMSTKBNFLL PIC X(01).
                       09  SPA-GMN-YKZHEIMSTKBNMEIL PIC X(30).
               05  SPA-GMN-SRYHEIWRNKBNLST-G.
                   07  SPA-GMN-SRYHEIWRNKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYHEIWRNKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYHEIWRNKBNL   PIC X(01).
                       09  SPA-GMN-SRYHEIWRNKBNFLL PIC X(01).
                       09  SPA-GMN-SRYHEIWRNKBNMEIL PIC X(30).
               05  SPA-GMN-SRYHEIMSTKBNLST-G.
                   07  SPA-GMN-SRYHEIMSTKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYHEIMSTKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYHEIMSTKBNL   PIC X(01).
                       09  SPA-GMN-SRYHEIMSTKBNFLL PIC X(01).
                       09  SPA-GMN-SRYHEIMSTKBNMEIL PIC X(30).
               05  SPA-GMN-SRYHEIEXPKBNLST-G.
                   07  SPA-GMN-SRYHEIEXPKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYHEIEXPKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYHEIEXPKBNL   PIC X(01).
                       09  SPA-GMN-SRYHEIEXPKBNFLL PIC X(01).
                       09  SPA-GMN-SRYHEIEXPKBNMEIL PIC X(30).
               05  SPA-GMN-SRYMATERKKBNLST-G.
                   07  SPA-GMN-SRYMATERKKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYMATERKKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYMATERKKBNL   PIC X(01).
                       09  SPA-GMN-SRYMATERKKBNFLL PIC X(01).
                       09  SPA-GMN-SRYMATERKKBNMEIL PIC X(30).
               05  SPA-GMN-SRYKAISUMSTKBNLST-G.
                   07  SPA-GMN-SRYKAISUMSTKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYKAISUMSTKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYKAISUMSTKBNL   PIC X(01).
                       09  SPA-GMN-SRYKAISUMSTKBNFLL PIC X(01).
                       09  SPA-GMN-SRYKAISUMSTKBNMEIL PIC X(20).
               05  SPA-GMN-SRYKAISUWRNKBNLST-G.
                   07  SPA-GMN-SRYKAISUWRNKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYKAISUWRNKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYKAISUWRNKBNL   PIC X(01).
                       09  SPA-GMN-SRYKAISUWRNKBNFLL PIC X(01).
                       09  SPA-GMN-SRYKAISUWRNKBNMEIL PIC X(20).
               05  SPA-GMN-INGAIPROCKBNLST-G.
                   07  SPA-GMN-INGAIPROCKBN-MAX     PIC 9(03).
                   07  SPA-GMN-INGAIPROCKBNLST      OCCURS   3.
                       09  SPA-GMN-INGAIPROCKBNL    PIC X(01).
                       09  SPA-GMN-INGAIPROCKBNFLL  PIC X(01).
                       09  SPA-GMN-INGAIPROCKBNMEIL PIC X(18).
               05  SPA-GMN-REIGAICHKKBNLST-G.
                   07  SPA-GMN-REIGAICHKKBN-MAX    PIC 9(03).
                   07  SPA-GMN-REIGAICHKKBNLST     OCCURS   2.
                       09  SPA-GMN-REIGAICHKKBNL   PIC X(01).
                       09  SPA-GMN-REIGAICHKKBNFLL PIC X(01).
                       09  SPA-GMN-REIGAICHKKBNMEIL PIC X(18).
               05  SPA-GMN-PRTSRTKBNLST-G.
                   07  SPA-GMN-PRTSRTKBN-MAX       PIC 9(03).
                   07  SPA-GMN-PRTSRTKBNLST        OCCURS   4.
                       09  SPA-GMN-PRTSRTKBNL      PIC X(01).
                       09  SPA-GMN-PRTSRTKBNFLL    PIC X(01).
                       09  SPA-GMN-PRTSRTKBNMEIL   PIC X(20).
               05  SPA-GMN-PRTPTLSTKBNLST-G.
                   07  SPA-GMN-PRTPTLSTKBN-MAX     PIC 9(03).
                   07  SPA-GMN-PRTPTLSTKBNLST      OCCURS   2.
                       09  SPA-GMN-PRTPTLSTKBNL    PIC X(01).
                       09  SPA-GMN-PRTPTLSTKBNFLL  PIC X(01).
                       09  SPA-GMN-PRTPTLSTKBNMEIL PIC X(20).
               05  SPA-GMN-ERR990009KBNLST-G.
                   07  SPA-GMN-ERR990009KBN-MAX    PIC 9(03).
                   07  SPA-GMN-ERR990009KBNLST     OCCURS   5.
                       09  SPA-GMN-ERR990009KBNL   PIC X(01).
                       09  SPA-GMN-ERR990009KBNFLL PIC X(01).
                       09  SPA-GMN-ERR990009KBNMEIL
                                                   PIC X(20).
               05  SPA-GMN-ERRA10001KBNLST-G.
                   07  SPA-GMN-ERRA10001KBN-MAX    PIC 9(03).
                   07  SPA-GMN-ERRA10001KBNLST     OCCURS   5.
                       09  SPA-GMN-ERRA10001KBNL   PIC X(01).
                       09  SPA-GMN-ERRA10001KBNFLL PIC X(01).
                       09  SPA-GMN-ERRA10001KBNMEIL
                                                   PIC X(20).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-MTRX                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG                  PIC X(70).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM             PIC X(200).
           03  WRK-PAGENO                  PIC 9(03).
      *
       01  CONST-AREA.
           03  CONST-CHKMTRX-MAX           PIC 9(03)   VALUE 5.
           03  CONST-CHKMTRX-VAL           PIC X(05)   VALUE "1 111".
           03  CONST-CHKMTRX-R REDEFINES   CONST-CHKMTRX-VAL.
               05  CONST-CHKMTRX           PIC X(01)   OCCURS  5.
           03  CONST-PAGE-MAX              PIC 9(03)   VALUE 2.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1101.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
           COPY    "CPCK1103.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W60
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W60         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W60    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-W60
           INITIALIZE                  SPA-W60
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    CONST-CHKMTRX (IDX-MTRX)
                                   TO  SPA-GMN-CHKMTRX (IDX-MTRX)
      *
           END-PERFORM
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    SPACE           TO  WRK-STYUKYMD
           PERFORM 3101-SPASET-SEC
      *
           PERFORM 3102-SPASET-SEC
      *
           MOVE    SPA-GMN-SEIHOJNUMCHKKBN TO  WRK-CMB-CD
           PERFORM 4200-SEIHOJNUMCHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SEIHOJNUMCHKKBNLST (2)
                                   TO  SPA-GMN-SEIHOJNUMCHKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SEIHOJNUMCHKKBN-G
           END-IF
      *
           MOVE    SPA-GMN-BYOSRYKAKBN TO  WRK-CMB-CD
           PERFORM 4200-BYOSRYKAKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-BYOSRYKAKBNLST (1)
                                   TO  SPA-GMN-BYOSRYKAKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-BYOSRYKAKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SYUBYOCHK TO  WRK-CMB-CD
           PERFORM 4200-SYUBYOCHKLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYUBYOCHKLST (2)
                                   TO  SPA-GMN-SYUBYOCHK-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SYUBYOCHK-G
           END-IF
      *
           IF    ( SPA-GMN-UTAGAIKKN    =   ZERO )
               MOVE    2            TO  SPA-GMN-UTAGAIKKN
           END-IF
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN TO  WRK-CMB-CD
           PERFORM 4200-KYUSEIKKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KYUSEIKKNKBNLST (1)
                                   TO  SPA-GMN-KYUSEIKKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KYUSEIKKNKBN-G
           END-IF
      *
           MOVE    SPA-GMN-COMPBYONISSUKBN TO  WRK-CMB-CD
           PERFORM 4200-COMPBYONISSUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-COMPBYONISSUKBNLST (1)
                                   TO  SPA-GMN-COMPBYONISSUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-COMPBYONISSUKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYSTTENKIKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYSTTENKIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYSTTENKIKBNLST (1)
                                   TO  SPA-GMN-SRYSTTENKIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYSTTENKIKBN-G
           END-IF
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN TO  WRK-CMB-CD
           PERFORM 4200-TENKITEKBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TENKITEKBYOKBNLST (1)
                                       TO  SPA-GMN-TENKITEKBYOKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TENKITEKBYOKBN-G
           END-IF
      *
           MOVE    SPA-GMN-TEKBYOYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-TEKBYOYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TEKBYOYKZKBNLST (1)
                                       TO  SPA-GMN-TEKBYOYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TEKBYOYKZKBN-G
           END-IF
      *
           MOVE    SPA-GMN-TEKBYOSRYKBN TO  WRK-CMB-CD
           PERFORM 4200-TEKBYOSRYKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TEKBYOSRYKBNLST (1)
                                       TO  SPA-GMN-TEKBYOSRYKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TEKBYOSRYKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN TO  WRK-CMB-CD
           PERFORM 4200-YBKENSAYAKUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBKENSAYAKUKBNLST (1)
                                       TO  SPA-GMN-YBKENSAYAKUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBKENSAYAKUKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBSJTYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-YBSJTYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBSJTYKZKBNLST (1)
                                       TO  SPA-GMN-YBSJTYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBSJTYKZKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBMSIYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-YBMSIYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBMSIYKZKBNLST (1)
                                       TO  SPA-GMN-YBMSIYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBMSIYKZKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBSHOHOYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-YBSHOHOYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBSHOHOYKZKBNLST (1)
                                       TO  SPA-GMN-YBSHOHOYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBSHOHOYKZKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBUTAGAIKBN TO  WRK-CMB-CD
           PERFORM 4200-YBUTAGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBUTAGAIKBNLST (1)
                                       TO  SPA-GMN-YBUTAGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBUTAGAIKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YKZHEIMSTKBN TO  WRK-CMB-CD
           PERFORM 4200-YKZHEIMSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YKZHEIMSTKBNLST (2)
                                       TO  SPA-GMN-YKZHEIMSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YKZHEIMSTKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIWRNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIWRNKBNLST (1)
                                       TO  SPA-GMN-SRYHEIWRNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYHEIWRNKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYHEIMSTKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIMSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIMSTKBNLST (2)
                                       TO  SPA-GMN-SRYHEIMSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYHEIMSTKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYHEIEXPKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIEXPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIEXPKBNLST (2)
                                       TO  SPA-GMN-SRYHEIEXPKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYHEIEXPKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYMATERKKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYMATERKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYMATERKKBNLST (2)
                                       TO  SPA-GMN-SRYMATERKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYMATERKKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYKAISUMSTKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYKAISUMSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYKAISUMSTKBNLST (2)
                                       TO  SPA-GMN-SRYKAISUMSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYKAISUMSTKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYKAISUWRNKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYKAISUWRNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYKAISUWRNKBNLST (2)
                                       TO  SPA-GMN-SRYKAISUWRNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYKAISUWRNKBN-G
           END-IF
      *
           MOVE    SPA-GMN-INGAIPROCKBN TO  WRK-CMB-CD
           PERFORM 4200-INGAIPROCKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-INGAIPROCKBNLST (2)
                                       TO  SPA-GMN-INGAIPROCKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-INGAIPROCKBN-G
           END-IF
      *
           MOVE    SPA-GMN-ERR990009KBN TO  WRK-CMB-CD
           PERFORM 4200-ERR990009KBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ERR990009KBNLST (1)
                                       TO  SPA-GMN-ERR990009KBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-ERR990009KBN-G
           END-IF
      *
           MOVE    SPA-GMN-ERRA10001KBN TO  WRK-CMB-CD
           PERFORM 4200-ERRA10001KBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ERRA10001KBNLST (1)
                                       TO  SPA-GMN-ERRA10001KBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-ERRA10001KBN-G
           END-IF
      *
           MOVE    SPA-GMN-REIGAICHKKBN TO  WRK-CMB-CD
           PERFORM 4200-REIGAICHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-REIGAICHKKBNLST (1)
                                       TO  SPA-GMN-REIGAICHKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-REIGAICHKKBN-G
           END-IF
      *
           MOVE    SPA-GMN-PRTSRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-PRTSRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTSRTKBNLST (1)
                                   TO  SPA-GMN-PRTSRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTSRTKBN-G
           END-IF
      *
           MOVE    SPA-GMN-PRTPTLSTKBN TO  WRK-CMB-CD
           PERFORM 4200-PRTPTLSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTPTLSTKBNLST (1)
                                   TO  SPA-GMN-PRTPTLSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTPTLSTKBN-G
           END-IF
      *
           MOVE    101             TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理(システム管理より)
      *****************************************************************
       3101-SPASET-SEC                 SECTION.
      *
           INITIALIZE                          SYS-1101-REC
           MOVE    "1101"                  TO  SYS-1101-KANRICD
           MOVE    "*"                     TO  SYS-1101-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYS-1101-STYUKYMD
                                               SYS-1101-EDYUKYMD
           MOVE    SYS-1101-REC            TO  SYSKANRI-REC
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           ELSE
               PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           END-IF
           IF    ( FLG-SYSKANRI        =   ZERO )
      *
               MOVE    SYSKANRI-REC        TO  SYS-1101-REC
      *
               PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                       UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
                   MOVE    SYS-1101-BYOCHKPTN(IDX-MTRX)
                                       TO  SPA-GMN-CHKMTRX (IDX-MTRX)
      *
               END-PERFORM
      *
               MOVE    SYS-1101-SEIHOJNUMCHKKBN
                                       TO  SPA-GMN-SEIHOJNUMCHKKBN
      *
               MOVE    SYS-1101-BYOSRYKAKBN
                                       TO  SPA-GMN-BYOSRYKAKBN
      *
               MOVE    SYS-1101-SYUBYOCHKKBN
                                       TO  SPA-GMN-SYUBYOCHK
      *
               IF    ( SYS-1101-UTAGAIKKN-X  IS NUMERIC )
      *
                   MOVE    SYS-1101-UTAGAIKKN
                                       TO  SPA-GMN-UTAGAIKKN
               ELSE
                   MOVE    2           TO  SPA-GMN-UTAGAIKKN
               END-IF
      *
               IF    ( SYS-1101-KYUSEIKKN-X  IS NUMERIC )
      *
                   EVALUATE    SYS-1101-KYUSEIKKN
                   WHEN    0
                       MOVE    "0"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    1
                       MOVE    "1"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    2
                       MOVE    "2"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    3
                       MOVE    "3"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    4
                       MOVE    "4"     TO  SPA-GMN-KYUSEIKKNKBN
                   END-EVALUATE
      *
               END-IF
      *
               MOVE    SYS-1101-COMPBYONISSUKBN
                                       TO  SPA-GMN-COMPBYONISSUKBN
      *
               MOVE    SYS-1101-SRYSTTENKIKBN
                                       TO  SPA-GMN-SRYSTTENKIKBN
      *
               MOVE    SYS-1101-PRTSRTKBN
                                       TO  SPA-GMN-PRTSRTKBN
      *
               MOVE    SYS-1101-PRTPTLSTKBN
                                       TO  SPA-GMN-PRTPTLSTKBN
      *
               MOVE    SYS-1101-YBUTAGAIKBN
                                       TO  SPA-GMN-YBUTAGAIKBN
      *
               MOVE    SYS-1101-YKZHEIMSTKBN
                                       TO  SPA-GMN-YKZHEIMSTKBN
      *
               MOVE    SYS-1101-SRYHEIWRNKBN
                                       TO  SPA-GMN-SRYHEIWRNKBN
      *
               MOVE    SYS-1101-SRYHEIMSTKBN
                                       TO  SPA-GMN-SRYHEIMSTKBN
      *
               MOVE    SYS-1101-SRYHEIEXPKBN
                                       TO  SPA-GMN-SRYHEIEXPKBN
      *
               MOVE    SYS-1101-SRYMATERKKBN
                                       TO  SPA-GMN-SRYMATERKKBN
      *
               MOVE    SYS-1101-SRYKAISUMSTKBN
                                       TO  SPA-GMN-SRYKAISUMSTKBN
      *
               MOVE    SYS-1101-SRYKAISUWRNKBN
                                       TO  SPA-GMN-SRYKAISUWRNKBN
      *
               MOVE    SYS-1101-INGAIPROCKBN
                                       TO  SPA-GMN-INGAIPROCKBN
      *
               MOVE    SYS-1101-REIGAICHKKBN
                                       TO  SPA-GMN-REIGAICHKKBN
      *
               MOVE    SYS-1101-YBKENSAYAKUKBN
                                       TO  SPA-GMN-YBKENSAYAKUKBN
               MOVE    SYS-1101-YBSJTYKZKBN
                                       TO  SPA-GMN-YBSJTYKZKBN
               MOVE    SYS-1101-YBMSIYKZKBN
                                       TO  SPA-GMN-YBMSIYKZKBN
               MOVE    SYS-1101-YBSHOHOYKZKBN
                                       TO  SPA-GMN-YBSHOHOYKZKBN
               MOVE    SYS-1101-TENKITEKBYOKBN
                                       TO  SPA-GMN-TENKITEKBYOKBN
               MOVE    SYS-1101-TEKBYOYKZKBN
                                       TO  SPA-GMN-TEKBYOYKZKBN
               MOVE    SYS-1101-TEKBYOSRYKBN
                                       TO  SPA-GMN-TEKBYOSRYKBN
               IF    ( SYS-1101-SYA-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-SYA-INGAITEN
                                       TO  SPA-GMN-SYA-INGAITEN
               END-IF
               IF    ( SYS-1101-KOK-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-KOK-INGAITEN
                                       TO  SPA-GMN-KOK-INGAITEN
               END-IF
               IF    ( SYS-1101-KOU-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-KOU-INGAITEN
                                       TO  SPA-GMN-KOU-INGAITEN
               END-IF
               IF    ( SYS-1101-RSI-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-RSI-INGAITEN
                                       TO  SPA-GMN-RSI-INGAITEN
               END-IF
               IF    ( SYS-1101-JBI-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-JBI-INGAITEN
                                       TO  SPA-GMN-JBI-INGAITEN
               END-IF
               IF    ( SYS-1101-JIH-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-JIH-INGAITEN
                                       TO  SPA-GMN-JIH-INGAITEN
               END-IF
               IF    ( SYS-1101-KGI-INGAITEN   IS  NUMERIC )
                   MOVE    SYS-1101-KGI-INGAITEN
                                       TO  SPA-GMN-KGI-INGAITEN
               END-IF
      *
               MOVE    SYS-1101-STYUKYMD
                                       TO  WRK-STYUKYMD
      *
           END-IF
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理(チェックマスタ４より)
      *****************************************************************
       3102-SPASET-SEC                 SECTION.
      *
           INITIALIZE                          CK4-1103-REC
           MOVE    "1103"                  TO  CK4-1103-KANRICD
           MOVE    WRK-STYUKYMD            TO  CK4-1103-STYUKYMD
                                               CK4-1103-EDYUKYMD
           MOVE    CK4-1103-REC            TO  CHK004-REC
           PERFORM 900-CHK004-KEY10-SEL-SEC
           MOVE    CHK004-REC              TO  CK4-1103-REC
      *
           PERFORM UNTIL ( FLG-CHK004      NOT =   ZERO )
      *
               EVALUATE    CK4-1103-KBNCD
               WHEN    "99"
                   MOVE    CK4-1103-ERRKBN (9)
                                           TO  SPA-GMN-ERR990009KBN
               WHEN    "A1"
                   MOVE    CK4-1103-ERRKBN (1)
                                           TO  SPA-GMN-ERRA10001KBN
               END-EVALUATE
      *
               PERFORM 900-CHK004-KEY10-FET-SEC
               MOVE    CHK004-REC      TO  CK4-1103-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3102-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    生保受給者番号抜けチェック
           PERFORM 3101-SEIHOJNUMCHKKBNLST-SEC
      *
      *    診療科別チェックコンボ設定処理
           PERFORM 3101-BYOSRYKAKBNLST-SEC
      *
      *    主病名チェックコンボ設定処理
           PERFORM 3101-SYUBYOCHKLST-SEC
      *
      *    急性病名期間チェックコンボ設定処理
           PERFORM 3101-KYUSEIKKNKBNLST-SEC
      *
      *    診療開始日と実日数の比較区分コンボ設定処理
           PERFORM 3101-COMPBYONISSUKBNLST-SEC
      *
      *    「中止」の取り扱いコンボ設定処理
           PERFORM 3101-SRYSTTENKIKBNLST-SEC
      *
      *    過月転帰済適応病名コンボ設定処理
           PERFORM 3101-TENKITEKBYOKBNLST-SEC
      *
      *    適応病名マスタ使用（薬剤と病名）コンボ設定処理
           PERFORM 3101-TEKBYOYKZKBNLST-SEC
      *
      *    適応病名マスタ使用（診療行為と病名）コンボ設定処理
           PERFORM 3101-TEKBYOSRYKBNLST-SEC
      *
      *    検査薬剤対象コンボ設定処理
           PERFORM 3101-YBKENSAYAKUKBNLST-SEC
      *
      *    手術薬剤対象コンボ設定処理
           PERFORM 3101-YBSJTYKZKBNLST-SEC
      *
      *    麻酔薬剤対象コンボ設定処理
           PERFORM 3101-YBMSIYKZKBNLST-SEC
      *
      *    処方のみの薬剤対象コンボ設定処理
           PERFORM 3101-YBSHOHOYKZKBNLST-SEC
      *
      *    薬剤と病名疑いコンボ設定処理
           PERFORM 3101-YBUTAGAIKBNLST-SEC
      *
      *    相互作用マスタ参照コンボ設定処理
           PERFORM 3101-YKZHEIMSTKBNLST-SEC
      *
      *    併用算定警告エラーコンボ設定処理
           PERFORM 3101-SRYHEIWRNKBNLST-SEC
      *
      *    併用算定電子点数表参照コンボ設定処理
           PERFORM 3101-SRYHEIMSTKBNLST-SEC
      *
      *    併用算定電子点数表特例コンボ設定処理
           PERFORM 3101-SRYHEIEXPKBNLST-SEC
      *
      *    算定漏れ特定器材関連手技マスタ参照コンボ設定処理
           PERFORM 3101-SRYMATERKKBNLST-SEC
      *
      *    電子点数表回数参照コンボ設定処理
           PERFORM 3101-SRYKAISUMSTKBNLST-SEC
      *
      *    点数マスタ警告エラーコンボ設定処理
           PERFORM 3101-SRYKAISUWRNKBNLST-SEC
      *
      *    院外処方の処理コンボ設定処理
           PERFORM 3101-INGAIPROCKBNLST-SEC
      *
      *    その他のチェックエラーコンボ設定処理
           PERFORM 3101-ERR990009KBNLST-SEC
      *
      *    「薬剤と病名」に未登録の薬剤チェックコンボ設定処理
           PERFORM 3101-ERRA10001KBNLST-SEC
      *
      *    患者毎の例外設定使用コンボ設定処理
           PERFORM 3101-REIGAICHKKBNLST-SEC
      *
      *    帳票出力順コンボ設定処理
           PERFORM 3101-PRTSRTKBNLST-SEC
      *
      *    患者別チェックリスト設定処理
           PERFORM 3101-PRTPTLSTKBNLST-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    生保受給者番号抜けチェックコンボ設定処理
      *****************************************************************
       3101-SEIHOJNUMCHKKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SEIHOJNUMCHKKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SEIHOJNUMCHKKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SEIHOJNUMCHKKBNL    (1)
           MOVE    "チェックする"  TO  SPA-GMN-SEIHOJNUMCHKKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SEIHOJNUMCHKKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-SEIHOJNUMCHKKBNMEIL (2)
      *
           MOVE    SPA-GMN-SEIHOJNUMCHKKBNLST  (2)
                                   TO  SPA-GMN-SEIHOJNUMCHKKBN-G
      *
           . 
       3101-SEIHOJNUMCHKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    診療科別チェックコンボ設定処理
      *****************************************************************
       3101-BYOSRYKAKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-BYOSRYKAKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-BYOSRYKAKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-BYOSRYKAKBNL    (1)
           MOVE    "する"          TO  SPA-GMN-BYOSRYKAKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-BYOSRYKAKBNL    (2)
           MOVE    "しない"        TO  SPA-GMN-BYOSRYKAKBNMEIL (2)
      *
           MOVE    SPA-GMN-BYOSRYKAKBNLST  (1)
                                   TO  SPA-GMN-BYOSRYKAKBN-G
      *
           . 
       3101-BYOSRYKAKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    主病名チェックコンボ設定処理
      *****************************************************************
       3101-SYUBYOCHKLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SYUBYOCHKLST-G
      *
           MOVE    2               TO  SPA-GMN-SYUBYOCHK-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SYUBYOCHKL    (1)
           MOVE    "する"          TO  SPA-GMN-SYUBYOCHKMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SYUBYOCHKL    (2)
           MOVE    "しない"        TO  SPA-GMN-SYUBYOCHKMEIL (2)
      *
           MOVE    SPA-GMN-SYUBYOCHKLST  (2)
                                   TO  SPA-GMN-SYUBYOCHK-G
      *
           . 
       3101-SYUBYOCHKLST-EXT.
           EXIT.
      *****************************************************************
      *    急性病名期間チェックコンボ設定処理
      *****************************************************************
       3101-KYUSEIKKNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-KYUSEIKKNKBNLST-G
      *
           MOVE    5               TO  SPA-GMN-KYUSEIKKNKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-KYUSEIKKNKBNL    (1)
           MOVE    "しない"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-KYUSEIKKNKBNL    (2)
           MOVE    "１ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-KYUSEIKKNKBNL    (3)
           MOVE    "２ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-KYUSEIKKNKBNL    (4)
           MOVE    "３ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-KYUSEIKKNKBNL    (5)
           MOVE    "４ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (5)
      *
           MOVE    SPA-GMN-KYUSEIKKNKBNLST  (1)
                                   TO  SPA-GMN-KYUSEIKKNKBN-G
      *
           . 
       3101-KYUSEIKKNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日と実日数の比較区分コンボ設定処理
      *****************************************************************
       3101-COMPBYONISSUKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-COMPBYONISSUKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-COMPBYONISSUKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-COMPBYONISSUKBNL    (1)
           MOVE    "チェックする"  TO  SPA-GMN-COMPBYONISSUKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-COMPBYONISSUKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-COMPBYONISSUKBNMEIL (2)
      *
           MOVE    SPA-GMN-COMPBYONISSUKBNLST  (1)
                                   TO  SPA-GMN-COMPBYONISSUKBN-G
      *
           . 
       3101-COMPBYONISSUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    「中止」の取り扱いコンボ設定処理
      *****************************************************************
       3101-SRYSTTENKIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYSTTENKIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYSTTENKIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYSTTENKIKBNL    (1)
           MOVE    "転帰日から１ヶ月経過後に初診料算定可能"
                                   TO  SPA-GMN-SRYSTTENKIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYSTTENKIKBNL    (2)
           MOVE    "転帰日から１ヶ月以内でも初診料算定可能"
                                   TO  SPA-GMN-SRYSTTENKIKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYSTTENKIKBNLST  (1)
                                   TO  SPA-GMN-SRYSTTENKIKBN-G
      *
           . 
       3101-SRYSTTENKIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    過月転帰済適応病名コンボ設定処理
      *****************************************************************
       3101-TENKITEKBYOKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-TENKITEKBYOKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-TENKITEKBYOKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-TENKITEKBYOKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-TENKITEKBYOKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-TENKITEKBYOKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-TENKITEKBYOKBNMEIL (2)
      *
           MOVE    SPA-GMN-TENKITEKBYOKBNLST  (1)
                                   TO  SPA-GMN-TENKITEKBYOKBN-G
      *
           . 
       3101-TENKITEKBYOKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ使用（薬剤と病名）コンボ設定処理
      *****************************************************************
       3101-TEKBYOYKZKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-TEKBYOYKZKBNLST-G
      *
           MOVE    3               TO  SPA-GMN-TEKBYOYKZKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-TEKBYOYKZKBNL    (1)
           MOVE    "行わない"
                                   TO  SPA-GMN-TEKBYOYKZKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-TEKBYOYKZKBNL    (2)
           MOVE    "行う"
                                   TO  SPA-GMN-TEKBYOYKZKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-TEKBYOYKZKBNL    (3)
           MOVE    "チェックマスタにない薬剤のみ行う"
                                   TO  SPA-GMN-TEKBYOYKZKBNMEIL (3)
      *
           MOVE    SPA-GMN-TEKBYOYKZKBNLST  (1)
                                   TO  SPA-GMN-TEKBYOYKZKBN-G
      *
           . 
       3101-TEKBYOYKZKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ使用（薬剤と病名）コンボ設定処理
      *****************************************************************
       3101-TEKBYOSRYKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-TEKBYOSRYKBNLST-G
      *
           MOVE    3               TO  SPA-GMN-TEKBYOSRYKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-TEKBYOSRYKBNL    (1)
           MOVE    "行わない"
                                   TO  SPA-GMN-TEKBYOSRYKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-TEKBYOSRYKBNL    (2)
           MOVE    "行う"
                                   TO  SPA-GMN-TEKBYOSRYKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-TEKBYOSRYKBNL    (3)
           MOVE    "チェックマスタにない診療行為のみ行う"
                                   TO  SPA-GMN-TEKBYOSRYKBNMEIL (3)
      *
           MOVE    SPA-GMN-TEKBYOSRYKBNLST  (1)
                                   TO  SPA-GMN-TEKBYOSRYKBN-G
      *
           . 
       3101-TEKBYOSRYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象コンボ設定処理
      *****************************************************************
       3101-YBKENSAYAKUKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-YBKENSAYAKUKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBKENSAYAKUKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBKENSAYAKUKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-YBKENSAYAKUKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBKENSAYAKUKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-YBKENSAYAKUKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBNLST  (1)
                                   TO  SPA-GMN-YBKENSAYAKUKBN-G
      *
           . 
       3101-YBKENSAYAKUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象コンボ設定処理
      *****************************************************************
       3101-YBSJTYKZKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-YBSJTYKZKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBSJTYKZKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBSJTYKZKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-YBSJTYKZKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBSJTYKZKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-YBSJTYKZKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBSJTYKZKBNLST  (1)
                                   TO  SPA-GMN-YBSJTYKZKBN-G
      *
           . 
       3101-YBSJTYKZKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象コンボ設定処理
      *****************************************************************
       3101-YBMSIYKZKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-YBMSIYKZKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBMSIYKZKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBMSIYKZKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-YBMSIYKZKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBMSIYKZKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-YBMSIYKZKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBMSIYKZKBNLST  (1)
                                   TO  SPA-GMN-YBMSIYKZKBN-G
      *
           . 
       3101-YBMSIYKZKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象コンボ設定処理
      *****************************************************************
       3101-YBSHOHOYKZKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-YBSHOHOYKZKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBSHOHOYKZKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBSHOHOYKZKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-YBSHOHOYKZKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBSHOHOYKZKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-YBSHOHOYKZKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBSHOHOYKZKBNLST  (1)
                                   TO  SPA-GMN-YBSHOHOYKZKBN-G
      *
           . 
       3101-YBSHOHOYKZKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名疑いコンボ設定処理
      *****************************************************************
       3101-YBUTAGAIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-YBUTAGAIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBUTAGAIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBUTAGAIKBNL    (1)
           MOVE    "適応病名とする"
                                   TO  SPA-GMN-YBUTAGAIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBUTAGAIKBNL    (2)
           MOVE    "適応病名としない"
                                   TO  SPA-GMN-YBUTAGAIKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBUTAGAIKBNLST  (1)
                                   TO  SPA-GMN-YBUTAGAIKBN-G
      *
           . 
       3101-YBUTAGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    相互作用マスタ参照コンボ設定処理
      *****************************************************************
       3101-YKZHEIMSTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-YKZHEIMSTKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YKZHEIMSTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YKZHEIMSTKBNL    (1)
           MOVE    "参照する"
                                   TO  SPA-GMN-YKZHEIMSTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YKZHEIMSTKBNL    (2)
           MOVE    "参照しない"
                                   TO  SPA-GMN-YKZHEIMSTKBNMEIL (2)
      *
           MOVE    SPA-GMN-YKZHEIMSTKBNLST  (2)
                                   TO  SPA-GMN-YKZHEIMSTKBN-G
      *
           . 
       3101-YKZHEIMSTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    併用算定警告エラーコンボ設定処理
      *****************************************************************
       3101-SRYHEIWRNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYHEIWRNKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYHEIWRNKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYHEIWRNKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-SRYHEIWRNKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYHEIWRNKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-SRYHEIWRNKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYHEIWRNKBNLST  (1)
                                   TO  SPA-GMN-SRYHEIWRNKBN-G
      *
           . 
       3101-SRYHEIWRNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    併用算定電子点数表参照コンボ設定処理
      *****************************************************************
       3101-SRYHEIMSTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYHEIMSTKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYHEIMSTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYHEIMSTKBNL    (1)
           MOVE    "参照する"
                                   TO  SPA-GMN-SRYHEIMSTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYHEIMSTKBNL    (2)
           MOVE    "参照しない"
                                   TO  SPA-GMN-SRYHEIMSTKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYHEIMSTKBNLST  (2)
                                   TO  SPA-GMN-SRYHEIMSTKBN-G
      *
           . 
       3101-SRYHEIMSTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    併用算定電子点数表特例コンボ設定処理
      *****************************************************************
       3101-SRYHEIEXPKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYHEIEXPKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYHEIEXPKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYHEIEXPKBNL    (1)
           MOVE    "特例条件があるものも参照する"
                                   TO  SPA-GMN-SRYHEIEXPKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYHEIEXPKBNL    (2)
           MOVE    "特例条件があるものは参照しない"
                                   TO  SPA-GMN-SRYHEIEXPKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYHEIEXPKBNLST  (2)
                                   TO  SPA-GMN-SRYHEIEXPKBN-G
      *
           . 
       3101-SRYHEIEXPKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    算定漏れ特定器材関連手技マスタ参照コンボ設定処理
      *****************************************************************
       3101-SRYMATERKKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYMATERKKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYMATERKKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYMATERKKBNL    (1)
           MOVE    "参照する"
                                   TO  SPA-GMN-SRYMATERKKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYMATERKKBNL    (2)
           MOVE    "参照しない"
                                   TO  SPA-GMN-SRYMATERKKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYMATERKKBNLST  (2)
                                   TO  SPA-GMN-SRYMATERKKBN-G
      *
           . 
       3101-SRYMATERKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表回数参照コンボ設定処理
      *****************************************************************
       3101-SRYKAISUMSTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKAISUMSTKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYKAISUMSTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYKAISUMSTKBNL    (1)
           MOVE    "参照する"      TO  SPA-GMN-SRYKAISUMSTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYKAISUMSTKBNL    (2)
           MOVE    "参照しない"    TO  SPA-GMN-SRYKAISUMSTKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYKAISUMSTKBNLST  (2)
                                   TO  SPA-GMN-SRYKAISUMSTKBN-G
      *
           . 
       3101-SRYKAISUMSTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ警告エラーコンボ設定処理
      *****************************************************************
       3101-SRYKAISUWRNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKAISUWRNKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYKAISUWRNKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYKAISUWRNKBNL    (1)
           MOVE    "チェックする"  TO  SPA-GMN-SRYKAISUWRNKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYKAISUWRNKBNL    (2)
           MOVE    "チェックしない" TO SPA-GMN-SRYKAISUWRNKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYKAISUWRNKBNLST  (2)
                                   TO  SPA-GMN-SRYKAISUWRNKBN-G
      *
           . 
       3101-SRYKAISUWRNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理項目コンボ編集処理
      *****************************************************************
       3101-INGAIPROCKBNLST-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-INGAIPROCKBNLST-G
           MOVE    3               TO  SPA-GMN-INGAIPROCKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-INGAIPROCKBNL    (1)
           MOVE    "含まない"      TO  SPA-GMN-INGAIPROCKBNMEIL (1)
           MOVE    "1"             TO  SPA-GMN-INGAIPROCKBNL    (2)
           MOVE    "院外処方を含む"
                                   TO  SPA-GMN-INGAIPROCKBNMEIL (2)
           MOVE    "2"             TO  SPA-GMN-INGAIPROCKBNL    (3)
           MOVE    "院外処方のみ"  TO  SPA-GMN-INGAIPROCKBNMEIL (3)
      *
           MOVE    SPA-GMN-INGAIPROCKBNLST  (2)
                                   TO  SPA-GMN-INGAIPROCKBN-G
      *
           .
       3101-INGAIPROCKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    その他エラーコンボ設定処理
      *****************************************************************
       3101-ERR990009KBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-ERR990009KBNLST-G
      *
           MOVE    2               TO  SPA-GMN-ERR990009KBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-ERR990009KBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-ERR990009KBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-ERR990009KBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-ERR990009KBNMEIL (2)
      *
           MOVE    SPA-GMN-ERR990009KBNLST  (1)
                                   TO  SPA-GMN-ERR990009KBN-G
      *
           . 
       3101-ERR990009KBNLST-EXT.
           EXIT.
      *****************************************************************
      *    「薬剤と病名」に未登録の薬剤チェックコンボ設定処理
      *****************************************************************
       3101-ERRA10001KBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-ERRA10001KBNLST-G
      *
           MOVE    2               TO  SPA-GMN-ERRA10001KBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-ERRA10001KBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-ERRA10001KBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-ERRA10001KBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-ERRA10001KBNMEIL (2)
      *
           MOVE    SPA-GMN-ERRA10001KBNLST  (2)
                                   TO  SPA-GMN-ERRA10001KBN-G
      *
           . 
       3101-ERRA10001KBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者毎の例外設定使用コンボ設定処理
      *****************************************************************
       3101-REIGAICHKKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-REIGAICHKKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-REIGAICHKKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-REIGAICHKKBNL    (1)
           MOVE    "使用しない"    TO  SPA-GMN-REIGAICHKKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-REIGAICHKKBNL    (2)
           MOVE    "使用する"      TO  SPA-GMN-REIGAICHKKBNMEIL (2)
      *
           MOVE    SPA-GMN-REIGAICHKKBNLST  (1)
                                   TO  SPA-GMN-REIGAICHKKBN-G
      *
           . 
       3101-REIGAICHKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順コンボ設定処理
      *****************************************************************
       3101-PRTSRTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-PRTSRTKBNLST-G
      *
           MOVE    4               TO  SPA-GMN-PRTSRTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-PRTSRTKBNL    (1)
           MOVE    "患者番号順"    TO  SPA-GMN-PRTSRTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-PRTSRTKBNL    (2)
           MOVE    "カナ氏名順"    TO  SPA-GMN-PRTSRTKBNMEIL (2)
      *
           MOVE    "3"                 TO  SPA-GMN-PRTSRTKBNL    (3)
           MOVE    "保険、患者番号順"  TO  SPA-GMN-PRTSRTKBNMEIL (3)
      *
           MOVE    "4"                 TO  SPA-GMN-PRTSRTKBNL    (4)
           MOVE    "保険、カナ氏名順"  TO  SPA-GMN-PRTSRTKBNMEIL (4)
      *
           MOVE    SPA-GMN-PRTSRTKBNLST  (1)
                                   TO  SPA-GMN-PRTSRTKBN-G
      *
           . 
       3101-PRTSRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者別チェックリスト設定処理
      *****************************************************************
       3101-PRTPTLSTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-PRTPTLSTKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-PRTPTLSTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-PRTPTLSTKBNL    (1)
           MOVE    "作成しない"    TO  SPA-GMN-PRTPTLSTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-PRTPTLSTKBNL    (2)
           MOVE    "作成する"      TO  SPA-GMN-PRTPTLSTKBNMEIL (2)
      *
           MOVE    SPA-GMN-PRTPTLSTKBNLST  (1)
                                   TO  SPA-GMN-PRTPTLSTKBN-G
      *
           . 
       3101-PRTPTLSTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               IF    ( SPA-WIDCD            =   "1002" )
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 403-DEL-SEC
      *    タブ切替
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 480-TAB-SWITCH-SEC
      *    チェックマトリクス
           WHEN    "CLICKED"       ALSO    "CHKMTRX01"
           WHEN    "CLICKED"       ALSO    "CHKMTRX02"
           WHEN    "CLICKED"       ALSO    "CHKMTRX03"
           WHEN    "CLICKED"       ALSO    "CHKMTRX04"
           WHEN    "CLICKED"       ALSO    "CHKMTRX05"
               PERFORM  4100-CHKMTRX-CHK-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    タブ切替
           WHEN    "SWITCH"       ALSO    "nb1"
               PERFORM 440-TAB-CHANGE-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマトリクスチェック処理
      *****************************************************************
       4100-CHKMTRX-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    ZERO            TO  IDX-MTRX
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHKMTRX01"
               MOVE    1           TO  IDX-MTRX
           WHEN    "CHKMTRX02"
               MOVE    2           TO  IDX-MTRX
           WHEN    "CHKMTRX03"
               MOVE    3           TO  IDX-MTRX
           WHEN    "CHKMTRX04"
               MOVE    4           TO  IDX-MTRX
           WHEN    "CHKMTRX05"
               MOVE    5           TO  IDX-MTRX
           END-EVALUATE
      *
      *
           IF    ( IDX-MTRX        >   ZERO )
            AND  ( SPA-GMN-CHKMTRX (IDX-MTRX)  =   " " )
               MOVE    "1"         TO  SPA-GMN-CHKMTRX (IDX-MTRX)
           ELSE
               MOVE    " "         TO  SPA-GMN-CHKMTRX (IDX-MTRX)
           END-IF
      *
           .
       4100-CHKMTRX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4101-INPUT-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4101-INPUT-CHK-SEC          SECTION.
      *
           EVALUATE    SPA-GMN-PAGENO
      *    設定１
           WHEN    ZERO
               PERFORM 4102-PAGE00-KIHON-CHK-SEC
      *    設定２
           WHEN    1
               PERFORM 4102-PAGE01-KIHON-CHK-SEC
           END-EVALUATE
      *
           .
      *
       4101-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W60-SEIHOJNUMCHKKBN TO  SPA-GMN-SEIHOJNUMCHKKBN-G
           MOVE    W60-BYOSRYKAKBN     TO  SPA-GMN-BYOSRYKAKBN-G
           MOVE    W60-SYUBYOCHK       TO  SPA-GMN-SYUBYOCHK-G
           MOVE    W60-UTAGAIKKN       TO  SPA-GMN-UTAGAIKKN
           MOVE    W60-KYUSEIKKNKBN    TO  SPA-GMN-KYUSEIKKNKBN-G
           MOVE    W60-COMPBYONISSUKBN TO  SPA-GMN-COMPBYONISSUKBN-G
           MOVE    W60-SRYSTTENKIKBN   TO  SPA-GMN-SRYSTTENKIKBN-G
           MOVE    W60-TENKITEKBYOKBN  TO  SPA-GMN-TENKITEKBYOKBN-G
           MOVE    W60-TEKBYOYKZKBN    TO  SPA-GMN-TEKBYOYKZKBN-G
           MOVE    W60-TEKBYOSRYKBN    TO  SPA-GMN-TEKBYOSRYKBN-G
           MOVE    W60-YBKENSAYAKUKBN  TO  SPA-GMN-YBKENSAYAKUKBN-G
           MOVE    W60-YBSJTYKZKBN     TO  SPA-GMN-YBSJTYKZKBN-G
           MOVE    W60-YBMSIYKZKBN     TO  SPA-GMN-YBMSIYKZKBN-G
           MOVE    W60-YBSHOHOYKZKBN   TO  SPA-GMN-YBSHOHOYKZKBN-G
           MOVE    W60-YBUTAGAIKBN     TO  SPA-GMN-YBUTAGAIKBN-G
           MOVE    W60-YKZHEIMSTKBN    TO  SPA-GMN-YKZHEIMSTKBN-G
           MOVE    W60-SRYHEIWRNKBN    TO  SPA-GMN-SRYHEIWRNKBN-G
           MOVE    W60-SRYHEIMSTKBN    TO  SPA-GMN-SRYHEIMSTKBN-G
           MOVE    W60-SRYHEIEXPKBN    TO  SPA-GMN-SRYHEIEXPKBN-G
           MOVE    W60-SRYMATERKKBN    TO  SPA-GMN-SRYMATERKKBN-G
           MOVE    W60-SRYKAISUMSTKBN  TO  SPA-GMN-SRYKAISUMSTKBN-G
           MOVE    W60-SRYKAISUWRNKBN  TO  SPA-GMN-SRYKAISUWRNKBN-G
           MOVE    W60-INGAIPROCKBN    TO  SPA-GMN-INGAIPROCKBN-G
           MOVE    W60-ERR990009KBN    TO  SPA-GMN-ERR990009KBN-G
           MOVE    W60-ERRA10001KBN    TO  SPA-GMN-ERRA10001KBN-G
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   7 )
               MOVE    W60-INGAITEN (IDX0)
                                       TO  SPA-GMN-INGAITEN (IDX0)
           END-PERFORM
           MOVE    W60-REIGAICHKKBN    TO  SPA-GMN-REIGAICHKKBN-G
           MOVE    W60-PRTSRTKBN       TO  SPA-GMN-PRTSRTKBN-G
           MOVE    W60-PRTPTLSTKBN     TO  SPA-GMN-PRTPTLSTKBN-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    設定１基本チェック処理
      *****************************************************************
       4102-PAGE00-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SEIHOJNUMCHKKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-BYOSRYKAKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYUBYOCHK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-UTAGAIKKN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KYUSEIKKNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-COMPBYONISSUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYSTTENKIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-TENKITEKBYOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-TEKBYOYKZKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-TEKBYOSRYKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YBKENSAYAKUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YBSJTYKZKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YBMSIYKZKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YBSHOHOYKZKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YBUTAGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YKZHEIMSTKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-PAGE00-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定２基本チェック処理
      *****************************************************************
       4102-PAGE01-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYHEIWRNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYHEIMSTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYHEIEXPKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYMATERKKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYKAISUMSTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRYKAISUWRNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-INGAIPROCKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ERR990009KBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ERRA10001KBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-REIGAICHKKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-PRTSRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-PRTPTLSTKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-PAGE01-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生保受給者番号抜けチェック区分チェック処理
      *****************************************************************
       4101-SEIHOJNUMCHKKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SEIHOJNUMCHKKBN TO  WRK-CMB-CD
           PERFORM 4200-SEIHOJNUMCHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SEIHOJNUMCHKKBNLST (2)
                                   TO  SPA-GMN-SEIHOJNUMCHKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SEIHOJNUMCHKKBN-G
           END-IF
      *
           .
      *
       4101-SEIHOJNUMCHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科別チェック区分チェック処理
      *****************************************************************
       4101-BYOSRYKAKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-BYOSRYKAKBN TO  WRK-CMB-CD
           PERFORM 4200-BYOSRYKAKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-BYOSRYKAKBNLST (2)
                                   TO  SPA-GMN-BYOSRYKAKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-BYOSRYKAKBN-G
           END-IF
      *
           .
      *
       4101-BYOSRYKAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名チェック区分チェック処理
      *****************************************************************
       4101-SYUBYOCHK-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SYUBYOCHK   TO  WRK-CMB-CD
           PERFORM 4200-SYUBYOCHKLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYUBYOCHKLST (2)
                                   TO  SPA-GMN-SYUBYOCHK-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SYUBYOCHK-G
           END-IF
      *
           .
      *
       4101-SYUBYOCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い期間チェック処理
      ***************************************************************
       4101-UTAGAIKKN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-UTAGAIKKN   >= 1 )
            AND  ( SPA-GMN-UTAGAIKKN   <= 9 )
               CONTINUE
           ELSE
               MOVE    "0001"      TO  SPA-ERRCD
               MOVE    104         TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4101-UTAGAIKKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性期間区分チェック処理
      *****************************************************************
       4101-KYUSEIKKNKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN TO  WRK-CMB-CD
           PERFORM 4200-KYUSEIKKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KYUSEIKKNKBNLST (1)
                                   TO  SPA-GMN-KYUSEIKKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KYUSEIKKNKBN-G
           END-IF
      *
           .
      *
       4101-KYUSEIKKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日と実日数の比較区分チェック処理
      *****************************************************************
       4101-COMPBYONISSUKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-COMPBYONISSUKBN TO  WRK-CMB-CD
           PERFORM 4200-COMPBYONISSUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-COMPBYONISSUKBNLST (1)
                                   TO  SPA-GMN-COMPBYONISSUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-COMPBYONISSUKBN-G
           END-IF
      *
           .
      *
       4101-COMPBYONISSUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「中止」の取り扱いチェック処理
      *****************************************************************
       4101-SRYSTTENKIKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-SRYSTTENKIKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYSTTENKIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYSTTENKIKBNLST (1)
                                   TO  SPA-GMN-SRYSTTENKIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYSTTENKIKBN-G
           END-IF
      *
           .
      *
       4101-SRYSTTENKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    過月転帰済適応病名区分チェック処理
      *****************************************************************
       4101-TENKITEKBYOKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN TO  WRK-CMB-CD
           PERFORM 4200-TENKITEKBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TENKITEKBYOKBNLST (1)
                                   TO  SPA-GMN-TENKITEKBYOKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TENKITEKBYOKBN-G
           END-IF
      *
           .
      *
       4101-TENKITEKBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ使用（薬剤と病名）区分チェック処理
      *****************************************************************
       4101-TEKBYOYKZKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-TEKBYOYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-TEKBYOYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TEKBYOYKZKBNLST (1)
                                   TO  SPA-GMN-TEKBYOYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TEKBYOYKZKBN-G
           END-IF
      *
           .
      *
       4101-TEKBYOYKZKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ使用（診療行為と病名）区分チェック処理
      *****************************************************************
       4101-TEKBYOSRYKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-TEKBYOSRYKBN TO  WRK-CMB-CD
           PERFORM 4200-TEKBYOSRYKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TEKBYOSRYKBNLST (1)
                                   TO  SPA-GMN-TEKBYOSRYKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TEKBYOSRYKBN-G
           END-IF
      *
           .
      *
       4101-TEKBYOSRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象区分チェック処理
      *****************************************************************
       4101-YBKENSAYAKUKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN TO  WRK-CMB-CD
           PERFORM 4200-YBKENSAYAKUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBKENSAYAKUKBNLST (1)
                                   TO  SPA-GMN-YBKENSAYAKUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBKENSAYAKUKBN-G
           END-IF
      *
           .
      *
       4101-YBKENSAYAKUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    手術薬剤対象区分チェック処理
      *****************************************************************
       4101-YBSJTYKZKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-YBSJTYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-YBSJTYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBSJTYKZKBNLST (1)
                                   TO  SPA-GMN-YBSJTYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBSJTYKZKBN-G
           END-IF
      *
           .
      *
       4101-YBSJTYKZKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    麻酔薬剤対象区分チェック処理
      *****************************************************************
       4101-YBMSIYKZKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-YBMSIYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-YBMSIYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBMSIYKZKBNLST (1)
                                   TO  SPA-GMN-YBMSIYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBMSIYKZKBN-G
           END-IF
      *
           .
      *
       4101-YBMSIYKZKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処方のみの薬剤対象区分チェック処理
      *****************************************************************
       4101-YBSHOHOYKZKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-YBSHOHOYKZKBN TO  WRK-CMB-CD
           PERFORM 4200-YBSHOHOYKZKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBSHOHOYKZKBNLST (1)
                                   TO  SPA-GMN-YBSHOHOYKZKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBSHOHOYKZKBN-G
           END-IF
      *
           .
      *
       4101-YBSHOHOYKZKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名疑い区分チェック処理
      *****************************************************************
       4101-YBUTAGAIKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-YBUTAGAIKBN TO  WRK-CMB-CD
           PERFORM 4200-YBUTAGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBUTAGAIKBNLST (1)
                                   TO  SPA-GMN-YBUTAGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBUTAGAIKBN-G
           END-IF
      *
           .
      *
       4101-YBUTAGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    相互作用マスタ参照区分チェック処理
      *****************************************************************
       4101-YKZHEIMSTKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-YKZHEIMSTKBN TO  WRK-CMB-CD
           PERFORM 4200-YKZHEIMSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YKZHEIMSTKBNLST (2)
                                   TO  SPA-GMN-YKZHEIMSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YKZHEIMSTKBN-G
           END-IF
      *
           .
      *
       4101-YKZHEIMSTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定警告エラー区分チェック処理
      *****************************************************************
       4101-SRYHEIWRNKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIWRNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIWRNKBNLST (1)
                                   TO  SPA-GMN-SRYHEIWRNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYHEIWRNKBN-G
           END-IF
      *
           .
      *
       4101-SRYHEIWRNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定電子点数表参照区分チェック処理
      *****************************************************************
       4101-SRYHEIMSTKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYHEIMSTKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIMSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIMSTKBNLST (2)
                                   TO  SPA-GMN-SRYHEIMSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYHEIMSTKBN-G
           END-IF
      *
           .
      *
       4101-SRYHEIMSTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定電子点数表特例区分チェック処理
      *****************************************************************
       4101-SRYHEIEXPKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYHEIEXPKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIEXPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIEXPKBNLST (2)
                                   TO  SPA-GMN-SRYHEIEXPKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYHEIEXPKBN-G
           END-IF
      *
           .
      *
       4101-SRYHEIEXPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定器材関連手技参照チェック処理
      *****************************************************************
       4101-SRYMATERKKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYMATERKKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYMATERKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYMATERKKBNLST (2)
                                   TO  SPA-GMN-SRYMATERKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYMATERKKBN-G
           END-IF
      *
           .
      *
       4101-SRYMATERKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表回数参照チェック処理
      *****************************************************************
       4101-SRYKAISUMSTKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYKAISUMSTKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYKAISUMSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYKAISUMSTKBNLST (2)
                                   TO  SPA-GMN-SRYKAISUMSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYKAISUMSTKBN-G
           END-IF
      *
           .
      *
       4101-SRYKAISUMSTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ警告エラーチェック処理
      *****************************************************************
       4101-SRYKAISUWRNKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYKAISUWRNKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYKAISUWRNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYKAISUWRNKBNLST (2)
                                   TO  SPA-GMN-SRYKAISUWRNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYKAISUWRNKBN-G
           END-IF
      *
           .
      *
       4101-SRYKAISUWRNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理チェック処理
      *****************************************************************
       4101-INGAIPROCKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-INGAIPROCKBN TO  WRK-CMB-CD
           PERFORM 4200-INGAIPROCKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-INGAIPROCKBNLST (2)
                                   TO  SPA-GMN-INGAIPROCKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-INGAIPROCKBN-G
           END-IF
      *
           .
      *
       4101-INGAIPROCKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他エラー区分チェック処理
      *****************************************************************
       4101-ERR990009KBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-ERR990009KBN TO  WRK-CMB-CD
           PERFORM 4200-ERR990009KBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ERR990009KBNLST (1)
                                   TO  SPA-GMN-ERR990009KBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-ERR990009KBN-G
           END-IF
      *
           .
      *
       4101-ERR990009KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「薬剤と病名」に未登録の薬剤チェック区分チェック処理
      *****************************************************************
       4101-ERRA10001KBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-ERRA10001KBN TO  WRK-CMB-CD
           PERFORM 4200-ERRA10001KBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ERRA10001KBNLST (2)
                                   TO  SPA-GMN-ERRA10001KBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-ERRA10001KBN-G
           END-IF
      *
           .
      *
       4101-ERRA10001KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者毎の例外設定チェック処理
      *****************************************************************
       4101-REIGAICHKKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-REIGAICHKKBN TO  WRK-CMB-CD
           PERFORM 4200-REIGAICHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-REIGAICHKKBNLST (1)
                                   TO  SPA-GMN-REIGAICHKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-REIGAICHKKBN-G
           END-IF
      *
           .
      *
       4101-REIGAICHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順区分チェック処理
      *****************************************************************
       4101-PRTSRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-PRTSRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-PRTSRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTSRTKBNLST (1)
                                   TO  SPA-GMN-PRTSRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTSRTKBN-G
           END-IF
      *
           .
      *
       4101-PRTSRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者別チェックリスト区分チェック処理
      *****************************************************************
       4101-PRTPTLSTKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-PRTPTLSTKBN TO  WRK-CMB-CD
           PERFORM 4200-PRTPTLSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTPTLSTKBNLST (1)
                                   TO  SPA-GMN-PRTPTLSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTPTLSTKBN-G
           END-IF
      *
           .
      *
       4101-PRTPTLSTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生保受給者番号抜けチェック区分コンボチェック処理
      *****************************************************************
       4200-SEIHOJNUMCHKKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SEIHOJNUMCHKKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SEIHOJNUMCHKKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SEIHOJNUMCHKKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SEIHOJNUMCHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科別チェック区分コンボチェック処理
      *****************************************************************
       4200-BYOSRYKAKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BYOSRYKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BYOSRYKAKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-BYOSRYKAKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BYOSRYKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名チェック区分コンボチェック処理
      *****************************************************************
       4200-SYUBYOCHKLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SYUBYOCHK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYUBYOCHKL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYUBYOCHKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SYUBYOCHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性期間区分コンボチェック処理
      *****************************************************************
       4200-KYUSEIKKNKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-KYUSEIKKNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KYUSEIKKNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KYUSEIKKNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KYUSEIKKNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日と実日数の比較区分コンボチェック処理
      *****************************************************************
       4200-COMPBYONISSUKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-COMPBYONISSUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-COMPBYONISSUKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-COMPBYONISSUKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-COMPBYONISSUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「中止」の取り扱い設定処理
      *****************************************************************
       4200-SRYSTTENKIKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SRYSTTENKIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYSTTENKIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYSTTENKIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYSTTENKIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    過月転帰済適応病名区分コンボチェック処理
      *****************************************************************
       4200-TENKITEKBYOKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TENKITEKBYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TENKITEKBYOKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TENKITEKBYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TENKITEKBYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ使用（薬剤と病名）区分コンボチェック処理
      *****************************************************************
       4200-TEKBYOYKZKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEKBYOYKZKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEKBYOYKZKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TEKBYOYKZKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TEKBYOYKZKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ使用（診療行為と病名）区分コンボチェック処理
      *****************************************************************
       4200-TEKBYOSRYKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEKBYOSRYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEKBYOSRYKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TEKBYOSRYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TEKBYOSRYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象区分コンボチェック処理
      *****************************************************************
       4200-YBKENSAYAKUKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-YBKENSAYAKUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBKENSAYAKUKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBKENSAYAKUKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBKENSAYAKUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    手術薬剤対象区分コンボチェック処理
      *****************************************************************
       4200-YBSJTYKZKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-YBSJTYKZKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBSJTYKZKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBSJTYKZKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBSJTYKZKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    麻酔薬剤対象区分コンボチェック処理
      *****************************************************************
       4200-YBMSIYKZKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-YBMSIYKZKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBMSIYKZKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBMSIYKZKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBMSIYKZKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処方のみの薬剤対象区分コンボチェック処理
      *****************************************************************
       4200-YBSHOHOYKZKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-YBSHOHOYKZKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBSHOHOYKZKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBSHOHOYKZKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBSHOHOYKZKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名疑い区分コンボチェック処理
      *****************************************************************
       4200-YBUTAGAIKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-YBUTAGAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBUTAGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBUTAGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBUTAGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    相互作用マスタ参照コンボチェック処理
      *****************************************************************
       4200-YKZHEIMSTKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-YKZHEIMSTKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YKZHEIMSTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YKZHEIMSTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YKZHEIMSTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定警告エラー区分コンボチェック処理
      *****************************************************************
       4200-SRYHEIWRNKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRYHEIWRNKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYHEIWRNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYHEIWRNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYHEIWRNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定電子点数表参照コンボチェック処理
      *****************************************************************
       4200-SRYHEIMSTKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRYHEIMSTKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYHEIMSTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYHEIMSTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYHEIMSTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定電子点数表特例コンボチェック処理
      *****************************************************************
       4200-SRYHEIEXPKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRYHEIEXPKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYHEIEXPKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYHEIEXPKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYHEIEXPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定器材関連手技参照コンボチェック処理
      *****************************************************************
       4200-SRYMATERKKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRYMATERKKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYMATERKKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYMATERKKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYMATERKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表回数参照コンボチェック処理
      *****************************************************************
       4200-SRYKAISUMSTKBNLST-CHK-SEC  SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SRYKAISUMSTKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYKAISUMSTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYKAISUMSTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYKAISUMSTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ警告エラーコンボチェック処理
      *****************************************************************
       4200-SRYKAISUWRNKBNLST-CHK-SEC  SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SRYKAISUWRNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYKAISUWRNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYKAISUWRNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYKAISUWRNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理コンボチェック処理
      *****************************************************************
       4200-INGAIPROCKBNLST-CHK-SEC  SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-INGAIPROCKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-INGAIPROCKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-INGAIPROCKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-INGAIPROCKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他エラー区分コンボチェック処理
      *****************************************************************
       4200-ERR990009KBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-ERR990009KBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ERR990009KBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ERR990009KBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ERR990009KBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「薬剤と病名」に未登録の薬剤チェック区分コンボチェック処理
      *****************************************************************
       4200-ERRA10001KBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-ERRA10001KBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ERRA10001KBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ERRA10001KBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ERRA10001KBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者毎の例外設定コンボチェック処理
      *****************************************************************
       4200-REIGAICHKKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-REIGAICHKKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-REIGAICHKKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-REIGAICHKKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-REIGAICHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順区分コンボチェック処理
      *****************************************************************
       4200-PRTSRTKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-PRTSRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PRTSRTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PRTSRTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-PRTSRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者別チェックリスト区分コンボチェック処理
      *****************************************************************
       4200-PRTPTLSTKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-PRTPTLSTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PRTPTLSTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PRTPTLSTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-PRTPTLSTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W60"               TO  SPA-MOTOPG
      *
           MOVE    "W01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-W60
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    CONST-CHKMTRX (IDX-MTRX)
                                   TO  SPA-GMN-CHKMTRX (IDX-MTRX)
      *
           END-PERFORM
      *
           MOVE    2               TO  SPA-GMN-UTAGAIKKN
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    タブ切替処理
      *****************************************************************
       480-TAB-SWITCH-SEC          SECTION.
      *
           EVALUATE    SPA-GMN-PAGENO
           WHEN    0
               MOVE    1           TO  W60-PAGENO
           WHEN    1
               MOVE    0           TO  W60-PAGENO
           END-EVALUATE
      *
           PERFORM 440-TAB-CHANGE-SEC
      *
           .
       480-TAB-SWITCH-EXT.
           EXIT.
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           PERFORM 40311-DEL-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 40312-DEL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4031-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理 (SYS 1101)
      *****************************************************************
       40311-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1101-REC
           INITIALIZE                      SYS-1101-REC
           MOVE    "1101"                  TO  SYS-1101-KANRICD
           MOVE    "*"                     TO  SYS-1101-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD    TO  SYS-1101-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYS-1101-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1101-HOSPNUM
           MOVE    SYS-1101-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "4003"              TO  SPA-ERRCD
           END-IF
      *
           .
       40311-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理 (CHK004 1103)
      *****************************************************************
       40312-DEL-SEC                              SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM    TO   CHK004-HOSPNUM
           MOVE    "1103"         TO   CHK004-KANRICD
           MOVE    "%"            TO   CHK004-KBNCD
           IF    ( SPA-W01-SELNUM          >   ZERO )
               MOVE    SPA-W01-SEL-STYUKYMD
                                  TO   CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           ELSE
               MOVE    SPA-W01-STYUKYMD
                                  TO   CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           END-IF
      *
           MOVE    CHK004-REC     TO   MCPDATA-REC
           MOVE    "tbl_chk004"   TO   MCP-TABLE
           MOVE    "del1"         TO   MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "4004"     TO  SPA-ERRCD
           END-IF
      *
           .
       40312-DEL-EXT.
           EXIT.
      *****************************************************************
      *    タブ編集処理
      *****************************************************************
       440-TAB-CHANGE-SEC              SECTION.
      *
      *    画面ＳＰＡ編集処理
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           EVALUATE    SPA-GMN-PAGENO
           WHEN    0
               PERFORM 4102-PAGE00-KIHON-CHK-SEC
           WHEN    1
               PERFORM 4102-PAGE01-KIHON-CHK-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4801-KANREN-CHK
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               MOVE    W60-PAGENO      TO  SPA-GMN-PAGENO
      *
               EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   MOVE    101         TO  SPA-GMN-CUR
               WHEN    1
                   MOVE    215         TO  SPA-GMN-CUR
               END-EVALUATE
      *
           END-IF
      *
           .
       440-TAB-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
           MOVE    SPA-GMN-PAGENO  TO  WRK-PAGENO
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-PAGE-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    IDX2        TO  SPA-GMN-PAGENO
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-INPUT-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4801-KANREN-CHK
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    WRK-PAGENO  TO  SPA-GMN-PAGENO
               MOVE    "1001"      TO  SPA-WIDCD
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    システム管理1101更新処理
           PERFORM 4901-KOUSHIN-SEC
      *
      *    チェックマスタ４(1103)更新処理
           PERFORM 4902-KOUSHIN-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理（SYSKANRI 1101)
      *****************************************************************
       4901-KOUSHIN-SEC           SECTION.
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               MOVE    SPACE           TO  SYS-1101-REC
               INITIALIZE                  SYS-1101-REC
               MOVE    "1101"          TO  SYS-1101-KANRICD
               MOVE    "*"             TO  SYS-1101-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-1101-STYUKYMD
                                           SYS-1101-EDYUKYMD
               MOVE    SYS-1101-REC    TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           ELSE
               MOVE    SPACE           TO  SYS-1101-REC
               INITIALIZE                  SYS-1101-REC
               MOVE    "1101"          TO  SYS-1101-KANRICD
               MOVE    "*"             TO  SYS-1101-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1101-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1101-EDYUKYMD
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    SPA-GMN-CHKMTRX (IDX-MTRX)
                                       TO  SYS-1101-BYOCHKPTN(IDX-MTRX)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SEIHOJNUMCHKKBN
                                       TO  SYS-1101-SEIHOJNUMCHKKBN
      *
           MOVE    SPA-GMN-BYOSRYKAKBN TO  SYS-1101-BYOSRYKAKBN
      *
           MOVE    SPA-GMN-SYUBYOCHK   TO  SYS-1101-SYUBYOCHKKBN
      *
           MOVE    SPA-GMN-UTAGAIKKN   TO  SYS-1101-UTAGAIKKN
      *
           EVALUATE    SPA-GMN-KYUSEIKKNKBN
           WHEN    "0"
               MOVE    0               TO  SYS-1101-KYUSEIKKN
           WHEN    "1"
               MOVE    1               TO  SYS-1101-KYUSEIKKN
           WHEN    "2"
               MOVE    2               TO  SYS-1101-KYUSEIKKN
           WHEN    "3"
               MOVE    3               TO  SYS-1101-KYUSEIKKN
           WHEN    "4"
               MOVE    4               TO  SYS-1101-KYUSEIKKN
           END-EVALUATE
      *
           MOVE    SPA-GMN-COMPBYONISSUKBN
                                       TO   SYS-1101-COMPBYONISSUKBN
           MOVE    SPA-GMN-SRYSTTENKIKBN
                                       TO   SYS-1101-SRYSTTENKIKBN
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN
                                       TO   SYS-1101-TENKITEKBYOKBN
      *
           MOVE    SPA-GMN-TEKBYOYKZKBN
                                       TO   SYS-1101-TEKBYOYKZKBN
      *
           MOVE    SPA-GMN-TEKBYOSRYKBN
                                       TO   SYS-1101-TEKBYOSRYKBN
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN  TO   SYS-1101-YBKENSAYAKUKBN
      *
           MOVE    SPA-GMN-YBSJTYKZKBN  TO   SYS-1101-YBSJTYKZKBN
      *
           MOVE    SPA-GMN-YBMSIYKZKBN  TO   SYS-1101-YBMSIYKZKBN
      *
           MOVE    SPA-GMN-YBSHOHOYKZKBN  TO   SYS-1101-YBSHOHOYKZKBN
      *
           MOVE    SPA-GMN-YBUTAGAIKBN TO   SYS-1101-YBUTAGAIKBN
      *
           MOVE    SPA-GMN-YKZHEIMSTKBN TO   SYS-1101-YKZHEIMSTKBN
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN TO   SYS-1101-SRYHEIWRNKBN
      *
           MOVE    SPA-GMN-SRYHEIMSTKBN TO   SYS-1101-SRYHEIMSTKBN
      *
           IF    ( SPA-GMN-SRYHEIMSTKBN     =   "1" )
               MOVE    SPA-GMN-SRYHEIEXPKBN
                                       TO   SYS-1101-SRYHEIEXPKBN
           ELSE
               MOVE    "2"             TO   SYS-1101-SRYHEIEXPKBN
           END-IF
      *
           MOVE    SPA-GMN-SRYMATERKKBN
                                          TO SYS-1101-SRYMATERKKBN
      *
           MOVE    SPA-GMN-SRYKAISUMSTKBN TO SYS-1101-SRYKAISUMSTKBN
      *
           MOVE    SPA-GMN-SRYKAISUWRNKBN TO SYS-1101-SRYKAISUWRNKBN
      *
           MOVE    SPA-GMN-INGAIPROCKBN TO SYS-1101-INGAIPROCKBN
      *
           MOVE    SPA-GMN-SYA-INGAITEN
                                       TO  SYS-1101-SYA-INGAITEN
           MOVE    SPA-GMN-KOK-INGAITEN
                                       TO  SYS-1101-KOK-INGAITEN
           MOVE    SPA-GMN-KOU-INGAITEN
                                       TO  SYS-1101-KOU-INGAITEN
           MOVE    SPA-GMN-RSI-INGAITEN
                                       TO  SYS-1101-RSI-INGAITEN
           MOVE    SPA-GMN-JBI-INGAITEN
                                       TO  SYS-1101-JBI-INGAITEN
           MOVE    SPA-GMN-JIH-INGAITEN
                                       TO  SYS-1101-JIH-INGAITEN
                                           SYS-1101-CKN-INGAITEN
           MOVE    SPA-GMN-KGI-INGAITEN
                                       TO  SYS-1101-KGI-INGAITEN
      *
           MOVE    SPA-GMN-REIGAICHKKBN TO   SYS-1101-REIGAICHKKBN
      *
           MOVE    SPA-GMN-PRTSRTKBN   TO   SYS-1101-PRTSRTKBN
      *
           MOVE    SPA-GMN-PRTPTLSTKBN TO   SYS-1101-PRTPTLSTKBN
      *
           MOVE    SYS-1101-REC            TO  SYSKANRI-REC
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4001"              TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4002"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理（CHK004 1103)
      *****************************************************************
       4902-KOUSHIN-SEC            SECTION.
      *
           PERFORM 40312-DEL-SEC
      *
           IF    ( SPA-ERRCD      =    SPACE )
               INITIALIZE                  CK4-1103-REC
               MOVE    "99"            TO  CK4-1103-KBNCD
               MOVE    SPA-GMN-ERR990009KBN
                                       TO  CK4-1103-ERRKBN (9)
               PERFORM 49021-KOUSHIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD      =    SPACE )
               INITIALIZE                  CK4-1103-REC
               MOVE    "A1"            TO  CK4-1103-KBNCD
               MOVE    SPA-GMN-ERRA10001KBN
                                       TO  CK4-1103-ERRKBN (1)
               PERFORM 49021-KOUSHIN-SEC
           END-IF
      *
           .
       4902-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理（CHK004 1103)
      *****************************************************************
       49021-KOUSHIN-SEC               SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  CK4-1103-HOSPNUM
           MOVE    "1103"          TO  CK4-1103-KANRICD
           MOVE    SPA-W01-STYUKYMD
                                   TO  CK4-1103-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD
                                   TO  CK4-1103-EDYUKYMD
           MOVE    CK4-1103-REC    TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4005"      TO  SPA-ERRCD
           END-IF
      *
           .
       49021-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック
      *****************************************************************
       4801-KANREN-CHK                 SECTION.
      *
           EVALUATE    SPA-GMN-PAGENO
           WHEN    ZERO
               IF    ( SPA-GMN-CHKMTRX-G   =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
                   MOVE    107         TO  SPA-GMN-CUR
               END-IF
           END-EVALUATE
      *
           .
       4801-KANREN-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W60    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE           TO  W60
           INITIALIZE                  W60
      *
      *    タブページ番号
           MOVE    SPA-GMN-PAGENO  TO  W60-PAGENO
      *
           MOVE    SPA-W01-STYUKYMDH   TO  W60-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  W60-EDYUKYMD
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   4 )
      *
               MOVE    WIDGET-NORMAL
                                       TO  W60-CHKMTRXNIL-STATE
                                                         (IDX-MTRX)
               MOVE    "whitebutton"   TO  W60-CHKMTRXNIL-STYLE
                                                         (IDX-MTRX)
           END-PERFORM
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    "whitebutton"
                                   TO  W60-CHKMTRX-STYLE (IDX-MTRX)
      *
               IF    ( SPA-GMN-CHKMTRX (IDX-MTRX)  =   "1" )
                   MOVE    "○"    TO  W60-MTRX    (IDX-MTRX)
               ELSE
                   MOVE    "×"    TO  W60-MTRX    (IDX-MTRX)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SEIHOJNUMCHKKBN-G
                                       TO  W60-SEIHOJNUMCHKKBN
           MOVE    SPA-GMN-SEIHOJNUMCHKKBN-MAX
                                       TO  W60-SEIHOJNUMCHKKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SEIHOJNUMCHKKBN-MAX )
      *
               MOVE    SPA-GMN-SEIHOJNUMCHKKBNLST (IDX0)
                                       TO  W60-SEIHOJNUMCHKKBN-ITEM
                                                                  (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-BYOSRYKAKBN-G   TO  W60-BYOSRYKAKBN
      *
           MOVE    SPA-GMN-BYOSRYKAKBN-MAX
                                           TO  W60-BYOSRYKAKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-BYOSRYKAKBN-MAX )
      *
               MOVE    SPA-GMN-BYOSRYKAKBNLST (IDX0)
                                       TO  W60-BYOSRYKAKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SYUBYOCHK-G     TO  W60-SYUBYOCHK
      *
           MOVE    SPA-GMN-SYUBYOCHK-MAX
                                           TO  W60-SYUBYOCHK-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SYUBYOCHK-MAX )
      *
               MOVE    SPA-GMN-SYUBYOCHKLST (IDX0)
                                           TO  W60-SYUBYOCHK-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-UTAGAIKKN       TO  W60-UTAGAIKKN
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN-G   TO  W60-KYUSEIKKNKBN
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN-MAX
                                           TO  W60-KYUSEIKKNKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-KYUSEIKKNKBN-MAX )
      *
               MOVE    SPA-GMN-KYUSEIKKNKBNLST (IDX0)
                                       TO  W60-KYUSEIKKNKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-COMPBYONISSUKBN-G   TO  W60-COMPBYONISSUKBN
      *
           MOVE    SPA-GMN-COMPBYONISSUKBN-MAX
                                           TO  W60-COMPBYONISSUKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-COMPBYONISSUKBN-MAX )
      *
               MOVE    SPA-GMN-COMPBYONISSUKBNLST (IDX0)
                                   TO  W60-COMPBYONISSUKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYSTTENKIKBN-G   TO  W60-SRYSTTENKIKBN
      *
           MOVE    SPA-GMN-SRYSTTENKIKBN-MAX
                                           TO  W60-SRYSTTENKIKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SRYSTTENKIKBN-MAX )
      *
               MOVE    SPA-GMN-SRYSTTENKIKBNLST (IDX0)
                                   TO  W60-SRYSTTENKIKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN-G   TO  W60-TENKITEKBYOKBN
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN-MAX
                                       TO  W60-TENKITEKBYOKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-TENKITEKBYOKBN-MAX )
      *
               MOVE    SPA-GMN-TENKITEKBYOKBNLST (IDX0)
                                       TO  W60-TENKITEKBYOKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-TEKBYOYKZKBN-G   TO  W60-TEKBYOYKZKBN
      *
           MOVE    SPA-GMN-TEKBYOYKZKBN-MAX
                                       TO  W60-TEKBYOYKZKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-TEKBYOYKZKBN-MAX )
      *
               MOVE    SPA-GMN-TEKBYOYKZKBNLST (IDX0)
                                       TO  W60-TEKBYOYKZKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-TEKBYOSRYKBN-G   TO  W60-TEKBYOSRYKBN
      *
           MOVE    SPA-GMN-TEKBYOSRYKBN-MAX
                                       TO  W60-TEKBYOSRYKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-TEKBYOSRYKBN-MAX )
      *
               MOVE    SPA-GMN-TEKBYOSRYKBNLST (IDX0)
                                       TO  W60-TEKBYOSRYKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN-G   TO  W60-YBKENSAYAKUKBN
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN-MAX
                                       TO  W60-YBKENSAYAKUKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBKENSAYAKUKBN-MAX )
      *
               MOVE    SPA-GMN-YBKENSAYAKUKBNLST (IDX0)
                                       TO  W60-YBKENSAYAKUKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBSJTYKZKBN-G   TO  W60-YBSJTYKZKBN
      *
           MOVE    SPA-GMN-YBSJTYKZKBN-MAX
                                       TO  W60-YBSJTYKZKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBSJTYKZKBN-MAX )
      *
               MOVE    SPA-GMN-YBSJTYKZKBNLST (IDX0)
                                       TO  W60-YBSJTYKZKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBMSIYKZKBN-G   TO  W60-YBMSIYKZKBN
      *
           MOVE    SPA-GMN-YBMSIYKZKBN-MAX
                                       TO  W60-YBMSIYKZKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBMSIYKZKBN-MAX )
      *
               MOVE    SPA-GMN-YBMSIYKZKBNLST (IDX0)
                                       TO  W60-YBMSIYKZKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBSHOHOYKZKBN-G   TO  W60-YBSHOHOYKZKBN
      *
           MOVE    SPA-GMN-YBSHOHOYKZKBN-MAX
                                       TO  W60-YBSHOHOYKZKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBSHOHOYKZKBN-MAX )
      *
               MOVE    SPA-GMN-YBSHOHOYKZKBNLST (IDX0)
                                       TO  W60-YBSHOHOYKZKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBUTAGAIKBN-G   TO  W60-YBUTAGAIKBN
      *
           MOVE    SPA-GMN-YBUTAGAIKBN-MAX
                                       TO  W60-YBUTAGAIKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBUTAGAIKBN-MAX )
      *
               MOVE    SPA-GMN-YBUTAGAIKBNLST (IDX0)
                                       TO  W60-YBUTAGAIKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YKZHEIMSTKBN-G   TO  W60-YKZHEIMSTKBN
      *
           MOVE    SPA-GMN-YKZHEIMSTKBN-MAX
                                       TO  W60-YKZHEIMSTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YKZHEIMSTKBN-MAX )
      *
               MOVE    SPA-GMN-YKZHEIMSTKBNLST (IDX0)
                                       TO  W60-YKZHEIMSTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN-G   TO  W60-SRYHEIWRNKBN
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN-MAX
                                       TO  W60-SRYHEIWRNKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SRYHEIWRNKBN-MAX )
      *
               MOVE    SPA-GMN-SRYHEIWRNKBNLST (IDX0)
                                       TO  W60-SRYHEIWRNKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYHEIMSTKBN-G   TO  W60-SRYHEIMSTKBN
      *
           MOVE    SPA-GMN-SRYHEIMSTKBN-MAX
                                       TO  W60-SRYHEIMSTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SRYHEIMSTKBN-MAX )
      *
               MOVE    SPA-GMN-SRYHEIMSTKBNLST (IDX0)
                                       TO  W60-SRYHEIMSTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-SRYHEIMSTKBN     =   "1" )
               MOVE    SPA-GMN-SRYHEIEXPKBN-G
                                       TO  W60-SRYHEIEXPKBN
      *
               MOVE    SPA-GMN-SRYHEIEXPKBN-MAX
                                       TO  W60-SRYHEIEXPKBN-COUNT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-SRYHEIEXPKBN-MAX )
      *
                   MOVE    SPA-GMN-SRYHEIEXPKBNLST (IDX0)
                                       TO  W60-SRYHEIEXPKBN-ITEM (IDX0)
      *
               END-PERFORM
           END-IF
      *
           MOVE    SPA-GMN-SRYMATERKKBN-G
                                       TO  W60-SRYMATERKKBN
      *
           MOVE    SPA-GMN-SRYMATERKKBN-MAX
                                       TO  W60-SRYMATERKKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SRYMATERKKBN-MAX )
      *
               MOVE    SPA-GMN-SRYMATERKKBNLST (IDX0)
                                       TO  W60-SRYMATERKKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYKAISUMSTKBN-G   TO  W60-SRYKAISUMSTKBN
      *
           MOVE    SPA-GMN-SRYKAISUMSTKBN-MAX
                                   TO  W60-SRYKAISUMSTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SRYKAISUMSTKBN-MAX )
      *
               MOVE    SPA-GMN-SRYKAISUMSTKBNLST (IDX0)
                                   TO  W60-SRYKAISUMSTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYKAISUWRNKBN-G   TO  W60-SRYKAISUWRNKBN
      *
           MOVE    SPA-GMN-SRYKAISUWRNKBN-MAX
                                   TO  W60-SRYKAISUWRNKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SRYKAISUWRNKBN-MAX )
      *
               MOVE    SPA-GMN-SRYKAISUWRNKBNLST (IDX0)
                                   TO  W60-SRYKAISUWRNKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-INGAIPROCKBN-G   TO  W60-INGAIPROCKBN
      *
           MOVE    SPA-GMN-INGAIPROCKBN-MAX
                                   TO  W60-INGAIPROCKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-INGAIPROCKBN-MAX )
      *
               MOVE    SPA-GMN-INGAIPROCKBNLST (IDX0)
                                   TO  W60-INGAIPROCKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-ERR990009KBN-G   TO  W60-ERR990009KBN
      *
           MOVE    SPA-GMN-ERR990009KBN-MAX
                                       TO  W60-ERR990009KBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-ERR990009KBN-MAX )
      *
               MOVE    SPA-GMN-ERR990009KBNLST (IDX0)
                                       TO  W60-ERR990009KBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-ERRA10001KBN-G   TO  W60-ERRA10001KBN
      *
           MOVE    SPA-GMN-ERRA10001KBN-MAX
                                       TO  W60-ERRA10001KBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-ERRA10001KBN-MAX )
      *
               MOVE    SPA-GMN-ERRA10001KBNLST (IDX0)
                                       TO  W60-ERRA10001KBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   7 )
      *
               MOVE    SPA-GMN-INGAITEN (IDX0)
                                       TO  W60-INGAITEN (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-REIGAICHKKBN-G   TO  W60-REIGAICHKKBN
      *
           MOVE    SPA-GMN-REIGAICHKKBN-MAX
                                       TO  W60-REIGAICHKKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-REIGAICHKKBN-MAX )
      *
               MOVE    SPA-GMN-REIGAICHKKBNLST (IDX0)
                                       TO  W60-REIGAICHKKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTSRTKBN-G     TO  W60-PRTSRTKBN
      *
           MOVE    SPA-GMN-PRTSRTKBN-MAX
                                       TO  W60-PRTSRTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-PRTSRTKBN-MAX )
      *
               MOVE    SPA-GMN-PRTSRTKBNLST (IDX0)
                                       TO  W60-PRTSRTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTPTLSTKBN-G   TO  W60-PRTPTLSTKBN
      *
           MOVE    SPA-GMN-PRTPTLSTKBN-MAX
                                       TO  W60-PRTPTLSTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-PRTPTLSTKBN-MAX )
      *
               MOVE    SPA-GMN-PRTPTLSTKBNLST (IDX0)
                                       TO  W60-PRTPTLSTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  W60-B03-STATE
                                       W60-SRYHEIEXPKBN-STATE
                                       W60-SRYHEIEXPKBN-ITEM-STATE
      *
           IF    ( SPA-W01-SELNUM         <=   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W60-B03-STATE
           END-IF
      *
           IF    ( SPA-W01-SEL-STYUKYMD    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W60-B03-STATE
           END-IF
      *
           IF    ( SPA-GMN-SRYHEIMSTKBN    =   "1" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                   TO  W60-SRYHEIEXPKBN-STATE
                                       W60-SRYHEIEXPKBN-ITEM-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    101
               MOVE    "SEIHOJNUMCHKKBN" TO MCP-WIDGET
           WHEN    102
               MOVE    "BYOSRYKAKBN"   TO  MCP-WIDGET
           WHEN    103
               MOVE    "SYUBYOCHK"     TO  MCP-WIDGET
           WHEN    104
               MOVE    "UTAGAIKKN"     TO  MCP-WIDGET
           WHEN    105
               MOVE    "KYUSEIKKNKBN"  TO  MCP-WIDGET
           WHEN    106
               MOVE    "COMPBYONISSUKBN"
                                       TO  MCP-WIDGET
           WHEN    107
               MOVE    "CHKMTRX01"     TO  MCP-WIDGET
           WHEN    108
               MOVE    "CHKMTRX02"     TO  MCP-WIDGET
           WHEN    109
               MOVE    "CHKMTRX03"     TO  MCP-WIDGET
           WHEN    110
               MOVE    "CHKMTRX04"     TO  MCP-WIDGET
           WHEN    111
               MOVE    "CHKMTRX05"     TO  MCP-WIDGET
           WHEN    112
               MOVE    "TENKITEKBYOKBN" TO MCP-WIDGET
           WHEN    113
               MOVE    "TEKBYOYKZKBN" TO MCP-WIDGET
           WHEN    114
               MOVE    "TEKBYOSRYKBN" TO MCP-WIDGET
           WHEN    115
               MOVE    "YBKENSAYAKUKBN" TO MCP-WIDGET
           WHEN    116
               MOVE    "YBSJTYKZKBN"   TO  MCP-WIDGET
           WHEN    117
               MOVE    "YBMSIYKZKBN"   TO  MCP-WIDGET
           WHEN    118
               MOVE    "YBSHOHOYKZKBN" TO  MCP-WIDGET
           WHEN    119
               MOVE    "YBUTAGAIKBN"   TO  MCP-WIDGET
           WHEN    120
               MOVE    "SRYSTTENKIKBN"  TO  MCP-WIDGET
           WHEN    123
               MOVE    "YKZHEIMSTKBN"   TO  MCP-WIDGET
           WHEN    201
               MOVE    "ERR990009KBN"   TO  MCP-WIDGET
           WHEN    202
               MOVE    "SYA_INGAITEN"  TO MCP-WIDGET
           WHEN    203
               MOVE    "KOK_INGAITEN"  TO MCP-WIDGET
           WHEN    204
               MOVE    "KOU_INGAITEN"  TO MCP-WIDGET
           WHEN    205
               MOVE    "RSI_INGAITEN"  TO MCP-WIDGET
           WHEN    206
               MOVE    "JBI_INGAITEN"  TO MCP-WIDGET
           WHEN    207
               MOVE    "KGI_INGAITEN"  TO MCP-WIDGET
           WHEN    208
               MOVE    "JIH_INGAITEN"  TO MCP-WIDGET
           WHEN    209
               MOVE    "ERRA10001KBN"  TO MCP-WIDGET
           WHEN    210
               MOVE    "REIGAICHKKBN"   TO  MCP-WIDGET
           WHEN    211
               MOVE    "PRTSRTKBN"     TO  MCP-WIDGET
           WHEN    212
               MOVE    "PRTPTLSTKBN"   TO  MCP-WIDGET
           WHEN    213
               MOVE    "SRYKAISUMSTKBN"
                                       TO  MCP-WIDGET
           WHEN    214
               MOVE    "SRYKAISUWRNKBN"
                                       TO  MCP-WIDGET
           WHEN    215
               MOVE    "SRYHEIMSTKBN"   TO  MCP-WIDGET
           WHEN    216
               MOVE    "SRYHEIEXPKBN"   TO  MCP-WIDGET
           WHEN    217
               MOVE    "SRYHEIWRNKBN"   TO  MCP-WIDGET
           WHEN    218
               MOVE    "SRYMATERKKBN"   TO  MCP-WIDGET
           WHEN    219
               MOVE    "INGAIPROCKBN"
                                       TO  MCP-WIDGET
           WHEN    908
               MOVE    "B08"           TO  MCP-WIDGET
           WHEN    912
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SEIHOJNUMCHKKBN"
               MOVE    102         TO  SPA-GMN-CUR
           WHEN    "BYOSRYKAKBN"
               MOVE    103         TO  SPA-GMN-CUR
           WHEN    "SYUBYOCHK"
               MOVE    104         TO  SPA-GMN-CUR
           WHEN    "UTAGAIKKN"
               MOVE    105         TO  SPA-GMN-CUR
           WHEN    "KYUSEIKKNKBN"
               MOVE    106         TO  SPA-GMN-CUR
           WHEN    "COMPBYONISSUKBN"
               MOVE    120         TO  SPA-GMN-CUR
           WHEN    "SRYSTTENKIKBN"
               MOVE    107         TO  SPA-GMN-CUR
           WHEN    "CHKMTRX01"
               MOVE    108         TO  SPA-GMN-CUR
           WHEN    "CHKMTRX02"
               MOVE    109         TO  SPA-GMN-CUR
           WHEN    "CHKMTRX03"
               MOVE    110         TO  SPA-GMN-CUR
           WHEN    "CHKMTRX04"
               MOVE    111         TO  SPA-GMN-CUR
           WHEN    "CHKMTRX05"
               MOVE    112         TO  SPA-GMN-CUR
           WHEN    "TENKITEKBYOKBN"
               MOVE    113         TO SPA-GMN-CUR
           WHEN    "TEKBYOYKZKBN"
               MOVE    114         TO SPA-GMN-CUR
           WHEN    "TEKBYOSRYKBN"
               MOVE    115         TO SPA-GMN-CUR
           WHEN    "YBKENSAYAKUKBN"
               MOVE    116         TO SPA-GMN-CUR
           WHEN    "YBSJTYKZKBN"
               MOVE    117         TO  SPA-GMN-CUR
           WHEN    "YBMSIYKZKBN"
               MOVE    118         TO  SPA-GMN-CUR
           WHEN    "YBSHOHOYKZKBN"
               MOVE    119         TO  SPA-GMN-CUR
           WHEN    "YBUTAGAIKBN"
               MOVE    123         TO  SPA-GMN-CUR
           WHEN    "YKZHEIMSTKBN"
               MOVE    908         TO  SPA-GMN-CUR
           WHEN    "SRYHEIMSTKBN"
               MOVE    216         TO  SPA-GMN-CUR
           WHEN    "SRYHEIEXPKBN"
               MOVE    217         TO  SPA-GMN-CUR
           WHEN    "SRYHEIWRNKBN"
               MOVE    218         TO  SPA-GMN-CUR
           WHEN    "SRYMATERKKBN"
               MOVE    213         TO  SPA-GMN-CUR
           WHEN    "SRYKAISUMSTKBN"
               MOVE    214         TO  SPA-GMN-CUR
           WHEN    "SRYKAISUWRNKBN"
               MOVE    219         TO  SPA-GMN-CUR
           WHEN    "INGAIPROCKBN"
               MOVE    201         TO  SPA-GMN-CUR
           WHEN    "ERR990009KBN"
               MOVE    202         TO  SPA-GMN-CUR
           WHEN    "SYA_INGAITEN"
               MOVE    203         TO SPA-GMN-CUR
           WHEN    "KOK_INGAITEN"
               MOVE    204         TO SPA-GMN-CUR
           WHEN    "KOU_INGAITEN"
               MOVE    205         TO SPA-GMN-CUR
           WHEN    "RSI_INGAITEN"
               MOVE    206         TO SPA-GMN-CUR
           WHEN    "JBI_INGAITEN"
               MOVE    207         TO SPA-GMN-CUR
           WHEN    "KGI_INGAITEN"
               MOVE    208         TO SPA-GMN-CUR
           WHEN    "JIH_INGAITEN"
               MOVE    209         TO SPA-GMN-CUR
           WHEN    "ERRA10001KBN"
               MOVE    210         TO SPA-GMN-CUR
           WHEN    "REIGAICHKKBN"
               MOVE    211         TO  SPA-GMN-CUR
           WHEN    "PRTSRTKBN"
               MOVE    212         TO  SPA-GMN-CUR
           WHEN    "PRTPTLSTKBN"
               MOVE    912         TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC          SECTION.
      *
           IF    ( SPA-GMN-CUR        =    216 )
               IF    ( W60-SRYHEIEXPKBN-STATE =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-CUR        =   217
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               STRING   "疑い病名チェック期間は"
                                       DELIMITED   BY  SIZE
                        "１〜９ヶ月の期間で設定してください"
                                       DELIMITED   BY  SIZE
               INTO     SPA-ERRMSG
               END-STRING
           WHEN    "0101"
               MOVE    "病名チェック条件を設定してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"              THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W60"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします"    TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします"    TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W60"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key8"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key8"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY10-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    CHK004-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               INITIALIZE                  CHK004-REC
               MOVE    1               TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY10-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               INITIALIZE                  CHK004-REC
               MOVE    1               TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY10-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"       TO   MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

