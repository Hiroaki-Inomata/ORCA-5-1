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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW121.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : レセプト印刷情報−編綴詳細設定（Ｗ１２１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/02/07    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-藤原    07/04/16  レセプト請求がない地方公費
      *                                     を入力時の登録を不可とする
      *
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.03.00    NACL-太田    08/06/17  タブ切替ボタン追加
      *
      *  04.07.01    NACL-藤原    12/05/28  タブラベル対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W121.
           03  SPA-W121-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-PREFKBN-PRTJYUNLST
                                              OCCURS  3.
                       09  SPA-GMN-PREFKBN-PRTJYUNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PREFKBN-PRTJYUNMEIL
                                              PIC X(26).
                   07  SPA-GMN-PREFKBN-PRTJYUN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-HKNJANUM-PRTJYUNLST
                                              OCCURS  6.
                       09  SPA-GMN-HKNJANUM-PRTJYUNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-HKNJANUM-PRTJYUNMEIL
                                              PIC X(44).
                   07  SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-HKNJANUM-PRTKBNLST
                                              OCCURS  2.
                       09  SPA-GMN-HKNJANUM-PRTKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-HKNJANUM-PRTKBNMEIL
                                              PIC X(22).
                   07  SPA-GMN-HKNJANUM-PRTKBN-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-PRTJYUN1LST    OCCURS  4.
                       09  SPA-GMN-PRTJYUN1L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN1MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN1-MAX   PIC 9(01).   
      * 
                   07  SPA-GMN-PRTJYUN2LST    OCCURS  4.
                       09  SPA-GMN-PRTJYUN2L  PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-PRTJYUN2MEIL
                                              PIC X(28).
                   07  SPA-GMN-PRTJYUN2-MAX   PIC 9(01).   
      * 
                   07  SPA-GMN-ZAITAKULST     OCCURS  2.
                       09  SPA-GMN-ZAITAKUL   PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-ZAITAKUMEIL
                                              PIC X(24).
                   07  SPA-GMN-ZAITAKU-MAX    PIC 9(02).   
      * 
                   07  SPA-GMN-SRT-LASTLST    OCCURS  2.
                       09  SPA-GMN-SRT-LASTL  PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SRT-LASTMEIL
                                              PIC X(12).
                   07  SPA-GMN-SRT-LAST-MAX   PIC 9(02).   
      * 
                   07  SPA-GMN-CHOUKILST      OCCURS  2.
                       09  SPA-GMN-CHOUKIL    PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-CHOUKIMEIL
                                              PIC X(24).
                   07  SPA-GMN-CHOUKI-MAX     PIC 9(02).   
      * 
                   07  SPA-GMN-SYUBETU-PRTJYUN1LST
                                              OCCURS  10.
                       09  SPA-GMN-SYUBETU-PRTJYUN1L
                                              PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUBETU-PRTJYUN1MEIL
                                              PIC X(28).
                   07  SPA-GMN-SYUBETU-PRTJYUN1-MAX
                                              PIC 9(02).   
      * 
                   07  SPA-GMN-SYUBETU-PRTJYUN2LST
                                              OCCURS  10.
                       09  SPA-GMN-SYUBETU-PRTJYUN2L
                                              PIC X(02).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SYUBETU-PRTJYUN2MEIL
                                              PIC X(28).
                   07  SPA-GMN-SYUBETU-PRTJYUN2-MAX
                                              PIC 9(02).
      *   
                   07  SPA-GMN-HEADLIST.
                       09  SPA-GMN-HEADLIST-TBL    OCCURS  10.
                           11  SPA-GMN-HEADLIST-TNO
                                              PIC 9(02).
                           11  SPA-GMN-HEADLIST-TSTYUKYMD
                                              PIC X(09).
                           11  SPA-GMN-HEADLIST-TEDYUKYMD
                                              PIC X(09).
                       09  SPA-GMN-HEADLIST-MAX
                                              PIC 9(02).
      * 
                   07  SPA-GMN-HEAD-KOUMOKU.
                       09  SPA-GMN-SELNUM1    PIC 9(02).
                       09  SPA-GMN-STYUKYMD   PIC X(10).
                       09  SPA-GMN-EDYUKYMD   PIC X(10).
      * 
                   07  SPA-GMN-HEAD1-KOUMOKU.
                       09  SPA-GMN-PREFKBN-PRTJYUN-G.
                           11  SPA-GMN-PREFKBN-PRTJYUN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PREFKBN-PRTJYUNMEI
                                              PIC X(26).
                       09  SPA-GMN-HKNJANUM-PRTJYUN-G.
                           11  SPA-GMN-HKNJANUM-PRTJYUN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-HKNJANUM-PRTJYUNMEI
                                              PIC X(44).
                       09  SPA-GMN-HKNJANUM   PIC X(08).
                       09  SPA-GMN-HKNJANAME  PIC X(50).
                       09  SPA-GMN-HKNJANUM-PRTKBN-G.
                           11  SPA-GMN-HKNJANUM-PRTKBN
                                              PIC X(01).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-HKNJANUM-PRTKBNMEI
                                              PIC X(22).
      *
                       09  SPA-GMN-HEAD1-OCC  OCCURS  2.
                           11  SPA-GMN-PRTJYUN1-G.
                               13  SPA-GMN-PRTJYUN1
                                               PIC X(02).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-PRTJYUN1MEI
                                               PIC X(28).
                           11  SPA-GMN-PRTJYUN2-G.
                               13  SPA-GMN-PRTJYUN2
                                               PIC X(02).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-PRTJYUN2MEI
                                               PIC X(28).
                           11  SPA-GMN-SRT-LAST-G.
                               13  SPA-GMN-SRT-LAST
                                               PIC X(01).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-SRT-LASTMEI
                                               PIC X(12).
                           11  SPA-GMN-ZAITAKU-G.
                               13  SPA-GMN-ZAITAKU
                                               PIC X(01).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-ZAITAKUMEI
                                               PIC X(24).
                           11  SPA-GMN-CHOUKI-G.
                               13  SPA-GMN-CHOUKI
                                               PIC X(01).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-CHOUKIMEI
                                               PIC X(24).
                           11  SPA-GMN-GRP1-NO-MAX
                                               PIC 9(03).
                           11  SPA-GMN-KOUGAKU-TEN
                                               PIC 9(07).
      *
                   07  SPA-GMN-GRP-KOUMOKU.
                       09  SPA-GMN-GRP-OCC     OCCURS  2.
                           11  SPA-GMN-GRP-SETTEI
                                               PIC X(01).
                           11  SPA-GMN-GRP-KAIJYO
                                               PIC X(01).
                           11  SPA-GMN-GRP1    PIC X(01).
                           11  SPA-GMN-GRP2    PIC X(01).
                           11  SPA-GMN-GRP1-NO PIC X(03).
                           11  SPA-GMN-GRP2-NO PIC X(03).
      *
                           11  SPA-NAI-GRP1-NO PIC 9(03).
      *
                   07  SPA-GMN-BODY-KOUMOKU.
                       09  SPA-GMN-BODY-OCC    OCCURS  2.
                           11  SPA-GMN-RECESYUBETU
                                               PIC X(04).
                           11  SPA-GMN-SYUBETUNAME 
                                               PIC X(40).
                           11  SPA-GMN-SYUBETU-PRTJYUN1-G.
                               13  SPA-GMN-SYUBETU-PRTJYUN1
                                               PIC X(02).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-SYUBETU-PRTJYUN1MEI
                                               PIC X(28).
                           11  SPA-GMN-SYUBETU-PRTJYUN2-G.
                               13  SPA-GMN-SYUBETU-PRTJYUN2
                                               PIC X(02).
                               13  SPA-GMN-F1  PIC X(01).
                               13  SPA-GMN-SYUBETU-PRTJYUN2MEI
                                               PIC X(28).
      *
                           11  SPA-GMN-KOH-KOUMOKU.   
                               13  SPA-GMN-HKNNUM  PIC X(03).
                               13  SPA-GMN-SEIDONAME
                                                   PIC X(20).
      *
                   07  SPA-GMN-BODYLIST.
                       09  SPA-GMN-BODYLIST-OCC    OCCURS  2.
                           11  SPA-GMN-BODYLIST-TBL        OCCURS  50.
                               13  SPA-GMN-BODYLIST-TSYUBETU
                                                       PIC X(04).
                               13  SPA-GMN-BODYLIST-TSYUBETUNAME
                                                       PIC X(40).
                               13  SPA-NAI-BODYLIST-TGRP1-NO.
                                   15  SPA-NAI-BODYLIST-TGRP1-NO-9
                                                       PIC 9(03).
                               13  SPA-NAI-BODYLIST-TGRP2-NO
                                                       PIC X(03).
                               13  SPA-NAI-BODYLIST-TPRTJYUN1
                                                       PIC X(02).
                               13  SPA-NAI-BODYLIST-TPRTJYUN2
                                                       PIC X(02).
      *
                               13  SPA-GMN-KOHLIST.
                                   15  SPA-GMN-KOHLIST-TBL OCCURS  30.
                                       17  SPA-GMN-KOHLIST-THKNNUM
                                                       PIC X(03).
                                       17  SPA-GMN-KOHLIST-TSEIDONAME
                                                       PIC X(20).
                                       17  SPA-GMN-KOHLIST-TJYUN
                                                       PIC X(01).
                                   15  SPA-GMN-KOHLIST-MAX PIC 9(03).
                        09  SPA-GMN-BODYLIST-MAX    PIC 9(02)
                                                    OCCURS  2.
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-HEADLIST.
                       09  SPA-NAI-HEADLIST-TBL    OCCURS  10.
                           11  SPA-NAI-HEADLIST-TSTYUKYMD
                                               PIC 9(08).
                           11  SPA-NAI-HEADLIST-TEDYUKYMD
                                               PIC 9(08).
      *
                   07  SPA-NAI-KOUMOKU.
                       09  SPA-NAI-HEAD-KOUMOKU.
                           11  SPA-NAI-STYUKYMD    PIC 9(08).
                           11  SPA-NAI-EDYUKYMD    PIC 9(08).
      *
                       09  SPA-NAI-BODY-KOUMOKU.
                           11  SPA-NAI-BODY-OCC    OCCURS  2.
                               13  SPA-NAI-SELNUM2     PIC 9(02).
                               13  SPA-NAI-KOH-KOUMOKU.
                                   15  SPA-NAI-SELNUM3 PIC 9(03).
                               13  SPA-NAI-SELNUM2-ROW PIC 9(02).
      *
               05  SPA-1001-PREFNUM    PIC 9(02). 
               05  SPA-W121-PAGENO     PIC S9(9)   BINARY.
      *
      *        レセプト印刷情報（編綴順）
               05  SPA-SYS-2008-REC    PIC X(600).
      *
               05  SPA-SYS-2003-TBL    OCCURS  10.
                   07  SPA-SYS-2003-STYUKYMD   PIC 9(08). 
                   07  SPA-SYS-2003-EDYUKYMD   PIC 9(08). 
               05  SPA-SYS-2003-MAX    PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-RECE-PUTH       PIC 9(01).
           03  FLG-RECE-PUTB       PIC 9(01).
      *
           03  FLG-HEADLIST-ROW    PIC 9(01).
           03  FLG-BODYLIST-ROW    PIC 9(01)   OCCURS  2.
           03  FLG-KOHLIST-ROW     PIC 9(01)   OCCURS  2.
           03  FLG-UPDFLG          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-PAGENO          PIC 9(01).
      *
           03  WRK-SELNUM2         PIC 9(03).
           03  WRK-SELNUM3         PIC 9(03).
      *
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ9             PIC ZZ9.
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-W121-HEADLIST-ROW
                                   PIC S9(09).
           03  WRK-W121-BODYLIST-ROW
                                   PIC S9(09)   OCCURS  2.
           03  WRK-W121-KOHLIST-ROW
                                   PIC S9(09)   OCCURS  2.
      *
           03  WRK-TOP-CNT         PIC 9(03).
           03  WRK-BOTTOM-CNT      PIC 9(03).
      *
           03  WRK-KOH-KBN         PIC X(01). 
      *
      *    公費並び設定用ワーク
           03  WRK-GMN-KOHLIST-AREA.
               05  WRK-GMN-KOHLIST-MAX
                                   PIC 9(03)   OCCURS  2.
               05  WRK-GMN-KOHLIST-TABLE.
                   07  WRK-GMN-KOHLIST-T       OCCURS  2.
                       09  WRK-GMN-KOHLIST-TBL OCCURS  30.
                           11  WRK-GMN-KOHLIST-THKNNUM
                                   PIC X(03).
                           11  WRK-GMN-KOHLIST-TSEIDONAME
                                   PIC X(20).
                           11  WRK-GMN-KOHLIST-TJYUN
                                   PIC X(01).
               05  WRK-GMN-KOHLIST-TABLE-R
                                   REDEFINES  WRK-GMN-KOHLIST-TABLE.
                   07  WRK-GMN-KOHLIST-TBL-R  OCCURS  60.
                       09  WRK-GMN-KOHLIST-THKNNUM-R
                                   PIC X(03).
                       09  WRK-GMN-KOHLIST-TSEIDONAME-R
                                   PIC X(20).
                       09  WRK-GMN-KOHLIST-TJYUN-R
                                   PIC X(01).
      *
      *    公費設定用ワーク
           03  WRK-GMN-BODYLIST-AREA.
               05  WRK-GMN-BODYLIST-TBL    OCCURS  50.
                   07  WRK-GMN-BODYLIST-TSYUBETU
                                   PIC X(04).
                   07  WRK-GMN-BODYLIST-TSYUBETUNAME
                                   PIC X(40).
                   07  WRK-NAI-BODYLIST-TGRP1-NO
                                   PIC X(03).
                   07  WRK-NAI-BODYLIST-TGRP2-NO
                                   PIC X(03).
                   07  WRK-NAI-BODYLIST-TPRTJYUN1
                                   PIC X(02).
                   07  WRK-NAI-BODYLIST-TPRTJYUN2
                                   PIC X(02).
                   07  WRK-GMN-BODYLIST-KOHLIST.
                       09  WRK-GMN-BODYLIST-KOHLIST-TBL OCCURS  30.
                           11  WRK-GMN-BODYLIST-KOHLIST-THKNNM
                                               PIC X(03).
                           11  WRK-GMN-BODYLIST-KOHLIST-TSEINM
                                               PIC X(20).
                           11  WRK-GMN-BODYLIST-KOHLIST-TJYUN
                                               PIC X(01).
                       09  WRK-GMN-BODYLIST-KOHLIST-MAX PIC 9(03).
      *
           03  WRK-GRP-INDEX       PIC 9(02).
           03  WRK-INDEX           PIC 9(02).
           03  WRK-SYUBETU-SRT1    PIC 9(03).
           03  WRK-SYUBETU-GAI     PIC X(04).
           03  WRK-SYUBETU-NYU     PIC X(04).
           03  WRK-GRP1-NO         PIC X(03).
      *
           03  WRK-2003-TABLE.
               05  WRK-2003-TBL    OCCURS  50.
                   07  WRK-2003-TSYUBETU
                                   PIC X(04).
                   07  WRK-2003-TSYUBETUNAME
                                   PIC X(40).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    ワーク更新用編綴管理レコード（明細部）
       01  WRK-RECE-PUTB-REC.
           COPY    "CPRECE-PUTB.INC" 
                                   REPLACING  //RECE-PUTB//
                                   BY         //WRK-RECE-PUTB//.
      *
       01  WRK-CONST-AREA.
           03  WRK-CONS-HEADLIST-MAX
                                   PIC 9(02)   VALUE   10.
           03  WRK-CONS-BODYLIST-MAX
                                   PIC 9(02)   VALUE   50.
           03  WRK-CONS-KOHLIST-MAX
                                   PIC 9(02)   VALUE   30.
           03  WRK-CONS-SRYYM-200610
                                   PIC X(08)   VALUE   "20061001".
           03  WRK-CONS-SRYYM-20080401
                                   PIC 9(08)   VALUE   20080401.
           03  WRK-CONS-SRYYM-20080331
                                   PIC 9(08)   VALUE   20080331.
      *         
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    レセプト作成指示（印刷順）情報
           COPY  "CPSK2003.INC".
      *
      *    レセプト印刷情報（編綴順）
           COPY  "CPSK2008.INC".
      *
      *    保険者番号
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    編綴管理レコード（管理部）
       01  RECE-PUTH-REC.
           COPY    "CPRECE-PUTH.INC".
      *
      *    編綴管理レコード（明細部）
       01  RECE-PUTB-REC.
           COPY    "CPRECE-PUTB.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W121
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W121        TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W121"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE    SPA-WIDCD
                   WHEN    "1001"
                       PERFORM 490-KOUSHIN-SEC
                   WHEN    "1002"
                       PERFORM 490-CLEAR-SEC
                   WHEN    "1003"
                       PERFORM 490-EDYUKYMD-KOUSHIN-SEC
                   WHEN    "1004"
                       MOVE    SPACE           TO  SPA-WID1-FLG
                       MOVE    SPACE           TO  SPA-WIDCD      
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                       GO  TO  330-WID1-SET-EXT
               END-EVALUATE
           ELSE    
               EVALUATE    SPA-WIDCD
                   WHEN    "1003"
                       MOVE    1               TO  SPA-GMN-CUR
                       INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                                   SPA-NAI-KOUMOKU
                                                   SPA-GMN-BODY-KOUMOKU
                                                   SPA-GMN-GRP-KOUMOKU
                                                   SPA-GMN-BODYLIST
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W121-PAGENO
               WHEN    ZERO 
                   IF      SPA-NAI-SELNUM2 (1)
                                       >   ZERO
                       MOVE    SPA-NAI-SELNUM2 (1)
                                               TO  W121-BODYLIST-ROW
                       MOVE    ZERO            TO  FLG-BODYLIST-ROW (1)
                   END-IF
               WHEN    1 
                   IF      SPA-NAI-SELNUM2 (2)
                                       >   ZERO
                       MOVE    SPA-NAI-SELNUM2 (2)
                                               TO  W121-BODYLIST-1-ROW
                       MOVE    ZERO            TO  FLG-BODYLIST-ROW (2)
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-W121-AREA
      *
           MOVE    "1"             TO  SPA-GMN-PREFKBN-PRTJYUNL    (1)
           MOVE    "県内・県外"    TO  SPA-GMN-PREFKBN-PRTJYUNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-PREFKBN-PRTJYUNL    (2)
           MOVE    "県外・県内"    TO  SPA-GMN-PREFKBN-PRTJYUNMEIL (2)
           MOVE    "3"             TO  SPA-GMN-PREFKBN-PRTJYUNL    (3)
           MOVE    "当該画面の編綴は使用しない"
                                   TO  SPA-GMN-PREFKBN-PRTJYUNMEIL (3)
           MOVE    3               TO  SPA-GMN-PREFKBN-PRTJYUN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (1)
           MOVE    "市町村国保・国保組合・県内扱いの県外国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (2)
           MOVE    "市町村国保・県内扱いの県外国保組合・国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (2)
           MOVE    "3"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (3)
           MOVE    "国保組合・市町村国保・県内扱いの県外国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (3)
           MOVE    "4"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (4)
           MOVE    "国保組合・県内扱いの県外国保組合・市町村国保"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (4)
           MOVE    "5"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (5)
           MOVE    "県内扱いの県外国保組合・市町村国保・国保組合"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (5)
           MOVE    "6"             TO  SPA-GMN-HKNJANUM-PRTJYUNL    (6)
           MOVE
               "県内扱いの県外国保組合・国保組合・市町村国保"
                                   TO  SPA-GMN-HKNJANUM-PRTJYUNMEIL (6)
           MOVE    6               TO  SPA-GMN-HKNJANUM-PRTJYUN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-HKNJANUM-PRTKBNL    (1)
           MOVE    "市町村国保の先頭に印刷" 
                                   TO  SPA-GMN-HKNJANUM-PRTKBNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-HKNJANUM-PRTKBNL    (2)
           MOVE    "市町村国保の最後に印刷"
                                   TO  SPA-GMN-HKNJANUM-PRTKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-HKNJANUM-PRTKBN-MAX
      *
           MOVE    "01"            TO  SPA-GMN-PRTJYUN1L   (1)
           MOVE    "保険者"        TO  SPA-GMN-PRTJYUN1MEIL(1)
           MOVE    "02"            TO  SPA-GMN-PRTJYUN1L   (2)
           MOVE    "レセプト種別"  TO  SPA-GMN-PRTJYUN1MEIL(2)
           MOVE    2               TO  SPA-GMN-PRTJYUN1-MAX 
      *
           MOVE    "01"            TO  SPA-GMN-PRTJYUN2L   (1)
           MOVE    "市町村国保は本人家族で分ける" 
                                   TO  SPA-GMN-PRTJYUN2MEIL(1)
           MOVE    "02"            TO  SPA-GMN-PRTJYUN2L   (2)
           MOVE    "組合国保は本人家族で分ける" 
                                   TO  SPA-GMN-PRTJYUN2MEIL(2)
           MOVE    2               TO  SPA-GMN-PRTJYUN2-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-SRT-LASTL   (1)
           MOVE    "カナ氏名順"    TO  SPA-GMN-SRT-LASTMEIL(1)
           MOVE    "2"             TO  SPA-GMN-SRT-LASTL   (2)
           MOVE    "患者番号順"    TO  SPA-GMN-SRT-LASTMEIL(2)
           MOVE    2               TO  SPA-GMN-SRT-LAST-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-ZAITAKUL    (1)
           MOVE    "県内・県外の先頭に印刷" 
                                   TO  SPA-GMN-ZAITAKUMEIL (1)
           MOVE    "2"             TO  SPA-GMN-ZAITAKUL    (2)
           MOVE    "保険者の先頭に印刷"
                                   TO  SPA-GMN-ZAITAKUMEIL (2)
           MOVE    2               TO  SPA-GMN-ZAITAKU-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-CHOUKIL    (1)
           MOVE    "県内・県外の先頭に印刷" 
                                   TO  SPA-GMN-CHOUKIMEIL (1)
           MOVE    "2"             TO  SPA-GMN-CHOUKIL    (2)
           MOVE    "保険者の先頭に印刷"
                                   TO  SPA-GMN-CHOUKIMEIL (2)
           MOVE    2               TO  SPA-GMN-CHOUKI-MAX 
      *
           MOVE    "01"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (1)
           MOVE    "給付割合でくくる（昇順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(1)
           MOVE    "02"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (2)
           MOVE    "給付割合でくくる（降順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(2)
           MOVE    "03"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (3)
           MOVE    "法別番号でくくる（昇順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(3)
           MOVE    "04"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (4)
           MOVE    "法別番号でくくる（降順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(4)
           MOVE    "05"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (5)
           MOVE    "記号番号順（昇順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(5)
           MOVE    "06"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (6)
           MOVE    "受給者番号順（昇順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(6)
           MOVE    "07"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN1L   (7)
           MOVE    "保険者でくくる"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN1MEIL(7)
           MOVE    7               TO  SPA-GMN-SYUBETU-PRTJYUN1-MAX 
      *
           MOVE    "01"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN2L   (1)
           MOVE    "給付割合でくくる（昇順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN2MEIL(1)
           MOVE    "02"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN2L   (2)
           MOVE    "給付割合でくくる（降順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN2MEIL(2)
           MOVE    "03"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN2L   (3)
           MOVE    "法別番号でくくる（昇順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN2MEIL(3)
           MOVE    "04"            TO
                                   SPA-GMN-SYUBETU-PRTJYUN2L   (4)
           MOVE    "法別番号でくくる（降順）"
                                   TO
                                   SPA-GMN-SYUBETU-PRTJYUN2MEIL(4)
           MOVE    4               TO  SPA-GMN-SYUBETU-PRTJYUN2-MAX 
      *
      *    医療機関情報
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1001-REC
               MOVE    SYS-1001-PREFNUM        TO  SPA-1001-PREFNUM
           END-IF
      *
      *    レセプト（編綴順）情報
           MOVE    SPACE               TO  SYS-2008-REC
           INITIALIZE                      SYS-2008-REC
           MOVE    "2008"              TO  SYS-2008-KANRICD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-2008-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-2008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2008-HOSPNUM
           MOVE    SYS-2008-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key13"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2008-REC
               MOVE    SYS-2008-REC        TO  SPA-SYS-2008-REC
           ELSE
               INITIALIZE                      SPA-SYS-2008-REC
           END-IF
      *
      *    レセプト種別適用期間情報
      *    （医保分のみ開始日で２００６年１０月以降のみまとめる）
           MOVE    SPACE               TO  SYS-2003-KEY
           MOVE    "2003"              TO  SYS-2003-KANRICD
           MOVE    "200"               TO  SYS-2003-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-2003-HOSPNUM
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-2003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM     UNTIL   FLG-SYSKANRI     =   1
               MOVE    MCPDATA-REC     TO  SYS-2003-REC
      *
               IF      SYS-2003-STYUKYMD   >=  WRK-CONS-SRYYM-200610
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       10
                       IF      SPA-SYS-2003-EDYUKYMD (IDX)
                                               =   ZERO
                           MOVE    SYS-2003-STYUKYMD
                                       TO  SPA-SYS-2003-STYUKYMD (IDX)
                           MOVE    SYS-2003-EDYUKYMD
                                       TO  SPA-SYS-2003-EDYUKYMD (IDX)
                           MOVE    IDX TO  SPA-SYS-2003-MAX 
                       END-IF
                       IF      SPA-SYS-2003-STYUKYMD (IDX)
                                               =   SYS-2003-STYUKYMD
                           MOVE    10              TO  IDX
                       END-IF 
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    管理部一覧編集処理
           PERFORM 3101-SPA-HENSYU-SEC
      *
      *    レセプト（編綴順）情報編集処理
           PERFORM 3102-SPA-2008-HENSYU-SEC
      *
           MOVE    "T"                 TO  SPA-GMN-GRP1 (1)
                                           SPA-GMN-GRP1 (2)
           MOVE    "F"                 TO  SPA-GMN-GRP2 (1)
                                           SPA-GMN-GRP2 (2)
      *
           MOVE    1                   TO  FLG-HEADLIST-ROW
                                           FLG-BODYLIST-ROW (1)
                                           FLG-BODYLIST-ROW (2)
                                           FLG-KOHLIST-ROW  (1)
                                           FLG-KOHLIST-ROW  (2)
                                           
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト特記事項編集情報処理
      *****************************************************************
       3101-SPA-HENSYU-SEC         SECTION.
      *
           INITIALIZE              SPA-GMN-HEADLIST
                                   SPA-NAI-HEADLIST
      *
           INITIALIZE                  RECE-PUTH-REC
           MOVE    SPA-HOSPNUM      TO  RECE-PUTH-HOSPNUM
           MOVE    RECE-PUTH-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_rece_puth" TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-RECE-PUTH-READ-N-SEC
           ELSE
               INITIALIZE                  RECE-PUTH-REC
               MOVE    1               TO  FLG-RECE-PUTH
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM     UNTIL   FLG-RECE-PUTH
                                           =   1
                           OR      IDX    >=  WRK-CONS-HEADLIST-MAX
      *
               ADD     1                   TO  IDX
               MOVE    IDX                 TO
                                       SPA-GMN-HEADLIST-TNO (IDX)
               MOVE    RECE-PUTH-STYUKYMD  TO
                                       SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
               MOVE    RECE-PUTH-STYUKYMD  TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO
                                       SPA-GMN-HEADLIST-TSTYUKYMD (IDX)
               MOVE    RECE-PUTH-EDYUKYMD  TO
                                       SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
               MOVE    RECE-PUTH-EDYUKYMD  TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO
                                       SPA-GMN-HEADLIST-TEDYUKYMD (IDX)
      *
               MOVE    "tbl_rece_puth" TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-RECE-PUTH-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-HEADLIST-MAX
      *
          MOVE    1                TO  SPA-GMN-CUR
           .
       3101-SPA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト（編綴順）情報編集処理
      *****************************************************************
       3102-SPA-2008-HENSYU-SEC    SECTION.
      *
           MOVE    SPA-SYS-2008-REC    TO  SYS-2008-REC
      *
           IF      SYS-2008-PREFKBN-PRTJYUN
                                       =   SPACE
               MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (1)
                                   TO  SPA-GMN-PREFKBN-PRTJYUN-G
           ELSE
               MOVE    SYS-2008-PREFKBN-PRTJYUN
                                       TO  SPA-GMN-PREFKBN-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PREFKBN-PRTJYUN-MAX
                   IF      SPA-GMN-PREFKBN-PRTJYUN
                                       =   SPA-GMN-PREFKBN-PRTJYUNL(IDY)
                       MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                       TO  SPA-GMN-PREFKBN-PRTJYUN-G
                       MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                       TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SYS-2008-HKNJANUM-PRTJYUN
                                       =   SPACE
               MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (1)
                                   TO  SPA-GMN-HKNJANUM-PRTJYUN-G
           ELSE
               MOVE    SYS-2008-HKNJANUM-PRTJYUN
                                       TO  SPA-GMN-HKNJANUM-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-HKNJANUM-PRTJYUN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTJYUN
                                   =   SPA-GMN-HKNJANUM-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                       TO  SPA-GMN-HKNJANUM-PRTJYUN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                       TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           MOVE    SYS-2008-HKNJANUM-PRTKBN
                                       TO  SPA-GMN-HKNJANUM-PRTKBN
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-HKNJANUM-PRTKBN-MAX
               IF      SPA-GMN-HKNJANUM-PRTKBN
                                       =   SPA-GMN-HKNJANUM-PRTKBNL(IDY)
                   MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                       TO  SPA-GMN-HKNJANUM-PRTKBN-G
                   MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-2008-HKNJANUM   TO  SPA-GMN-HKNJANUM
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           PERFORM 900-HKNJAINF-READ-SEC
           MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME
      *         
           MOVE    SYS-2008-ZAITAKU    TO  SPA-GMN-ZAITAKU (1)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-ZAITAKU-MAX
               IF      SPA-GMN-ZAITAKU (1) =   SPA-GMN-ZAITAKUL (IDY)
                   MOVE    SPA-GMN-ZAITAKULST (IDY)
                                       TO  SPA-GMN-ZAITAKU-G (1)
                   MOVE    SPA-GMN-ZAITAKU-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-ZAITAKU-G (1)
                                       TO  SPA-GMN-ZAITAKU-G (2)
      *
           MOVE    SYS-2008-CHOUKI     TO  SPA-GMN-CHOUKI (1)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-CHOUKI-MAX
               IF      SPA-GMN-CHOUKI  (1) =   SPA-GMN-CHOUKIL (IDY)
                   MOVE    SPA-GMN-CHOUKILST (IDY)
                                       TO  SPA-GMN-CHOUKI-G (1)
                   MOVE    SPA-GMN-CHOUKI-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-CHOUKI-G (1)
                                       TO  SPA-GMN-CHOUKI-G (2)
      *
           .
       3102-SPA-2008-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別取得処理
      *****************************************************************
       3103-2003-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX
           INITIALIZE                      WRK-2003-TABLE 
      * 
      *    レセプト作成指示（印刷順）読み込み
           MOVE    SPACE               TO  SYS-2003-KEY
           MOVE    "2003"              TO  SYS-2003-KANRICD
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-2003-STYUKYMD
                                           SYS-2003-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-2003-HOSPNUM
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-2003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM    UNTIL   FLG-SYSKANRI     =   1
               MOVE    MCPDATA-REC   TO  SYS-2003-REC
               IF    ( SYS-2003-TEISYUTUSAKI   =   "2" )
               AND   ( SYS-2003-NYUGAIKBN      =   "2" )
                   MOVE    SYS-2003-DSPSEQ         TO  IDX
                   MOVE    SYS-2003-SYUBETU (1)    TO
                                           WRK-2003-TSYUBETU     (IDX)
                   MOVE    SYS-2003-SYURUINAME     TO
                                           WRK-2003-TSYUBETUNAME (IDX)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           . 
       3103-2003-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.  
      *
           IF      SPA-W121-PAGENO     =   0
               MOVE    1           TO  WRK-PAGENO
           ELSE
               MOVE    2           TO  WRK-PAGENO
           END-IF
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      * 
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 490-CLEAR-GID-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    ↑
               WHEN    "CLICKED"       ALSO    "SYUBETU_UP"
               WHEN    "CLICKED"       ALSO    "SYUBETU_UP_1"
                   PERFORM 490-SYUBETU-UP-SEC
      *    ↓
               WHEN    "CLICKED"       ALSO    "SYUBETU_DOWN"
               WHEN    "CLICKED"       ALSO    "SYUBETU_DOWN_1"
                   PERFORM 490-SYUBETU-DOWN-SEC
      *    グループ設定
               WHEN    "CLICKED"       ALSO    "GRP_SETTEI"
               WHEN    "CLICKED"       ALSO    "GRP_SETTEI_1"
                   PERFORM 490-GRP-SETTEI-SEC
      *    グループ解除
               WHEN    "CLICKED"       ALSO    "GRP_KAIJYO"
               WHEN    "CLICKED"       ALSO    "GRP_KAIJYO_1"
                   PERFORM 490-GRP-KAIJYO-SEC
      *    グループ１
               WHEN    "CLICKED"       ALSO    "GRP1"
               WHEN    "CLICKED"       ALSO    "GRP1_1"
                   PERFORM 490-GRP1-SETTEI-SEC
      *    グループ２
               WHEN    "CLICKED"       ALSO    "GRP2"
               WHEN    "CLICKED"       ALSO    "GRP2_1"
                   PERFORM 490-GRP2-SETTEI-SEC
      *    ↑
               WHEN    "CLICKED"       ALSO    "KOH_UP"
               WHEN    "CLICKED"       ALSO    "KOH_UP_1"
                   PERFORM 490-KOH-UP-SEC
      *    ↓
               WHEN    "CLICKED"       ALSO    "KOH_DOWN"
               WHEN    "CLICKED"       ALSO    "KOH_DOWN_1"
                   PERFORM 490-KOH-DOWN-SEC
      *    先頭
               WHEN    "CLICKED"       ALSO    "TOP"
               WHEN    "CLICKED"       ALSO    "TOP_1"
                   PERFORM 490-KOH-TOP-HENSYU-SEC
      *    後尾
               WHEN    "CLICKED"       ALSO    "BOTTOM"
               WHEN    "CLICKED"       ALSO    "BOTTOM_1"
                   PERFORM 490-KOH-BOTTOM-HENSYU-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "DEL"
               WHEN    "CLICKED"       ALSO    "DEL_1"
                   PERFORM 490-KOH-DELETE-SEC
      *    タグ
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 440-TAG-CHANGE-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           IF      SPA-W121-PAGENO     =   0
               MOVE    1           TO  WRK-PAGENO
           ELSE
               MOVE    2           TO  WRK-PAGENO
           END-IF
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM1"
                   PERFORM 4101-SELNUM1-CHK-SEC
      *
               WHEN    ANY             ALSO    "HEADLIST"
                   PERFORM 41010-HEADLIST-SEL-SEC
      *
      *        有効開始年月日
               WHEN    "ACTIVATE"       ALSO    "STYUKYMD"
                   PERFORM 41021-STYUKYMD-CHK-SEC
      *        有効終了年月日
               WHEN    "ACTIVATE"       ALSO    "EDYUKYMD"
                   PERFORM 41021-EDYUKYMD-CHK-SEC
      *        グループ１番号
               WHEN    "ACTIVATE"       ALSO    "GRP1_NO"
               WHEN    "ACTIVATE"       ALSO    "GRP1_NO_1"
                   PERFORM 41021-GRP1-NO-CHK-SEC
      *        グループ２番号
               WHEN    "ACTIVATE"       ALSO    "GRP2_NO"
               WHEN    "ACTIVATE"       ALSO    "GRP2_NO_1"
                   PERFORM 41021-GRP2-NO-CHK-SEC
      *        タグ
               WHEN    "SWITCH"        ALSO    "nt1"
                   PERFORM 440-TAG-CHANGE-SEC
      *        一括分
               WHEN    OTHER
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       4101-SELNUM1-CHK-SEC          SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-GMN-BODY-KOUMOKU
                                       SPA-GMN-GRP-KOUMOKU
                                       SPA-GMN-BODYLIST
      ***                              SPA-GMN-BODYLIST-MAX
      *
           MOVE    W121-SELNUM1    TO  SPA-GMN-SELNUM1
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM1 =   ZERO
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM1-CHK-EXT
           ELSE
               IF    ( SPA-GMN-SELNUM1 >=  1                    )
               AND   ( SPA-GMN-SELNUM1 <=  SPA-GMN-HEADLIST-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0001"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE    
               PERFORM 41011-SELNUM1-HENSYU-SEC
           END-IF
           .
       4101-SELNUM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41011-SELNUM1-HENSYU-SEC        SECTION.
      *
      *    平成２０年４月を跨ってるとき
           IF    ( SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       <   WRK-CONS-SRYYM-20080401 )
           AND   ( SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                       >=  WRK-CONS-SRYYM-20080401 )
               MOVE    "1003"              TO  SPA-WIDCD
               GO  TO  41011-SELNUM1-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  SPA-GMN-HEAD1-KOUMOKU
      *
      *    ＨＥＡＤ部表示
           INITIALIZE                      RECE-PUTH-REC
           MOVE    SPA-HOSPNUM          TO  RECE-PUTH-HOSPNUM
           MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTH-STYUKYMD
           MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTH-EDYUKYMD
           MOVE    RECE-PUTH-REC       TO  MCPDATA-REC
           PERFORM 900-RECE-PUTH-READ-SEC
           IF     FLG-RECE-PUTH        =   ZERO
               MOVE    RECE-PUTH-STYUKYMD  TO  SPA-NAI-STYUKYMD
               MOVE    RECE-PUTH-STYUKYMD  TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
               MOVE    RECE-PUTH-EDYUKYMD  TO  SPA-NAI-EDYUKYMD
               MOVE    RECE-PUTH-EDYUKYMD  TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
               MOVE    RECE-PUTH-PREFKBN-PRTJYUN
                                           TO  SPA-GMN-PREFKBN-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PREFKBN-PRTJYUN-MAX
                   IF      SPA-GMN-PREFKBN-PRTJYUN
                                   =   SPA-GMN-PREFKBN-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                           TO  SPA-GMN-PREFKBN-PRTJYUN-G
                       MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    RECE-PUTH-HKNJANUM-PRTJYUN
                                         TO  SPA-GMN-HKNJANUM-PRTJYUN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-HKNJANUM-PRTJYUN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTJYUN
                                   =   SPA-GMN-HKNJANUM-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                           TO
                                           SPA-GMN-HKNJANUM-PRTJYUN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    RECE-PUTH-HKNJANUM-PRTKBN
                                           TO  SPA-GMN-HKNJANUM-PRTKBN
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-HKNJANUM-PRTKBN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTKBN
                               =   SPA-GMN-HKNJANUM-PRTKBNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                           TO  SPA-GMN-HKNJANUM-PRTKBN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                           TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    RECE-PUTH-HKNJANUM  TO  SPA-GMN-HKNJANUM
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME
      *         
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       2  
                 MOVE    RECE-PUTH-SRT-01 (IDXX)
                                             TO  SPA-GMN-PRTJYUN1 (IDXX)
                 PERFORM VARYING IDY FROM    1   BY  1
                         UNTIL   IDY >       SPA-GMN-PRTJYUN1-MAX
                   IF      SPA-GMN-PRTJYUN1 (IDXX)
                                         =   SPA-GMN-PRTJYUN1L (IDY)
                       MOVE    SPA-GMN-PRTJYUN1LST (IDY)
                                           TO  SPA-GMN-PRTJYUN1-G (IDXX)
                       MOVE    SPA-GMN-PRTJYUN1-MAX
                                           TO  IDY
                   END-IF
                 END-PERFORM
      *         
                 MOVE    RECE-PUTH-KKHKBN (IDXX)
                                             TO  SPA-GMN-PRTJYUN2 (IDXX)
                 PERFORM VARYING IDY FROM    1   BY  1
                         UNTIL   IDY >       SPA-GMN-PRTJYUN2-MAX
                   IF      SPA-GMN-PRTJYUN2 (IDXX)
                                       =   SPA-GMN-PRTJYUN2L (IDY)
                       MOVE    SPA-GMN-PRTJYUN2LST (IDY)
                                           TO  SPA-GMN-PRTJYUN2-G (IDXX)
                       MOVE    SPA-GMN-PRTJYUN2-MAX
                                           TO  IDY
                   END-IF
                 END-PERFORM
      *         
                 MOVE    RECE-PUTH-SRT-LAST (IDXX)
                                             TO  SPA-GMN-SRT-LAST (IDXX)
                 PERFORM VARYING IDY FROM    1   BY  1
                         UNTIL   IDY >       SPA-GMN-SRT-LAST-MAX
                   IF      SPA-GMN-SRT-LAST (IDXX)
                                       =   SPA-GMN-SRT-LASTL (IDY)
                       MOVE    SPA-GMN-SRT-LASTLST (IDY)
                                           TO  SPA-GMN-SRT-LAST-G (IDXX)
                       MOVE    SPA-GMN-SRT-LAST-MAX
                                           TO  IDY
                   END-IF
                 END-PERFORM
      *         
                 MOVE    RECE-PUTH-KOUGAKU-TEN (IDXX)
                                           TO  SPA-GMN-KOUGAKU-TEN(IDXX)
      *         
                 MOVE    RECE-PUTH-ZAITAKU (IDXX)
                                             TO  SPA-GMN-ZAITAKU (IDXX)
                 PERFORM VARYING IDY FROM    1   BY  1
                         UNTIL   IDY >       SPA-GMN-ZAITAKU-MAX
                   IF      SPA-GMN-ZAITAKU (IDXX)
                                       =   SPA-GMN-ZAITAKUL (IDY)
                       MOVE    SPA-GMN-ZAITAKULST (IDY)
                                           TO  SPA-GMN-ZAITAKU-G (IDXX)
                       MOVE    SPA-GMN-ZAITAKU-MAX
                                           TO  IDY
                   END-IF
                 END-PERFORM
      *
                 MOVE    RECE-PUTH-CHOUKI (IDXX)
                                             TO  SPA-GMN-CHOUKI (IDXX)
                 PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-CHOUKI-MAX
                   IF      SPA-GMN-CHOUKI (IDXX)
                                       =   SPA-GMN-CHOUKIL (IDY)
                       MOVE    SPA-GMN-CHOUKILST (IDY)
                                           TO  SPA-GMN-CHOUKI-G (IDXX)
                       MOVE    SPA-GMN-CHOUKI-MAX
                                           TO  IDY
                   END-IF
                 END-PERFORM
               END-PERFORM
           ELSE
               MOVE    "0101"              TO  SPA-ERRCD
               GO  TO  41011-SELNUM1-HENSYU-EXT
           END-IF
      *
      *    レセプト種別取得処理
           PERFORM 3103-2003-HENSYU-SEC
      *
      *    ＢＯＤＹ部表示
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2  
               INITIALIZE                  RECE-PUTB-REC
               MOVE    SPA-HOSPNUM      TO  RECE-PUTB-HOSPNUM
               EVALUATE    IDXX
                   WHEN    1
                       MOVE    "1"             TO  RECE-PUTB-PREFKBN
                   WHEN    2
                       MOVE    "2"             TO  RECE-PUTB-PREFKBN
               END-EVALUATE 
               MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTB-STYUKYMD
               MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTB-EDYUKYMD
               MOVE    RECE-PUTB-REC   TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "tbl_rece_putb" TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 900-RECE-PUTB-READ-N-SEC
               ELSE
                   INITIALIZE                  RECE-PUTB-REC
                   MOVE    1               TO  FLG-RECE-PUTB
               END-IF
      *
               MOVE    ZERO            TO  IDX
               PERFORM     UNTIL   FLG-RECE-PUTB   =   1
                           OR      IDX             >=
                                               WRK-CONS-BODYLIST-MAX
      *
                   EVALUATE    RECE-PUTB-KOUHI-NO
                       WHEN    ZERO
                           ADD     1   TO  IDX
                           MOVE    RECE-PUTB-GRP-NO1
                               TO  SPA-NAI-BODYLIST-TGRP1-NO (IDXX IDX)
                           IF      SPA-GMN-GRP1-NO-MAX (IDXX)  <
                               SPA-NAI-BODYLIST-TGRP1-NO-9 (IDXX IDX)
                               MOVE    SPA-NAI-BODYLIST-TGRP1-NO-9
                                                           (IDXX IDX)
                                       TO  SPA-GMN-GRP1-NO-MAX (IDXX)
                           END-IF
                           MOVE    RECE-PUTB-GRP-NO2
                               TO  SPA-NAI-BODYLIST-TGRP2-NO (IDXX IDX)
                           MOVE    RECE-PUTB-SYUBETU-GAI
                               TO  SPA-GMN-BODYLIST-TSYUBETU (IDXX IDX)
                           PERFORM VARYING IDZ FROM    1   BY  1
                                   UNTIL   IDZ >       50
                                   OR  WRK-2003-TSYUBETU (IDZ) =   SPACE
                               IF      RECE-PUTB-SYUBETU-GAI
                                           =   WRK-2003-TSYUBETU (IDZ)
                                   MOVE    WRK-2003-TSYUBETUNAME (IDZ)
                                       TO  SPA-GMN-BODYLIST-TSYUBETUNAME
                                                              (IDXX IDX)
                               END-IF
                           END-PERFORM
                           MOVE    RECE-PUTB-SYUBETU-UNDER-NO1
                               TO  SPA-NAI-BODYLIST-TPRTJYUN1 (IDXX IDX)
                           MOVE    RECE-PUTB-SYUBETU-UNDER-NO2
                               TO  SPA-NAI-BODYLIST-TPRTJYUN2 (IDXX IDX)
                       WHEN    OTHER
                           ADD     1   TO  SPA-GMN-KOHLIST-MAX(IDXX IDX)
                           MOVE    SPA-GMN-KOHLIST-MAX (IDXX IDX)
                                       TO  IDY
                           MOVE    RECE-PUTB-KOUHI-NUM
                                       TO  SPA-GMN-KOHLIST-THKNNUM
                                                       (IDXX IDX IDY)
                           IF      RECE-PUTB-KOUHI-NO  <   500
                               MOVE    "T"                                    
                                   TO  SPA-GMN-KOHLIST-TJYUN
                                                       (IDXX IDX IDY)
                           ELSE
                               MOVE    "B"                                    
                                   TO  SPA-GMN-KOHLIST-TJYUN
                                                       (IDXX IDX IDY)
                           END-IF
                           EVALUATE    RECE-PUTB-KOUHI-NUM
                               WHEN    "XXX"
                                   MOVE    "長期"              TO
                                       SPA-GMN-KOHLIST-TSEIDONAME
                                                         (IDXX IDX IDY)
                               WHEN    "YYY"
                                   MOVE    "在総診・在医総"    TO
                                       SPA-GMN-KOHLIST-TSEIDONAME
                                                         (IDXX IDX IDY)
                               WHEN    OTHER
                                   INITIALIZE              HKNNUM-REC
                                   MOVE    SPA-HOSPNUM  TO  HKN-HOSPNUM
                                   MOVE    RECE-PUTB-KOUHI-NUM
                                                       TO  HKN-HKNNUM
                                   MOVE    "00"        TO  HKN-PAYKBN
                                   MOVE    HKNNUM-REC  TO  MCPDATA-REC
                                   PERFORM 900-HKNNUM-READ-SEC
                                   MOVE    HKN-TANSEIDONAME    TO
                                       SPA-GMN-KOHLIST-TSEIDONAME
                                                        (IDXX IDX IDY)
                           END-EVALUATE
                   END-EVALUATE
      *
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 900-RECE-PUTB-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_rece_putb" TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    IDX             TO  SPA-GMN-BODYLIST-MAX (IDXX)
           END-PERFORM
      *
           MOVE    SPA-GMN-SELNUM1 TO  W121-HEADLIST-ROW
           MOVE    ZERO            TO  FLG-HEADLIST-ROW
      *
           MOVE    4               TO  SPA-GMN-CUR
      *
           .
       41011-SELNUM1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41010-HEADLIST-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-HEADLIST-MAX
               IF      W121-HEADLIST-SELECT (IDX)  =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM1
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-HEADLIST-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-GMN-BODY-KOUMOKU
                                       SPA-GMN-GRP-KOUMOKU
                                       SPA-GMN-BODYLIST
      *****                            SPA-GMN-BODYLIST-MAX
      *
           IF      WRK-SELNUM         >   ZERO
               MOVE    WRK-SELNUM         TO  SPA-GMN-SELNUM1
           END-IF
      *
           PERFORM 41011-SELNUM1-HENSYU-SEC
           .
       41010-HEADLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括分入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
               WHEN    ANY             ALSO    "BODYLIST"
               WHEN    ANY             ALSO    "BODYLIST_1"
                   PERFORM 41011-BODYLIST-SEL-SEC
      *
               WHEN    ANY             ALSO    "KOHLIST"
               WHEN    ANY             ALSO    "KOHLIST_1"
                   PERFORM 41012-KOHLIST-SEL-SEC
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
                   IF      WRK-PAGENO          =   1
                       MOVE    101                 TO  SPA-GMN-CUR
                   ELSE
                       MOVE    301                 TO  SPA-GMN-CUR
                   END-IF
               WHEN    OTHER
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41011-BODYLIST-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX FROM    1   BY  1
                   UNTIL       IDX >       SPA-GMN-BODYLIST-MAX
                                                   (WRK-PAGENO)
               IF      WRK-PAGENO      =   1
                   IF      W121-BODYLIST-SELECT (IDX)  =   "T"
                       IF      IDX     =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                           CONTINUE
                       ELSE
                           MOVE    IDX                 TO  WRK-SELNUM
                           MOVE    SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                                       TO  IDX
                       END-IF
                   END-IF
               ELSE
                   IF      W121-BODYLIST-1-SELECT (IDX) =   "T"
                       IF      IDX     =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                           CONTINUE
                       ELSE
                           MOVE    IDX                 TO  WRK-SELNUM
                           MOVE    SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                                       TO  IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               INITIALIZE              SPA-GMN-BODY-OCC    (WRK-PAGENO)
                                       SPA-NAI-BODY-OCC    (WRK-PAGENO)
                                       SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
               MOVE    WRK-SELNUM      TO  SPA-NAI-SELNUM2 (WRK-PAGENO)
           END-IF
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                       TO  WRK-SELNUM3
     *
           EVALUATE    TRUE
               WHEN    SPA-GMN-GRP-SETTEI (WRK-PAGENO) =   "1"
                   PERFORM 410111-GRP-HENSYU-SEC
                   IF      WRK-PAGENO      =   1
                       MOVE    50              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    250             TO  SPA-GMN-CUR
                   END-IF
               WHEN    SPA-GMN-GRP-KAIJYO(WRK-PAGENO)  =   "1"
                   PERFORM 410112-GRP-KAIJYO-SEC
                   IF      WRK-PAGENO      =   1
                       MOVE    100             TO  SPA-GMN-CUR
                   ELSE
                       MOVE    300             TO  SPA-GMN-CUR
                   END-IF
               WHEN    OTHER
                   PERFORM 41021-SELNUM2-HENSYU-SEC
                   IF      WRK-PAGENO      =   1
                       MOVE    13             TO  SPA-GMN-CUR
                   ELSE
                       MOVE    213            TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       41011-BODYLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ設定時編集処理
      *****************************************************************
       410111-GRP-HENSYU-SEC       SECTION.
      *
      *    グループ１の設定
           IF      SPA-GMN-GRP1 (WRK-PAGENO)   =   "T"
               IF      SPA-NAI-GRP1-NO(WRK-PAGENO) =   ZERO
                   IF      WRK-PAGENO      =   1
                       MOVE    50              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    250             TO  SPA-GMN-CUR
                   END-IF
                   MOVE    "0011"          TO  SPA-ERRCD
                   MOVE    ZERO            TO  FLG-BODYLIST-ROW
                                                       (WRK-PAGENO)
                   MOVE    ZERO            TO  SPA-NAI-SELNUM2
                                                       (WRK-PAGENO)
                                               SPA-NAI-SELNUM2-ROW
                                                       (WRK-PAGENO)
                   GO  TO  410111-GRP-HENSYU-EXT
               END-IF 
      *
      *        元情報と設定場所を取得
               MOVE    ZERO            TO  WRK-GRP-INDEX
               MOVE    ZERO            TO  WRK-INDEX
               IF      SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                                       <   SPA-NAI-GRP1-NO (WRK-PAGENO)
      *            既にグループ１番号を設定済のとき設定場所を取得
                   IF      SPA-NAI-BODYLIST-TGRP1-NO
                                               (WRK-PAGENO WRK-SELNUM2)
                                           NOT =   "000"
                       COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       +   1
                       IF    ( SPA-NAI-BODYLIST-TGRP1-NO(WRK-PAGENO IDX)
                                           =   "000"                )
                       OR    ( SPA-NAI-BODYLIST-TGRP1-NO(WRK-PAGENO IDX)
                                       NOT =
                               SPA-NAI-BODYLIST-TGRP1-NO
                                           (WRK-PAGENO WRK-SELNUM2) ) 
                           CONTINUE
                       ELSE
                           PERFORM VARYING IDX FROM   WRK-SELNUM2  BY  1
                                   UNTIL   IDX >
                                       SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                               IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                                     (WRK-PAGENO IDX)
                                                   NOT =
                                         SPA-NAI-BODYLIST-TGRP1-NO
                                            (WRK-PAGENO WRK-SELNUM2)  ) 
                               AND   ( SPA-NAI-SELNUM2(WRK-PAGENO)
                                                  NOT =   IDX         )
                                   COMPUTE WRK-GRP-INDEX
                                                      =    IDX -   1
                                   MOVE    SPA-GMN-BODYLIST-MAX 
                                                           (WRK-PAGENO)
                                                           TO  IDX
                               END-IF
                           END-PERFORM
                       END-IF    
                   END-IF
               ELSE
                   PERFORM VARYING IDX FROM
                               SPA-GMN-BODYLIST-MAX (WRK-PAGENO)  BY  -1
                           UNTIL   IDX <       1
                       IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX)
                                       =   SPA-NAI-GRP1-NO(WRK-PAGENO) )
                       AND   ( IDX NOT =   SPA-NAI-SELNUM2(WRK-PAGENO) )
                           MOVE    IDX             TO  WRK-GRP-INDEX
                           IF    ( SPA-NAI-BODYLIST-TPRTJYUN1
                                          (WRK-PAGENO IDX)
                                                   =   SPACE        )
                           AND   ( SPA-NAI-BODYLIST-TPRTJYUN2
                                          (WRK-PAGENO IDX)
                                                   =   SPACE        )
                           AND   ( SPA-GMN-KOHLIST-MAX (WRK-PAGENO IDX)
                                                   =   ZERO         )
                               CONTINUE
                           ELSE
                               MOVE    IDX             TO  WRK-INDEX
                           END-IF
                           MOVE    1               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        既に設定済の同グループの設定との設定のチェック
               IF    ( SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                                   <   SPA-NAI-GRP1-NO (WRK-PAGENO) )
               OR    ((SPA-NAI-BODYLIST-TPRTJYUN1
                                               (WRK-PAGENO WRK-SELNUM2)
                                           =   SPACE    )
                  AND (SPA-NAI-BODYLIST-TPRTJYUN2
                                               (WRK-PAGENO WRK-SELNUM2)
                                           =   SPACE    )
                  AND (SPA-GMN-KOHLIST-MAX     (WRK-PAGENO WRK-SELNUM2)
                                           =   ZERO     )           )
               OR    ( WRK-INDEX           =   ZERO                 )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-BODYLIST-TPRTJYUN1
                                           (WRK-PAGENO WRK-SELNUM2)
                                           NOT =
                               SPA-NAI-BODYLIST-TPRTJYUN1
                                               (WRK-PAGENO WRK-INDEX) )
                   OR    ( SPA-NAI-BODYLIST-TPRTJYUN2
                                           (WRK-PAGENO WRK-SELNUM2)
                                           NOT =
                               SPA-NAI-BODYLIST-TPRTJYUN2
                                               (WRK-PAGENO WRK-INDEX) )
                   OR    ( SPA-GMN-KOHLIST (WRK-PAGENO WRK-SELNUM2)
                                           NOT =
                               SPA-GMN-KOHLIST (WRK-PAGENO WRK-INDEX) )
                       MOVE    "0002"      TO  SPA-ERRCD
                       GO  TO  410111-GRP-HENSYU-EXT
                   END-IF
               END-IF
      *
      *        グループ１番号と設定内容の設定
               MOVE    SPA-NAI-GRP1-NO (WRK-PAGENO)
                                       TO  SPA-NAI-BODYLIST-TGRP1-NO
                                               (WRK-PAGENO WRK-SELNUM2)
      *
               IF      WRK-INDEX       >   ZERO
                   MOVE    SPA-NAI-BODYLIST-TPRTJYUN1
                                              (WRK-PAGENO WRK-INDEX)
                                           TO
                               SPA-NAI-BODYLIST-TPRTJYUN1
                               (WRK-PAGENO WRK-SELNUM2)
                   MOVE    SPA-NAI-BODYLIST-TPRTJYUN2
                                              (WRK-PAGENO WRK-INDEX)
                                           TO
                               SPA-NAI-BODYLIST-TPRTJYUN2
                                               (WRK-PAGENO WRK-SELNUM2)
                   MOVE    SPA-GMN-KOHLIST  (WRK-PAGENO WRK-INDEX)
                                           TO
                               SPA-GMN-KOHLIST (WRK-PAGENO WRK-SELNUM2)
               ELSE
                   IF    ( SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                                       <   SPA-NAI-GRP1-NO (WRK-PAGENO))
                   OR    ((SPA-NAI-BODYLIST-TPRTJYUN1
                                           (WRK-PAGENO WRK-SELNUM2)
                                       =   SPACE           )
                      AND (SPA-NAI-BODYLIST-TPRTJYUN2
                                            (WRK-PAGENO WRK-SELNUM2)
                                       =   SPACE    )
                      AND (SPA-GMN-KOHLIST-MAX
                                            (WRK-PAGENO WRK-SELNUM2)
                                       =   ZERO     )      )
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                       SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                           IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                                       (WRK-PAGENO IDX)
                                                   =   SPA-NAI-GRP1-NO
                                                       (WRK-PAGENO) )
                           AND   ( IDX         NOT =   SPA-NAI-SELNUM2
                                                       (WRK-PAGENO) )
                               MOVE    SPA-NAI-BODYLIST-TPRTJYUN1
                                               (WRK-PAGENO WRK-SELNUM2)
                                       TO  SPA-NAI-BODYLIST-TPRTJYUN1
                                                       (WRK-PAGENO IDX)
                               MOVE    SPA-NAI-BODYLIST-TPRTJYUN2
                                               (WRK-PAGENO WRK-SELNUM2)
                                       TO  SPA-NAI-BODYLIST-TPRTJYUN2
                                                       (WRK-PAGENO IDX)
                               MOVE    SPA-GMN-KOHLIST
                                               (WRK-PAGENO WRK-SELNUM2)
                                       TO  SPA-GMN-KOHLIST
                                                      (WRK-PAGENO IDX)
                           END-IF
                       END-PERFORM    
                   END-IF
               END-IF
      *
      *        既に設定済の同グループの最後に置き換える
               IF      WRK-GRP-INDEX       >   ZERO    
                   MOVE    SPA-GMN-BODYLIST-OCC (WRK-PAGENO)
                                           TO  WRK-GMN-BODYLIST-AREA
                   INITIALIZE                  SPA-GMN-BODYLIST-OCC
                                                       (WRK-PAGENO)
                   MOVE    ZERO            TO  IDY
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-BODYLIST-MAX
                                                       (WRK-PAGENO)
                       IF      IDX     =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDY
                           MOVE    WRK-GMN-BODYLIST-TBL (IDX)
                                                   TO
                                   SPA-GMN-BODYLIST-TBL(WRK-PAGENO IDY)
                           IF      IDX             =  WRK-GRP-INDEX
                               ADD     1               TO  IDY
                               MOVE    WRK-GMN-BODYLIST-TBL
                                                       (WRK-SELNUM2)
                                                       TO
                                   SPA-GMN-BODYLIST-TBL(WRK-PAGENO IDY)
                               IF      WRK-PAGENO      =   1
                                   MOVE    IDY TO  W121-BODYLIST-ROW
                               ELSE
                                   MOVE    IDY TO  W121-BODYLIST-1-ROW
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   INITIALIZE                  WRK-GMN-BODYLIST-AREA
               ELSE
                   IF      WRK-PAGENO      =   1
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)    TO
                                                   W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)    TO
                                                   W121-BODYLIST-1-ROW
                   END-IF
               END-IF
               MOVE    ZERO            TO  SPA-NAI-SELNUM2 (WRK-PAGENO)
               IF      WRK-PAGENO      =   1
                   MOVE    W121-BODYLIST-ROW
                                       TO  SPA-NAI-SELNUM2-ROW
                                                           (WRK-PAGENO)
               ELSE
                   MOVE    W121-BODYLIST-1-ROW
                                       TO  SPA-NAI-SELNUM2-ROW
                                                           (WRK-PAGENO)
               END-IF
      *
               MOVE    ZERO            TO  SPA-GMN-GRP1-NO-MAX
                                                           (WRK-PAGENO)
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-BODYLIST-MAX
                                                           (WRK-PAGENO)
                   IF      SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                               <   SPA-NAI-BODYLIST-TGRP1-NO-9
                                                   (WRK-PAGENO IDX)
                       MOVE    SPA-NAI-BODYLIST-TGRP1-NO-9
                                                       (WRK-PAGENO IDX)
                                   TO  SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                   END-IF
               END-PERFORM
           END-IF
      * 
      *    グループ２の設定
           IF      SPA-GMN-GRP2 (WRK-PAGENO)       =   "T"
               IF      SPA-GMN-GRP2-NO (WRK-PAGENO)     =   SPACE
                   IF      WRK-PAGENO      =   1
                       MOVE    51              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    251             TO  SPA-GMN-CUR
                   END-IF
                   MOVE    "0012"      TO  SPA-ERRCD
                   MOVE    ZERO        TO  FLG-BODYLIST-ROW (WRK-PAGENO)
                   MOVE    ZERO        TO  SPA-NAI-SELNUM2  (WRK-PAGENO)
                                           SPA-NAI-SELNUM2-ROW
                                                            (WRK-PAGENO)
                   GO  TO  410111-GRP-HENSYU-EXT
               END-IF 
               MOVE    SPA-GMN-GRP2-NO (WRK-PAGENO) TO
                               SPA-NAI-BODYLIST-TGRP2-NO
                                           (WRK-PAGENO WRK-SELNUM2)
               IF      WRK-PAGENO      =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) TO
                                                   W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) TO
                                                   W121-BODYLIST-1-ROW
               END-IF
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) TO
                                       SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
           END-IF
      *
           MOVE    ZERO            TO  FLG-BODYLIST-ROW (WRK-PAGENO)
      **** MOVE    SPA-NAI-SELNUM2 TO  W121-BODYLIST-ROW
      *
           .
       410111-GRP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ解除時編集処理
      *****************************************************************
       410112-GRP-KAIJYO-SEC       SECTION.
      *
      *    グループ１の解除
           IF      SPA-GMN-GRP1 (WRK-PAGENO)       =   "T"
               IF      SPA-NAI-BODYLIST-TGRP1-NO
                                           (WRK-PAGENO WRK-SELNUM2)
                                       =   "000"
                   GO  TO  410112-GRP-KAIJYO-EXT
               END-IF
      *
               MOVE    SPA-NAI-BODYLIST-TGRP1-NO
                                          (WRK-PAGENO WRK-SELNUM2)
                                           TO  WRK-GRP1-NO
      *
               MOVE    "000"               TO
                           SPA-NAI-BODYLIST-TGRP1-NO
                                               (WRK-PAGENO WRK-SELNUM2)
               INITIALIZE      SPA-GMN-KOHLIST (WRK-PAGENO WRK-SELNUM2)
                               SPA-GMN-KOHLIST-MAX
                                               (WRK-PAGENO WRK-SELNUM2)
                               SPA-NAI-BODYLIST-TPRTJYUN1
                                               (WRK-PAGENO WRK-SELNUM2)
                               SPA-NAI-BODYLIST-TPRTJYUN2
                                               (WRK-PAGENO WRK-SELNUM2)
      *
               IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   =   SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                   IF      WRK-PAGENO     =   1
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                              TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                              TO  W121-BODYLIST-1-ROW
                   END-IF
               ELSE
                   COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                               +   1
                   IF    ( SPA-NAI-BODYLIST-TGRP1-NO (WRK-PAGENO IDX)
                                       =   "000"       )
                   OR    ( SPA-NAI-BODYLIST-TGRP1-NO (WRK-PAGENO IDX)
                                   NOT =   WRK-GRP1-NO ) 
                       IF      WRK-PAGENO     =   1
                           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                              TO  W121-BODYLIST-ROW
                       ELSE
                           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                              TO  W121-BODYLIST-1-ROW
                       END-IF
                   ELSE
                       MOVE    ZERO            TO  WRK-GRP-INDEX
                       PERFORM VARYING IDX FROM
                                       SPA-NAI-SELNUM2 (WRK-PAGENO)
                                                   BY  1
                               UNTIL   IDX > 
                                       SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                           IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX)
                                                   NOT =   WRK-GRP1-NO ) 
                           AND   ( SPA-NAI-SELNUM2 (WRK-PAGENO)
                                                   NOT =   IDX         )
                               MOVE    IDX         TO  WRK-GRP-INDEX
                               MOVE    SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                                   TO  IDX
                           END-IF
                       END-PERFORM
                       IF      WRK-GRP-INDEX       =   ZERO
                           COMPUTE WRK-GRP-INDEX
                                   =   SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                   +   1
                       ELSE
                           COMPUTE WRK-GRP-INDEX
                                   =   WRK-GRP-INDEX           -   1
                       END-IF
                       PERFORM 4901-SYUBETU-HENSYU-SEC
                   END-IF    
               END-IF
               IF      WRK-PAGENO     =   1
                   MOVE    W121-BODYLIST-ROW
                                   TO  SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
               ELSE
                   MOVE    W121-BODYLIST-1-ROW
                                   TO  SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
               END-IF
      *
               MOVE    ZERO        TO  SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX > 
                                       SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                   IF      SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                               <   SPA-NAI-BODYLIST-TGRP1-NO-9
                                                       (WRK-PAGENO IDX)
                       MOVE    SPA-NAI-BODYLIST-TGRP1-NO-9
                                                       (WRK-PAGENO IDX)
                                   TO  SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                   END-IF
               END-PERFORM
           END-IF
      *    グループ２の解除
           IF      SPA-GMN-GRP2 (WRK-PAGENO)        =   "T"
               MOVE    SPACE               TO
                           SPA-NAI-BODYLIST-TGRP2-NO
                                               (WRK-PAGENO WRK-SELNUM2)
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
           END-IF
      *
           MOVE    ZERO            TO  FLG-BODYLIST-ROW (WRK-PAGENO)
      **** MOVE    SPA-NAI-SELNUM2 TO  W121-BODYLIST-ROW
      *
           .
       410112-GRP-KAIJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41021-SELNUM2-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-GMN-BODYLIST-TSYUBETU
                           (WRK-PAGENO WRK-SELNUM2)
                                   TO  SPA-GMN-RECESYUBETU (WRK-PAGENO)
           MOVE    SPA-GMN-BODYLIST-TSYUBETUNAME
                           (WRK-PAGENO WRK-SELNUM2)
                                   TO  SPA-GMN-SYUBETUNAME (WRK-PAGENO)
           MOVE    SPA-NAI-BODYLIST-TPRTJYUN1
                           (WRK-PAGENO WRK-SELNUM2)
                                   TO  SPA-GMN-SYUBETU-PRTJYUN1
                                                           (WRK-PAGENO)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-SYUBETU-PRTJYUN1-MAX
               IF      SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                               =   SPA-GMN-SYUBETU-PRTJYUN1L (IDY)
                   MOVE    SPA-GMN-SYUBETU-PRTJYUN1LST (IDY)
                                       TO  SPA-GMN-SYUBETU-PRTJYUN1-G
                                                           (WRK-PAGENO)
                   MOVE    SPA-GMN-SYUBETU-PRTJYUN1-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
           MOVE    SPA-NAI-BODYLIST-TPRTJYUN2
                               (WRK-PAGENO WRK-SELNUM2)
                                   TO  SPA-GMN-SYUBETU-PRTJYUN2
                                                       (WRK-PAGENO)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   SPA-GMN-SYUBETU-PRTJYUN2-MAX
               IF      SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                               =   SPA-GMN-SYUBETU-PRTJYUN2L (IDY)
                   MOVE    SPA-GMN-SYUBETU-PRTJYUN2LST (IDY)
                                       TO  SPA-GMN-SYUBETU-PRTJYUN2-G
                                                            (WRK-PAGENO)
                   MOVE    SPA-GMN-SYUBETU-PRTJYUN2-MAX
                                       TO  IDY
               END-IF                                     
           END-PERFORM
      *
           IF      WRK-PAGENO      =   1
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) 
                                       TO  W121-BODYLIST-ROW
           ELSE
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) 
                                       TO  W121-BODYLIST-1-ROW
           END-IF 
           MOVE    ZERO            TO  FLG-BODYLIST-ROW  (WRK-PAGENO)
      *
           IF      WRK-PAGENO      =   1
               MOVE    13              TO  SPA-GMN-CUR
           ELSE
               MOVE    213             TO  SPA-GMN-CUR
           END-IF
           .
       41021-SELNUM2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-KOHLIST-SEL-SEC            SECTION.
      *
           MOVE    SPACE                TO  WRK-HKNNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-KOHLIST-MAX
                                               (WRK-PAGENO WRK-SELNUM2)
               IF      WRK-PAGENO          =   1
                   IF      W121-KOHLIST-SELECT (IDX)   =   "T"
                       MOVE    SPA-GMN-KOHLIST-THKNNUM
                           (WRK-PAGENO WRK-SELNUM2 IDX)
                                                   TO  WRK-HKNNUM
                       MOVE    SPA-GMN-KOHLIST-MAX
                               (WRK-PAGENO WRK-SELNUM2)
                                                   TO  IDX
                   END-IF
               ELSE
                   IF      W121-KOHLIST-1-SELECT (IDX)  =   "T"
                       MOVE    SPA-GMN-KOHLIST-THKNNUM
                           (WRK-PAGENO WRK-SELNUM2 IDX)
                                                   TO  WRK-HKNNUM
                       MOVE    SPA-GMN-KOHLIST-MAX
                               (WRK-PAGENO WRK-SELNUM2)
                                                   TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-PAGENO      =   1
               MOVE    WRK-HKNNUM      TO  W121-HKNNUM
           ELSE
               MOVE    WRK-HKNNUM      TO  W121-HKNNUM-1
           END-IF
           .
       41012-KOHLIST-SEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    関連チェック処理
           PERFORM 4103-KANREN-CHK-SEC
      *    基本チェック処理
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4102-KIHON-CHK-SEC
           END-IF
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
               END-IF 
               MOVE    ZERO            TO  FLG-BODYLIST-ROW (WRK-PAGENO)
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W121-PR-PRTJYUN     TO  SPA-GMN-PREFKBN-PRTJYUN-G
           MOVE    W121-HKN-PRTJYUN    TO  SPA-GMN-HKNJANUM-PRTJYUN-G
           MOVE    W121-HKNJANUM       TO  SPA-GMN-HKNJANUM
           MOVE    W121-HKNJ-PRTKBN    TO  SPA-GMN-HKNJANUM-PRTKBN-G
      *
      *    県内
           MOVE    W121-PRTJYUN1       TO  SPA-GMN-PRTJYUN1-G        (1)
           MOVE    W121-PRTJYUN2       TO  SPA-GMN-PRTJYUN2-G        (1)
           MOVE    W121-SRT-LAST       TO  SPA-GMN-SRT-LAST-G        (1)
           MOVE    W121-ZAITAKU        TO  SPA-GMN-ZAITAKU-G         (1)
           MOVE    W121-CHOUKI         TO  SPA-GMN-CHOUKI-G          (1)
           MOVE    W121-KOUGAKU-TEN    TO  SPA-GMN-KOUGAKU-TEN       (1)
      *
           MOVE    W121-SYU-PRTJYUN1   TO  SPA-GMN-SYUBETU-PRTJYUN1-G(1)
           MOVE    W121-SYU-PRTJYUN2   TO  SPA-GMN-SYUBETU-PRTJYUN2-G(1)
           MOVE    W121-HKNNUM         TO  SPA-GMN-HKNNUM            (1)
      *
      *    県外
           MOVE    W121-PRTJYUN1-1     TO  SPA-GMN-PRTJYUN1-G        (2)
           MOVE    W121-PRTJYUN2-1     TO  SPA-GMN-PRTJYUN2-G        (2)
           MOVE    W121-SRT-LAST-1     TO  SPA-GMN-SRT-LAST-G        (2)
           MOVE    W121-ZAITAKU-1      TO  SPA-GMN-ZAITAKU-G         (2)
           MOVE    W121-CHOUKI-1       TO  SPA-GMN-CHOUKI-G          (2)
           MOVE    W121-KOUGAKU-TEN-1  TO  SPA-GMN-KOUGAKU-TEN       (2)
      *
           MOVE    W121-SYU-PRTJYUN1-1 TO  SPA-GMN-SYUBETU-PRTJYUN1-G(2)
           MOVE    W121-SYU-PRTJYUN2-1 TO  SPA-GMN-SYUBETU-PRTJYUN2-G(2)
           MOVE    W121-HKNNUM-1       TO  SPA-GMN-HKNNUM            (2)
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41021-PREFKBN-PRTJYUN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-HKNJANUM-PRTJYUN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-HKNJANUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-HKNJANUM-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-PRTJYUN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-PRTJYUN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-SRT-LAST-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-ZAITAKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-CHOUKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-KOUGAKU-TEN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-SYUBETU-PRTJYUN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-SYUBETU-PRTJYUN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-HKNNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4103-KANREN-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                    =   ZERO
               IF    ( SPA-GMN-GRP1-NO  (WRK-PAGENO)
                                    NOT =   SPACE )
               OR    ( SPA-GMN-GRP2-NO  (WRK-PAGENO)
                                    NOT =   SPACE )
                   MOVE    "0102"           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD        =   SPACE
               IF    ( SPA-NAI-SELNUM2  (WRK-PAGENO)
                                        =   ZERO )
                   IF    ( SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                                        NOT =   SPACE )
                   OR    ( SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                                        NOT =   SPACE )
                   OR    ( SPA-GMN-HKNNUM           (WRK-PAGENO)
                                        NOT =   SPACE )
                       MOVE    "0103"           TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効開始年月日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC          SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-NAI-STYUKYMD
           MOVE    W121-STYUKYMD       TO  SPA-GMN-STYUKYMD 
      *
           IF    ( SPA-GMN-STYUKYMD    =   SPACE )
           AND   ( SPA-GMN-HEADLIST-MAX
                                       =   ZERO  )
               MOVE    SPA-SYS-2003-STYUKYMD (1)
                                          TO  SPA-GMN-STYUKYMD
               MOVE    SPA-SYS-2003-EDYUKYMD (1)
                                          TO  SPA-GMN-EDYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
               IF      SPA-NAI-STYUKYMD    NOT =   ZERO
                   IF      SPA-NAI-STYUKYMD (7:2)  =   "01"
                       CONTINUE
                   ELSE
                       MOVE    "0006"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF   
           .
       41021-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了年月日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    W121-EDYUKYMD       TO  SPA-GMN-EDYUKYMD 
           IF      SPA-GMN-EDYUKYMD    =   SPACE
               MOVE    "99999999"          TO  SPA-GMN-EDYUKYMD
               MOVE    99999999            TO  SPA-NAI-EDYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
      *        末日チェック
               IF      SPA-NAI-EDYUKYMD
                                       NOT =   99999999
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY7-AREA
                   MOVE    "71"                TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-EDYUKYMD (1:6)
                                               TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                   IF      LNK-DAY7-KOYOMI NOT =   SPA-NAI-EDYUKYMD
                       MOVE    "0007"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    開始日との関連チェック
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-STYUKYMD    NOT =   SPACE )  AND
                     ( SPA-GMN-EDYUKYMD    NOT =   SPACE )
                   IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    新規入力のとき改正前後を跨ぐときの開始日の設定
           IF      SPA-ERRCD       =   SPACE
               IF      SPA-GMN-SELNUM1     =   ZERO
                   IF    ( SPA-NAI-STYUKYMD <  WRK-CONS-SRYYM-20080401 )
                   AND   ( SPA-NAI-EDYUKYMD >= WRK-CONS-SRYYM-20080401 )
                       MOVE    WRK-CONS-SRYYM-20080401
                                           TO  SPA-NAI-STYUKYMD
                                               WRK-YMD
                       PERFORM 5002-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG TO  SPA-GMN-STYUKYMD
                   END-IF
               END-IF
           END-IF 
      *
      *    有効期間の重複チェック
           IF      SPA-ERRCD       =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-HEADLIST-MAX
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF      SPA-GMN-SELNUM1     =   IDX
      *                選択された内容の開始日が変更された場合
      ***                 IF      SPA-NAI-STYUKYMD    =
      ***                             SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                           MOVE    ZERO            TO  FLG-CHK
      ***                 ELSE
      ***                     MOVE    1               TO  FLG-CHK
      ***                 END-IF
                   ELSE
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   IF      FLG-CHK             =   1
                       IF    ( SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
                                               <   SPA-NAI-STYUKYMD )
                       OR    ( SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                                               >   SPA-NAI-EDYUKYMD )
                           CONTINUE
                       ELSE
                           MOVE    "0004"              TO  SPA-ERRCD
                           MOVE    2                   TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               IF      SPA-GMN-SELNUM1     =   ZERO
      *
               INITIALIZE                  SPA-NAI-BODY-KOUMOKU
                                           SPA-GMN-BODY-KOUMOKU
                                           SPA-GMN-GRP-KOUMOKU
                                           SPA-GMN-BODYLIST
                                           SPA-GMN-HEAD1-OCC (1)
                                           SPA-GMN-HEAD1-OCC (2)
      *            レセプト（編綴順）情報編集処理
                   PERFORM 3102-SPA-2008-HENSYU-SEC
      *            レセプト種別情報編集処理
                   PERFORM 410211-RECESYUBETU-HENSYU-SEC
      *
                   MOVE    SPA-GMN-PRTJYUN1LST (1)
                                       TO  SPA-GMN-PRTJYUN1-G (1)
                                           SPA-GMN-PRTJYUN1-G (2)
      *
                   MOVE    SPA-GMN-SRT-LASTLST (1)
                                       TO  SPA-GMN-SRT-LAST-G (1)
                                           SPA-GMN-SRT-LAST-G (2)
               END-IF 
      *
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別情報編集処理
      *****************************************************************
       410211-RECESYUBETU-HENSYU-SEC     SECTION.
      *
      *    レセプト種別取得処理
           PERFORM 3103-2003-HENSYU-SEC
      *
      *    レセプト種別印刷区分による初期表示
           MOVE    ZERO                TO  IDY
           IF      SYS-2008-RECESYUBETU-PRTJYUN
                                       =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       50
                       OR      WRK-2003-TSYUBETU    (IDX)
                                   =       SPACE
                       OR      IDY >=      WRK-CONS-BODYLIST-MAX
                   ADD     1                           TO  IDY   
                   MOVE    WRK-2003-TSYUBETU    (IDX)  TO
                                   SPA-GMN-BODYLIST-TSYUBETU     (1 IDY)
                                   SPA-GMN-BODYLIST-TSYUBETU     (2 IDY)
                   MOVE    WRK-2003-TSYUBETUNAME(IDX)  TO
                                   SPA-GMN-BODYLIST-TSYUBETUNAME (1 IDY)
                                   SPA-GMN-BODYLIST-TSYUBETUNAME (2 IDY)
      *
                   MOVE    IDY     TO  SPA-GMN-BODYLIST-MAX (1)
                                       SPA-GMN-BODYLIST-MAX (2)
               END-PERFORM
           ELSE     
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       3
                       OR      IDY >=      WRK-CONS-BODYLIST-MAX
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       50
                           OR      WRK-2003-TSYUBETU    (IDX)
                                       =       SPACE
                           OR      IDY >=      WRK-CONS-BODYLIST-MAX
                     EVALUATE  IDZ ALSO    SYS-2008-RECESYUBETU-PRTJYUN
                                   ALSO    WRK-2003-TSYUBETU (IDX)(2:1)
                       WHEN    SPACE   ALSO    ANY     ALSO    ANY
                       WHEN    1   ALSO        "1"     ALSO    "1"
                       WHEN    1   ALSO        "2"     ALSO    "1"
                       WHEN    1   ALSO        "3"     ALSO    "3"
                       WHEN    1   ALSO        "4"     ALSO    "3"
                       WHEN    1   ALSO        "5"     ALSO    "4"
                       WHEN    1   ALSO        "6"     ALSO    "4"
                       WHEN    2   ALSO        "3"     ALSO    "1"
                       WHEN    2   ALSO        "5"     ALSO    "1"
                       WHEN    2   ALSO        "1"     ALSO    "3"
                       WHEN    2   ALSO        "6"     ALSO    "3"
                       WHEN    2   ALSO        "2"     ALSO    "4"
                       WHEN    2   ALSO        "4"     ALSO    "4"
                       WHEN    3   ALSO        "4"     ALSO    "1"
                       WHEN    3   ALSO        "6"     ALSO    "1"
                       WHEN    3   ALSO        "2"     ALSO    "3"
                       WHEN    3   ALSO        "5"     ALSO    "3"
                       WHEN    3   ALSO        "1"     ALSO    "4"
                       WHEN    3   ALSO        "3"     ALSO    "4"
                           ADD     1                           TO  IDY   
                           MOVE    WRK-2003-TSYUBETU    (IDX)  TO
                                   SPA-GMN-BODYLIST-TSYUBETU     (1 IDY)
                                   SPA-GMN-BODYLIST-TSYUBETU     (2 IDY)
                           MOVE    WRK-2003-TSYUBETUNAME(IDX)  TO
                                   SPA-GMN-BODYLIST-TSYUBETUNAME (1 IDY)
                                   SPA-GMN-BODYLIST-TSYUBETUNAME (2 IDY)
      *
                           MOVE    IDY     TO  SPA-GMN-BODYLIST-MAX (1)
                                               SPA-GMN-BODYLIST-MAX (2)
                     END-EVALUATE
                   END-PERFORM
               END-PERFORM
           END-IF        
      *
           . 
       410211-RECESYUBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・県内県外チェック
      *****************************************************************
       41021-PREFKBN-PRTJYUN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-PREFKBN-PRTJYUN   =   SPACE
               MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (1)
                                           TO  SPA-GMN-PREFKBN-PRTJYUN-G
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PREFKBN-PRTJYUN-MAX
                   IF      SPA-GMN-PREFKBN-PRTJYUN
                                   =   SPA-GMN-PREFKBN-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                           TO  SPA-GMN-PREFKBN-PRTJYUN-G
                       MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PREFKBN-PRTJYUNMEI  =   SPACE
                   MOVE    "0021"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-PREFKBN-PRTJYUN   =   "3"
                   MOVE    999             TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41021-PREFKBN-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・保険者チェック
      *****************************************************************
       41021-HKNJANUM-PRTJYUN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-HKNJANUM-PRTJYUN
                                           =   SPACE
               IF      SPA-GMN-PRTJYUN1 (1)    =   "02"
                   CONTINUE
               ELSE 
                   MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (1)
                                   TO  SPA-GMN-HKNJANUM-PRTJYUN-G
               END-IF
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-HKNJANUM-PRTJYUN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTJYUN
                                   =   SPA-GMN-HKNJANUM-PRTJYUNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                           TO
                                           SPA-GMN-HKNJANUM-PRTJYUN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-HKNJANUM-PRTJYUNMEI =   SPACE
                   MOVE    "0022"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
           .
       41021-HKNJANUM-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関の所在地保険者チェック
      *****************************************************************
       41021-HKNJANUM-CHK-SEC   SECTION.
      *
           MOVE    SPACE                TO SPA-GMN-HKNJANAME
      *
           IF      SPA-GMN-HKNJANUM        =   SPACE
               MOVE    SPACE               TO  SPA-GMN-HKNJANUM-PRTKBN-G
           ELSE
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               IF      FLG-HKNJAINF        =   ZERO
                   MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME
                   IF      HKNJA-HKNNUM    NOT =   "060"      
                       MOVE    75                      TO  SPA-GMN-CUR
                       MOVE    "0026"                  TO  SPA-ERRCD
                   ELSE
                       IF      HKNJA-HKNJANUM (1:2)
                                               NOT =   SPA-1001-PREFNUM
                           MOVE    75                  TO  SPA-GMN-CUR
                           MOVE    "0029"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    "0026"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
           .
       41021-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関の所在地保険者印刷位置チェック
      *****************************************************************
       41021-HKNJANUM-PRTKBN-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-HKNJANUM-PRTKBN
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-HKNJANUM-PRTKBN-MAX
                   IF      SPA-GMN-HKNJANUM-PRTKBN
                                   =
                                   SPA-GMN-HKNJANUM-PRTKBNL (IDY)
                       MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                           TO
                                       SPA-GMN-HKNJANUM-PRTKBN-G
                       MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-HKNJANUM-PRTKBNMEI =   SPACE
                   MOVE    "0027"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7               TO  SPA-GMN-CUR
           END-IF
           .
       41021-HKNJANUM-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定１チェック
      *****************************************************************
       41021-PRTJYUN1-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-PRTJYUN1 (WRK-PAGENO)
                                       =   SPACE
               MOVE    SPA-GMN-PRTJYUN1LST (1)
                                   TO  SPA-GMN-PRTJYUN1-G (WRK-PAGENO)
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-PRTJYUN1-MAX
                   IF      SPA-GMN-PRTJYUN1(WRK-PAGENO)
                                   =   SPA-GMN-PRTJYUN1L (IDY)
                       MOVE    SPA-GMN-PRTJYUN1LST (IDY)
                                   TO  SPA-GMN-PRTJYUN1-G (WRK-PAGENO)
                       MOVE    SPA-GMN-PRTJYUN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN1MEI (WRK-PAGENO) 
                                               =   SPACE
                   MOVE    "0009"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *----> take add st
               IF      SPA-GMN-PRTJYUN1-G (2)(1:2)
                                               =   "02"
                   MOVE    "0048"                  TO  SPA-ERRCD
               END-IF
      *----> take add ed
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    8               TO  SPA-GMN-CUR
               ELSE
                   MOVE    208             TO  SPA-GMN-CUR
               END-IF
           ELSE
               IF    ( WRK-PAGENO      =   1    )
               AND   ( SPA-GMN-PRTJYUN1 (WRK-PAGENO)
                                       =   "02" )
                   MOVE    SPACE       TO  SPA-GMN-HKNJANUM
                                           SPA-GMN-HKNJANAME
                                           SPA-GMN-HKNJANUM-PRTKBN-G
                                           SPA-GMN-HKNJANUM-PRTJYUN-G
               END-IF    
           END-IF
           .
       41021-PRTJYUN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定２チェック
      *****************************************************************
       41021-PRTJYUN2-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-PRTJYUN2 (WRK-PAGENO)
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-PRTJYUN2-MAX
                   IF      SPA-GMN-PRTJYUN2 (WRK-PAGENO)
                                   =   SPA-GMN-PRTJYUN2L (IDY)
                       MOVE    SPA-GMN-PRTJYUN2LST (IDY)
                                   TO  SPA-GMN-PRTJYUN2-G (WRK-PAGENO)
                       MOVE    SPA-GMN-PRTJYUN2-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-PRTJYUN2MEI (WRK-PAGENO)
                                               =   SPACE
                   MOVE    "0010"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    9               TO  SPA-GMN-CUR
               ELSE
                   MOVE    209             TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41021-PRTJYUN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終印刷順チェック
      *****************************************************************
       41021-SRT-LAST-CHK-SEC   SECTION.
      *
           IF      SPA-GMN-SRT-LAST (WRK-PAGENO)
                                       =   SPACE
               MOVE    SPA-GMN-SRT-LASTLST (1)
                                   TO  SPA-GMN-SRT-LAST-G (WRK-PAGENO)
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SRT-LAST-MAX
                   IF      SPA-GMN-SRT-LAST (WRK-PAGENO)
                                       =   SPA-GMN-SRT-LASTL (IDY)
                       MOVE    SPA-GMN-SRT-LASTLST (IDY)
                                   TO  SPA-GMN-SRT-LAST-G (WRK-PAGENO)
                       MOVE    SPA-GMN-SRT-LAST-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SRT-LASTMEI (WRK-PAGENO)
                                           =   SPACE
                   MOVE    "0013"              TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    16              TO  SPA-GMN-CUR
               ELSE
                   MOVE    216             TO  SPA-GMN-CUR
               END-IF
           END-IF
            .
       41021-SRT-LAST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    高額点数チェック
      *****************************************************************
       41021-KOUGAKU-TEN-CHK-SEC       SECTION.
      *
          .
       41021-KOUGAKU-TEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診・在医総チェック
      *****************************************************************
       41021-ZAITAKU-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-ZAITAKU (WRK-PAGENO)
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-ZAITAKU-MAX
                   IF      SPA-GMN-ZAITAKU (WRK-PAGENO)
                                   =   SPA-GMN-ZAITAKUL (IDY)
                       MOVE    SPA-GMN-ZAITAKULST (IDY)
                                   TO  SPA-GMN-ZAITAKU-G (WRK-PAGENO)
                       MOVE    SPA-GMN-ZAITAKU-MAX
                                   TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-ZAITAKUMEI (WRK-PAGENO)
                                               =   SPACE
                   MOVE    "0024"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    10              TO  SPA-GMN-CUR
               ELSE
                   MOVE    210             TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41021-ZAITAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期（長期高額療養費）チェック
      *****************************************************************
       41021-CHOUKI-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-CHOUKI (WRK-PAGENO)
                                   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-CHOUKI-MAX
                   IF      SPA-GMN-CHOUKI (WRK-PAGENO)
                                   =   SPA-GMN-CHOUKIL (IDY)
                       MOVE    SPA-GMN-CHOUKILST (IDY)
                                           TO
                                           SPA-GMN-CHOUKI-G (WRK-PAGENO)
                       MOVE    SPA-GMN-CHOUKI-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-CHOUKIMEI (WRK-PAGENO)
                                           =   SPACE
                   MOVE    "0025"              TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    11              TO  SPA-GMN-CUR
               ELSE
                   MOVE    211             TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41021-CHOUKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・レセプト種別チェック
      *****************************************************************
       41021-SYUBETU-PRTJYUN1-CHK-SEC   SECTION.
      *
           IF      WRK-SELNUM2          =   ZERO
               GO  TO  41021-SYUBETU-PRTJYUN1-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                                       =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUBETU-PRTJYUN1-MAX
                   IF      SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                               =   SPA-GMN-SYUBETU-PRTJYUN1L (IDY)
                       MOVE    SPA-GMN-SYUBETU-PRTJYUN1LST (IDY)
                                           TO
                               SPA-GMN-SYUBETU-PRTJYUN1-G (WRK-PAGENO)
                       MOVE    SPA-GMN-SYUBETU-PRTJYUN1-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUBETU-PRTJYUN1MEI (WRK-PAGENO)
                                               =   SPACE
                   MOVE    "0030"                  TO  SPA-ERRCD
               ELSE
                   IF    ( SPA-GMN-PRTJYUN1 (WRK-PAGENO)
                                               NOT =   "02" )
                   AND   ( SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                                                   =   "07" ) 
                       MOVE    "0031"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    12              TO  SPA-GMN-CUR
               ELSE
                   MOVE    212             TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                                           TO
                                           SPA-NAI-BODYLIST-TPRTJYUN1
                                               (WRK-PAGENO WRK-SELNUM2)
               IF      SPA-NAI-BODYLIST-TGRP1-NO
                                   (WRK-PAGENO WRK-SELNUM2)
                               NOT =   "000"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-BODYLIST-MAX
                                                       (WRK-PAGENO)
                       IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                           (WRK-PAGENO WRK-SELNUM2)
                                       =   SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX)   )
                       AND   ( IDX NOT =   SPA-NAI-SELNUM2
                                                   (WRK-PAGENO)       )
                           MOVE    SPA-GMN-SYUBETU-PRTJYUN1 (WRK-PAGENO)
                                               TO
                                           SPA-NAI-BODYLIST-TPRTJYUN1
                                                       (WRK-PAGENO IDX)
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       41021-SYUBETU-PRTJYUN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順指定・レセプト種別チェック
      *****************************************************************
       41021-SYUBETU-PRTJYUN2-CHK-SEC   SECTION.
      *
           IF      WRK-SELNUM2          =   ZERO
               GO  TO  41021-SYUBETU-PRTJYUN2-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                                           =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >   SPA-GMN-SYUBETU-PRTJYUN2-MAX
                   IF      SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                                   =
                                   SPA-GMN-SYUBETU-PRTJYUN2L (IDY)
                       MOVE    SPA-GMN-SYUBETU-PRTJYUN2LST (IDY)
                                           TO
                               SPA-GMN-SYUBETU-PRTJYUN2-G (WRK-PAGENO)
                       MOVE    SPA-GMN-SYUBETU-PRTJYUN2-MAX
                                           TO  IDY
                   END-IF                                     
               END-PERFORM
      *     
               IF      SPA-GMN-SYUBETU-PRTJYUN2MEI (WRK-PAGENO)
                                               =   SPACE
                   MOVE    "0032"                  TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-PRTJYUN1         (WRK-PAGENO)
                                           NOT =   "02"  )
               AND   ( SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                                           NOT =   SPACE )
                   MOVE    SPACE           TO 
                               SPA-GMN-SYUBETU-PRTJYUN2-G (WRK-PAGENO)
                   MOVE    "0033"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    13              TO  SPA-GMN-CUR
               ELSE
                   MOVE    213             TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                                           TO
                                           SPA-NAI-BODYLIST-TPRTJYUN2
                                               (WRK-PAGENO WRK-SELNUM2)
      *
               IF      SPA-NAI-BODYLIST-TGRP1-NO
                               (WRK-PAGENO WRK-SELNUM2)
                                   NOT =   "000"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-BODYLIST-MAX
                                                           (WRK-PAGENO)
                       IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                       (WRK-PAGENO WRK-SELNUM2)
                                       =   SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX)   )
                       AND   ( IDX NOT =   SPA-NAI-SELNUM2
                                                   (WRK-PAGENO)       )
                           MOVE    SPA-GMN-SYUBETU-PRTJYUN2 (WRK-PAGENO)
                                               TO
                                           SPA-NAI-BODYLIST-TPRTJYUN2
                                                       (WRK-PAGENO IDX)
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       41021-SYUBETU-PRTJYUN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号チェック処理
      *****************************************************************
       41021-HKNNUM-CHK-SEC           SECTION.
      *
           IF      WRK-SELNUM2          =   ZERO
               GO  TO  41021-HKNNUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-GMN-SEIDONAME (WRK-PAGENO)
           MOVE    ZERO            TO  SPA-NAI-SELNUM3   (WRK-PAGENO)
      *
           IF      SPA-GMN-HKNNUM  (WRK-PAGENO)
                                   =   SPACE
               CONTINUE
           ELSE
               EVALUATE    TRUE
                   WHEN    SPA-GMN-HKNNUM (WRK-PAGENO) (1:1)
                                           >=  "1" AND <=  "8"
                       MOVE    SPACE           TO  HKNNUM-REC
                       INITIALIZE                  HKNNUM-REC
                       MOVE    SPA-HOSPNUM      TO  HKN-HOSPNUM
                       MOVE    SPA-GMN-HKNNUM (WRK-PAGENO)
                                               TO  HKN-HKNNUM
                       MOVE    "00"            TO  HKN-PAYKBN
                       MOVE    HKNNUM-REC      TO  MCPDATA-REC
                       PERFORM 900-HKNNUM-READ-SEC
      *
                       IF      FLG-HKNNUM      =   ZERO
                           MOVE    HKN-TANSEIDONAME
                                                  TO  SPA-GMN-SEIDONAME
                                                           (WRK-PAGENO)
                           IF      HKN-RECESKYKBN  =   "1" OR "3"
                               MOVE    SPACE           TO
                                          SPA-GMN-SEIDONAME (WRK-PAGENO)
                               MOVE    "0034"          TO  SPA-ERRCD
                           END-IF
                       ELSE
                           MOVE    "0035"          TO  SPA-ERRCD
                       END-IF
                   WHEN    SPA-GMN-HKNNUM (WRK-PAGENO) =   "XXX"
                       MOVE    "長期"          TO  SPA-GMN-SEIDONAME
                                                           (WRK-PAGENO)
                   WHEN    SPA-GMN-HKNNUM  (WRK-PAGENO)=   "YYY"
                       MOVE    "在総診・在医総"
                                               TO  SPA-GMN-SEIDONAME
                                                           (WRK-PAGENO)
                   WHEN    OTHER
                       MOVE    "0035"          TO  SPA-ERRCD
               END-EVALUATE
      * 
               IF      SPA-ERRCD           =   SPACE
                   IF      SPA-NAI-SELNUM2 (WRK-PAGENO)     =   ZERO
                       MOVE    "0103"              TO  SPA-ERRCD
                       IF      WRK-PAGENO      =   1
                           MOVE    4               TO  SPA-GMN-CUR
                       ELSE
                           MOVE    204             TO  SPA-GMN-CUR
                       END-IF
                       GO  TO   41021-HKNNUM-CHK-EXT
                   END-IF
      * 
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-KOHLIST-MAX
                                                (WRK-PAGENO WRK-SELNUM2)
                       IF      SPA-GMN-KOHLIST-THKNNUM
                           (WRK-PAGENO WRK-SELNUM2 IDX)
                                           =   SPA-GMN-HKNNUM
                                                       (WRK-PAGENO)
                           MOVE    IDX     TO  SPA-NAI-SELNUM3
                                                       (WRK-PAGENO)
                           MOVE    SPA-GMN-KOHLIST-MAX 
                               (WRK-PAGENO WRK-SELNUM2)
                                           TO  IDX
                       END-IF
                   END-PERFORM
      *
                   IF      SPA-NAI-SELNUM3 (WRK-PAGENO)
                                           >   ZERO
                       IF      WRK-PAGENO      =   1
                           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                               TO  W121-KOHLIST-ROW
                       ELSE
                           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                               TO  W121-KOHLIST-1-ROW
                       END-IF
                       MOVE    ZERO    TO  FLG-KOHLIST-ROW (WRK-PAGENO)
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                       TO  WRK-SELNUM3
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    15              TO  SPA-GMN-CUR
               ELSE
                   MOVE    215             TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41021-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ１チェック処理
      *****************************************************************
       41021-GRP1-NO-CHK-SEC           SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "GRP1_NO"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    50          TO  SPA-GMN-CUR
               WHEN    "GRP1_NO_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    250         TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
           ELSE
               IF      SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                       >   ZERO
                   IF      WRK-PAGENO      =   1
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                   END-IF
               END-IF 
           END-IF
           MOVE    2                   TO  FLG-BODYLIST-ROW (WRK-PAGENO)
           MOVE    ZERO                TO  SPA-NAI-SELNUM2  (WRK-PAGENO)
      *
           IF      WRK-PAGENO      =   1
               MOVE    W121-GRP1-NO    TO  SPA-GMN-GRP1-NO (WRK-PAGENO)
           ELSE
               MOVE    W121-GRP1-NO-1  TO  SPA-GMN-GRP1-NO (WRK-PAGENO)
           END-IF
           MOVE    ZERO            TO  SPA-NAI-GRP1-NO (WRK-PAGENO)
      * 
           IF    ( SPA-GMN-GRP1-NO (WRK-PAGENO)
                                   NOT =   SPACE )
           AND   ( SPA-GMN-GRP1    (WRK-PAGENO)
                                       =   "F"   )    
               MOVE    "0036"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    SPA-GMN-GRP1-NO (WRK-PAGENO)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC         NOT =   ZERO  )
               OR    ( SNUM-MINKBN     NOT =   SPACE )
               OR    ( SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0037"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-GRP1-NO
                                                           (WRK-PAGENO)
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    100             TO  SPA-GMN-CUR
               ELSE
                   MOVE    300             TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41021-GRP1-NO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ２チェック処理
      *****************************************************************
       41021-GRP2-NO-CHK-SEC           SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "GRP2_NO"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    51          TO  SPA-GMN-CUR
               WHEN    "GRP2_NO_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    251         TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
           ELSE
               IF      SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                       >   ZERO
                   IF      WRK-PAGENO      =   1
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                   END-IF
               END-IF 
           END-IF
           MOVE    2                   TO  FLG-BODYLIST-ROW (WRK-PAGENO)
           MOVE    ZERO                TO  SPA-NAI-SELNUM2  (WRK-PAGENO)
      *
           IF      WRK-PAGENO      =   1
               MOVE    W121-GRP2-NO    TO  SPA-GMN-GRP2-NO (WRK-PAGENO)
           ELSE
               MOVE    W121-GRP2-NO-1  TO  SPA-GMN-GRP2-NO (WRK-PAGENO)
           END-IF
      *
           IF      WRK-PAGENO      =   1
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       TO  W121-BODYLIST-ROW
           ELSE 
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       TO  W121-BODYLIST-1-ROW
           END-IF
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
           MOVE    ZERO            TO  FLG-BODYLIST-ROW    (WRK-PAGENO)
           MOVE    ZERO            TO  SPA-NAI-SELNUM2     (WRK-PAGENO)
      *
      *
           IF    ( SPA-GMN-GRP2-NO (WRK-PAGENO)
                                   NOT =   SPACE )
           AND   ( SPA-GMN-GRP2    (WRK-PAGENO)
                                       =   "F"   )    
               MOVE    "0038"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               IF      WRK-PAGENO      =   1
                   MOVE    100             TO  SPA-GMN-CUR
               ELSE
                   MOVE    300             TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41021-GRP2-NO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ編集処理
      *****************************************************************
       440-TAG-CHANGE-SEC            SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "B08"
                   IF    ( W121-PAGENO  =   0 )
                       MOVE    1       TO  IDYY
                   ELSE
                       MOVE    0       TO  IDYY
                   END-IF
               WHEN    OTHER
                   MOVE   W121-PAGENO  TO  IDYY
           END-EVALUATE
      *
      *    タグが変わったとき項目チェック
      *****IF      IDYY              NOT =   W121-PAGENO
               MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
               MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
               IF      SPA-ERRCD            =   SPACE
      *            関連チェック
      ****         PERFORM 4900-KANRENCHK-SEC
                   IF      SPA-ERRCD        =   SPACE
      *
      *                グループ設定解除の設定
                       INITIALIZE              SPA-GMN-GRP-OCC
                                                           (WRK-PAGENO) 
                       MOVE    "T"         TO  SPA-GMN-GRP1(WRK-PAGENO)
                       MOVE    "F"         TO  SPA-GMN-GRP2(WRK-PAGENO)
      *
                       EVALUATE    IDYY
                           WHEN    0
                               MOVE    8            TO  SPA-GMN-CUR
                           WHEN    1
                               MOVE    208          TO  SPA-GMN-CUR
                       END-EVALUATE
                   END-IF
               END-IF
      *****END-IF        
           .
       440-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
      *    平成２０年４月を跨ってるとき
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-HEADLIST-MAX
               IF    ( SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                                       <   WRK-CONS-SRYYM-20080401 )
               AND   ( SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
                                       >=  WRK-CONS-SRYYM-20080401 )
                   MOVE    "1004"                  TO  SPA-WIDCD 
                   GO  TO  210-BACK-EXT
               END-IF
           END-PERFORM
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "W12"               TO  SPA-SAKIPG
           MOVE    "W121"              TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *       種別↑処理
      *****************************************************************
       490-SYUBETU-UP-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "SYUBETU_UP"
                   MOVE    1           TO  WRK-PAGENO
               WHEN    "SYUBETU_UP_1"
                   MOVE    2           TO  WRK-PAGENO
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       =   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           =   1 )
               OR    ( SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                           =   1 )
                   GO  TO  490-SYUBETU-UP-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)    -   1
      *        上のグループ１と違うときの処理
               IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                              (WRK-PAGENO WRK-SELNUM2)
                                       NOT =   SPA-NAI-BODYLIST-TGRP1-NO
                                                  (WRK-PAGENO IDX) )
      *            既にグループ１が設定済のとき        
                   IF      SPA-NAI-BODYLIST-TGRP1-NO
                               (WRK-PAGENO WRK-SELNUM2)
                                           NOT =   "000"    
                       IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               <   SPA-GMN-BODYLIST-MAX
                                                       (WRK-PAGENO)
                           COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           +   1
                           IF     SPA-NAI-BODYLIST-TGRP1-NO
                               (WRK-PAGENO WRK-SELNUM2)
                                       =   SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX)
                               MOVE    "0039"      TO  SPA-ERRCD
                               GO  TO  490-SYUBETU-UP-EXT
                           END-IF
                       END-IF        
                   END-IF
      *
                   MOVE    ZERO            TO  WRK-GRP-INDEX
                   COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)    -   1
                   MOVE    SPA-NAI-BODYLIST-TGRP1-NO (WRK-PAGENO IDX)
                                           TO  WRK-GRP1-NO
                   PERFORM VARYING IDX FROM
                                   SPA-NAI-SELNUM2 (WRK-PAGENO)   BY  -1
                           UNTIL   IDX <       1
                       IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               NOT =   IDX
                           IF    ( WRK-GRP1-NO NOT =   "000"       )
                           AND   ( SPA-NAI-BODYLIST-TGRP1-NO
                                                       (WRK-PAGENO IDX)
                                               NOT =   WRK-GRP1-NO )
                               MOVE    IDX         TO  WRK-GRP-INDEX
                               MOVE    1           TO  IDX
                           ELSE
                               IF      SPA-NAI-BODYLIST-TGRP1-NO
                                                       (WRK-PAGENO IDX)
                                                           =   "000"
                                   COMPUTE WRK-GRP-INDEX   =   IDX -   1
                                   MOVE    1           TO  IDX
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   PERFORM 4901-SYUBETU-HENSYU-SEC
               ELSE
      *            グループ１が同じときの処理
                   MOVE    SPA-GMN-BODYLIST-TBL
                               (WRK-PAGENO WRK-SELNUM2)
                                           TO  WRK-GMN-BODYLIST-TBL (1)
                   MOVE    SPA-GMN-BODYLIST-TBL (WRK-PAGENO IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                               (WRK-PAGENO WRK-SELNUM2)
                   MOVE    WRK-GMN-BODYLIST-TBL (1)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                    (WRK-PAGENO IDX)
                   MOVE    IDX             TO  SPA-NAI-SELNUM2
                                                    (WRK-PAGENO)
                   IF      WRK-PAGENO      =   1 
                       MOVE    IDX             TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    IDX             TO  W121-BODYLIST-1-ROW
                   END-IF   
               END-IF
           END-IF
           .
       490-SYUBETU-UP-EXT.
           EXIT.
      *
      *****************************************************************
      *       種別移動編集処理
      *****************************************************************
       4901-SYUBETU-HENSYU-SEC              SECTION.
      *
           MOVE    SPA-GMN-BODYLIST-OCC (WRK-PAGENO)
                                       TO  WRK-GMN-BODYLIST-AREA
           INITIALIZE                      SPA-GMN-BODYLIST-OCC
                                                       (WRK-PAGENO)
           MOVE    ZERO                TO  IDY
      *
      *    先頭に移動するとき
           IF      WRK-GRP-INDEX       =   ZERO
               ADD     1                   TO  IDY
               MOVE    WRK-GMN-BODYLIST-TBL (WRK-SELNUM2)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                       (WRK-PAGENO IDY)
               IF      WRK-PAGENO      =   1 
                   MOVE    IDY             TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    IDY             TO  W121-BODYLIST-1-ROW
               END-IF   
           END-IF
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
               IF      IDX             =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-GMN-BODYLIST-TBL (IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                       (WRK-PAGENO IDY)
                   IF      IDX             =  WRK-GRP-INDEX
                       ADD     1               TO  IDY
                       MOVE    WRK-GMN-BODYLIST-TBL (WRK-SELNUM2)
                                               TO
                                   SPA-GMN-BODYLIST-TBL(WRK-PAGENO IDY)
                       IF      WRK-PAGENO      =   1 
                           MOVE    IDY         TO  W121-BODYLIST-ROW
                       ELSE
                           MOVE    IDY         TO  W121-BODYLIST-1-ROW
                       END-IF   
                   END-IF
               END-IF
           END-PERFORM
      *
      *    最後に移動するとき
           IF      WRK-GRP-INDEX       >   SPA-GMN-BODYLIST-MAX
                                                   (WRK-PAGENO)
               ADD     1                   TO  IDY
               MOVE    WRK-GMN-BODYLIST-TBL (WRK-SELNUM2)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                   (WRK-PAGENO IDY)
               IF      WRK-PAGENO       =   1 
                   MOVE    IDY         TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    IDY         TO  W121-BODYLIST-1-ROW
               END-IF   
           END-IF
      *
           INITIALIZE                      WRK-GMN-BODYLIST-AREA
      *
           IF      WRK-PAGENO       =   1 
               MOVE    W121-BODYLIST-ROW   TO
                                           SPA-NAI-SELNUM2 (WRK-PAGENO)
           ELSE
               MOVE    W121-BODYLIST-1-ROW TO
                                           SPA-NAI-SELNUM2 (WRK-PAGENO)
           END-IF
      *
           .
       4901-SYUBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *       種別↓処理
      *****************************************************************
       490-SYUBETU-DOWN-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "SYUBETU_DOWN"
                   MOVE    1           TO  WRK-PAGENO
               WHEN    "SYUBETU_DOWN_1"
                   MOVE    2           TO  WRK-PAGENO
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       =   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           =   SPA-GMN-BODYLIST-MAX
                                                        (WRK-PAGENO))
               OR    ( SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                           =   1                    )
                   GO  TO  490-SYUBETU-DOWN-EXT
               END-IF
           END-IF
      *    
           IF      SPA-ERRCD           =   SPACE
               COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)    +   1
               IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                               (WRK-PAGENO WRK-SELNUM2)
                                   NOT =   SPA-NAI-BODYLIST-TGRP1-NO
                                                      (WRK-PAGENO IDX) )
      *            下のグループ１と違うときの処理
                   IF      SPA-NAI-BODYLIST-TGRP1-NO
                               (WRK-PAGENO WRK-SELNUM2)
                                       NOT =   "000"    
                       IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               >   1
                           COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           -   1
                           IF     SPA-NAI-BODYLIST-TGRP1-NO
                               (WRK-PAGENO WRK-SELNUM2)
                                       =   SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX)
                               MOVE    "0039"      TO  SPA-ERRCD
                               GO  TO  490-SYUBETU-DOWN-EXT
                           END-IF
                       END-IF        
                   END-IF
      *
                   MOVE    ZERO            TO  WRK-GRP-INDEX
                   COMPUTE IDX =   SPA-NAI-SELNUM2 (WRK-PAGENO)    +   1
                   MOVE    SPA-NAI-BODYLIST-TGRP1-NO (WRK-PAGENO IDX)
                                           TO  WRK-GRP1-NO
                   PERFORM VARYING IDX FROM
                                   SPA-NAI-SELNUM2 (WRK-PAGENO)    BY  1
                           UNTIL   IDX >   SPA-GMN-BODYLIST-MAX
                                                       (WRK-PAGENO)
                       IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               NOT =   IDX
                           IF    ( WRK-GRP1-NO NOT =   "000"       )
                           AND   ( SPA-NAI-BODYLIST-TGRP1-NO
                                                       (WRK-PAGENO IDX)
                                               NOT =   WRK-GRP1-NO )
                               COMPUTE WRK-GRP-INDEX   =   IDX -   1
                               MOVE    SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                                   TO  IDX
                           ELSE
                               IF      SPA-NAI-BODYLIST-TGRP1-NO
                                                       (WRK-PAGENO IDX)
                                                           =   "000"
                                   MOVE    IDX         TO  WRK-GRP-INDEX
                                   MOVE    SPA-GMN-BODYLIST-MAX
                                                           (WRK-PAGENO)
                                                       TO  IDX
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-GRP-INDEX       =   ZERO
                       COMPUTE WRK-GRP-INDEX
                                   =   SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                       +   1
                   END-IF
                   PERFORM 4901-SYUBETU-HENSYU-SEC
               ELSE
      *            グループ１が同じときの処理
                   MOVE    SPA-GMN-BODYLIST-TBL
                               (WRK-PAGENO WRK-SELNUM2)
                                           TO  WRK-GMN-BODYLIST-TBL (1)
                   MOVE    SPA-GMN-BODYLIST-TBL (WRK-PAGENO IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                               (WRK-PAGENO WRK-SELNUM2)
                   MOVE    WRK-GMN-BODYLIST-TBL (1)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                    (WRK-PAGENO IDX)
                   MOVE    IDX             TO  SPA-NAI-SELNUM2
                                                    (WRK-PAGENO)
                   IF      WRK-PAGENO      =   1 
                       MOVE    IDX             TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    IDX             TO  W121-BODYLIST-1-ROW
                   END-IF
               END-IF
           END-IF
           .
       490-SYUBETU-DOWN-EXT.
           EXIT.
      *
      *****************************************************************
      *       グループ設定・終了処理
      *****************************************************************
       490-GRP-SETTEI-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "GRP_SETTEI"
                   MOVE    1           TO  WRK-PAGENO
               WHEN    "GRP_SETTEI_1"
                   MOVE    2           TO  WRK-PAGENO
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                       =   ZERO
               MOVE    "0102"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           >   ZERO
                   IF      WRK-PAGENO      =   1 
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                   END-IF
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO
                                       SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
               ELSE
                   IF      SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               >   ZERO
                       IF      WRK-PAGENO      =   1 
                           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                       ELSE
                           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                       END-IF
                   END-IF 
               END-IF
               MOVE    2                   TO  FLG-BODYLIST-ROW
                                                           (WRK-PAGENO)
               MOVE    ZERO                TO  SPA-NAI-SELNUM2
                                                           (WRK-PAGENO)
      *
               EVALUATE    SPA-GMN-GRP-SETTEI (WRK-PAGENO)
                   WHEN    SPACE
                       MOVE    SPACE       TO  SPA-GMN-GRP-KAIJYO
                                                           (WRK-PAGENO)
                       MOVE    "1"         TO  SPA-GMN-GRP-SETTEI
                                                           (WRK-PAGENO)
                       COMPUTE IDX =   SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                                       +   1
                       MOVE    IDX         TO  SPA-NAI-GRP1-NO
                                                           (WRK-PAGENO)
                       MOVE    IDX (2:3)   TO  SPA-GMN-GRP1-NO
                                                           (WRK-PAGENO)
                       MOVE    SPACE       TO  SPA-GMN-GRP2-NO
                                                           (WRK-PAGENO)
                       MOVE    "T"         TO  SPA-GMN-GRP1(WRK-PAGENO)
                       MOVE    "F"         TO  SPA-GMN-GRP2(WRK-PAGENO)
                       INITIALIZE              SPA-GMN-BODY-OCC
                                                           (WRK-PAGENO)
                                               SPA-NAI-BODY-OCC
                                                           (WRK-PAGENO)
                       IF      WRK-PAGENO      =   1
                           MOVE    50              TO  SPA-GMN-CUR
                       ELSE
                           MOVE    250             TO  SPA-GMN-CUR
                       END-IF
                   WHEN    "1"
                       INITIALIZE              SPA-GMN-GRP-OCC
                                                           (WRK-PAGENO) 
                       MOVE    "T"         TO  SPA-GMN-GRP1(WRK-PAGENO)
                       MOVE    "F"         TO  SPA-GMN-GRP2(WRK-PAGENO)
                       IF      WRK-PAGENO      =   1
                           MOVE    13              TO  SPA-GMN-CUR
                       ELSE
                           MOVE    213             TO  SPA-GMN-CUR
                       END-IF
               END-EVALUATE
           END-IF
           .
       490-GRP-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       グループ解除・終了処理
      *****************************************************************
       490-GRP-KAIJYO-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "GRP_KAIJYO"
                   MOVE    1           TO  WRK-PAGENO
               WHEN    "GRP_KAIJYO_1"
                   MOVE    2           TO  WRK-PAGENO
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                                       =   ZERO
               MOVE    "0102"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           >   ZERO
                   IF      WRK-PAGENO      =   1 
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                   END-IF
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  SPA-NAI-SELNUM2-ROW
                                                           (WRK-PAGENO)
               ELSE
                   IF      SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               >   ZERO
                       IF      WRK-PAGENO      =   1 
                           MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                       ELSE
                           MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                       END-IF
                   END-IF 
               END-IF
               MOVE    2                   TO  FLG-BODYLIST-ROW
                                                           (WRK-PAGENO)
               MOVE    ZERO                TO  SPA-NAI-SELNUM2
                                                           (WRK-PAGENO)
      *
               EVALUATE    SPA-GMN-GRP-KAIJYO (WRK-PAGENO)
                   WHEN    SPACE
                       INITIALIZE              SPA-GMN-GRP-OCC
                                                           (WRK-PAGENO)
                       MOVE    SPACE       TO  SPA-GMN-GRP-SETTEI
                                                           (WRK-PAGENO)
                       MOVE    "1"         TO  SPA-GMN-GRP-KAIJYO
                                                           (WRK-PAGENO)
                       INITIALIZE              SPA-GMN-BODY-OCC
                                                           (WRK-PAGENO)
                                               SPA-NAI-BODY-OCC
                                                           (WRK-PAGENO)
                       MOVE    "T"         TO  SPA-GMN-GRP1(WRK-PAGENO)
                       MOVE    "F"         TO  SPA-GMN-GRP2(WRK-PAGENO)
                       IF      WRK-PAGENO      =   1
                           MOVE    100             TO  SPA-GMN-CUR
                       ELSE
                           MOVE    300             TO  SPA-GMN-CUR
                       END-IF
                   WHEN    "1"
                       INITIALIZE              SPA-GMN-GRP-OCC
                                                           (WRK-PAGENO)
                       MOVE    "T"         TO  SPA-GMN-GRP1(WRK-PAGENO)
                       MOVE    "F"         TO  SPA-GMN-GRP2(WRK-PAGENO)
                       IF      WRK-PAGENO      =   1
                           MOVE    13              TO  SPA-GMN-CUR
                       ELSE
                           MOVE    213             TO  SPA-GMN-CUR
                       END-IF
               END-EVALUATE
           END-IF
           .
       490-GRP-KAIJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *       グループ１設定処理
      *****************************************************************
       490-GRP1-SETTEI-SEC          SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "GRP1"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    W121-GRP1   TO  SPA-GMN-GRP1 (WRK-PAGENO)
                   MOVE    W121-GRP2   TO  SPA-GMN-GRP2 (WRK-PAGENO)
               WHEN    "GRP1_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    W121-GRP1-1 TO  SPA-GMN-GRP1 (WRK-PAGENO)
                   MOVE    W121-GRP2-1 TO  SPA-GMN-GRP2 (WRK-PAGENO)
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                        >   ZERO
               IF      WRK-PAGENO      =   1 
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  SPA-NAI-SELNUM2-ROW
                                                           (WRK-PAGENO)
           ELSE
               IF      SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                           >   ZERO
                   IF      WRK-PAGENO      =   1 
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                   END-IF
               END-IF 
           END-IF
           MOVE    2                   TO  FLG-BODYLIST-ROW (WRK-PAGENO)
           MOVE    ZERO                TO  SPA-NAI-SELNUM2  (WRK-PAGENO)
      *
           IF      SPA-GMN-GRP1 (WRK-PAGENO)
                                       =   "T"
               IF      SPA-GMN-GRP-SETTEI (WRK-PAGENO)
                                           =   "1"
                   COMPUTE IDX     =   SPA-GMN-GRP1-NO-MAX (WRK-PAGENO)
                                       +   1
                   MOVE    IDX         TO  SPA-NAI-GRP1-NO(WRK-PAGENO)
                   MOVE    IDX (2:3)   TO  SPA-GMN-GRP1-NO(WRK-PAGENO)
               ELSE
                   MOVE    SPACE       TO  SPA-GMN-GRP2-NO(WRK-PAGENO)
                   MOVE    ZERO        TO  SPA-NAI-GRP1-NO(WRK-PAGENO)
               END-IF
               MOVE    SPACE           TO  SPA-GMN-GRP2-NO(WRK-PAGENO)
      *
               IF      SPA-GMN-GRP-SETTEI (WRK-PAGENO)
                                           =   "1"
                   IF      WRK-PAGENO      =   1
                       MOVE    50              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    250             TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      WRK-PAGENO      =   1
                       MOVE    100             TO  SPA-GMN-CUR
                   ELSE
                       MOVE    300             TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       490-GRP1-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       グループ２設定処理
      *****************************************************************
       490-GRP2-SETTEI-SEC          SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "GRP2"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    W121-GRP1   TO  SPA-GMN-GRP1 (WRK-PAGENO)
                   MOVE    W121-GRP2   TO  SPA-GMN-GRP2 (WRK-PAGENO)
               WHEN    "GRP2_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    W121-GRP1-1 TO  SPA-GMN-GRP1 (WRK-PAGENO)
                   MOVE    W121-GRP2-1 TO  SPA-GMN-GRP2 (WRK-PAGENO)
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) 
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) 
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO) 
                                           TO  SPA-NAI-SELNUM2-ROW
                                                       (WRK-PAGENO)
           ELSE
               IF      SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                       >   ZERO
                   IF      WRK-PAGENO          =   1
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-ROW
                   ELSE
                       MOVE    SPA-NAI-SELNUM2-ROW (WRK-PAGENO)
                                               TO  W121-BODYLIST-1-ROW
                   END-IF
               END-IF 
           END-IF
           MOVE    2                   TO  FLG-BODYLIST-ROW(WRK-PAGENO)
           MOVE    ZERO                TO  SPA-NAI-SELNUM2 (WRK-PAGENO)
      *
           IF      SPA-GMN-GRP2 (WRK-PAGENO)
                                       =   "T"
               MOVE    SPACE           TO  SPA-GMN-GRP1-NO (WRK-PAGENO)
               MOVE    ZERO            TO  SPA-NAI-GRP1-NO (WRK-PAGENO)
               IF      SPA-GMN-GRP-SETTEI (WRK-PAGENO)
                                           =   "1"
                   IF      WRK-PAGENO      =   1
                       MOVE    51              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    251             TO  SPA-GMN-CUR
                       END-IF
               ELSE
                   IF      WRK-PAGENO      =   1
                       MOVE    100             TO  SPA-GMN-CUR
                   ELSE
                       MOVE    300             TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       490-GRP2-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費↑処理  
      *****************************************************************
       490-KOH-UP-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "KOH_UP"
                   MOVE    1           TO  WRK-PAGENO
               WHEN    "KOH_UP_1"
                   MOVE    2           TO  WRK-PAGENO
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    ZERO                TO  FLG-BODYLIST-ROW 
                                                       (WRK-PAGENO)
           END-IF
      *
           IF    ( SPA-NAI-SELNUM2 (WRK-PAGENO) 
                                       =   ZERO )
           OR    ( SPA-NAI-SELNUM3 (WRK-PAGENO)
                                       =   ZERO )
               MOVE    "0104"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-SELNUM3 (WRK-PAGENO)
                                           =   1 )
               OR    ( SPA-GMN-KOHLIST-MAX (WRK-PAGENO WRK-SELNUM2)
                                           =   1 )
                   GO  TO  490-KOH-UP-EXT
               ELSE
                   COMPUTE IDX =   SPA-NAI-SELNUM3 (WRK-PAGENO)    -   1
                   IF      SPA-GMN-KOHLIST-TJYUN
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                                   NOT =   SPA-GMN-KOHLIST-TJYUN
                                           (WRK-PAGENO WRK-SELNUM2 IDX)
                       MOVE    "0040"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SPA-GMN-KOHLIST-TBL
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                                           TO  WRK-GMN-KOHLIST-TBL-R (1)
                       MOVE    SPA-GMN-KOHLIST-TBL
                                          (WRK-PAGENO WRK-SELNUM2 IDX)
                                           TO
                               SPA-GMN-KOHLIST-TBL
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                       MOVE    WRK-GMN-KOHLIST-TBL-R (1)
                                           TO  SPA-GMN-KOHLIST-TBL
                                           (WRK-PAGENO WRK-SELNUM2 IDX)
                       MOVE    IDX         TO  SPA-NAI-SELNUM3
                                                            (WRK-PAGENO)
      *
      *               グループ１設定済のとき 
                      IF      SPA-NAI-BODYLIST-TGRP1-NO
                                               (WRK-PAGENO WRK-SELNUM2)
                                           NOT = "000"
                          PERFORM VARYING IDX FROM    1   BY  1
                                  UNTIL   IDX >   SPA-GMN-BODYLIST-MAX
                                                           (WRK-PAGENO)
                              IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                           (WRK-PAGENO WRK-SELNUM2)
                                       =   SPA-NAI-BODYLIST-TGRP1-NO
                                                      (WRK-PAGENO IDX) )
                              AND   ( IDX NOT =   SPA-NAI-SELNUM2
                                                      (WRK-PAGENO)     )
                                  MOVE    SPA-GMN-KOHLIST
                                              (WRK-PAGENO WRK-SELNUM2)
                                               TO  SPA-GMN-KOHLIST
                                                       (WRK-PAGENO IDX)
                              END-IF
                          END-PERFORM
                       END-IF
      *
      ***              MOVE    SPACE       TO  SPA-GMN-HKNNUM
      ***                                      SPA-GMN-SEIDONAME
                   END-IF
                   IF      WRK-PAGENO          =   1
                       MOVE    101         TO  SPA-GMN-CUR
                   ELSE 
                       MOVE    301         TO  SPA-GMN-CUR
                   END-IF 
               END-IF
           END-IF
           .
       490-KOH-UP-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費↓処理
      *****************************************************************
       490-KOH-DOWN-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "KOH_DOWN"
                   MOVE    1           TO  WRK-PAGENO
               WHEN    "KOH_DOWN_1"
                   MOVE    2           TO  WRK-PAGENO
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    ZERO                TO  FLG-BODYLIST-ROW 
                                                       (WRK-PAGENO)
           END-IF
      *
           IF    ( SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       =   ZERO )
           OR    ( SPA-NAI-SELNUM3 (WRK-PAGENO)
                                       =   ZERO )
               MOVE    "0104"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-SELNUM3 (WRK-PAGENO) 
                                       =   SPA-GMN-KOHLIST-MAX
                             (WRK-PAGENO WRK-SELNUM2) )
               OR    ( SPA-GMN-KOHLIST-MAX
                             (WRK-PAGENO WRK-SELNUM2)
                                           =   1      )
                   GO  TO  490-KOH-DOWN-EXT
               ELSE
      *
                   COMPUTE IDX =   SPA-NAI-SELNUM3 (WRK-PAGENO) +   1
                   IF      SPA-GMN-KOHLIST-TJYUN
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                                   NOT =   SPA-GMN-KOHLIST-TJYUN
                                           (WRK-PAGENO WRK-SELNUM2 IDX)
                       MOVE    "0040"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SPA-GMN-KOHLIST-TBL
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                                           TO  WRK-GMN-KOHLIST-TBL-R (1)
                       MOVE    SPA-GMN-KOHLIST-TBL
                                      (WRK-PAGENO WRK-SELNUM2 IDX)
                                           TO  SPA-GMN-KOHLIST-TBL
                                    (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                       MOVE    WRK-GMN-KOHLIST-TBL-R (1)
                                           TO  SPA-GMN-KOHLIST-TBL
                                           (WRK-PAGENO WRK-SELNUM2 IDX)
                       MOVE    IDX         TO  SPA-NAI-SELNUM3
                                                       (WRK-PAGENO)
      *
      *               グループ１設定済のとき 
                      IF      SPA-NAI-BODYLIST-TGRP1-NO
                                           (WRK-PAGENO WRK-SELNUM2)
                                           NOT = "000"
                          PERFORM VARYING IDX FROM    1   BY  1
                                  UNTIL   IDX >   SPA-GMN-BODYLIST-MAX
                                                     (WRK-PAGENO)
                              IF    ( SPA-NAI-BODYLIST-TGRP1-NO
                                           (WRK-PAGENO WRK-SELNUM2)
                                       =   SPA-NAI-BODYLIST-TGRP1-NO
                                                   (WRK-PAGENO IDX) )
                              AND   ( IDX NOT =   SPA-NAI-SELNUM2
                                                    (WRK-PAGENO)    )
                                  MOVE    SPA-GMN-KOHLIST
                                          (WRK-PAGENO WRK-SELNUM2)
                                               TO  SPA-GMN-KOHLIST
                                                       (WRK-PAGENO IDX)
                              END-IF
                          END-PERFORM
                       END-IF
      *
      ***                 MOVE    SPACE       TO  SPA-GMN-HKNNUM
      ***                                         SPA-GMN-SEIDONAME
                   END-IF
                   IF      WRK-PAGENO          =   1
                       MOVE    101         TO  SPA-GMN-CUR
                   ELSE 
                       MOVE    301         TO  SPA-GMN-CUR
                   END-IF 
               END-IF
           END-IF
           .
       490-KOH-DOWN-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費先頭設定処理
      *****************************************************************
       490-KOH-TOP-HENSYU-SEC          SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "TOP"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "TOP_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    215         TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   1
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    ZERO                TO  FLG-BODYLIST-ROW 
                                                       (WRK-PAGENO)
           END-IF
      * 
           IF    ( SPA-GMN-HKNNUM (WRK-PAGENO)
                                       =   SPACE )
           OR    ( SPA-GMN-SEIDONAME (WRK-PAGENO)
                                       =   SPACE )
               MOVE    "0041"              TO  SPA-ERRCD
               GO  TO  490-KOH-TOP-HENSYU-EXT
           END-IF
      * 
           EVALUATE    SPA-GMN-HKNNUM (WRK-PAGENO)
               WHEN    "XXX"
                   IF      SPA-GMN-CHOUKI (WRK-PAGENO)
                                               NOT =   SPACE
                       MOVE    "0042"              TO  SPA-ERRCD
                       GO  TO  490-KOH-TOP-HENSYU-EXT
                   END-IF
               WHEN    "YYY"
                   IF      SPA-GMN-ZAITAKU (WRK-PAGENO)
                                               NOT =   SPACE
                       MOVE    "0043"              TO  SPA-ERRCD
                       GO  TO  490-KOH-TOP-HENSYU-EXT
                   END-IF
           END-EVALUATE
      * 
           IF      SPA-NAI-SELNUM3 (WRK-PAGENO)
                                        =   ZERO
               IF      SPA-GMN-KOHLIST-MAX
                      (WRK-PAGENO WRK-SELNUM2)
                                            >=  WRK-CONS-KOHLIST-MAX
                   MOVE    "0105"              TO  SPA-ERRCD
                   MOVE    SPACE               TO  SPA-GMN-HKNNUM
                                                           (WRK-PAGENO)
                                                   SPA-GMN-SEIDONAME
                                                           (WRK-PAGENO)
                   GO  TO  490-KOH-TOP-HENSYU-EXT
               END-IF
           ELSE
               IF      SPA-GMN-KOHLIST-TJYUN
                              (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                                           =   "T"
                   MOVE    SPACE               TO  SPA-GMN-HKNNUM
                                                           (WRK-PAGENO)
                                                   SPA-GMN-SEIDONAME
                                                           (WRK-PAGENO)
                   GO  TO  490-KOH-TOP-HENSYU-EXT
               ELSE
                   INITIALIZE          SPA-GMN-KOHLIST-TBL
                              (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
               END-IF
           END-IF     
      *
           MOVE    "T"                 TO  WRK-KOH-KBN
           PERFORM 4901-KOH-HENYSU-SEC
           .
       490-KOH-TOP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費設定編集処理
      *****************************************************************
       4901-KOH-HENYSU-SEC          SECTION.
      *
      *    並びでわける
           INITIALIZE              WRK-GMN-KOHLIST-AREA           
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-KOHLIST-MAX
                                           (WRK-PAGENO WRK-SELNUM2)
               IF      SPA-GMN-KOHLIST-THKNNUM
                                      (WRK-PAGENO WRK-SELNUM2 IDX)
                                   NOT =   SPACE
                   EVALUATE    SPA-GMN-KOHLIST-TJYUN
                                     (WRK-PAGENO WRK-SELNUM2 IDX)
                       WHEN    "T"
                           MOVE    1           TO  IDY
                       WHEN    "B"             
                           MOVE    2           TO  IDY
                   END-EVALUATE    
      *
                   ADD     1           TO  WRK-GMN-KOHLIST-MAX (IDY)
                   MOVE    WRK-GMN-KOHLIST-MAX (IDY)
                                       TO  IDZ
                   MOVE    SPA-GMN-KOHLIST-TBL
                               (WRK-PAGENO WRK-SELNUM2 IDX)
                                       TO  WRK-GMN-KOHLIST-TBL (IDY IDZ)
               END-IF
           END-PERFORM
      *
      *    削除以外は該当の並びの最後にセット
           EVALUATE    WRK-KOH-KBN
               WHEN    "T"
                   MOVE    1           TO  IDY
               WHEN    "B"             
                   MOVE    2           TO  IDY
               WHEN    OTHER             
                   MOVE    ZERO        TO  IDY
           END-EVALUATE
           IF      IDY             >   ZERO
               ADD     1               TO  WRK-GMN-KOHLIST-MAX(IDY)
               MOVE    WRK-GMN-KOHLIST-MAX  (IDY)
                                       TO  IDZ
               MOVE    SPA-GMN-HKNNUM (WRK-PAGENO)
                                       TO  WRK-GMN-KOHLIST-THKNNUM 
                                                              (IDY IDZ)
               MOVE    SPA-GMN-SEIDONAME (WRK-PAGENO)
                                       TO  WRK-GMN-KOHLIST-TSEIDONAME 
                                                              (IDY IDZ)
               MOVE    WRK-KOH-KBN     TO  WRK-GMN-KOHLIST-TJYUN
                                                              (IDY IDZ)
           END-IF
      *
      *    先頭・後尾の順にＳＰＡへ設定 
           INITIALIZE                  SPA-GMN-KOHLIST
                                           (WRK-PAGENO WRK-SELNUM2)
           MOVE    ZERO            TO  IDZ
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       60
                   OR      IDZ     >=      WRK-CONS-KOHLIST-MAX
               IF      WRK-GMN-KOHLIST-THKNNUM-R (IDX)
                                       NOT =   SPACE      
                   ADD     1               TO  IDZ
                   MOVE    WRK-GMN-KOHLIST-TBL-R (IDX)
                                           TO
                                    SPA-GMN-KOHLIST-TBL
                                       (WRK-PAGENO WRK-SELNUM2 IDZ)
               END-IF
           END-PERFORM
           MOVE    IDZ                 TO  SPA-GMN-KOHLIST-MAX
                                            (WRK-PAGENO WRK-SELNUM2)
      *
      *    グループ１設定済のとき 
           IF      SPA-NAI-BODYLIST-TGRP1-NO-9 (WRK-PAGENO WRK-SELNUM2)
                               >   ZERO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >
                                   SPA-GMN-BODYLIST-MAX (WRK-PAGENO)
                   IF    ( SPA-NAI-BODYLIST-TGRP1-NO-9
                                           (WRK-PAGENO WRK-SELNUM2)
                                   =   SPA-NAI-BODYLIST-TGRP1-NO-9
                                                 (WRK-PAGENO IDX)   )
                   AND   ( IDX NOT =   WRK-SELNUM2 )
                           MOVE    SPA-GMN-KOHLIST
                                       (WRK-PAGENO WRK-SELNUM2)
                                       TO
                                       SPA-GMN-KOHLIST (WRK-PAGENO IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-HKNNUM   (WRK-PAGENO)
                                           SPA-GMN-SEIDONAME(WRK-PAGENO)
           MOVE    ZERO                TO  SPA-NAI-SELNUM3  (WRK-PAGENO)
           .
       4901-KOH-HENYSU-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費後尾設定処理
      *****************************************************************
       490-KOH-BOTTOM-HENSYU-SEC          SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "BOTTOM"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "BOTTOM_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    215         TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   2
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    ZERO                TO  FLG-BODYLIST-ROW 
                                                       (WRK-PAGENO)
           END-IF
      *
           IF    ( SPA-GMN-HKNNUM    (WRK-PAGENO)
                                       =   SPACE )
           OR    ( SPA-GMN-SEIDONAME (WRK-PAGENO)
                                       =   SPACE )
               MOVE    "0041"              TO  SPA-ERRCD
               GO  TO  490-KOH-BOTTOM-HENSYU-EXT
           END-IF
      * 
           EVALUATE    SPA-GMN-HKNNUM (WRK-PAGENO)
               WHEN    "XXX"
                   IF      SPA-GMN-CHOUKI (WRK-PAGENO)
                                           NOT =   SPACE
                       MOVE    "0042"              TO  SPA-ERRCD
                       GO  TO  490-KOH-BOTTOM-HENSYU-EXT
                   END-IF
               WHEN    "YYY"
                   IF      SPA-GMN-ZAITAKU(WRK-PAGENO)
                                           NOT =   SPACE
                       MOVE    "0043"              TO  SPA-ERRCD
                       GO  TO  490-KOH-BOTTOM-HENSYU-EXT
                   END-IF
           END-EVALUATE
      * 
           IF      SPA-NAI-SELNUM3 (WRK-PAGENO)
                                        =   ZERO
               IF      SPA-GMN-KOHLIST-MAX
                       (WRK-PAGENO WRK-SELNUM2)
                                            >=  WRK-CONS-KOHLIST-MAX
                   MOVE    "0105"      TO  SPA-ERRCD
                   MOVE    SPACE       TO  SPA-GMN-HKNNUM (WRK-PAGENO)
                                           SPA-GMN-SEIDONAME(WRK-PAGENO)
                   GO  TO  490-KOH-BOTTOM-HENSYU-EXT
               END-IF
           ELSE
               IF      SPA-GMN-KOHLIST-TJYUN
                                  (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
                                           =   "B"
                   MOVE    SPACE       TO  SPA-GMN-HKNNUM (WRK-PAGENO)
                                           SPA-GMN-SEIDONAME(WRK-PAGENO)
                   GO  TO  490-KOH-BOTTOM-HENSYU-EXT
               ELSE
                   INITIALIZE          SPA-GMN-KOHLIST-TBL
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
               END-IF 
           END-IF     
      *
           MOVE    "B"                 TO  WRK-KOH-KBN
           PERFORM 4901-KOH-HENYSU-SEC
           .
       490-KOH-BOTTOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費削除設定処理
      *****************************************************************
       490-KOH-DELETE-SEC          SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "DEL"
                   MOVE    1           TO  WRK-PAGENO
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "DEL_1"
                   MOVE    2           TO  WRK-PAGENO
                   MOVE    215         TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                   TO  WRK-SELNUM2
           MOVE    SPA-NAI-SELNUM3 (WRK-PAGENO)
                                   TO  WRK-SELNUM3
      *
           IF      SPA-NAI-SELNUM2 (WRK-PAGENO)
                                       >   ZERO
               IF      WRK-PAGENO          =   2
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-ROW
               ELSE
                   MOVE    SPA-NAI-SELNUM2 (WRK-PAGENO)
                                           TO  W121-BODYLIST-1-ROW
               END-IF
               MOVE    ZERO                TO  FLG-BODYLIST-ROW 
                                                       (WRK-PAGENO)
           END-IF
      *
           IF    ( SPA-GMN-HKNNUM    (WRK-PAGENO)
                                       =   SPACE )
           OR    ( SPA-GMN-SEIDONAME (WRK-PAGENO)
                                       =   SPACE )
               MOVE    "0104"              TO  SPA-ERRCD
               GO  TO  490-KOH-DELETE-EXT
           END-IF
      * 
           IF      SPA-NAI-SELNUM3 (WRK-PAGENO) 
                                       =   ZERO
               MOVE    "0044"              TO  SPA-ERRCD
               GO  TO  490-KOH-DELETE-EXT
           END-IF
      *  
           INITIALIZE                  SPA-GMN-KOHLIST-TBL
                                   (WRK-PAGENO WRK-SELNUM2 WRK-SELNUM3)
      *
           MOVE    SPACE               TO  WRK-KOH-KBN
           PERFORM 4901-KOH-HENYSU-SEC
           .
       490-KOH-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *       クリア確認処理
      *****************************************************************
       490-CLEAR-GID-SEC              SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
           .
       490-CLEAR-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *       クリア処理
      *****************************************************************
       490-CLEAR-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-GMN-HEAD1-KOUMOKU
                                       SPA-GMN-BODY-KOUMOKU
                                       SPA-GMN-GRP-KOUMOKU
                                       SPA-GMN-BODYLIST
      ****                             SPA-GMN-BODYLIST-MAX
      *
           MOVE    "T"                 TO  SPA-GMN-GRP1 (1)
                                           SPA-GMN-GRP1 (2)
           MOVE    "F"                 TO  SPA-GMN-GRP2 (1)
                                           SPA-GMN-GRP2 (2)
      *
      *    レセプト（編綴順）情報編集処理
           PERFORM 3102-SPA-2008-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-HEADLIST-ROW
                                           FLG-BODYLIST-ROW (1)
                                           FLG-BODYLIST-ROW (2)
                                           FLG-KOHLIST-ROW  (1)
                                           FLG-KOHLIST-ROW  (2)
      *
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    ZERO            TO  SPA-W121-PAGENO
           .
       490-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      490-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    有効期間の未入力チェック
           IF      SPA-NAI-STYUKYMD    =   ZERO
               IF      SPA-GMN-STYUKYMD    =   "00000000"
                   CONTINUE
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
                   MOVE    "0003"              TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
           IF      SPA-NAI-EDYUKYMD    =   ZERO
               IF      SPA-GMN-EDYUKYMD    =   SPACE
                   MOVE    99999999            TO  SPA-NAI-EDYUKYMD
                   MOVE    "99999999"          TO  SPA-GMN-EDYUKYMD
               ELSE
                   MOVE    3                   TO  SPA-GMN-CUR
                   MOVE    "0003"              TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    有効期間と改正の重複チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-SYS-2003-MAX
                   OR      SPA-ERRCD   NOT =   SPACE
               IF    ( SPA-NAI-STYUKYMD    >=
                                   SPA-SYS-2003-STYUKYMD (IDX) )
               OR    ( SPA-NAI-EDYUKYMD    <=
                                   SPA-SYS-2003-STYUKYMD (IDX) )
                   CONTINUE
               ELSE
                   MOVE    "0049"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効期間の重複チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-HEADLIST-MAX
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-GMN-SELNUM1     =   IDX
      *            選択された内容の開始日が変更された場合
      ***             IF      SPA-NAI-STYUKYMD    =
      ***                             SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                       MOVE    ZERO            TO  FLG-CHK
      ***             ELSE
      ***                 MOVE    1               TO  FLG-CHK
      ***             END-IF
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF      FLG-CHK             =   1
                   IF    ( SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
                                           <   SPA-NAI-STYUKYMD )
                   OR    ( SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                                           >   SPA-NAI-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    種別一覧の未入力チェック
           IF      SPA-GMN-BODYLIST-MAX (1)
                                       =   ZERO    
               MOVE    8                   TO  SPA-GMN-CUR
               MOVE    "0102"              TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
           IF      SPA-GMN-BODYLIST-MAX (2)
                                       =   ZERO    
               MOVE    208                 TO  SPA-GMN-CUR
               MOVE    "0102"              TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    第一出力順と保険者印刷順（県内）
           IF      SPA-GMN-PRTJYUN1 (1)
                                       =   "02"
               IF      SPA-GMN-HKNJANUM-PRTJYUN
                                           =   SPACE
                   CONTINUE
               ELSE
                   MOVE    8                   TO  SPA-GMN-CUR  
                   MOVE    "0047"              TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF 
           END-IF 
      *
      *    第一出力順と所在地保険者（県内）
           IF      SPA-GMN-PRTJYUN1 (1)
                                       =   "02"
               IF    ( SPA-GMN-HKNJANUM    =   SPACE )
               AND   ( SPA-GMN-HKNJANUM-PRTKBN
                                           =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    8                   TO  SPA-GMN-CUR  
                   MOVE    "0045"              TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF 
           END-IF 
      *
      *    第一出力順とグループ２
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
                   OR      SPA-ERRCD   NOT =   SPACE             
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       SPA-GMN-BODYLIST-MAX (IDXX)
                     OR      SPA-ERRCD   NOT =   SPACE
      *        グループ２
               IF    ( SPA-GMN-PRTJYUN1 (IDXX) =   "01"  )
               AND   ( SPA-NAI-BODYLIST-TGRP2-NO (IDXX IDX)
                                           NOT =   SPACE )
                   MOVE    "0046"              TO  SPA-ERRCD
                   IF      IDXX                =   1
                       MOVE    8                   TO  SPA-GMN-CUR
                   ELSE   
                       MOVE    208                 TO  SPA-GMN-CUR  
                   END-IF         
               END-IF
      *        第一出力順と種別内設定１
               IF      SPA-ERRCD       =   SPACE             
                   IF    ( SPA-GMN-PRTJYUN1 (IDXX) NOT =   "02" )
                   AND   ( SPA-NAI-BODYLIST-TPRTJYUN1 (IDXX IDX)
                                                       =   "07" )
                       MOVE    "0033"              TO  SPA-ERRCD
                   IF      IDXX                =   1
                       MOVE    8                   TO  SPA-GMN-CUR
                   ELSE   
                       MOVE    208                 TO  SPA-GMN-CUR  
                   END-IF         
                   END-IF
               END-IF
      *        第一出力順と種別内設定２
               IF      SPA-ERRCD       =   SPACE             
                   IF    ( SPA-GMN-PRTJYUN1 (IDXX) NOT =   "02"  )
                   AND   ( SPA-NAI-BODYLIST-TPRTJYUN2 (IDXX IDX)
                                                   NOT =   SPACE )
                       MOVE    "0033"              TO  SPA-ERRCD
                       IF      IDXX                =   1
                           MOVE    8                   TO  SPA-GMN-CUR
                       ELSE   
                           MOVE    208                 TO  SPA-GMN-CUR  
                       END-IF         
                   END-IF
               END-IF
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-KOHLIST-MAX(IDXX IDX) 
                       OR      SPA-ERRCD   NOT =   SPACE             
      *            在総診・在医総
                   IF    ( SPA-GMN-ZAITAKU (IDXX)  NOT =   SPACE )
                   AND   ( SPA-GMN-KOHLIST-THKNNUM (IDXX IDX IDY)
                                                       =   "YYY" )
                       MOVE    "0043"              TO  SPA-ERRCD
                       IF      IDXX                =   1
                           MOVE    10                  TO  SPA-GMN-CUR
                       ELSE   
                           MOVE    210                 TO  SPA-GMN-CUR  
                       END-IF         
                   END-IF
      *            長期
                   IF    ( SPA-GMN-CHOUKI  (IDXX)  NOT =   SPACE )
                   AND   ( SPA-GMN-KOHLIST-THKNNUM (IDXX IDX IDY)
                                                       =   "XXX" )
                       MOVE    "0042"              TO  SPA-ERRCD
                       IF      IDXX                =   1
                           MOVE    11                  TO  SPA-GMN-CUR
                       ELSE   
                           MOVE    211                 TO  SPA-GMN-CUR  
                       END-IF         
                   END-IF
               END-PERFORM
             END-PERFORM
           END-PERFORM        
      *
      *    
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    ＨＥＡＤ部更新処理
           IF      SPA-GMN-SELNUM1     >   ZERO
               INITIALIZE                      RECE-PUTH-REC
               MOVE    SPA-HOSPNUM          TO  RECE-PUTH-HOSPNUM
               MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                           TO  RECE-PUTH-STYUKYMD
               MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                           TO  RECE-PUTH-EDYUKYMD
               MOVE    RECE-PUTH-REC       TO  MCPDATA-REC
               PERFORM 900-RECE-PUTH-READ-SEC
               IF     FLG-RECE-PUTH        =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "0101"              TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-SELNUM1     =   ZERO
               INITIALIZE                      RECE-PUTH-REC
               MOVE    SPA-HOSPNUM          TO  RECE-PUTH-HOSPNUM
               MOVE    SPA-NAI-STYUKYMD    TO  RECE-PUTH-STYUKYMD
               MOVE    SPA-NAI-EDYUKYMD    TO  RECE-PUTH-EDYUKYMD
           ELSE    
               MOVE    SPA-NAI-STYUKYMD    TO  RECE-PUTH-STYUKYMD
               MOVE    SPA-NAI-EDYUKYMD    TO  RECE-PUTH-EDYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-PREFKBN-PRTJYUN
                                       TO  RECE-PUTH-PREFKBN-PRTJYUN
           MOVE    SPA-GMN-HKNJANUM-PRTJYUN
                                       TO  RECE-PUTH-HKNJANUM-PRTJYUN 
           MOVE    SPA-GMN-HKNJANUM
                                       TO  RECE-PUTH-HKNJANUM
           MOVE    SPA-GMN-HKNJANUM-PRTKBN
                                       TO  RECE-PUTH-HKNJANUM-PRTKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               MOVE    SPA-GMN-PRTJYUN1(IDXX) TO RECE-PUTH-SRT-01 (IDXX) 
               MOVE    SPA-GMN-PRTJYUN2(IDXX) TO RECE-PUTH-KKHKBN (IDXX) 
               MOVE    SPA-GMN-CHOUKI  (IDXX) TO RECE-PUTH-CHOUKI (IDXX) 
               MOVE    SPA-GMN-ZAITAKU (IDXX) TO RECE-PUTH-ZAITAKU(IDXX) 
               MOVE    SPA-GMN-KOUGAKU-TEN
                                       (IDXX) TO RECE-PUTH-KOUGAKU-TEN
                                                                  (IDXX) 
               MOVE    SPA-GMN-SRT-LAST(IDXX) TO RECE-PUTH-SRT-LAST
                                                                  (IDXX)
           END-PERFORM
           MOVE    SPA-TERMID          TO  RECE-PUTH-TERMID
           MOVE    SPA-OPID            TO  RECE-PUTH-OPID
      *
           IF      SPA-GMN-SELNUM1     =   ZERO
               MOVE    SMCNDATE-YMD        TO  RECE-PUTH-CREYMD
               MOVE    SMCNDATE-YMD        TO  RECE-PUTH-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECE-PUTH-UPHMS
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rece_puth"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
           ELSE
               MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                           TO  RECE-PUTH-UPSTYUKYMD
               MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                           TO  RECE-PUTH-UPEDYUKYMD
               MOVE    SMCNDATE-YMD        TO  RECE-PUTH-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECE-PUTH-UPHMS
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_rece_puth"     TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
           END-IF
      *
           MOVE    RECE-PUTH-REC       TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               IF      SPA-GMN-SELNUM1     =   ZERO
                   MOVE    "1001"              TO  SPA-ERRCD
               ELSE
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
      *   ＢＯＤＹ部更新処理
           IF      SPA-GMN-SELNUM1     >   ZERO
               INITIALIZE                      RECE-PUTB-REC
               MOVE    SPA-HOSPNUM          TO  RECE-PUTB-HOSPNUM
               MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                           TO  RECE-PUTB-STYUKYMD
               MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                           TO  RECE-PUTB-EDYUKYMD
               MOVE    RECE-PUTB-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_rece_putb"     TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1003"              TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               MOVE    ZERO            TO  WRK-SYUBETU-SRT1
               MOVE    SPACE           TO  WRK-SYUBETU-GAI
                                           WRK-SYUBETU-NYU
               MOVE    SPACE           TO  WRK-GRP1-NO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-BODYLIST-MAX (IDXX)
                       OR      SPA-ERRCD   NOT =   SPACE
                   INITIALIZE                      RECE-PUTB-REC
                   MOVE    SPA-HOSPNUM          TO  RECE-PUTB-HOSPNUM
                   EVALUATE    IDXX
                       WHEN    1
                           MOVE    "1"         TO  RECE-PUTB-PREFKBN
                       WHEN    2
                           MOVE    "2"         TO  RECE-PUTB-PREFKBN
                   END-EVALUATE
                   MOVE    SPA-NAI-STYUKYMD    TO  RECE-PUTB-STYUKYMD
                   MOVE    SPA-NAI-EDYUKYMD    TO  RECE-PUTB-EDYUKYMD
                   MOVE    SPA-GMN-BODYLIST-TSYUBETU (IDXX IDX)
                                               TO  RECE-PUTB-SYUBETU-GAI
                                                   RECE-PUTB-SYUBETU-NYU
                   EVALUATE    RECE-PUTB-SYUBETU-GAI (4:1)
                       WHEN    "2"
                           MOVE    "1"         TO
                                           RECE-PUTB-SYUBETU-NYU (4:1)
                       WHEN    "4"
                           MOVE    "3"         TO
                                           RECE-PUTB-SYUBETU-NYU (4:1)
                       WHEN    "6"
                           MOVE    "5"         TO
                                           RECE-PUTB-SYUBETU-NYU (4:1)
                       WHEN    "8"
                           MOVE    "7"         TO
                                           RECE-PUTB-SYUBETU-NYU (4:1)
                       WHEN    "0"
                           MOVE    "9"         TO
                                           RECE-PUTB-SYUBETU-NYU (4:1)
                   END-EVALUATE
                   IF      SPA-NAI-BODYLIST-TGRP1-NO (IDXX IDX)
                                           NOT =   "000"
                       IF      SPA-NAI-BODYLIST-TGRP1-NO (IDXX IDX)
                                               NOT =   WRK-GRP1-NO
                           MOVE    SPA-NAI-BODYLIST-TGRP1-NO (IDXX IDX)
                                                   TO  WRK-GRP1-NO
                           MOVE    IDX             TO  WRK-SYUBETU-SRT1
                           MOVE    RECE-PUTB-SYUBETU-GAI
                                                   TO  WRK-SYUBETU-GAI
                           MOVE    RECE-PUTB-SYUBETU-NYU
                                                   TO  WRK-SYUBETU-NYU
                       END-IF
                       MOVE    WRK-SYUBETU-SRT1    TO
                                           RECE-PUTB-SYUBETU-SRT1
                       MOVE    WRK-SYUBETU-GAI     TO
                                           RECE-PUTB-SYUBETU-GAI-SRT1
                       MOVE    WRK-SYUBETU-NYU     TO
                                           RECE-PUTB-SYUBETU-NYU-SRT1
                   ELSE
                       MOVE    ZERO        TO  WRK-SYUBETU-SRT1
                       MOVE    IDX         TO  RECE-PUTB-SYUBETU-SRT1
                       MOVE    RECE-PUTB-SYUBETU-GAI
                                       TO  RECE-PUTB-SYUBETU-GAI-SRT1
                       MOVE    RECE-PUTB-SYUBETU-NYU
                                       TO  RECE-PUTB-SYUBETU-NYU-SRT1
                   END-IF  
                   MOVE    SPA-NAI-BODYLIST-TGRP1-NO (IDXX IDX)
                                           TO  RECE-PUTB-GRP-NO1
                   MOVE    IDX             TO  RECE-PUTB-SYUBETU-SRT2
                   MOVE    SPA-NAI-BODYLIST-TGRP2-NO (IDXX IDX)
                                           TO  RECE-PUTB-GRP-NO2
                   MOVE    SPA-NAI-BODYLIST-TPRTJYUN1(IDXX IDX) TO
                                           RECE-PUTB-SYUBETU-UNDER-NO1
                   MOVE    SPA-NAI-BODYLIST-TPRTJYUN2(IDXX IDX) TO
                                           RECE-PUTB-SYUBETU-UNDER-NO2 
                   MOVE    ZERO            TO  RECE-PUTB-KOUHI-NO
                   MOVE    SPACE           TO  RECE-PUTB-KOUHI-NUM 
                   MOVE    SPA-TERMID      TO  RECE-PUTB-TERMID
                   MOVE    SPA-OPID        TO  RECE-PUTB-OPID
                   MOVE    SMCNDATE-YMD    TO  RECE-PUTB-CREYMD
                   MOVE    SMCNDATE-YMD    TO  RECE-PUTB-UPYMD
                   MOVE    SMCNDATE-HMS    TO  RECE-PUTB-UPHMS
grpsys*            MOVE    SPA-HOSPNUM         TO  -HOSPNUM
                   MOVE    RECE-PUTB-REC   TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "1004"          TO  SPA-ERRCD
                   ELSE
      *                公費情報更新
                       MOVE    RECE-PUTB-REC   TO  WRK-RECE-PUTB-REC
                       MOVE    ZERO            TO  WRK-TOP-CNT
                       MOVE    500             TO  WRK-BOTTOM-CNT
      * 
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       SPA-GMN-KOHLIST-MAX
                                                           (IDXX IDX)
                               OR      SPA-ERRCD   NOT =   SPACE
                           MOVE    WRK-RECE-PUTB-REC   TO  RECE-PUTB-REC
                           MOVE    SPA-GMN-KOHLIST-THKNNUM(IDXX IDX IDY)
                                               TO  RECE-PUTB-KOUHI-NUM
                           EVALUATE    SPA-GMN-KOHLIST-TJYUN
                                                       (IDXX IDX IDY)
                               WHEN    "T"
                                   ADD     1           TO  WRK-TOP-CNT
                                   MOVE    WRK-TOP-CNT TO
                                                   RECE-PUTB-KOUHI-NO
                               WHEN    "B"
                                   ADD     1           TO
                                                   WRK-BOTTOM-CNT
                                   MOVE    WRK-BOTTOM-CNT
                                                       TO
                                                   RECE-PUTB-KOUHI-NO
                           END-EVALUATE
                           MOVE    SPA-TERMID      TO  RECE-PUTB-TERMID
                           MOVE    SPA-OPID        TO  RECE-PUTB-OPID
                           MOVE    SMCNDATE-YMD    TO  RECE-PUTB-CREYMD
                           MOVE    SMCNDATE-YMD    TO  RECE-PUTB-UPYMD
                           MOVE    SMCNDATE-HMS    TO  RECE-PUTB-UPHMS
                           MOVE    RECE-PUTB-REC   TO  MCPDATA-REC
                           MOVE    "DBINSERT"      TO  MCP-FUNC
                           MOVE    "tbl_rece_putb" TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"     USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                           IF      MCP-RC      NOT =   ZERO
                               MOVE    "1004"          TO  SPA-ERRCD
                           END-IF 
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 490-CLEAR-SEC
      *
      *        管理部一覧編集処理
               PERFORM 3101-SPA-HENSYU-SEC
           END-IF    
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       更新処理（有効終了日）
      *****************************************************************
       490-EDYUKYMD-KOUSHIN-SEC      SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    ＨＥＡＤ部更新処理
           INITIALIZE                      RECE-PUTH-REC
           MOVE    SPA-HOSPNUM         TO  RECE-PUTH-HOSPNUM
           MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTH-STYUKYMD
           MOVE    WRK-CONS-SRYYM-20080331
                                       TO  RECE-PUTH-EDYUKYMD
      *
           MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTH-UPSTYUKYMD
           MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTH-UPEDYUKYMD
           MOVE    SMCNDATE-YMD        TO  RECE-PUTH-UPYMD
           MOVE    SMCNDATE-HMS        TO  RECE-PUTH-UPHMS
           MOVE    SPA-TERMID          TO  RECE-PUTH-TERMID
           MOVE    SPA-OPID            TO  RECE-PUTH-OPID
      *
           MOVE    RECE-PUTH-REC       TO  MCPDATA-REC
           MOVE    "tbl_rece_puth"     TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               GO  TO  490-EDYUKYMD-KOUSHIN-EXT
           END-IF
      *
      *   ＢＯＤＹ部更新処理
           INITIALIZE                      RECE-PUTB-REC
           MOVE    SPA-HOSPNUM         TO  RECE-PUTB-HOSPNUM
           MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTB-STYUKYMD
           MOVE    WRK-CONS-SRYYM-20080331
                                       TO  RECE-PUTB-EDYUKYMD
      *
           MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTB-UPSTYUKYMD
           MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                       TO  RECE-PUTB-UPEDYUKYMD
           MOVE    SMCNDATE-YMD        TO  RECE-PUTB-UPYMD
           MOVE    SMCNDATE-HMS        TO  RECE-PUTB-UPHMS
           MOVE    SPA-TERMID          TO  RECE-PUTB-TERMID
           MOVE    SPA-OPID            TO  RECE-PUTB-OPID
      *
           MOVE    RECE-PUTB-REC       TO  MCPDATA-REC
           MOVE    "tbl_rece_putb"     TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  490-EDYUKYMD-KOUSHIN-EXT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 490-CLEAR-SEC
      *
      *        管理部一覧編集処理
               PERFORM 3101-SPA-HENSYU-SEC
           END-IF    
      *
           .
       490-EDYUKYMD-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W121"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-HEADLIST-ROW    =   1
               MOVE    ZERO                TO  WRK-W121-HEADLIST-ROW
           ELSE
               MOVE    W121-HEADLIST-ROW   TO  WRK-W121-HEADLIST-ROW
           END-IF
           IF      FLG-BODYLIST-ROW(1) =   1
               MOVE    ZERO                TO  WRK-W121-BODYLIST-ROW(1)
           ELSE
               MOVE    W121-BODYLIST-ROW   TO  WRK-W121-BODYLIST-ROW(1)
           END-IF
           IF      FLG-BODYLIST-ROW(2) =   1
               MOVE    ZERO                TO  WRK-W121-BODYLIST-ROW(2)
           ELSE
               MOVE    W121-BODYLIST-1-ROW TO  WRK-W121-BODYLIST-ROW(2)
           END-IF
           IF      FLG-KOHLIST-ROW (1) =   1
               MOVE    ZERO                TO  WRK-W121-KOHLIST-ROW (1)
           ELSE
               MOVE    W121-KOHLIST-ROW    TO  WRK-W121-KOHLIST-ROW (1)
           END-IF
           IF      FLG-KOHLIST-ROW (2) =   1
               MOVE    ZERO                TO  WRK-W121-KOHLIST-ROW (2)
           ELSE
               MOVE    W121-KOHLIST-1-ROW  TO  WRK-W121-KOHLIST-ROW (2)
           END-IF
      *
           INITIALIZE                      W121
      *
           MOVE    SPA-GMN-SELNUM1         TO  W121-SELNUM1
           MOVE    SPA-GMN-STYUKYMD        TO  W121-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD        TO  W121-EDYUKYMD
      *
           MOVE    SPA-GMN-PREFKBN-PRTJYUN-G
                                           TO  W121-PR-PRTJYUN
           MOVE    SPA-GMN-HKNJANUM-PRTJYUN-G
                                           TO  W121-HKN-PRTJYUN
           MOVE    SPA-GMN-HKNJANUM        TO  W121-HKNJANUM
           MOVE    SPA-GMN-HKNJANAME       TO  W121-HKNJANAME
           MOVE    SPA-GMN-HKNJANUM-PRTKBN-G
                                           TO  W121-HKNJ-PRTKBN
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PREFKBN-PRTJYUN-MAX
               MOVE    SPA-GMN-PREFKBN-PRTJYUNLST (IDY)
                                       TO  W121-PR-PRTJYUNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PREFKBN-PRTJYUN-MAX
                                       TO  W121-PR-PRTJYUN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-HKNJANUM-PRTJYUN-MAX
               MOVE    SPA-GMN-HKNJANUM-PRTJYUNLST (IDY)
                                       TO  W121-HKN-PRTJYUNLST(IDY)
           END-PERFORM
           MOVE    SPA-GMN-HKNJANUM-PRTJYUN-MAX
                                       TO  W121-HKN-PRTJYUN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-HKNJANUM-PRTKBN-MAX
               MOVE    SPA-GMN-HKNJANUM-PRTKBNLST (IDY)
                                       TO  W121-HKNJ-PRTKBNLST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-HKNJANUM-PRTKBN-MAX
                                       TO  W121-HKNJ-PRTKBN-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-PRTJYUN1-MAX
               MOVE    SPA-GMN-PRTJYUN1LST (IDY)
                                           TO  W121-PRTJYUN1LST   (IDY)
                                               W121-PRTJYUN1-1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN1-MAX
                                       TO  W121-PRTJYUN1-COUNT 
                                           W121-PRTJYUN1-1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-PRTJYUN2-MAX
               MOVE    SPA-GMN-PRTJYUN2LST (IDY)
                                           TO  W121-PRTJYUN2LST   (IDY)
                                               W121-PRTJYUN2-1LST (IDY) 
           END-PERFORM
           MOVE    SPA-GMN-PRTJYUN2-MAX
                                       TO  W121-PRTJYUN2-COUNT 
                                           W121-PRTJYUN2-1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SRT-LAST-MAX
               MOVE    SPA-GMN-SRT-LASTLST (IDY)
                                       TO  W121-SRT-LASTLST (IDY)
                                           W121-SRT-LAST-1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SRT-LAST-MAX
                                   TO  W121-SRT-LAST-COUNT
                                       W121-SRT-LAST-1-COUNT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-ZAITAKU-MAX
               MOVE    SPA-GMN-ZAITAKULST (IDY)
                                       TO  W121-ZAITAKULST (IDY)
                                           W121-ZAITAKU-1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-ZAITAKU-MAX TO  W121-ZAITAKU-COUNT
                                           W121-ZAITAKU-1-COUNT
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-CHOUKI-MAX
               MOVE    SPA-GMN-CHOUKILST (IDY)
                                       TO  W121-CHOUKILST (IDY)
                                           W121-CHOUKI-1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-CHOUKI-MAX  TO  W121-CHOUKI-COUNT
                                           W121-CHOUKI-1-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUBETU-PRTJYUN1-MAX
               MOVE    SPA-GMN-SYUBETU-PRTJYUN1LST (IDY)
                                       TO  W121-SYU-PRTJYUN1LST   (IDY)
                                           W121-SYU-PRTJYUN1-1LST (IDY)
           END-PERFORM
           IF      SPA-GMN-PRTJYUN1 (1)    NOT =   "02"
               MOVE    6               TO  W121-SYU-PRTJYUN1-COUNT
           ELSE
               MOVE    SPA-GMN-SYUBETU-PRTJYUN1-MAX
                                       TO  W121-SYU-PRTJYUN1-COUNT
           END-IF 
           IF      SPA-GMN-PRTJYUN1 (2)    NOT =   "02"
               MOVE    6               TO  W121-SYU-PRTJYUN1-1-COUNT
           ELSE
               MOVE    SPA-GMN-SYUBETU-PRTJYUN1-MAX
                                       TO  W121-SYU-PRTJYUN1-1-COUNT
           END-IF 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   SPA-GMN-SYUBETU-PRTJYUN2-MAX
               MOVE    SPA-GMN-SYUBETU-PRTJYUN2LST (IDY)
                                       TO  W121-SYU-PRTJYUN2LST   (IDY)
                                           W121-SYU-PRTJYUN2-1LST (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SYUBETU-PRTJYUN2-MAX
                                       TO  W121-SYU-PRTJYUN2-COUNT
                                           W121-SYU-PRTJYUN2-1-COUNT 
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-HEADLIST-MAX
               MOVE    SPA-GMN-HEADLIST-TNO  (IDX) 
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  W121-HEADLIST-TNO  (IDX)
               MOVE    SPA-GMN-HEADLIST-TSTYUKYMD   (IDX)
                                       TO  W121-HEADLIST-TSTYUKYMD (IDX)
               MOVE    SPA-GMN-HEADLIST-TEDYUKYMD(IDX)
                                       TO  W121-HEADLIST-TEDYUKYMD (IDX)
      *
               IF      IDX             =   SPA-GMN-SELNUM1
                   MOVE    "T"         TO  W121-HEADLIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  W121-HEADLIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-HEADLIST-MAX
                                       TO  W121-HEADLIST-COUNT
           MOVE    ZERO                TO  W121-HEADLIST-ROW
           IF      WRK-W121-HEADLIST-ROW   >   ZERO
               MOVE    WRK-W121-HEADLIST-ROW
                                           TO  W121-HEADLIST-ROW
           END-IF
      *
      *    県内設定
           MOVE    SPA-GMN-PRTJYUN1-G (1)  TO  W121-PRTJYUN1
           MOVE    SPA-GMN-PRTJYUN2-G (1)  TO  W121-PRTJYUN2
           MOVE    SPA-GMN-ZAITAKU-G  (1)  TO  W121-ZAITAKU
           MOVE    SPA-GMN-CHOUKI-G   (1)  TO  W121-CHOUKI
           MOVE    SPA-GMN-SRT-LAST-G (1)  TO  W121-SRT-LAST
           MOVE    SPA-GMN-KOUGAKU-TEN(1)  TO  W121-KOUGAKU-TEN
      *
           MOVE    SPA-GMN-RECESYUBETU(1)  TO  W121-RECESYUBETU
           MOVE    SPA-GMN-SYUBETUNAME(1)  TO  W121-SYUBETUNAME
           MOVE    SPA-GMN-HKNNUM     (1)  TO  W121-HKNNUM
           MOVE    SPA-GMN-SEIDONAME  (1)  TO  W121-SEIDONAME
           MOVE    SPA-GMN-SYUBETU-PRTJYUN1-G (1)
                                           TO  W121-SYU-PRTJYUN1
           MOVE    SPA-GMN-SYUBETU-PRTJYUN2-G (1)
                                           TO  W121-SYU-PRTJYUN2
           MOVE    SPA-GMN-GRP1       (1)  TO  W121-GRP1
           MOVE    SPA-GMN-GRP2       (1)  TO  W121-GRP2
           MOVE    SPA-GMN-GRP1-NO    (1)  TO  W121-GRP1-NO
           MOVE    SPA-GMN-GRP2-NO    (1)  TO  W121-GRP2-NO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-BODYLIST-MAX (1)
               MOVE    IDX             TO  WRK-Z9
               MOVE    WRK-Z9          TO  W121-BODYLIST-TNO   (IDX)
               MOVE    SPA-GMN-BODYLIST-TSYUBETU   (1 IDX)
                                       TO
                                       W121-BODYLIST-TRECESYUBETU (IDX)
               MOVE    SPA-GMN-BODYLIST-TSYUBETUNAME (1 IDX)
                                       TO
                                       W121-BODYLIST-TSYUBETUNAME (IDX)
               IF      SPA-NAI-BODYLIST-TGRP1-NO (1 IDX)
                                   NOT =   "000"    
                   MOVE    SPA-NAI-BODYLIST-TGRP1-NO (1 IDX)
                                           TO  W121-BODYLIST-TGRP1 (IDX)
               END-IF
               MOVE    SPA-NAI-BODYLIST-TGRP2-NO (1 IDX)
                                       TO  W121-BODYLIST-TGRP2  (IDX)
               IF    ( SPA-NAI-BODYLIST-TPRTJYUN1(1 IDX) NOT =   SPACE )
               OR    ( SPA-NAI-BODYLIST-TPRTJYUN2(1 IDX) NOT =   SPACE )
               OR    ( SPA-GMN-KOHLIST-MAX (1 IDX)           >   ZERO  )
                   MOVE    "○"        TO  W121-BODYLIST-TSETTEI(IDX)
               END-IF
      *
               IF      FLG-BODYLIST-ROW (1)    =   2
                   MOVE    "F"     TO  W121-BODYLIST-SELECT (IDX)
               ELSE
                   IF      IDX         =   SPA-NAI-SELNUM2     (1)
                                       OR  SPA-NAI-SELNUM2-ROW (1) 
                       MOVE    "T"     TO  W121-BODYLIST-SELECT (IDX)
                   ELSE
                       MOVE    "F"     TO  W121-BODYLIST-SELECT (IDX)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-BODYLIST-MAX (1)
                                       TO  W121-BODYLIST-COUNT
           MOVE    ZERO                TO  W121-BODYLIST-ROW
           IF      WRK-W121-BODYLIST-ROW (1)
                                       >   ZERO
               MOVE    WRK-W121-BODYLIST-ROW (1)
                                           TO  W121-BODYLIST-ROW
               MOVE    2                   TO  W121-BODYLIST-ROWATTR
           END-IF
      *
           IF    ( FLG-BODYLIST-ROW(1) NOT =   2    )
           AND   ( SPA-NAI-SELNUM2 (1)     >   ZERO )
               MOVE    SPA-NAI-SELNUM2 (1)
                                           TO  IDYY
               MOVE    SPA-GMN-KOHLIST-MAX (1 IDYY)
                                           TO  IDZZ
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       IDZZ
                   MOVE    IDX             TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  W121-KOHLIST-TNO    (IDX)
                   MOVE    SPA-GMN-KOHLIST-THKNNUM    (1 IDYY IDX)
                                       TO  W121-KOHLIST-THKNNUM    (IDX)
                   MOVE    SPA-GMN-KOHLIST-TSEIDONAME (1 IDYY IDX)
                                       TO  W121-KOHLIST-TSEIDONAME (IDX)
                   EVALUATE    SPA-GMN-KOHLIST-TJYUN  (1 IDYY IDX)
                       WHEN    "T"
                           MOVE    "先頭"  TO  W121-KOHLIST-TJYUN  (IDX)
                       WHEN    "B"
                           MOVE    "後尾"  TO  W121-KOHLIST-TJYUN  (IDX)
                   END-EVALUATE
      *
                   IF      IDX         =   SPA-NAI-SELNUM3 (1)
                       MOVE    "T"         TO  W121-KOHLIST-SELECT (IDX)
                   ELSE
                       MOVE    "F"         TO  W121-KOHLIST-SELECT (IDX)
                   END-IF
               END-PERFORM
               MOVE    IDZZ            TO  W121-KOHLIST-COUNT
               MOVE    ZERO            TO  W121-KOHLIST-ROW
               IF      WRK-W121-KOHLIST-ROW (1)
                                       >   ZERO
                   MOVE    WRK-W121-KOHLIST-ROW (1)
                                              TO  W121-KOHLIST-ROW
               END-IF
           END-IF 
      *
           EVALUATE    SPA-GMN-GRP-SETTEI(1)
                                           ALSO    SPA-GMN-GRP-KAIJYO(1)
               WHEN    "1"                 ALSO    SPACE
                   MOVE    WIDGET-NORMAL       TO  W121-GRP1-STATE
                                                   W121-GRP1-NO-STATE
                                                   W121-GRP2-STATE
                                                   W121-GRP2-NO-STATE
               WHEN    SPACE               ALSO    "1"
                   MOVE    WIDGET-NORMAL       TO  W121-GRP1-STATE
                                                   W121-GRP2-STATE
                   MOVE    WIDGET-INSENSITIVE  TO  W121-GRP1-NO-STATE
                                                   W121-GRP2-NO-STATE
               WHEN    OTHER
                   MOVE    WIDGET-INSENSITIVE  TO  W121-GRP1-STATE
                                                   W121-GRP1-NO-STATE
                                                   W121-GRP2-STATE
                                                   W121-GRP2-NO-STATE
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-GRP-SETTEI (1)
               WHEN    SPACE
                   MOVE    "グループ設定"      TO  W121-GRP-SETTEI
               WHEN    "1"
                   MOVE    "グループ終了"      TO  W121-GRP-SETTEI
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-GRP-KAIJYO (1)
               WHEN    SPACE
                   MOVE    "グループ解除"      TO  W121-GRP-KAIJYO
               WHEN    "1"
                   MOVE    "解除設定終了"      TO  W121-GRP-KAIJYO
           END-EVALUATE
      *
           IF      SPA-GMN-GRP-SETTEI  (1) =   "1"
               MOVE    "グループ設定中"    TO  W121-GRP-DSP
           END-IF
           IF      SPA-GMN-GRP-KAIJYO  (1) =   "1"
               MOVE    "グループ解除中"    TO  W121-GRP-DSP
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  W121-HKNJ-PRTKBN-STATE
                                           W121-HKNJANUM-STATE
                                           W121-HKN-PRTJYUN-STATE
           MOVE    WIDGET-INSENSITIVE  TO  W121-SYU-PRTJYUN2-STATE
           EVALUATE    SPA-GMN-PRTJYUN1 (1)
               WHEN    "01"
                   MOVE    WIDGET-INSENSITIVE
                                           TO  W121-GRP2-STATE
                                               W121-GRP2-NO-STATE
               WHEN    "02"
                   MOVE    WIDGET-INSENSITIVE
                                           TO  W121-HKNJ-PRTKBN-STATE
                                               W121-HKNJANUM-STATE
                                               W121-HKN-PRTJYUN-STATE
                   MOVE    WIDGET-NORMAL   TO  W121-SYU-PRTJYUN2-STATE
           END-EVALUATE
      *
           IF    ( SPA-GMN-GRP-SETTEI  (1) =   SPACE )
           AND   ( SPA-GMN-GRP-KAIJYO  (1) =   SPACE )
               MOVE    WIDGET-NORMAL       TO  W121-SYU-PRTJYUN1-STATE
                                               W121-SYU-PRTJYUN2-STATE
                                               W121-HKNNUM-STATE
                                               W121-SYUBETU-UP-STATE
                                               W121-SYUBETU-DOWN-STATE
                                               W121-KOH-UP-STATE
                                               W121-KOH-DOWN-STATE
                                               W121-TOP-STATE
                                               W121-BOTTOM-STATE
                                               W121-DEL-STATE
                                               W121-SCR4-STATE
               IF      SPA-GMN-PRTJYUN1 (1)    =   "02"
                   CONTINUE
               ELSE 
                   MOVE    WIDGET-INSENSITIVE  TO
                                               W121-SYU-PRTJYUN2-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  W121-SYU-PRTJYUN1-STATE
                                               W121-SYU-PRTJYUN2-STATE
                                               W121-HKNNUM-STATE
                                               W121-SYUBETU-UP-STATE
                                               W121-SYUBETU-DOWN-STATE
                                               W121-KOH-UP-STATE
                                               W121-KOH-DOWN-STATE
                                               W121-TOP-STATE
                                               W121-BOTTOM-STATE
                                               W121-DEL-STATE
                                               W121-SCR4-STATE
           END-IF
      *
      *    県外設定
           MOVE    SPA-GMN-PRTJYUN1-G (2)  TO  W121-PRTJYUN1-1
           MOVE    SPA-GMN-PRTJYUN2-G (2)  TO  W121-PRTJYUN2-1
           MOVE    SPA-GMN-ZAITAKU-G  (2)  TO  W121-ZAITAKU-1
           MOVE    SPA-GMN-CHOUKI-G   (2)  TO  W121-CHOUKI-1
           MOVE    SPA-GMN-SRT-LAST-G (2)  TO  W121-SRT-LAST-1
           MOVE    SPA-GMN-KOUGAKU-TEN(2)  TO  W121-KOUGAKU-TEN-1
      *
           MOVE    SPA-GMN-RECESYUBETU(2)  TO  W121-RECESYUBETU-1
           MOVE    SPA-GMN-SYUBETUNAME(2)  TO  W121-SYUBETUNAME-1
           MOVE    SPA-GMN-HKNNUM     (2)  TO  W121-HKNNUM-1
           MOVE    SPA-GMN-SEIDONAME  (2)  TO  W121-SEIDONAME-1
           MOVE    SPA-GMN-SYUBETU-PRTJYUN1-G (2)
                                           TO  W121-SYU-PRTJYUN1-1
           MOVE    SPA-GMN-SYUBETU-PRTJYUN2-G (2)
                                           TO  W121-SYU-PRTJYUN2-1
           MOVE    SPA-GMN-GRP1       (2)  TO  W121-GRP1-1
           MOVE    SPA-GMN-GRP2       (2)  TO  W121-GRP2-1
           MOVE    SPA-GMN-GRP1-NO    (2)  TO  W121-GRP1-NO-1
           MOVE    SPA-GMN-GRP2-NO    (2)  TO  W121-GRP2-NO-1
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-BODYLIST-MAX (2)
               MOVE    IDX             TO  WRK-Z9
               MOVE    WRK-Z9          TO  W121-BODYLIST-1-TNO   (IDX)
               MOVE    SPA-GMN-BODYLIST-TSYUBETU   (2 IDX)
                                       TO
                                       W121-BODYLIST-1-TRECESYUBETU(IDX)
               MOVE    SPA-GMN-BODYLIST-TSYUBETUNAME (2 IDX)
                                       TO
                                       W121-BODYLIST-1-TSYUBETUNAME(IDX)
               IF      SPA-NAI-BODYLIST-TGRP1-NO (2 IDX)
                                   NOT =   "000"    
                   MOVE    SPA-NAI-BODYLIST-TGRP1-NO (2 IDX)
                                       TO  W121-BODYLIST-1-TGRP1 (IDX)
               END-IF
               MOVE    SPA-NAI-BODYLIST-TGRP2-NO (2 IDX)
                                       TO  W121-BODYLIST-1-TGRP2(IDX)
               IF    ( SPA-NAI-BODYLIST-TPRTJYUN1(2 IDX) NOT =   SPACE )
               OR    ( SPA-NAI-BODYLIST-TPRTJYUN2(2 IDX) NOT =   SPACE )
               OR    ( SPA-GMN-KOHLIST-MAX (2 IDX)           >   ZERO  )
                   MOVE    "○"        TO  W121-BODYLIST-1-TSETTEI(IDX)
               END-IF
      *
               IF      FLG-BODYLIST-ROW (2)    =   2
                   MOVE    "F"     TO  W121-BODYLIST-1-SELECT (IDX)
               ELSE
                   IF      IDX         =   SPA-NAI-SELNUM2     (2)
                                       OR  SPA-NAI-SELNUM2-ROW (2) 
                       MOVE    "T"     TO  W121-BODYLIST-1-SELECT (IDX)
                   ELSE
                       MOVE    "F"     TO  W121-BODYLIST-1-SELECT (IDX)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-BODYLIST-MAX (2)
                                       TO  W121-BODYLIST-1-COUNT
           MOVE    ZERO                TO  W121-BODYLIST-1-ROW
           IF      WRK-W121-BODYLIST-ROW (2)
                                       >   ZERO
               MOVE    WRK-W121-BODYLIST-ROW (2)
                                           TO  W121-BODYLIST-1-ROW
               MOVE    2                   TO  W121-BODYLIST-1-ROWATTR
           END-IF
      *
           IF    ( FLG-BODYLIST-ROW(2) NOT =   2    )
           AND   ( SPA-NAI-SELNUM2 (2)     >   ZERO )
               MOVE    SPA-NAI-SELNUM2 (2)
                                           TO  IDYY
               MOVE    SPA-GMN-KOHLIST-MAX (2 IDYY)
                                           TO  IDZZ
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       IDZZ
                   MOVE    IDX             TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  W121-KOHLIST-1-TNO  (IDX)
                   MOVE    SPA-GMN-KOHLIST-THKNNUM    (2 IDYY IDX)
                                       TO  W121-KOHLIST-1-THKNNUM  (IDX)
                   MOVE    SPA-GMN-KOHLIST-TSEIDONAME (2 IDYY IDX)
                                       TO  W121-KOHLIST-1-TSEIDONAME
                                                                  (IDX)
                   EVALUATE    SPA-GMN-KOHLIST-TJYUN  (2 IDYY IDX)
                       WHEN    "T"
                           MOVE    "先頭"  TO  W121-KOHLIST-1-TJYUN(IDX)
                       WHEN    "B"
                           MOVE    "後尾"  TO  W121-KOHLIST-1-TJYUN(IDX)
                   END-EVALUATE
      *
                   IF      IDX         =   SPA-NAI-SELNUM3 (2)
                       MOVE    "T"     TO  W121-KOHLIST-1-SELECT (IDX)
                   ELSE
                       MOVE    "F"     TO  W121-KOHLIST-1-SELECT (IDX)
                   END-IF
               END-PERFORM
               MOVE    IDZZ            TO  W121-KOHLIST-1-COUNT
               MOVE    ZERO            TO  W121-KOHLIST-1-ROW
               IF      WRK-W121-KOHLIST-ROW (2)
                                       >   ZERO
                   MOVE    WRK-W121-KOHLIST-ROW (2)
                                              TO  W121-KOHLIST-1-ROW
               END-IF
           END-IF 
      *
           EVALUATE    SPA-GMN-GRP-SETTEI(2)
                                           ALSO    SPA-GMN-GRP-KAIJYO(2)
               WHEN    "1"                 ALSO    SPACE
                   MOVE    WIDGET-NORMAL       TO  W121-GRP1-1-STATE
                                                   W121-GRP1-NO-1-STATE
                                                   W121-GRP2-1-STATE
                                                   W121-GRP2-NO-1-STATE
               WHEN    SPACE               ALSO    "1"
                   MOVE    WIDGET-NORMAL       TO  W121-GRP1-1-STATE
                                                   W121-GRP2-1-STATE
                   MOVE    WIDGET-INSENSITIVE  TO  W121-GRP1-NO-1-STATE
                                                   W121-GRP2-NO-1-STATE
               WHEN    OTHER
                   MOVE    WIDGET-INSENSITIVE  TO  W121-GRP1-1-STATE
                                                   W121-GRP1-NO-1-STATE
                                                   W121-GRP2-1-STATE
                                                   W121-GRP2-NO-1-STATE
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-GRP-SETTEI (2)
               WHEN    SPACE
                   MOVE    "グループ設定"      TO  W121-GRP-SETTEI-1
               WHEN    "1"
                   MOVE    "グループ終了"      TO  W121-GRP-SETTEI-1
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-GRP-KAIJYO (2)
               WHEN    SPACE
                   MOVE    "グループ解除"      TO  W121-GRP-KAIJYO-1
               WHEN    "1"
                   MOVE    "解除設定終了"      TO  W121-GRP-KAIJYO-1
           END-EVALUATE
      *
           IF      SPA-GMN-GRP-SETTEI  (2) =   "1"
               MOVE    "グループ設定中"    TO  W121-GRP-DSP-1
           END-IF
           IF      SPA-GMN-GRP-KAIJYO  (2) =   "1"
               MOVE    "グループ解除中"    TO  W121-GRP-DSP-1
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE  TO  W121-SYU-PRTJYUN2-1-STATE
           EVALUATE    SPA-GMN-PRTJYUN1 (2)
               WHEN    "01"
                   MOVE    WIDGET-INSENSITIVE
                                           TO  W121-GRP2-1-STATE
                                               W121-GRP2-NO-1-STATE
               WHEN    "02"
                   MOVE    WIDGET-NORMAL   TO  W121-SYU-PRTJYUN2-1-STATE
           END-EVALUATE
      *
           IF    ( SPA-GMN-GRP-SETTEI  (2) =   SPACE )
           AND   ( SPA-GMN-GRP-KAIJYO  (2) =   SPACE )
               MOVE    WIDGET-NORMAL       TO  W121-SYU-PRTJYUN1-1-STATE
                                               W121-SYU-PRTJYUN2-1-STATE
                                               W121-HKNNUM-1-STATE
                                               W121-SYUBETU-UP-1-STATE
                                               W121-SYUBETU-DOWN-1-STATE
                                               W121-KOH-UP-1-STATE
                                               W121-KOH-DOWN-1-STATE
                                               W121-TOP-1-STATE
                                               W121-BOTTOM-1-STATE
                                               W121-DEL-1-STATE
                                               W121-SCR4-1-STATE
               IF      SPA-GMN-PRTJYUN1 (2)    =   "02"
                   CONTINUE
               ELSE 
                   MOVE    WIDGET-INSENSITIVE  TO
                                               W121-SYU-PRTJYUN2-1-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  W121-SYU-PRTJYUN1-1-STATE
                                               W121-SYU-PRTJYUN2-1-STATE
                                               W121-HKNNUM-1-STATE
                                               W121-SYUBETU-UP-1-STATE
                                               W121-SYUBETU-DOWN-1-STATE
                                               W121-KOH-UP-1-STATE
                                               W121-KOH-DOWN-STATE
                                               W121-TOP-1-STATE
                                               W121-BOTTOM-1-STATE
                                               W121-DEL-1-STATE
                                               W121-SCR4-1-STATE
           END-IF
      *
           MOVE    "blue"          TO  W121-RECESYUBETU-STYLE
                                       W121-SYUBETUNAME-STYLE
                                       W121-GRP-DSP-STYLE
                                       W121-RECESYUBETU-1-STYLE
                                       W121-SYUBETUNAME-1-STYLE
                                       W121-GRP-DSP-1-STYLE
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません"     TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "移動先の設定内容と異なります"
                                                      TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "エラーがある項目があります"
                                                      TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "有効期間が重複します"     TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "日付入力エラー"           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "１日で入力して下さい"     TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "末日で入力して下さい"     TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "有効開始日＜有効終了日で入力して下さい"
                                                      TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "第一印刷順入力エラー"     TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "一般国保設定入力エラー"   TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "グループ１番号が未入力です"
                                                      TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "グループ２番号が未入力です"
                                                      TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "最終印刷順入力エラー"     TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "印刷順指定・県内県外入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "印刷順指定・保険者入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "印刷順指定・レセプト種別入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "在総診入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "長期（長期高額療養費）入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "所在地保険者番号入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "所在地保険者印刷位置入力エラー"
                                                         TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "所在地保険者は県内の保険者を入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "種別内設定１入力エラー"      TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE
                       "第一印刷順がレセプト種別のときのみ選択できます"
                                                      TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "種別内設定２入力エラー"      TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE
                       "第一印刷順がレセプト種別のときのみ選択できます"
                                                      TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "レセプト請求する地方公費のみ入力できます"
                                                      TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "保険番号入力エラー"       TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "グループ１が選択してありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "数字のみ入力できます"     TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "グループ２が選択してありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "グループが異なるので移動はできません"
                                                      TO  SPA-ERRMSG
               WHEN    "0040"
                   MOVE
                   "公費の並び順設定が異なるので移動はできません"
                                                      TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "公費が未入力です"         TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "全体の長期の設定がしてあります"
                                                      TO  SPA-ERRMSG
               WHEN    "0043"
                   MOVE    "全体の在総診・在医総の設定がしてあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0044"
                   MOVE    "該当の公費は登録してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0045"
                   MOVE
                  "第一印刷順がレセプト種別のとき保険者は入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0046"
                   MOVE
                  "第一印刷順が保険者のときグループ２の設定はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE
            "第一印刷順がレセプト種別のとき保険者印刷順は入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "県内のみ選択可能な区分です"
                                                       TO  SPA-ERRMSG
               WHEN    "0049"
                   MOVE
                       "改正の月を跨いでの有効期間の設定はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0098"
                   STRING
                       "医療機関の所在地保険者印刷位置を選択して"
                                               DELIMITED  BY  SIZE
                       "あるのに所在地保険者が未入力です"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                                       TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE
                       "編綴順詳細設定（ＨＥＡＤ）がありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "種別一覧が表示してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "処理対象の種別が選択してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "処理対象の公費が選択してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0105"
                   MOVE    "これ以上公費の登録ができません"
                                               TO  SPA-ERRMSG
      *
               WHEN    "1001"
                   MOVE
                   "編綴順詳細設定（ＨＥＡＤ）の追加ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE
                   "編綴順詳細設定（ＨＥＡＤ）の更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE
                   "編綴順詳細設定（ＨＥＡＤ）の削除ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE
                   "編綴順詳細設定（ＢＯＤＹ）の更新ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W121"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "編綴詳細設定情報を登録します"
                                                TO  WID1-ID1MSG
                   MOVE    "B12"                TO  MCP-WIDGET
               WHEN    "1002"
                   MOVE    "入力内容をクリアします。よろしいですか。"
                                                TO  WID1-ID1MSG
                   MOVE    "B01"                TO  MCP-WIDGET
               WHEN    "1003"
                   STRING  "期限を平成２０年３月３１日に変更します。"
                                                DELIMITED  BY  SIZE
                           "よろしいですか。"
                                                DELIMITED  BY  SIZE
                                                INTO  WID1-ID1MSG
                   END-STRING
                   MOVE    "B12"                TO  MCP-WIDGET
               WHEN    "1004"
                   STRING  "期限が平成２０年４月を跨いでい"
                                                DELIMITED  BY  SIZE
                   "るデータがあります。処理を終了してよろしいですか。"
                                                DELIMITED  BY  SIZE
                                                INTO  WID1-ID1MSG
                   END-STRING
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W121"               TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "SELNUM1"        TO  MCP-WIDGET
                WHEN    2
                    MOVE  "STYUKYMD"       TO  MCP-WIDGET
                WHEN    3
                    MOVE  "EDYUKYMD"       TO  MCP-WIDGET
                WHEN    4
                    MOVE  "PR_PRTJYUN"     TO  MCP-WIDGET
                WHEN    5
                    MOVE  "HKN_PRTJYUN"    TO  MCP-WIDGET
                WHEN    6
                    MOVE  "HKNJANUM"       TO  MCP-WIDGET
                WHEN    7
                    MOVE  "HKNJ_PRTKBN"    TO  MCP-WIDGET
      *         県内
                WHEN    8
                    MOVE  "PRTJYUN1"       TO  MCP-WIDGET
                WHEN    9
                    MOVE  "PRTJYUN2"       TO  MCP-WIDGET
                WHEN    10
                    MOVE  "ZAITAKU"        TO  MCP-WIDGET
                WHEN    11
                    MOVE  "CHOUKI"         TO  MCP-WIDGET
                WHEN    13
                    MOVE  "SYU_PRTJYUN1"   TO  MCP-WIDGET
                WHEN    14
                    MOVE  "SYU_PRTJYUN2"   TO  MCP-WIDGET
                WHEN    15
                    MOVE  "HKNNUM"         TO  MCP-WIDGET
                WHEN    16
                    MOVE  "SRT_LAST"       TO  MCP-WIDGET
                WHEN    17
                    MOVE  "KOUGAKU_TEN"    TO  MCP-WIDGET
                WHEN    50
                    MOVE  "GRP1_NO"        TO  MCP-WIDGET
                WHEN    51
                    MOVE  "GRP2_NO"        TO  MCP-WIDGET
                WHEN    100
                    MOVE  "SYUBETU_UP"     TO  MCP-WIDGET
                WHEN    101
                    MOVE  "KOH_UP"         TO  MCP-WIDGET
      *         県外
                WHEN    208
                    MOVE  "PRTJYUN1_1"     TO  MCP-WIDGET
                WHEN    209
                    MOVE  "PRTJYUN2_1"     TO  MCP-WIDGET
                WHEN    210
                    MOVE  "ZAITAKU_1"      TO  MCP-WIDGET
                WHEN    211
                    MOVE  "CHOUKI_1"       TO  MCP-WIDGET
                WHEN    213
                    MOVE  "SYU_PRTJYUN1_1" TO  MCP-WIDGET
                WHEN    214
                    MOVE  "SYU_PRTJYUN2_1" TO  MCP-WIDGET
                WHEN    215
                    MOVE  "HKNNUM_1"       TO  MCP-WIDGET
                WHEN    216
                    MOVE  "SRT_LAST_1"     TO  MCP-WIDGET
                WHEN    217
                    MOVE  "KOUGAKU_TEN_1"  TO  MCP-WIDGET
                WHEN    250
                    MOVE  "GRP1_NO_1"      TO  MCP-WIDGET
                WHEN    251
                    MOVE  "GRP2_NO_1"      TO  MCP-WIDGET
                WHEN    300
                    MOVE  "SYUBETU_UP_1"   TO  MCP-WIDGET
                WHEN    301
                    MOVE  "KOH_UP_1"       TO  MCP-WIDGET
      *
                WHEN    999
                    MOVE  "B12"            TO  MCP-WIDGET
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-CUR
                WHEN    8   THRU    199
                    MOVE    0              TO  W121-PAGENO
                WHEN    208 THRU    399
                    MOVE    1              TO  W121-PAGENO
                WHEN    OTHER
                    MOVE    SPA-W121-PAGENO
                                           TO  W121-PAGENO
            END-EVALUATE
      *
           MOVE    W121-PAGENO             TO  SPA-W121-PAGENO
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM1"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "PR_PRTJYUN"
                   IF      SPA-GMN-PRTJYUN1 (1)    =   "02"
                       IF      W121-PAGENO         =   ZERO 
                           MOVE    8          TO  SPA-GMN-CUR
                       ELSE
                           MOVE    208        TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    5          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "HKN_PRTJYUN"
                   IF      SPA-GMN-PRTJYUN1 (1)    =   "02"
                       IF      W121-PAGENO         =   ZERO 
                           MOVE    8          TO  SPA-GMN-CUR
                       ELSE
                           MOVE    208        TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    6          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "HKNJANUM"
                   IF      SPA-GMN-HKNJANUM    NOT =   SPACE
                       MOVE    7           TO  SPA-GMN-CUR
                   ELSE
                       IF      W121-PAGENO         =   ZERO 
                           MOVE    8           TO  SPA-GMN-CUR
                       ELSE
                           MOVE    208         TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               WHEN    "HKNJ_PRTKBN"
                   IF      W121-PAGENO         =   ZERO 
                       MOVE    8           TO  SPA-GMN-CUR
                   ELSE
                       MOVE    208         TO  SPA-GMN-CUR
                   END-IF
      * 
               WHEN    "PRTJYUN1"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "PRTJYUN2"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "SRT_LAST"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "ZAITAKU"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "CHOUKI"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "KOUGAKU_TEN"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "SYU_PRTJYUN1"
                   IF      SPA-GMN-PRTJYUN1 (1)    =   "02"
                       MOVE    14          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    15          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SYU_PRTJYUN2"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "GRP1_NO"
               WHEN    "GRP2_NO"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "HKNNUM"
                   MOVE    15          TO  SPA-GMN-CUR
      * 
               WHEN    "PRTJYUN1_1"
                   MOVE    209           TO  SPA-GMN-CUR
               WHEN    "PRTJYUN2_1"
                   MOVE    216          TO  SPA-GMN-CUR
               WHEN    "SRT_LAST_1"
                   MOVE    210          TO  SPA-GMN-CUR
               WHEN    "ZAITAKU_1"
                   MOVE    211          TO  SPA-GMN-CUR
               WHEN    "CHOUKI_1"
                   MOVE    217          TO  SPA-GMN-CUR
               WHEN    "KOUGAKU_TEN_1"
                   MOVE    213          TO  SPA-GMN-CUR
               WHEN    "SYU_PRTJYUN1_1"
                   IF      SPA-GMN-PRTJYUN1 (2)    =   "02"
                       MOVE    214          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    215          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SYU_PRTJYUN2_1"
                   MOVE    215          TO  SPA-GMN-CUR
               WHEN    "GRP1_NO_1"
               WHEN    "GRP2_NO_1"
                   MOVE    213          TO  SPA-GMN-CUR
               WHEN    "HKNNUM_1"
                   MOVE    215          TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
           MOVE    ZERO            TO  WRK-SYMD
      *
           IF    ( WRK-YMD         =   "00000000"
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-YMD             TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC              =   ZERO )
                   MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY          TO  WRK-SYMD
               END-IF
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
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
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           PERFORM     UNTIL (     FLG-HKNNUM    =  1                  )
                        OR   (    (HKN-TEKSTYMD  <= SPA-NAI-EDYUKYMD )
                              AND (HKN-TEKEDYMD  >= SPA-NAI-STYUKYMD ) )
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-N-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNNUM
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＨＥＡＤレコード読込
      *****************************************************************
       900-RECE-PUTH-READ-SEC        SECTION.
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_rece_puth" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-RECE-PUTH-READ-N-SEC
           ELSE
               INITIALIZE                  RECE-PUTH-REC
               MOVE    1               TO  FLG-RECE-PUTH
           END-IF
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE-PUTH-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＨＥＡＤレコード読込
      *****************************************************************
       900-RECE-PUTH-READ-N-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE-PUTH
               MOVE    MCPDATA-REC         TO  RECE-PUTH-REC
           ELSE
               MOVE    1                   TO  FLG-RECE-PUTH
           END-IF
      *
           .
       900-RECE-PUTH-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＢＯＤＹレコード読込
      *****************************************************************
       900-RECE-PUTB-READ-N-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE-PUTB
               MOVE    MCPDATA-REC         TO  RECE-PUTB-REC
           ELSE
               MOVE    1                   TO  FLG-RECE-PUTB
           END-IF
      *
           .
       900-RECE-PUTB-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
