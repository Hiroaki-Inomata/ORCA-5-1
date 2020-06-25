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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGX04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ情報設定（Ｘ０４）
      *  管理者            : 
      *  04/10/20    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
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
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    チェックマスタ情報用画面スパ
           COPY    "X01SCR-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-X04.
           03  SPA-X04-AREA.
               05  SPA-GMN-CUR                 PIC 9(03).
               05  SPA-GMN-BYOLST-PAGE         PIC 9(05).
               05  SPA-GMN-SRCBYOLST-PAGE      PIC 9(05).
               05  SPA-GMN-SRCBYOLST-PTN       PIC X(01).
               05  SPA-GMN-SRCBYOLST-SRT       PIC X(03).
               05  SPA-GMN-HEAD-AREA.
                   07  SPA-GMN-STYUKYMD        PIC X(09).
                   07  SPA-GMN-EDYUKYMD        PIC X(09).
                   07  SPA-GMN-CHKKBN          PIC X(50).
                   07  SPA-GMN-SRYCD           PIC X(09).
                   07  SPA-GMN-NAME            PIC X(100).
               05  SPA-GMN-NYUGAIKBN-G.
                   07  SPA-GMN-NYUGAIKBN       PIC X(01).
                   07  SPA-GMN-NYUGAIKBNFL     PIC X(01).
                   07  SPA-GMN-NYUGAIKBNMEI    PIC X(20).
               05  SPA-GMN-TEKBYOKBN-G.
                   07  SPA-GMN-TEKBYOKBN       PIC X(01).
                   07  SPA-GMN-TEKBYOKBNFL     PIC X(01).
                   07  SPA-GMN-TEKBYOKBNMEI    PIC X(98).
               05  SPA-GMN-BYO-INPUT-AREA.
                   07  SPA-GMN-BYOMEI-CHK-FLG  PIC 9(1).
                   07  SPA-GMN-BYOMEICD-CHK-FLG    PIC 9(1).
                   07  SPA-GMN-SELNUM          PIC X(50).
                   07  SPA-GMN-BYOSTATUS       PIC X(1).
                   07  SPA-GMN-BYOCD-G.
                       09  SPA-GMN-BYOCD       PIC X(20)
                                               OCCURS  6.
                   07  SPA-GMN-BYOMEI          PIC X(400).
                   07  SPA-GMN-JYKNKBN-G.
                       09  SPA-GMN-JYKNKBN     PIC X(01).
                       09  SPA-GMN-JYKNKBNFL   PIC X(01).
                       09  SPA-GMN-JYKNKBNMEI  PIC X(10).
                   07  SPA-GMN-HOSOKU          PIC X(40).
                   07  SPA-GMN-HOSOKU-JYKNKBN-G.
                       09  SPA-GMN-HOSOKU-JYKNKBN
                                               PIC X(01).
                       09  SPA-GMN-HOSOKU-JYKNKBNFL
                                               PIC X(01).
                       09  SPA-GMN-HOSOKU-JYKNKBNMEI
                                               PIC X(10).
                   07  SPA-GMN-UTAGAIKBN-G.
                       09  SPA-GMN-UTAGAIKBN     PIC X(01).
                       09  SPA-GMN-UTAGAIKBNFL   PIC X(01).
                       09  SPA-GMN-UTAGAIKBNMEI  PIC X(78).
               05  SPA-GMN-SRCBYO-INPUT-AREA.
                   07  SPA-GMN-SEL-BYOMEI      PIC X(100).
                   07  SPA-GMN-SRCBYOKBN-G.
                       09  SPA-GMN-SRCBYOKBN   PIC X(01).
                       09  SPA-GMN-SRCBYOKBNFL PIC X(01).
                       09  SPA-GMN-SRCBYOKBNMEI
                                               PIC X(20).
                   07  SPA-GMN-SRTKBN-G.
                       09  SPA-GMN-SRTKBN      PIC X(03).
                       09  SPA-GMN-SRTKBNMEI   PIC X(20).
                   07  SPA-GMN-SRCJYKNKBN-G.
                       09  SPA-GMN-SRCJYKNKBN  PIC X(01).
                       09  SPA-GMN-SRCJYKNKBNFL
                                               PIC X(01).
                       09  SPA-GMN-SRCJYKNKBNMEI
                                               PIC X(10).
                   07  SPA-GMN-SRCUTAGAIKBN-G.
                       09  SPA-GMN-SRCUTAGAIKBN  PIC X(01).
                       09  SPA-GMN-SRCUTAGAIKBNFL
                                               PIC X(01).
                       09  SPA-GMN-SRCUTAGAIKBNMEI
                                               PIC X(30).
                   07  SPA-GMN-GRPID-G.
                       09  SPA-GMN-GRPID     PIC X(01).
                       09  SPA-GMN-GRPIDFL   PIC X(01).
                       09  SPA-GMN-GRPIDMEI  PIC X(06).
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-NYUGAIKBNLST-G.
                       09  SPA-GMN-NYUGAIKBN-MAX   PIC 9(03).
                       09  SPA-GMN-NYUGAIKBNLST    OCCURS    3.
                           11  SPA-GMN-NYUGAIKBNL  PIC X(01).
                           11  SPA-GMN-NYUGAIKBNFLL
                                                   PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEIL
                                                   PIC X(20).
                   07  SPA-GMN-TEKBYOKBNLST-G.
                       09  SPA-GMN-TEKBYOKBN-MAX   PIC 9(03).
                       09  SPA-GMN-TEKBYOKBNLST    OCCURS    5.
                           11  SPA-GMN-TEKBYOKBNL  PIC X(01).
                           11  SPA-GMN-TEKBYOKBNFLL
                                                   PIC X(01).
                           11  SPA-GMN-TEKBYOKBNMEIL
                                                   PIC X(98).
                   07  SPA-GMN-JYKNKBNLST-G.
                       09  SPA-GMN-JYKNKBN-MAX     PIC 9(03).
                       09  SPA-GMN-JYKNKBNLST      OCCURS    5.
                           11  SPA-GMN-JYKNKBNL    PIC X(01).
                           11  SPA-GMN-JYKNKBNFLL  PIC X(01).
                           11  SPA-GMN-JYKNKBNMEIL PIC X(10).
                   07  SPA-GMN-UTAGAIKBNLST-G.
                       09  SPA-GMN-UTAGAIKBN-MAX     PIC 9(03).
                       09  SPA-GMN-UTAGAIKBNLST      OCCURS    5.
                           11  SPA-GMN-UTAGAIKBNL    PIC X(01).
                           11  SPA-GMN-UTAGAIKBNFLL  PIC X(01).
                           11  SPA-GMN-UTAGAIKBNMEIL PIC X(78).
                   07  SPA-GMN-SRTKBNLST-G.
                       09  SPA-GMN-SRTKBN-MAX      PIC 9(03).
                       09  SPA-GMN-SRTKBNLST       OCCURS    50.
                           11  SPA-GMN-SRTKBNL     PIC X(03).
                           11  SPA-GMN-SRTKBNMEIL  PIC X(20).
                   07  SPA-GMN-SRCBYOKBNLST-G.
                       09  SPA-GMN-SRCBYOKBN-MAX   PIC 9(03).
                       09  SPA-GMN-SRCBYOKBNLST    OCCURS    5.
                           11  SPA-GMN-SRCBYOKBNL  PIC X(01).
                           11  SPA-GMN-SRCBYOKBNFLL PIC X(01).
                           11  SPA-GMN-SRCBYOKBNMEIL
                                                   PIC X(20).
                   07  SPA-GMN-GRPIDLST-G.
                       09  SPA-GMN-GRPID-MAX     PIC 9(03).
                       09  SPA-GMN-GRPIDLST      OCCURS    6.
                           11  SPA-GMN-GRPIDL    PIC X(01).
                           11  SPA-GMN-GRPIDFLL  PIC X(01).
                           11  SPA-GMN-GRPIDMEIL PIC X(08).
               05  SPA-GMN-BYOLST-AREA.
                   07  SPA-GMN-BYOLST-NEXT         PIC 9(01).
                   07  SPA-GMN-BYOLST-MAX          PIC 9(05).
                   07  SPA-GMN-BYOLST-SEL          PIC 9(05).
                   07  SPA-GMN-BYOLST-OCC          OCCURS  401.
                       09  SPA-GMN-TNUM            PIC 9(05).
                       09  SPA-GMN-TBYOMEI         PIC X(100).
                       09  SPA-GMN-TJYKN           PIC X(01).
                       09  SPA-GMN-TUTAGAI         PIC X(01).
                       09  SPA-GMN-TSRTKEY.
                           11  SPA-GMN-TGRPID    PIC X(01).
                           11  SPA-GMN-TGROUPCHG   PIC 9(05).
                       09  SPA-GMN-THOSOKU         PIC X(40).
                       09  SPA-GMN-THOSOKU-JYKN    PIC X(01).
               05  SPA-GMN-SRCBYOLST-AREA.
                   07  SPA-GMN-SRCBYOLST-NEXT      PIC 9(01).
                   07  SPA-GMN-SRCBYOLST-MAX       PIC 9(05).
                   07  SPA-GMN-SRCBYOLST-OCC       OCCURS  100.
                       09  SPA-GMN-TSRCBYOMEI      PIC X(100).
                       09  SPA-GMN-TSRCHOSOKU      PIC X(40).
                       09  SPA-GMN-TSRCCD          PIC X(12).
                   07  SPA-GMN-SRCBYOLST-SELECT-G.
                       09  SPA-GMN-SRCBYOLST-SELECT
                                                   PIC X(01)
                                                   OCCURS  100.
               05  SPA-GMN-CDLST-AREA.
                   07  SPA-GMN-CDLST-CNT       PIC 9(05).
                   07  SPA-GMN-CDLST-MAX       PIC 9(05).
                   07  SPA-GMN-CDLST-SEL       PIC 9(05).
                   07  SPA-GMN-CDLST-OCC       OCCURS  150.
                       09  SPA-GMN-TCDNUM      PIC 9(02).
                       09  SPA-GMN-TCDBYOCD    PIC X(07).
                       09  SPA-GMN-TCDBYOMEI   PIC X(50).
               05  SPA-NAI-HEAD-AREA.
                   07  SPA-NAI-SYS-UTAGAIKBN       PIC X(01).
                   07  SPA-NAI-SYS-TEKBYOKBN       PIC X(01).
               05  SPA-NAI-BYOLST-AREA.
                   07  SPA-NAI-BYOLST-OCC      OCCURS  401.
                       09  SPA-NAI-TMSTKBN     PIC X(01).
                       09  SPA-NAI-TBYOSTATUS  PIC X(01).
                       09  SPA-NAI-TBYOCD-G.
                           11  SPA-NAI-TBYOCD  PIC X(07)
                                               OCCURS  21.
               05  SPA-NAI-SRCBYOLST-AREA.
                   07  SPA-NAI-SRCBYOLST-OCC   OCCURS  100.
                       09  SPA-NAI-TSRCBYOSTATUS
                                               PIC X(01).
                       09  SPA-NAI-TSRCBYOCD-G.
                           11  SPA-NAI-TSRCBYOCD
                                               PIC X(07)
                                               OCCURS  21.
               05  SPA-NAI-BYO-INPUT-AREA.
                   07  SPA-NAI-INPUT-MODE      PIC 9(1).
                   07  SPA-NAI-SELNUM          PIC 9(3).
                   07  SPA-NAI-SELNUM-FLG      PIC 9(1)
                                               OCCURS  400.
                   07  SPA-NAI-BYOINF-G.
                       09  SPA-NAI-BYOINF-OVER     PIC 9(01).
                       09  SPA-NAI-BYOINF-CNT      PIC 9(03).
                       09  SPA-NAI-BYOINF-MAX      PIC 9(03).
                       09  SPA-NAI-BYOINF-OCC      OCCURS 6.
                           11  SPA-NAI-BYOIDX      PIC 9(03).
                           11  SPA-NAI-BYOMEI      PIC X(100).
                           11  SPA-NAI-BYOSTATUS   PIC X(1).
                           11  SPA-NAI-BYOCD-G.
                               13  SPA-NAI-BYOCD-OCC   OCCURS 21.
                                   15  SPA-NAI-BYOCD   PIC X(07).
                05 SPA-MST-AREA.
                   07  SPA-MST-UPD                 PIC 9(01).
                   07  SPA-MST-MAX                 PIC 9(05).
                   07  SPA-MST-OCC                 OCCURS  401.
                       09  SPA-MST-TBYOMEI         PIC X(100).
                       09  SPA-MST-TJYKN           PIC X(01).
                       09  SPA-MST-TBYOCD-G.
                           11  SPA-MST-TBYOCD      PIC X(07)
                                                   OCCURS  21.
                       09  SPA-MST-TUTAGAI         PIC X(01).
                       09  SPA-MST-TSRTKEY.
                           11  SPA-MST-TGRPID      PIC X(01).
                           11  SPA-MST-TGROUPCHG   PIC 9(05).
                           11  SPA-MST-TIDX        PIC 9(05).
                       09  SPA-MST-THOSOKU         PIC X(40).
                       09  SPA-MST-THOSOKU-JYKN    PIC X(01).
                       09  SPA-MST-TBYOSTATUS      PIC X(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-BYOMEI                  PIC 9(01).
           03  FLG-USERBYOMEI              PIC 9(01).
           03  FLG-PTBYOMEI                PIC 9(01).
           03  FLG-BYOLST-ROW              PIC 9(01).
           03  FLG-SRCBYOLST-ROW           PIC 9(01).
           03  FLG-CDLST-ROW               PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-SKIP                    PIC 9(01).
           03  FLG-LOOP-END                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-CHKSND                  PIC 9(01).
           03  FLG-TEKIOUBYOMEI            PIC 9(01).
           03  FLG-HAISHI                  PIC 9(01).
           03  FLG-GROUP                   PIC 9(01).
           03  FLG-UNGROUP                 PIC 9(01).
           03  FLG-ZENKAKU                 PIC 9(01).
           03  FLG-MSTCHG                  PIC 9(01).
           03  FLG-TEMPTBL                 PIC 9(01).
           03  FLG-SRCBYOLST-SELECT        PIC 9(01).
           03  FLG-LIMITCHK                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRCBYOLST               PIC 9(05).
           03  CNT-STR                     PIC 9(05).
           03  CNT-GROUPCHG                PIC 9(05).
           03  CNT-OTHER                   PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXB                        PIC 9(05).
           03  IDXC                        PIC 9(05).
           03  IDXD                        PIC 9(05).
           03  IDXE                        PIC 9(05).
           03  IDXF                        PIC 9(05).
           03  IDXI                        PIC 9(05).
           03  IDXK                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXM                        PIC 9(05).
           03  IDXN                        PIC 9(05).
           03  IDXS                        PIC 9(05).
           03  IDXU                        PIC 9(05).
           03  IDXZ                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-ST                      PIC 9(05).
           03  IDX-SKIP                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-XIDMSG                  PIC X(70). 
           03  WRK-MCP-PATH.
               05  WRK-MCP-BLOCKS          PIC S9(9)   BINARY.
               05  WRK-MCP-RNAME           PIC S9(9)   BINARY.
               05  WRK-MCP-PNAME           PIC S9(9)   BINARY.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM             PIC X(200).
               05  WRK-CMB-ITM2            PIC X(200).
               05  WRK-CMB-IDX             PIC 9(03).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-SKIP-MAX                PIC 9(05).
           03  WRK-MAE-INPUT               PIC X(500).
           03  WRK-OUT-INPUT               PIC X(500).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-SEL-BYOMEI-G.
               05  WRK-SEL-BUNRUICD        PIC X(03).
               05  WRK-SEL-SRYKA           PIC X(02).
               05  WRK-SEL-BYOMEI          PIC X(100).
               05  WRK-SEL-STBYOMEICD      PIC X(07).
               05  WRK-SEL-EDBYOMEICD      PIC X(07).
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                 PIC ZZZ.
           03  WRK-STR-MAX                 PIC 9(05).
           03  WRK-STR-TMP                 PIC X(50).
           03  WRK-SELNUM-G.
               05  WRK-INPUT-MODE          PIC 9(03).
               05  WRK-SELNUM-X            PIC X(50).
               05  WRK-SELNUM              PIC 9(03).
               05  WRK-SELNUM-FLG          PIC 9(01)
                                           OCCURS  400.
           03  WRK-NO                      PIC 9(05).
           03  WRK-PRT-NO-G.
               05  WRK-PRT-NO              PIC ZZ9.
               05  WRK-PRT-MARK            PIC X(02).
      *
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM             PIC X(10)
                                           OCCURS 5.
               05  WRK-MUL-STNUM-X         PIC X(10).
               05  WRK-MUL-EDNUM-X         PIC X(10).
               05  WRK-MUL-STNUM           PIC 9(05).
               05  WRK-MUL-EDNUM           PIC 9(05).
           03  WRK-SAVE-G.
               05  WRK-SAVE-MAX            PIC 9(05).
               05  WRK-SAVE-IDX-G.
                   07  WRK-SAVE-OCC        OCCURS  400.
                       09  WRK-SAVE-IDX    PIC 9(05).
                       09  WRK-SAVE-CTRL   PIC X(01).
           03  WRK-BYOCD-CUR-G.
               05  WRK-BYOCD-CUR-SPC       PIC 9(03).
               05  WRK-BYOCD-CUR-MAX       PIC 9(03).
           03  WRK-BFMST-MAX            PIC 9(05).
           03  WRK-KEY-CD-G.
               05  WRK-KEY-CD-OCC          OCCURS  21.
                   07  WRK-KEY-CD          PIC X(07).
                   07  WRK-KEY-CDSEP       PIC X(01).
           03  WRK-IKOCD                   PIC X(07).
           03  WRK-TBYOMEICD-G.
               05  WRK-TBYOMEICD           PIC X(07)
                                           OCCURS  21.
           03  WRK-GRPCHKKBN               PIC X(01).
           03  WRK-GRPID                   PIC X(01).
           03  WRK-BYOCD                   PIC X(07).
           03  WRK-BYOMEI                  PIC X(100).
           03  WRK-GMN-CUR                 PIC 9(05).
           03  WRK-HOSOKU                  PIC X(40).
           03  WRK-BYOMEIINPUTCD           PIC X(20).
           03  WRK-BYOMEI-JYKN             PIC X(10).
           03  WRK-HOSOKU-JYKN             PIC X(10).
           03  WRK-KANACHK-MAX             PIC 9(04).
           03  WRK-TEKBYOKBN               PIC X(01).
           03  WRK-BYOLST-SEL              PIC 9(05).
           03  WRK-SRTKEY-G.
               05  WRK-SRTKEY              OCCURS 2.
                   07  WRK-SRTKEY-TGRPID   PIC X(01).
                   07  WRK-SRTKEY-TGROUPCHG PIC 9(05).
                   07  WRK-SRTKEY-TIDX     PIC 9(05).
      *
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET-BYOMEICD-G.
               05  WRK-WIDGET-BYOMEICD-STR PIC X(08).
               05  WRK-WIDGET-BYOMEICD-NUM PIC 9(01).
      *
       01  WRK-ROW-AREA.
           03  WRK-BYOLST-ROW              PIC S9(9)   BINARY.
           03  WRK-SRCBYOLST-ROW           PIC S9(9)   BINARY.
           03  WRK-CDLST-ROW               PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-BYOCD-MAX             PIC 9(05)   VALUE   6.
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
      *    1ページの最大件数はチェックマスタの最大適応病名数とする。
           03  CONST-BYOLST-MAX            PIC 9(05)   VALUE   400.
           03  CONST-SRCBYOLST-MAX         PIC 9(05)   VALUE   100.
           03  CONST-CDLST-MAX             PIC 9(05)   VALUE   150.
           03  CONST-SRTKBN-MAX            PIC 9(05)   VALUE   50.
           03  CONST-REIGAIYKZ-MAX         PIC 9(05)   VALUE   100.
           03  CONST-HOSOKUCD-MAX          PIC 9(05)   VALUE   3.
      *
           03  CONST-JYKNKBN-G.
               05  CONST-JYKNKBN-MAX    PIC 9(05) VALUE 4.
               05  CONST-JYKNKBN-VAL.
                 07                    PIC X(01) VALUE "1".
                 07                    PIC X(10) VALUE "部分一致".
                 07                    PIC X(10) VALUE "部分".
      *
                 07                    PIC X(01) VALUE "2".
                 07                    PIC X(10) VALUE "完全一致".
                 07                    PIC X(10) VALUE "完全".
      *
                 07                    PIC X(01) VALUE "3".
                 07                    PIC X(10) VALUE "前方一致".
                 07                    PIC X(10) VALUE "前方".
      *
                 07                    PIC X(01) VALUE "4".
                 07                    PIC X(10) VALUE "後方一致".
                 07                    PIC X(10) VALUE "後方".
               05  CONST-JYKNKBN-R  REDEFINES  CONST-JYKNKBN-VAL.
                 07  CONST-JYKNKBN-OCC  OCCURS    4.
                     09  CONST-JYKNKBNCD   PIC X(01).
                     09  CONST-JYKNKBNMEI1 PIC X(10).
                     09  CONST-JYKNKBNMEI2 PIC X(10).
      *
           03  CONST-UTAGAIKBN-G.
               05  CONST-UTAGAIKBN-VAL.
                 07        PIC X(01) VALUE "0".
                 07        PIC X(30) VALUE "システム管理の設定に従う".
      *
                 07        PIC X(01) VALUE "1".
                 07        PIC X(30) VALUE "認める(適応病名とする)".
      *
                 07        PIC X(01) VALUE "2".
                 07        PIC X(30) VALUE "認めない(適応病名としない)".
      *
                 07        PIC X(01) VALUE "1".
                 07        PIC X(30) VALUE "認める".
      *
                 07        PIC X(01) VALUE "2".
                 07        PIC X(30) VALUE "認めない".
               05  CONST-UTAGAIKBN-R  REDEFINES  CONST-UTAGAIKBN-VAL.
                 07  CONST-UTAGAIKBN-OCC  OCCURS   5.
                     09  CONST-UTAGAIKBNCD   PIC X(01).
                     09  CONST-UTAGAIKBNMEI1 PIC X(30).
           03  CONST-TEKBYOKBN-G.
               05  CONST-TEKBYOKBN-VAL.
                 07        PIC X(01) VALUE
                   "9".
                 07        PIC X(50) VALUE
                   "システム管理の設定に従う".
      *
                 07        PIC X(01) VALUE
                   "0".
                 07        PIC X(50) VALUE
                   "行わない".
      *
                 07        PIC X(01) VALUE
                   "1".
                 07        PIC X(50) VALUE
                   "行う".
      *
                 07        PIC X(01) VALUE
                   "2".
                 07        PIC X(50) VALUE
                   "画面左側の一覧に登録がない場合は行う".
      *
      *
               05  CONST-TEKBYOKBN-R  REDEFINES  CONST-TEKBYOKBN-VAL.
                 07  CONST-TEKBYOKBN-OCC  OCCURS   4.
                     09  CONST-TEKBYOKBNCD   PIC X(01).
                     09  CONST-TEKBYOKBNMEI1 PIC X(50).
           03  CONST-SELKBN-BYOMEI           PIC X(01)     VALUE "1".
           03  CONST-SELKBN-HOSOKU           PIC X(01)     VALUE "2".
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
           COPY    "CPXSUB01.INC"     REPLACING  //XSUB01-//
                                      BY         //TXSUB01-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1101.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名マスター
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    適応病名マスター
       01  TEKIOUBYOMEI-REC.
           COPY    "CPTEKIOUBYOMEI.INC".
      *
      *    チェックマスタ２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSBYOMEI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    一覧印刷変換領域
           COPY    "CPORCHXLST.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    チェックマスタ業務サブ１（病名欄編集）
           COPY    "CPXSUB01.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時チェックマスタ編集サブ
           COPY    "CPORCSS013.INC".
           COPY    "CPORCSS013.INC"    REPLACING    //SS013-//
                                       BY           //TSS013-//.
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X04
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
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
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X04         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
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
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "X04"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
      *
               MOVE    SPACE           TO  SPA-XID1-FLG
               MOVE    SPACE           TO  SPA-XIDCD
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "XID1"
               PERFORM 330-XID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "X03"
               PERFORM 340-X03-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "X06"
               GO  TO  300-SCREEN-EXT
           WHEN    "X07"
               PERFORM 340-X07-SET-SEC
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
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-X04
                                       SPA-X06-AREA
      *
           MOVE    SPA-X01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-X01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-X01-CHKKBN-G    TO  SPA-GMN-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  SPA-GMN-SRYCD
           MOVE    SPA-X01-NAME        TO  SPA-GMN-NAME
      *
           MOVE    ZERO                TO  SPA-NAI-INPUT-MODE
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
           IF    ( SYS-1101-YBUTAGAIKBN    =   SPACE )
               MOVE    "1"         TO  SPA-NAI-SYS-UTAGAIKBN
           ELSE
               MOVE    SYS-1101-YBUTAGAIKBN
                                   TO  SPA-NAI-SYS-UTAGAIKBN
           END-IF
      *
           EVALUATE    SPA-X01-CHKKBN
           WHEN    "1"
               IF    ( SYS-1101-TEKBYOYKZKBN   =   SPACE )
                   MOVE    ZERO    TO  SPA-NAI-SYS-TEKBYOKBN
               ELSE
                   MOVE    SYS-1101-TEKBYOYKZKBN
                                   TO  SPA-NAI-SYS-TEKBYOKBN
               END-IF
           WHEN    "2"
               IF    ( SYS-1101-TEKBYOSRYKBN   =   SPACE )
                   MOVE    ZERO    TO  SPA-NAI-SYS-TEKBYOKBN
               ELSE
                   MOVE    SYS-1101-TEKBYOSRYKBN
                                   TO  SPA-NAI-SYS-TEKBYOKBN
               END-IF
           END-EVALUATE
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
      *    一致条件初期値
           MOVE    SPA-GMN-JYKNKBNLST (1)
                                       TO  SPA-GMN-JYKNKBN-G
                                           SPA-GMN-HOSOKU-JYKNKBN-G
                                           SPA-GMN-SRCJYKNKBN-G
      *
      *    疑い区分初期値
           MOVE    SPA-GMN-UTAGAIKBNLST (1)
                                       TO  SPA-GMN-UTAGAIKBN-G
                                           SPA-GMN-SRCUTAGAIKBN-G
      *    グループＩＤ初期値
           MOVE    SPA-GMN-GRPIDLST (1)
                                   TO  SPA-GMN-GRPID-G
      *
           PERFORM 901-CHKSND-KEY3-SEL-SEC
      *
           MOVE    CHKSND-NYUGAIKBN    TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
      *    適応病名区分初期値
           IF    ( SPA-X01-CHKKBN      =   "1" OR "2" )
               IF    ( CHKSND-TEKBYOKBN =  SPACE )
                   MOVE    "9"         TO  WRK-CMB-CD
               ELSE
                   MOVE    CHKSND-TEKBYOKBN
                                       TO  WRK-CMB-CD
               END-IF
               PERFORM 4200-TEKBYOKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TEKBYOKBN-G
           END-IF
      *
           PERFORM 3101-SAVE-MST-SEC
      *
           MOVE    1               TO  SPA-GMN-BYOLST-PAGE
      *
           PERFORM 3101-BYOLST-SEC
      *
           PERFORM 4100-SRCBYOLST-SEC
      *
           IF    ( SPA-X01-CHKKBN      =   "1" )
               PERFORM 900-CHKSND-KEY3-SEL-SEC
      *
               PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                        OR   ( SPA-X06-MAX    >=  CONST-REIGAIYKZ-MAX )
      *
                   COMPUTE SPA-X06-MAX
                       =   SPA-X06-MAX +   1
      *
                   MOVE    CHKSND-CD   TO  SPA-X06-TEKCD (SPA-X06-MAX)
      *
                   PERFORM 900-CHKSND-KEY3-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           IF    ( SPA-X01-BYOMEI      =   SPACE )
            AND  ( SPA-X01-HOSOKU      =   SPACE )
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF    ( SPA-X01-BYOMEI  NOT =   SPACE )
                   MOVE    SPA-X01-BYOMEI  TO  X04-BYOMEI
                   PERFORM 4100-BYOMEI-CHK-SEC
               END-IF
               IF    ( SPA-X01-HOSOKU  NOT =   SPACE )
                   MOVE    SPA-X01-HOSOKU  TO  X04-HOSOKU
                   PERFORM 4100-HOSOKU-CHK-SEC
               END-IF
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
      *
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    入外区分コンボ設定処理
           PERFORM 3101-NYUGAIKBNLST-SEC
      *
      *    適応病名参照コンボ設定処理
           PERFORM 3101-TEKBYOKBNLST-SEC
      *
      *    一致条件区分コンボ設定処理
           PERFORM 3101-JYKNKBNLST-SET-SEC
      *
      *    疑いコンボ設定処理
           PERFORM 3101-UTAGAIKBNLST-SET-SEC
      *
      *    病名ソースコンボ設定処理
           PERFORM 3101-SRCBYOKBNLST-SET-SEC
      *
      *    グループＩＤコンボ設定処理
           PERFORM 3101-GRPIDLST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボ設定処理
      *****************************************************************
       3101-NYUGAIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUGAIKBNLST-G
      *
           IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
               MOVE    3           TO  SPA-GMN-NYUGAIKBN-MAX
               MOVE    "0"         TO  SPA-GMN-NYUGAIKBNL    (1)
               MOVE    "チェックしない"
                                   TO  SPA-GMN-NYUGAIKBNMEIL (1)
      *
               MOVE    "1"         TO  SPA-GMN-NYUGAIKBNL    (2)
               MOVE    "入院限定"  TO  SPA-GMN-NYUGAIKBNMEIL (2)
      *
               MOVE    "2"         TO  SPA-GMN-NYUGAIKBNL    (3)
               MOVE    "外来限定"  TO  SPA-GMN-NYUGAIKBNMEIL (3)
           ELSE
               MOVE    2           TO  SPA-GMN-NYUGAIKBN-MAX
      *
               MOVE    "1"         TO  SPA-GMN-NYUGAIKBNL    (1)
               MOVE    "入院限定"  TO  SPA-GMN-NYUGAIKBNMEIL (1)
      *
               MOVE    "2"         TO  SPA-GMN-NYUGAIKBNL    (2)
               MOVE    "外来限定"  TO  SPA-GMN-NYUGAIKBNMEIL (2)
           END-IF
      *
           . 
       3101-NYUGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    適応病名参照コンボ設定処理
      *****************************************************************
       3101-TEKBYOKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-TEKBYOKBNLST-G
      *
           IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
      *
               INITIALIZE              WRK-CMB-ITM-G
      *
               MOVE    3           TO  SPA-GMN-TEKBYOKBN-MAX
      *
               MOVE    "0"         TO  SPA-GMN-TEKBYOKBNL (1)
               MOVE    CONST-TEKBYOKBNMEI1(2)
                                   TO  SPA-GMN-TEKBYOKBNMEIL (1)
      *
               MOVE    "1"         TO  SPA-GMN-TEKBYOKBNL (2)
               MOVE    CONST-TEKBYOKBNMEI1(3)
                                   TO  SPA-GMN-TEKBYOKBNMEIL (2)
      *
               MOVE    "9"         TO  SPA-GMN-TEKBYOKBNL (3)
      *
               EVALUATE    SPA-NAI-SYS-TEKBYOKBN
               WHEN    "1"
                   MOVE    CONST-TEKBYOKBNMEI1(3)
                                   TO  WRK-CMB-ITM
               WHEN    "2"
                   MOVE    CONST-TEKBYOKBNMEI1(4)
                                   TO  WRK-CMB-ITM
               WHEN    OTHER
                   MOVE    CONST-TEKBYOKBNMEI1(2)
                                   TO  WRK-CMB-ITM
               END-EVALUATE
      *
               STRING  CONST-TEKBYOKBNMEI1(1)
                               DELIMITED   BY  SPACE
                        "【"   DELIMITED   BY  SIZE
                        WRK-CMB-ITM
                               DELIMITED   BY  SPACE
                        "】"   DELIMITED   BY  SIZE
               INTO    SPA-GMN-TEKBYOKBNMEIL (3)
               END-STRING
      *
           END-IF
      *
           . 
       3101-TEKBYOKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一致条件区分コンボ設定処理
      *****************************************************************
       3101-JYKNKBNLST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-JYKNKBNLST-G
      *
           MOVE    ZERO            TO  IDX-MAX
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   CONST-JYKNKBN-MAX )
      *
               COMPUTE IDX-MAX =   IDX-MAX +   1
      *
               MOVE    CONST-JYKNKBNCD   (IDX-CMB)
                                   TO  SPA-GMN-JYKNKBNL    (IDX-MAX)
               MOVE    CONST-JYKNKBNMEI1 (IDX-CMB)
                                   TO  SPA-GMN-JYKNKBNMEIL (IDX-MAX)
      *
           END-PERFORM
      *
           MOVE    IDX-MAX         TO  SPA-GMN-JYKNKBN-MAX
      *
           . 
       3101-JYKNKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    疑い区分コンボ設定処理
      *****************************************************************
       3101-UTAGAIKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-UTAGAIKBNLST-G
      *
           EVALUATE    SPA-X01-CHKKBN
           WHEN    "1"
               MOVE    CONST-UTAGAIKBNCD (1)
                                   TO  SPA-GMN-UTAGAIKBNL (1)
               IF    ( SPA-NAI-SYS-UTAGAIKBN   =   "1" )
                   STRING    CONST-UTAGAIKBNMEI1 (1)  DELIMITED BY SPACE
                             "【"   DELIMITED   BY  SIZE
                             CONST-UTAGAIKBNMEI1 (2)  DELIMITED BY SPACE
                             "】"   DELIMITED   BY  SIZE
                   INTO      SPA-GMN-UTAGAIKBNMEIL (1)
                   END-STRING
               ELSE
                   STRING    CONST-UTAGAIKBNMEI1 (1)  DELIMITED BY SPACE
                             "【"   DELIMITED   BY  SIZE
                             CONST-UTAGAIKBNMEI1 (3)  DELIMITED BY SPACE
                             "】"   DELIMITED   BY  SIZE
                   INTO      SPA-GMN-UTAGAIKBNMEIL (1)
                   END-STRING
               END-IF
               MOVE    CONST-UTAGAIKBNCD (2)
                                   TO  SPA-GMN-UTAGAIKBNL (2)
               MOVE    CONST-UTAGAIKBNMEI1 (2)
                                   TO  SPA-GMN-UTAGAIKBNMEIL (2)
               MOVE    CONST-UTAGAIKBNCD (3)
                                   TO  SPA-GMN-UTAGAIKBNL (3)
               MOVE    CONST-UTAGAIKBNMEI1 (3)
                                   TO  SPA-GMN-UTAGAIKBNMEIL (3)
      *
               MOVE    3           TO  SPA-GMN-UTAGAIKBN-MAX
           WHEN    "2"
               MOVE    CONST-UTAGAIKBNCD (2)
                                   TO  SPA-GMN-UTAGAIKBNL (1)
               MOVE    CONST-UTAGAIKBNMEI1 (2)
                                   TO  SPA-GMN-UTAGAIKBNMEIL (1)
               MOVE    CONST-UTAGAIKBNCD (3)
                                   TO  SPA-GMN-UTAGAIKBNL (2)
               MOVE    CONST-UTAGAIKBNMEI1 (3)
                                   TO  SPA-GMN-UTAGAIKBNMEIL (2)
      *
               MOVE    2           TO  SPA-GMN-UTAGAIKBN-MAX
           WHEN    "6"
               MOVE    CONST-UTAGAIKBNCD (4)
                                   TO  SPA-GMN-UTAGAIKBNL (1)
               MOVE    CONST-UTAGAIKBNMEI1 (4)
                                   TO  SPA-GMN-UTAGAIKBNMEIL (1)
      *
               MOVE    1           TO  SPA-GMN-UTAGAIKBN-MAX
           END-EVALUATE
      *
           . 
       3101-UTAGAIKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（患者病名）
      *****************************************************************
       3101-SRTKBNLST-PTBYO-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-SRTKBNLST-G
      *
           MOVE    1               TO  SPA-GMN-SRTKBN-MAX
      *
           MOVE    "00"            TO  SPA-GMN-SRTKBNL    (1)
           MOVE    "全科"          TO  SPA-GMN-SRTKBNMEIL (1)
      *
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC    TO  SYSKANRI-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-SRTKBN-MAX >=   CONST-SRTKBN-MAX )
      *
               COMPUTE SPA-GMN-SRTKBN-MAX  =   SPA-GMN-SRTKBN-MAX  +   1
      *
               MOVE    SYS-1005-KBNCD
                           TO  SPA-GMN-SRTKBNL    (SPA-GMN-SRTKBN-MAX)
      *
               MOVE    SYS-1005-SRYKANAME1
                           TO  SPA-GMN-SRTKBNMEIL (SPA-GMN-SRTKBN-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC TO  SYS-1005-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SRTKBNLST-PTBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（病名）
      *****************************************************************
       3101-SRTKBNLST-BYO-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-SRTKBNLST-G
      *
           MOVE    3               TO  SPA-GMN-SRTKBN-MAX
      *
           MOVE    "00"            TO  SPA-GMN-SRTKBNL    (1)
           MOVE    "すべて"        TO  SPA-GMN-SRTKBNMEIL (1)
      *
           MOVE    "01"            TO  SPA-GMN-SRTKBNL    (2)
           MOVE    "傷病名"        TO  SPA-GMN-SRTKBNMEIL (2)
      *
           MOVE    "02"            TO  SPA-GMN-SRTKBNL    (3)
           MOVE    "修飾語"        TO  SPA-GMN-SRTKBNMEIL (3)
      *
           . 
       3101-SRTKBNLST-BYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（自院病名）
      *****************************************************************
       3101-SRTKBNLST-UBYO-SET-SEC     SECTION.
      *
           INITIALIZE                  SPA-GMN-SRTKBNLST-G
      *
           MOVE    1               TO  SPA-GMN-SRTKBN-MAX
      *
           MOVE    "000"           TO  SPA-GMN-SRTKBNL    (1)
           MOVE    "すべて"        TO  SPA-GMN-SRTKBNMEIL (1)
      *
           PERFORM 900-USERBYOMEI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( SPA-GMN-SRTKBN-MAX >=   CONST-SRTKBN-MAX )
      *
               COMPUTE SPA-GMN-SRTKBN-MAX  =   SPA-GMN-SRTKBN-MAX  +   1
      *
               MOVE    USBYO-BYOMEIINPUTCD
                           TO  SPA-GMN-SRTKBNL    (SPA-GMN-SRTKBN-MAX)
      *
               MOVE    USBYO-BYOMEI
                           TO  SPA-GMN-SRTKBNMEIL (SPA-GMN-SRTKBN-MAX)
      *
               PERFORM 900-USERBYOMEI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SRTKBNLST-UBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（適応病名）
      *****************************************************************
       3101-SRTKBNLST-TEKBYO-SET-SEC   SECTION.
      *
           INITIALIZE                  SPA-GMN-SRTKBNLST-G
      *
           MOVE    0               TO  SPA-GMN-SRTKBN-MAX
      *
           . 
       3101-SRTKBNLST-TEKBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースコンボ設定処理
      *****************************************************************
       3101-SRCBYOKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SRCBYOKBNLST-G
      *
           MOVE    3               TO  SPA-GMN-SRCBYOKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRCBYOKBNL    (1)
           MOVE    "患者病名"      TO  SPA-GMN-SRCBYOKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRCBYOKBNL    (2)
           MOVE    "病名"
                                   TO  SPA-GMN-SRCBYOKBNMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-SRCBYOKBNL    (3)
           MOVE    "自院病名"
                                   TO  SPA-GMN-SRCBYOKBNMEIL (3)
      *
           IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
               MOVE    4           TO  SPA-GMN-SRCBYOKBN-MAX
      *
               MOVE    "M"         TO  SPA-GMN-SRCBYOKBNL    (4)
               MOVE    "適応病名"  TO  SPA-GMN-SRCBYOKBNMEIL (4)
           END-IF
      *
           . 
       3101-SRCBYOKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    グループＩＤコンボ設定処理
      *****************************************************************
       3101-GRPIDLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-GRPIDLST-G
      *
           MOVE    6               TO  SPA-GMN-GRPID-MAX
      *
           MOVE    "0"             TO  SPA-GMN-GRPIDL    (1)
           MOVE    "指定なし"      TO  SPA-GMN-GRPIDMEIL (1)
      *
           MOVE    "A"             TO  SPA-GMN-GRPIDL    (2)
           MOVE    "Ａ"            TO  SPA-GMN-GRPIDMEIL (2)
      *
           MOVE    "B"             TO  SPA-GMN-GRPIDL    (3)
           MOVE    "Ｂ"            TO  SPA-GMN-GRPIDMEIL (3)
      *
           MOVE    "C"             TO  SPA-GMN-GRPIDL    (4)
           MOVE    "Ｃ"            TO  SPA-GMN-GRPIDMEIL (4)
      *
           MOVE    "D"             TO  SPA-GMN-GRPIDL    (5)
           MOVE    "Ｄ"            TO  SPA-GMN-GRPIDMEIL (5)
      *
           MOVE    "E"             TO  SPA-GMN-GRPIDL    (6)
           MOVE    "Ｅ"            TO  SPA-GMN-GRPIDMEIL (6)
      *
           . 
       3101-GRPIDLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ退避処理
      *****************************************************************
       3101-SAVE-MST-SEC                 SECTION.
      *
           INITIALIZE                    SPA-MST-AREA
      *
           MOVE    ZERO           TO   FLG-TEMPTBL
           PERFORM 900-CHKSND-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( SPA-MST-MAX    >=  CONST-BYOLST-MAX )
      *
               COMPUTE SPA-MST-MAX =   SPA-MST-MAX  +   1
      *
               MOVE    SPA-MST-MAX     TO  IDX-MAX
      *
               MOVE    CHKSND-BYOMEI   TO  SPA-MST-TBYOMEI (IDX-MAX)
               MOVE    CHKSND-COMPKBN  TO  SPA-MST-TJYKN   (IDX-MAX)
               MOVE    CHKSND-UTAGAIKBN
                                       TO  SPA-MST-TUTAGAI (IDX-MAX)
               MOVE    CHKSND-GRPID    TO  SPA-MST-TGRPID(IDX-MAX)
               MOVE    CHKSND-BYOMEIHENFLG
                                       TO  SPA-MST-TBYOSTATUS
                                                               (IDX-MAX)
               MOVE    CHKSND-CD       TO  WRK-KEY-CD-G
               PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
                   MOVE    WRK-KEY-CD (IDX0)
                                       TO  SPA-MST-TBYOCD (IDX-MAX IDX0)
               END-PERFORM
      *
               MOVE    CHKSND-HOSOKU   TO  SPA-MST-THOSOKU (IDX-MAX)
               MOVE    CHKSND-HOSOKU-COMPKBN
                                       TO  SPA-MST-THOSOKU-JYKN
                                                           (IDX-MAX)
      *
               PERFORM 900-CHKSND-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SAVE-MST-EXT.
           EXIT.
      *****************************************************************
      *    病名リスト編集処理
      *****************************************************************
       3101-BYOLST-SEC                 SECTION.
     *
           MOVE    SPA-GMN-BYOLST-SEL
                                   TO  WRK-BYOLST-SEL
     *
           INITIALIZE                  SPA-GMN-BYOLST-AREA
                                       SPA-NAI-BYOLST-AREA
           MOVE    SPACE           TO  SPA-GMN-SELNUM
      *
           MOVE    1               TO  FLG-BYOLST-ROW
      *
           MOVE    ZERO            TO  IDX-SKIP
                                       IDXZ
                                       CNT-OTHER
                                       FLG-MSTCHG
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-BYOLST-PAGE  -   1 )
               *   CONST-BYOLST-MAX
      *
           INITIALIZE                      SS013-AREA
           MOVE    "DBCREATE"          TO  SS013-FUNC
           MOVE    "9"                 TO  SS013-CREATEKBN
           MOVE    SPA-X01-CHKKBN      TO  SS013-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  SS013-SRYCD
           MOVE    "0"                 TO  SS013-TEKBYOKBN
           MOVE    SPA-X01-KJNYMD      TO  SS013-KJNYMD
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
           MOVE    SS013-AREA          TO  TSS013-AREA
      *
           PERFORM 31011-BYOLST-SEC
      *
           MOVE    1                   TO  FLG-TEMPTBL
           PERFORM 900-CHKSND-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =  ZERO )
                    OR   ( IDX-SKIP       >=  WRK-SKIP-MAX )
      *
               COMPUTE IDX-SKIP  = IDX-SKIP   +   1
               COMPUTE IDXZ =  IDXZ    +   1
               IF    ( CHKSND-TERMID   =   SS013-TEMP-NAME )
                AND  ( CHKSND-OPID     =   SS013-TEMP-KEY )
                   COMPUTE CNT-OTHER
                       =   CNT-OTHER   +   1
                   IF    ( CNT-OTHER   =   1 )
                       COMPUTE IDX-SKIP  = IDX-SKIP   +   1
                   END-IF
               END-IF
      *
               IF    ( IDX-SKIP       >    WRK-SKIP-MAX )
                   CONTINUE
               ELSE
                   PERFORM 900-CHKSND-KEY2-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( SPA-GMN-BYOLST-MAX  >=  CONST-BYOLST-MAX )
      *
               COMPUTE SPA-GMN-BYOLST-MAX
                   =   SPA-GMN-BYOLST-MAX  +   1
      *
               COMPUTE IDXZ =  IDXZ    +   1
      *
               IF    ( CHKSND-TERMID   =   SS013-TEMP-NAME )
                AND  ( CHKSND-OPID     =   SS013-TEMP-KEY )
                   COMPUTE CNT-OTHER
                       =   CNT-OTHER   +   1
                   IF    ( CNT-OTHER   =   1 )
                       MOVE    "■■■　以下適応病名マスタより　■■■"
                               TO  SPA-GMN-TBYOMEI (SPA-GMN-BYOLST-MAX)
                       MOVE    "1"
                               TO  SPA-NAI-TMSTKBN (SPA-GMN-BYOLST-MAX)
                       COMPUTE SPA-GMN-BYOLST-MAX
                           =   SPA-GMN-BYOLST-MAX  +   1
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-BYOLST-MAX  >   CONST-BYOLST-MAX )
                   CONTINUE
               ELSE
      *
                   PERFORM 31012-BYOLST-SEC
      *
                   PERFORM 900-CHKSND-KEY2-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-CHKSND          =   ZERO )
               MOVE    1               TO  SPA-GMN-BYOLST-NEXT
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           IF    ( WRK-BYOLST-SEL       >  ZERO )
            OR   ( SPA-NAI-INPUT-MODE   =   2 )
               MOVE    SPACE        TO  X04-SELNUM
               PERFORM 4100-SELNUM-CHK-SEC
           END-IF
      *
           . 
       3101-BYOLST-EXT.
           EXIT.
      *****************************************************************
      *    病名リスト編集処理
      *****************************************************************
       31011-BYOLST-SEC                SECTION.
      *
           MOVE    1               TO  FLG-TEMPTBL
      *
           PERFORM 4901-KOUSHIN-SEC
      *
           MOVE    TSS013-AREA         TO  SS013-AREA
           MOVE    "DBINSERT"          TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           . 
       31011-BYOLST-EXT.
           EXIT.
      *****************************************************************
      *    病名リスト編集処理
      *****************************************************************
       31012-BYOLST-SEC                 SECTION.
      *
           MOVE    SPA-GMN-BYOLST-MAX  TO  IDX-MAX
      *
           MOVE    IDXZ            TO  SPA-GMN-TNUM    (IDX-MAX)
      *
           MOVE    CHKSND-BYOMEI   TO  SPA-GMN-TBYOMEI (IDX-MAX)
           MOVE    CHKSND-COMPKBN  TO  SPA-GMN-TJYKN   (IDX-MAX)
           MOVE    CHKSND-UTAGAIKBN
                                   TO  SPA-GMN-TUTAGAI (IDX-MAX)
           MOVE    CHKSND-GRPID    TO  SPA-GMN-TGRPID(IDX-MAX)
           MOVE    CHKSND-BYOMEIHENFLG
                                   TO  SPA-NAI-TBYOSTATUS
                                                               (IDX-MAX)
           MOVE    CHKSND-CD       TO  WRK-KEY-CD-G
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
               MOVE    WRK-KEY-CD (IDX0)
                                   TO  SPA-NAI-TBYOCD (IDX-MAX IDX0)
           END-PERFORM
      *
           MOVE    CHKSND-HOSOKU   TO  SPA-GMN-THOSOKU (IDX-MAX)
           MOVE    CHKSND-HOSOKU-COMPKBN
                                   TO  SPA-GMN-THOSOKU-JYKN (IDX-MAX)
      *
           IF    ( CHKSND-TERMID   =   SS013-TEMP-NAME )
            AND  ( CHKSND-OPID     =   SS013-TEMP-KEY )
               MOVE    "1"         TO  SPA-NAI-TMSTKBN (IDX-MAX)
           END-IF
      *
           . 
       31012-BYOLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-XID1-SET-SEC            SECTION.
      *
           IF    ( SPA-XID1-FLG        =   "OK" )
               EVALUATE    SPA-XIDCD
               WHEN    "1001"
                   PERFORM 490-KOUSHIN-SEC
               WHEN    "1002"
                   PERFORM 4031-DEL-SEC
               WHEN    "2001"
                   PERFORM 4111-PRINT-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-XID1-FLG
           MOVE    SPACE           TO  SPA-XIDCD
           .
       330-XID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック項目複写画面戻り時処理
      *****************************************************************
       340-X03-SET-SEC             SECTION.
      *
           IF    ( SPA-X03-GMN-KAKUTEI     NOT =   1 )
               GO  TO  340-X03-SET-EXT
           END-IF
      *
           INITIALIZE              SPA-MST-AREA
                                   SPA-MST-AREA
      *
           MOVE    1                   TO  SPA-MST-UPD
      *
           IF    ( SPA-X03-GMN-LCHKLST-MAX >   CONST-BYOLST-MAX )
               MOVE    CONST-BYOLST-MAX
                                       TO  SPA-MST-MAX
           ELSE
               MOVE    SPA-X03-GMN-LCHKLST-MAX
                                       TO  SPA-MST-MAX
           END-IF
      *
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   SPA-MST-MAX )
      *
               MOVE    SPA-X03-GMN-TLCHK-NAME (IDXE)
                                       TO  SPA-MST-TBYOMEI (IDXE)
               MOVE    SPA-X03-GMN-TLCHK-JYKN (IDXE)
                                       TO  SPA-MST-TJYKN (IDXE)
      *
               PERFORM VARYING IDXF    FROM    1   BY  1
                       UNTIL ( IDXF    >   CONST-MKCD-MAX )
                   MOVE    SPA-X03-NAI-TLCHK-BYOCD(IDXE IDXF)
                                       TO  SPA-MST-TBYOCD  (IDXE IDXF)
               END-PERFORM
      *
               MOVE    SPA-X03-NAI-TLCHK-BYOSTATUS (IDXE)
                                       TO  SPA-MST-TBYOSTATUS  (IDXE)
      *
               IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
                   MOVE    SPA-X03-NAI-TLCHK-UTAGAI (IDXE)
                                       TO  SPA-MST-TUTAGAI  (IDXE)
                   MOVE    SPA-X03-GMN-TLCHK-HOSOKU (IDXE)
                                       TO  SPA-MST-THOSOKU  (IDXE)
                   MOVE    SPA-X03-GMN-TLCHK-HOSOKU-JYKN (IDXE)
                                       TO  SPA-MST-THOSOKU-JYKN (IDXE)
                   MOVE    SPA-X03-GMN-TLCHK-GRPID(IDXE)
                                       TO  SPA-MST-TGRPID (IDXE)
               ELSE
                   MOVE    "1"         TO  SPA-MST-TUTAGAI  (IDXE)
                   MOVE    SPACE       TO  SPA-MST-THOSOKU  (IDXE)
                   MOVE    "1"         TO  SPA-MST-THOSOKU-JYKN (IDXE)
                   MOVE    SPACE       TO  SPA-MST-TGRPID (IDXE)
               END-IF
      *
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-BYOLST-PAGE
      *
           PERFORM 3101-BYOLST-SEC
      *
           .
       340-X03-SET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索画面戻り時処理
      *****************************************************************
       340-X07-SET-SEC             SECTION.
      *
           IF    ( SPA-X01-UERBYOMEI-KAKUTEI   =   1 )
      *
               IF    ( SPA-X01-SELKBN    =   CONST-SELKBN-BYOMEI )
      *
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-BYOCD-MAX )
      *
                       MOVE    SPA-X01-INPUTCD (IDX0)
                                   TO  X04-BYOMEICD (IDX0)
      *
                   END-PERFORM
      *
                   IF    ( SPA-X01-POSITION    NOT =   ZERO )
                       MOVE    "BYOMEICD"  TO  WRK-WIDGET-BYOMEICD-STR
                       MOVE    SPA-X01-POSITION
                                       TO  WRK-WIDGET-BYOMEICD-NUM
                       MOVE    WRK-WIDGET-BYOMEICD-G
                                       TO  WRK-MCP-WIDGET
                   END-IF
                   PERFORM 4100-BYOMEICD-CHK-SEC
                   MOVE    SPACE           TO  SPA-ERRCD
               ELSE
      *
                   INITIALIZE          XSUB01-LNK
      *
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-HOSOKUCD-MAX )
      *
                       MOVE    SPA-X01-INPUTCD (IDX0)
                                   TO  XSUB01-BYOCD (IDX0)
      *
                   END-PERFORM
      *
                   MOVE    1               TO  XSUB01-SINGLE
                   CALL    "ORCGXSUB01"    USING   SPA-AREA
                                           XSUB01-LNK
      *
                   MOVE    XSUB01-O-BYOMEIALL
                                           TO  X04-HOSOKU
                   MOVE    "HOSOKU"        TO  WRK-MCP-WIDGET
                   PERFORM 4100-HOSOKU-CHK-SEC
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
      *
           ELSE
               IF    ( SPA-X01-SELKBN      =   CONST-SELKBN-BYOMEI )
                   CONTINUE
               ELSE
                   MOVE    18              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       340-X07-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"     TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 403-DEL-SEC
      *    自院CD検索
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 409-X07-SENI-SEC
      *    病名登録
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 4101-BYOTRK-SEC
      *    例外薬剤登録画面
           WHEN    "CLICKED"       ALSO    "B06"
               IF    ( SPA-X01-CHKKBN      =   "1" )
                   PERFORM 409-X06-SENI-SEC
               END-IF
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06S"
               PERFORM 4100-PREV-PAGE-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07S"
               PERFORM 4100-NEXT-PAGE-SEC
      *    病名削除
           WHEN    "CLICKED"       ALSO    "B07"
           WHEN    "CLICKED"       ALSO    "BTN_DEL"
               PERFORM 4101-BYODEL-SEC
      *    病名リストへ追加
           WHEN    "CLICKED"       ALSO    "BTN_INS"
               PERFORM 4101-BYOLST-INS-SEC
      *    病名リストへ追加（全て）
           WHEN    "CLICKED"       ALSO    "BTN_ALLINS"
               PERFORM 4101-BYOLST-ALLINS-SEC
      *    チェック項目複写
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 409-X03-SENI-SEC
      *    入外区分編集処理
           WHEN    "CLICKED"       ALSO    "B08S"
               PERFORM 4101-NYUGAIKBN-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 4101-PREV-PAGE-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM 4101-NEXT-PAGE-SEC
      *    選択解除
           WHEN    "CLICKED"       ALSO    "B09S"
               PERFORM 490-CANCEL-SEL-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-KAKUTEI-SEC
      *    印刷
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 411-PRINT-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN        ANY         ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN        ANY         ALSO    "BYOMEICD1"
           WHEN        ANY         ALSO    "BYOMEICD2"
           WHEN        ANY         ALSO    "BYOMEICD3"
           WHEN        ANY         ALSO    "BYOMEICD4"
           WHEN        ANY         ALSO    "BYOMEICD5"
           WHEN        ANY         ALSO    "BYOMEICD6"
               PERFORM 4100-BYOMEICD-CHK-SEC
           WHEN        ANY         ALSO    "BYOMEI"
               PERFORM 4100-BYOMEI-CHK-SEC
           WHEN        ANY         ALSO    "HOSOKU"
               PERFORM 4100-HOSOKU-CHK-SEC
           WHEN        ANY         ALSO    "SEL_BYOMEI"
               PERFORM 4100-SEL-BYOMEI-CHK-SEC
           WHEN        ANY         ALSO    "TEKBYOKBN"
               PERFORM 4100-TEKBYOKBN-CHK-SEC
           WHEN        ANY         ALSO    "BYOLST"
               PERFORM 4101-BYOLST-SEL-SEC
           WHEN        ANY         ALSO    "SRCBYOLST"
               PERFORM 4101-SRCBYOLST-SEL-SEC
           WHEN    OTHER
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE   X04-NYUGAIKBN    TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE   X04-JYKNKBN      TO  SPA-GMN-JYKNKBN-G
           MOVE   X04-UTAGAIKBN    TO  SPA-GMN-UTAGAIKBN-G
           MOVE   X04-HOSOKU-JYKNKBN
                                   TO  SPA-GMN-HOSOKU-JYKNKBN-G
           MOVE   X04-GRPID        TO  SPA-GMN-GRPID-G
           MOVE   X04-SRTKBN       TO  SPA-GMN-SRTKBN-G
           MOVE   X04-SRCBYOKBN    TO  SPA-GMN-SRCBYOKBN-G
           MOVE   X04-SRCJYKNKBN   TO  SPA-GMN-SRCJYKNKBN-G
           MOVE   X04-SRCUTAGAIKBN TO  SPA-GMN-SRCUTAGAIKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-GMN-BYOMEICD-CHK-FLG    =   1 )
                   PERFORM 4101-BYOMEICD-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-GMN-BYOMEI-CHK-FLG      =   1 )
                   PERFORM 4101-BYOMEI-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-JYKNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-HOSOKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-HOSOKU-JYKNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-UTAGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-GRPID-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SEL-BYOMEI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRCBYOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRCJYKNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SRCUTAGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "SRTKBN" )
                OR   ( WRK-MCP-WIDGET  =   "SRCBYOKBN" )
      *
                   PERFORM 4101-SRCBYOLST-SEC
      *
               END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-SELNUM-G
      *
           IF    ( SPA-GMN-SELNUM  =   SPACE )
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM (1:1)    =   "." )
               PERFORM 41011-MULTI-SEL-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    1           TO  SPA-GMN-CUR
                   GO  TO  4101-SELNUM-CHK-EXT
               END-IF
           ELSE
      *
               MOVE    SPA-GMN-SELNUM  TO  WRK-SELNUM-X
               PERFORM 4101-SELNUM-LIMIT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    1           TO  SPA-GMN-CUR
                   GO  TO  4101-SELNUM-CHK-EXT
               END-IF
      *
               MOVE    1               TO  WRK-INPUT-MODE
      *
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号範囲チェック処理
      *****************************************************************
       4101-SELNUM-LIMIT-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-SELNUM-X    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
      *
               MOVE    ZERO        TO  WRK-SELNUM
      *
               PERFORM VARYING IDX5    FROM    1   BY  1
                       UNTIL ( IDX5    >   SPA-GMN-BYOLST-MAX )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-TNUM (IDX5)
                                   =   SNUM-OUTNUM )
                       MOVE    SNUM-OUTNUM
                                   TO  WRK-SELNUM
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-SELNUM  =   ZERO )
                   MOVE    "0001"  TO  SPA-ERRCD
               END-IF
      *
           ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
           END-IF
      *
           .
       4101-SELNUM-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数行選択チェック処理
      *****************************************************************
       41011-MULTI-SEL-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
      *    入力がないとき
           IF    ( SPA-GMN-SELNUM (2:) =   SPACE )
               MOVE    "0001"      TO  SPA-ERRCD
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-IF
      *
           UNSTRING    SPA-GMN-SELNUM (2:)     DELIMITED   BY  ","
                                                           OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
           ON  OVERFLOW
               MOVE    "0002"      TO  SPA-ERRCD  
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-UNSTRING
      *
      *    入力番号チェック
           IF    (     WRK-MUL-NUM (1)     =   "*" )
               IF    ( WRK-MUL-NUM (2) NOT =   SPACE )
                OR   ( WRK-MUL-NUM (3) NOT =   SPACE )
                OR   ( WRK-MUL-NUM (4) NOT =   SPACE )
                OR   ( WRK-MUL-NUM (5) NOT =   SPACE )
                   MOVE    "0081"  TO  SPA-ERRCD
                   GO  TO  41011-MULTI-SEL-CHK-EXT
               END-IF
      *
               PERFORM VARYING IDX5    FROM    1   BY  1
                       UNTIL ( IDX5    >       SPA-GMN-BYOLST-MAX )
      *
                   IF    ( SPA-NAI-TMSTKBN (IDX5)
                                       =       SPACE )
                       MOVE    1       TO  WRK-SELNUM-FLG (IDX5)
                   END-IF
      *
               END-PERFORM
      *
               MOVE    2           TO  WRK-INPUT-MODE
               GO  TO  41011-MULTI-SEL-CHK-EXT
      *
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >       5 )
      *
               IF    ( WRK-MUL-NUM (IDX4)  =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   UNSTRING    WRK-MUL-NUM (IDX4)  DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       MOVE    "0001"      TO  SPA-ERRCD
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    WRK-MUL-EDNUM-X
                                           TO  WRK-MUL-STNUM-X
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-STNUM-X     TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-EDNUM
      *
                   PERFORM VARYING IDX5    FROM    1   BY  1
                           UNTIL ( IDX5    >       SPA-GMN-BYOLST-MAX )
                       IF    ( SPA-GMN-TNUM (IDX5) >=  WRK-MUL-STNUM)
                        AND  ( SPA-GMN-TNUM (IDX5) <=  WRK-MUL-EDNUM )
                           IF    ( SPA-NAI-TMSTKBN (IDX5)
                                                   =       SPACE )
                               MOVE    1       TO  WRK-SELNUM-FLG (IDX5)
                           ELSE
                               MOVE    "0013"  TO  SPA-ERRCD
                               GO  TO  41011-MULTI-SEL-CHK-EXT
                           END-IF
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    2               TO  WRK-INPUT-MODE
      *
           .
       41011-MULTI-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4101-BYOMEICD-CHK-SEC           SECTION.
      *
           INITIALIZE                  XSUB01-LNK
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       CONST-BYOCD-MAX )
      *
               MOVE    SPA-GMN-BYOCD (IDX0)    TO  WRK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               MOVE    WRK-OUT-INPUT           TO  SPA-GMN-BYOCD (IDX0)
      *
               MOVE    SPA-GMN-BYOCD (IDX0)
                                   TO  XSUB01-BYOCD (IDX0)
      *
           END-PERFORM
      *
           MOVE    XSUB01-LNK      TO  TXSUB01-LNK
           CALL    "ORCGXSUB01"    USING   SPA-AREA
                                           XSUB01-LNK
      *
           INITIALIZE                      SPA-NAI-BYOINF-G
      *
           MOVE    XSUB01-O-BYOMEI-CNT     TO  SPA-NAI-BYOINF-CNT
      *
           IF    ( SPA-GMN-BYOLST-SEL  >   ZERO )
            OR   ( SPA-NAI-INPUT-MODE  =   2 )
      *
               MOVE    TXSUB01-LNK TO  XSUB01-LNK
               MOVE    1           TO  XSUB01-SINGLE
      *
               CALL    "ORCGXSUB01"    USING   SPA-AREA
                                               XSUB01-LNK
           END-IF
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >       CONST-BYOCD-MAX )
      *
               MOVE    XSUB01-O-BYOCD (IDXB)
                                   TO  SPA-GMN-BYOCD (IDXB)
      *
               IF    ( XSUB01-O-TOP (IDXB) =   "1" )
      *
                   COMPUTE SPA-NAI-BYOINF-MAX
                       =   SPA-NAI-BYOINF-MAX  +   1
                   MOVE    SPA-NAI-BYOINF-MAX
                                   TO  IDXI
                   MOVE    IDXB    TO  SPA-NAI-BYOIDX (IDXI)
                   MOVE    XSUB01-O-BYOSTATUS (IDXB)
                                   TO  SPA-NAI-BYOSTATUS (IDXI)
                   MOVE    XSUB01-O-BYOMEI(IDXB)
                                   TO  SPA-NAI-BYOMEI (IDXI)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-MKCD-MAX )
      *
                       MOVE    XSUB01-O-RECECD (IDXB IDX0)
                                   TO  SPA-NAI-BYOCD (IDXI IDX0)
      *
                   END-PERFORM
      *
               END-IF
           END-PERFORM
      *
           MOVE    XSUB01-O-BYOSTATUSALL
                                       TO  SPA-GMN-BYOSTATUS
      *
           MOVE    XSUB01-O-BYOMEIALL  TO  SPA-GMN-BYOMEI
      *
           PERFORM 4101-CDLST-SEC
      *
           EVALUATE    XSUB01-O-RC
           WHEN    1
               MOVE    "0005"          TO  SPA-ERRCD
           WHEN    2
               MOVE    "0006"          TO  SPA-ERRCD
           WHEN    3
               MOVE    "0007"          TO  SPA-ERRCD
           WHEN    4
               IF    ( WRK-MCP-WIDGET (1:8)    =   "BYOMEICD" )
                AND (( XSUB01-O-ERRIDX >   ZERO )
                OR   ( XSUB01-O-ERRIDX <=  CONST-BYOCD-MAX ))
                   INITIALIZE      SPA-X01-USERBYOMEI-G
                   MOVE    CONST-SELKBN-BYOMEI
                                       TO  SPA-X01-SELKBN
                   MOVE    XSUB01-O-ERRIDX
                                       TO  SPA-X01-POSITION
                   MOVE    XSUB01-O-ERRIDX
                                       TO  IDXB
                   IF    ( SPA-GMN-BYOCD (IDXB)(1:2)
                                       =   "//" )
                       INITIALIZE      ORCSNUMAREA
                       MOVE    SPA-GMN-BYOCD (IDXB)(3:)
                                               TO  SNUM-INX
                       CALL    "ORCSNUM"       USING
                                               ORCSNUMAREA
                       IF    ( SNUM-RC         =   ZERO )
                        AND  ( SNUM-SEISUU     <   4 )
                           MOVE    SNUM-OUTNUM TO  SPA-X01-BUNRUICD
                       END-IF
                       MOVE    SPACE           TO  SPA-GMN-BYOCD (IDXB)
                       PERFORM 4091-X07-SENI-SEC
                       GO  TO  4101-BYOMEICD-CHK-EXT
                   ELSE
                           MOVE    SPA-GMN-BYOCD (IDXB)
                                               TO  WRK-BYOMEIINPUTCD
                           PERFORM 900-USERBYOMEI-KEY11-SEL-SEC
                           IF    ( FLG-USERBYOMEI  =   ZERO )
                               MOVE    IDXB        TO  SPA-X01-INDEX
                               MOVE    SPA-GMN-BYOCD (IDXB)
                                               TO  SPA-X01-BYOMEIINPUTCD
                               PERFORM 4091-X07-SENI-SEC
                               GO  TO  4101-BYOMEICD-CHK-EXT
                           ELSE
                               MOVE    "0008"      TO  SPA-ERRCD
                           END-IF
                   END-IF
               ELSE
                   MOVE    "0008"      TO  SPA-ERRCD
               END-IF
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
            AND  ( SPA-ERRCD       NOT =   "0006" )
               IF    ( XSUB01-O-ERRIDX >   ZERO )
                OR   ( XSUB01-O-ERRIDX <=  CONST-BYOCD-MAX )
                   COMPUTE SPA-GMN-CUR
                       =   XSUB01-O-ERRIDX
                       +   1
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BYOLST-SEL  >   ZERO )
                OR   ( SPA-NAI-INPUT-MODE  =   2 )
                   IF    ( SPA-NAI-BYOINF-MAX  >   1 )
                       MOVE    2       TO  SPA-GMN-CUR
                       MOVE    "0114"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-BYOINF-MAX  >   1 )
                   MOVE    SPACE       TO  SPA-GMN-HOSOKU
               END-IF
           END-IF
      *
           .
       4101-BYOMEICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4101-BYOMEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-BYOMEI      =   SPACE )
               CONTINUE
           ELSE
               PERFORM 41011-BYOMEI-CHK-SEC
           END-IF
      *
           .
       4101-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       41011-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-BYOMEI      TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-BYOMEI
           IF    ( FLG-ERR         NOT =   ZERO )
               MOVE    "0009"          TO SPA-ERRCD
               MOVE    8               TO SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO                TO  CNT-STR
               INSPECT SPA-GMN-BYOMEI          TALLYING
                           CNT-STR FOR ALL "■"
               IF    ( CNT-STR        >    ZERO )
                   MOVE    "0010"          TO SPA-ERRCD
                   MOVE    8               TO SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE                      ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    SPA-GMN-BYOMEI      TO  LNK-BYO-BYOMEI
               CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                               SPA-AREA
               MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  SPA-GMN-BYOSTATUS
      *
               MOVE    1               TO  SPA-NAI-BYOINF-MAX
               MOVE    1               TO  SPA-NAI-BYOIDX (1)
               MOVE    SPA-GMN-BYOMEI  TO  SPA-NAI-BYOMEI (1)
               MOVE    SPA-GMN-BYOSTATUS
                                       TO  SPA-NAI-BYOSTATUS (1)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >       LNK-BYO-BYOMEI-MAX )
      *
                   MOVE    LNK-BYO-TBYOMEICD (IDX0)
                                   TO  SPA-NAI-BYOCD (1 IDX0)
      *
               END-PERFORM
      *
      *
               PERFORM 4101-CDLST-SEC
               PERFORM 41011-BYOMEICD-SEC
      *
               IF    ( LNK-BYO-BYOMEICD-MAX-ERR    =   1 )
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41011-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一致条件区分チェック処理
      *****************************************************************
       4101-JYKNKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-JYKNKBN      TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE   =   2 )
                   MOVE    SPACE       TO  SPA-GMN-JYKNKBN-G
               ELSE
                   MOVE    SPA-GMN-JYKNKBNLST (1)
                                       TO  SPA-GMN-JYKNKBN-G
               END-IF
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYKNKBN-G
           END-IF
      *
           .
       4101-JYKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    補足コメントチェック処理
      *****************************************************************
       4101-HOSOKU-CHK-SEC             SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-HOSOKU      =   SPACE )
               MOVE    "1"             TO  SPA-GMN-HOSOKU-JYKNKBN
           WHEN  ( SPA-GMN-HOSOKU(1:2) =   "//"  )
              OR ( SPA-GMN-HOSOKU      =   "・" OR "・・"
                                       OR  "／" OR "／／" )
                   MOVE    SPACE       TO  SPA-GMN-HOSOKU
                   INITIALIZE          SPA-X01-USERBYOMEI-G
                   MOVE    CONST-SELKBN-HOSOKU
                                       TO  SPA-X01-SELKBN
                   PERFORM 4091-X07-SENI-SEC
           WHEN   ( SPA-GMN-HOSOKU(1:1)
                                       =   "/"  )
               IF    ( SPA-GMN-HOSOKU(2:)  =   SPACE )
                   MOVE    18      TO  SPA-GMN-CUR
                   MOVE    "0011"  TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-HOSOKU(2:)
                                       TO  WRK-HOSOKU
                   PERFORM 41011-HOSOKU-CHK-SEC
               END-IF
           WHEN  OTHER
               MOVE    SPA-GMN-HOSOKU  TO  WRK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               MOVE    WRK-OUT-INPUT   TO  SPA-GMN-HOSOKU
               IF    ( FLG-ERR         =   ZERO )
                AND  ( FLG-ZENKAKU     =   1 )
                    CONTINUE
               ELSE
                   MOVE    SPA-GMN-HOSOKU
                                       TO  WRK-HOSOKU
                   PERFORM 41011-HOSOKU-CHK-SEC
               END-IF
           END-EVALUATE
      *
           .
       4101-HOSOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    補足コメントチェック処理
      *****************************************************************
       41011-HOSOKU-CHK-SEC            SECTION.
      *
           INITIALIZE                  XSUB01-LNK
      *
           UNSTRING    WRK-HOSOKU      DELIMITED   BY  ","
           INTO        XSUB01-BYOCD (1)
           END-UNSTRING
           MOVE        XSUB01-BYOCD (1)
                                   TO  WRK-HOSOKU
      *
           MOVE    1               TO  XSUB01-SINGLE
           CALL    "ORCGXSUB01"    USING   SPA-AREA
                                           XSUB01-LNK
           EVALUATE    XSUB01-O-RC
           WHEN    ZERO
           WHEN    2
               MOVE    XSUB01-O-BYOMEIALL
                                   TO  SPA-GMN-HOSOKU
           WHEN    1
               MOVE    SPACE       TO  SPA-GMN-HOSOKU
           WHEN    4
               IF    ( WRK-MCP-WIDGET  =   "HOSOKU" )
                   MOVE    WRK-HOSOKU  TO  WRK-BYOMEIINPUTCD
                   PERFORM 900-USERBYOMEI-KEY11-SEL-SEC
                   IF    ( FLG-USERBYOMEI  =   ZERO )
                       INITIALIZE          SPA-X01-USERBYOMEI-G
                       MOVE    CONST-SELKBN-HOSOKU
                                       TO  SPA-X01-SELKBN
                       MOVE    1       TO  SPA-X01-INDEX
                       MOVE    WRK-HOSOKU
                                       TO  SPA-X01-BYOMEIINPUTCD
                       PERFORM 4091-X07-SENI-SEC
                       GO  TO  41011-HOSOKU-CHK-EXT
                   ELSE
                       MOVE    18      TO  SPA-GMN-CUR
                       MOVE    "0011"  TO  SPA-ERRCD
                   END-IF
               ELSE
                       MOVE    18      TO  SPA-GMN-CUR
                       MOVE    "0011"  TO  SPA-ERRCD
               END-IF
           END-EVALUATE
      *
           .
       41011-HOSOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    補足コメント一致条件区分チェック処理
      *****************************************************************
       4101-HOSOKU-JYKNKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-HOSOKU-JYKNKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE   =   2 )
                   MOVE    SPACE       TO  SPA-GMN-HOSOKU-JYKNKBN-G
               ELSE
                   MOVE    SPA-GMN-JYKNKBNLST (1)
                                       TO  SPA-GMN-HOSOKU-JYKNKBN-G
               END-IF
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-HOSOKU-JYKNKBN-G
           END-IF
      *
           .
       4101-HOSOKU-JYKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い区分チェック処理
      *****************************************************************
       4101-UTAGAIKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-UTAGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-UTAGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE   =   2 )
                   MOVE    SPACE       TO  SPA-GMN-UTAGAIKBN-G
               ELSE
                   MOVE    SPA-GMN-UTAGAIKBNLST (1)
                                       TO  SPA-GMN-UTAGAIKBN-G
               END-IF
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-UTAGAIKBN-G
           END-IF
      *
           .
       4101-UTAGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    グループＩＤチェック処理
      *****************************************************************
       4101-GRPID-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-GRPID   TO  WRK-CMB-CD
           PERFORM 4200-GRPIDLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE   =   2 )
                   MOVE    SPACE       TO  SPA-GMN-GRPID-G
               ELSE
                   MOVE    SPA-GMN-GRPIDLST (1)
                                       TO  SPA-GMN-GRPID-G
               END-IF
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-GRPID-G
           END-IF
      *
           .
       4101-GRPID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       4101-SEL-BYOMEI-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-SEL-BYOMEI  TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-SEL-BYOMEI
           IF    ( FLG-ERR         NOT =   ZERO )
               MOVE    "0004"          TO SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
               GO  TO  4101-SEL-BYOMEI-CHK-EXT
           END-IF
      *
           .
       4101-SEL-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分チェック処理
      *****************************************************************
       4101-SRTKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-SRTKBN      TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-SRTKBNLST (1)
                                       TO  SPA-GMN-SRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRTKBN-G
           END-IF
      *
           .
       4101-SRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソース区分チェック処理
      *****************************************************************
       4101-SRCBYOKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-SRCBYOKBN   =   SPACE )
               GO  TO  4101-SRCBYOKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SRCBYOKBN   TO  WRK-CMB-CD
           PERFORM 4200-SRCBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
               GO  TO  4101-SRCBYOKBN-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRCBYOKBN-G
      *
           IF    ( WRK-MCP-WIDGET  =   "SRCBYOKBN" )
      *
      *
               EVALUATE    SPA-GMN-SRCBYOKBN
               WHEN    "1"
                   PERFORM 3101-SRTKBNLST-PTBYO-SET-SEC
               WHEN    "2"
                   PERFORM 3101-SRTKBNLST-BYO-SET-SEC
               WHEN    "3"
                   PERFORM 3101-SRTKBNLST-UBYO-SET-SEC
               WHEN    "M"
                   PERFORM 3101-SRTKBNLST-TEKBYO-SET-SEC
               END-EVALUATE
      *
               MOVE    SPA-GMN-SRTKBNLST (1)
                                   TO  SPA-GMN-SRTKBN-G
      *
           END-IF
      *
           .
       4101-SRCBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名一致条件区分チェック処理
      *****************************************************************
       4101-SRCJYKNKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-SRCJYKNKBN      TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-JYKNKBNLST (1)
                                       TO  SPA-GMN-SRCJYKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRCJYKNKBN-G
           END-IF
      *
           .
       4101-SRCJYKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名疑い区分チェック処理
      *****************************************************************
       4101-SRCUTAGAIKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-SRCUTAGAIKBN    TO  WRK-CMB-CD
           PERFORM 4200-UTAGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-UTAGAIKBNLST (1)
                                       TO  SPA-GMN-SRCUTAGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRCUTAGAIKBN-G
           END-IF
      *
           .
       4101-SRCUTAGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4101-NYUGAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
           .
       4101-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名リスト編集処理
      *****************************************************************
       4100-SRCBYOLST-SEC              SECTION.
      *
           IF   ( SPA-X01-CHKKBN      =   "1" OR "2" )
      *
               MOVE    ZERO            TO  FLG-GROUP
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-MST-MAX )
                   IF    ( SPA-MST-TGRPID (IDX0)   NOT =   SPACE )
                       MOVE    1       TO  FLG-GROUP
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-GROUP           =   ZERO )
                   EVALUATE    TRUE
                   WHEN ( SPA-GMN-TEKBYOKBN   =   "1" )
      *
                   WHEN ( SPA-GMN-TEKBYOKBN   =   "9" )
                    AND ( SPA-NAI-SYS-TEKBYOKBN    =  "1" )
      *
                   WHEN ( SPA-GMN-TEKBYOKBN   =   "9" )
                    AND ( SPA-NAI-SYS-TEKBYOKBN    =   "2" )
                    AND ( SPA-MST-MAX              =   ZERO )
                       MOVE    "M"             TO  WRK-CMB-CD
                       PERFORM 4200-SRCBYOKBNLST-CHK-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRCBYOKBN-G
                       PERFORM 4101-SRCBYOLST-SEC
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       4100-SRCBYOLST-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名リスト編集処理
      *****************************************************************
       4101-SRCBYOLST-SEC              SECTION.
      *
           MOVE    1           TO  SPA-GMN-SRCBYOLST-PAGE
      *
           MOVE    SPA-GMN-SRCBYOKBN
                               TO  SPA-GMN-SRCBYOLST-PTN
           MOVE    SPA-GMN-SRTKBN
                               TO  SPA-GMN-SRCBYOLST-SRT
      *
           PERFORM 3101-SRTKBNLST-SET-SEC
      *
           .
       4101-SRCBYOLST-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名検索キー編集
      *****************************************************************
       3101-SRTKBNLST-SET-SEC          SECTION.
      *
           INITIALIZE              WRK-SEL-BYOMEI-G
           MOVE    SPA-GMN-SEL-BYOMEI
                               TO  WRK-SEL-BYOMEI
      *
           INSPECT WRK-SEL-BYOMEI
                   REPLACING   ALL "＊"    BY  "%%"
      *
           EVALUATE    SPA-GMN-SRCBYOLST-PTN
           WHEN    "1"
               IF    ( SPA-GMN-SRCBYOLST-SRT   =   "00" )
                   MOVE    "%"         TO  WRK-SEL-SRYKA
               ELSE
                   MOVE    SPA-GMN-SRCBYOLST-SRT
                                       TO  WRK-SEL-SRYKA
               END-IF
           WHEN    "2"
               EVALUATE    SPA-GMN-SRCBYOLST-SRT
               WHEN    "01"
                   MOVE    "0000000"   TO  WRK-SEL-STBYOMEICD
                   MOVE    "9999999"   TO  WRK-SEL-EDBYOMEICD
               WHEN    "02"
                   MOVE    "ZZZ0000"   TO  WRK-SEL-STBYOMEICD
                   MOVE    "ZZZ9999"   TO  WRK-SEL-EDBYOMEICD
               WHEN    OTHER
                   MOVE    "0000000"   TO  WRK-SEL-STBYOMEICD
                   MOVE    "ZZZ9999"   TO  WRK-SEL-EDBYOMEICD
               END-EVALUATE
           WHEN   "3"
               IF    ( SPA-GMN-SRCBYOLST-SRT   =   "000" )
                   MOVE    "%"         TO  WRK-SEL-BUNRUICD
               ELSE
                   MOVE    SPA-GMN-SRCBYOLST-SRT
                                       TO  WRK-SEL-BUNRUICD
               END-IF
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-SRCBYOLST-PTN
           WHEN    "1"
               PERFORM 41011-SRCBYOLST-PTBYO-SEC
           WHEN    "2"
               PERFORM 41012-SRCBYOLST-BYO-SEC
           WHEN    "3"
               PERFORM 41013-SRCBYOLST-UBYO-SEC
           WHEN    "M"
               PERFORM 41014-SRCBYOLST-TEKBYO-SEC
           WHEN    OTHER
      *
               INITIALIZE              SPA-GMN-SRCBYOLST-AREA
                                       SPA-NAI-SRCBYOLST-AREA
                                       SPA-GMN-SRTKBNLST-G
           END-EVALUATE
      *
           .
       3101-SRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（患者病名より）
      *****************************************************************
       41011-SRCBYOLST-PTBYO-SEC       SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-SRCBYOLST-AREA
                                   SPA-NAI-SRCBYOLST-AREA
      *
           PERFORM 900-PTBYOMEI-KEY26-SEL-SEC
      *
           PERFORM 41011-PTBYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    PTBYO-BYOMEI    TO  SPA-GMN-TSRCBYOMEI (IDX1)
               MOVE    SPACE           TO  SPA-GMN-TSRCHOSOKU (IDX1)
               MOVE    SPACE           TO  SPA-GMN-TSRCCD     (IDX1)
      *
               PERFORM 900-PTBYOMEI-KEY26-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDX1            TO  SPA-GMN-SRCBYOLST-MAX
      *
           IF    ( FLG-PTBYOMEI    =   ZERO )
               MOVE    1           TO  SPA-GMN-SRCBYOLST-NEXT
           END-IF
      *
           .
       41011-SRCBYOLST-PTBYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41011-PTBYO-SKIP-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST      >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-PTBYOMEI-KEY26-FET-SEC
      *
           END-PERFORM
      *
           .
       41011-PTBYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（病名より）
      *****************************************************************
       41012-SRCBYOLST-BYO-SEC         SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-SRCBYOLST-AREA
                                   SPA-NAI-SRCBYOLST-AREA
      *
           PERFORM 900-BYOMEI-KEY7-SEL-SEC
      *
           PERFORM 41012-BYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-BYOMEI  NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    BYO-BYOMEI      TO  SPA-GMN-TSRCBYOMEI (IDX1)
               MOVE    SPACE           TO  SPA-GMN-TSRCHOSOKU (IDX1)
               MOVE    BYO-BYOMEICD    TO  SPA-GMN-TSRCCD     (IDX1)
                                           SPA-NAI-TSRCBYOCD  (IDX1 1)
      *
               PERFORM 900-BYOMEI-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDX1            TO  SPA-GMN-SRCBYOLST-MAX
      *
           IF    ( FLG-BYOMEI      =   ZERO )
               MOVE    1           TO  SPA-GMN-SRCBYOLST-NEXT
           END-IF
      *
           .
       41012-SRCBYOLST-BYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41012-BYO-SKIP-SEC    SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-BYOMEI      NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST      >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-BYOMEI-KEY7-FET-SEC
      *
           END-PERFORM
      *
           .
       41012-BYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（自院病名より）
      *****************************************************************
       41013-SRCBYOLST-UBYO-SEC        SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-SRCBYOLST-AREA
                                   SPA-NAI-SRCBYOLST-AREA
      *
           PERFORM 900-USERBYOMEI-KEY10-SEL-SEC
      *
           PERFORM 41013-UBYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    USBYO-BYOMEI    TO  SPA-GMN-TSRCBYOMEI (IDX1)
               MOVE    SPACE           TO  SPA-GMN-TSRCHOSOKU (IDX1)
               MOVE    USBYO-BYOMEIINPUTCD
                                       TO  SPA-GMN-TSRCCD     (IDX1)
               MOVE    USBYO-BYOMEIHENFLG
                                       TO  SPA-NAI-TSRCBYOSTATUS(IDX1)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-MKCD-MAX )
                        OR   ( USBYO-BYOMEICD (IDX0)   =   "9999999" )
                        OR   ( USBYO-BYOMEICD (IDX0)   =   SPACE )
      *
                   MOVE    USBYO-BYOMEICD (IDX0)
                                   TO  SPA-NAI-TSRCBYOCD(IDX1 IDX0)
      *
               END-PERFORM
      *
               PERFORM 900-USERBYOMEI-KEY10-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDX1            TO  SPA-GMN-SRCBYOLST-MAX
      *
           IF    ( FLG-USERBYOMEI  =   ZERO )
               MOVE    1           TO  SPA-GMN-SRCBYOLST-NEXT
           END-IF
      *
           .
       41013-SRCBYOLST-UBYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41013-UBYO-SKIP-SEC  SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST      >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-USERBYOMEI-KEY10-FET-SEC
      *
           END-PERFORM
      *
           .
       41013-UBYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（適応病名マスタより）
      *    ORCSS013内でCHKSNDに適応病名マスタの内容を編集
      *****************************************************************
       41014-SRCBYOLST-TEKBYO-SEC      SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-SRCBYOLST-AREA
                                   SPA-NAI-SRCBYOLST-AREA
      *
      *
           INITIALIZE                      SS013-AREA
           MOVE    "DBCREATE"          TO  SS013-FUNC
           MOVE     "1"                TO  SS013-CREATEKBN
           MOVE     "1"                TO  SS013-TEKBYOKBN
           MOVE    SPA-X01-CHKKBN      TO  SS013-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  SS013-SRYCD
           MOVE    SPA-X01-KJNYMD      TO  SS013-KJNYMD
           CALL    "ORCSS013"          USING
                                       SS013-AREA
                                       SPA-AREA
           PERFORM 900-CHKSND-KEY29-SEL-SEC
      *
           PERFORM 41014-TEKBYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    CHKSND-BYOMEI
                                   TO  SPA-GMN-TSRCBYOMEI (IDX1)
               MOVE    SPACE       TO  SPA-GMN-TSRCHOSOKU (IDX1)
               MOVE    CHKSND-BYOMEIHENFLG
                                   TO  SPA-NAI-TSRCBYOSTATUS(IDX1)
      *
               MOVE    CHKSND-CD       TO  WRK-KEY-CD-G
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
                    OR   ( WRK-KEY-CD (IDX0)   =   SPACE )
                   MOVE    WRK-KEY-CD (IDX0)
                                           TO  SPA-NAI-TSRCBYOCD
                                                  (IDX1 IDX0)
               END-PERFORM
      *
               PERFORM 900-CHKSND-KEY29-FET-SEC
      *
           END-PERFORM
    *
           MOVE    IDX1            TO  SPA-GMN-SRCBYOLST-MAX
      *
           IF    ( FLG-CHKSND      =   ZERO )
               MOVE    1           TO  SPA-GMN-SRCBYOLST-NEXT
           END-IF
      *
           MOVE    "tbl_chksnd"            TO  MCP-TABLE
           MOVE    "key29"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           .
       41014-SRCBYOLST-TEKBYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41014-TEKBYO-SKIP-SEC  SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-CHKSND          NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST          >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-CHKSND-KEY29-FET-SEC
      *
           END-PERFORM
      *
           .
       41014-TEKBYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    ZERO            TO  SPA-NAI-INPUT-MODE
      *
           MOVE    X04-SELNUM      TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-INPUT-MODE  TO  SPA-NAI-INPUT-MODE
      *
           EVALUATE    SPA-NAI-INPUT-MODE
      *
           WHEN    1
               MOVE    WRK-SELNUM      TO  WRK-ZZZ
      *
               IF    ( WRK-ZZZ-G (1:1)     =   SPACE )
      *
                   UNSTRING    WRK-ZZZ-G   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR-TMP
                               SPA-GMN-SELNUM
                   END-UNSTRING
      *
               ELSE
                   MOVE    WRK-ZZZ-G   TO  SPA-GMN-SELNUM
               END-IF
      *
               MOVE    WRK-SELNUM      TO  SPA-GMN-BYOLST-SEL
      *
               PERFORM 41011-INPUT-AREA-HEN-SEC
      *
           WHEN    2
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-BYOLST-MAX )
                   MOVE    WRK-SELNUM-FLG (IDX0)
                                   TO  SPA-NAI-SELNUM-FLG (IDX0)
               END-PERFORM
               MOVE    SPACE       TO  SPA-GMN-JYKNKBN-G
                                       SPA-GMN-UTAGAIKBN-G
                                       SPA-GMN-GRPID-G
      *
           END-EVALUATE
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4100-BYOMEICD-CHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-BYOSTATUS
                                       SPA-GMN-BYOCD-G
                                       SPA-NAI-BYOINF-G
                                       SPA-GMN-BYOMEI
                                       SPA-GMN-CDLST-AREA
      *
           MOVE    1               TO  SPA-GMN-BYOMEICD-CHK-FLG
           MOVE    ZERO            TO  SPA-GMN-BYOMEI-CHK-FLG
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       CONST-BYOCD-MAX )
      *
               MOVE    X04-BYOMEICD (IDX0)
                                   TO  SPA-GMN-BYOCD (IDX0)
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-BYOCD-G =   SPACE )
               MOVE    8       TO  SPA-GMN-CUR
               GO  TO  4100-BYOMEICD-CHK-EXT
           END-IF
      *
           PERFORM 4101-BYOMEICD-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
            OR   ( SPA-ERRCD       =   "0006" )
               MOVE    ZERO        TO  SPA-GMN-BYOMEICD-CHK-FLG
      *
               IF    ( SPA-ERRCD   =   "0006" )
                   MOVE    9       TO  SPA-GMN-CUR
               ELSE
                   IF    ( WRK-MCP-WIDGET (1:8)    =   "BYOMEICD" )
                       PERFORM 41001-BYOCD-CUR-HEN-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       4100-BYOMEICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置決定処理
      *****************************************************************
       41001-BYOCD-CUR-HEN-SEC         SECTION.
      *
           MOVE    WRK-MCP-WIDGET  TO  WRK-WIDGET-BYOMEICD-G
      *
           INITIALIZE                  WRK-BYOCD-CUR-G
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-BYOCD-MAX )
               IF    ( SPA-GMN-BYOCD (IDX0)
                                   =   SPACE )
                   IF    ( WRK-BYOCD-CUR-SPC   =   ZERO )
                       MOVE    IDX0    TO  WRK-BYOCD-CUR-SPC
                   END-IF
               ELSE
                       MOVE    IDX0    TO  WRK-BYOCD-CUR-MAX
               END-IF
           END-PERFORM
      *
           IF    ( WRK-BYOCD-CUR-MAX   >   WRK-BYOCD-CUR-SPC )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-BYOCD-CUR-SPC   +   1
           ELSE
               IF    ( WRK-BYOCD-CUR-MAX   >=  WRK-WIDGET-BYOMEICD-NUM )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-WIDGET-BYOMEICD-NUM   +   2
               ELSE
                   IF    ( X04-BYOMEICD (WRK-WIDGET-BYOMEICD-NUM)
                                       NOT =   SPACE )
                       COMPUTE SPA-GMN-CUR
                           =   WRK-BYOCD-CUR-MAX   +   2
                   ELSE
                       MOVE    9       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41001-BYOCD-CUR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4100-BYOMEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-NAI-BYOINF-MAX          >   1 )
               MOVE    "0113"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                  SPA-GMN-BYOSTATUS
                                           SPA-GMN-BYOCD-G
                                           SPA-NAI-BYOINF-G
                                           SPA-GMN-BYOMEI
                                           SPA-GMN-CDLST-AREA
      *
               MOVE    ZERO            TO  SPA-GMN-BYOMEICD-CHK-FLG
               MOVE    1               TO  SPA-GMN-BYOMEI-CHK-FLG
      *
               MOVE    X04-BYOMEI      TO  SPA-GMN-BYOMEI
      *
               PERFORM 4101-BYOMEI-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                OR   ( SPA-ERRCD       =   "0006" )
                   MOVE    ZERO        TO  SPA-GMN-BYOMEI-CHK-FLG
               END-IF
      *
           END-IF
      *
           .
       4100-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    補足コメントチェック処理
      *****************************************************************
       4100-HOSOKU-CHK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               MOVE    X04-HOSOKU      TO  SPA-GMN-HOSOKU
      *
               PERFORM 4101-HOSOKU-CHK-SEC
      *
               PERFORM 4101-HOSOKU-JYKNKBN-CHK-SEC
      *
           END-IF
      *
           .
       4100-HOSOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適応病名区分チェック処理
      *****************************************************************
       4100-TEKBYOKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-TEKBYOKBN
                                   TO  WRK-TEKBYOKBN
      *
           MOVE    X04-TEKBYOKBN   TO  SPA-GMN-TEKBYOKBN-G
           MOVE    SPA-GMN-TEKBYOKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-TEKBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0012"      TO  SPA-ERRCD
               MOVE    20          TO  SPA-GMN-CUR
               MOVE    WRK-TEKBYOKBN
                                   TO  WRK-CMB-CD
               PERFORM 4200-TEKBYOKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TEKBYOKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TEKBYOKBN-G
               IF    ( SPA-GMN-TEKBYOKBN
                               NOT =   WRK-TEKBYOKBN )
                   MOVE    1       TO  SPA-GMN-BYOLST-PAGE
                   PERFORM 3101-BYOLST-SEC
                   IF    ( SPA-GMN-SRCBYOLST-MAX   =   ZERO )
                       PERFORM 4100-SRCBYOLST-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       4100-TEKBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       4100-SEL-BYOMEI-CHK-SEC         SECTION.
      *
           MOVE    X04-SEL-BYOMEI      TO  SPA-GMN-SEL-BYOMEI
      *
           PERFORM 4101-SEL-BYOMEI-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRCBYOLST-SEC
           END-IF
      *
           .
       4100-SEL-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名リスト選択処理
      *****************************************************************
       4101-BYOLST-SEL-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-BYO-INPUT-AREA
                                       SPA-NAI-BYO-INPUT-AREA
           MOVE    ZERO            TO  SPA-GMN-BYOLST-SEL
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-BYOLST-MAX )
      *
               IF    ( X04-BYOLST-SELECT (IDX-LST) =   "T" )
                AND  ( X04-TNO (IDX-LST)       NOT =   SPACE )
                   MOVE    IDX-LST
                               TO  SPA-GMN-BYOLST-SEL
                   PERFORM 41011-INPUT-AREA-HEN-SEC
                   MOVE    SPA-GMN-BYOLST-MAX
                               TO  IDX-LST
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4101-BYOLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力エリア編集処理
      *****************************************************************
       41011-INPUT-AREA-HEN-SEC        SECTION.
      *
           PERFORM 410111-INPUT-AREA-HEN-SEC
      *
           .
       41011-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力エリア編集処理
      *****************************************************************
       410111-INPUT-AREA-HEN-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-BYO-INPUT-AREA
                                       SPA-NAI-BYO-INPUT-AREA
      *
           MOVE    1                   TO  SPA-NAI-INPUT-MODE
      *
           MOVE    SPA-GMN-BYOLST-SEL  TO  IDX3
      *
           MOVE    SPA-GMN-TNUM (IDX3)
                                       TO  WRK-ZZZ
      *
           IF    ( WRK-ZZZ-G (1:1)     =   SPACE )
      *
               UNSTRING    WRK-ZZZ-G   DELIMITED   BY  ALL SPACE
               INTO        WRK-STR-TMP
                           SPA-GMN-SELNUM
               END-UNSTRING
      *
           ELSE
               MOVE    WRK-ZZZ-G       TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    SPA-GMN-TBYOMEI (IDX3)
                                       TO  SPA-GMN-BYOMEI
      *
           MOVE    SPA-GMN-TJYKN (IDX3)
                                       TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYKNKBN-G
      *
           MOVE    SPA-GMN-TUTAGAI (IDX3)
                                       TO  WRK-CMB-CD
           PERFORM 4200-UTAGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-UTAGAIKBN-G
      *
           IF    ( SPA-GMN-TGRPID (IDX3)   =   SPACE )
               MOVE    "0"             TO  WRK-CMB-CD
           ELSE
               MOVE    SPA-GMN-TGRPID (IDX3)
                                       TO  WRK-CMB-CD
           END-IF
           PERFORM 4200-GRPIDLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GRPID-G
      *
           MOVE    SPA-NAI-TBYOSTATUS (IDX3)
                                       TO  SPA-GMN-BYOSTATUS
      *
           MOVE    1               TO  SPA-NAI-BYOINF-MAX
           MOVE    1               TO  SPA-NAI-BYOIDX (1)
           MOVE    SPA-GMN-BYOMEI  TO  SPA-NAI-BYOMEI (1)
           MOVE    SPA-GMN-BYOSTATUS
                                   TO  SPA-NAI-BYOSTATUS (1)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
      *
               MOVE    SPA-NAI-TBYOCD (IDX3 IDX0)
                                       TO  SPA-NAI-BYOCD (1 IDX0)
      *
           END-PERFORM
      *
           PERFORM 4101-CDLST-SEC
      *
           PERFORM 41011-BYOMEICD-SEC
      *
           MOVE    SPA-GMN-THOSOKU (IDX3)
                                       TO  SPA-GMN-HOSOKU
      *
           MOVE    SPA-GMN-THOSOKU-JYKN (IDX3)
                                       TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HOSOKU-JYKNKBN-G
      *
           IF    ( SPA-GMN-BYOCD (1)   =   SPACE )
               MOVE    8               TO  SPA-GMN-CUR
           ELSE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           .
       410111-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名コード欄編集処理
      *****************************************************************
       41011-BYOMEICD-SEC              SECTION.
      *
           IF    ( SPA-GMN-CDLST-MAX          <=  CONST-BYOCD-MAX )
            AND  ( SPA-GMN-TCDBYOCD  (1)   NOT =  "0000999" )
      *
               PERFORM VARYING IDX6    FROM    1   BY  1
                       UNTIL ( IDX6    >   CONST-BYOCD-MAX )
      *
                   MOVE    SPA-GMN-TCDBYOCD (IDX6)
                                       TO  SPA-GMN-BYOCD (IDX6)
      *
               END-PERFORM
      *
           END-IF
      *
           IF    ( SPA-GMN-CDLST-MAX   >   CONST-BYOCD-MAX )
               MOVE    1               TO  SPA-NAI-BYOINF-OVER
           END-IF
      *
           .
      *
       41011-BYOMEICD-EXT.
           EXIT.
      *****************************************************************
      *    コード表示リスト編集処理
      *****************************************************************
       4101-CDLST-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-CDLST-ROW
           INITIALIZE                  SPA-GMN-CDLST-AREA
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SPA-NAI-BYOINF-MAX )
      *
               MOVE    SPA-NAI-BYOMEI (IDXB)
                                       TO  WRK-BYOMEI
      *
               PERFORM VARYING IDX6    FROM    1   BY  1
                       UNTIL ( IDX6    >   CONST-MKCD-MAX )
                        OR   ( SPA-NAI-BYOCD (IDXB IDX6)
                                       =  SPACE )
                        OR   ( SPA-GMN-CDLST-MAX
                                      >=  CONST-CDLST-MAX )
      *
                   MOVE    SPA-NAI-BYOCD (IDXB IDX6)
                                       TO  WRK-BYOCD
                   MOVE    IDX6        TO  IDXN
                   PERFORM 41011-CDLST-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       4101-CDLST-EXT.
           EXIT.
      *****************************************************************
      *    コード表示リスト編集処理
      *****************************************************************
       41011-CDLST-SEC                 SECTION.
      *
           COMPUTE SPA-GMN-CDLST-MAX   =   SPA-GMN-CDLST-MAX   +   1
      *
           MOVE    IDXN        TO  SPA-GMN-TCDNUM   (SPA-GMN-CDLST-MAX)
      *
           MOVE    WRK-BYOCD   TO  SPA-GMN-TCDBYOCD (SPA-GMN-CDLST-MAX)
      *
           IF    ( WRK-BYOCD   =   "0000999" )
               IF    ( IDXN    =   1 )
                   MOVE    WRK-BYOMEI
                               TO  SPA-GMN-TCDBYOMEI (SPA-GMN-CDLST-MAX)
               END-IF
           ELSE
               MOVE    WRK-BYOCD
                               TO  WRK-CMB-CD
               PERFORM 4200-YYKBYOCD-CHK-SEC
               IF    ( WRK-CMB-ITM NOT =   SPACE )
                   MOVE    WRK-CMB-ITM
                               TO  SPA-GMN-TCDBYOMEI (SPA-GMN-CDLST-MAX)
               ELSE
      *
                   INITIALIZE                  BYOMEI-REC
                   MOVE    WRK-BYOCD       TO  BYO-BYOMEICD
                   MOVE    BYOMEI-REC      TO  MCPDATA-REC
                   MOVE    "tbl_byomei"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC TO  BYOMEI-REC
                   ELSE
                       INITIALIZE              BYOMEI-REC
                   END-IF
      *
                   MOVE    "tbl_byomei"     TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
                   MOVE    BYO-BYOMEI
                               TO  SPA-GMN-TCDBYOMEI (SPA-GMN-CDLST-MAX)
               END-IF
           END-IF
      *
           .
       41011-CDLST-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト選択処理
      *****************************************************************
       4101-SRCBYOLST-SEL-SEC          SECTION.
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-SRCBYOLST-MAX )
      *
               MOVE    X04-SRCBYOLST-SELECT (IDX-LST)
                               TO  SPA-GMN-SRCBYOLST-SELECT (IDX-LST)
      *
           END-PERFORM
      *
           .
       4101-SRCBYOLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ登録処理
      *****************************************************************
       4101-BYOTRK-SEC                 SECTION.
      *
           MOVE    ZERO            TO  FLG-LIMITCHK
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4101-BYOTRK-EXT
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE  NOT =   2 )
               PERFORM 41011-KANREN-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4101-BYOTRK-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-NAI-INPUT-MODE
           WHEN    1
               MOVE    1           TO  IDXL
               PERFORM 4101-BYOTRK-SIN-UPD-SEC
               PERFORM 4101-BYOLST-UPD-SEC
           WHEN    2
               PERFORM 4101-BYOTRK-MUL-UPD-SEC
               PERFORM 4101-BYOLST-UPD-SEC
           WHEN    OTHER
               MOVE    1           TO  FLG-LIMITCHK
               PERFORM 4101-BYOTRK-INS-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 4101-BYOLST-SRT-SEC
      *
               MOVE    1                   TO  SPA-GMN-BYOLST-PAGE
               PERFORM 3101-BYOLST-SEC
      *
               PERFORM 220-CLEAR-SEC
      *
               IF    ( FLG-LIMITCHK        =  1 )
                   IF    ( SPA-MST-MAX    >=  CONST-BYOLST-MAX )
                       MOVE    "0102"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-BYOTRK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック
      *****************************************************************
       41011-KANREN-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-BYOMEI      =   SPACE )
                   MOVE    "0104"      TO  SPA-ERRCD
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-BYOINF-MAX  =   ZERO )
                   MOVE    "0105"      TO  SPA-ERRCD
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-JYKNKBN-G   =   SPACE )
                   MOVE    "0106"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-UTAGAIKBN-G   =   SPACE )
                   MOVE    "0110"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41011-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ登録処理（１件選択）
      *****************************************************************
       4101-BYOTRK-SIN-UPD-SEC         SECTION.
      *
           IF    ( SPA-GMN-BYOLST-SEL  >   ZERO )
            AND  ( SPA-GMN-BYOLST-SEL  <=  SPA-MST-MAX )
      *
               MOVE    1                   TO  SPA-MST-UPD
      *
               MOVE    SPA-GMN-BYOLST-SEL
                                       TO  IDX-LST
      *
               MOVE    SPA-NAI-BYOMEI (IDXL)
                                           TO  SPA-MST-TBYOMEI (IDX-LST)
      *
               MOVE    SPA-GMN-JYKNKBN     TO  SPA-MST-TJYKN   (IDX-LST)
      *
               MOVE    SPA-GMN-UTAGAIKBN   TO  SPA-MST-TUTAGAI(IDX-LST)
      *
               IF    ( SPA-GMN-GRPID   =   "0" )
                   MOVE    SPACE           TO  WRK-GRPID
               ELSE
                   MOVE    SPA-GMN-GRPID   TO  WRK-GRPID
               END-IF
      *
               IF    ( WRK-GRPID   NOT =   SPA-MST-TGRPID(IDX-LST))
                   MOVE    1           TO  SPA-MST-TGROUPCHG(IDX-LST)
               END-IF
      *
               MOVE    WRK-GRPID       TO  SPA-MST-TGRPID(IDX-LST)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >       CONST-MKCD-MAX )
      *
                   MOVE    SPA-NAI-BYOCD (IDXL IDX0)
                                       TO  SPA-MST-TBYOCD (IDX-LST IDX0)
               END-PERFORM
      *
               MOVE    SPA-NAI-BYOSTATUS (IDXL)
                                       TO  SPA-MST-TBYOSTATUS (IDX-LST)
      *
               MOVE    SPA-GMN-HOSOKU
                                       TO  SPA-MST-THOSOKU (IDX-LST)
      *
               MOVE    SPA-GMN-HOSOKU-JYKNKBN
                                       TO  SPA-MST-THOSOKU-JYKN(IDX-LST)
      *
           END-IF
      *
           .
       4101-BYOTRK-SIN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ登録処理（複数選択）
      *****************************************************************
       4101-BYOTRK-MUL-UPD-SEC         SECTION.
      *
           MOVE    ZERO            TO  CNT-GROUPCHG
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       SPA-GMN-BYOLST-MAX )
      *
               IF    ( SPA-NAI-SELNUM-FLG (IDX0)   =   1 )
                AND  ( SPA-NAI-TMSTKBN    (IDX0)   =   SPACE )
      *
                   MOVE    1       TO  SPA-MST-UPD
      *
                   IF    ( SPA-GMN-JYKNKBN NOT =   SPACE )
                       MOVE    SPA-GMN-JYKNKBN
                                           TO  SPA-MST-TJYKN (IDX0)
                   END-IF
      *
                   IF    ( SPA-GMN-UTAGAIKBN NOT =   SPACE )
                       MOVE    SPA-GMN-UTAGAIKBN
                                           TO  SPA-MST-TUTAGAI (IDX0)
                   END-IF
      *
                   IF    ( SPA-GMN-GRPID NOT   =   SPACE )
                       IF    ( SPA-GMN-GRPID   =   "0" )
                           MOVE    SPACE           TO  WRK-GRPID
                       ELSE
                           MOVE    SPA-GMN-GRPID   TO  WRK-GRPID
                       END-IF
      *
                       IF    ( WRK-GRPID
                                   NOT = SPA-MST-TGRPID(IDX0))
      *
                           COMPUTE CNT-GROUPCHG
                               =   CNT-GROUPCHG    +   1
      *
                           MOVE    CNT-GROUPCHG
                                           TO  SPA-MST-TGROUPCHG(IDX0)
                       END-IF
                       MOVE    WRK-GRPID   TO  SPA-MST-TGRPID (IDX0)
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4101-BYOTRK-MUL-UPD-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ病名リスト更新処理
      *****************************************************************
       4101-BYOLST-UPD-SEC             SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       SPA-MST-MAX )
      *
               MOVE    SPA-MST-TBYOMEI (IDX0)
                                   TO  SPA-GMN-TBYOMEI (IDX0)
      *
               MOVE    SPA-MST-TJYKN (IDX0)
                                   TO  SPA-GMN-TJYKN (IDX0)
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   CONST-MKCD-MAX )
                   MOVE    SPA-MST-TBYOCD (IDX0 IDX7)
                                   TO  SPA-NAI-TBYOCD (IDX0 IDX7)
               END-PERFORM
      *
               MOVE    SPA-MST-TUTAGAI (IDX0)
                                   TO  SPA-GMN-TUTAGAI (IDX0)
               MOVE    SPA-MST-TGRPID (IDX0)
                                   TO  SPA-GMN-TGRPID (IDX0)
               MOVE    SPA-MST-TGROUPCHG (IDX0)
                                   TO  SPA-GMN-TGROUPCHG (IDX0)
               MOVE    SPA-MST-THOSOKU (IDX0)
                                   TO  SPA-GMN-THOSOKU (IDX0)
               MOVE    SPA-MST-THOSOKU-JYKN (IDX0)
                                   TO  SPA-GMN-THOSOKU-JYKN (IDX0)
               MOVE    SPA-MST-TBYOSTATUS (IDX0)
                                   TO  SPA-NAI-TBYOSTATUS (IDX0)
      *
           END-PERFORM
      *
           .
       4101-BYOLST-UPD-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ登録処理（追加）
      *****************************************************************
       4101-BYOTRK-INS-SEC             SECTION.
      *
           IF    ( SPA-MST-MAX        >=  CONST-BYOLST-MAX )
               MOVE    "0103"          TO  SPA-ERRCD
               GO  TO  4101-BYOTRK-INS-EXT
           END-IF
      *
           IF    ( SPA-MST-MAX
               +   SPA-NAI-BYOINF-MAX  >  CONST-BYOLST-MAX )
               MOVE    "0112"          TO  SPA-ERRCD
               GO  TO  4101-BYOTRK-INS-EXT
           END-IF
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   SPA-NAI-BYOINF-MAX )
      *
               COMPUTE SPA-MST-MAX
                   =   SPA-MST-MAX +   1
      *
               MOVE    SPA-MST-MAX TO  SPA-GMN-BYOLST-SEL
      *
               MOVE    IDXK        TO  IDXL
               PERFORM 4101-BYOTRK-SIN-UPD-SEC
      *
           END-PERFORM
      *
           .
       4101-BYOTRK-INS-EXT.
           EXIT.
      *****************************************************************
      *    病名リストＳＯＲＴ処理
      *****************************************************************
       4101-BYOLST-SRT-SEC             SECTION.
      *
           COMPUTE IDXS = SPA-MST-MAX + 1
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-MST-MAX )
               MOVE    IDX1       TO   SPA-MST-TIDX (IDX1)
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-MST-MAX - 1 )
               COMPUTE IDX-ST  =   IDX1    +   1
               PERFORM VARYING IDX2    FROM    IDX-ST  BY  1
                       UNTIL ( IDX2    >   SPA-MST-MAX )
      *
                   MOVE    SPA-MST-TSRTKEY (IDX1)
                                       TO  WRK-SRTKEY (1)
                   MOVE    SPA-MST-TSRTKEY (IDX2)
                                       TO  WRK-SRTKEY (2)
      *
                   IF    ( WRK-SRTKEY-TGRPID (1)   =   SPACE )
                       MOVE    HIGH-VALUE
                                       TO  WRK-SRTKEY-TGRPID (1)
                   END-IF
      *
                   IF    ( WRK-SRTKEY-TGRPID (2)   =   SPACE )
                       MOVE    HIGH-VALUE
                                       TO  WRK-SRTKEY-TGRPID (2)
                   END-IF
      *
                   IF    ( WRK-SRTKEY (1)  >   WRK-SRTKEY (2) )
                       MOVE    SPA-MST-OCC (IDX1)
                                      TO   SPA-MST-OCC (IDXS)
      *
                       MOVE    SPA-MST-OCC (IDX2)
                                      TO   SPA-MST-OCC (IDX1)
      *
                       MOVE    SPA-MST-OCC (IDXS)
                                      TO   SPA-MST-OCC (IDX2)
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-MST-MAX )
               MOVE    ZERO        TO   SPA-MST-TGROUPCHG (IDX1)
                                        SPA-MST-TIDX (IDX1)
           END-PERFORM
      *
           .
       4101-BYOLST-SRT-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストより削除処理
      *****************************************************************
       4101-BYODEL-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4101-BYODEL-EXT
           END-IF
      *
           INITIALIZE                  WRK-SAVE-G
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1   OR  2 )
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   SPA-GMN-BYOLST-MAX )
                        OR   ( SPA-NAI-TMSTKBN (IDX8)  NOT =   SPACE )
      *
                   IF   ( SPA-NAI-INPUT-MODE   =   1 )
                       IF    ( SPA-GMN-BYOLST-SEL  =   IDX8 )
                           CONTINUE
                       ELSE
                           COMPUTE WRK-SAVE-MAX
                               =   WRK-SAVE-MAX    +   1
                           MOVE IDX8   TO  WRK-SAVE-IDX (WRK-SAVE-MAX)
                       END-IF
                   ELSE
                       IF    ( SPA-NAI-SELNUM-FLG (IDX8)   =   1  )
                           CONTINUE
                       ELSE
                           COMPUTE WRK-SAVE-MAX
                               =   WRK-SAVE-MAX    +   1
                           MOVE IDX8   TO  WRK-SAVE-IDX (WRK-SAVE-MAX)
                       END-IF
                   END-IF
      *
               END-PERFORM
           ELSE
               MOVE    "0101"          TO  SPA-ERRCD
               GO  TO  4101-BYODEL-EXT
           END-IF
      *
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8    >   SPA-MST-MAX )
               IF    ( WRK-SAVE-MAX    >=  IDX8 )
                   MOVE    WRK-SAVE-IDX (IDX8)
                                       TO  IDX9
                   IF    ( IDX9        >   IDX8 )
                       MOVE    SPA-MST-OCC (IDX9)
                                       TO  SPA-MST-OCC (IDX8)
                   END-IF
               ELSE
                   INITIALIZE              SPA-MST-OCC (IDX8)
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-SAVE-MAX        TO  SPA-MST-MAX
      *
           MOVE    X04-BYOLST-ROW      TO  WRK-BYOLST-ROW
           PERFORM 3101-BYOLST-SEC
           MOVE    ZERO                TO  FLG-BYOLST-ROW
           MOVE    WRK-BYOLST-ROW      TO  X04-BYOLST-ROW
      *
           PERFORM 220-CLEAR-SEC
      *
           .
       4101-BYODEL-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ追加処理
      *****************************************************************
       4101-BYOLST-INS-SEC             SECTION.
      *
           IF    ( SPA-MST-MAX         >=  CONST-BYOLST-MAX )
               MOVE    "0103"          TO  SPA-ERRCD
           ELSE
               PERFORM 41011-BYOLST-INS-SEC
           END-IF
      *
           .
       4101-BYOLST-INS-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ追加処理
      *****************************************************************
       41011-BYOLST-INS-SEC             SECTION.
      *
           MOVE    SPA-MST-MAX     TO  WRK-BFMST-MAX
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SRCBYOLST-MAX )
                    OR   ( SPA-MST-MAX    >=  CONST-BYOLST-MAX )
      *
               IF    ( SPA-GMN-SRCBYOLST-SELECT (IDX1)   =   "T" )
                   MOVE    ZERO        TO  FLG-HIT
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   SPA-MST-MAX )
                            OR   ( FLG-HIT =   1 )
      *
                       IF    ( SPA-GMN-TSRCBYOMEI (IDX1)
                                       =   SPA-MST-TBYOMEI (IDX2) )
                        AND  ( SPA-GMN-SRCJYKNKBN
                                       =   SPA-MST-TJYKN(IDX2) )
                        AND  ( SPA-GMN-TSRCHOSOKU  (IDX1)
                                       =   SPA-MST-THOSOKU (IDX2) )
                           MOVE    1   TO  FLG-HIT
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( FLG-HIT     =   ZERO )
      *
                       PERFORM 410111-BYOLST-INS-SEC 
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM 4101-BYOLST-SRT-SEC
           MOVE    1                   TO  SPA-GMN-BYOLST-PAGE
           PERFORM 3101-BYOLST-SEC
      *
           INITIALIZE      SPA-GMN-SRCBYOLST-SELECT-G
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF    ( SPA-MST-MAX         >=  CONST-BYOLST-MAX )
            AND  ( WRK-BFMST-MAX        <  CONST-BYOLST-MAX )
               MOVE    "0102"          TO  SPA-ERRCD
           END-IF
      *
           .
       41011-BYOLST-INS-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ追加処理（全て）
      *****************************************************************
       4101-BYOLST-ALLINS-SEC          SECTION.
      *
           IF    ( SPA-MST-MAX         >=  CONST-BYOLST-MAX )
               MOVE    "0103"          TO  SPA-ERRCD
           ELSE
               PERFORM 41011-BYOLST-ALLINS-SEC
           END-IF
      *
           .
       4101-BYOLST-ALLINS-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ追加処理（全て）
      *****************************************************************
       41011-BYOLST-ALLINS-SEC         SECTION.
      *
           MOVE    SPA-MST-MAX     TO  WRK-BFMST-MAX
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SRCBYOLST-MAX )
                    OR   ( SPA-MST-MAX  >=  CONST-BYOLST-MAX )
      *
               MOVE    ZERO        TO  FLG-HIT
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   SPA-MST-MAX )
                        OR   ( FLG-HIT =   1 )
      *
                   IF    ( SPA-GMN-TSRCBYOMEI (IDX1)
                                       =   SPA-MST-TBYOMEI (IDX2) )
                    AND  ( SPA-GMN-SRCJYKNKBN
                                       =   SPA-MST-TJYKN(IDX2) )
                    AND  ( SPA-GMN-TSRCHOSOKU (IDX1)
                                       =   SPA-MST-THOSOKU (IDX2) )
                       MOVE    1   TO  FLG-HIT
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-HIT     =   ZERO )
      *
                   PERFORM 410111-BYOLST-INS-SEC 
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 4101-BYOLST-SRT-SEC
           MOVE    1                   TO  SPA-GMN-BYOLST-PAGE
           PERFORM 3101-BYOLST-SEC
      *
           INITIALIZE      SPA-GMN-SRCBYOLST-SELECT-G
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF    ( SPA-MST-MAX      >=  CONST-BYOLST-MAX )
            AND  ( WRK-BFMST-MAX     <  CONST-BYOLST-MAX )
               MOVE    "0102"          TO  SPA-ERRCD
           END-IF
      *
           .
       41011-BYOLST-ALLINS-EXT.
           EXIT.
      *****************************************************************
      *    病名を病名リストへ追加処理
      *****************************************************************
       410111-BYOLST-INS-SEC            SECTION.
      *
           COMPUTE SPA-MST-MAX
               =   SPA-MST-MAX  +   1
      *
           MOVE    1            TO  SPA-MST-UPD
      *
           MOVE    SPA-MST-MAX  TO  IDX-MAX
      *
           MOVE    SPA-GMN-TSRCBYOMEI (IDX1)
                               TO  SPA-MST-TBYOMEI (IDX-MAX)
           MOVE    SPA-GMN-SRCJYKNKBN
                               TO  SPA-MST-TJYKN   (IDX-MAX)
           MOVE    SPA-GMN-SRCUTAGAIKBN
                               TO  SPA-MST-TUTAGAI (IDX-MAX)
      *
           MOVE    SPACE       TO  SPA-MST-THOSOKU (IDX-MAX)
      *
           MOVE    "1"         TO  SPA-MST-THOSOKU-JYKN (IDX-MAX)
      *
           EVALUATE    SPA-GMN-SRCBYOLST-PTN
           WHEN    "2"
           WHEN    "3"
           WHEN    "M"
               PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >       CONST-MKCD-MAX )
      *
                   MOVE    SPA-NAI-TSRCBYOCD (IDX1 IDX0)
                                   TO  SPA-MST-TBYOCD (IDX-MAX IDX0)
               END-PERFORM
      *
               IF    ( SPA-GMN-SRCBYOLST-PTN   =   "2" )
                   INITIALIZE              ORCSBYOMEIAREA
                   MOVE    "2" TO  LNK-BYO-SYORI
                   MOVE    SPA-NAI-TSRCBYOCD (IDX1 1)
                                   TO  LNK-BYO-TBYOMEICD (1)
      *
                   CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                                   SPA-AREA
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                   TO  SPA-MST-TBYOSTATUS (IDX-MAX)
               ELSE
                   MOVE    SPA-NAI-TSRCBYOSTATUS (IDX1)
                                   TO  SPA-MST-TBYOSTATUS (IDX-MAX)
               END-IF
      *
           WHEN    OTHER
      *
      *        病名からコード検索処理
               INITIALIZE                      ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    SPA-MST-TBYOMEI (IDX-MAX)
                                           TO  LNK-BYO-BYOMEI
      *
               CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                               SPA-AREA
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >       LNK-BYO-BYOMEI-MAX )
      *
                       MOVE    LNK-BYO-TBYOMEICD (IDX0)
                                   TO  SPA-MST-TBYOCD (IDX-MAX IDX0)
      *
               END-PERFORM
      *
               MOVE    LNK-BYO-BYOMEIHENFLG
                                   TO  SPA-MST-TBYOSTATUS (IDX-MAX)
           END-EVALUATE
      *
      *
           .
       410111-BYOLST-INS-EXT.
           EXIT.
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       4101-NYUGAIKBN-SEC              SECTION.
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           COMPUTE WRK-CMB-IDX    =    WRK-CMB-IDX +   1
           IF    ( WRK-CMB-IDX    >    SPA-GMN-NYUGAIKBN-MAX )
               MOVE    SPACE       TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               MOVE    SPA-GMN-NYUGAIKBNLST (WRK-CMB-IDX)
                                   TO  SPA-GMN-NYUGAIKBN-G
           END-IF
      *
           
      *
           .
       4101-NYUGAIKBN-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       4100-PREV-PAGE-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BYOLST-PAGE  >   1 )
                   COMPUTE SPA-GMN-BYOLST-PAGE
                       =   SPA-GMN-BYOLST-PAGE  -   1
                   PERFORM 3101-BYOLST-SEC
               END-IF
           END-IF
      *
           .
       4100-PREV-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    後頁処理
      *****************************************************************
       4100-NEXT-PAGE-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BYOLST-NEXT  =   1 )
                   COMPUTE SPA-GMN-BYOLST-PAGE
                       =   SPA-GMN-BYOLST-PAGE  +   1
                   PERFORM 3101-BYOLST-SEC
               END-IF
           END-IF
      *
           .
       4100-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       4101-PREV-PAGE-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SRCBYOLST-PAGE  >   1 )
                   COMPUTE SPA-GMN-SRCBYOLST-PAGE
                       =   SPA-GMN-SRCBYOLST-PAGE  -   1
      *
                   PERFORM 3101-SRTKBNLST-SET-SEC
      *
               END-IF
           END-IF
      *
           .
       4101-PREV-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    後頁処理
      *****************************************************************
       4101-NEXT-PAGE-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SRCBYOLST-NEXT  =   1 )
                   COMPUTE SPA-GMN-SRCBYOLST-PAGE
                       =   SPA-GMN-SRCBYOLST-PAGE  +   1
      *
                   PERFORM 3101-SRTKBNLST-SET-SEC
      *
               END-IF
           END-IF
      *
           .
       4101-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    一致条件区分コンボチェック処理
      *****************************************************************
       4200-JYKNKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-JYKNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYKNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JYKNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-JYKNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い区分コンボチェック処理
      *****************************************************************
       4200-UTAGAIKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-UTAGAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-UTAGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-UTAGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-UTAGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    グループＩＤコンボチェック処理
      *****************************************************************
       4200-GRPIDLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-GRPID-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-GRPIDL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-GRPIDLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-GRPIDLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NYUGAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適応病名区分コンボチェック処理
      *****************************************************************
       4200-TEKBYOKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-TEKBYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEKBYOKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TEKBYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TEKBYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ソート区分コンボチェック処理
      *****************************************************************
       4200-SRTKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRCBYOLST-PTN   =   "3" )
      *
                   IF    ( SPA-GMN-SRTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                       MOVE    SPA-GMN-SRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   END-IF
               ELSE
                   IF    ( SPA-GMN-SRTKBNL (IDX-CMB)(1:2)
                                   =   WRK-CMB-CD (1:2) )
      *
                       MOVE    SPA-GMN-SRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソース区分コンボチェック処理
      *****************************************************************
       4200-SRCBYOKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRCBYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRCBYOKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRCBYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRCBYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約病名コードチェック処理
      *****************************************************************
       4200-YYKBYOCD-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   TBL-BYOMEICD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( TBL-YYK-BYOMEICD (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE   TBL-YYK-BYOMEI(IDX-CMB)
                                           TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YYKBYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "X01"               TO  SPA-SAKIPG
           MOVE    "X04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
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
           MOVE    1                   TO  FLG-CDLST-ROW
           INITIALIZE                  SPA-GMN-CDLST-AREA
                                       SPA-GMN-BYO-INPUT-AREA
                                       SPA-NAI-BYO-INPUT-AREA
      *
           INITIALIZE                  SPA-GMN-BYOLST-SEL
      *
           MOVE    SPA-GMN-JYKNKBNLST (1)
                                   TO  SPA-GMN-JYKNKBN-G
      *
           MOVE    SPA-GMN-UTAGAIKBNLST (1)
                                   TO  SPA-GMN-UTAGAIKBN-G
      *
           MOVE    SPA-GMN-GRPIDLST (1)
                                   TO  SPA-GMN-GRPID-G
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ検索画面
      *****************************************************************
       409-X07-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE          SPA-X01-USERBYOMEI-G
               MOVE    CONST-SELKBN-BYOMEI
                                   TO  SPA-X01-SELKBN
               PERFORM 4091-X07-SENI-SEC
           END-IF
      *
           .
       409-X07-SENI-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ検索画面
      *****************************************************************
       4091-X07-SENI-SEC               SECTION.
      *
           MOVE    ZERO            TO  SPA-X01-BYOSEL
           IF    ( SPA-GMN-BYOLST-SEL  >   ZERO )
            OR   ( SPA-NAI-INPUT-MODE  =   2 )
            OR   ( SPA-X01-SELKBN      =   CONST-SELKBN-HOSOKU )
               MOVE    1           TO  SPA-X01-BYOSEL
           END-IF
      *
           IF    ( SPA-X01-SELKBN  =   CONST-SELKBN-BYOMEI )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-BYOCD-MAX )
      *
                   MOVE    SPA-GMN-BYOCD (IDX0)
                                   TO  SPA-X01-INPUTCD (IDX0)
      *
               END-PERFORM
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X04         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGX07"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X04
      *
           MOVE    "X07"               TO  SPA-SAKIPG
           MOVE    "X04"               TO  SPA-MOTOPG
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4091-X07-SENI-EXT.
           EXIT.
      *****************************************************************
      *    例外薬剤登録画面
      *****************************************************************
       409-X06-SENI-SEC                SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X04         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGX06"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X04
      *
           MOVE    "X06"               TO  SPA-SAKIPG
           MOVE    "X04"               TO  SPA-MOTOPG
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-X06-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目複写処理
      *****************************************************************
       409-X03-SENI-SEC                SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
      *    チェック項目複写画面初期処理
           PERFORM 4091-X03-INIT-SEC
      *
           MOVE    "X03"               TO  SPA-SAKIPG
           MOVE    "X04"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-X03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目複写画面初期処理
      *****************************************************************
       4091-X03-INIT-SEC           SECTION.
      *
      *    画面の初期化と左コラムリストの設定を行う
           INITIALIZE                  SPA-X03-GMN-AREA
                                       SPA-X03-NAI-AREA
      *
           MOVE    SPA-GMN-CHKKBN      TO  SPA-X03-GMN-LCHKKBNMEI
           MOVE    SPA-X01-STYUKYMDH   TO  SPA-X03-GMN-LYUKOSTYMD
           MOVE    SPA-X01-EDYUKYMDH   TO  SPA-X03-GMN-LYUKOEDYMD
           MOVE    SPA-X01-NAME        TO  SPA-X03-GMN-LNAME
      *
           MOVE    SPA-MST-MAX         TO  SPA-X03-GMN-LCHKLST-MAX
      *
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   SPA-X03-GMN-LCHKLST-MAX )
      *
               MOVE    IDXE            TO  SPA-X03-GMN-TLCHK-NUM  (IDXE)
               MOVE    SPA-MST-TBYOMEI (IDXE)
                                       TO  SPA-X03-GMN-TLCHK-NAME (IDXE)
               MOVE    SPA-MST-TJYKN   (IDXE)
                                       TO  SPA-X03-GMN-TLCHK-JYKN (IDXE)
      *
               MOVE    SPA-MST-TBYOCD  (IDXE 1)
                                       TO  SPA-X03-NAI-TLCHK-SRYCD(IDXE)
               PERFORM VARYING IDXF    FROM    1   BY  1
                       UNTIL ( IDXF    >   CONST-MKCD-MAX )
      *
                   MOVE    SPA-MST-TBYOCD  (IDXE IDXF)
                                       TO  SPA-X03-NAI-TLCHK-BYOCD
                                                             (IDXE IDXF)
      *
               END-PERFORM
      *
               MOVE    SPA-MST-TBYOSTATUS  (IDXE)
                                       TO  SPA-X03-NAI-TLCHK-BYOSTATUS
                                                                  (IDXE)
               IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
                   MOVE    SPA-MST-TUTAGAI  (IDXE)
                                       TO  SPA-X03-NAI-TLCHK-UTAGAI
                                                                  (IDXE)
                   MOVE    SPA-MST-THOSOKU  (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-HOSOKU
                                                                  (IDXE)
                   MOVE    SPA-MST-THOSOKU-JYKN  (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-HOSOKU-JYKN
                                                                  (IDXE)
                   MOVE    SPA-MST-TGRPID (IDXE)
                                       TO  SPA-X03-GMN-TLCHK-GRPID
                                                                  (IDXE)
               ELSE
                   MOVE    "1"     TO  SPA-X03-NAI-TLCHK-UTAGAI   (IDXE)
                   MOVE    SPACE   TO  SPA-X03-GMN-TLCHK-HOSOKU   (IDXE)
                   MOVE    "1"     TO  SPA-X03-GMN-TLCHK-HOSOKU-JYKN
                                                                  (IDXE)
                   MOVE    SPACE   TO  SPA-X03-GMN-TLCHK-GRPID    (IDXE)
               END-IF
      *
           END-PERFORM
      *
           .
       4091-X03-INIT-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト選択解除処理
      *****************************************************************
       490-CANCEL-SEL-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-SRCBYOLST-SELECT-G
      *
           .
       490-CANCEL-EXT.
           EXIT.
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-XIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      * 
      *    チェックマスタ削除 
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-X01-CHKKBN          TO  CHKSND-CHKKBN
           MOVE    SPA-X01-SRYCD           TO  CHKSND-SRYCD
           MOVE    SPA-X01-SEL-STYUKYMD    TO  CHKSND-YUKOSTYMD
           MOVE    SPA-X01-SEL-EDYUKYMD    TO  CHKSND-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC              TO  MCPDATA-REC
           MOVE    "tbl_chksnd"            TO  MCP-TABLE
           MOVE    "del11"                 TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "1002"              TO  SPA-ERRCD
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"                   TO   MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4031-DEL-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       490-KAKUTEI-SEC                 SECTION.
      *
           PERFORM 490-KANREN-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-X01-MOTOPG  =   SPACE )
                   MOVE    "1001"      TO  SPA-XIDCD
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       490-KANREN-CHK-SEC         SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BYOMEI  NOT =   SPACE )
                   IF    ( SPA-X01-MOTOPG  =   SPACE )
                       MOVE    "0107"      TO  SPA-ERRCD
                   ELSE
                       PERFORM 4101-BYOTRK-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE  NOT =   ZERO )
                   MOVE    "0109"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-BYOLST-MAX  >   ZERO )
                 OR  ( SPA-GMN-NYUGAIKBN   =   "0" )
                 OR  ( SPA-GMN-TEKBYOKBN   =   "1" OR "2" )
                 OR  ( SPA-X06-MAX         >   ZERO )
                       CONTINUE
               ELSE
                   MOVE    "0108"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    ZERO            TO  FLG-UNGROUP
                                           FLG-GROUP
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-MST-MAX )
      *
                   IF    ( SPA-MST-TGRPID (IDX0)   =   SPACE )
                       MOVE    1       TO  FLG-UNGROUP
                   ELSE
                       MOVE    1       TO  FLG-GROUP
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-GROUP       =   1 )
                AND  ( FLG-UNGROUP     =   1 )
                   MOVE    "0111"      TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
       490-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       411-PRINT-SEC               SECTION.
      *
           PERFORM 490-KANREN-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "2001"          TO  SPA-XIDCD
           END-IF
      *
           .
       411-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4111-PRINT-SEC                  SECTION.
      *
           PERFORM 4900-KOUSHIN-SEC
      *
           IF    ( SPA-ERRCD           =    SPACE )
      *
               IF    ( SPA-X01-SELNUM      =   ZERO )
                   MOVE    99              TO  SPA-X01-SELNUM
                   MOVE    SPA-X01-STYUKYMD
                                           TO  SPA-X01-SEL-STYUKYMD
                   MOVE    SPA-X01-EDYUKYMD
                                           TO  SPA-X01-SEL-EDYUKYMD
               END-IF
      *
               INITIALIZE                      SS013-AREA
               MOVE    "DBCREATE"          TO  SS013-FUNC
               MOVE    SPA-X01-CHKKBN      TO  SS013-CHKKBN
               MOVE    SPA-X01-SRYCD       TO  SS013-SRYCD
               MOVE    SPA-X01-KJNYMD      TO  SS013-KJNYMD
               CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
      *
               INITIALIZE                  ORCHXLSTAREA
               MOVE    SPA-GMN-CHKKBN      TO  HXLST-CHKKBN
               MOVE    SPA-GMN-SRYCD       TO  HXLST-SRYCD
               MOVE    "1"                 TO  HXLST-PROCKBN
               MOVE    SPA-X01-KJNYMD      TO  HXLST-KJNYMD
               MOVE    SS013-TEMP-NAME     TO  HXLST-TEMP-NAME
               MOVE    SS013-TEMP-KEY      TO  HXLST-TEMP-KEY
               CALL    "ORCHXLST"      USING   SPA-AREA
                                               ORCHXLSTAREA
      *
               INITIALIZE                      SS013-AREA
               MOVE    "DBDROP"            TO  SS013-FUNC
               MOVE    SPA-X01-CHKKBN      TO  SS013-CHKKBN
               MOVE    SPA-X01-SRYCD       TO  SS013-SRYCD
               MOVE    SPA-X01-KJNYMD      TO  SS013-KJNYMD
               CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
           END-IF
      *
           .
       4111-PRINT-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           PERFORM 4900-KOUSHIN-SEC
      *
           IF    ( SPA-ERRCD           =    SPACE )
               IF    ( SPA-X01-MOTOPG  =   SPACE )
                   MOVE   "JOIN"               TO   MCP-PUTTYPE
                   PERFORM 210-BACK
               ELSE
                   MOVE   "CHANGE"             TO   MCP-PUTTYPE
                   PERFORM 210-BACK
               END-IF
           END-IF
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       4900-KOUSHIN-SEC                SECTION.
      *
      *    チェックマスタ削除
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-X01-CHKKBN          TO  CHKSND-CHKKBN
           MOVE    SPA-X01-SRYCD           TO  CHKSND-SRYCD
           IF    ( SPA-X01-SEL-STYUKYMD    =    SPACE )
               MOVE    SPA-X01-STYUKYMD
                                       TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD
                                       TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           END-IF
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC              TO  MCPDATA-REC
           MOVE    "tbl_chksnd"            TO  MCP-TABLE
           MOVE    "del11"                 TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  4900-KOUSHIN-EXT
           END-IF
      *
      *    チェックマスタ追加
           MOVE    ZERO                TO  FLG-TEMPTBL
           PERFORM 4901-KOUSHIN-SEC
      *
           IF    ( SPA-ERRCD           =    SPACE )
               IF    ( SPA-X06-MAX         >    ZERO )
                   PERFORM 4901-REIGAIYKZ-KOUSHIN-SEC
               END-IF
           END-IF
      *
           .
       4900-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       4901-KOUSHIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDXD
      *
           MOVE    SPACE               TO  WRK-GRPCHKKBN
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   SPA-MST-MAX )
                    OR   ( WRK-GRPCHKKBN   NOT =   SPACE )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
               IF    ( SPA-X01-CHKKBN      =   "1" OR "2" )
                AND  ( SPA-MST-TGRPID (IDXD)
                                       NOT =   SPACE )
                   MOVE    "1"         TO  WRK-GRPCHKKBN
                END-IF
           END-PERFORM
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   SPA-MST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
      *
               INITIALIZE                  CHKSND-REC
      *
               MOVE    SPA-X01-CHKKBN      TO  CHKSND-CHKKBN
               MOVE    SPA-X01-SRYCD       TO  CHKSND-SRYCD
               MOVE    SPA-X01-STYUKYMD    TO  CHKSND-YUKOSTYMD
               MOVE    SPA-X01-EDYUKYMD    TO  CHKSND-YUKOEDYMD
               MOVE    "1"                 TO  CHKSND-CDKBN
      *
               MOVE    IDXD                TO  CHKSND-RENNUM
      *
               PERFORM 4901-CD-KEY-SEC
      *
               MOVE    WRK-KEY-CD-G        TO  CHKSND-CD
               MOVE    SPA-MST-TBYOMEI (IDXD)
                                           TO  CHKSND-BYOMEI
      *
               MOVE    SPA-MST-TJYKN   (IDXD)
                                           TO  CHKSND-COMPKBN
               IF    ( SPA-X01-CHKKBN      =   "6" )
                   MOVE    "1"             TO  CHKSND-UTAGAIKBN
               ELSE
                   MOVE    SPA-MST-TUTAGAI   (IDXD)
                                           TO  CHKSND-UTAGAIKBN
               END-IF
               MOVE    WRK-GRPCHKKBN       TO  CHKSND-GRPCHKKBN
               IF    ( SPA-X01-CHKKBN      =   "1" OR "2" )
                   MOVE    SPA-MST-TGRPID (IDXD)
                                           TO  CHKSND-GRPID
                   MOVE    SPA-MST-THOSOKU (IDXD)
                                           TO  CHKSND-HOSOKU
                   MOVE    SPA-MST-THOSOKU-JYKN (IDXD)
                                           TO  CHKSND-HOSOKU-COMPKBN
               ELSE
                   MOVE    SPACE           TO  CHKSND-GRPID
                   MOVE    SPACE           TO  CHKSND-HOSOKU
                   MOVE    "1"             TO  CHKSND-HOSOKU-COMPKBN
               END-IF
               MOVE    SPA-MST-TBYOSTATUS (IDXD)
                                           TO  CHKSND-BYOMEIHENFLG
               MOVE    SPA-GMN-NYUGAIKBN   TO  CHKSND-NYUGAIKBN
               IF    ( SPA-GMN-TEKBYOKBN   NOT =   "9" )
                   MOVE    SPA-GMN-TEKBYOKBN
                                           TO  CHKSND-TEKBYOKBN
               END-IF
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  CHKSND-CREYMD
                                               CHKSND-UPYMD
               MOVE    SMCNDATE-HMS        TO  CHKSND-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
      *
               MOVE    CHKSND-REC          TO  MCPDATA-REC
      *
               IF    ( FLG-TEMPTBL         =   ZERO )
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "1001"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "tmpins2"       TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =    SPACE )
      *
               IF    ( SPA-MST-MAX         >   ZERO )
                OR   ( SPA-GMN-NYUGAIKBN   =   "0" )
                OR   ( SPA-GMN-TEKBYOKBN   =   "1" )
                OR   ( SPA-X06-MAX         >   ZERO )
      *
                   INITIALIZE                  CHKSND-REC
                   MOVE    SPA-X01-CHKKBN      TO  CHKSND-CHKKBN
                   MOVE    SPA-X01-SRYCD       TO  CHKSND-SRYCD
                   MOVE    SPA-X01-STYUKYMD    TO  CHKSND-YUKOSTYMD
                   MOVE    SPA-X01-EDYUKYMD    TO  CHKSND-YUKOEDYMD
                   MOVE    "0"                 TO  CHKSND-CDKBN
                   MOVE    SPA-MST-MAX         TO  CHKSND-RENNUM
                   MOVE    WRK-GRPCHKKBN       TO  CHKSND-GRPCHKKBN
                   MOVE    SPA-GMN-NYUGAIKBN   TO  CHKSND-NYUGAIKBN
                   IF    ( SPA-GMN-TEKBYOKBN   NOT =   "9" )
                       MOVE    SPA-GMN-TEKBYOKBN
                                               TO  CHKSND-TEKBYOKBN
                   END-IF
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD        TO  CHKSND-CREYMD
                                                   CHKSND-UPYMD
                   MOVE    SMCNDATE-HMS        TO  CHKSND-UPHMS
                   MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
      *
                   MOVE    CHKSND-REC          TO  MCPDATA-REC
      *
                   IF    ( FLG-TEMPTBL         =   ZERO )
                       MOVE    "tbl_chksnd"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC          NOT =   ZERO )
                           MOVE    "1005"      TO  SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE    "tbl_chksnd"    TO  MCP-TABLE
                       MOVE    "tmpins2"       TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    例外薬剤更新処理
      *****************************************************************
       4901-REIGAIYKZ-KOUSHIN-SEC      SECTION.
      *
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   SPA-X06-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE                  CHKSND-REC
               MOVE    SPA-X01-CHKKBN      TO  CHKSND-CHKKBN
               MOVE    SPA-X01-SRYCD       TO  CHKSND-SRYCD
               MOVE    SPA-X01-STYUKYMD    TO  CHKSND-YUKOSTYMD
               MOVE    SPA-X01-EDYUKYMD    TO  CHKSND-YUKOEDYMD
               MOVE    "3"                 TO  CHKSND-CDKBN
      *
               MOVE    IDXD                TO  CHKSND-RENNUM
      *
               MOVE    SPA-X06-TEKCD(IDXD) TO  CHKSND-CD
               MOVE    SPA-GMN-NYUGAIKBN   TO  CHKSND-NYUGAIKBN
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  CHKSND-CREYMD
                                               CHKSND-UPYMD
               MOVE    SMCNDATE-HMS        TO  CHKSND-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
               MOVE    CHKSND-REC          TO  MCPDATA-REC
               MOVE    "tbl_chksnd"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    "1004"          TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           .
       4901-REIGAIYKZ-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    適用コード編集処理
      *****************************************************************
       4901-CD-KEY-SEC                 SECTION.
      *
           INITIALIZE                  WRK-KEY-CD-G
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
                    OR   ( SPA-MST-TBYOCD (IDXD IDX0)  =   SPACE )
      *
               MOVE    SPA-MST-TBYOCD (IDXD IDX0)
                                   TO  WRK-KEY-CD    (IDX0)
               IF    ( IDX0        >   1 )
                   MOVE    "-"     TO  WRK-KEY-CDSEP (IDX0 - 1)
               END-IF
      *
           END-PERFORM
      *
           .
       4901-CD-KEY-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-XIDCD       NOT =   SPACE )
               PERFORM 520-XIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE   X04-BYOLST-ROW   TO  WRK-BYOLST-ROW
           MOVE   X04-SRCBYOLST-ROW 
                                   TO  WRK-SRCBYOLST-ROW
           MOVE   X04-CDLST-ROW    TO  WRK-CDLST-ROW
      *
           MOVE   SPACE            TO  X04
           INITIALIZE                  X04
      *
           EVALUATE    FLG-BYOLST-ROW
           WHEN    1
               MOVE    ZERO            TO  X04-BYOLST-ROW
               MOVE    ZERO            TO  X04-BYOLST-ROWATTR
           WHEN    2
               MOVE    SPA-GMN-BYOLST-MAX
                                       TO  X04-BYOLST-ROW
               MOVE    ZERO            TO  X04-BYOLST-ROWATTR
           WHEN   OTHER
               MOVE    WRK-BYOLST-ROW  TO  X04-BYOLST-ROW
               MOVE    ZERO            TO  X04-BYOLST-ROWATTR
           END-EVALUATE
      *
           IF    ( FLG-SRCBYOLST-ROW   =   1 )
               MOVE    ZERO            TO  X04-SRCBYOLST-ROW
               MOVE    ZERO            TO  X04-SRCBYOLST-ROWATTR
           ELSE
               MOVE    WRK-SRCBYOLST-ROW
                                       TO  X04-SRCBYOLST-ROW
               MOVE    ZERO            TO  X04-SRCBYOLST-ROWATTR
           END-IF
      *
           IF    ( FLG-CDLST-ROW       =   1 )
               MOVE    ZERO            TO  X04-CDLST-ROW
               MOVE    ZERO            TO  X04-CDLST-ROWATTR
           ELSE
               MOVE    WRK-CDLST-ROW   TO  X04-CDLST-ROW
               MOVE    ZERO            TO  X04-CDLST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  X04-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  X04-EDYUKYMD
           MOVE    SPA-GMN-CHKKBN      TO  X04-CHKKBN
           STRING  SPA-GMN-SRYCD       DELIMITED BY SPACE
                   " "                 DELIMITED BY SIZE
                   SPA-GMN-NAME        DELIMITED BY SPACE
           INTO    X04-NAME
           END-STRING
      *
           MOVE    SPA-GMN-NYUGAIKBN-G TO  X04-NYUGAIKBN
      *
           MOVE    SPA-GMN-NYUGAIKBN-MAX TO  X04-NYUGAIKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-NYUGAIKBN-MAX )
      *
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX0)
                                       TO  X04-NYUGAIKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-TEKBYOKBN-G TO  X04-TEKBYOKBN
      *
           MOVE    SPA-GMN-TEKBYOKBN-MAX TO  X04-TEKBYOKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-TEKBYOKBN-MAX )
      *
               MOVE    SPA-GMN-TEKBYOKBNLST (IDX0)
                                       TO  X04-TEKBYOKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SELNUM      TO  X04-SELNUM
      *
           IF    ( SPA-GMN-BYOSTATUS   =   "1" )
               MOVE    "病名編集"      TO  X04-BYOSTATUS
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-BYOCD-MAX )
      *
               MOVE    SPA-GMN-BYOCD (IDX0)
                                       TO  X04-BYOMEICD (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-BYOMEI          TO  X04-BYOMEI
           MOVE    SPA-GMN-JYKNKBN-G       TO  X04-JYKNKBN
           MOVE    SPA-GMN-HOSOKU          TO  X04-HOSOKU
           MOVE    SPA-GMN-HOSOKU-JYKNKBN-G
                                           TO  X04-HOSOKU-JYKNKBN
           MOVE    SPA-GMN-UTAGAIKBN-G     TO  X04-UTAGAIKBN
           MOVE    SPA-GMN-GRPID-G       TO  X04-GRPID
           MOVE    SPA-GMN-SEL-BYOMEI      TO  X04-SEL-BYOMEI
           MOVE    SPA-GMN-SRTKBN-G        TO  X04-SRTKBN
           MOVE    SPA-GMN-SRCBYOKBN-G     TO  X04-SRCBYOKBN
           MOVE    SPA-GMN-SRCJYKNKBN-G    TO  X04-SRCJYKNKBN
           MOVE    SPA-GMN-SRCUTAGAIKBN-G  TO  X04-SRCUTAGAIKBN
      *
           MOVE    SPA-GMN-JYKNKBN-MAX TO  X04-JYKNKBN-COUNT
                                           X04-HOSOKU-JYKNKBN-COUNT
                                           X04-SRCJYKNKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-JYKNKBN-MAX )
      *
               MOVE    SPA-GMN-JYKNKBNLST (IDX0)
                                   TO  X04-JYKNKBN-ITEM (IDX0)
                                       X04-HOSOKU-JYKNKBN-ITEM (IDX0)
                                       X04-SRCJYKNKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-UTAGAIKBN-MAX TO  X04-UTAGAIKBN-COUNT
                                             X04-SRCUTAGAIKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-UTAGAIKBN-MAX )
      *
               MOVE    SPA-GMN-UTAGAIKBNLST (IDX0)
                                       TO  X04-UTAGAIKBN-ITEM (IDX0)
                                           X04-SRCUTAGAIKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-GRPID-MAX TO  X04-GRPID-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-GRPID-MAX )
      *
               MOVE    SPA-GMN-GRPIDLST (IDX0)
                                       TO  X04-GRPID-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRTKBN-MAX  TO  X04-SRTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SRTKBN-MAX )
      *
               MOVE    SPA-GMN-SRTKBNLST (IDX0)
                                       TO  X04-SRTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRCBYOKBN-MAX
                                       TO  X04-SRCBYOKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SRCBYOKBN-MAX )
      *
               MOVE    SPA-GMN-SRCBYOKBNLST (IDX0)
                                       TO  X04-SRCBYOKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-BYOLST-MAX  TO  X04-BYOLST-COUNT
      *
           MOVE    ZERO                TO  IDXM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-BYOLST-MAX )
      *
               COMPUTE IDXM  = IDXM    +   1
      *
               IF    ( SPA-GMN-TNUM (IDX0)
                                   NOT =   ZERO )
                   INITIALIZE          WRK-PRT-NO-G
                   MOVE    SPA-GMN-TNUM (IDX0)
                                       TO  WRK-PRT-NO
      *
                   IF    ( SPA-NAI-INPUT-MODE  =   2 )
                       IF    ( SPA-NAI-SELNUM-FLG (IDX0)   =   "1" )
                           MOVE    " *"    TO  WRK-PRT-MARK
                       ELSE
                           MOVE    SPACE   TO  WRK-PRT-MARK
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PRT-NO-G    TO  X04-TNO   (IDXM)
      *
               END-IF
      *
               MOVE    SPACE       TO  WRK-BYOMEI-JYKN
                                       WRK-HOSOKU-JYKN
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-JYKNKBN-MAX )
      *
                   IF    ( SPA-GMN-TJYKN (IDX0)
                                       =   CONST-JYKNKBNCD (IDX1))
                       MOVE    CONST-JYKNKBNMEI2 (IDX1)
                                       TO  WRK-BYOMEI-JYKN
                   END-IF
      *
                   IF    ( SPA-GMN-THOSOKU-JYKN (IDX0)
                                       =   CONST-JYKNKBNCD (IDX1))
                       MOVE    CONST-JYKNKBNMEI2 (IDX1)
                                       TO  WRK-HOSOKU-JYKN
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SPA-GMN-THOSOKU (IDX0)  =   SPACE )
                   MOVE    SPA-GMN-TBYOMEI (IDX0)
                                       TO  X04-TBYOMEI(IDXM)
      *
                   MOVE    WRK-BYOMEI-JYKN
                                       TO  X04-TJYKN(IDXM)
               ELSE
                   STRING  SPA-GMN-TBYOMEI (IDX0)  DELIMITED BY SPACE
                           "（"                    DELIMITED BY SIZE
                           SPA-GMN-THOSOKU (IDX0)  DELIMITED BY SPACE
                           "）"                    DELIMITED BY SIZE
                   INTO    X04-TBYOMEI(IDXM)
                   END-STRING
      *
                   STRING  WRK-BYOMEI-JYKN  DELIMITED BY SPACE
                           "（"             DELIMITED BY SIZE
                           WRK-HOSOKU-JYKN  DELIMITED BY SPACE
                           "）"             DELIMITED BY SIZE
                   INTO    X04-TJYKN(IDXM)
                   END-STRING
      *
               END-IF
      *
               EVALUATE    SPA-GMN-TUTAGAI (IDX0)
               WHEN    "0"
                   IF    ( SPA-NAI-SYS-UTAGAIKBN   =   "1" )
                       MOVE    SPACE   TO  X04-TUTAGAI(IDXM)
                   ELSE
                       MOVE    "×"    TO  X04-TUTAGAI(IDXM)
                   END-IF
               WHEN    "1"
                   MOVE    SPACE       TO  X04-TUTAGAI(IDXM)
               WHEN    "2"
                       MOVE    "×"    TO  X04-TUTAGAI(IDXM)
               END-EVALUATE
      *
               IF    ( SPA-NAI-TMSTKBN (IDX0) =   SPACE )
                   MOVE    SPA-GMN-TGRPID (IDX0)
                                       TO  X04-TMARK (IDXM)
               ELSE
                   IF    ( SPA-GMN-TNUM (IDX0)    NOT =   ZERO )
                       MOVE    "・"    TO  X04-TMARK (IDXM)
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-BYOLST-SEL  =   IDX0 )
                   MOVE    "T"         TO  X04-BYOLST-SELECT (IDXM)
               ELSE
                   MOVE    "F"         TO  X04-BYOLST-SELECT (IDXM)
                   IF    ( SPA-NAI-INPUT-MODE  NOT =   2 )
                       IF    ( SPA-GMN-BYOMEI
                                       =   SPA-GMN-TBYOMEI (IDX0) )
                        AND  ( SPA-GMN-HOSOKU
                                       =   SPA-GMN-THOSOKU (IDX0) )
                           MOVE    "==>" 
                                       TO  X04-TNO    (IDXM)
                           MOVE    "<="
                                       TO  X04-TUTAGAI(IDXM) (3:)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    SPA-GMN-SRCBYOLST-PTN
           WHEN    "2"
               MOVE    "コード"        TO  X04-LBLSRCBYOCD
           WHEN    "3"
               MOVE    "入力コード"    TO  X04-LBLSRCBYOCD
           WHEN    OTHER
               MOVE    SPACE           TO  X04-LBLSRCBYOCD
           END-EVALUATE
      *
           COMPUTE WRK-NO
               = ( SPA-GMN-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           MOVE    SPA-GMN-SRCBYOLST-MAX   TO  X04-SRCBYOLST-COUNT
           MOVE    ZERO                    TO  FLG-SRCBYOLST-SELECT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SRCBYOLST-MAX )
      *
               COMPUTE WRK-NO  =   WRK-NO  +   1
      *
               MOVE    WRK-NO          TO  WRK-ZZ9
      *
               MOVE    WRK-ZZ9         TO  X04-TSRCNO (IDX0)
               MOVE    SPA-GMN-TSRCBYOMEI (IDX0)
                                       TO  X04-TSRCBYOMEI(IDX0)
      *
               MOVE    SPA-GMN-TSRCCD (IDX0)
                                       TO  X04-TSRCCD (IDX0)
      *
               MOVE    SPA-GMN-SRCBYOLST-SELECT (IDX0)
                                       TO  X04-SRCBYOLST-SELECT (IDX0)
               IF    ( SPA-GMN-SRCBYOLST-SELECT (IDX0)
                                   =   "T" )
                   MOVE    1           TO  FLG-SRCBYOLST-SELECT
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-SRCBYOKBN   NOT =   SPACE )
            AND  ( SPA-GMN-SRCBYOLST-PAGE  =   1 )
            AND  ( SPA-GMN-SRCBYOLST-MAX   =   ZERO )
               MOVE    "該当なし"      TO  X04-TSRCBYOMEI(1)
               MOVE    1               TO  X04-SRCBYOLST-COUNT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-CDLST-MAX )
      *
               IF    ( IDX0           >   1 )
                AND  ( SPA-GMN-TCDNUM(IDX0)  =   1 )
                   COMPUTE X04-CDLST-COUNT   =   X04-CDLST-COUNT +   1
               END-IF
      *
               COMPUTE X04-CDLST-COUNT =   X04-CDLST-COUNT +   1
      *
               MOVE    SPA-GMN-TCDNUM(IDX0)
                                       TO  WRK-ZZ9
      *
               MOVE    WRK-ZZ9     TO  X04-TCDNO   (X04-CDLST-COUNT)
               MOVE    SPA-GMN-TCDBYOCD (IDX0)
                                   TO  X04-TCDBYOCD(X04-CDLST-COUNT)
      *
               MOVE    SPA-GMN-TCDBYOMEI (IDX0)
                                   TO  X04-TCDBYOMEI
                                                   (X04-CDLST-COUNT)
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-INPUT-MODE  =   ZERO )
               MOVE    "病名追加"      TO  X04-B05-LABEL
           ELSE
               MOVE    "病名変更"      TO  X04-B05-LABEL
           END-IF
      *
           MOVE    WIDGET-NORMAL   TO  X04-B03-STATE
                                       X04-B04-STATE
                                       X04-B05-STATE
      *                                X04-B06S-STATE
                                       X04-B07-STATE
      *                                X04-B07S-STATE
                                       X04-B09-STATE
                                       X04-B10-STATE
                                       X04-BTN-INS-STATE
                                       X04-BTN-ALLINS-STATE
                                       X04-BTN-ALLDEL-STATE
                                       X04-BYOMEI-STATE
                                       X04-HOSOKU-STATE
                                       X04-HOSOKU-JYKNKBN-STATE
                                       X04-HOSOKU-JYKNKBN-ITEM-STATE
                                       X04-UTAGAIKBN-STATE
                                       X04-UTAGAIKBN-ITEM-STATE
                                       X04-GRPID-STATE
                                       X04-GRPID-ITEM-STATE
      *
           MOVE    "T"
                                   TO  X04-TEKBYOKBN-VISIBLE
                                       X04-HOSOKU-VISIBLE
                                       X04-HOSOKU-JYKNKBN-VISIBLE
                                       X04-UTAGAIKBN-VISIBLE
                                       X04-GRPID-VISIBLE
      *
           MOVE    "black"         TO  X04-LBLMSG01-STYLE
      *
           IF    ( SPA-X01-SELNUM         <=  ZERO )
            OR   ( SPA-GMN-BYOMEI      NOT =   SPACE )
            OR   ( SPA-NAI-INPUT-MODE  NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  X04-B03-STATE
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-BYOCD-MAX )
               MOVE    WIDGET-NORMAL   TO  X04-BYOMEICD-STATE (IDX0)
           END-PERFORM
      *
           IF    ( SPA-MST-MAX        >=  CONST-BYOLST-MAX )
            AND  ( SPA-NAI-INPUT-MODE  =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X04-B05-STATE
           END-IF
      *
           IF    ( SPA-GMN-BYOLST-SEL >=   ZERO )
            AND  ( SPA-NAI-TMSTKBN (SPA-GMN-BYOLST-SEL)
                                   NOT =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  X04-B05-STATE
                                               X04-B07-STATE
                                               X04-BTN-ALLDEL-STATE
               MOVE    "blue"              TO  X04-LBLMSG01-STYLE
               MOVE    "適応病名マスタ"    TO  X04-LBLMSG01
           END-IF
      *
           IF    ( SPA-GMN-SRCBYOLST-PAGE  <=  1 )
               MOVE    WIDGET-INSENSITIVE  TO  X04-B09-STATE
           END-IF
      *
           IF    ( SPA-GMN-SRCBYOLST-NEXT   =  ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X04-B10-STATE
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
            OR   ( SPA-NAI-BYOINF-OVER =   1 )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-BYOCD-MAX )
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X04-BYOMEICD-STATE (IDX0)
               END-PERFORM
      *
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  X04-BYOMEI-STATE
                                           X04-HOSOKU-STATE
                                           X04-HOSOKU-JYKNKBN-STATE
                                           X04-HOSOKU-JYKNKBN-ITEM-STATE
               END-IF
      *
               MOVE    WIDGET-INSENSITIVE  TO  X04-B04-STATE
      *
           END-IF
      *
      *    IF    ( SPA-GMN-BYOLST-PAGE =   1 )
      *        MOVE    WIDGET-INSENSITIVE  TO  X04-B06S-STATE
      *    END-IF
      *
      *    IF    ( SPA-GMN-BYOLST-NEXT =   ZERO )
      *        MOVE    WIDGET-INSENSITIVE  TO  X04-B07S-STATE
      *    END-IF
      *
           IF    ( FLG-SRCBYOLST-SELECT    =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X04-BTN-INS-STATE
           END-IF
      *
           IF    ( SPA-GMN-SRCBYOKBN   =    "M" )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  X04-BTN-ALLINS-STATE
           END-IF
      *
           MOVE    "適応病名マスタからの直接データチェック"
                                   TO  X04-LBLTEKBYOKBN
      *
           MOVE    "補足コメント"  TO  X04-LBLHOSOKU
           MOVE    "（"            TO  X04-LBLHOSOKU-KAKKO-L
           MOVE    "）"            TO  X04-LBLHOSOKU-KAKKO-R
           MOVE    "一致条件"      TO  X04-LBLHOSOKU-JYKNKBN
      *
           MOVE    "疑い病名"      TO  X04-LBLUTAGAIKBN
      *
           MOVE    "グループ"      TO  X04-LBLGRPID
      *
           IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
               CONTINUE
           ELSE
               INITIALIZE              X04-LBLUTAGAIKBN
                                       X04-UTAGAIKBN
                                       X04-CMB-UTAGAIKBN
                                       X04-LBLGRPID
                                       X04-GRPID
                                       X04-CMB-GRPID
                                       X04-LBLHOSOKU
                                       X04-LBLHOSOKU-KAKKO-L
                                       X04-LBLHOSOKU-KAKKO-R
                                       X04-HOSOKU
                                       X04-LBLHOSOKU-JYKNKBN
                                       X04-HOSOKU-JYKNKBN
                                       X04-LBLTEKBYOKBN
      *
               MOVE    WIDGET-INSENSITIVE
                                   TO  X04-UTAGAIKBN-STATE
                                       X04-UTAGAIKBN-ITEM-STATE
                                       X04-GRPID-STATE
                                       X04-GRPID-ITEM-STATE
                                       X04-HOSOKU-STATE
                                       X04-HOSOKU-JYKNKBN-STATE
                                       X04-HOSOKU-JYKNKBN-ITEM-STATE
      *
               MOVE    "F"
                                   TO  X04-TEKBYOKBN-VISIBLE
                                       X04-HOSOKU-VISIBLE
                                       X04-HOSOKU-JYKNKBN-VISIBLE
                                       X04-UTAGAIKBN-VISIBLE
                                       X04-GRPID-VISIBLE
      *
           END-IF
      *
           IF    ( SPA-NAI-BYOINF-MAX  >   1 )
               MOVE    SPACE       TO  X04-HOSOKU
               MOVE    WIDGET-INSENSITIVE
                                       TO  X04-HOSOKU-STATE
           END-IF
      *
           IF    ( SPA-GMN-HOSOKU  =   SPACE )
               MOVE    SPACE       TO  X04-HOSOKU-JYKNKBN
               MOVE    WIDGET-INSENSITIVE
                                   TO  X04-HOSOKU-JYKNKBN-STATE
                                       X04-HOSOKU-JYKNKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-X01-CHKKBN  =   "1" )
               MOVE    "例外薬剤"  TO  X04-B06-LABEL
           END-IF
      *
           MOVE    "blue"          TO  X04-LBLREIGAIYKZ-STYLE
           IF    ( SPA-X06-MAX     >   ZERO )
               MOVE    "例外薬剤の登録があります。"
                                   TO  X04-LBLREIGAIYKZ
           END-IF
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           IF    ( SPA-GMN-CUR         >=  2 )
            AND  ( SPA-GMN-CUR         <=  7 )
                IF   ( X04-BYOMEICD-STATE (1)
                                       =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   8 )
                IF   ( X04-BYOMEI-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    9           TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   18 )
                IF   ( X04-HOSOKU-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    19          TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   19 )
                IF   ( X04-HOSOKU-JYKNKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    15          TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   15 )
                IF   ( X04-UTAGAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    17          TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   17 )
                IF   ( X04-GRPID-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    105         TO  SPA-GMN-CUR
                END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    02
               MOVE    "BYOMEICD1"     TO  MCP-WIDGET
           WHEN    03
               MOVE    "BYOMEICD2"     TO  MCP-WIDGET
           WHEN    04
               MOVE    "BYOMEICD3"     TO  MCP-WIDGET
           WHEN    05
               MOVE    "BYOMEICD4"     TO  MCP-WIDGET
           WHEN    06
               MOVE    "BYOMEICD5"     TO  MCP-WIDGET
           WHEN    07
               MOVE    "BYOMEICD6"     TO  MCP-WIDGET
           WHEN    08
               MOVE    "BYOMEI"        TO  MCP-WIDGET
           WHEN    09
               MOVE    "JYKNKBN"       TO  MCP-WIDGET
           WHEN    10
               MOVE    "SEL_BYOMEI"    TO  MCP-WIDGET
           WHEN    11
               MOVE    "SRCBYOKBN"     TO  MCP-WIDGET
           WHEN    12
               MOVE    "SRTKBN"        TO  MCP-WIDGET
           WHEN    13
               MOVE    "SRCJYKNKBN"    TO  MCP-WIDGET
           WHEN    14
               MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
           WHEN    15
               MOVE    "UTAGAIKBN"     TO  MCP-WIDGET
           WHEN    16
               MOVE    "SRCUTAGAIKBN"  TO  MCP-WIDGET
           WHEN    17
               MOVE    "GRPID"         TO  MCP-WIDGET
           WHEN    18
               MOVE    "HOSOKU"        TO  MCP-WIDGET
           WHEN    19
               MOVE    "HOSOKU_JYKNKBN" TO MCP-WIDGET
           WHEN    20
               MOVE    "TEKBYOKBN"     TO MCP-WIDGET
           WHEN    105
               MOVE    "B05"           TO  MCP-WIDGET
           WHEN    112
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
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "BYOMEICD1"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "BYOMEICD2"
               MOVE    04              TO  SPA-GMN-CUR
           WHEN    "BYOMEICD3"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "BYOMEICD4"
               MOVE    06              TO  SPA-GMN-CUR
           WHEN    "BYOMEICD5"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "BYOMEICD6"
               MOVE    08              TO  SPA-GMN-CUR
           WHEN    "BYOMEI"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "JYKNKBN"
               MOVE    18              TO  SPA-GMN-CUR
           WHEN    "HOSOKU"
               MOVE    19              TO  SPA-GMN-CUR
           WHEN    "HOSOKU_JYKNKBN"
               MOVE    15              TO  SPA-GMN-CUR
           WHEN    "UTAGAIKBN"
               MOVE    17              TO  SPA-GMN-CUR
           WHEN    "GRPID"
               MOVE    105             TO  SPA-GMN-CUR
           WHEN    "SEL_BYOMEI"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "SRCBYOKBN"
               MOVE    12              TO  SPA-GMN-CUR
           WHEN    "SRTKBN"
               MOVE    13              TO  SPA-GMN-CUR
           WHEN    "SRCJYKNKBN"
               MOVE    16              TO  SPA-GMN-CUR
           WHEN    "SRCUTAGAIKBN"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "NYUGAIKBN"
               MOVE    20               TO  SPA-GMN-CUR
           WHEN    "TEKBYOKBN"
               MOVE    1               TO  SPA-GMN-CUR
           WHEN    "B05"
               MOVE    105             TO  SPA-GMN-CUR
           WHEN    "B12"
               MOVE    112             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "病名番号の指定数が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "検索対象マスタの入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "検索病名の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               STRING  "未コード化傷病名は他のコードといっしょに"
                                                   DELIMITED   BY  SIZE
                       "入力できません"            DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0006"
               STRING  "病名コードが２２個以上になりますので、編"
                                                   DELIMITED   BY  SIZE
                       "集病名とします"            DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0007"
               MOVE    "病名コードがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "自院病名コードがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "病名の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "不明の病名コードが入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "補足コメントの入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "適応病名マスタ参照区分の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "適応病名マスタの変更はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "病名番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "適応病名の登録上限に到達しました"
                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "登録上限に達しているため追加登録はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "病名を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "病名を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "一致条件を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE    "疑い区分を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0111"
               STRING  "グループ指定のない病名と"
                                           DELIMITED   BY  SIZE
                       "ある病名は混在して登録できません"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0107"
           WHEN    "0109"
               MOVE    "病名が入力途中です"
                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "病名の登録がありません"
                                       TO  SPA-ERRMSG
           WHEN    "0112"
               MOVE    "登録上限を超えるため、追加登録はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0113"
               STRING  "複数の病名を入力中のため、"
                                           DELIMITED   BY  SIZE
                       "直接病名の編集はできません"
                                           DELIMITED   BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0114"
               MOVE    "変更処理のときは複数病名の入力はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0115"
               MOVE    "補足コメントの前後に（）があります"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
           WHEN    "1004"
           WHEN    "1005"
               MOVE    "更新ができませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "削除ができませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "更新処理前の削除ができませんでした"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X04"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
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
       520-XIDSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-XIDMSG
      *
           EVALUATE    SPA-XIDCD
           WHEN    "1001"
               MOVE    "チェックマスタ設定情報を登録します"
                                       TO  WRK-XIDMSG
           WHEN    "1002"
               MOVE    "チェックマスタ設定情報を削除します"
                                       TO  WRK-XIDMSG
           WHEN    "2001"
               MOVE    "チェックマスタを登録後、印刷を行います"
                                       TO  WRK-XIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XID1
           INITIALIZE                      XID1
           MOVE    SPA-XIDCD           TO  XID1-ID1CODE
           MOVE    WRK-XIDMSG          TO  XID1-ID1MSG
           MOVE    "戻る"              TO  XID1-B01
           MOVE    "ＯＫ"              TO  XID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "X04"               TO  SPA-MOTOPG
           MOVE    "XID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-XIDSET-EXT.
           EXIT.
      *****************************************************************
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ZENKAKU
           MOVE    ZERO                TO  WRK-KANACHK-MAX
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-KANACHK-MAX
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT
           END-IF
      *
           IF    ( KANACHK-SYUBETU     =   2   )
               MOVE    1               TO  FLG-ZENKAKU
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
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
      *    チェックマスタ２検索処理(KEY2)
      *****************************************************************
       900-CHKSND-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-X01-CHKKBN      TO  CHKSND-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHKSND-SRYCD
           IF    ( SPA-X01-SEL-STYUKYMD    =    SPACE )
            OR   ( FLG-TEMPTBL             =    1 )
               MOVE    SPA-X01-STYUKYMD
                                       TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD
                                       TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理(KEY2)
      *****************************************************************
       900-CHKSND-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-X01-CHKKBN      TO  CHKSND-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHKSND-SRYCD
           IF    ( SPA-X01-SEL-STYUKYMD    =    SPACE )
               MOVE    SPA-X01-STYUKYMD
                                       TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD
                                       TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           END-IF
           MOVE    "3"                 TO  CHKSND-CDKBN
      *
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY3-FET-SEC      SECTION.
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       901-CHKSND-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           INITIALIZE                  CHKSND-REC
           MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
           MOVE    SPA-X01-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    SPA-X01-SRYCD   TO  CHKSND-SRYCD
           IF    ( SPA-X01-SEL-STYUKYMD    =   SPACE )
               MOVE    SPA-X01-STYUKYMD
                                   TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD
                                   TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           END-IF
           MOVE    ZERO            TO  CHKSND-CDKBN
      *
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1           TO  FLG-CHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       901-CHKSND-KEY3-SEL-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名検索処理(KEY26)
      *****************************************************************
       900-PTBYOMEI-KEY26-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-PTBYOMEI
      *
           INITIALIZE              PTBYOMEI-REC
      *
           MOVE    SPA-SYSYMD      TO  WRK-YMD
           MOVE    "2"             TO  WRK-ZOGENPTN
           MOVE    -3              TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  PTBYO-SRYYMD
      *
           MOVE    SPA-SYSYMD      TO  PTBYO-UPYMD
      *
           MOVE    WRK-SEL-SRYKA   TO  PTBYO-SRYKA
      *
           STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    PTBYO-BYOMEI
           END-STRING
      *
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key26"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           .
      *
       900-PTBYOMEI-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名FETCH処理(KEY26)
      *****************************************************************
       900-PTBYOMEI-KEY26-FET-SEC      SECTION.
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key26"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           .
      *
       900-PTBYOMEI-KEY26-FET-EXT.
           EXIT.
      *****************************************************************
      *    病名検索処理(KEY7)
      *****************************************************************
       900-BYOMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BYOMEI
      *
           INITIALIZE              BYOMEI-REC
      *
           STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    BYO-BYOMEI
           END-STRING
      *
           MOVE    WRK-SEL-STBYOMEICD  TO  BYO-BYOMEICD
           MOVE    WRK-SEL-EDBYOMEICD  TO  BYO-IKOSAKICD
      *
           MOVE    BYO-BYOMEI      TO  BYO-BYOMEIKANA
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               PERFORM 900-BYOMEI-KEY7-FET-SEC
           END-IF
      *
           .
      *
       900-BYOMEI-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名FETCH処理(KEY7)
      *****************************************************************
       900-BYOMEI-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-BYOMEI      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "tbl_byomei"    TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC    =     ZERO )
                   MOVE    MCPDATA-REC TO  BYOMEI-REC
      *
                   IF    ( BYO-BYOMEICD    NOT =   "0000999" )
                       MOVE    1   TO  FLG-LOOP-END
                   END-IF
      *
               ELSE
                   INITIALIZE              BYOMEI-REC
                   MOVE    1           TO  FLG-BYOMEI
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-BYOMEI-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY4)
      *****************************************************************
       900-USERBYOMEI-KEY4-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-USERBYOMEI
      *
           INITIALIZE              USERBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
      *
           MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名FETCH処理(KEY4)
      *****************************************************************
       900-USERBYOMEI-KEY4-FET-SEC    SECTION.
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY10)
      *****************************************************************
       900-USERBYOMEI-KEY10-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-USERBYOMEI
      *
           INITIALIZE              USERBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
      *
           MOVE    WRK-SEL-BUNRUICD
                                   TO  USBYO-BUNRUICD
      *
           STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    USBYO-BYOMEI
           END-STRING
      *
           MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名FETCH処理(KEY10)
      *****************************************************************
       900-USERBYOMEI-KEY10-FET-SEC    SECTION.
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY10-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY11)
      *****************************************************************
       900-USERBYOMEI-KEY11-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
           INITIALIZE                      USERBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
      *
           STRING  WRK-BYOMEIINPUTCD       DELIMITED   BY  SPACE
                   "%"                     DELIMITED   BY  SIZE
           INTO    USBYO-BYOMEIINPUTCD
           END-STRING
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-USERBYOMEI-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY29-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           INITIALIZE                  CHKSND-REC
           MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
           MOVE    SPA-X01-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    SPA-X01-SRYCD   TO  CHKSND-SRYCD
      *
           STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    CHKSND-BYOMEI
           END-STRING
      *
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1           TO  FLG-CHKSND
           END-IF
      *
           .
      *
       900-CHKSND-KEY29-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY29-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1           TO  FLG-CHKSND
           END-IF
      *
           .
      *
       900-CHKSND-KEY29-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
