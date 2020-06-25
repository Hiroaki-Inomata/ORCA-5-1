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
       PROGRAM-ID.             ORCGV02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 保険番号情報入力
      *  コンポーネント名  : 保険番号情報入力（Ｖ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.00.02    MCC-藤原     01/10/09  削除の追加      
      *  01.00.03    MCC-藤原     01/11/14  項目をコンボボックスに変更
      *  01.00.04    MCC-藤原     02/03/13  負担区分の追加      
      *  01.00.05    MCC-藤原     02/03/22  保険番号マスタの読込みの修正
      *  01.00.06    NACL-多々納  02/12/24  負担区分の初期値の編集
      *  01.00.07    NACL-太田    03/01/08  レセプト負担金額コンボ追加
      *  01.00.08    NACL-多々納  03/02/14  法別番号チェック区分追加
      *                                     一括入力へ変更
      *  01.00.09    NACL-多々納  04/02/10  入院食事助成額追加
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *  04.05.00    NACL-太田    09/06/23  食事療養費区分"5"追加
      *  04.06.00    NACL-多々納  11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      *  04.08.00    NACL-森脇    15/11/05  複写元保険番号追加
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
           COPY    "V01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-V02.
           03  SPA-V02-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-HKNNUM         PIC X(03).
                   07  SPA-GMN-PAYKBN         PIC X(02).
                   07  SPA-GMN-PAGENO         PIC 9(01).
      *             
                   07  SPA-GMN-GAIFTNKBNLST   OCCURS  4.
                       09  SPA-GMN-GAIFTNKBNL PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-GAIFTNKBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-GAIFTNKBN-MAX  PIC 9(01).
                   07  SPA-GMN-GAIYKZFTNKBNLST
                                              OCCURS  4.
                       09  SPA-GMN-GAIYKZFTNKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-GAIYKZFTNKBNMEIL
                                              PIC X(24).
                   07  SPA-GMN-GAIYKZFTNKBN-MAX
                                              PIC 9(01).
                   07  SPA-GMN-NYUFTNKBNLST
                                              OCCURS  4.
                       09  SPA-GMN-NYUFTNKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-NYUFTNKBNMEIL
                                              PIC X(20).
                   07  SPA-GMN-NYUFTNKBN-MAX  PIC 9(01).
                   07  SPA-GMN-NYUSKJRYYKBNLST
                                              OCCURS  6.
                       09  SPA-GMN-NYUSKJRYYKBNL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-NYUSKJRYYKBNMEIL
                                              PIC X(26).
                   07  SPA-GMN-NYUSKJRYYKBN-MAX
                                              PIC 9(01).
                   07  SPA-GMN-HKNKOHSBTKBNLST    OCCURS  9.
                       09  SPA-GMN-HKNKOHSBTKBNL  PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-HKNKOHSBTKBNMEIL
                                                  PIC X(18).
                   07  SPA-GMN-HKNKOHSBTKBN-MAX   PIC 9(01).
                   07  SPA-GMN-KENSNUMCHKKBNLST    OCCURS  2.
                       09  SPA-GMN-KENSNUMCHKKBNL PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-KENSNUMCHKKBNMEIL
                                                  PIC X(14).
                   07  SPA-GMN-KENSNUMCHKKBN-MAX  PIC 9(01).
                   07  SPA-GMN-JKYSKENSNUMCHKKBNLST   OCCURS  3.
                       09  SPA-GMN-JKYSKENSNUMCHKKBNL PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-JKYSKENSNUMCHKKBNMEIL
                                                  PIC X(14).
                   07  SPA-GMN-JKYSKENSNUMCHKKBN-MAX
                                                  PIC 9(01).
                   07  SPA-GMN-HKNKOHKBNLST       OCCURS  3.
                       09  SPA-GMN-HKNKOHKBNL     PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-HKNKOHKBNMEIL  PIC X(08).
                   07  SPA-GMN-HKNKOHKBN-MAX      PIC 9(01).
                   07  SPA-GMN-RECESKYKBNLST      OCCURS  10.
                       09  SPA-GMN-RECESKYKBNL    PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-RECESKYKBNMEIL PIC X(34).
                   07  SPA-GMN-RECESKYKBN-MAX     PIC 9(02).
      *
                   07  SPA-GMN-RECEFTNDSPKBNLST   OCCURS  2.
                       09  SPA-GMN-RECEFTNDSPKBNL PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-RECEFTNDSPKBNMEIL
                                                  PIC X(28).
                   07  SPA-GMN-RECEFTNDSPKBN-MAX  PIC 9(01).
                   07  SPA-GMN-RECEFTNJGNDSPKBNLST  OCCURS  2.
                       09  SPA-GMN-RECEFTNJGNDSPKBNL PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-RECEFTNJGNDSPKBNMEIL PIC X(22).
                   07  SPA-GMN-RECEFTNJGNDSPKBN-MAX     PIC 9(01).
      *
                   07  SPA-GMN-HBTNUM         PIC X(02).
                   07  SPA-GMN-SEIDONAME      PIC X(100).
                   07  SPA-GMN-TANSEIDONAME   PIC X(10).
                   07  SPA-GMN-HKNKOHSBTKBN-G.
                       09  SPA-GMN-HKNKOHSBTKBN   PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-HKNKOHSBTKBNMEI
                                                  PIC X(18).
                   07  SPA-GMN-KENSNUMCHKKBN-G.
                       09  SPA-GMN-KENSNUMCHKKBN  PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-KENSNUMCHKKBNMEI
                                                  PIC X(14).
                   07  SPA-GMN-JKYSKENSNUMCHKKBN-G.
                       09  SPA-GMN-JKYSKENSNUMCHKKBN
                                                  PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-JKYSKENSNUMCHKKBNMEI
                                                  PIC X(14).
                   07  SPA-GMN-HKNKOHKBN-G.
                       09  SPA-GMN-HKNKOHKBN      PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-HKNKOHKBNMEI   PIC X(08).
                   07  SPA-GMN-GENHKNNUM-G        OCCURS  3.
                       09  SPA-GMN-GENHKNNUM      PIC X(03).
                   07  SPA-GMN-JOKENSTYEAR        PIC 9(03).
                   07  SPA-GMN-JOKENEDYEAR        PIC 9(03).
                   07  SPA-GMN-TENTNK             PIC 9(03).
                   07  SPA-GMN-RECESKYKBN-G.
                       09  SPA-GMN-RECESKYKBN     PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-RECESKYKBNMEI  PIC X(34).
                   07  SPA-GMN-RECEFTNDSPKBN-G.
                       09  SPA-GMN-RECEFTNDSPKBN  PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-RECEFTNDSPKBNMEI
                                                  PIC X(28).
                   07  SPA-GMN-RECEFTNJGNDSPKBN-G.
                       09  SPA-GMN-RECEFTNJGNDSPKBN     PIC X(01).
                       09  SPA-GMN-F1             PIC X(01).
                       09  SPA-GMN-RECEFTNJGNDSPKBNMEI  PIC X(22).
      *            法別番号チェック
                   07  SPA-GMN-HBTNUMCHKKBN-G.
                       09  SPA-GMN-HBTNUMCHKKBN   PIC X(01).
                       09  SPA-GMN-HBT1           PIC X(01).
                       09  SPA-GMN-HBTNUMCHKKBNMEI
                                                  PIC X(14).
                   07  SPA-GMN-HBTNUMCHKKBN-LST   PIC X(16)
                                                  OCCURS   2.
      *
                   07  SPA-GMN-HKN-AREA-G     OCCURS  4.
                       09  SPA-GMN-GAIFTNKBN-G.
                           11  SPA-GMN-GAIFTNKBN  PIC X(01).
                           11  SPA-GMN-F1         PIC X(01).
                           11  SPA-GMN-GAIFTNKBNMEI
                                                  PIC X(20).
                       09  SPA-GMN-GAIKFTNRATE    PIC 9(03).
                       09  SPA-GMN-GAIKKOTGAK     PIC 9(06).
                       09  SPA-GMN-GAIKJGNGAK     PIC 9(06).
                       09  SPA-GMN-GAIDJGNGAK     PIC 9(06). 
                       09  SPA-GMN-GAIDJGNCNT     PIC 9(02).
                       09  SPA-GMN-GAIMNAIJGNGAK  PIC 9(06).
                       09  SPA-GMN-GAIMGAIJGNGAK  PIC 9(06).
                       09  SPA-GMN-GAIMJGNCNT     PIC 9(02).
                       09  SPA-GMN-GAIYKZFTNKBN-G.
                           11  SPA-GMN-GAIYKZFTNKBN
                                                  PIC X(01).
                           11  SPA-GMN-F1         PIC X(01).
                           11  SPA-GMN-GAIYKZFTNKBNMEI
                                                  PIC X(24).
                       09  SPA-GMN-NYUFTNKBN-G.
                           11  SPA-GMN-NYUFTNKBN  PIC X(01).
                           11  SPA-GMN-F1         PIC X(01).
                           11  SPA-GMN-NYUFTNKBNMEI
                                                  PIC X(20).
                       09  SPA-GMN-NYUKFTNRATE    PIC 9(03).
                       09  SPA-GMN-NYUKKOTGAK     PIC 9(06).
                       09  SPA-GMN-NYUKJGNGAK     PIC 9(06).
                       09  SPA-GMN-NYUDJGNGAK     PIC 9(06). 
                       09  SPA-GMN-NYUDJGNCNT     PIC 9(02).
                       09  SPA-GMN-NYUMJGNGAK     PIC 9(06).
                       09  SPA-GMN-NYUMJGNCNT     PIC 9(02).
                       09  SPA-GMN-NYUSKJRYYKBN-G.
                           11  SPA-GMN-NYUSKJRYYKBN
                                                  PIC X(01).
                           11  SPA-GMN-F1         PIC X(01).
                           11  SPA-GMN-NYUSKJRYYKBNMEI
                                                  PIC X(26).
      *                食事助成額
                       09  SPA-GMN-NYUSKJJYOGAK   PIC 9(06).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD           PIC 9(08).
                   07  SPA-NAI-EDYUKYMD           PIC 9(08).
      *
      *   
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-VIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-PAGENO          PIC 9(01).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HKNNUM-REC.
           COPY  "CPHKNNUM.INC".
      *
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *orca104
           COPY    "V01.INC".
           COPY    "V02.INC".
           COPY    "V03.INC".
           COPY    "VERR.INC".
           COPY    "VID1.INC".
      *orca105
           COPY    "N01.INC".
           COPY    "NERR.INC".
           COPY    "NID1.INC".
      *orca106
           COPY    "N11.INC".
           COPY    "N11ERR.INC".
           COPY    "N11ID1.INC".
      *orca108
           COPY    "N31.INC".
           COPY    "N31ERR.INC".
           COPY    "N31ID1.INC".
      *orca109
           COPY    "N41.INC".
           COPY    "N41ERR.INC".
           COPY    "N41ID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-V01KYOUTU
           MOVE    SPA-FREE        TO  SPA-V02
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-V02         TO  SPA-FREE
           MOVE    SPA-V01KYOUTU   TO  SPA-WORK 
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
           IF      SPA-MOTOPG          =   "VERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "V02"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "VID1"
                   PERFORM 330-VID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-VID1-SET-SEC            SECTION.
      *
           IF      SPA-VID1-FLG    =   "OK"
               IF      SPA-VIDCD            =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-VID1-FLG
           MOVE    SPACE           TO  SPA-VIDCD      
           .
       330-VID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-V02-AREA
      *
           MOVE    SPA-V01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-V01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-V01-HKNNUM      TO  SPA-GMN-HKNNUM
           MOVE    SPA-V01-PAYKBN      TO  SPA-GMN-PAYKBN
      *
           MOVE    "1"                 TO  SPA-GMN-HKNKOHSBTKBNL    (1)
           MOVE    "主保険"            TO  SPA-GMN-HKNKOHSBTKBNMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-HKNKOHSBTKBNL    (2)
           MOVE    "長期高額療養費"    TO  SPA-GMN-HKNKOHSBTKBNMEIL (2)
           MOVE    "3"                 TO  SPA-GMN-HKNKOHSBTKBNL    (3)
           MOVE    "老人保健"          TO  SPA-GMN-HKNKOHSBTKBNMEIL (3)
           MOVE    "4"                 TO  SPA-GMN-HKNKOHSBTKBNL    (4)
      *****MOVE    "労災"              TO  SPA-GMN-HKNKOHSBTKBNMEIL (4)
      **** MOVE    "労災・自賠"        TO  SPA-GMN-HKNKOHSBTKBNMEIL (4)
           MOVE    "労災・自賠・公害"  TO  SPA-GMN-HKNKOHSBTKBNMEIL (4)
           MOVE    "5"                 TO  SPA-GMN-HKNKOHSBTKBNL    (5)
           MOVE    "生活保護"          TO  SPA-GMN-HKNKOHSBTKBNMEIL (5)
           MOVE    "6"                 TO  SPA-GMN-HKNKOHSBTKBNL    (6)
           MOVE    "単独公費"          TO  SPA-GMN-HKNKOHSBTKBNMEIL (6)
           MOVE    "7"                 TO  SPA-GMN-HKNKOHSBTKBNL    (7)
           MOVE    "一般公費"          TO  SPA-GMN-HKNKOHSBTKBNMEIL (7)
           MOVE    "8"                 TO  SPA-GMN-HKNKOHSBTKBNL    (8)
           MOVE    "自費"              TO  SPA-GMN-HKNKOHSBTKBNMEIL (8)
           MOVE    "9"                 TO  SPA-GMN-HKNKOHSBTKBNL    (9)
           MOVE    "治験"              TO  SPA-GMN-HKNKOHSBTKBNMEIL (9)
           MOVE    9                   TO  SPA-GMN-HKNKOHSBTKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-KENSNUMCHKKBNL   (1)
           MOVE    "チェックする"      TO  SPA-GMN-KENSNUMCHKKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-KENSNUMCHKKBNL   (2)
           MOVE    "チェックしない"    TO  SPA-GMN-KENSNUMCHKKBNMEIL(2)
           MOVE    2                   TO  SPA-GMN-KENSNUMCHKKBN-MAX
      *
           MOVE    "1"                 TO
                                       SPA-GMN-JKYSKENSNUMCHKKBNL   (1)
           MOVE    "チェックする"      TO
                                       SPA-GMN-JKYSKENSNUMCHKKBNMEIL(1)
           MOVE    "2"                 TO
                                       SPA-GMN-JKYSKENSNUMCHKKBNL   (2)
           MOVE    "チェックしない"    TO
                                       SPA-GMN-JKYSKENSNUMCHKKBNMEIL(2)
           MOVE    "3"                 TO
                                       SPA-GMN-JKYSKENSNUMCHKKBNL   (3)
           MOVE    "警告チェック"      TO
                                       SPA-GMN-JKYSKENSNUMCHKKBNMEIL(3)
           MOVE    3                   TO  
                                       SPA-GMN-JKYSKENSNUMCHKKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-HKNKOHKBNL    (1)
           MOVE    "主公費"            TO  SPA-GMN-HKNKOHKBNMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-HKNKOHKBNL    (2)
           MOVE    "補助公費"          TO  SPA-GMN-HKNKOHKBNMEIL (2)
           MOVE    "3"                 TO  SPA-GMN-HKNKOHKBNL    (3)
           MOVE    "主補公費"          TO  SPA-GMN-HKNKOHKBNMEIL (3)
           MOVE    3                   TO  SPA-GMN-HKNKOHKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-RECESKYKBNL   (1)
           MOVE    "社保、国保、広域連合の場合に印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-RECESKYKBNL   (2)
           MOVE    "社保との併用に限り印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL(2)
           MOVE    "2"                 TO  SPA-GMN-RECESKYKBNL   (3)
           MOVE    "国保又は広域連合との併用に限り印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL(3)
           MOVE    "3"                 TO  SPA-GMN-RECESKYKBNL   (4)
           MOVE    "社保、国保、広域連合とも印刷不可"
                                       TO  SPA-GMN-RECESKYKBNMEIL(4)
           MOVE    "4"                 TO  SPA-GMN-RECESKYKBNL   (5)
           MOVE    "国保との併用に限り印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL(5)
           MOVE    "5"                 TO  SPA-GMN-RECESKYKBNL   (6)
           MOVE    "広域連合との併用に限り印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL(6)
           MOVE    6                   TO  SPA-GMN-RECESKYKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-RECEFTNDSPKBNL(1)
           MOVE    "１０円未満四捨五入する"
                                       TO  SPA-GMN-RECEFTNDSPKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-RECEFTNDSPKBNL(2)
           MOVE    "１０円未満四捨五入しない"
                                       TO  SPA-GMN-RECEFTNDSPKBNMEIL(2)
           MOVE    2                   TO  SPA-GMN-RECEFTNDSPKBN-MAX
           MOVE    SPA-GMN-RECEFTNDSPKBNLST (1)
                                       TO  SPA-GMN-RECEFTNDSPKBN-G
      *
           MOVE    "0"              TO  SPA-GMN-RECEFTNJGNDSPKBNL(1)
           MOVE    "負担上限未満記載あり"
                                    TO SPA-GMN-RECEFTNJGNDSPKBNMEIL(1)
           MOVE    "1"              TO  SPA-GMN-RECEFTNJGNDSPKBNL(2)
           MOVE    "負担上限未満記載なし"
                                    TO SPA-GMN-RECEFTNJGNDSPKBNMEIL(2)
           MOVE    SPA-GMN-RECEFTNJGNDSPKBNLST (1)
                                       TO  SPA-GMN-RECEFTNJGNDSPKBN-G
           MOVE    2                TO  SPA-GMN-RECEFTNJGNDSPKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-GAIFTNKBNL    (1)
           MOVE    "使用しない"        TO  SPA-GMN-GAIFTNKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-GAIFTNKBNL    (2)
           MOVE    "患者負担あり"      TO  SPA-GMN-GAIFTNKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-GAIFTNKBNL    (3)
           MOVE    "患者負担なし"      TO  SPA-GMN-GAIFTNKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-GAIFTNKBNL    (4)
           MOVE    "患者負担あり（固定）"
                                       TO  SPA-GMN-GAIFTNKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-GAIFTNKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-GAIYKZFTNKBNL    (1)
           MOVE    "使用しない"        TO  SPA-GMN-GAIYKZFTNKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-GAIYKZFTNKBNL    (2)
           MOVE    "患者負担あり"      TO  SPA-GMN-GAIYKZFTNKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-GAIYKZFTNKBNL    (3)
           MOVE    "患者負担あり（上限あり）"
                                       TO  SPA-GMN-GAIYKZFTNKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-GAIYKZFTNKBNL    (4)
           MOVE    "患者負担なし"      TO  SPA-GMN-GAIYKZFTNKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-GAIYKZFTNKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-NYUFTNKBNL       (1)
           MOVE    "使用しない"        TO  SPA-GMN-NYUFTNKBNMEIL    (1)
           MOVE    "1"                 TO  SPA-GMN-NYUFTNKBNL       (2)
           MOVE    "患者負担あり"      TO  SPA-GMN-NYUFTNKBNMEIL    (2)
           MOVE    "2"                 TO  SPA-GMN-NYUFTNKBNL       (3)
           MOVE    "患者負担なし"      TO  SPA-GMN-NYUFTNKBNMEIL    (3)
           MOVE    "3"                 TO  SPA-GMN-NYUFTNKBNL       (4)
           MOVE    "患者負担あり（固定）"
                                       TO  SPA-GMN-NYUFTNKBNMEIL    (4)
           MOVE    4                   TO  SPA-GMN-NYUFTNKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-NYUSKJRYYKBNL    (1)
           MOVE    "使用しない"        TO  SPA-GMN-NYUSKJRYYKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-NYUSKJRYYKBNL    (2)
           MOVE    "患者負担あり"      TO  SPA-GMN-NYUSKJRYYKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-NYUSKJRYYKBNL    (3)
           MOVE    "患者負担あり（上限あり）"
                                       TO  SPA-GMN-NYUSKJRYYKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-NYUSKJRYYKBNL    (4)
           MOVE    "患者負担なし"      TO  SPA-GMN-NYUSKJRYYKBNMEIL (4)
           MOVE    "4"                 TO  SPA-GMN-NYUSKJRYYKBNL    (5)
           MOVE    "患者負担あり（消費税あり）"
                                       TO  SPA-GMN-NYUSKJRYYKBNMEIL (5)
           MOVE    "5"                 TO  SPA-GMN-NYUSKJRYYKBNL    (6)
           MOVE    "患者負担あり（消費税なし）"
                                       TO  SPA-GMN-NYUSKJRYYKBNMEIL (6)
           MOVE    6                   TO  SPA-GMN-NYUSKJRYYKBN-MAX
      *
      *    法別番号チェック区分
           MOVE    "0"                 TO  SPA-GMN-HBTNUMCHKKBN-LST(1)
                                                               (1:1)
           MOVE    "チェックしない"    TO  SPA-GMN-HBTNUMCHKKBN-LST(1)
                                                               (3:)
           MOVE    "1"                 TO  SPA-GMN-HBTNUMCHKKBN-LST(2)
                                                               (1:1)
           MOVE    "チェックする"      TO  SPA-GMN-HBTNUMCHKKBN-LST(2)
                                                               (3:)
      *
      *----(04.08.00)--UPD-START---
      *    複写元保険番号がある場合
           IF      SPA-V01-CPHKNNUM    NOT =   SPACE
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-V01-CPHKNNUM        TO  HKN-HKNNUM
               MOVE    SPA-V01-PAYKBN          TO  HKN-PAYKBN
               MOVE    SPA-SYSYMD              TO  HKN-TEKSTYMD
               MOVE    SPA-SYSYMD              TO  HKN-TEKEDYMD
           ELSE
      *        保険番号マスタより編集  
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-V01-HKNNUM          TO  HKN-HKNNUM
               MOVE    SPA-V01-PAYKBN          TO  HKN-PAYKBN
               IF      SPA-OLD-STYUKYMD        =   SPACE
                   MOVE    SPA-V01-STYUKYMD        TO  HKN-TEKSTYMD
                   MOVE    SPA-V01-EDYUKYMD        TO  HKN-TEKEDYMD
               ELSE
                   MOVE    SPA-OLD-STYUKYMD        TO  HKN-TEKSTYMD
                   MOVE    SPA-OLD-EDYUKYMD        TO  HKN-TEKEDYMD
               END-IF
           END-IF
      *----(04.08.00)--UPD-END-----
      *
           MOVE    SPA-HOSPNUM             TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC              TO  MCPDATA-REC
           PERFORM 900-HKNNUM-INV-SEC
           IF      FLG-HKNNUM              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    HKN-HBTNUM          TO  SPA-GMN-HBTNUM
               MOVE    HKN-SEIDONAME       TO  SPA-GMN-SEIDONAME
               MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-TANSEIDONAME
               MOVE    HKN-HKNKOHSBTKBN    TO  SPA-GMN-HKNKOHSBTKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-HKNKOHSBTKBN-MAX
                   IF      SPA-GMN-HKNKOHSBTKBN
                                      =   SPA-GMN-HKNKOHSBTKBNL (IDY)
                       MOVE    SPA-GMN-HKNKOHSBTKBNMEIL (IDY)
                                      TO  SPA-GMN-HKNKOHSBTKBNMEI
                       MOVE    SPA-GMN-HKNKOHSBTKBN-MAX
                                      TO  IDY
                   END-IF
               END-PERFORM
               MOVE    HKN-KENSNUMCHKKBN   TO  SPA-GMN-KENSNUMCHKKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-KENSNUMCHKKBN-MAX
                   IF      SPA-GMN-KENSNUMCHKKBN
                                      =   SPA-GMN-KENSNUMCHKKBNL (IDY)
                       MOVE    SPA-GMN-KENSNUMCHKKBNMEIL (IDY)
                                      TO  SPA-GMN-KENSNUMCHKKBNMEI
                       MOVE    SPA-GMN-KENSNUMCHKKBN-MAX
                                      TO  IDY
                   END-IF
               END-PERFORM
               MOVE    HKN-JKYSKENSNUMCHKKBN   TO 
                                          SPA-GMN-JKYSKENSNUMCHKKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-JKYSKENSNUMCHKKBN-MAX
                   IF      SPA-GMN-JKYSKENSNUMCHKKBN
                                  =   SPA-GMN-JKYSKENSNUMCHKKBNL (IDY)
                       MOVE    SPA-GMN-JKYSKENSNUMCHKKBNMEIL (IDY)
                                      TO  SPA-GMN-JKYSKENSNUMCHKKBNMEI
                       MOVE    SPA-GMN-JKYSKENSNUMCHKKBN-MAX
                                      TO  IDY
                   END-IF
               END-PERFORM
               MOVE    HKN-HKNKOHKBN       TO  SPA-GMN-HKNKOHKBN 
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-HKNKOHKBN-MAX
                   IF      SPA-GMN-HKNKOHKBN
                                          =   SPA-GMN-HKNKOHKBNL (IDY)
                       MOVE    SPA-GMN-HKNKOHKBNMEIL (IDY)
                                      TO  SPA-GMN-HKNKOHKBNMEI
                       MOVE    SPA-GMN-HKNKOHKBN-MAX
                                      TO  IDY
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3  
                   MOVE    HKN-GENHKNNUM (IDX)
                                           TO  SPA-GMN-GENHKNNUM (IDX) 
               END-PERFORM 
               MOVE    HKN-JOKEN-STYEAR    TO  SPA-GMN-JOKENSTYEAR
               MOVE    HKN-JOKEN-EDYEAR    TO  SPA-GMN-JOKENEDYEAR 
               MOVE    HKN-TENTNK          TO  SPA-GMN-TENTNK
               MOVE    HKN-RECESKYKBN      TO  SPA-GMN-RECESKYKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-RECESKYKBN-MAX
                   IF      SPA-GMN-RECESKYKBN
                                          =   SPA-GMN-RECESKYKBNL (IDY)
                       MOVE    SPA-GMN-RECESKYKBNMEIL (IDY)
                                          TO  SPA-GMN-RECESKYKBNMEI
                       MOVE    SPA-GMN-RECESKYKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    HKN-RECEFTNDSPKBN  TO  SPA-GMN-RECEFTNDSPKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-RECEFTNDSPKBN-MAX
                   IF      SPA-GMN-RECEFTNDSPKBN
                                      =   SPA-GMN-RECEFTNDSPKBNL (IDY)
                       MOVE    SPA-GMN-RECEFTNDSPKBNMEIL (IDY)
                                          TO  SPA-GMN-RECEFTNDSPKBNMEI
                       MOVE    SPA-GMN-RECEFTNDSPKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    HKN-RECEJGNDSPKBN  TO  SPA-GMN-RECEFTNJGNDSPKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-RECEFTNJGNDSPKBN-MAX
                   IF      SPA-GMN-RECEFTNJGNDSPKBN
                                   =   SPA-GMN-RECEFTNJGNDSPKBNL (IDY)
                       MOVE    SPA-GMN-RECEFTNJGNDSPKBNMEIL (IDY)
                                       TO  SPA-GMN-RECEFTNJGNDSPKBNMEI
                       MOVE    SPA-GMN-RECEFTNJGNDSPKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
      *        法別番号チェック区分
               MOVE    HKN-HBTNUMCHKKBN  TO  SPA-GMN-HBTNUMCHKKBN
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      2
                   IF      SPA-GMN-HBTNUMCHKKBN
                                      =   SPA-GMN-HBTNUMCHKKBN-LST(IDY)
                                                           (1:1)
                       MOVE    SPA-GMN-HBTNUMCHKKBN-LST (IDY)
                                          TO  SPA-GMN-HBTNUMCHKKBN-G
                       MOVE    2          TO  IDY
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4  
                   MOVE    HKN-GAIFTNKBN       (IDX)
                                       TO  SPA-GMN-GAIFTNKBN     (IDX)
                   PERFORM VARYING IDY FROM   1   BY  1
                           UNTIL   IDY >      SPA-GMN-GAIFTNKBN-MAX
                       IF      SPA-GMN-GAIFTNKBN (IDX)
                                          =   SPA-GMN-GAIFTNKBNL (IDY)
                           MOVE    SPA-GMN-GAIFTNKBNMEIL (IDY)
                                          TO  SPA-GMN-GAIFTNKBNMEI(IDX)
                           MOVE    SPA-GMN-GAIFTNKBN-MAX
                                          TO  IDY
                       END-IF
                   END-PERFORM
                   MOVE    HKN-GAIKFTNRATE     (IDX)
                                       TO  SPA-GMN-GAIKFTNRATE   (IDX)
                   MOVE    HKN-GAIKKOTGAK      (IDX)
                                       TO  SPA-GMN-GAIKKOTGAK    (IDX) 
                   MOVE    HKN-GAIKJGNGAK      (IDX)
                                       TO  SPA-GMN-GAIKJGNGAK    (IDX)
                   MOVE    HKN-GAIDJGNGAK      (IDX)
                                       TO  SPA-GMN-GAIDJGNGAK    (IDX)
                   MOVE    HKN-GAIDJGNCNT      (IDX) 
                                       TO  SPA-GMN-GAIDJGNCNT    (IDX)
                   MOVE    HKN-GAIMNAIJGNGAK   (IDX)
                                       TO  SPA-GMN-GAIMNAIJGNGAK (IDX)
                   MOVE    HKN-GAIMGAIJGNGAK   (IDX) 
                                       TO  SPA-GMN-GAIMGAIJGNGAK (IDX)
                   MOVE    HKN-GAIMJGNCNT      (IDX)
                                       TO  SPA-GMN-GAIMJGNCNT    (IDX)
                   MOVE    HKN-GAIYKZFTNKBN    (IDX)
                                       TO  SPA-GMN-GAIYKZFTNKBN  (IDX)
                  PERFORM VARYING IDY FROM   1   BY  1
                           UNTIL   IDY >     SPA-GMN-GAIYKZFTNKBN-MAX
                       IF      SPA-GMN-GAIYKZFTNKBN (IDX)
                                      =   SPA-GMN-GAIYKZFTNKBNL (IDY)
                           MOVE    SPA-GMN-GAIYKZFTNKBNMEIL (IDY)
                                      TO SPA-GMN-GAIYKZFTNKBNMEI(IDX)
                           MOVE    SPA-GMN-GAIYKZFTNKBN-MAX
                                      TO  IDY
                       END-IF
                   END-PERFORM
                   MOVE    HKN-NYUFTNKBN       (IDX)
                                       TO  SPA-GMN-NYUFTNKBN     (IDX)
                   PERFORM VARYING IDY FROM   1   BY  1
                           UNTIL   IDY >      SPA-GMN-NYUFTNKBN-MAX
                       IF      SPA-GMN-NYUFTNKBN (IDX)
                                          =   SPA-GMN-NYUFTNKBNL (IDY)
                           MOVE    SPA-GMN-NYUFTNKBNMEIL (IDY)
                                          TO  SPA-GMN-NYUFTNKBNMEI(IDX)
                           MOVE    SPA-GMN-NYUFTNKBN-MAX
                                          TO  IDY
                       END-IF
                   END-PERFORM
                   MOVE    HKN-NYUKFTNRATE     (IDX) 
                                       TO  SPA-GMN-NYUKFTNRATE   (IDX)
                   MOVE    HKN-NYUKKOTGAK      (IDX)
                                       TO  SPA-GMN-NYUKKOTGAK    (IDX)
                   MOVE    HKN-NYUKJGNGAK      (IDX) 
                                       TO  SPA-GMN-NYUKJGNGAK    (IDX)
                   MOVE    HKN-NYUDJGNGAK      (IDX)
                                       TO  SPA-GMN-NYUDJGNGAK    (IDX)
                   MOVE    HKN-NYUDJGNCNT      (IDX)
                                       TO  SPA-GMN-NYUDJGNCNT    (IDX)
                   MOVE    HKN-NYUMJGNGAK      (IDX)
                                       TO  SPA-GMN-NYUMJGNGAK    (IDX)
                   MOVE    HKN-NYUMJGNCNT      (IDX)
                                       TO  SPA-GMN-NYUMJGNCNT    (IDX)
                   MOVE    HKN-NYUSKJRYYKBN    (IDX)
                                       TO  SPA-GMN-NYUSKJRYYKBN  (IDX)
      *            食事助成額
                   MOVE    HKN-NYUSKJJYOGAK    (IDX)
                                       TO  SPA-GMN-NYUSKJJYOGAK    (IDX)
                   PERFORM VARYING IDY FROM   1   BY  1
                           UNTIL   IDY >      SPA-GMN-NYUSKJRYYKBN-MAX
                       IF      SPA-GMN-NYUSKJRYYKBN (IDX)
                                      =   SPA-GMN-NYUSKJRYYKBNL (IDY)
                           MOVE    SPA-GMN-NYUSKJRYYKBNMEIL (IDY)
                                      TO SPA-GMN-NYUSKJRYYKBNMEI(IDX)
                           MOVE    SPA-GMN-NYUSKJRYYKBN-MAX 
                                      TO  IDY
                       END-IF
                   END-PERFORM
               END-PERFORM 
           END-IF
      *
      *    初期値設定
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
      *        外来負担区分
               IF      SPA-GMN-GAIFTNKBN (IDX) =   SPACE
                   MOVE    SPA-GMN-GAIFTNKBNLST (01)
                                          TO  SPA-GMN-GAIFTNKBN-G (IDX)
               END-IF
               IF      SPA-GMN-GAIYKZFTNKBN (IDX) =   SPACE
                   MOVE    SPA-GMN-GAIYKZFTNKBNLST (01)
                                          TO  SPA-GMN-GAIYKZFTNKBN-G
                                                                  (IDX)
               END-IF
      *        入院負担区分
               IF      SPA-GMN-NYUFTNKBN (IDX) =   SPACE
                   MOVE    SPA-GMN-NYUFTNKBNLST (01)
                                          TO  SPA-GMN-NYUFTNKBN-G (IDX)
               END-IF
               IF      SPA-GMN-NYUSKJRYYKBN (IDX)  =   SPACE
                   MOVE    SPA-GMN-NYUSKJRYYKBNLST (01)
                                          TO  SPA-GMN-NYUSKJRYYKBN-G
                                                                  (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-GMN-PAGENO 
      *  
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 403-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    タグ
               WHEN    "CLICKED"       ALSO    "B08"
      *        WHEN    "CLICKED"       ALSO    "N01"
      *        WHEN    "CLICKED"       ALSO    "N02"
      *        WHEN    "CLICKED"       ALSO    "N03"
      *        WHEN    "CLICKED"       ALSO    "N04"
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    1                   TO  FLG-KOUSIN
      *
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "SWITCH"        ALSO    "not3"
                   PERFORM 440-TAG-CHANGE-SEC
           WHEN    OTHER
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
           
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
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    V02-HBTNUM          TO  SPA-GMN-HBTNUM
           MOVE    V02-SEIDONAME       TO  SPA-GMN-SEIDONAME
           MOVE    V02-TANSEIDONAME    TO  SPA-GMN-TANSEIDONAME
           MOVE    V02-HKNKOHSBTKBN    TO  SPA-GMN-HKNKOHSBTKBN-G
           MOVE    V02-HBTNUMCHKKBN    TO  SPA-GMN-HBTNUMCHKKBN-G
           MOVE    V02-KENSNUMCHKKBN   TO  SPA-GMN-KENSNUMCHKKBN-G
           MOVE    V02-JKYSKENSNUMCHKKBN
                                       TO  SPA-GMN-JKYSKENSNUMCHKKBN-G
           MOVE    V02-HKNKOHKBN       TO  SPA-GMN-HKNKOHKBN-G
      *    限定保険番号チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    V02-GENHKNNUM (IDX)  TO  SPA-GMN-GENHKNNUM (IDX)
           END-PERFORM
      *
           MOVE    V02-JOKENSTYEAR     TO  SPA-GMN-JOKENSTYEAR
           MOVE    V02-JOKENEDYEAR     TO  SPA-GMN-JOKENEDYEAR
           MOVE    V02-TENTNK          TO  SPA-GMN-TENTNK
           MOVE    V02-RECESKYKBN      TO  SPA-GMN-RECESKYKBN-G
           MOVE    V02-RECEFTNDSPKBN   TO  SPA-GMN-RECEFTNDSPKBN-G
           MOVE    V02-RECEFTNJGNDSPKBN
                                       TO  SPA-GMN-RECEFTNJGNDSPKBN-G
      *
      *    タグ別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               PERFORM 41011-GMN-SPA-TAU-SEC
           END-PERFORM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ別画面をＳＰＡセット処理
      *****************************************************************
       41011-GMN-SPA-TAU-SEC       SECTION.
      *
      *    外来負担区分チェック
           MOVE    V02-GAIFTNKBN (IDX) TO  SPA-GMN-GAIFTNKBN-G (IDX)
           MOVE    V02-GAIKFTNRATE (IDX)
                                       TO  SPA-GMN-GAIKFTNRATE (IDX)
           MOVE    V02-GAIKKOTGAK  (IDX)
                                       TO  SPA-GMN-GAIKKOTGAK (IDX)
           MOVE    V02-GAIKJGNGAK  (IDX)
                                       TO  SPA-GMN-GAIKJGNGAK (IDX)
           MOVE    V02-GAIDJGNGAK(IDX) TO  SPA-GMN-GAIDJGNGAK (IDX)
           MOVE    V02-GAIDJGNCNT(IDX) TO  SPA-GMN-GAIDJGNCNT (IDX)
           MOVE    V02-GAIMNAIJGNGAK(IDX)
                                       TO  SPA-GMN-GAIMNAIJGNGAK (IDX)
           MOVE    V02-GAIMGAIJGNGAK (IDX)
                                       TO  SPA-GMN-GAIMGAIJGNGAK (IDX)
           MOVE    V02-GAIMJGNCNT    (IDX)
                                       TO  SPA-GMN-GAIMJGNCNT (IDX)
           MOVE    V02-GAIYKZFTNKBN  (IDX)
                                       TO  SPA-GMN-GAIYKZFTNKBN-G(IDX)
           MOVE    V02-NYUFTNKBN     (IDX)
                                       TO  SPA-GMN-NYUFTNKBN-G (IDX)
           MOVE    V02-NYUKFTNRATE   (IDX)
                                       TO  SPA-GMN-NYUKFTNRATE (IDX)
           MOVE    V02-NYUKKOTGAK(IDX) TO  SPA-GMN-NYUKKOTGAK (IDX)
           MOVE    V02-NYUKJGNGAK(IDX) TO  SPA-GMN-NYUKJGNGAK (IDX)
           MOVE    V02-NYUDJGNGAK(IDX) TO  SPA-GMN-NYUDJGNGAK (IDX)
           MOVE    V02-NYUDJGNCNT(IDX) TO  SPA-GMN-NYUDJGNCNT (IDX)
           MOVE    V02-NYUMJGNGAK (IDX)
                                       TO  SPA-GMN-NYUMJGNGAK (IDX)
           MOVE    V02-NYUMJGNCNT(IDX) TO  SPA-GMN-NYUMJGNCNT (IDX)
           MOVE    V02-NYUSKJRYYKBN(IDX)
                                       TO  SPA-GMN-NYUSKJRYYKBN-G (IDX)
      *    食事助成額
           MOVE    V02-NYUSKJJYOGAK (IDX)
                                       TO  SPA-GMN-NYUSKJJYOGAK (IDX)
      *
           .
       41011-GMN-SPA-TAU-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 4100-HBTNUM-CHK-SEC
      *    制度名チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-SEIDONAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-TANSEIDONAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-HKNKOHSBTKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-HBTNUMCHKKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-KENSNUMCHKKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4105-JKYSKENSNUMCHKKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4106-HKNKOHKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4130-RECESKYKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4140-RECEFTNDSPKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4150-RECEFTNJGNDSPKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4111-GAIFTNKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4112-GAIKFTNRATE-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-GAIKKOTGAK-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4120-GAIYKZFTNKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4121-NYUFTNKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4129-NYUSKJRYYKBN-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号チェック
      *****************************************************************
       4100-HBTNUM-CHK-SEC       SECTION.
      *
           .
       4100-HBTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    制度名チェック
      *****************************************************************
       4101-SEIDONAME-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SEIDONAME   =   SPACE
               IF      FLG-KOUSIN          =   2
                   MOVE    "0020"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
               GO      TO  4101-SEIDONAME-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SEIDONAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-SEIDONAME
           END-IF
      *
           .
       4101-SEIDONAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    短縮制度名チェック
      *****************************************************************
       4102-TANSEIDONAME-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-TANSEIDONAME    =   SPACE
               IF      FLG-KOUSIN          =   2
                   MOVE    "0021"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN-CUR
               END-IF
               GO      TO  4102-TANSEIDONAME-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-TANSEIDONAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0021"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-TANSEIDONAME
           END-IF
      *
           .
       4102-TANSEIDONAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険公費種別区分チェック
      *****************************************************************
       4103-HKNKOHSBTKBN-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-HKNKOHSBTKBN    =   SPACE
               IF      FLG-KOUSIN      NOT =   2
                   GO      TO  4103-HKNKOHSBTKBN-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNKOHSBTKBN-MAX
               IF      SPA-GMN-HKNKOHSBTKBN
                                      =   SPA-GMN-HKNKOHSBTKBNL (IDX)
                   MOVE    SPA-GMN-HKNKOHSBTKBNMEIL (IDX)
                                           TO  SPA-GMN-HKNKOHSBTKBNMEI
                   MOVE    SPA-GMN-HKNKOHSBTKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
       4103-HKNKOHSBTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号チェック区分チェック処理
      *****************************************************************
       4103-HBTNUMCHKKBN-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-HBTNUMCHKKBN    =   SPACE
               IF      FLG-KOUSIN      NOT =   2
                   GO      TO  4103-HBTNUMCHKKBN-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-HBTNUMCHKKBN
                                      =   SPA-GMN-HBTNUMCHKKBN-LST(IDX)
                                                                 (1:1)
                   MOVE    SPA-GMN-HBTNUMCHKKBN-LST (IDX)
                                           TO  SPA-GMN-HBTNUMCHKKBN-G
                   MOVE    2               TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    16                  TO  SPA-GMN-CUR
               MOVE    "0022"              TO  SPA-ERRCD
           END-IF
           .
       4103-HBTNUMCHKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検証番号チェック区分チェック
      *****************************************************************
       4104-KENSNUMCHKKBN-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-KENSNUMCHKKBN    =   SPACE
               IF      FLG-KOUSIN      NOT =   2
                   GO      TO  4104-KENSNUMCHKKBN-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KENSNUMCHKKBN-MAX
               IF      SPA-GMN-KENSNUMCHKKBN
                                      =   SPA-GMN-KENSNUMCHKKBNL (IDX)
                   MOVE    SPA-GMN-KENSNUMCHKKBNMEIL (IDX)
                                           TO  SPA-GMN-KENSNUMCHKKBNMEI
                   MOVE    SPA-GMN-KENSNUMCHKKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    5                   TO  SPA-GMN-CUR
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
      *
           .
       4104-KENSNUMCHKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者検証番号チェック区分チェック
      *****************************************************************
       4105-JKYSKENSNUMCHKKBN-CHK-SEC          SECTION.
      *
      *    未入力でもＯＫとする
           IF      SPA-GMN-JKYSKENSNUMCHKKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-JKYSKENSNUMCHKKBN-G
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-JKYSKENSNUMCHKKBN-MAX
               IF      SPA-GMN-JKYSKENSNUMCHKKBN
                                      =   SPA-GMN-JKYSKENSNUMCHKKBNL
                                                              (IDX)
                   MOVE    SPA-GMN-JKYSKENSNUMCHKKBNMEIL (IDX)
                                      TO  SPA-GMN-JKYSKENSNUMCHKKBNMEI
                   MOVE    SPA-GMN-JKYSKENSNUMCHKKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO  )  AND
                  (SPA-GMN-JKYSKENSNUMCHKKBN   NOT =   SPACE )
               MOVE    6                   TO  SPA-GMN-CUR
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
           .
       4105-JKYSKENSNUMCHKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費主保区分チェック
      *****************************************************************
       4106-HKNKOHKBN-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HKNKOHKBN   =   SPACE
               MOVE    SPACE           TO   SPA-GMN-HKNKOHKBN-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNKOHKBN-MAX
               IF      SPA-GMN-HKNKOHKBN
                                      =   SPA-GMN-HKNKOHKBNL (IDX)
                   MOVE    SPA-GMN-HKNKOHKBNMEIL (IDX)
                                           TO  SPA-GMN-HKNKOHKBNMEI
                   MOVE    SPA-GMN-HKNKOHKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO  )  AND
                  (SPA-GMN-HKNKOHKBN   NOT =   SPACE )
               MOVE    7                   TO  SPA-GMN-CUR
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-GMN-HKNKOHSBTKBN
               WHEN    "6"
                   IF      SPA-GMN-HKNKOHKBN   NOT =   SPACE
                       MOVE    "0004"              TO  SPA-ERRCD
                       MOVE    7                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "7"
                   CONTINUE
      *             IF      SPA-GMN-HKNKOHKBN   =   SPACE
      *                 MOVE    "0010"              TO  SPA-ERRCD
      *                 MOVE    7                   TO  SPA-GMN-CUR
      *             END-IF
               END-EVALUATE
           END-IF
      *
           .
       4106-HKNKOHKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    限定保険番号チェック
      *****************************************************************
       4107-GENHKNNUM-CHK-SEC            SECTION.
      *
           .
       4107-GENHKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件−年齢開始チェック
      *****************************************************************
       4108-JOKENSTYEAR-CHK-SEC        SECTION.
      *
           .
       4108-JOKENSTYEAR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件−年齢終了チェック
      *****************************************************************
       4109-JOKENEDYEAR-CHK-SEC            SECTION.
      *
           .
       4109-JOKENEDYEAR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数単価チェック
      *****************************************************************
       4110-TENTNK-CHK-SEC         SECTION.
      *
           .
       4110-TENTNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト請求チェック
      *****************************************************************
       4130-RECESKYKBN-CHK-SEC         SECTION.
      *
      *    IF     (SPA-GMN-RECESKYKBN    =   SPACE )
      *      AND  (FLG-KOUSIN        NOT =   2     )
      *        GO      TO  4130-RECESKYKBN-CHK-EXT
      *    END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECESKYKBN-MAX
               IF      SPA-GMN-RECESKYKBN
                                      =   SPA-GMN-RECESKYKBNL (IDX)
                   MOVE    SPA-GMN-RECESKYKBNMEIL (IDX)
                                           TO  SPA-GMN-RECESKYKBNMEI
                   MOVE    SPA-GMN-RECESKYKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (FLG-CHK              =   ZERO )  AND
                  (SPA-GMN-RECESKYKBN   NOT =   SPACE )
               MOVE    14                  TO  SPA-GMN-CUR
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF
      *
      *    地方公費の時必須とする
           IF      FLG-KOUSIN          =   2
               IF      (SPA-V01-HKNNUM     >=  "100"  AND
                                           <=  "899" )
                   IF      SPA-GMN-RECESKYKBN  =   SPACE
                       MOVE    14                  TO  SPA-GMN-CUR
                       MOVE    "0005"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       4130-RECESKYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト金額チェック
      *****************************************************************
       4140-RECEFTNDSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECEFTNDSPKBN-MAX
               IF      SPA-GMN-RECEFTNDSPKBN
                                      =   SPA-GMN-RECEFTNDSPKBNL (IDX)
                   MOVE    SPA-GMN-RECEFTNDSPKBNMEIL (IDX)
                                           TO  SPA-GMN-RECEFTNDSPKBNMEI
                   MOVE    SPA-GMN-RECEFTNDSPKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (FLG-CHK              =   ZERO )
               MOVE    15                  TO  SPA-GMN-CUR
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *
           .
       4140-RECEFTNDSPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ記載チェック
      *****************************************************************
       4150-RECEFTNJGNDSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >  SPA-GMN-RECEFTNJGNDSPKBN-MAX
               IF      SPA-GMN-RECEFTNJGNDSPKBN
                                  =   SPA-GMN-RECEFTNJGNDSPKBNL (IDX)
                   MOVE    SPA-GMN-RECEFTNJGNDSPKBNMEIL (IDX)
                                      TO  SPA-GMN-RECEFTNJGNDSPKBNMEI
                   MOVE    SPA-GMN-RECEFTNJGNDSPKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (FLG-CHK              =   ZERO )
               MOVE    17                  TO  SPA-GMN-CUR
               MOVE    "0012"              TO  SPA-ERRCD
           END-IF
      *
           .
       4150-RECEFTNJGNDSPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来負担区分チェック
      *****************************************************************
       4111-GAIFTNKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   MOVE    1               TO  IDX
                   MOVE    20              TO  WRK-GMN-CUR
               WHEN    1
                   MOVE    2               TO  IDX
                   MOVE    50              TO  WRK-GMN-CUR
               WHEN    2
                   MOVE    3               TO  IDX
                   MOVE    80              TO  WRK-GMN-CUR
               WHEN    3
                   MOVE    4               TO  IDX
                   MOVE    110             TO  WRK-GMN-CUR
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL   IDY     >   SPA-GMN-GAIFTNKBN-MAX
               IF      SPA-GMN-GAIFTNKBN (IDX)
                                       =   SPA-GMN-GAIFTNKBNL (IDY)
                   MOVE    SPA-GMN-GAIFTNKBNMEIL (IDY)
                                           TO  SPA-GMN-GAIFTNKBNMEI(IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-GAIFTNKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
      *    IF     (SPA-GMN-GAIFTNKBN (IDX) =   SPACE )
      *      AND  (FLG-KOUSIN          NOT =   2     )
      *         GO      TO  4111-GAIFTNKBN-CHK-EXT
      *    END-IF
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
           END-IF
      *
          IF      SPA-GMN-GAIFTNKBN (IDX) =   "3"
          AND   ( SPA-ERRCD            =   SPACE  )
               IF      SPA-V01-HKNNUM (1:1)    >=  "1" AND <= "8"
                   CONTINUE
               ELSE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4111-GAIFTNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来薬剤負担区分チェック処理
      *****************************************************************
       4120-GAIYKZFTNKBN-CHK-SEC        SECTION.
      *
      *
           EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   MOVE    1               TO  IDX
                   MOVE    29              TO  WRK-GMN-CUR
               WHEN    1
                   MOVE    2               TO  IDX
                   MOVE    59              TO  WRK-GMN-CUR
               WHEN    2
                   MOVE    3               TO  IDX
                   MOVE    89              TO  WRK-GMN-CUR
               WHEN    3
                   MOVE    4               TO  IDX
                   MOVE    119             TO  WRK-GMN-CUR
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL   IDY     >   SPA-GMN-GAIYKZFTNKBN-MAX
               IF      SPA-GMN-GAIYKZFTNKBN (IDX)
                                       =   SPA-GMN-GAIYKZFTNKBNL (IDY)
                   MOVE    SPA-GMN-GAIYKZFTNKBNMEIL (IDY)
                                           TO  SPA-GMN-GAIYKZFTNKBNMEI
                                                                (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-GAIYKZFTNKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
      *    IF      SPA-GMN-GAIYKZFTNKBN (IDX) =   SPACE
      *        IF      FLG-KOUSIN      NOT =   2
      *            GO      TO      4120-GAIYKZFTNKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
           END-IF
      *
           .
       4120-GAIYKZFTNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院負担区分チェック
      *****************************************************************
       4121-NYUFTNKBN-CHK-SEC       SECTION.
      *
      *
           EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   MOVE    1               TO  IDX
                   MOVE    30              TO  WRK-GMN-CUR
               WHEN    1
                   MOVE    2               TO  IDX
                   MOVE    60              TO  WRK-GMN-CUR
               WHEN    2
                   MOVE    3               TO  IDX
                   MOVE    90              TO  WRK-GMN-CUR
               WHEN    3
                   MOVE    4               TO  IDX
                   MOVE    120             TO  WRK-GMN-CUR
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL   IDY     >   SPA-GMN-NYUFTNKBN-MAX
               IF      SPA-GMN-NYUFTNKBN (IDX)
                                       =   SPA-GMN-NYUFTNKBNL (IDY)
                   MOVE    SPA-GMN-NYUFTNKBNMEIL (IDY)
                                           TO  SPA-GMN-NYUFTNKBNMEI
                                                                (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-NYUFTNKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
      *    IF      SPA-GMN-NYUFTNKBN (IDX) =   SPACE
      *        IF      FLG-KOUSIN      NOT =   2
      *            GO      TO      4121-NYUFTNKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
               GO      TO      4121-NYUFTNKBN-CHK-EXT
           END-IF
           IF      SPA-GMN-NYUFTNKBN (IDX) =   "3"
               IF      SPA-V01-HKNNUM (1:1)    >=  "1" AND <= "8"
                   CONTINUE
               ELSE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4121-NYUFTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養費区分チェック処理
      *****************************************************************
       4129-NYUSKJRYYKBN-CHK-SEC        SECTION.
      *
      *
           EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   MOVE    1               TO  IDX
                   MOVE    39              TO  WRK-GMN-CUR
               WHEN    1
                   MOVE    2               TO  IDX
                   MOVE    69              TO  WRK-GMN-CUR
               WHEN    2
                   MOVE    3               TO  IDX
                   MOVE    99              TO  WRK-GMN-CUR
               WHEN    3
                   MOVE    4               TO  IDX
                   MOVE    129             TO  WRK-GMN-CUR
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL   IDY     >   SPA-GMN-NYUSKJRYYKBN-MAX
               IF      SPA-GMN-NYUSKJRYYKBN (IDX)
                                       =   SPA-GMN-NYUSKJRYYKBNL (IDY)
                   MOVE    SPA-GMN-NYUSKJRYYKBNMEIL (IDY)
                                           TO  SPA-GMN-NYUSKJRYYKBNMEI
                                                                (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-NYUSKJRYYKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
      *
      *    IF      SPA-GMN-NYUSKJRYYKBN (IDX) =   SPACE
      *        IF      FLG-KOUSIN      NOT =   2
      *            GO      TO      4129-NYUSKJRYYKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYUSKJRYYKBN (IDX)  =   "5" )
                   IF    ( SPA-GMN-HKNKOHSBTKBN    =   "8" OR "9" )
                       CONTINUE
                   ELSE
                       MOVE    "0023"          TO  SPA-ERRCD
                       MOVE    WRK-GMN-CUR     TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4129-NYUSKJRYYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来回負担割合チェック処理
      *****************************************************************
       4112-GAIKFTNRATE-CHK-SEC           SECTION.
      *
           .
       4112-GAIKFTNRATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来回固定額チェック処理
      *****************************************************************
       4113-GAIKKOTGAK-CHK-SEC        SECTION.
      *
           . 
       4113-GAIKKOTGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来回上限額チェック処理
      *****************************************************************
       4114-GAIKJGNGAK-CHK-SEC        SECTION.
      *
           .
       4114-GAIKJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来日上限額チェック処理
      *****************************************************************
       4115-GAIDJGNGAK-CHK-SEC        SECTION.
      *
           .
       4115-GAIDJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来日上限回数チェック処理
      *****************************************************************
       4116-GAIDJGNCNT-CHK-SEC        SECTION.
      *
           .
       4116-GAIDJGNCNT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来月院内上限額チェック処理
      *****************************************************************
       4117-GAIMNAIJGNGAK-CHK-SEC        SECTION.
      *
           .
       4117-GAIMNAIJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来月院外上限額チェック処理
      *****************************************************************
       4118-GAIMGAIJGNGAK-CHK-SEC        SECTION.
      *
           .
       4118-GAIMGAIJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来月上限回数チェック処理
      *****************************************************************
       4119-GAIMJGNCNT-CHK-SEC        SECTION.
      *
           .
       4119-GAIMJGNCNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院回負担割合チェック処理
      *****************************************************************
       4122-NYUKFTNRATE-CHK-SEC           SECTION.
      *
           .
       4122-NYUKFTNRATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院回固定額チェック処理
      *****************************************************************
       4123-NYUKKOTGAK-CHK-SEC        SECTION.
      *
           . 
       4123-NYUKKOTGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院回上限額チェック処理
      *****************************************************************
       4124-NYUKJGNGAK-CHK-SEC        SECTION.
      *
           .
       4124-NYUKJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院月上限額チェック処理
      *****************************************************************
       4125-NYUDJGNGAK-CHK-SEC        SECTION.
      *
           .
       4125-NYUDJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院日上限回数チェック処理
      *****************************************************************
       4126-NYUDJGNCNT-CHK-SEC        SECTION.
      *
           .
       4126-NYUDJGNCNT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院月院内上限額チェック処理
      *****************************************************************
       4127-NYUMJGNGAK-CHK-SEC        SECTION.
           .
       4127-NYUMJGNGAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院月上限回数チェック処理
      *****************************************************************
       4128-NYUMJGNCNT-CHK-SEC        SECTION.
      *
           .
       4128-NYUMJGNCNT-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    タグ編集処理
      *****************************************************************
       440-TAG-CHANGE-SEC            SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "B08"
                   IF    ( SPA-GMN-PAGENO  >=   3 )
                       MOVE    0       TO  WRK-PAGENO
                   ELSE
                       COMPUTE WRK-PAGENO
                            =  SPA-GMN-PAGENO   +  1
                   END-IF
               WHEN     OTHER
                   MOVE   V02-PAGENO   TO  WRK-PAGENO
           END-EVALUATE
      *
      *    タグが変わったとき関連チェック
           IF      WRK-PAGENO      NOT =   SPA-GMN-PAGENO
               MOVE    1                   TO  FLG-KOUSIN
               PERFORM 410-INPUT-CHK-SEC
               IF      SPA-ERRCD            =   SPACE
                   PERFORM 4900-KANRENCHK-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-PAGENO           TO  SPA-GMN-PAGENO
               EVALUATE    WRK-PAGENO
                   WHEN    0
                       MOVE    20           TO  SPA-GMN-CUR
                   WHEN    1
                       MOVE    50           TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    80           TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    110          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF        
           .
       440-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "V01"               TO  SPA-SAKIPG
           MOVE    "V02"               TO  SPA-MOTOPG
      *
      *    INITIALIZE                  SPA-V01KYOUTU
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-VIDCD
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
           INITIALIZE                      HKNNUM-REC
      *****MOVE    SPA-HOSPID              TO  HKN-HOSPID
           MOVE    SPA-V01-HKNNUM          TO  HKN-HKNNUM
           MOVE    SPA-V01-PAYKBN          TO  HKN-PAYKBN
           MOVE    SPA-V01-STYUKYMD        TO  HKN-TEKSTYMD
           MOVE    SPA-V01-EDYUKYMD        TO  HKN-TEKEDYMD
           MOVE    SPA-HOSPNUM             TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC              TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "V02 HKNNUM DEL ERR:"  MCP-RC
                       ",KEY:" HKN-KEY
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           MOVE    2                   TO  FLG-KOUSIN
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD        =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-VIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      * 
           INITIALIZE                      HKNNUM-REC
      *****MOVE    SPA-HOSPID              TO  HKN-HOSPID
           MOVE    SPA-V01-HKNNUM          TO  HKN-HKNNUM
           MOVE    SPA-V01-PAYKBN          TO  HKN-PAYKBN
      *
           IF     (SPA-V01-SYORI           =   2  )  AND
                  (SPA-OLD-STYUKYMD    NOT =   SPACE)
               MOVE    SPA-OLD-STYUKYMD        TO  HKN-TEKSTYMD
               MOVE    SPA-OLD-EDYUKYMD        TO  HKN-TEKEDYMD
           ELSE
               MOVE    SPA-V01-STYUKYMD        TO  HKN-TEKSTYMD
               MOVE    SPA-V01-EDYUKYMD        TO  HKN-TEKEDYMD
           END-IF
           MOVE    SPA-HOSPNUM             TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC              TO  MCPDATA-REC
           PERFORM 900-HKNNUM-INV-SEC
           IF      FLG-HKNNUM              =   ZERO
               MOVE    MCPDATA-REC             TO  HKNNUM-REC
      *        開始日に変更があれば削除して追加
               IF      HKN-TEKSTYMD        NOT =   SPA-V01-STYUKYMD
                   PERFORM 4901-KOUSHIN-DEL-SEC
                   MOVE    1               TO  FLG-HKNNUM
                   INITIALIZE                      HKNNUM-REC
               END-IF
           END-IF
           IF      FLG-HKNNUM          NOT =   ZERO
               MOVE    SPACE                   TO  HKNNUM-REC
               INITIALIZE                      HKNNUM-REC
      *********MOVE    SPA-HOSPID              TO  HKN-HOSPID
               MOVE    SPA-V01-HKNNUM          TO  HKN-HKNNUM
               MOVE    SPA-V01-PAYKBN          TO  HKN-PAYKBN
               MOVE    SPA-V01-STYUKYMD        TO  HKN-TEKSTYMD
           END-IF 
           MOVE    SPA-V01-EDYUKYMD        TO  HKN-TEKEDYMD
           MOVE    SPA-GMN-HBTNUM          TO  HKN-HBTNUM 
           MOVE    SPA-GMN-SEIDONAME       TO  HKN-SEIDONAME 
           MOVE    SPA-GMN-TANSEIDONAME    TO  HKN-TANSEIDONAME 
           MOVE    SPA-GMN-HKNKOHSBTKBN    TO  HKN-HKNKOHSBTKBN 
           MOVE    SPA-GMN-HBTNUMCHKKBN    TO  HKN-HBTNUMCHKKBN
           MOVE    SPA-GMN-KENSNUMCHKKBN   TO  HKN-KENSNUMCHKKBN 
           MOVE    SPA-GMN-JKYSKENSNUMCHKKBN
                                           TO  HKN-JKYSKENSNUMCHKKBN 
           MOVE    SPA-GMN-HKNKOHKBN       TO  HKN-HKNKOHKBN 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3  
               MOVE    SPA-GMN-GENHKNNUM (IDX) TO  HKN-GENHKNNUM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-JOKENSTYEAR     TO  HKN-JOKEN-STYEAR 
           MOVE    SPA-GMN-JOKENEDYEAR     TO  HKN-JOKEN-EDYEAR 
           MOVE    SPA-GMN-TENTNK          TO  HKN-TENTNK
           MOVE    SPA-GMN-RECESKYKBN      TO  HKN-RECESKYKBN
           MOVE    SPA-GMN-RECEFTNDSPKBN   TO  HKN-RECEFTNDSPKBN
           MOVE    SPA-GMN-RECEFTNJGNDSPKBN
                                           TO  HKN-RECEJGNDSPKBN
      *        
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4  
               MOVE    SPA-GMN-GAIFTNKBN       (IDX)
                                       TO  HKN-GAIFTNKBN     (IDX)
               MOVE    SPA-GMN-GAIKFTNRATE     (IDX)
                                       TO  HKN-GAIKFTNRATE   (IDX)
               MOVE    SPA-GMN-GAIKKOTGAK      (IDX) 
                                       TO  HKN-GAIKKOTGAK    (IDX) 
               MOVE    SPA-GMN-GAIKJGNGAK      (IDX)
                                       TO  HKN-GAIKJGNGAK    (IDX)
               MOVE    SPA-GMN-GAIDJGNGAK      (IDX)
                                       TO  HKN-GAIDJGNGAK    (IDX)
               MOVE    SPA-GMN-GAIDJGNCNT      (IDX)
                                       TO  HKN-GAIDJGNCNT    (IDX)    
               MOVE    SPA-GMN-GAIMNAIJGNGAK   (IDX)
                                       TO  HKN-GAIMNAIJGNGAK (IDX)    
               MOVE    SPA-GMN-GAIMGAIJGNGAK   (IDX)
                                       TO  HKN-GAIMGAIJGNGAK (IDX)    
               MOVE    SPA-GMN-GAIMJGNCNT      (IDX)
                                       TO  HKN-GAIMJGNCNT    (IDX)    
               MOVE    SPA-GMN-GAIYKZFTNKBN    (IDX)
                                       TO  HKN-GAIYKZFTNKBN  (IDX)    
               MOVE    SPA-GMN-NYUFTNKBN       (IDX)
                                       TO  HKN-NYUFTNKBN     (IDX)   
               MOVE    SPA-GMN-NYUKFTNRATE     (IDX)
                                       TO  HKN-NYUKFTNRATE   (IDX)    
               MOVE    SPA-GMN-NYUKKOTGAK      (IDX)
                                       TO  HKN-NYUKKOTGAK    (IDX)    
               MOVE    SPA-GMN-NYUKJGNGAK      (IDX)
                                       TO  HKN-NYUKJGNGAK    (IDX)    
               MOVE    SPA-GMN-NYUDJGNGAK      (IDX)
                                       TO  HKN-NYUDJGNGAK    (IDX)   
               MOVE    SPA-GMN-NYUDJGNCNT      (IDX)
                                       TO  HKN-NYUDJGNCNT    (IDX)    
               MOVE    SPA-GMN-NYUMJGNGAK      (IDX)
                                       TO  HKN-NYUMJGNGAK    (IDX)    
               MOVE    SPA-GMN-NYUMJGNCNT      (IDX)
                                       TO  HKN-NYUMJGNCNT    (IDX)    
               MOVE    SPA-GMN-NYUSKJRYYKBN    (IDX)
                                       TO  HKN-NYUSKJRYYKBN  (IDX)   
      *    食事助成額
               MOVE    SPA-GMN-NYUSKJJYOGAK    (IDX)
                                       TO  HKN-NYUSKJJYOGAK  (IDX)    
           END-PERFORM 
      *
           IF      FLG-HKNNUM              =   ZERO
      *        更新
               MOVE    SPA-HOSPNUM             TO  HKN-HOSPNUM
               MOVE    HKNNUM-REC              TO  MCPDATA-REC
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1001"                  TO  SPA-ERRCD
                   DISPLAY "V02 HKNNUM UPD ERR:"  MCP-RC
                           ",KEY:" HKN-KEY
               END-IF
           ELSE
      *        追加
               MOVE    SPA-HOSPNUM             TO  HKN-HOSPNUM
               MOVE    HKNNUM-REC              TO  MCPDATA-REC
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1001"                  TO  SPA-ERRCD
                   DISPLAY "V02 HKNNUM INS ERR:"  MCP-RC
                           ",KEY:" HKN-KEY
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    更新分削除処理
      *****************************************************************
       4901-KOUSHIN-DEL-SEC           SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       4901-KOUSHIN-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
           MOVE    SPA-GMN-CUR          TO  WRK-GMN-CUR
      *
      *    保険公費種別区分が一般公費であれば、公費主保区分が
      *    設定されていること
           IF    SPA-GMN-HKNKOHSBTKBN   =   "7"
               IF   SPA-GMN-HKNKOHKBN   =   SPACE
                   MOVE   "0013"        TO  SPA-ERRCD
                   MOVE    7            TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO SPA-GMN-CUR
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-VIDCD       NOT =   SPACE
               PERFORM 520-VIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "V02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE                TO  V02
           INITIALIZE                  V02
      *
           MOVE    SPA-GMN-STYUKYMD     TO  V02-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD     TO  V02-EDYUKYMD
           MOVE    SPA-GMN-HKNNUM       TO  V02-HKNNUM
           MOVE    SPA-GMN-PAYKBN       TO  V02-PAYKBN
      *
           MOVE    SPA-GMN-HBTNUM       TO  V02-HBTNUM
           MOVE    SPA-GMN-SEIDONAME    TO  V02-SEIDONAME
           MOVE    SPA-GMN-TANSEIDONAME TO  V02-TANSEIDONAME
           MOVE    SPA-GMN-HKNKOHSBTKBN-G
                                        TO  V02-HKNKOHSBTKBN
           MOVE    SPA-GMN-KENSNUMCHKKBN-G
                                        TO  V02-KENSNUMCHKKBN
           MOVE    SPA-GMN-JKYSKENSNUMCHKKBN-G
                                        TO  V02-JKYSKENSNUMCHKKBN
           MOVE    SPA-GMN-HKNKOHKBN-G  TO  V02-HKNKOHKBN
           PERFORM VARYING IDX FROM     1  BY  1
                   UNTIL   IDX >        3  
               MOVE    SPA-GMN-GENHKNNUM (IDX) TO  V02-GENHKNNUM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-JOKENSTYEAR  TO  V02-JOKENSTYEAR  
           MOVE    SPA-GMN-JOKENEDYEAR  TO  V02-JOKENEDYEAR
           MOVE    SPA-GMN-TENTNK       TO  V02-TENTNK
           MOVE    SPA-GMN-RECESKYKBN-G TO  V02-RECESKYKBN
           MOVE    SPA-GMN-RECEFTNDSPKBN-G
                                        TO  V02-RECEFTNDSPKBN
           MOVE    SPA-GMN-RECEFTNJGNDSPKBN-G
                                        TO  V02-RECEFTNJGNDSPKBN
      *
           PERFORM VARYING IDX FROM     1  BY  1
                   UNTIL   IDX >        4  
               MOVE    SPA-GMN-GAIFTNKBN-G     (IDX)
                                        TO  V02-GAIFTNKBN     (IDX)
               MOVE    SPA-GMN-GAIKFTNRATE     (IDX)
                                        TO  V02-GAIKFTNRATE   (IDX) 
               MOVE    SPA-GMN-GAIKKOTGAK      (IDX)
                                        TO  V02-GAIKKOTGAK    (IDX) 
               MOVE    SPA-GMN-GAIKJGNGAK      (IDX)
                                        TO  V02-GAIKJGNGAK    (IDX) 
               MOVE    SPA-GMN-GAIDJGNGAK      (IDX)
                                        TO  V02-GAIDJGNGAK    (IDX)
               MOVE    SPA-GMN-GAIDJGNCNT      (IDX)
                                        TO  V02-GAIDJGNCNT    (IDX) 
               MOVE    SPA-GMN-GAIMNAIJGNGAK   (IDX)
                                        TO  V02-GAIMNAIJGNGAK (IDX) 
               MOVE    SPA-GMN-GAIMGAIJGNGAK   (IDX)
                                        TO  V02-GAIMGAIJGNGAK (IDX)
               MOVE    SPA-GMN-GAIMJGNCNT      (IDX)
                                        TO  V02-GAIMJGNCNT    (IDX)
               MOVE    SPA-GMN-GAIYKZFTNKBN-G  (IDX)
                                        TO  V02-GAIYKZFTNKBN  (IDX)
               MOVE    SPA-GMN-NYUFTNKBN-G     (IDX)
                                        TO  V02-NYUFTNKBN     (IDX)
               MOVE    SPA-GMN-NYUKFTNRATE     (IDX)
                                        TO  V02-NYUKFTNRATE   (IDX)
               MOVE    SPA-GMN-NYUKKOTGAK      (IDX)
                                        TO  V02-NYUKKOTGAK    (IDX)
               MOVE    SPA-GMN-NYUKJGNGAK      (IDX)
                                        TO  V02-NYUKJGNGAK    (IDX)
               MOVE    SPA-GMN-NYUDJGNGAK      (IDX)
                                        TO  V02-NYUDJGNGAK    (IDX)
               MOVE    SPA-GMN-NYUDJGNCNT      (IDX)
                                        TO  V02-NYUDJGNCNT    (IDX)
               MOVE    SPA-GMN-NYUMJGNGAK      (IDX)
                                        TO  V02-NYUMJGNGAK    (IDX)
               MOVE    SPA-GMN-NYUMJGNCNT      (IDX)
                                        TO  V02-NYUMJGNCNT    (IDX)
               MOVE    SPA-GMN-NYUSKJRYYKBN-G  (IDX)
                                        TO  V02-NYUSKJRYYKBN  (IDX)
      *    食事助成額
               MOVE    SPA-GMN-NYUSKJJYOGAK     (IDX)
                                        TO  V02-NYUSKJJYOGAK   (IDX)
           END-PERFORM
      *     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-HKNKOHSBTKBN-MAX
               MOVE    SPA-GMN-HKNKOHSBTKBNLST (IDX)
                                       TO  V02-HKNKOHSBTKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HKNKOHSBTKBN-MAX
                                       TO  V02-HKNKOHSBTKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-KENSNUMCHKKBN-MAX
               MOVE    SPA-GMN-KENSNUMCHKKBNLST (IDX)
                                       TO  V02-KENSNUMCHKKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-KENSNUMCHKKBN-MAX
                                       TO  V02-KENSNUMCHKKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-JKYSKENSNUMCHKKBN-MAX
               MOVE    SPA-GMN-JKYSKENSNUMCHKKBNLST (IDX)
                                   TO  V02-JKYSKENSNUMCHKKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-JKYSKENSNUMCHKKBN-MAX
                                       TO  V02-JKYSKENSNUMCHKKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-HKNKOHKBN-MAX
               MOVE    SPA-GMN-HKNKOHKBNLST (IDX)
                                       TO  V02-HKNKOHKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HKNKOHKBN-MAX
                                       TO  V02-HKNKOHKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECESKYKBN-MAX
               MOVE    SPA-GMN-RECESKYKBNLST (IDX)
                                       TO  V02-RECESKYKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECESKYKBN-MAX
                                       TO  V02-RECESKYKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECEFTNDSPKBN-MAX
               MOVE    SPA-GMN-RECEFTNDSPKBNLST (IDX)
                                       TO  V02-RECEFTNDSPKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEFTNDSPKBN-MAX
                                       TO  V02-RECEFTNDSPKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-RECEFTNJGNDSPKBN-MAX
               MOVE    SPA-GMN-RECEFTNJGNDSPKBNLST (IDX)
                                    TO  V02-RECEFTNJGNDSPKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEFTNJGNDSPKBN-MAX
                                       TO  V02-RECEFTNJGNDSPKBN-COUNT
      *
      *    法別番号チェック区分
           MOVE    SPA-GMN-HBTNUMCHKKBN-G  TO  V02-HBTNUMCHKKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   2
               MOVE    SPA-GMN-HBTNUMCHKKBN-LST (IDX)
                                       TO  V02-HBTNUMCHKKBNLST (IDX)
           END-PERFORM
           MOVE    2                   TO  V02-HBTNUMCHKKBN-COUNT
     *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX     >   SPA-GMN-GAIYKZFTNKBN-MAX
                   MOVE    SPA-GMN-GAIYKZFTNKBNLST (IDX)
                                   TO  V02-GAIYKZFTNKBNLST (IDY IDX)
               END-PERFORM
               MOVE    SPA-GMN-GAIYKZFTNKBN-MAX
                                   TO  V02-GAIYKZFTNKBN-COUNT (IDY)
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-GAIFTNKBN-MAX
                   MOVE    SPA-GMN-GAIFTNKBNLST (IDX)
                                   TO  V02-GAIFTNKBNLST (IDY IDX)
               END-PERFORM
               MOVE    SPA-GMN-GAIFTNKBN-MAX
                                   TO  V02-GAIFTNKBN-COUNT (IDY)
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-NYUFTNKBN-MAX
                  MOVE    SPA-GMN-NYUFTNKBNLST (IDX)
                                   TO  V02-NYUFTNKBNLST (IDY IDX)
               END-PERFORM
               MOVE    SPA-GMN-NYUFTNKBN-MAX
                                   TO  V02-NYUFTNKBN-COUNT (IDY)
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-NYUSKJRYYKBN-MAX
                   MOVE    SPA-GMN-NYUSKJRYYKBNLST (IDX)
                                   TO  V02-NYUSKJRYYKBNLST (IDY IDX)
               END-PERFORM
               MOVE    SPA-GMN-NYUSKJRYYKBN-MAX
                                   TO  V02-NYUSKJRYYKBN-COUNT (IDY)
           END-PERFORM
      *
           MOVE    SPA-GMN-PAGENO       TO  V02-PAGENO      
      *
      *
           MOVE    WIDGET-INSENSITIVE     TO  V02-B03-STATE
      *
           IF     (SPA-V01-SELNUM      >   ZERO )  AND
                  (SPA-V01-SYORI   NOT =   1    )
               MOVE    WIDGET-NORMAL      TO  V02-B03-STATE
           END-IF
      *      
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "保険公費種別区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "検証番号チェック区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "受給者検証番号チェック区分区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "公費主保区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "レセプト請求区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "外来・負担区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "薬剤負担区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "入院・負担区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "食事療養費入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "該当の保険番号のとき選べない負担区分です"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "レセプト負担金額入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "レセプト記載入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "一般公費のとき公費主保区分が必須です"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "制度名入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "短縮制度名入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "法別番号チェック区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0023"
                 MOVE "該当の食事療養費の設定は自費保険のみ選択可能です"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "保険番号マスタの更新処理ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "保険番号マスタの削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  VERR
           MOVE    SPA-ERRCD            TO  VERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  VERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "V02"                TO  SPA-MOTOPG
           MOVE    "VERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "VERR"               TO  MCP-WINDOW
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
       520-VIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-VIDMSG
      *
           EVALUATE    SPA-VIDCD
               WHEN    "1001"
                   MOVE    "保険番号マスタを登録します"
                                       TO  WRK-VIDMSG
               WHEN    "1002"
                   MOVE    "保険番号マスタを削除します"
                                       TO  WRK-VIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  VID1
           INITIALIZE                       VID1
           MOVE    SPA-VIDCD            TO  VID1-ID1CODE
           MOVE    WRK-VIDMSG           TO  VID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "V02"                TO  SPA-MOTOPG
           MOVE    "VID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "VID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-VIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF     SPA-GMN-CUR           =   ZERO
               PERFORM 50011-GMN-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE  "HBTNUM"         TO   MCP-WIDGET
                WHEN    02
                    MOVE  "SEIDONAME"      TO   MCP-WIDGET
                WHEN    03
                    MOVE  "TANSEIDONAME"   TO   MCP-WIDGET
                WHEN    04
                    MOVE  "HKNKOHSBTKBN"   TO   MCP-WIDGET
                WHEN    05
                    MOVE  "KENSNUMCHKKBN"  TO   MCP-WIDGET
                WHEN    06
                    MOVE  "JKYSKENSNUMCHKKBN"
                                           TO   MCP-WIDGET
                WHEN    07
                    MOVE  "HKNKOHKBN"      TO   MCP-WIDGET
                WHEN    08
                    MOVE  "GENHKNNUM1"      TO   MCP-WIDGET
                WHEN    09
                    MOVE  "GENHKNNUM2"      TO   MCP-WIDGET
                WHEN    10
                    MOVE  "GENHKNNUM3"      TO   MCP-WIDGET
                WHEN    11
                    MOVE  "JOKENSTYEAR"     TO   MCP-WIDGET
                WHEN    12
                    MOVE  "JOKENEDYEAR"     TO   MCP-WIDGET
                WHEN    13
                    MOVE  "TENTNK"          TO   MCP-WIDGET
                WHEN    14
                    MOVE  "RECESKYKBN"      TO   MCP-WIDGET
                WHEN    15
                    MOVE  "RECEFTNDSPKBN"   TO   MCP-WIDGET
                WHEN    16
                    MOVE  "HBTNUMCHKKBN"    TO   MCP-WIDGET
                WHEN    17
                    MOVE  "RECEFTNJGNDSPKBN" TO  MCP-WIDGET
      * 
                WHEN    20
                    MOVE  "GAIFTNKBN1"      TO   MCP-WIDGET
                WHEN    21
                    MOVE  "GAIKFTNRATE1"    TO   MCP-WIDGET
                WHEN    22
                    MOVE  "GAIKKOTGAK1"     TO   MCP-WIDGET
                WHEN    23
                    MOVE  "GAIKJGNGAK1"     TO   MCP-WIDGET
                WHEN    24
                    MOVE  "GAIDJGNGAK1"     TO   MCP-WIDGET
                WHEN    25
                    MOVE  "GAIDJGNCNT1"     TO   MCP-WIDGET
                WHEN    26
                    MOVE  "GAIMNAIJGNGAK1"  TO   MCP-WIDGET
                WHEN    27
                    MOVE  "GAIMGAIJGNGAK1"  TO   MCP-WIDGET
                WHEN    28
                    MOVE  "GAIMJGNCNT1"     TO   MCP-WIDGET
                WHEN    29
                    MOVE  "GAIYKZFTNKBN1"   TO   MCP-WIDGET
                WHEN    30
                    MOVE  "NYUFTNKBN1"      TO   MCP-WIDGET
                WHEN    31
                    MOVE  "NYUKFTNRATE1"    TO   MCP-WIDGET
                WHEN    32
                    MOVE  "NYUKKOTGAK1"     TO   MCP-WIDGET
                WHEN    33
                    MOVE  "NYUKJGNGAK1"     TO   MCP-WIDGET
                WHEN    34
                    MOVE  "NYUDJGNGAK1"     TO   MCP-WIDGET
                WHEN    35
                    MOVE  "NYUDJGNCNT1"     TO   MCP-WIDGET
                WHEN    36
                    MOVE  "NYUMJGNGAK1"     TO   MCP-WIDGET
                WHEN    37
                    MOVE  "NYUMJGNCNT1"     TO   MCP-WIDGET
                WHEN    38
                    MOVE  "NYUSKJJYOGAK1"   TO   MCP-WIDGET
                WHEN    39
                    MOVE  "NYUSKJRYYKBN1"   TO   MCP-WIDGET
      * 
                WHEN    50
                    MOVE  "GAIFTNKBN2"      TO   MCP-WIDGET
                WHEN    51
                    MOVE  "GAIKFTNRATE2"    TO   MCP-WIDGET
                WHEN    52
                    MOVE  "GAIKKOTGAK2"     TO   MCP-WIDGET
                WHEN    53
                    MOVE  "GAIKJGNGAK2"     TO   MCP-WIDGET
                WHEN    54
                    MOVE  "GAIDJGNGAK2"     TO   MCP-WIDGET
                WHEN    55
                    MOVE  "GAIDJGNCNT2"     TO   MCP-WIDGET
                WHEN    56
                    MOVE  "GAIMNAIJGNGAK2"  TO   MCP-WIDGET
                WHEN    57
                    MOVE  "GAIMGAIJGNGAK2"  TO   MCP-WIDGET
                WHEN    58
                    MOVE  "GAIMJGNCNT2"     TO   MCP-WIDGET
                WHEN    59
                    MOVE  "GAIYKZFTNKBN2"   TO   MCP-WIDGET
                WHEN    60
                    MOVE  "NYUFTNKBN2"      TO   MCP-WIDGET
                WHEN    61
                    MOVE  "NYUKFTNRATE2"    TO   MCP-WIDGET
                WHEN    62
                    MOVE  "NYUKKOTGAK2"     TO   MCP-WIDGET
                WHEN    63
                    MOVE  "NYUKJGNGAK2"     TO   MCP-WIDGET
                WHEN    64
                    MOVE  "NYUDJGNGAK2"     TO   MCP-WIDGET
                WHEN    65
                    MOVE  "NYUDJGNCNT2"     TO   MCP-WIDGET
                WHEN    66
                    MOVE  "NYUMJGNGAK2"     TO   MCP-WIDGET
                WHEN    67
                    MOVE  "NYUMJGNCNT2"     TO   MCP-WIDGET
                WHEN    68
                    MOVE  "NYUSKJJYOGAK2"   TO   MCP-WIDGET
                WHEN    69
                    MOVE  "NYUSKJRYYKBN2"   TO   MCP-WIDGET
      * 
                WHEN    80
                    MOVE  "GAIFTNKBN3"      TO   MCP-WIDGET
                WHEN    81
                    MOVE  "GAIKFTNRATE3"    TO   MCP-WIDGET
                WHEN    82
                    MOVE  "GAIKKOTGAK3"     TO   MCP-WIDGET
                WHEN    83
                    MOVE  "GAIKJGNGAK3"     TO   MCP-WIDGET
                WHEN    84
                    MOVE  "GAIDJGNGAK3"     TO   MCP-WIDGET
                WHEN    85
                    MOVE  "GAIDJGNCNT3"     TO   MCP-WIDGET
                WHEN    86
                    MOVE  "GAIMNAIJGNGAK3"  TO   MCP-WIDGET
                WHEN    87
                    MOVE  "GAIMGAIJGNGAK3"  TO   MCP-WIDGET
                WHEN    88
                    MOVE  "GAIMJGNCNT3"     TO   MCP-WIDGET
                WHEN    89
                    MOVE  "GAIYKZFTNKBN3"   TO   MCP-WIDGET
                WHEN    90
                    MOVE  "NYUFTNKBN3"      TO   MCP-WIDGET
                WHEN    91
                    MOVE  "NYUKFTNRATE3"    TO   MCP-WIDGET
                WHEN    92
                    MOVE  "NYUKKOTGAK3"     TO   MCP-WIDGET
                WHEN    93
                    MOVE  "NYUKJGNGAK3"     TO   MCP-WIDGET
                WHEN    94
                    MOVE  "NYUDJGNGAK3"     TO   MCP-WIDGET
                WHEN    95
                    MOVE  "NYUDJGNCNT3"     TO   MCP-WIDGET
                WHEN    96
                    MOVE  "NYUMJGNGAK3"     TO   MCP-WIDGET
                WHEN    97
                    MOVE  "NYUMJGNCNT3"     TO   MCP-WIDGET
                WHEN    98
                    MOVE  "NYUSKJJYOGAK3"   TO   MCP-WIDGET
                WHEN    99
                    MOVE  "NYUSKJRYYKBN3"   TO   MCP-WIDGET
      * 
                WHEN    110
                    MOVE  "GAIFTNKBN4"      TO   MCP-WIDGET
                WHEN    111
                    MOVE  "GAIKFTNRATE4"    TO   MCP-WIDGET
                WHEN    112
                    MOVE  "GAIKKOTGAK4"     TO   MCP-WIDGET
                WHEN    113
                    MOVE  "GAIKJGNGAK4"     TO   MCP-WIDGET
                WHEN    114
                    MOVE  "GAIDJGNGAK4"     TO   MCP-WIDGET
                WHEN    115
                    MOVE  "GAIDJGNCNT4"     TO   MCP-WIDGET
                WHEN    116
                    MOVE  "GAIMNAIJGNGAK4"  TO   MCP-WIDGET
                WHEN    117
                    MOVE  "GAIMGAIJGNGAK4"  TO   MCP-WIDGET
                WHEN    118
                    MOVE  "GAIMJGNCNT4"     TO   MCP-WIDGET
                WHEN    119
                    MOVE  "GAIYKZFTNKBN4"   TO   MCP-WIDGET
                WHEN    120
                    MOVE  "NYUFTNKBN4"      TO   MCP-WIDGET
                WHEN    121
                    MOVE  "NYUKFTNRATE4"    TO   MCP-WIDGET
                WHEN    122
                    MOVE  "NYUKKOTGAK4"     TO   MCP-WIDGET
                WHEN    123
                    MOVE  "NYUKJGNGAK4"     TO   MCP-WIDGET
                WHEN    124
                    MOVE  "NYUDJGNGAK4"     TO   MCP-WIDGET
                WHEN    125
                    MOVE  "NYUDJGNCNT4"     TO   MCP-WIDGET
                WHEN    126
                    MOVE  "NYUMJGNGAK4"     TO   MCP-WIDGET
                WHEN    127
                    MOVE  "NYUMJGNCNT4"     TO   MCP-WIDGET
                WHEN    128
                    MOVE  "NYUSKJJYOGAK4"   TO   MCP-WIDGET
                WHEN    129
                    MOVE  "NYUSKJRYYKBN4"   TO   MCP-WIDGET
            END-EVALUATE
      *
      **      EVALUATE    SPA-GMN-CUR
      **          WHEN    20  THRU    49
      **              MOVE    0                TO  V02-PAGENO
      **          WHEN    50  THRU    79
      **              MOVE    1                TO  V02-PAGENO
      **          WHEN    80  THRU    109
      **              MOVE    2                TO  V02-PAGENO
      **          WHEN    110 THRU    139
      **              MOVE    3                TO  V02-PAGENO
      **      END-EVALUATE      
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力カーソル位置編集処理
      *****************************************************************
       50011-GMN-CUR-SET-SEC      SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "HBTNUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SEIDONAME"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "TANSEIDONAME"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "HKNKOHSBTKBN"
                   MOVE    16              TO  SPA-GMN-CUR
               WHEN    "HBTNUMCHKKBN"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "KENSNUMCHKKBN"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "JKYSKENSNUMCHKKBN"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "HKNKOHKBN"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "GENHKNNUM1"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "GENHKNNUM2"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "GENHKNNUM3"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "JOKENSTYEAR"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "JOKENEDYEAR"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "TENTNK"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN    "RECESKYKBN"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "RECEFTNDSPKBN"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN    "RECEFTNJGNDSPKBN"
                   MOVE    20              TO  SPA-GMN-CUR
      *
      *
               WHEN    "GAIFTNKBN1"
                   MOVE    21              TO  SPA-GMN-CUR
               WHEN    "GAIKFTNRATE1"
                   MOVE    22              TO  SPA-GMN-CUR
               WHEN    "GAIKKOTGAK1"
                   MOVE    23              TO  SPA-GMN-CUR
               WHEN    "GAIKJGNGAK1"
                   MOVE    24              TO  SPA-GMN-CUR
               WHEN    "GAIDJGNGAK1"
                   MOVE    25              TO  SPA-GMN-CUR
               WHEN    "GAIDJGNCNT1"
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    "GAIMNAIJGNGAK1"
                   MOVE    27              TO  SPA-GMN-CUR
               WHEN    "GAIMGAIJGNGAK1"
                   MOVE    28              TO  SPA-GMN-CUR
               WHEN    "GAIMJGNCNT1"
                   MOVE    29              TO  SPA-GMN-CUR
               WHEN    "GAIYKZFTNKBN1"
                   MOVE    30              TO  SPA-GMN-CUR
      *
               WHEN    "NYUFTNKBN1"
                   MOVE    31              TO  SPA-GMN-CUR
               WHEN    "NYUKFTNRATE1"
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    "NYUKKOTGAK1"
                   MOVE    33              TO  SPA-GMN-CUR
               WHEN    "NYUKJGNGAK1"
                   MOVE    34              TO  SPA-GMN-CUR
               WHEN    "NYUDJGNGAK1"
                   MOVE    35              TO  SPA-GMN-CUR
               WHEN    "NYUDJGNCNT1"
                   MOVE    36              TO  SPA-GMN-CUR
               WHEN    "NYUMJGNGAK1"
                   MOVE    37              TO  SPA-GMN-CUR
               WHEN    "NYUMJGNCNT1"
                   MOVE    38              TO  SPA-GMN-CUR
               WHEN    "NYUSKJJYOGAK1"
                   MOVE    39              TO  SPA-GMN-CUR
               WHEN    "NYUSKJRYYKBN1"
                   MOVE    20              TO  SPA-GMN-CUR
      *
      *
               WHEN    "GAIFTNKBN2"
                   MOVE    51              TO  SPA-GMN-CUR
               WHEN    "GAIKFTNRATE2"
                   MOVE    52              TO  SPA-GMN-CUR
               WHEN    "GAIKKOTGAK2"
                   MOVE    53              TO  SPA-GMN-CUR
               WHEN    "GAIKJGNGAK2"
                   MOVE    54              TO  SPA-GMN-CUR
               WHEN    "GAIDJGNGAK2"
                   MOVE    55              TO  SPA-GMN-CUR
               WHEN    "GAIDJGNCNT2"
                   MOVE    56              TO  SPA-GMN-CUR
               WHEN    "GAIMNAIJGNGAK2"
                   MOVE    57              TO  SPA-GMN-CUR
               WHEN    "GAIMGAIJGNGAK2"
                   MOVE    58              TO  SPA-GMN-CUR
               WHEN    "GAIMJGNCNT2"
                   MOVE    59              TO  SPA-GMN-CUR
               WHEN    "GAIYKZFTNKBN2"
                   MOVE    60              TO  SPA-GMN-CUR
      *
               WHEN    "NYUFTNKBN2"
                   MOVE    61              TO  SPA-GMN-CUR
               WHEN    "NYUKFTNRATE2"
                   MOVE    62              TO  SPA-GMN-CUR
               WHEN    "NYUKKOTGAK2"
                   MOVE    63              TO  SPA-GMN-CUR
               WHEN    "NYUKJGNGAK2"
                   MOVE    64              TO  SPA-GMN-CUR
               WHEN    "NYUDJGNGAK2"
                   MOVE    65              TO  SPA-GMN-CUR
               WHEN    "NYUDJGNCNT2"
                   MOVE    66              TO  SPA-GMN-CUR
               WHEN    "NYUMJGNGAK2"
                   MOVE    67              TO  SPA-GMN-CUR
               WHEN    "NYUMJGNCNT2"
                   MOVE    68              TO  SPA-GMN-CUR
               WHEN    "NYUSKJJYOGAK2"
                   MOVE    69              TO  SPA-GMN-CUR
               WHEN    "NYUSKJRYYKBN2"
                   MOVE    50              TO  SPA-GMN-CUR
      *
      *
      *
               WHEN    "GAIFTNKBN3"
                   MOVE    81              TO  SPA-GMN-CUR
               WHEN    "GAIKFTNRATE3"
                   MOVE    82              TO  SPA-GMN-CUR
               WHEN    "GAIKKOTGAK3"
                   MOVE    83              TO  SPA-GMN-CUR
               WHEN    "GAIKJGNGAK3"
                   MOVE    84              TO  SPA-GMN-CUR
               WHEN    "GAIDJGNGAK3"
                   MOVE    85              TO  SPA-GMN-CUR
               WHEN    "GAIDJGNCNT3"
                   MOVE    86              TO  SPA-GMN-CUR
               WHEN    "GAIMNAIJGNGAK3"
                   MOVE    87              TO  SPA-GMN-CUR
               WHEN    "GAIMGAIJGNGAK3"
                   MOVE    88              TO  SPA-GMN-CUR
               WHEN    "GAIMJGNCNT3"
                   MOVE    89              TO  SPA-GMN-CUR
               WHEN    "GAIYKZFTNKBN3"
                   MOVE    90              TO  SPA-GMN-CUR
      *
               WHEN    "NYUFTNKBN3"
                   MOVE    91              TO  SPA-GMN-CUR
               WHEN    "NYUKFTNRATE3"
                   MOVE    92              TO  SPA-GMN-CUR
               WHEN    "NYUKKOTGAK3"
                   MOVE    93              TO  SPA-GMN-CUR
               WHEN    "NYUKJGNGAK3"
                   MOVE    94              TO  SPA-GMN-CUR
               WHEN    "NYUDJGNGAK3"
                   MOVE    95              TO  SPA-GMN-CUR
               WHEN    "NYUDJGNCNT3"
                   MOVE    96              TO  SPA-GMN-CUR
               WHEN    "NYUMJGNGAK3"
                   MOVE    97              TO  SPA-GMN-CUR
               WHEN    "NYUMJGNCNT3"
                   MOVE    98              TO  SPA-GMN-CUR
               WHEN    "NYUSKJJYOGAK3"
                   MOVE    99              TO  SPA-GMN-CUR
               WHEN    "NYUSKJRYYKBN3"
                   MOVE    80              TO  SPA-GMN-CUR
      *
      *
               WHEN    "GAIFTNKBN4"
                   MOVE    111             TO  SPA-GMN-CUR
               WHEN    "GAIKFTNRATE4"
                   MOVE    112             TO  SPA-GMN-CUR
               WHEN    "GAIKKOTGAK4"
                   MOVE    113             TO  SPA-GMN-CUR
               WHEN    "GAIKJGNGAK4"
                   MOVE    114             TO  SPA-GMN-CUR
               WHEN    "GAIDJGNGAK4"
                   MOVE    115             TO  SPA-GMN-CUR
               WHEN    "GAIDJGNCNT4"
                   MOVE    116             TO  SPA-GMN-CUR
               WHEN    "GAIMNAIJGNGAK4"
                   MOVE    117             TO  SPA-GMN-CUR
               WHEN    "GAIMGAIJGNGAK4"
                   MOVE    118             TO  SPA-GMN-CUR
               WHEN    "GAIMJGNCNT4"
                   MOVE    119             TO  SPA-GMN-CUR
               WHEN    "GAIYKZFTNKBN4"
                   MOVE    120             TO  SPA-GMN-CUR
      *
               WHEN    "NYUFTNKBN4"
                   MOVE    121             TO  SPA-GMN-CUR
               WHEN    "NYUKFTNRATE4"
                   MOVE    122             TO  SPA-GMN-CUR
               WHEN    "NYUKKOTGAK4"
                   MOVE    123             TO  SPA-GMN-CUR
               WHEN    "NYUKJGNGAK4"
                   MOVE    124             TO  SPA-GMN-CUR
               WHEN    "NYUDJGNGAK4"
                   MOVE    125             TO  SPA-GMN-CUR
               WHEN    "NYUDJGNCNT4"
                   MOVE    126             TO  SPA-GMN-CUR
               WHEN    "NYUMJGNGAK4"
                   MOVE    127             TO  SPA-GMN-CUR
               WHEN    "NYUMJGNCNT4"
                   MOVE    128             TO  SPA-GMN-CUR
               WHEN    "NYUSKJJYOGAK4"
                   MOVE    129             TO  SPA-GMN-CUR
               WHEN    "NYUSKJRYYKBN4"
                   MOVE    110             TO  SPA-GMN-CUR
      *
           END-EVALUATE
      *
           .
       50011-GMN-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " "
                                        BY          "0"         
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNNUM
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCDBMAIN"      USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
