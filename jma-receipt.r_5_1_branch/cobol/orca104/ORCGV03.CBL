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
       PROGRAM-ID.             ORCGV03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 公費付加情報入力
      *  コンポーネント名  : 公費付加情報入力（Ｖ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/27    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−太田   04/02/19  登録上限数を500に変更
      *  04.02.00    NACL−小豆沢 08/03/19  変更処理のグループ診療対応漏れ
      *  04.02.00    NACL−小豆沢 08/04/18  レセプト請求コンボの内容に
      *                                     広域連合を追加
      *  04.06.00    NACL-多々納  11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "V01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-V03.
           03  SPA-V03-AREA.
               05  SPA-GMN-CUR                         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SELNUM              PIC 9(3).
                   07  SPA-GMN-INPUT-AREA.
                       09  SPA-GMN-PREFNUM-G.
                           11  SPA-GMN-PREFNUM         PIC X(2).
                           11  SPA-GMN-PREFNUMFL       PIC X(1).
                           11  SPA-GMN-PREFNUMMEI      PIC X(20).
                       09  SPA-GMN-CITYNUM             PIC X(3).
                       09  SPA-GMN-KOHNUM              PIC X(3).
                       09  SPA-GMN-SEIDONAME           PIC X(100).
                       09  SPA-GMN-PTKBN-G.
                           11  SPA-GMN-PTKBN           PIC X(1).
                           11  SPA-GMN-PTKBNFL         PIC X(1).
                           11  SPA-GMN-PTKBNMEI        PIC X(18).
                       09  SPA-GMN-FTNKBN-G.
                           11  SPA-GMN-FTNKBN          PIC X(1).
                           11  SPA-GMN-FTNKBNFL        PIC X(1).
                           11  SPA-GMN-FTNKBNMEI       PIC X(38).
                       09  SPA-GMN-RECESKYKBN-G.
                           11  SPA-GMN-RECESKYKBN      PIC X(1).
                           11  SPA-GMN-RECESKYKBNFL    PIC X(1).
                           11  SPA-GMN-RECESKYKBNMEI   PIC X(38).
                       09  SPA-GMN-NYUSKJRYYKBN-G.
                           11  SPA-GMN-NYUSKJRYYKBN    PIC X(1).
                           11  SPA-GMN-NYUSKJRYYKBNFL  PIC X(1).
                           11  SPA-GMN-NYUSKJRYYKBNMEI PIC X(38).
                       09  SPA-GMN-YUKOSTYMD           PIC X(10).
                       09  SPA-GMN-YUKOEDYMD           PIC X(10).
                   07  SPA-GMN-PREFNUMLST-G.
                       09  SPA-GMN-PREFNUM-MAX         PIC 9(03).
                       09  SPA-GMN-PREFNUMLST          OCCURS  50.
                           11  SPA-GMN-PREFNUML        PIC X(02).
                           11  SPA-GMN-PREFNUMFLL      PIC X(01).
                           11  SPA-GMN-PREFNUMMEIL     PIC X(20).
                   07  SPA-GMN-PTKBNLST-G.
                       09  SPA-GMN-PTKBN-MAX           PIC 9(03).
                       09  SPA-GMN-PTKBNLST            OCCURS  5.
                           11  SPA-GMN-PTKBNL          PIC X(01).
                           11  SPA-GMN-PTKBNFLL        PIC X(01).
                           11  SPA-GMN-PTKBNMEIL       PIC X(18).
                   07  SPA-GMN-FTNKBNLST-G.
                       09  SPA-GMN-FTNKBN-MAX          PIC 9(03).
                       09  SPA-GMN-FTNKBNLST           OCCURS  5.
                           11  SPA-GMN-FTNKBNL         PIC X(01).
                           11  SPA-GMN-FTNKBNFLL       PIC X(01).
                           11  SPA-GMN-FTNKBNMEIL      PIC X(38).
                   07  SPA-GMN-RECESKYKBNLST-G.
                       09  SPA-GMN-RECESKYKBN-MAX      PIC 9(03).
                       09  SPA-GMN-RECESKYKBNLST       OCCURS  10.
                           11  SPA-GMN-RECESKYKBNL     PIC X(01).
                           11  SPA-GMN-RECESKYKBNFLL   PIC X(01).
                           11  SPA-GMN-RECESKYKBNMEIL  PIC X(38).
                   07  SPA-GMN-NYUSKJRYYKBNLST-G.
                       09  SPA-GMN-NYUSKJRYYKBN-MAX    PIC 9(03).
                       09  SPA-GMN-NYUSKJRYYKBNLST     OCCURS  10.
                           11  SPA-GMN-NYUSKJRYYKBNL   PIC X(01).
                           11  SPA-GMN-NYUSKJRYYKBNFLL PIC X(01).
                           11  SPA-GMN-NYUSKJRYYKBNMEIL
                                                       PIC X(38).
                   07  SPA-GMN-KOHPLUSLST.
                       09  SPA-GMN-KOHPLUSLST-MAX      PIC 9(03).
                       09  SPA-GMN-KOHPLUSLST-OCC      OCCURS 500.
                           11  SPA-GMN-TSELNUM         PIC 9(3).
                           11  SPA-GMN-TPREFNUM        PIC X(10).
                           11  SPA-GMN-TCITYNUM        PIC X(3).
                           11  SPA-GMN-TKOHNUM         PIC X(3).
                           11  SPA-GMN-TSEIDONAME      PIC X(100).
                           11  SPA-GMN-THKNKBN         PIC X(4).
                           11  SPA-GMN-TPTKBN          PIC X(6).
                           11  SPA-GMN-TFTNKBN         PIC X(4).
                           11  SPA-GMN-TYUKOSTYMD      PIC X(9).
                           11  SPA-GMN-TYUKOEDYMD      PIC X(9).
                       09  SPA-GMN-KOHPLUSLST-SEL      PIC 9(03).
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-INPUT-AREA.
                       09  SPA-NAI-YUKOSTYMD           PIC X(08).
                       09  SPA-NAI-YUKOEDYMD           PIC X(08).
                       09  SPA-NAI-PREFNUM-MOT         PIC X(02).
                       09  SPA-NAI-CITYNUM-MOT         PIC X(03).
                       09  SPA-NAI-KOHNUM-MOT          PIC X(03).
                       09  SPA-NAI-PTKBN-MOT           PIC X(01).
                       09  SPA-NAI-YUKOSTYMD-MOT       PIC X(08).
                       09  SPA-NAI-YUKOEDYMD-MOT       PIC X(08).
                   07  SPA-NAI-KOHPLUSLST.
                       09  SPA-NAI-KOHPLUSLST-MAX      PIC 9(03).
                       09  SPA-NAI-KOHPLUSLST-OCC      OCCURS 500.
                           11  SPA-NAI-TYUKOFLG        PIC 9(1).
                           11  SPA-NAI-TPREFNUM        PIC X(02).
                           11  SPA-NAI-THKNKBN         PIC X(1).
                           11  SPA-NAI-TPTKBN          PIC X(1).
                           11  SPA-NAI-TFTNKBN         PIC X(1).
                           11  SPA-NAI-TYUKOSTYMD      PIC X(8).
                           11  SPA-NAI-TYUKOEDYMD      PIC X(8).
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-KOHPLUS                 PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-HKNNUM                  PIC 9(01).
           03  FLG-KIKAN-DABURI            PIC 9(01).
           03  FLG-KOHPLUSLST-ROW          PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-LST                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH-TABLE              PIC X(64).
           03  WRK-PATH-PATHNAME           PIC X(64).
           03  WRK-VIDMSG                  PIC X(70). 
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
           03  WRK-YUKOSTYMD               PIC X(08).
           03  WRK-YUKOEDYMD               PIC X(08).
           03  WRK-UPDKBN                  PIC X(03).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-STR                     PIC X(500).
           03  WRK-LEN                     PIC 9(05).
      *
      *
       01  WRK-ROW.
           05  WRK-KOHPLUSLST-ROW          PIC S9(9)   BINARY.
      *
      *    固定値領域
       01  CONST-REC.
           03  CONST-KOHPLUSLST-MAX        PIC 9(03)   VALUE   500.
      *    公費付加情報ワーク
       01  WKKOHPLUS-REC.
           COPY  "CPKOHPLUS.INC"   REPLACING  //KOHPLUS//
                                           BY //WKKOHPLUS//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    公費付加情報
       01  KOHPLUS-REC.
           COPY  "CPKOHPLUS.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY  "CPHKNNUM.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
       PROCEDURE                       DIVISION    USING
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
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-V01KYOUTU
           MOVE    SPA-FREE            TO  SPA-V03
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
           WHEN    "LINK"              ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"              ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-V03         TO  SPA-FREE
           MOVE    SPA-V01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "VERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "V03"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "VID1"
               PERFORM 330-VID1-SET-SEC
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
      *    確認画面（ＶＩＤ１）ＯＫ処理
      *****************************************************************
       330-VID1-SET-SEC                SECTION.
      *
           IF    ( SPA-VID1-FLG        =   "OK" )
               IF    ( SPA-VIDCD       =   "2001" )
                   PERFORM 491-SAKUJO-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-VID1-FLG
           MOVE    SPACE               TO  SPA-VIDCD
           .
       330-VID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                      SPA-V03-AREA
      *
      *    コンボ項目設定処理
           PERFORM 3101-COMBO-SET-SEC
      *
      *    入力領域初期化
           PERFORM 420-CLEAR-SEC
      *
      *    公費付加情報リスト編集処理
           PERFORM 3102-KOHPLUSLST-HEN-SEC
           MOVE    1                   TO  FLG-KOHPLUSLST-ROW
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボ項目設定処理
      *****************************************************************
       3101-COMBO-SET-SEC              SECTION.
      *
      *    都道府県番号コンボ項目設定
           PERFORM 3101-PREFNUMLST-SET-SEC
      *
      *    患者区分コンボ項目設定
           PERFORM 3101-PTKBNLST-SET-SEC
      *
      *    患者負担区分コンボ項目設定
           PERFORM 3101-FTNKBNLST-SET-SEC
      *
      *    レセプト請求区分コンボ項目設定
           PERFORM 3101-RECESKYKBNLST-SET-SEC
      *
      *    食事療養費コンボ項目設定
           PERFORM 3101-NYUSKJRYYKBNLST-SET-SEC
      *
           . 
       3101-CMB-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    都道府県番号コンボ項目設定処理
      *****************************************************************
       3101-PREFNUMLST-SET-SEC         SECTION.
      *
           INITIALIZE                      SPA-GMN-PREFNUMLST-G
           MOVE    47                  TO  SPA-GMN-PREFNUM-MAX
      *
           MOVE    "01"                TO  SPA-GMN-PREFNUML   (1) 
           MOVE    "北海道"            TO  SPA-GMN-PREFNUMMEIL(1)
           MOVE    "02"                TO  SPA-GMN-PREFNUML   (2)
           MOVE    "青森"              TO  SPA-GMN-PREFNUMMEIL(2)
           MOVE    "03"                TO  SPA-GMN-PREFNUML   (3)
           MOVE    "岩手"              TO  SPA-GMN-PREFNUMMEIL(3) 
           MOVE    "04"                TO  SPA-GMN-PREFNUML   (4)
           MOVE    "宮城"              TO  SPA-GMN-PREFNUMMEIL(4)
           MOVE    "05"                TO  SPA-GMN-PREFNUML   (5)
           MOVE    "秋田"              TO  SPA-GMN-PREFNUMMEIL(5)
           MOVE    "06"                TO  SPA-GMN-PREFNUML   (6)
           MOVE    "山形"              TO  SPA-GMN-PREFNUMMEIL(6)
           MOVE    "07"                TO  SPA-GMN-PREFNUML   (7)
           MOVE    "福島"              TO  SPA-GMN-PREFNUMMEIL(7)
           MOVE    "08"                TO  SPA-GMN-PREFNUML   (8)
           MOVE    "茨城"              TO  SPA-GMN-PREFNUMMEIL(8)
           MOVE    "09"                TO  SPA-GMN-PREFNUML   (9)
           MOVE    "栃木"              TO  SPA-GMN-PREFNUMMEIL(9)
           MOVE    "10"                TO  SPA-GMN-PREFNUML   (10)
           MOVE    "群馬"              TO  SPA-GMN-PREFNUMMEIL(10)
           MOVE    "11"                TO  SPA-GMN-PREFNUML   (11)
           MOVE    "埼玉"              TO  SPA-GMN-PREFNUMMEIL(11)
           MOVE    "12"                TO  SPA-GMN-PREFNUML   (12)
           MOVE    "千葉"              TO  SPA-GMN-PREFNUMMEIL(12)
           MOVE    "13"                TO  SPA-GMN-PREFNUML   (13)
           MOVE    "東京"              TO  SPA-GMN-PREFNUMMEIL(13)
           MOVE    "14"                TO  SPA-GMN-PREFNUML   (14)
           MOVE    "神奈川"            TO  SPA-GMN-PREFNUMMEIL(14)
           MOVE    "15"                TO  SPA-GMN-PREFNUML   (15)
           MOVE    "新潟"              TO  SPA-GMN-PREFNUMMEIL(15)
           MOVE    "16"                TO  SPA-GMN-PREFNUML   (16)
           MOVE    "富山"              TO  SPA-GMN-PREFNUMMEIL(16)
           MOVE    "17"                TO  SPA-GMN-PREFNUML   (17)
           MOVE    "石川"              TO  SPA-GMN-PREFNUMMEIL(17)
           MOVE    "18"                TO  SPA-GMN-PREFNUML   (18)
           MOVE    "福井"              TO  SPA-GMN-PREFNUMMEIL(18)
           MOVE    "19"                TO  SPA-GMN-PREFNUML   (19)
           MOVE    "山梨"              TO  SPA-GMN-PREFNUMMEIL(19)
           MOVE    "20"                TO  SPA-GMN-PREFNUML   (20)
           MOVE    "長野"              TO  SPA-GMN-PREFNUMMEIL(20)
           MOVE    "21"                TO  SPA-GMN-PREFNUML   (21)
           MOVE    "岐阜"              TO  SPA-GMN-PREFNUMMEIL(21)
           MOVE    "22"                TO  SPA-GMN-PREFNUML   (22)
           MOVE    "静岡"              TO  SPA-GMN-PREFNUMMEIL(22)
           MOVE    "23"                TO  SPA-GMN-PREFNUML   (23)
           MOVE    "愛知"              TO  SPA-GMN-PREFNUMMEIL(23)
           MOVE    "24"                TO  SPA-GMN-PREFNUML   (24)
           MOVE    "三重"              TO  SPA-GMN-PREFNUMMEIL(24)
           MOVE    "25"                TO  SPA-GMN-PREFNUML   (25)
           MOVE    "滋賀"              TO  SPA-GMN-PREFNUMMEIL(25)
           MOVE    "26"                TO  SPA-GMN-PREFNUML   (26)
           MOVE    "京都"              TO  SPA-GMN-PREFNUMMEIL(26)
           MOVE    "27"                TO  SPA-GMN-PREFNUML   (27)
           MOVE    "大阪"              TO  SPA-GMN-PREFNUMMEIL(27)
           MOVE    "28"                TO  SPA-GMN-PREFNUML   (28)
           MOVE    "兵庫"              TO  SPA-GMN-PREFNUMMEIL(28)
           MOVE    "29"                TO  SPA-GMN-PREFNUML   (29)
           MOVE    "奈良"              TO  SPA-GMN-PREFNUMMEIL(29)
           MOVE    "30"                TO  SPA-GMN-PREFNUML   (30)
           MOVE    "和歌山"            TO  SPA-GMN-PREFNUMMEIL(30)
           MOVE    "31"                TO  SPA-GMN-PREFNUML   (31)
           MOVE    "鳥取"              TO  SPA-GMN-PREFNUMMEIL(31)
           MOVE    "32"                TO  SPA-GMN-PREFNUML   (32)
           MOVE    "島根"              TO  SPA-GMN-PREFNUMMEIL(32)
           MOVE    "33"                TO  SPA-GMN-PREFNUML   (33)
           MOVE    "岡山"              TO  SPA-GMN-PREFNUMMEIL(33)
           MOVE    "34"                TO  SPA-GMN-PREFNUML   (34)
           MOVE    "広島"              TO  SPA-GMN-PREFNUMMEIL(34)
           MOVE    "35"                TO  SPA-GMN-PREFNUML   (35)
           MOVE    "山口"              TO  SPA-GMN-PREFNUMMEIL(35)
           MOVE    "36"                TO  SPA-GMN-PREFNUML   (36)
           MOVE    "徳島"              TO  SPA-GMN-PREFNUMMEIL(36)
           MOVE    "37"                TO  SPA-GMN-PREFNUML   (37)
           MOVE    "香川"              TO  SPA-GMN-PREFNUMMEIL(37)
           MOVE    "38"                TO  SPA-GMN-PREFNUML   (38)
           MOVE    "愛媛"              TO  SPA-GMN-PREFNUMMEIL(38)
           MOVE    "39"                TO  SPA-GMN-PREFNUML   (39)
           MOVE    "高知"              TO  SPA-GMN-PREFNUMMEIL(39)
           MOVE    "40"                TO  SPA-GMN-PREFNUML   (40)
           MOVE    "福岡"              TO  SPA-GMN-PREFNUMMEIL(40)
           MOVE    "41"                TO  SPA-GMN-PREFNUML   (41)
           MOVE    "佐賀"              TO  SPA-GMN-PREFNUMMEIL(41)
           MOVE    "42"                TO  SPA-GMN-PREFNUML   (42)
           MOVE    "長崎"              TO  SPA-GMN-PREFNUMMEIL(42)
           MOVE    "43"                TO  SPA-GMN-PREFNUML   (43)
           MOVE    "熊本"              TO  SPA-GMN-PREFNUMMEIL(43)
           MOVE    "44"                TO  SPA-GMN-PREFNUML   (44)
           MOVE    "大分"              TO  SPA-GMN-PREFNUMMEIL(44)
           MOVE    "45"                TO  SPA-GMN-PREFNUML   (45)
           MOVE    "宮崎"              TO  SPA-GMN-PREFNUMMEIL(45)
           MOVE    "46"                TO  SPA-GMN-PREFNUML   (46)
           MOVE    "鹿児島"            TO  SPA-GMN-PREFNUMMEIL(46)
           MOVE    "47"                TO  SPA-GMN-PREFNUML   (47)
           MOVE    "沖縄"              TO  SPA-GMN-PREFNUMMEIL(47)
      *
           . 
       3101-PREFNUMLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者区分コンボ項目設定処理
      *****************************************************************
       3101-PTKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-PTKBNLST-G
           MOVE     3                  TO  SPA-GMN-PTKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-PTKBNL    (1)
           MOVE    "一般・老人"        TO  SPA-GMN-PTKBNMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-PTKBNL    (2)
           MOVE    "一般"              TO  SPA-GMN-PTKBNMEIL (2)
      *
           MOVE    "2"                 TO  SPA-GMN-PTKBNL    (3)
           MOVE    "老人"              TO  SPA-GMN-PTKBNMEIL (3)
      *
           . 
       3101-PTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者負担区分コンボ項目設定処理
      *****************************************************************
       3101-FTNKBNLST-SET-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-FTNKBNLST-G
           MOVE     2                  TO  SPA-GMN-FTNKBN-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-FTNKBNL    (1)
           MOVE    "患者負担あり（初診、往診料相当）"
                                       TO  SPA-GMN-FTNKBNMEIL (1)
      *
           MOVE    "2"                 TO  SPA-GMN-FTNKBNL    (2)
           MOVE    "患者負担なし"      TO  SPA-GMN-FTNKBNMEIL (2)
      *
           . 
       3101-FTNKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    レセプト請求区分コンボ項目設定処理
      *****************************************************************
       3101-RECESKYKBNLST-SET-SEC      SECTION.
      *
           INITIALIZE                      SPA-GMN-RECESKYKBNLST-G
           MOVE     4                  TO  SPA-GMN-RECESKYKBN-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-RECESKYKBNL    (1)
           MOVE    "社保、国保、広域の場合に印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-RECESKYKBNL    (2)
           MOVE    "社保との併用に限り印刷"
                                       TO  SPA-GMN-RECESKYKBNMEIL (2)
      *
           MOVE    "2"                 TO  SPA-GMN-RECESKYKBNL    (3)
           MOVE    "国保又は広域との併用に限り印刷" 
                                       TO  SPA-GMN-RECESKYKBNMEIL (3)
      *
           MOVE    "3"                 TO  SPA-GMN-RECESKYKBNL    (4)
           MOVE    "社保、国保、広域とも印刷不可"
                                       TO  SPA-GMN-RECESKYKBNMEIL (4)
      *
           . 
       3101-RECESKYKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費コンボ項目設定
      *****************************************************************
       3101-NYUSKJRYYKBNLST-SET-SEC    SECTION.
      *
           INITIALIZE                      SPA-GMN-NYUSKJRYYKBNLST-G
           MOVE     5                  TO  SPA-GMN-NYUSKJRYYKBN-MAX
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
      *
           . 
       3101-NYUSKJRYYKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報リスト編集処理
      *****************************************************************
       3102-KOHPLUSLST-HEN-SEC     SECTION.
      *
           INITIALIZE                      SPA-GMN-KOHPLUSLST
                                           SPA-NAI-KOHPLUSLST
      *
           MOVE    ZERO            TO  FLG-KOHPLUS
           INITIALIZE                  KOHPLUS-REC
           MOVE    SPA-HOSPNUM     TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "all"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-KOHPLUS
           ELSE
               MOVE    MCPDATA-REC     TO  KOHPLUS-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-KOHPLUS NOT =   ZERO )
                    OR   ( SPA-GMN-KOHPLUSLST-MAX
                                           >=  CONST-KOHPLUSLST-MAX  )
      *
               PERFORM 31021-KOHPLUSLST-HEN-SEC
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC        NOT =   ZERO )
                   MOVE    1               TO  FLG-KOHPLUS
               ELSE
                   MOVE    MCPDATA-REC     TO  KOHPLUS-REC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-TBL-CLOSE-SEC
      *
           . 
       3102-KOHPLUSLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報リスト編集処理
      *****************************************************************
       31021-KOHPLUSLST-HEN-SEC        SECTION.
      *
           COMPUTE SPA-GMN-KOHPLUSLST-MAX
               =   SPA-GMN-KOHPLUSLST-MAX  +   1
      *
           MOVE    SPA-GMN-KOHPLUSLST-MAX
                                       TO  IDX-LST
      *
           MOVE    IDX-LST             TO  SPA-GMN-TSELNUM     (IDX-LST)
      *
           IF    ( SPA-SYSYMD          >=  KOHPLUS-YUKOSTYMD )
            AND  ( SPA-SYSYMD          <=  KOHPLUS-YUKOEDYMD )
                MOVE    1              TO  SPA-NAI-TYUKOFLG    (IDX-LST)
           END-IF
      *
           MOVE    KOHPLUS-PREFNUM     TO  SPA-NAI-TPREFNUM    (IDX-LST)
      *
           MOVE    KOHPLUS-PREFNUM     TO  WRK-CMB-CD
           PERFORM 4200-PREFNUMLST-CHK-SEC
           MOVE    WRK-CMB-ITM (4:)    TO  SPA-GMN-TPREFNUM    (IDX-LST)
      *
           MOVE    KOHPLUS-CITYNUM     TO  SPA-GMN-TCITYNUM    (IDX-LST)
      *
           MOVE    KOHPLUS-KOHNUM      TO  SPA-GMN-TKOHNUM     (IDX-LST)
      *
           MOVE    KOHPLUS-SEIDONAME   TO  SPA-GMN-TSEIDONAME  (IDX-LST)
      *
           MOVE    KOHPLUS-PTKBN   TO  SPA-NAI-TPTKBN  (IDX-LST)
      *
           EVALUATE    KOHPLUS-PTKBN
           WHEN    "0"
               MOVE    "一・老"        TO  SPA-GMN-TPTKBN  (IDX-LST)
           WHEN    "1"
               MOVE    "一般"          TO  SPA-GMN-TPTKBN  (IDX-LST)
           WHEN    "2"
               MOVE    "老人"          TO  SPA-GMN-TPTKBN  (IDX-LST)
           END-EVALUATE
      *
           MOVE    KOHPLUS-FTNKBN      TO  SPA-NAI-TFTNKBN     (IDX-LST)
      *
           IF    ( KOHPLUS-FTNKBN      =   "1" )
               MOVE    "　〇"          TO  SPA-GMN-TFTNKBN     (IDX-LST)
           ELSE
               MOVE    "　−"          TO  SPA-GMN-TFTNKBN     (IDX-LST)
           END-IF
      *
           MOVE    KOHPLUS-YUKOSTYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TYUKOSTYMD  (IDX-LST)
           MOVE    WRK-SYMD            TO  SPA-NAI-TYUKOSTYMD  (IDX-LST)
      *
           MOVE    KOHPLUS-YUKOEDYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TYUKOEDYMD  (IDX-LST)
           MOVE    WRK-SYMD            TO  SPA-NAI-TYUKOEDYMD  (IDX-LST)
      *
           . 
       31021-KOHPLUSLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 470-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 480-HENKOU-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 490-SAKUJO-SEC
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
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4100-SELNUM-CHK-SEC
      *
      *    公費付加情報リスト
           WHEN    ANY             ALSO    "KOHPLUSLST"
                   PERFORM 4100-KOHPLUSLST-CHK-SEC
           WHEN    OTHER
      *
               MOVE    ZERO            TO  SPA-GMN-CUR
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
           MOVE    V03-PREFNUM         TO  SPA-GMN-PREFNUM-G
           MOVE    V03-CITYNUM         TO  SPA-GMN-CITYNUM
           MOVE    V03-KOHNUM          TO  SPA-GMN-KOHNUM
           MOVE    V03-SEIDONAME       TO  SPA-GMN-SEIDONAME
           MOVE    V03-PTKBN           TO  SPA-GMN-PTKBN-G
           MOVE    V03-FTNKBN          TO  SPA-GMN-FTNKBN-G
           MOVE    V03-RECESKYKBN      TO  SPA-GMN-RECESKYKBN-G
           MOVE    V03-NYUSKJRYYKBN    TO  SPA-GMN-NYUSKJRYYKBN-G
           MOVE    V03-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    V03-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    選択番号チェック
           PERFORM 41001-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    有効期間開始日チェック
           PERFORM 4110-YUKOSTYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    有効期間終了日チェック
           PERFORM 4111-YUKOEDYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    都道府県番号チェック
           PERFORM 4101-PREFNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    市町村番号チェック
           PERFORM 4102-CITYNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    公費番号チェック
           PERFORM 4103-KOHNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    制度名チェック
           PERFORM 4104-SEIDONAME-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    患者区分チェック
           PERFORM 4106-PTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    患者負担区分チェック
           PERFORM 4107-FTNKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    レセプト請求区分チェック処理
           PERFORM 4108-RECESKYKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    食事療養費区分チェック処理
           PERFORM 4109-NYUSKJRYYKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費付加情報リストチェック
      *****************************************************************
       4100-KOHPLUSLST-CHK-SEC         SECTION.
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-KOHPLUSLST-MAX )
      *
               IF    ( V03-KOHPLUSLST-SELECT (IDX-LST) =   "T" )
                   PERFORM 41001-INPUT-AREA-HEN-SEC
                   MOVE    IDX-LST     TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-KOHPLUSLST-MAX
                                       TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-KOHPLUSLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41001-INPUT-AREA-HEN-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
      *
           MOVE    ZERO                    TO  FLG-KOHPLUS
           INITIALIZE                          KOHPLUS-REC
      *****MOVE    SPA-HOSPID              TO  KOHPLUS-HOSPID
           MOVE    SPA-NAI-TPREFNUM    (IDX-LST)
                                           TO  KOHPLUS-PREFNUM
           MOVE    SPA-GMN-TCITYNUM    (IDX-LST)
                                           TO  KOHPLUS-CITYNUM
           MOVE    SPA-GMN-TKOHNUM     (IDX-LST)
                                           TO  KOHPLUS-KOHNUM
           MOVE    SPA-NAI-TPTKBN  (IDX-LST)
                                           TO  KOHPLUS-PTKBN
           MOVE    SPA-NAI-TYUKOSTYMD  (IDX-LST)
                                           TO  KOHPLUS-YUKOSTYMD
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC             TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE    1                   TO  FLG-KOHPLUS
           ELSE
               MOVE    MCPDATA-REC         TO  KOHPLUS-REC
           END-IF
      *
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-KOHPLUS         NOT =   ZERO )
               GO  TO  41001-INPUT-AREA-HEN-EXT
           END-IF
      *
      *    都道府県番号
           MOVE    KOHPLUS-PREFNUM         TO  WRK-CMB-CD
           PERFORM 4200-PREFNUMLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-PREFNUM-G
      *
      *    市町村番号
           MOVE    KOHPLUS-CITYNUM         TO  SPA-GMN-CITYNUM
      *
      *    公費番号
           MOVE    KOHPLUS-KOHNUM          TO  SPA-GMN-KOHNUM
      *
      *    制度名
           MOVE    KOHPLUS-SEIDONAME       TO  SPA-GMN-SEIDONAME
      *
      *    患者区分
           MOVE    KOHPLUS-PTKBN       TO  WRK-CMB-CD
           PERFORM 4200-PTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-PTKBN-G
      *
      *    患者負担区分
           MOVE    KOHPLUS-FTNKBN          TO  WRK-CMB-CD
           PERFORM 4200-FTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKBN-G
      *
      *    レセプト請求区分
           MOVE    KOHPLUS-RECESKYKBN      TO  WRK-CMB-CD
           PERFORM 4200-RECESKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-RECESKYKBN-G
      *
      *    食事療養費区分
           MOVE    KOHPLUS-NYUSKJRYYKBN    TO  WRK-CMB-CD
           PERFORM 4200-NYUSKJRYYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-NYUSKJRYYKBN-G
      *
      *
      *    有効開始日
           MOVE    KOHPLUS-YUKOSTYMD       TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  SPA-GMN-YUKOSTYMD
           MOVE    WRK-SYMD                TO  SPA-NAI-YUKOSTYMD
      *
      *    有効終了日日
           MOVE    KOHPLUS-YUKOEDYMD       TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  SPA-GMN-YUKOEDYMD
           MOVE    WRK-SYMD                TO  SPA-NAI-YUKOEDYMD
      *
           MOVE    KOHPLUS-PREFNUM         TO  SPA-NAI-PREFNUM-MOT
           MOVE    KOHPLUS-CITYNUM         TO  SPA-NAI-CITYNUM-MOT
           MOVE    KOHPLUS-KOHNUM          TO  SPA-NAI-KOHNUM-MOT
           MOVE    KOHPLUS-PTKBN           TO  SPA-NAI-PTKBN-MOT
           MOVE    KOHPLUS-YUKOSTYMD       TO  SPA-NAI-YUKOSTYMD-MOT
           MOVE    KOHPLUS-YUKOEDYMD       TO  SPA-NAI-YUKOEDYMD-MOT
      *
           MOVE    IDX-LST                 TO  SPA-GMN-KOHPLUSLST-SEL
      *
           .
      *
       41001-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    V03-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               MOVE    ZERO            TO  SPA-GMN-KOHPLUSLST-SEL
               PERFORM 420-CLEAR-SEC
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           PERFORM 41001-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX-LST
           PERFORM 41001-INPUT-AREA-HEN-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNUM      >   SPA-GMN-KOHPLUSLST-MAX )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  41001-SELNUM-CHK-EXT
           END-IF
      *
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県番号チェック処理
      *****************************************************************
       4101-PREFNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-PREFNUM-G   =   SPACE )
               GO  TO  4101-PREFNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PREFNUM     TO  WRK-CMB-CD
           PERFORM 4200-PREFNUMLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4101-PREFNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PREFNUM-G
      *
           .
      *
       4101-PREFNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    市町村番号チェック処理
      *****************************************************************
       4102-CITYNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-CITYNUM     =   SPACE )
               GO  TO  4102-CITYNUM-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  SPA-GMN-CITYNUM     DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH    POINTER WRK-LEN
           END-STRING
           COMPUTE WRK-LEN =   WRK-LEN -   1
           IF    ( WRK-LEN     NOT =   3 )
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4102-CITYNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-CITYNUM     TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   1 )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4102-CITYNUM-CHK-EXT
           END-IF
      *
           .
      *
       4102-CITYNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       4103-KOHNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-KOHNUM      =   SPACE )
               GO  TO  4103-KOHNUM-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE  HKNNUM-REC
      *****MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    SPA-GMN-KOHNUM      TO  HKN-HKNNUM
           MOVE    "00"                TO  HKN-PAYKBN
           MOVE    SPA-NAI-YUKOSTYMD   TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-HKNNUM      NOT =   ZERO )
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  4103-KOHNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-SEIDONAME   =   SPACE )
               MOVE    HKN-SEIDONAME   TO  SPA-GMN-SEIDONAME
           END-IF
      *
           .
      *
       4103-KOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    制度名チェック処理
      *****************************************************************
       4104-SEIDONAME-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-SEIDONAME   =   SPACE )
               GO  TO  4104-SEIDONAME-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SEIDONAME   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =  2  )
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
               GO  TO  4104-SEIDONAME-CHK-EXT
           END-IF
      *
           .
      *
       4104-SEIDONAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者区分チェック処理
      *****************************************************************
       4106-PTKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-PTKBN-G =   SPACE )
               GO  TO  4106-PTKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PTKBN   TO  WRK-CMB-CD
           PERFORM 4200-PTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
               GO  TO  4106-PTKBN-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTKBN-G
      *
           .
      *
       4106-PTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者負担区分チェック処理
      *****************************************************************
       4107-FTNKBN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-FTNKBN-G    =   SPACE )
               GO  TO  4107-FTNKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-FTNKBN      TO  WRK-CMB-CD
           PERFORM 4200-FTNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
               GO  TO  4107-FTNKBN-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKBN-G
      *
           .
      *
       4107-FTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプト請求区分チェック処理
      *****************************************************************
       4108-RECESKYKBN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-RECESKYKBN-G    =   SPACE )
               GO  TO  4108-RECESKYKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-RECESKYKBN  TO  WRK-CMB-CD
           PERFORM 4200-RECESKYKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
               GO  TO  4108-RECESKYKBN-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECESKYKBN-G
      *
           .
      *
       4108-RECESKYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費区分チェック処理
      *****************************************************************
       4109-NYUSKJRYYKBN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-NYUSKJRYYKBN-G  =   SPACE )
               GO  TO  4109-NYUSKJRYYKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-NYUSKJRYYKBN    TO  WRK-CMB-CD
           PERFORM 4200-NYUSKJRYYKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-CUR
               GO  TO  4109-NYUSKJRYYKBN-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUSKJRYYKBN-G
      *
           .
      *
       4109-NYUSKJRYYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日チェック処理
      *****************************************************************
       4110-YUKOSTYMD-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-YUKOSTYMD   =   SPACE )
               MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                           SPA-NAI-YUKOSTYMD
               GO  TO  4110-YUKOSTYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-YUKOSTYMD
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE    WRK-YUKOSTYMD   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOSTYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-YUKOSTYMD
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
               GO  TO  4110-YUKOSTYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG         TO  SPA-GMN-YUKOSTYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
      *
           .
      *
       4110-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日チェック処理
      *****************************************************************
       4111-YUKOEDYMD-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-YUKOEDYMD   =   SPACE )
               MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                           SPA-NAI-YUKOEDYMD
               GO  TO  4111-YUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-YUKOEDYMD
      *
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE    WRK-YUKOEDYMD   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOEDYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-YUKOEDYMD
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
               GO  TO  4111-YUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG         TO  SPA-GMN-YUKOEDYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
      *
           .
       4111-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    都道府県コンボチェック処理
      *****************************************************************
       4200-PREFNUMLST-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-PREFNUM-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-PREFNUML (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-PREFNUM-MAX )
               MOVE    SPA-GMN-PREFNUMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PREFNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者区分コンボチェック処理
      *****************************************************************
       4200-PTKBNLST-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-PTKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-PTKBNL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-PTKBN-MAX )
               MOVE    SPA-GMN-PTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者負担区分コンボチェック処理
      *****************************************************************
       4200-FTNKBNLST-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-FTNKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-FTNKBNL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-FTNKBN-MAX )
               MOVE    SPA-GMN-FTNKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNKBNLST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト請求区分コンボチェック処理
      *****************************************************************
       4200-RECESKYKBNLST-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-RECESKYKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-RECESKYKBNL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-RECESKYKBN-MAX )
               MOVE    SPA-GMN-RECESKYKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-RECESKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費コンボチェック処理
      *****************************************************************
       4200-NYUSKJRYYKBNLST-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-NYUSKJRYYKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-NYUSKJRYYKBNL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-NYUSKJRYYKBN-MAX )
               MOVE    SPA-GMN-NYUSKJRYYKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYUSKJRYYKBNLST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "V01"               TO  SPA-SAKIPG
           MOVE    "V03"               TO  SPA-MOTOPG
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
      *    初期化処理
      *****************************************************************
       420-CLEAR-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-SELNUM
                                           SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
      *
           MOVE    SPA-PREFNUM         TO  WRK-CMB-CD
           PERFORM 4200-PREFNUMLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PREFNUM-G
      *
           MOVE    SPA-GMN-PTKBNLST (1)
                                       TO  SPA-GMN-PTKBN-G
      *
           MOVE    SPA-GMN-FTNKBNLST (1)
                                       TO  SPA-GMN-FTNKBN-G
      *
           MOVE    SPA-GMN-RECESKYKBNLST (1)
                                       TO  SPA-GMN-RECESKYKBN-G
      *
           MOVE    SPA-GMN-NYUSKJRYYKBNLST (2)
                                       TO  SPA-GMN-NYUSKJRYYKBN-G
      *
           MOVE    "00000000"          TO  SPA-GMN-YUKOSTYMD
                                           SPA-NAI-YUKOSTYMD
      *
           MOVE    "99999999"          TO  SPA-GMN-YUKOEDYMD
                                           SPA-NAI-YUKOEDYMD
      *
           MOVE    ZERO                TO  SPA-GMN-KOHPLUSLST-SEL
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .    
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       470-TUIKA-SEC                   SECTION.
      *
      *    入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  470-TUIKA-EXT
           END-IF
      *
           MOVE    "INS"           TO  WRK-UPDKBN
      *
      *    関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  470-TUIKA-EXT
           END-IF
      *
      *    履歴チェック
           PERFORM 4901-RIREKI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  470-TUIKA-EXT
           END-IF
      *
           PERFORM 4701-KOHPLUS-HEN-SEC
      *
      *    更新直前チェック
           MOVE    KOHPLUS-REC             TO  WKKOHPLUS-REC
           MOVE    ZERO                    TO  FLG-KOHPLUS
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC             TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE    1                   TO  FLG-KOHPLUS
           END-IF
      *
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-KOHPLUS             =   ZERO )
               MOVE   "4001"               TO  SPA-ERRCD
               GO  TO 470-TUIKA-EXT
           END-IF
      *
      *    公費付加情報マスタ更新
           MOVE    WKKOHPLUS-REC           TO  KOHPLUS-REC
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC             TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-INSERT-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE   "4002"               TO  SPA-ERRCD
               GO  TO 470-TUIKA-EXT
           END-IF
      *
      *    公費付加情報リスト編集
           PERFORM 3102-KOHPLUSLST-HEN-SEC
           MOVE    2                       TO  FLG-KOHPLUSLST-ROW
      *
      *    入力項目を初期化する
           PERFORM 420-CLEAR-SEC
      *
      *    公費付加情報最大件数チェック
           IF    ( SPA-GMN-KOHPLUSLST-MAX  >=  CONST-KOHPLUSLST-MAX )
               MOVE    "1104"              TO  SPA-ERRCD
           END-IF
      *
           .    
       470-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報編集処理
      *****************************************************************
       4701-KOHPLUS-HEN-SEC                SECTION.
      *
           INITIALIZE                          KOHPLUS-REC
      *
      *    医療機関ＩＤ
      *****MOVE    SPA-HOSPID              TO  KOHPLUS-HOSPID
      *    県番号
           MOVE    SPA-GMN-PREFNUM         TO  KOHPLUS-PREFNUM
      *    市町村番号
           MOVE    SPA-GMN-CITYNUM         TO  KOHPLUS-CITYNUM
      *    公費番号
           MOVE    SPA-GMN-KOHNUM          TO  KOHPLUS-KOHNUM
      *    患者区分
           MOVE    SPA-GMN-PTKBN           TO  KOHPLUS-PTKBN
      *    有効開始年月日
           MOVE    SPA-NAI-YUKOSTYMD       TO  KOHPLUS-YUKOSTYMD
      *    有効終了年月日
           MOVE    SPA-NAI-YUKOEDYMD       TO  KOHPLUS-YUKOEDYMD
      *    負担区分
           MOVE    SPA-GMN-FTNKBN          TO  KOHPLUS-FTNKBN
      *    入院−食事療養区分
           MOVE    SPA-GMN-NYUSKJRYYKBN    TO  KOHPLUS-NYUSKJRYYKBN
      *    レセプト請求区分
           MOVE    SPA-GMN-RECESKYKBN      TO  KOHPLUS-RECESKYKBN
      *    制度名
           MOVE    SPA-GMN-SEIDONAME       TO  KOHPLUS-SEIDONAME
      *
           IF    ( WRK-UPDKBN              =   "UPD" )
      *
      *        更新時間
               ACCEPT  SYS-TIME        FROM    TIME
               MOVE    SYS-TIME            TO  KOHPLUS-UPHMS
      *        更新年月日
               MOVE    SPA-SYSYMD          TO  KOHPLUS-UPYMD
           ELSE
      *        作成年月日
               MOVE    SPA-SYSYMD          TO  KOHPLUS-CREYMD
           END-IF
      *
           .    
       4701-KOHPLUS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       480-HENKOU-SEC                  SECTION.
      *
      *    入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-HENKOU-EXT
           END-IF
      *
           MOVE    "UPD"                   TO  WRK-UPDKBN
      *
      *    関連チェック
           MOVE    "UPD"           TO  WRK-UPDKBN
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-HENKOU-EXT
           END-IF
      *
      *    履歴チェック
           PERFORM 4901-RIREKI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-HENKOU-EXT
           END-IF
      *
           PERFORM 4701-KOHPLUS-HEN-SEC
      *
      *****MOVE    SPA-HOSPID              TO  KOHPLUSUP-HOSPID
           MOVE    SPA-HOSPNUM             TO  KOHPLUSUP-HOSPNUM
           MOVE    SPA-NAI-PREFNUM-MOT     TO  KOHPLUSUP-PREFNUM
           MOVE    SPA-NAI-CITYNUM-MOT     TO  KOHPLUSUP-CITYNUM
           MOVE    SPA-NAI-KOHNUM-MOT      TO  KOHPLUSUP-KOHNUM
           MOVE    SPA-NAI-PTKBN-MOT       TO  KOHPLUSUP-PTKBN
           MOVE    SPA-NAI-YUKOSTYMD-MOT   TO  KOHPLUSUP-YUKOSTYMD
      *
      *    更新直前チェック
           MOVE    KOHPLUS-REC             TO  WKKOHPLUS-REC
           MOVE    ZERO                    TO  FLG-KOHPLUS
           INITIALIZE                          KOHPLUS-REC
      *****MOVE    SPA-HOSPID              TO  KOHPLUS-HOSPID
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    SPA-NAI-PREFNUM-MOT     TO  KOHPLUS-PREFNUM
           MOVE    SPA-NAI-CITYNUM-MOT     TO  KOHPLUS-CITYNUM
           MOVE    SPA-NAI-KOHNUM-MOT      TO  KOHPLUS-KOHNUM
           MOVE    SPA-NAI-PTKBN-MOT       TO  KOHPLUS-PTKBN
           MOVE    SPA-NAI-YUKOSTYMD-MOT   TO  KOHPLUS-YUKOSTYMD
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC             TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE    1                   TO  FLG-KOHPLUS
           ELSE
               MOVE    MCPDATA-REC         TO  KOHPLUS-REC
           END-IF
      *
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-KOHPLUS             =   1 )
               MOVE   "4003"               TO  SPA-ERRCD
               GO  TO 480-HENKOU-EXT
           END-IF
      *
      *    公費付加情報マスタ更新
           MOVE    KOHPLUS-CREYMD          TO  WKKOHPLUS-CREYMD
           MOVE    WKKOHPLUS-REC           TO  KOHPLUS-REC
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC             TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "upd1"          TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-UPDATE-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE   "4004"               TO  SPA-ERRCD
               GO  TO 480-HENKOU-EXT
           END-IF
      *
      *    公費付加情報リスト編集
           PERFORM 3102-KOHPLUSLST-HEN-SEC
      *
      *    入力項目を初期化する
           PERFORM 420-CLEAR-SEC
      *
           .    
       480-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       490-SAKUJO-SEC                  SECTION.
      *
      *    関連チェック
           MOVE    "DEL"           TO  WRK-UPDKBN
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-SAKUJO-EXT
           END-IF
      *
           MOVE    "2001"          TO  SPA-VIDCD
      *
           .    
       490-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       491-SAKUJO-SEC                  SECTION.
      *
           INITIALIZE                          KOHPLUS-REC
      *****MOVE    SPA-HOSPID              TO  KOHPLUS-HOSPID
           MOVE    SPA-NAI-PREFNUM-MOT     TO  KOHPLUS-PREFNUM
           MOVE    SPA-NAI-CITYNUM-MOT     TO  KOHPLUS-CITYNUM
           MOVE    SPA-NAI-KOHNUM-MOT      TO  KOHPLUS-KOHNUM
           MOVE    SPA-NAI-PTKBN-MOT       TO  KOHPLUS-PTKBN
           MOVE    SPA-NAI-YUKOSTYMD-MOT   TO  KOHPLUS-YUKOSTYMD
           MOVE    SPA-HOSPNUM             TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC             TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "key"           TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-DELETE-SEC
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE    "4005"              TO  SPA-ERRCD
               GO  TO  491-SAKUJO-EXT
           END-IF
      *
      *    公費付加情報リスト編集
           PERFORM 3102-KOHPLUSLST-HEN-SEC
      *
      *    入力項目を初期化する
           PERFORM 420-CLEAR-SEC
      *
           .    
       491-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    選択番号番号
           IF    ( SPA-GMN-SELNUM      NOT =   ZERO )
               IF    ( WRK-UPDKBN          =   "UPD"
                                       OR      "DEL" )
                   CONTINUE
               ELSE
                   MOVE    "1101"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           ELSE
               IF    ( WRK-UPDKBN          =   "INS" )
                   CONTINUE
               ELSE
                   MOVE    "1102"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    有効開始日
           IF    ( SPA-GMN-YUKOSTYMD       =   SPACE )
               MOVE    "1011"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効終了日日
           IF    ( SPA-GMN-YUKOEDYMD       =   SPACE )
               MOVE    "1012"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    削除については以下チェックは行わない
           IF    ( WRK-UPDKBN              =   "DEL" )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    都道府県番号
           IF    ( SPA-GMN-PREFNUM-G       =   SPACE )
               MOVE    "1002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    市町村番号
           IF    ( SPA-GMN-CITYNUM         =   SPACE )
               MOVE    "1003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    公費番号
           IF    ( SPA-GMN-KOHNUM          =   SPACE )
               MOVE    "1004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    制度名
           IF    ( SPA-GMN-SEIDONAME       =   SPACE )
               MOVE    "1005"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    患者区分
           IF    ( SPA-GMN-PTKBN-G     =   SPACE )
               MOVE    "1007"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    患者負担区分
           IF    ( SPA-GMN-FTNKBN-G        =   SPACE )
               MOVE    "1008"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    レセプト請求区分
           IF    ( SPA-GMN-RECESKYKBN-G    =   SPACE )
               MOVE    "1009"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    食事療養費区分
           IF    ( SPA-GMN-NYUSKJRYYKBN-G  =   SPACE )
               MOVE    "1010"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4901-RIREKI-CHK-SEC             SECTION.
      *
      *    履歴チェック
           MOVE    ZERO                TO  FLG-KIKAN-DABURI
           MOVE    ZERO                TO  FLG-KOHPLUS
           INITIALIZE                      KOHPLUS-REC
      *****MOVE    SPA-HOSPID          TO  KOHPLUS-HOSPID
           MOVE    SPA-GMN-PREFNUM     TO  KOHPLUS-PREFNUM
           MOVE    SPA-GMN-CITYNUM     TO  KOHPLUS-CITYNUM
           MOVE    SPA-GMN-KOHNUM      TO  KOHPLUS-KOHNUM
           MOVE    SPA-GMN-PTKBN   TO  KOHPLUS-PTKBN
           MOVE    SPA-HOSPNUM         TO  KOHPLUS-HOSPNUM
           MOVE    KOHPLUS-REC         TO  MCPDATA-REC
           MOVE    "tbl_kohplus"   TO  WRK-PATH-TABLE
           MOVE    "key3"          TO  WRK-PATH-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-KOHPLUS
           ELSE
               MOVE    MCPDATA-REC     TO  KOHPLUS-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-KOHPLUS     NOT =   ZERO )
      *
               IF    ( WRK-UPDKBN              =   "UPD" )
      **********AND  ( SPA-HOSPID              =   KOHPLUS-HOSPID )
                AND  ( SPA-HOSPNUM             =   KOHPLUS-HOSPNUM )
                AND  ( SPA-NAI-PREFNUM-MOT     =   KOHPLUS-PREFNUM )
                AND  ( SPA-NAI-CITYNUM-MOT     =   KOHPLUS-CITYNUM )
                AND  ( SPA-NAI-KOHNUM-MOT      =   KOHPLUS-KOHNUM )
                AND  ( SPA-NAI-PTKBN-MOT
                                               =   KOHPLUS-PTKBN )
                AND  ( SPA-NAI-YUKOSTYMD-MOT
                                               =   KOHPLUS-YUKOSTYMD )
                AND  ( SPA-NAI-YUKOEDYMD-MOT
                                               =   KOHPLUS-YUKOEDYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-YUKOEDYMD
                                               <   KOHPLUS-YUKOSTYMD )
                    OR   ( SPA-NAI-YUKOSTYMD
                                               >   KOHPLUS-YUKOEDYMD )
                       CONTINUE
                   ELSE
                       MOVE    1           TO  FLG-KIKAN-DABURI
                   END-IF
               END-IF
      *
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC        NOT =   ZERO )
                   MOVE    1               TO  FLG-KOHPLUS
               ELSE
                   MOVE    MCPDATA-REC     TO  KOHPLUS-REC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-KIKAN-DABURI    =   1 )
               MOVE    "1103"              TO  SPA-ERRCD
               GO  TO  4901-RIREKI-CHK-EXT
           END-IF
      *
           .
       4901-RIREKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-VIDCD       NOT =   SPACE )
               PERFORM 520-VIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "V03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *  
           MOVE    V03-KOHPLUSLST-ROW  TO  WRK-KOHPLUSLST-ROW
      *
           INITIALIZE                      V03
      *
           IF    ( SPA-GMN-KOHPLUSLST-MAX  >=  CONST-KOHPLUSLST-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  V03-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  V03-B06-STATE
           END-IF
      *
      *    選択番号
           MOVE    SPA-GMN-SELNUM          TO  V03-SELNUM
      *
      *    都道府県番号
           MOVE    SPA-GMN-PREFNUM-G       TO  V03-PREFNUM
           MOVE    SPA-GMN-PREFNUM-MAX     TO  V03-PREFNUM-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-PREFNUM-MAX )
               MOVE    SPA-GMN-PREFNUMLST (IDX-CMB)
                                           TO  V03-PREFNUMLST (IDX-CMB)
           END-PERFORM
      *
      *    市町村番号
           MOVE    SPA-GMN-CITYNUM         TO  V03-CITYNUM
      *
      *    公費番号
           MOVE    SPA-GMN-KOHNUM          TO  V03-KOHNUM
      *
      *    制度名
           MOVE    SPA-GMN-SEIDONAME       TO  V03-SEIDONAME
      *
      *    患者区分
           MOVE    SPA-GMN-PTKBN-G     TO  V03-PTKBN
           MOVE    SPA-GMN-PTKBN-MAX   TO  V03-PTKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-PTKBN-MAX )
               MOVE    SPA-GMN-PTKBNLST (IDX-CMB)
                                           TO  V03-PTKBNLST(IDX-CMB)
           END-PERFORM
      *
      *    患者負担区分
           MOVE    SPA-GMN-FTNKBN-G        TO  V03-FTNKBN
           MOVE    SPA-GMN-FTNKBN-MAX      TO  V03-FTNKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-FTNKBN-MAX )
               MOVE    SPA-GMN-FTNKBNLST (IDX-CMB)
                                           TO  V03-FTNKBNLST (IDX-CMB)
           END-PERFORM
      *
      *    レセプト請求区分
           MOVE    SPA-GMN-RECESKYKBN-G    TO  V03-RECESKYKBN
           MOVE    SPA-GMN-RECESKYKBN-MAX  TO  V03-RECESKYKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-RECESKYKBN-MAX )
               MOVE    SPA-GMN-RECESKYKBNLST (IDX-CMB)
                                           TO  V03-RECESKYKBNLST
                                                               (IDX-CMB)
           END-PERFORM
      *
      *    食事療養費区分
           MOVE    SPA-GMN-NYUSKJRYYKBN-G  TO  V03-NYUSKJRYYKBN
           MOVE    SPA-GMN-NYUSKJRYYKBN-MAX
                                           TO  V03-NYUSKJRYYKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-NYUSKJRYYKBN-MAX )
               MOVE    SPA-GMN-NYUSKJRYYKBNLST (IDX-CMB)
                                           TO  V03-NYUSKJRYYKBNLST
                                                               (IDX-CMB)
           END-PERFORM
      *
      *    有効開始日
           MOVE    SPA-GMN-YUKOSTYMD       TO  V03-YUKOSTYMD
      *
      *    有効終了日日
           MOVE    SPA-GMN-YUKOEDYMD       TO  V03-YUKOEDYMD
      *
      *    公費付加情報リスト
           MOVE    SPA-GMN-KOHPLUSLST-MAX  TO  V03-KOHPLUSLST-COUNT
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >       SPA-GMN-KOHPLUSLST-MAX )
      *
      *        有効表示
               IF   (  SPA-NAI-TYUKOFLG (IDX-LST)
                                               =   ZERO )
                   MOVE    SPACE           TO  V03-TYUKO (IDX-LST)
               ELSE
                   MOVE    "◎"            TO  V03-TYUKO (IDX-LST)
               END-IF
      *
      *        選択番号
               MOVE    SPA-GMN-TSELNUM   (IDX-LST)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  V03-TSELNUM     (IDX-LST)
      *        都道府県番号
               MOVE    SPA-GMN-TPREFNUM  (IDX-LST)
                                           TO  V03-TPREFNUM    (IDX-LST)
      *        市町村番号
               MOVE    SPA-GMN-TCITYNUM  (IDX-LST)
                                           TO  V03-TCITYNUM    (IDX-LST)
      *        公費番号
               MOVE    SPA-GMN-TKOHNUM   (IDX-LST)
                                           TO  V03-TKOHNUM     (IDX-LST)
      *        制度名
               MOVE    SPA-GMN-TSEIDONAME(IDX-LST)
                                           TO  V03-TSEIDONAME  (IDX-LST)
      *        患者区分
               MOVE    SPA-GMN-TPTKBN(IDX-LST)
                                           TO  V03-TPTKBN  (IDX-LST)
      *        患者負担区分
               MOVE    SPA-GMN-TFTNKBN   (IDX-LST)
                                           TO  V03-TFTNKBN     (IDX-LST)
      *        有効開始日
               MOVE    SPA-GMN-TYUKOSTYMD(IDX-LST)
                                           TO  V03-TYUKOSTYMD  (IDX-LST)
      *        有効終了日日
               MOVE    SPA-GMN-TYUKOEDYMD(IDX-LST)
                                           TO  V03-TYUKOEDYMD  (IDX-LST)
      *
               IF    ( SPA-GMN-KOHPLUSLST-SEL  =   IDX-LST )
                   MOVE    "T"             TO  V03-KOHPLUSLST-SELECT
                                                               (IDX-LST)
               ELSE
                   MOVE    "F"             TO  V03-KOHPLUSLST-SELECT
                                                               (IDX-LST)
               END-IF
      *
           END-PERFORM
      *  
           EVALUATE    FLG-KOHPLUSLST-ROW
           WHEN    1
               MOVE    ZERO               TO  V03-KOHPLUSLST-ROW
               MOVE    ZERO               TO  V03-KOHPLUSLST-ROWATTR
           WHEN    2
               MOVE    SPA-GMN-KOHPLUSLST-MAX
                                          TO  V03-KOHPLUSLST-ROW
               MOVE    1                  TO  V03-KOHPLUSLST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-KOHPLUSLST-ROW TO  V03-KOHPLUSLST-ROW
               MOVE    ZERO               TO  V03-KOHPLUSLST-ROWATTR
           END-EVALUATE
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
      *
           MOVE    SPACE                       TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "都道府県番号入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "市町村番号入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "この公費番号は登録されていません"
                                               TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "制度名入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "主保険区分入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "患者区分入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "患者負担区分入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "レセプト請求区分入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "食事療養費入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "有効期間開始日入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "有効期間終了日入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "市町村番号は３桁で入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "選択番号を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "都道府県番号を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "市町村番号を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "公費番号を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "制度名を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "主保険区分を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "患者区分を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "患者負担区分を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "レセプト請求区分を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "食事療養費を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "有効期間開始日を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1012"
               MOVE    "有効期間終了日を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE    "対象のデータが選択してあります"
                                               TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE    "対象のデータを選択してありません"
                                               TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE    "有効期間の重複するデータが存在します"
                                               TO  SPA-ERRMSG
           WHEN    "1104"
               MOVE    "追加可能最大件数に達しました"
                                               TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  VERR
           MOVE    SPA-ERRCD            TO  VERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  VERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "V03"                TO  SPA-MOTOPG
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
           WHEN    "2001"
               MOVE    "削除します。よろしいですか"
                                       TO  WRK-VIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  VID1
           INITIALIZE                      VID1
           MOVE    SPA-VIDCD           TO  VID1-ID1CODE
           MOVE    WRK-VIDMSG          TO  VID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "V03"               TO  SPA-MOTOPG
           MOVE    "VID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "VID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-VIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-GMN-CUR         =   ZERO )
               PERFORM 50011-GMN-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE  "SELNUM"          TO  MCP-WIDGET
           WHEN    02
               MOVE  "PREFNUM"         TO  MCP-WIDGET
           WHEN    03
               MOVE  "CITYNUM"         TO  MCP-WIDGET
           WHEN    04
               MOVE  "KOHNUM"          TO  MCP-WIDGET
           WHEN    05
               MOVE  "SEIDONAME"       TO  MCP-WIDGET
           WHEN    06
               MOVE  "PTKBN"           TO  MCP-WIDGET
           WHEN    07
               MOVE  "FTNKBN"          TO  MCP-WIDGET
           WHEN    08
               MOVE  "RECESKYKBN"      TO  MCP-WIDGET
           WHEN    09
               MOVE  "NYUSKJRYYKBN"    TO  MCP-WIDGET
           WHEN    10
               MOVE  "YUKOSTYMD"       TO  MCP-WIDGET
           WHEN    11
               MOVE  "YUKOEDYMD"       TO  MCP-WIDGET
           WHEN    206
               MOVE  "B06"             TO  MCP-WIDGET
           END-EVALUATE
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
           WHEN    "SELNUM"
               MOVE    01              TO  SPA-GMN-CUR
           WHEN    "PREFNUM"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "CITYNUM"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "KOHNUM"
               MOVE    04              TO  SPA-GMN-CUR
           WHEN    "SEIDONAME"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "PTKBN"
               MOVE    06              TO  SPA-GMN-CUR
           WHEN    "FTNKBN"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "RECESKYKBN"
               MOVE    08              TO  SPA-GMN-CUR
           WHEN    "NYUSKJRYYKBN"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "YUKOSTYMD"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "YUKOEDYMD"
               MOVE    11              TO  SPA-GMN-CUR
      *
           END-EVALUATE
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "YUKOEDYMD"
               MOVE    206             TO  SPA-GMN-CUR
           WHEN    OTHER
               COMPUTE SPA-GMN-CUR =   SPA-GMN-CUR +   1
           END-EVALUATE
      *
           .
       50011-GMN-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           IF    ( WRK-YMD             =   "00000000" 
                                   OR      "99999999" )
               MOVE    WRK-YMD         TO  WRK-HENYMDG
                                           WRK-SYMD
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9               TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU     TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-DBRC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-TBL-READ-SEC
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-DBRC
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル更新処理
      *****************************************************************
       900-TBL-UPDATE-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-DBRC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル追加処理
      *****************************************************************
       900-TBL-INSERT-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-DBRC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル削除処理
      *****************************************************************
       900-TBL-DELETE-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-DBRC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-DELETE-EXT.
           EXIT.
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
