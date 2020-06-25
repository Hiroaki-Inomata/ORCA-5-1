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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGV01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 保険番号情報入力
      *  コンポーネント名  : 保険番号情報入力（Ｖ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.00.02    MCC-藤原     01/10/09  共通ＳＰＡの項目追加     
      *  01.02.01    MCC-藤原     02/02/01  入力エラーの時の確定のチェック     
      *  01.02.02    MCC-藤原     02/02/21  支払区分の初期値のセット     
      *  01.02.03    MCC-藤原     02/03/22  選択番号未入力のときの遷移修正     
      *  01.02.04    NACL-多々納  02/12/19  リスト選択修正
      *  01.02.05    NACL-多々納  03/02/14  複写追加の追加
      *                                     一括入力へ変更
      *  04.06.00    NACL-多々納  11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      *  04.07.00    NACL-太田    11/08/09  システム管理業務への遷移処理追加
      *                                     前回ＣＤボタン追加
      *  04.07.00    NACL-太田    11/08/09  システム管理業務への遷移処理追加
      *  04.08.00    NACL-森脇    15/11/05  複写元保険番号を追加
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
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
       01  SPA-V01.
           03  SPA-V01-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLO    OCCURS  100.
                           11  SPA-GMN-TBANGO     PIC 9(03).
                           11  SPA-GMN-TPAYKBN    PIC X(02). 
                           11  SPA-GMN-TSTYUKYMD  PIC X(09).
                           11  SPA-GMN-TEDYUKYMD  PIC X(09).  
                   07  SPA-GMN-HKNNUM             PIC X(03).
                   07  SPA-GMN-HKNNUM-NO          PIC 9(03).
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SELNUM         PIC 9(03).
                       09  SPA-GMN-STYUKYMD       PIC X(10).
                       09  SPA-GMN-EDYUKYMD       PIC X(10).
                       09  SPA-GMN-TANSEIDONAME   PIC X(10).
                       09  SPA-GMN-PAYKBN         PIC X(02). 
      *----(04.08.00)--ADD-START---
                       09  SPA-GMN-CPHKNNUM       PIC X(03).
      *----(04.08.00)--ADD-END-----
                   07  SPA-GMN-PREFNUM-G.
                       09  SPA-GMN-PREFNUM         PIC X(2).
                       09  SPA-GMN-PREFNUMFL       PIC X(1).
                       09  SPA-GMN-PREFNUMMEI      PIC X(20).
                   07  SPA-GMN-PREFNUMLST-G.
                       09  SPA-GMN-PREFNUM-MAX         PIC 9(03).
                       09  SPA-GMN-PREFNUMLST          OCCURS  50.
                           11  SPA-GMN-PREFNUML        PIC X(02).
                           11  SPA-GMN-PREFNUMFLL      PIC X(01).
                           11  SPA-GMN-PREFNUMMEIL     PIC X(20).
                   07  SPA-GMN-MESSAGE-STYLE   PIC X(20).
                   07  SPA-GMN-MESSAGE         PIC X(100).
                   07  SPA-GMN-DURATION        PIC S9(09).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-HKNNUM             PIC X(03).
                   07  SPA-NAI-KOUMOKU.
                       09  SPA-NAI-STYUKYMD       PIC X(08).
                       09  SPA-NAI-EDYUKYMD       PIC X(08).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO    OCCURS  100.
                           11  SPA-NAI-TSTYUKYMD  PIC 9(08).
                           11  SPA-NAI-TEDYUKYMD  PIC 9(08).
      *
              05  SPA-NAI-SELNO                   PIC 9(03).
      *
           COPY    "ENUM-VALUE".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPMKPASS.INC".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-HKNNUM                         PIC 9(01).
           03  FLG-CHK                            PIC 9(01).
           03  FLG-KOUSIN                         PIC 9(01).
           03  FLG-OK                             PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
           03  FLG-SPATMP                         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
      *
      *    処理区分領域
       01  SKBN-AREA.
           03  SKBN-RUN            PIC X(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-FILEMEI         PIC X(20).
           03  WRK-PATH-TABLE      PIC X(64).
           03  WRK-PATH-PATHNAME   PIC X(64).
           03  WRK-VIDMSG          PIC X(70). 
           03  WRK-SELNO           PIC 9(03).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-MAE-HKNNUM      PIC X(03).
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
       01  SHELL-AREA.
           03  SHELL-NAME          PIC X(13)  VALUE
               "master_get.sh".
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "MSTGET".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
           COPY    "CPLNKSYS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HKNNUM-REC.
           COPY  "CPHKNNUM.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-V01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-V01
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-V01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-V01         TO  SPA-FREE
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
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "V01    "           TO  MCP-WINDOW.
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
           MOVE    1               TO  SPA-GMN-CUR
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               IF    ( SPA-MOTOPG      =   "W01" )
                   PERFORM 800-SPA-INPUT-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "VID1"
                   PERFORM 330-VID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
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
               PERFORM 4101-KAKUTEI-SYORI-SEC
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    SPACE           TO  SPA-VID1-FLG
           MOVE    SPACE           TO  SPA-VIDCD      
           .
       330-VID1-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPA-V01-MAE-HKNNUM
                                   TO  WRK-MAE-HKNNUM
      *
           INITIALIZE              SPA-V01-AREA
                                   SPA-V01KYOUTU
      *
           MOVE    WRK-MAE-HKNNUM  TO  SPA-V01-MAE-HKNNUM
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
      *    都道府県番号コンボ項目設定
           PERFORM 3101-PREFNUMLST-SET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
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
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM  210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  290-CLREA-SEC
      *        前回ＣＤ
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  230-ZENKAICD-SEC
      *        公費付加情報遷移
               WHEN    "CLICKED"       ALSO    "B05"
                   IF    ( SPA-PREFNUM =   40 )
                       PERFORM  450-V03-SENI-SEC
                   END-IF
      *        付加設定（システム管理）
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-W65-SENI-SEC
      *        状況
               WHEN    "CLICKED"       ALSO    "pandatimer1"
                   PERFORM 400-JYOKYO-SEC
      *        情報削除
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 400-JOBDEL-SEC
      *        参照
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 400-JYOUHOU-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  410-KAKUTEI-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-MESSAGE
           MOVE    SPACE           TO  SPA-GMN-MESSAGE-STYLE
           MOVE    ZERO            TO  SPA-GMN-DURATION
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        保険番号
               WHEN    "ACTIVATE"       ALSO    "HKNNUM"
                   PERFORM  4104-HKNNUM-CHK-SEC
                   PERFORM  41041-HKNNUM-HENSYU-SEC
      *----(04.08.00)--ADD-START---
      *        都道府県番号
               WHEN    "ACTIVATE"      ALSO    "PREFNUM"
                   PERFORM 4101-PREFNUM-CHK-SEC
      *        複写元保険番号
               WHEN    "ACTIVATE"      ALSO    "CPHKNNUM"
                   PERFORM 4101-CPHKNNUM-CHK-SEC
      *        支払区分
               WHEN    "ACTIVATE"      ALSO    "PAYKBN"
                   PERFORM 4105-PAYKBN-CHK-SEC
      *        有効開始年月日
               WHEN    "ACTIVATE"      ALSO    "STYUKYMD"
                   PERFORM 4102-STYUKYMD-CHK-SEC
      *        有効終了年月日
               WHEN    "ACTIVATE"      ALSO    "EDYUKYMD"
                   PERFORM 4103-EDYUKYMD-CHK-SEC
      *----(04.08.00)--ADD-END-----
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  4101-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 41015-LIST1SEL-SEC
      *        その他一括
               WHEN    OTHER
                   MOVE    1                   TO  FLG-KOUSIN
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
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
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
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
           MOVE    V01-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    V01-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
           MOVE    V01-PAYKBN          TO  SPA-GMN-PAYKBN
           MOVE    V01-PREFNUM         TO  SPA-GMN-PREFNUM-G
            .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    都道府県番号チェック
           PERFORM 4101-PREFNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM  4105-PAYKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効開始年月日
           PERFORM  4102-STYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効終了年月日
           PERFORM 4103-EDYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       290-CLREA-SEC                    SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           .
      *
       290-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M02"               TO  SPA-SAKIPG
           MOVE    "V01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    前回ＣＤ処理
      *****************************************************************
       230-ZENKAICD-SEC                SECTION.
      *
           IF    ( SPA-V01-MAE-HKNNUM  NOT =   SPACE )
               MOVE    SPA-V01-MAE-HKNNUM  TO  V01-HKNNUM
               PERFORM 4104-HKNNUM-CHK-SEC
               PERFORM  41041-HKNNUM-HENSYU-SEC
           END-IF
      *
           .
       230-ZENKAICD-EXT.
           EXIT.
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       4101-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    3               TO  SPA-GMN-CUR
      *
           INITIALIZE                     SPA-GMN-KOUMOKU
                                          SPA-NAI-KOUMOKU
                                          SPA-NAI-SELNO
      *
           MOVE    V01-SELNUM             TO  SPA-GMN-SELNUM
      *
      *    選択番号チェック
      ***     IF      SPA-GMN-SELNUM      =   ZERO
           IF     (SPA-GMN-SELNUM      =   ZERO)   AND
                  (SPA-GMN-MAX         =   ZERO)
               IF      SPA-GMN-HKNNUM  =   "027"
                   MOVE    6                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    ZERO                TO  SPA-GMN-PAYKBN
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF         
               GO  TO  4101-SELNUM-CHK-EXT 
           ELSE
               IF     (SPA-GMN-SELNUM      >=  1  )
               AND    (SPA-GMN-SELNUM      <=  SPA-GMN-MAX )
                   CONTINUE
                      
               ELSE
                   IF   (SPA-GMN-SELNUM   =    ZERO)  AND
                        (SPA-GMN-MAX      NOT =   ZERO)
                      MOVE    SPA-GMN-MAX  TO   SPA-GMN-SELNUM
                   ELSE
                      MOVE    "0001"                  TO  SPA-ERRCD
                      GO  TO  4101-SELNUM-CHK-EXT 
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-SELNO
      *
           PERFORM 41011-SELNUM-HENSYU-SEC               
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41011-SELNUM-HENSYU-SEC           SECTION.
      *
           MOVE    SPA-GMN-TSTYUKYMD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-STYUKYMD
           MOVE    SPA-NAI-TSTYUKYMD(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-STYUKYMD
           MOVE    SPA-GMN-TEDYUKYMD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-NAI-TEDYUKYMD(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-EDYUKYMD
           MOVE    SPA-GMN-TPAYKBN  (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-PAYKBN
      * 
           INITIALIZE                      HKNNUM-REC
      *****MOVE    SPA-HOSPID              TO  HKN-HOSPID
           MOVE    SPA-GMN-HKNNUM          TO  HKN-HKNNUM
           MOVE    SPA-GMN-PAYKBN          TO  HKN-PAYKBN
           MOVE    SPA-NAI-STYUKYMD        TO  HKN-TEKSTYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  HKN-TEKEDYMD
           MOVE    SPA-HOSPNUM             TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC              TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  WRK-PATH-TABLE
           MOVE    "key"               TO  WRK-PATH-PATHNAME
           PERFORM 900-HKNNUM-SELECT-SEC
           PERFORM 900-HKNNUM-CLOSE-SEC
           IF      FLG-HKNNUM              =   ZERO
               MOVE    MCPDATA-REC             TO  HKNNUM-REC
               MOVE    HKN-TANSEIDONAME        TO  SPA-GMN-TANSEIDONAME
           END-IF
      *
           MOVE    4                   TO  SPA-GMN-CUR 
           .
       41011-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効開始年月日チェック処理
      *****************************************************************
       4102-STYUKYMD-CHK-SEC          SECTION.
      *
      *----(04.08.00)--ADD-START---
           MOVE    V01-STYUKYMD        TO  SPA-GMN-STYUKYMD
      *----(04.08.00)--ADD-END-----
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               MOVE    ZERO               TO  SPA-NAI-STYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-SYMD             TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG          TO  SPA-GMN-STYUKYMD
           ELSE
               MOVE    4                  TO  SPA-GMN-CUR
               GO  TO  4102-STYUKYMD-CHK-EXT
           END-IF
      *
           .
       4102-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了年月日チェック処理
      *****************************************************************
       4103-EDYUKYMD-CHK-SEC          SECTION.
      *
      *----(04.08.00)--ADD-START---
           MOVE    V01-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
      *----(04.08.00)--ADD-END-----
      *
           IF      SPA-GMN-EDYUKYMD    =   SPACE
               IF     (MCP-WIDGET      =   "EDYUKYMD"  )  OR
                      (FLG-KOUSIN      =   2           )
                   MOVE    "99999999"      TO  SPA-GMN-EDYUKYMD
                   MOVE    ALL "9"         TO  SPA-NAI-EDYUKYMD
               ELSE
                   MOVE    ZERO               TO  SPA-NAI-EDYUKYMD
                   GO      TO      4103-EDYUKYMD-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-EDYUKYMD   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-SYMD             TO  SPA-NAI-EDYUKYMD 
               MOVE    WRK-HENYMDG          TO  SPA-GMN-EDYUKYMD
           ELSE
               MOVE    5                  TO  SPA-GMN-CUR
           END-IF
      *
      *----(04.08.00)--ADD-START---
           IF      SPA-NAI-STYUKYMD   >   SPA-NAI-EDYUKYMD
               MOVE    "0008"             TO  SPA-ERRCD
               MOVE    4                  TO  SPA-GMN-CUR
           END-IF
      *----(04.08.00)--ADD-END-----
      *
           .
       4103-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号　入力処理
      *****************************************************************
       4104-HKNNUM-CHK-SEC       SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-GMN-MAX
                                       SPA-GMN-TBL
                                       SPA-NAIB-AREA
                                       SPA-NAI-SELNO 
      *
           MOVE    V01-HKNNUM     TO  SPA-GMN-HKNNUM
      * 
           INITIALIZE                  HKNNUM-REC
      *****MOVE    SPA-HOSPID      TO  HKN-HOSPID
           MOVE    SPA-GMN-HKNNUM  TO  HKN-HKNNUM
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  WRK-PATH-TABLE
           MOVE    "key2"              TO  WRK-PATH-PATHNAME
           PERFORM 900-HKNNUM-SELECT-SEC
           IF      FLG-HKNNUM              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
           ELSE
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
      *----(04.08.00)--ADD-START---
           MOVE    ZERO                TO  FLG-OK
      *----(04.08.00)--ADD-END-----
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-HKNNUM   =   1
                               OR      IDX            >=  100
               MOVE    MCPDATA-REC        TO  HKNNUM-REC
               ADD     1                  TO  IDX
               MOVE    IDX                TO  SPA-GMN-TBANGO    (IDX)
               MOVE    HKN-PAYKBN         TO  SPA-GMN-TPAYKBN   (IDX)
               MOVE    HKN-TEKSTYMD       TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    HKN-TEKSTYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG        TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    HKN-TEKEDYMD       TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    HKN-TEKEDYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG        TO  SPA-GMN-TEDYUKYMD (IDX)
               MOVE    IDX                TO  SPA-GMN-MAX
      *----(04.08.00)--ADD-START---
               IF    ( HKN-TEKSTYMD       =   "99999999" ) AND
                     ( HKN-TEKEDYMD       =   "99999999" )
                   MOVE    1                  TO  FLG-OK
               END-IF
      *----(04.08.00)--ADD-END-----
      *
               PERFORM 900-HKNNUM-FETCH-SEC
           END-PERFORM
      *    カーソルクロース
           PERFORM 900-HKNNUM-CLOSE-SEC
      *
           MOVE    SPA-GMN-HKNNUM      TO  SPA-NAI-HKNNUM
           MOVE    SPA-GMN-HKNNUM      TO  SPA-GMN-HKNNUM-NO
      *     
           .
       4104-HKNNUM-CHK-EXT.
           EXIT.
      *
      *----(04.08.00)--UPD-START---
      *****************************************************************
      *    保険番号　編集処理
      *****************************************************************
       41041-HKNNUM-HENSYU-SEC     SECTION.
      *
           EVALUATE    SPA-GMN-MAX
               WHEN    ZERO
                   IF      SPA-GMN-HKNNUM  =   "027"
                       MOVE    6                   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    ZERO            TO  SPA-GMN-PAYKBN
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
               WHEN    1
                   MOVE    SPA-GMN-TSTYUKYMD (1)   TO  SPA-GMN-STYUKYMD
                   MOVE    SPA-NAI-TSTYUKYMD (1)   TO  SPA-NAI-STYUKYMD
                   MOVE    SPA-GMN-TEDYUKYMD (1)   TO  SPA-GMN-EDYUKYMD
                   MOVE    SPA-NAI-TEDYUKYMD (1)   TO  SPA-NAI-EDYUKYMD
                   MOVE    SPA-GMN-TPAYKBN   (1)   TO  SPA-GMN-PAYKBN
                   MOVE    HKN-TANSEIDONAME        TO
                                                   SPA-GMN-TANSEIDONAME
                   MOVE    1                       TO  SPA-GMN-SELNUM
                   MOVE    1                       TO  SPA-NAI-SELNO 
                   MOVE    4                       TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    3                       TO  SPA-GMN-CUR
           END-EVALUATE
      *
           IF    ( SPA-GMN-HKNNUM-NO   >=  100 )  AND
                 ( SPA-GMN-HKNNUM-NO   <=  899 )
               IF    ( SPA-GMN-MAX         =   ZERO )  OR
                     ( SPA-NAI-TEDYUKYMD(SPA-GMN-MAX)
                                           NOT =   "99999999" )
                   PERFORM 900-SYSKANRI-READ-SEC
                   MOVE    SYS-1001-PREFNUM    TO  V01-PREFNUM
                   PERFORM 4101-PREFNUM-CHK-SEC
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
               END-IF
           END-IF 
      *
           .
       41041-HKNNUM-HENSYU-EXT.
           EXIT.
      *----(04.08.00)--UPD-END-----
      *****************************************************************
      *    支払区分　入力処理
      *****************************************************************
       4105-PAYKBN-CHK-SEC       SECTION.
      *
      *----(04.08.00)--ADD-START---
           MOVE    V01-PAYKBN          TO  SPA-GMN-PAYKBN
      *----(04.08.00)--ADD-END-----
      *
           IF      SPA-GMN-PAYKBN  =   SPACE
               MOVE    ZERO           TO  SPA-GMN-PAYKBN
           END-IF
           .
       4105-PAYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県番号チェック処理
      *****************************************************************
       4101-PREFNUM-CHK-SEC            SECTION.
      *
      *----(04.08.00)--ADD-START---
           MOVE    V01-PREFNUM         TO  SPA-GMN-PREFNUM-G
           IF    ( SPA-GMN-MAX         =   ZERO )  OR
                 ( SPA-GMN-CPHKNNUM    NOT =   SPACE )
               MOVE    4                   TO  SPA-GMN-CUR
           ELSE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *----(04.08.00)--ADD-END-----
      *
           IF    ( SPA-GMN-PREFNUM-G   =   SPACE )
               GO  TO  4101-PREFNUM-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL ( IDY     >   SPA-GMN-PREFNUM-MAX )
               IF    ( SPA-GMN-PREFNUM =   SPA-GMN-PREFNUML (IDY) )
                   MOVE    SPA-GMN-PREFNUMLST (IDY)
                                       TO  SPA-GMN-PREFNUM-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-PREFNUM-MAX TO  IDY
               END-IF
           END-PERFORM
      *
          IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
           .
      *
       4101-PREFNUM-CHK-EXT.
           EXIT.
      *
      *----(04.08.00)--ADD-START---
      *****************************************************************
      *    複写元保険番号チェック処理
      *****************************************************************
       4101-CPHKNNUM-CHK-SEC       SECTION.
      *
           MOVE    V01-CPHKNNUM    TO  SPA-GMN-CPHKNNUM
           MOVE    2               TO  SPA-GMN-CUR
           .
      *
       4101-CPHKNNUM-CHK-EXT.
           EXIT.
      *
      *----(04.08.00)--ADD-END-----
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41015-LIST1SEL-SEC       SECTION.
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               IF      V01-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-SELNO 
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNO
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-SELNO 
           END-IF
      *
           MOVE    SPA-NAI-SELNO      TO  SPA-GMN-SELNUM 
      *
           PERFORM 41011-SELNUM-HENSYU-SEC
           .
       41015-LIST1SEL-EXT.
           EXIT.
      *
     *****************************************************************
      *    状況
      *****************************************************************
       400-JYOKYO-SEC              SECTION.
      *
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "参照中です。"  TO  SPA-GMN-MESSAGE
               MOVE    "blue"          TO  SPA-GMN-MESSAGE-STYLE
               MOVE    5               TO  SPA-GMN-DURATION
               GO  TO  400-JYOKYO-EXT
           ELSE
               MOVE    SPACE           TO  SPA-GMN-MESSAGE
               MOVE    SPACE           TO  SPA-GMN-MESSAGE-STYLE
               MOVE    ZERO            TO  SPA-GMN-DURATION
           END-IF
      *
           PERFORM 4104-HKNNUM-CHK-SEC
      *
      *----(04.08.00)--UPD-START----
           IF      FLG-OK          =   ZERO
               MOVE    SPACE           TO  SKBN-RUN
               MOVE    "該当データはありませんでした。"
                                       TO  SPA-GMN-MESSAGE
               MOVE    "blue"          TO  SPA-GMN-MESSAGE-STYLE
               IF      V01-CPHKNNUM    =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               ELSE
                   MOVE    V01-CPHKNNUM    TO  SPA-GMN-CPHKNNUM
                   MOVE    7               TO  SPA-GMN-CUR
               END-IF
               GO  TO  400-JYOKYO-EXT
           END-IF
      *----(04.08.00)--UPD-END-----
      *
           PERFORM 41041-HKNNUM-HENSYU-SEC
      *
           .
       400-JYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報入手
      *****************************************************************
       400-JYOUHOU-SEC             SECTION.
      *
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "参照中です。"  TO  SPA-GMN-MESSAGE
               MOVE    "blue"          TO  SPA-GMN-MESSAGE-STYLE
               MOVE    5               TO  SPA-GMN-DURATION
               GO  TO  400-JYOUHOU-EXT
           END-IF
      *
      *    ジョブ管理削除
           PERFORM 820-JOBKANRI-DEL-SEC
      *
      *    ジョブ管理作成
           PERFORM 820-JOBKANRI-JBS-SEC
      *
      *    処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
      *
           MOVE    SHELL-NAME      TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG2
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG3
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG4
           MOVE    "HKNNUM"        TO  SHELLTBL-ARG5
           MOVE    SPA-GMN-PREFNUM TO  SHELLTBL-ARG6
      *----(04.08.00)--UPD-START---
           MOVE    SPA-GMN-HKNNUM  TO  SHELLTBL-ARG8
           IF      SPA-GMN-CPHKNNUM    =   SPACE
               MOVE    SPA-GMN-HKNNUM      TO  SHELLTBL-ARG7
           ELSE
               MOVE    SPA-GMN-CPHKNNUM    TO  SHELLTBL-ARG7
           END-IF
      *----(04.08.00)--UPD-END-----
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM 400-JYOKYO-SEC
      *
           .
       400-JYOUHOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ削除
      *****************************************************************
       400-JOBDEL-SEC              SECTION.
      *
           PERFORM 820-JOBKANRI-DEL-SEC
           MOVE    SPACE           TO  SPA-GMN-MESSAGE
           MOVE    SPACE           TO  SPA-GMN-MESSAGE-STYLE
           MOVE    ZERO            TO  SPA-GMN-DURATION
      *
           .
       400-JOBDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
           MOVE    2                   TO  FLG-KOUSIN
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-KAKUTEI-EXT
           END-IF
      *
           IF      SPA-NAI-STYUKYMD        =   ZERO
               IF      SPA-GMN-STYUKYMD(1:8)   =   ALL "0"
                   CONTINUE
               ELSE
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "0006"              TO  SPA-ERRCD
                   GO  TO  410-KAKUTEI-EXT
               END-IF
           END-IF    
           IF     (SPA-NAI-EDYUKYMD    =   ZERO   )
           OR     (SPA-NAI-HKNNUM      =   SPACE  )
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0006"              TO  SPA-ERRCD
               GO  TO  410-KAKUTEI-EXT
           END-IF
      *----(04.08.00)--ADD-START---
           IF    ( SPA-GMN-MAX     >   1 )
           AND   ( SPA-NAI-STYUKYMD    =   "00000000" )
           AND   ( SPA-NAI-EDYUKYMD    =   "99999999" )
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    "0004"              TO  SPA-ERRCD 
               GO  TO  410-KAKUTEI-EXT
           END-IF
      *----(04.08.00)--ADD-END-----
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL  (IDX >       SPA-GMN-MAX  )
                   OR     (SPA-ERRCD   NOT =   SPACE)
               IF      SPA-GMN-SELNUM      =   IDX
                   CONTINUE
               ELSE
      *----(04.08.00)--UPD-START---
                   IF   (( SPA-NAI-STYUKYMD
                                       <   SPA-NAI-TSTYUKYMD(IDX) )
                     AND ( SPA-NAI-EDYUKYMD
                                       <   SPA-NAI-TSTYUKYMD(IDX) ))
                     OR (( SPA-NAI-STYUKYMD
                                       >   SPA-NAI-TEDYUKYMD(IDX))
                     AND ( SPA-NAI-EDYUKYMD
                                       >   SPA-NAI-TEDYUKYMD(IDX) ))
      *H30.10        支払区分判定
                     OR  ( SPA-GMN-PAYKBN  NOT =  SPA-GMN-TPAYKBN
                                                            (IDX ))
                       CONTINUE
                   ELSE
                       MOVE    "0004"      TO  SPA-ERRCD 
                   END-IF
               END-IF
           END-PERFORM 
           IF      SPA-ERRCD           NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
               GO  TO  410-KAKUTEI-EXT
           END-IF      
      *----(04.08.00)--UPD-END-----
      *
           INITIALIZE                  SPA-V01KYOUTU
      *
      *    選択があり、有効年月日が違う時、新規とする
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    SPA-GMN-SELNUM      TO  IDX
               MOVE    ZERO                TO  FLG-CHK
               IF      (SPA-NAI-STYUKYMD   >=  SPA-NAI-TSTYUKYMD (IDX))
                 AND   (SPA-NAI-STYUKYMD   <=  SPA-NAI-TEDYUKYMD (IDX))
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF     (SPA-NAI-EDYUKYMD    >=  SPA-NAI-TSTYUKYMD (IDX))
                 AND  (SPA-NAI-EDYUKYMD    <=  SPA-NAI-TEDYUKYMD (IDX))
                   MOVE    1               TO  FLG-CHK
               END-IF
      *        範囲外
               IF      FLG-CHK             =   ZERO
                   MOVE    1                   TO  SPA-V01-SYORI
               ELSE
                   MOVE    2                   TO  SPA-V01-SYORI
               END-IF
               IF      (SPA-NAI-STYUKYMD   =   SPA-NAI-TSTYUKYMD(IDX))
                 AND   (SPA-NAI-EDYUKYMD   =   SPA-NAI-TEDYUKYMD(IDX))
                   MOVE    ZERO                TO  SPA-V01-SYORI
               END-IF
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-OLD-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-OLD-EDYUKYMD
               IF      SPA-V01-SYORI       =   1
                   MOVE    "1001"              TO  SPA-VIDCD
               END-IF
               IF      SPA-V01-SYORI       =   2
                   MOVE    "1002"              TO  SPA-VIDCD
               END-IF
           ELSE
               MOVE    1                  TO  SPA-V01-SYORI
           END-IF
      *
           IF      SPA-VIDCD           =   SPACE
               MOVE    SPACE           TO  SPA-VID1-FLG
               PERFORM 4101-KAKUTEI-SYORI-SEC
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新画面遷移処理
      *****************************************************************
       4101-KAKUTEI-SYORI-SEC         SECTION.
      *
      *----(04.08.00)--ADD-START---
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD      TO  HKN-TEKSTYMD
                                       HKN-TEKEDYMD
           MOVE    SPA-GMN-CPHKNNUM
                                   TO  HKN-HKNNUM
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  WRK-PATH-TABLE
           MOVE    "key5"              TO  WRK-PATH-PATHNAME
           PERFORM 900-HKNNUM-SELECT-SEC
           IF      FLG-HKNNUM              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
           ELSE
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
      *    複写元が存在すれば
           IF      FLG-HKNNUM      =   ZERO
               MOVE    SPA-GMN-CPHKNNUM    TO  SPA-V01-CPHKNNUM
           END-IF
      *----(04.08.00)--ADD-END-----
      *
      *    画面移行用のパラメタ
           MOVE    SPA-GMN-HKNNUM      TO  SPA-V01-HKNNUM
                                           SPA-V01-MAE-HKNNUM
           IF      SPA-GMN-PAYKBN      =   SPACE
               MOVE    "00"                TO  SPA-V01-PAYKBN
           ELSE
               MOVE    SPA-GMN-PAYKBN      TO  SPA-V01-PAYKBN
           END-IF
           MOVE    SPA-NAI-STYUKYMD    TO  SPA-V01-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  SPA-V01-EDYUKYMD 
           MOVE    SPA-GMN-STYUKYMD    TO  SPA-V01-STYUKYMDH
           MOVE    SPA-GMN-EDYUKYMD    TO  SPA-V01-EDYUKYMDH
           MOVE    SPA-GMN-SELNUM      TO  SPA-V01-SELNUM
           MOVE    SPA-V01KYOUTU       TO  SPA-WORK
      *
           MOVE    "V02"               TO  SPA-SAKIPG
           MOVE    "V01"               TO  SPA-MOTOPG
      *
           IF      SPA-VID1-FLG        =   "OK"
               MOVE   "JOIN"               TO  MCP-PUTTYPE
           ELSE
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
           END-IF
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4101-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費付加情報設定処理へ
      *****************************************************************
       450-V03-SENI-SEC                SECTION.
      *
           MOVE    "V03"               TO  SPA-SAKIPG
           MOVE    "V01"               TO  SPA-MOTOPG
      *
           MOVE    SPA-V01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-V03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    付加設定（システム管理）へ
      *****************************************************************
       450-W65-SENI-SEC                SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "V01"               TO  SPA-MOTOPG
      *
           PERFORM 800-SPA-OUTPUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       LNKSYS-AREA
           MOVE    SPA-GMN-HKNNUM      TO  LNKSYS-HKNNUM
           MOVE    LNKSYS-AREA         TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-W65-SENI-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
      *----(01.00.03) LINE ADD START --------------------------------
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    "0002"              TO  SPA-ERRCD
                   END-IF
      *----(01.00.03) LINE ADD END   --------------------------------
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           MOVE    "V01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  V01
      *
           INITIALIZE                      V01
           MOVE    ZERO                TO  V01-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               MOVE    SPA-GMN-TBANGO     (IDX)
                                          TO  V01-TBANGO   (IDX)
               MOVE    SPA-GMN-TPAYKBN    (IDX)
                                          TO  V01-TPAYKBN  (IDX)
               MOVE    SPA-GMN-TSTYUKYMD  (IDX)
                                          TO  V01-TSTYUKYMD(IDX)
               MOVE    SPA-GMN-TEDYUKYMD  (IDX)
                                          TO  V01-TEDYUKYMD(IDX)
               MOVE    IDX                TO  V01-COUNT
               IF      IDX             =   SPA-NAI-SELNO 
                   MOVE    "T"             TO  V01-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  V01-SELECT (IDX)
               END-IF
           END-PERFORM   
           MOVE    SPA-GMN-SELNUM      TO  V01-SELNUM
           MOVE    SPA-GMN-STYUKYMD    TO  V01-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  V01-EDYUKYMD
           MOVE    SPA-GMN-HKNNUM      TO  V01-HKNNUM
           MOVE    SPA-GMN-TANSEIDONAME
                                       TO  V01-TANSEIDONAME
           MOVE    SPA-GMN-PAYKBN      TO  V01-PAYKBN
      *
           IF    ( SPA-NAI-HKNNUM     =   "027" )
           AND   ( SPA-GMN-SELNUM     =   ZERO  )
               MOVE    WIDGET-NORMAL      TO  V01-PAYKBN-STATE
           ELSE         
               MOVE    WIDGET-INSENSITIVE TO  V01-PAYKBN-STATE
           END-IF
      *
           IF    ( SPA-PREFNUM        =   40 )
               MOVE    "公費付加"      TO V01-B05
           ELSE
               MOVE    SPACE           TO V01-B05
           END-IF
      *
           MOVE    "付加設定"          TO V01-B06
      *
      *----(04.08.00)--UPD-START---
           IF    ( SPA-GMN-HKNNUM-NO   >=  100 )  AND
                 ( SPA-GMN-HKNNUM-NO   <=  899 )
               IF    ( SPA-GMN-MAX         =   ZERO )  OR
                     ( SPA-NAI-TEDYUKYMD(SPA-GMN-MAX)
                                           NOT =   "99999999" )
                   MOVE    WIDGET-NORMAL       TO  V01-PREFNUM-STATE
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  V01-PREFNUM-STATE
                   MOVE    SPACE               TO  SPA-GMN-PREFNUM-G
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  V01-PREFNUM-STATE
               MOVE    SPACE               TO  SPA-GMN-PREFNUM-G
           END-IF
      *----(04.08.00)--UPD-END-----
      *
      *    都道府県番号
           MOVE    SPA-GMN-PREFNUM-G       TO  V01-PREFNUM
           MOVE    SPA-GMN-PREFNUM-MAX     TO  V01-PREFNUM-COUNT
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL ( IDY     >   SPA-GMN-PREFNUM-MAX )
               MOVE    SPA-GMN-PREFNUMLST (IDY)
                                           TO  V01-PREFNUMLST (IDY)
           END-PERFORM
      *
           IF        ( SPA-GMN-PREFNUM NOT =   SPACE )
      *         AND   ( SPA-ERRCD           =   SPACE )
               MOVE    WIDGET-NORMAL       TO  V01-B11-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  V01-B11-STATE
           END-IF
      *
           MOVE    SPA-GMN-MESSAGE     TO  V01-MESSAGE
           MOVE    SPA-GMN-MESSAGE-STYLE
                                       TO  V01-MESSAGE-STYLE
           MOVE    SPA-GMN-DURATION    TO  V01-DURATION
      *
      *----(04.08.00)--ADD-START---
           IF    ( SPA-GMN-HKNNUM-NO   >=  100 )  AND
                 ( SPA-GMN-HKNNUM-NO   <=  899 )
               IF    ( SPA-GMN-MAX         =   ZERO )  OR
                     ( SPA-NAI-TEDYUKYMD(SPA-GMN-MAX)
                                           NOT =   "99999999" )
                   MOVE    WIDGET-NORMAL       TO  V01-CPHKNNUM-STATE
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  V01-CPHKNNUM-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  V01-CPHKNNUM-STATE
           END-IF
      *
           MOVE    SPA-GMN-CPHKNNUM    TO  V01-CPHKNNUM
      *----(04.08.00)--ADD-END-----
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
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
                WHEN   1
                    MOVE  "HKNNUM"     TO  MCP-WIDGET
                WHEN   2
                    MOVE  "PREFNUM"    TO  MCP-WIDGET
                WHEN   3
                    MOVE  "SELNUM"     TO  MCP-WIDGET
                WHEN   4
                    MOVE  "STYUKYMD"   TO  MCP-WIDGET
                WHEN   5
                    MOVE  "EDYUKYMD"   TO  MCP-WIDGET
                WHEN   6
                    MOVE  "PAYKBN"      TO  MCP-WIDGET
      *----(04.08.00)--ADD-START---
                WHEN   7
                    MOVE  "CPHKNNUM"   TO  MCP-WIDGET
      *----(04.08.00)--ADD-END-----
                WHEN   12
                    MOVE  "B12"        TO  MCP-WIDGET
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
      *----(04.08.00)--UPD-START---
               WHEN    "PREFNUM"
                   IF    ( SPA-GMN-MAX         =   ZERO )
                       MOVE    4                   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    3                   TO  SPA-GMN-CUR
                   END-IF
      *----(04.08.00)--UPD-END-----
               WHEN    "PAYKBN"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    12              TO  SPA-GMN-CUR
      *----(04.08.00)--ADD-START---
                WHEN   "CPHKNNUM"
                    MOVE    2              TO  SPA-GMN-CUR
      *----(04.08.00)--ADD-END-----
           END-EVALUATE
      *
           .
       50011-GMN-CUR-SET-EXT.
           EXIT.
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
                   MOVE    "選択番号が有りません"     TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "日付入力エラー"           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "有効期間が重複します"     TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "管理コードがちがいます"   TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "エラーがある項目があります"
                                                      TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "都道府県がちがいます"     TO  SPA-ERRMSG
      *----(04.08.00)--ADD-START---
               WHEN    "0008"
                 MOVE    "有効開始日＜有効終了日で入力して下さい"
                                                      TO  SPA-ERRMSG
      *----(04.08.00)--ADD-END-----
           END-EVALUATE
      *
           MOVE    SPACE                TO  VERR
           MOVE    SPA-ERRCD            TO  VERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  VERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "V01"                TO  SPA-MOTOPG
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
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-VIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-VIDMSG
      *
           EVALUATE    SPA-VIDCD
               WHEN    "1001"
                   MOVE    "保険番号マスタを複写して追加します。"
                                       TO  WRK-VIDMSG
               WHEN    "1002"
                   MOVE    "保険番号マスタの有効年月日を更新します"
                                       TO  WRK-VIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  VID1
           INITIALIZE                       VID1
           MOVE    SPA-VIDCD            TO  VID1-ID1CODE
           MOVE    WRK-VIDMSG           TO  VID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "V01"                TO  SPA-MOTOPG
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
      *    ＳＣＲＳＰＡ読込処理
      *****************************************************************
       800-SPA-INPUT-SEC               SECTION.
      *
           MOVE    "SCREEN"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-INPUT-SEC
      *
           MOVE    "COMMON"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-INPUT-SEC
      *
           .
       800-SPA-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＣＲＳＰＡ読込処理
      *****************************************************************
       8001-SPA-INPUT-SEC               SECTION.
      *
           PERFORM 900-SPATMP-KEY-SEL-SEC
           IF    ( WRK-FILEMEI     =   "SCREEN" )
               MOVE    SPATMP-DATA-REC
                                   TO  SPA-V01
           ELSE
               MOVE    SPATMP-DATA-REC
                                   TO  SPA-V01KYOUTU
           END-IF
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBDELETE-SEC
      *
           .
       8001-SPA-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＣＲＳＰＡ出力処理
      *****************************************************************
       800-SPA-OUTPUT-SEC              SECTION.
      *
           MOVE    "SCREEN"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-OUTPUT-SEC
      *
           MOVE    "COMMON"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-OUTPUT-SEC
      *
           .
       800-SPA-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＣＲＳＰＡ出力処理
      *****************************************************************
       8001-SPA-OUTPUT-SEC             SECTION.
      *
           INITIALIZE                  SPATMP-REC
      *
           PERFORM 9001-SPATMP-KEY-EDIT-SEC
      *
           IF    ( WRK-FILEMEI     =   "SCREEN" )
               MOVE    SPA-V01
                                   TO  SPATMP-DATA-REC
           ELSE
               MOVE    SPA-V01KYOUTU
                                   TO  SPATMP-DATA-REC
           END-IF
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBINSERT-SEC
      *
           .
       8001-SPA-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ-SEC       SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ERRCD(1:2)      =   "99"
                   MOVE    SPACE           TO  SKBN-RUN
               ELSE
                   IF      JOB-ERRCD(1:2)      =   "01"
                       MOVE    SPACE           TO  SKBN-RUN
                   ELSE
                       IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                              (JOB-ENDYMD      NOT =   SPACE)
                           MOVE    SPACE           TO  SKBN-RUN
                       ELSE
                           MOVE    "RUN"           TO  SKBN-RUN
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE            TO  SKBN-RUN
           END-IF
           .
      *
       820-JOBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理削除
      *****************************************************************
       820-JOBKANRI-DEL-SEC        SECTION.
      *
      *    ジョブ管理削除
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
      *
       820-JOBKANRI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理作成
      *****************************************************************
       820-JOBKANRI-JBS-SEC        SECTION.
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "保険番号情報問合せ"
                                   TO  JOB-SHELLMSG
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
      *
       820-JOBKANRI-JBS-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-HKNNUM-SELECT-SEC       SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNNUM
               PERFORM 900-HKNNUM-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-HKNNUM-FETCH-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-HKNNUM-CLOSE-SEC        SECTION.
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
       900-HKNNUM-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
      *
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
                   INITIALIZE                  SYS-1001-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       900-SPATMP-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SPATMP
      *
           INITIALIZE                  SPATMP-REC
      *
           PERFORM 9001-SPATMP-KEY-EDIT-SEC
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBSELECT-SEC
           IF    ( FLG-SPATMP      =   ZERO )
               MOVE    MCPDATA2-REC TO SPATMP-REC
           ELSE
               MOVE    1           TO  FLG-SPATMP
               INITIALIZE              SPATMP-REC
           END-IF
      *
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBCLOSECURSOR-SEC
      *
           .
       900-SPATMP-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索条件編集処理
      *****************************************************************
       9001-SPATMP-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  SPATMP-HOSPNUM
           MOVE    "V01"           TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID      TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI     TO  SPATMP-FILEMEI
           MOVE    1               TO  SPATMP-EDANUM
      *
           .
       9001-SPATMP-KEY-EDIT-EXT.
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
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       920-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           .
      *
       920-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       920-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       920-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       920-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＯＲＣＳＰＡＴＭＰ呼出処理
      *****************************************************************
       920-ORCDBSPATMP-SEC                SECTION.
      *
           CALL    "ORCDBSPATMP"       USING
                                       MCPAREA
                                       MCPDATA2-REC
                                       SPA-AREA
      *
           .
      *
       920-ORCDBSPATMP-EXT.
           EXIT.
