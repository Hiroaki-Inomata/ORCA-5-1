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
       PROGRAM-ID.                               ORCGZ02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタコメント設置画面（Ｚ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.01.01    MCC-藤原     01/09/26  JOINの修正      
      *  01.01.02    MCC-藤原     01/11/26  項目をコンボボックスに変更
      *  01.02.04    MCC-多々納   02/07/18  服用方法画面追加
      *  01.02.05    NACL-多々納  02/09/03  新規の時付加情報が存在した時更新とする
      *  01.02.06    NACL-多々納  02/10/09  半角空白→全角空白追加
      *  02.00.00    NACL-多々納  03/06/13  一括入力へ変更
      *  02.00.01    NACL-多々納  05/12/09  登録コードチェック追加
      *                                     MONFUNC対応他
      *  03.04.00    NACL-多々納  07/01/24  H19/4/1から自費総額設定追加
      *  04.02.00    NACL-多々納  08/03/07  病理診断判定追加
      *  04.03.00    NACL-多々納  08/09/19  自賠責集計等取扱追加
      *  04.05.00    NACL-多々納  09/09/19  '0086'コメント追加
      *  04.06.00    NACL-多々納  11/02/23  コメント変更チェック追加
      *  04.06.00    NACL-多々納  12/08/16  期間変更警告追加
      *  04.07.00    NACL-多々納  12/08/21  ユーザ登録複写機能追加
      *  04.08.00    NACL-多々納  16/01/20  金額チェック追加他
      *  04.08.00    NACL-多々納  16/01/20  金額チェック追加他
      *  04.08.00    NACL-多々納  17/02/XX  薬剤対応（単位追加）
      *  05.01.00    NACL-多々納  17/02/XX  点数マスタ分離対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
       01  SPA-Z02.
      *    第二画面ＳＰＡエリア
           03  SPA-GMN-AREA.
               05  SPA-GMN-SRYCDH              PIC X(09).
               05  SPA-GMN-NAMEH               PIC X(100).
               05  SPA-GMN-YUKOSTYMDH          PIC X(09).
               05  SPA-GMN-YUKOEDYMDH          PIC X(09).
               05  SPA-GMN-YUKOSTYMD           PIC X(10).
               05  SPA-GMN-YUKOEDYMD           PIC X(10).
               05  SPA-GMN-KANANAME            PIC X(100).
               05  SPA-GMN-NAME                PIC X(100).
               05  SPA-GMN-FORMALNAME          PIC X(200).
               05  SPA-GMN-COLUMN1             PIC 9(3).
               05  SPA-GMN-COLUMN2             PIC 9(3).
               05  SPA-GMN-COLUMN3             PIC 9(3).
               05  SPA-GMN-COLUMN4             PIC 9(3).
               05  SPA-GMN-DIGIT1              PIC 9(3).
               05  SPA-GMN-DIGIT2              PIC 9(3).
               05  SPA-GMN-DIGIT3              PIC 9(3).
               05  SPA-GMN-DIGIT4              PIC 9(3).
               05  SPA-GMN-TEN                 PIC 9(09).
      *********05  SPA-GMN-HKNTEKKBN           PIC 9(01).
               05  SPA-GMN-HKNTEKKBN-G.
                   11  SPA-GMN-HKNTEKKBN       PIC X(01).
                   11  SPA-GMN-F1              PIC X(01).
                   11  SPA-GMN-HKNTEKKBNMEI    PIC X(10).
               05  SPA-GMN-HKNTEKKBNLST        OCCURS  3.
                   09  SPA-GMN-HKNTEKKBNL      PIC X(01).
                   09  SPA-GMN-F1L             PIC X(01).
                   09  SPA-GMN-HKNTEKKBNMEIL   PIC X(10).
               05  SPA-GMN-HKNTEKKBN-MAX       PIC 9(02).    
               05  SPA-GMN-GAITENTTLKBN        PIC 9(03).
               05  SPA-GMN-NYUTENTTLKBN        PIC 9(03).
      *********05  SPA-GMN-BUIKBN              PIC 9(01).
               05  SPA-GMN-BUIKBN-G.
                   11  SPA-GMN-BUIKBN          PIC X(01).
                   11  SPA-GMN-F1              PIC X(01).
                   11  SPA-GMN-BUIKBNMEI       PIC X(06).
               05  SPA-GMN-BUIKBNLST-G.
                 07  SPA-GMN-BUIKBNLST           OCCURS  8.
                   09  SPA-GMN-BUIKBNL         PIC X(01).
                   09  SPA-GMN-F1L             PIC X(01).
                   09  SPA-GMN-BUIKBNMEIL      PIC X(06).
               05  SPA-GMN-BUIKBN-MAX          PIC 9(02).    
      *****    05  SPA-GMN-INPCHKKBN           PIC 9(01).
      *        自賠責集計等取扱（外来）
               05  SPA-GMN-GAIJIBAIKBN-G.
                   07  SPA-GMN-GAIJIBAIKBN     PIC X(01).
                   07  SPA-GMN-GAIF1           PIC X(01).
                   07  SPA-GMN-GAIJIBAIKBN-MEI PIC X(38).
               05  SPA-GMN-GAIJIBAIKBN-LST     OCCURS   10
                                               PIC X(40).
               05  SPA-GMN-GAIJIBAI-MAX        PIC 9(02).
      *        自賠責集計等取扱（入院）
               05  SPA-GMN-NYUJIBAIKBN-G.
                   07  SPA-GMN-NYUJIBAIKBN     PIC X(01).
                   07  SPA-GMN-NYUF1           PIC X(01).
                   07  SPA-GMN-NYUJIBAIKBN-MEI PIC X(38).
               05  SPA-GMN-NYUJIBAIKBN-LST     OCCURS   10
                                               PIC X(40).
               05  SPA-GMN-NYUJIBAI-MAX        PIC 9(02).
      *        複写元コード
               05  SPA-GMN-COPYCD              PIC X(09).
      *
               05  SPA-GMN-JIHINAME1           PIC X(30).
               05  SPA-GMN-JIHINAME2           PIC X(30).
      *H29.2
      *        単位コード
               05  SPA-GMN-TANICD-LST          OCCURS  200.
                   07  SPA-GMN-TANICDL         PIC X(03).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-TANICDMEIL      PIC X(24).
               05  SPA-GMN-TANICD-MAX          PIC 9(04).
               05  SPA-GMN-TANICD-G.
                   07  SPA-GMN-TANICD          PIC X(03).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-TANICDMEI       PIC X(24).
      *        薬剤区分
               05  SPA-GMN-YKZKBN-LST          OCCURS  5.
                   07  SPA-GMN-YKZKBNL         PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-YKZKBNMEIL      PIC X(10).
               05  SPA-GMN-YKZKBNMAX           PIC 9(02).
               05  SPA-GMN-YKZKBN-G.
                   07  SPA-GMN-YKZKBN          PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-YKZKBNMEI       PIC X(10).
      *
               05  SPA-GMN-CUR                 PIC 9(02).
      *
      *    第二画面内容
           03  SPA-NAI-AREA.
               05  SPA-NAI-YUKOSTYMD           PIC X(08).
               05  SPA-NAI-YUKOEDYMD           PIC X(08).
      *
               05  SPA-SRYCDCHK                PIC 9(01).
      *
               05  SPA-JIHIKBN                 PIC 9(01).
      *        ユーザ登録
               05  SPA-NAI-USERCD              PIC 9(01).
      *
               05  SPA-NAI-KEIFLG1             PIC 9(01).
               05  SPA-NAI-KEIFLG2             PIC 9(01).
               05  SPA-NAI-KEIFLG3             PIC 9(01).
      *H29.2
      *        薬剤対象
               05  SPA-NAI-YKZKBN              PIC X(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
           03  FLG-SYSKANRI                      PIC 9(01).
      *
           03  FLG-ERR                           PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-END                           PIC 9(01).
           03  FLG-TOUROKU                       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDXX                              PIC 9(02).
           03  IDXY                              PIC 9(02).
           03  IDXZ                              PIC 9(02).
      *
           03  IDX1                              PIC 9(04).
           03  IDX2                              PIC 9(04).
           03  IDX-H                             PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-ZIDMSG                        PIC X(70).
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 200.
      *
           03  WRK-YUKOEDYMD                     PIC X(08).
      *
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *
           03  WRK-JIHIKBN         PIC 9(01).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *v47
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-CD-MCNT             PIC 9(02).
           03  FLG-KANJI               PIC 9(01).
      *    自費名称編集
           03  WRK-KBNCD.
               05  WRK-KBNCD1          PIC X(01).
               05  WRK-KBNCD2          PIC 9(01).
           03  WRK-JIHINAME            PIC X(30).
      *
      *    更新テーブル名称
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
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
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *----(01.02.03) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.02.03) LINE ADD END   ----------------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    点数附加情報更新内容
      **** COPY    "CPORCGZS02.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA102SCRAREA.INC".
      *    COPY    "Z01.INC".
      *    COPY    "Z02.INC".
      *    COPY    "Z03.INC".
      *    COPY    "Z04.INC".
      *    COPY    "Z05.INC".
      *    COPY    "Z06.INC".
      *    COPY    "Z07.INC".
      *    COPY    "Z08.INC".
      *    COPY    "Z90.INC".
      *    COPY    "Z91.INC".
      *    COPY    "Z97.INC".
      *    COPY    "Z98.INC".
      *    COPY    "Z99.INC".
      *    COPY    "ZERR.INC".
      *    COPY    "ZID1.INC".
      *    COPY    "ZID2.INC".
      *    COPY    "ZID3.INC".
      *    COPY    "Z100.INC".
      *    COPY    "CPAPIMOD.INC".
      *    COPY    "CPAPIMODV2.INC".
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
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z02
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z02                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                    PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z02"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *    更新処理
               WHEN    "Z99"
                   MOVE    1                   TO  SPA-GMN-CUR
      *
      *    検索画面より
               WHEN    "Z98"
                   PERFORM 320-Z98SPASET-SEC
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *    点数マスタ該当データ読み出し
           INITIALIZE                        SPA-GMN-AREA
           INITIALIZE                        SPA-NAI-AREA
      *
           MOVE  SPA-Z01-SRYCD               TO  SPA-GMN-SRYCDH
      *
           MOVE  SPA-Z01-YUKOSTYMD           TO  SPA-NAI-YUKOSTYMD
           IF  SPA-Z01-SYORIKBN              =  2
               MOVE  SPA-Z01-YUKOSTYMDH      TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE  SPA-Z01-YUKOSTYMDH          TO  SPA-GMN-YUKOSTYMD
      *
           MOVE  SPA-Z01-YUKOEDYMD           TO  SPA-NAI-YUKOEDYMD
           IF  SPA-Z01-SYORIKBN              =  2
                MOVE  SPA-Z01-YUKOEDYMDH     TO  SPA-GMN-YUKOEDYMDH
           END-IF
           MOVE  SPA-Z01-YUKOEDYMDH          TO  SPA-GMN-YUKOEDYMD
      *
           MOVE    "0"                   TO  SPA-GMN-HKNTEKKBNL    (1)
           MOVE    SPACE                 TO  SPA-GMN-HKNTEKKBNMEIL (1)
           MOVE    "1"                   TO  SPA-GMN-HKNTEKKBNL    (2)
           MOVE    "保険適用"            TO  SPA-GMN-HKNTEKKBNMEIL (2)
           MOVE    "2"                   TO  SPA-GMN-HKNTEKKBNL    (3) 
           MOVE    "保険適用外"          TO  SPA-GMN-HKNTEKKBNMEIL (3)
           MOVE    3                     TO  SPA-GMN-HKNTEKKBN-MAX
      *
           MOVE    "1"                   TO  SPA-GMN-BUIKBNL    (1)
           MOVE    "頭部"                TO  SPA-GMN-BUIKBNMEIL (1)
           MOVE    "2"                   TO  SPA-GMN-BUIKBNL    (2)
           MOVE    "躯幹"                TO  SPA-GMN-BUIKBNMEIL (2)
           MOVE    "3"                   TO  SPA-GMN-BUIKBNL    (3) 
           MOVE    "四肢"                TO  SPA-GMN-BUIKBNMEIL (3)
           MOVE    "5"                   TO  SPA-GMN-BUIKBNL    (4)
           MOVE    "胸部"                TO  SPA-GMN-BUIKBNMEIL (4)
           MOVE    "6"                   TO  SPA-GMN-BUIKBNL    (5)
           MOVE    "腹部"                TO  SPA-GMN-BUIKBNMEIL (5)
           MOVE    "7"                   TO  SPA-GMN-BUIKBNL    (6) 
           MOVE    "脊椎"                TO  SPA-GMN-BUIKBNMEIL (6)
           MOVE    "8"                   TO  SPA-GMN-BUIKBNL    (7)
           MOVE    "消化管"              TO  SPA-GMN-BUIKBNMEIL (7)
           MOVE    "0"                   TO  SPA-GMN-BUIKBNL    (8)
           MOVE    "その他"              TO  SPA-GMN-BUIKBNMEIL (8)
           MOVE    8                     TO  SPA-GMN-BUIKBN-MAX
      *
      *    自賠責
           IF      SPA-Z01-SRYCD(1:5)  =   "09593"
      *        自賠責集計等取扱（外来）
               MOVE    "1 初診"            TO  SPA-GMN-GAIJIBAIKBN-LST
                                                                   (1)
               MOVE    "2 再診"            TO  SPA-GMN-GAIJIBAIKBN-LST
                                                                   (2)
               MOVE    "3 指導管理"        TO  SPA-GMN-GAIJIBAIKBN-LST
                                                                   (3)
               MOVE    "4 救急医療管理加算"  
                                           TO  SPA-GMN-GAIJIBAIKBN-LST
                                                                   (4)
               MOVE    "5 その他"          TO  SPA-GMN-GAIJIBAIKBN-LST
                                                                   (5)
               MOVE    5                   TO  SPA-GMN-GAIJIBAI-MAX
      *        自賠責集計等取扱（入院）
               MOVE    "1 初診"            TO  SPA-GMN-NYUJIBAIKBN-LST
                                                                   (1)
               MOVE    "4 救急医療管理加算"  
                                           TO  SPA-GMN-NYUJIBAIKBN-LST
                                                                   (2)
               MOVE    "5 その他"          TO  SPA-GMN-NYUJIBAIKBN-LST
                                                                   (3)
               MOVE    3                   TO  SPA-GMN-NYUJIBAI-MAX
           ELSE
               MOVE    0                   TO  SPA-GMN-GAIJIBAI-MAX
               MOVE    0                   TO  SPA-GMN-NYUJIBAI-MAX
           END-IF
      *
      *    自費コードの時、金額種別表示とする
           MOVE    ZERO                TO  SPA-JIHIKBN
           IF      (SPA-Z01-SRYCD(1:3) =   "095"
                                       OR  "096" )
               MOVE    1               TO  SPA-JIHIKBN
               IF     (SPA-NAI-YUKOSTYMD   >=  "20070401")  AND
                      (SPA-Z01-SRYCD(1:3)  =   "096"     )
                   MOVE    2                 TO  SPA-JIHIKBN
               END-IF
           END-IF
           IF      SPA-JIHIKBN     NOT =   ZERO
               INITIALIZE                  SPA-GMN-BUIKBNLST-G
               MOVE    "0"                 TO  SPA-GMN-BUIKBNL    (1)
               MOVE    "税抜き"            TO  SPA-GMN-BUIKBNMEIL (1)
               IF      SPA-JIHIKBN         =   2
                   MOVE    "4"             TO  SPA-GMN-BUIKBNL    (2)
                   MOVE    "税込み"        TO  SPA-GMN-BUIKBNMEIL (2)
                   MOVE    2               TO  SPA-GMN-BUIKBN-MAX
               ELSE
                   MOVE    1               TO  SPA-GMN-BUIKBN-MAX
               END-IF
           END-IF
      *v47
      *    ユーザ登録判定
           IF      (SPA-Z01-SRYCD(1:1) =   "8"  )
               MOVE    ZERO            TO  SPA-NAI-USERCD
           ELSE
               MOVE    1               TO  SPA-NAI-USERCD
           END-IF
      *H29.2
      *    薬剤対応
           MOVE    SPACE               TO  SPA-NAI-YKZKBN
           IF      (SPA-Z01-SRYCD(1:3) =   "095"
                                       OR  "096" )
               IF      (SPA-Z01-SRYCD(4:2) >=  "21"
                                       AND <=  "27"  )
                   MOVE    "1"             TO  SPA-NAI-YKZKBN
              END-IF
           END-IF
           IF      SPA-NAI-YKZKBN      =   "1"
      *        薬剤区分
               MOVE    "1"                 TO  SPA-GMN-YKZKBNL(1)
               MOVE    "内用薬"            TO  SPA-GMN-YKZKBNMEIL(1)
               MOVE    "6"                 TO  SPA-GMN-YKZKBNL(2)
               MOVE    "外用薬"            TO  SPA-GMN-YKZKBNMEIL(2)
               MOVE    2                   TO  SPA-GMN-YKZKBNMAX
           END-IF
      *    単位コード
           MOVE    ZERO                TO  SPA-GMN-TANICD-MAX
      *    IF      SPA-Z01-SRYCD(1:3)  =   "095"
      *                                OR  "096"
           IF      SPA-NAI-YKZKBN      =   "1"
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       MTANI-MAX
                   MOVE    MTANI-TANICD  (IDXX)
                                       TO  SPA-GMN-TANICDL   (IDXX)
                   MOVE    MTANI-TANINAME(IDXX)
                                       TO  SPA-GMN-TANICDMEIL(IDXX)
                   MOVE    IDXX        TO  SPA-GMN-TANICD-MAX
               END-PERFORM
           END-IF
      *
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           IF      SPA-Z01-SELNUM      =   ZERO
               MOVE    ALL "9"             TO  TNS-YUKOSTYMD
               MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           ELSE
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *
               MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
      *
               MOVE    TNS-NAME            TO  SPA-GMN-NAME
      *
               MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
                   MOVE  TNS-SSTKIJUNCD(1)   TO SPA-GMN-COLUMN1
                   MOVE  TNS-SSTKIJUNCD(2)   TO SPA-GMN-DIGIT1
                   MOVE  TNS-SSTKIJUNCD(3)   TO SPA-GMN-COLUMN2
                   MOVE  TNS-SSTKIJUNCD(4)   TO SPA-GMN-DIGIT2
                   MOVE  TNS-SSTKIJUNCD(5)   TO SPA-GMN-COLUMN3
                   MOVE  TNS-SSTKIJUNCD(6)   TO SPA-GMN-DIGIT3
                   MOVE  TNS-SSTKIJUNCD(7)   TO SPA-GMN-COLUMN4
                   MOVE  TNS-SSTKIJUNCD(8)   TO SPA-GMN-DIGIT4
      *
                   MOVE  TNS-TEN             TO  SPA-GMN-TEN
                   MOVE  TNS-HKNTEKKBN       TO  SPA-GMN-HKNTEKKBN
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   SPA-GMN-HKNTEKKBN-MAX
                       IF      SPA-GMN-HKNTEKKBN 
                                       =   SPA-GMN-HKNTEKKBNL (IDXX)
                           MOVE    SPA-GMN-HKNTEKKBNLST (IDXX)
                                             TO  SPA-GMN-HKNTEKKBN-G
                           MOVE    SPA-GMN-HKNTEKKBN-MAX
                                             TO  IDXX
                       END-IF
                   END-PERFORM
                   MOVE  TNS-GAITENTTLKBN    TO  SPA-GMN-GAITENTTLKBN
                   MOVE  TNS-NYUTENTTLKBN    TO  SPA-GMN-NYUTENTTLKBN
                   MOVE  TNS-BUIKBN          TO  SPA-GMN-BUIKBN  
      *            自費コードの時、点数識別とする
                   IF      SPA-JIHIKBN       NOT =   ZERO
                       MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-BUIKBN
                   END-IF
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   SPA-GMN-BUIKBN-MAX
                       IF      SPA-GMN-BUIKBN 
                                       =   SPA-GMN-BUIKBNL (IDXX)
                           MOVE    SPA-GMN-BUIKBNLST (IDXX)
                                             TO  SPA-GMN-BUIKBN-G
                           MOVE    SPA-GMN-BUIKBN-MAX
                                             TO  IDXX
                       END-IF
                   END-PERFORM
      *
      ************ MOVE  TNSP-INPCHKKBN      TO  SPA-GMN-INPCHKKBN  
      *        自賠責集計等取扱
               MOVE    TNS-NAMECHGKBN      TO  SPA-GMN-GAIJIBAIKBN
               MOVE    TNS-KANANAMECHGKBN  TO  SPA-GMN-NYUJIBAIKBN
      *H29.2
      *        薬剤区分
               IF      SPA-NAI-YKZKBN      =   "1"
                   MOVE  TNS-YKZKBN        TO  SPA-GMN-YKZKBN
               END-IF
               MOVE    TNS-TANICD          TO  SPA-GMN-TANICD
           ELSE
      *        初期設定
               MOVE    SPA-GMN-HKNTEKKBNLST (01)
                                           TO  SPA-GMN-HKNTEKKBN-G
      *        自費コードは、２とする
               IF      SPA-Z01-SRYCD(1:3)  =   "095"
                                           OR  "096"
                   MOVE    SPA-GMN-HKNTEKKBNLST (03)
                                           TO  SPA-GMN-HKNTEKKBN-G
               END-IF
      *        部位はその他とする
               MOVE    SPA-GMN-BUIKBNLST (8)
                                           TO  SPA-GMN-BUIKBN-G
               IF      SPA-JIHIKBN     NOT =   ZERO
                   MOVE    SPA-GMN-BUIKBNLST (1)
                                           TO  SPA-GMN-BUIKBN-G
               END-IF
           END-IF
           IF      SPA-Z01-SRYCD(1:5)  =   "09593"
      *        自賠責集計等取扱
               IF      SPA-GMN-GAIJIBAIKBN     =   ZERO
                                               OR  SPACE
                   MOVE    "5"                 TO  SPA-GMN-GAIJIBAIKBN
               END-IF
               IF      SPA-GMN-NYUJIBAIKBN     =   ZERO
                                               OR  SPACE
                   MOVE    "5"                 TO  SPA-GMN-NYUJIBAIKBN
               END-IF
               PERFORM 3010-JIBAIKBN-HEN-SEC
           ELSE
               MOVE    SPACE               TO  SPA-GMN-GAIJIBAIKBN
               MOVE    SPACE               TO  SPA-GMN-NYUJIBAIKBN
           END-IF
      *
      *!! 
      *    自費集計先
           PERFORM 3012-JIHINAME-HEN-SEC
      *
      *    薬剤区分
           IF      SPA-NAI-YKZKBN      =   "1"
               IF      SPA-GMN-YKZKBN      =   ZERO
                   MOVE    SPACE               TO  SPA-GMN-YKZKBN-G
               END-IF
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       SPA-GMN-YKZKBNMAX
                   IF      SPA-GMN-YKZKBN      =   SPA-GMN-YKZKBNL
                                                               (IDXX)
                       MOVE    SPA-GMN-YKZKBN-LST (IDXX)
                                                   TO  SPA-GMN-YKZKBN-G
                       MOVE    SPA-GMN-YKZKBNMAX   TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *    単位コード
           IF      SPA-GMN-TANICD      NOT =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       SPA-GMN-TANICD-MAX
                   IF      SPA-GMN-TANICD
                                       =   SPA-GMN-TANICDL(IDXX)
                       MOVE    SPA-GMN-TANICD-LST (IDXX)
                                           TO  SPA-GMN-TANICD-G
                       MOVE    SPA-GMN-TANICD-MAX
                                           TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    点数マスタ付加情報検索
      *    IF      FLG-TENSU           =   ZERO
      *        MOVE    SPACE               TO  TENSUPLUS-REC
      *        MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *        MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
      *        MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
      *        MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *        MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *        MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        PERFORM 910-DBSELECT-SEC
      *        IF      MCP-RC               =   ZERO
      *            MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *            MOVE    "key"               TO  MCP-PATHNAME
      *            PERFORM 990-TENSUPLUS-READ-SEC
      *            IF      FLG-TENSUPLUS       =   ZERO
      *                MOVE    TNSP-INPCHKKBN  TO  SPA-GMN-INPCHKKBN
      *            END-IF
      *        END-IF
      *        MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        PERFORM 990-DBLOSEC-SEC
      *****END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    自賠責集計等取扱編集処理
      *****************************************************************
       3010-JIBAIKBN-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-GAIJIBAI-MAX
               IF      SPA-GMN-GAIJIBAIKBN
                                       =   SPA-GMN-GAIJIBAIKBN-LST
                                                   (IDX)(1:1)
                   MOVE    SPA-GMN-GAIJIBAIKBN-LST (IDX)
                                           TO  SPA-GMN-GAIJIBAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-GAIJIBAIKBN-LST (5)
                                           TO  SPA-GMN-GAIJIBAIKBN-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYUJIBAI-MAX
               IF      SPA-GMN-NYUJIBAIKBN
                                       =   SPA-GMN-NYUJIBAIKBN-LST
                                                   (IDX)(1:1)
                   MOVE    SPA-GMN-NYUJIBAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUJIBAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-NYUJIBAIKBN-LST (3)
                                           TO  SPA-GMN-NYUJIBAIKBN-G
           END-IF
      *
           .
       3010-JIBAIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費集計先名称処理
      *****************************************************************
       3012-JIHINAME-HEN-SEC          SECTION.
      *
           IF      (SPA-Z01-SRYCD(1:5) =   "09500"
                                       OR  "09600"
                                       OR  "09591"
                                       OR  "09592"
                                       OR  "09593"
                                       OR  "09594"
                                       OR  "09691"
                                       OR  "09692"
                                       OR  "09693"
                                       OR  "09694" )
               CONTINUE
           ELSE
               GO      TO      3012-JIHINAME-HEN-EXT
           END-IF
      *
      *    外来
           IF     (SPA-GMN-GAITENTTLKBN    =   ZERO )
              OR  (SPA-GMN-GAITENTTLKBN    >   10   )
               MOVE    SPACE                   TO  SPA-GMN-JIHINAME1
           ELSE
               MOVE    "2"                     TO  WRK-KBNCD1
               MOVE    SPA-GMN-GAITENTTLKBN    TO  WRK-KBNCD2
               PERFORM 30121-JIHINAME-HENSYU-SEC
               MOVE    WRK-JIHINAME            TO  SPA-GMN-JIHINAME1
           END-IF
      *    入院
           IF     (SPA-GMN-NYUTENTTLKBN    =   ZERO )
              OR  (SPA-GMN-NYUTENTTLKBN    >   10   )
               MOVE    SPACE                   TO  SPA-GMN-JIHINAME2
           ELSE
               MOVE    "1"                     TO  WRK-KBNCD1
               MOVE    SPA-GMN-NYUTENTTLKBN    TO  WRK-KBNCD2
               PERFORM 30121-JIHINAME-HENSYU-SEC
               MOVE    WRK-JIHINAME            TO  SPA-GMN-JIHINAME2
           END-IF
      *
           .
       3012-JIHINAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費集計先名称処理
      *****************************************************************
       30121-JIHINAME-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1013-KBNCD
      *
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 960-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    SPACE               TO  WRK-JIHINAME
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
      *        システム日付で有効
               IF     (SPA-NAI-YUKOSTYMD   <=  SPA-SYSYMD )  AND
                      (SPA-NAI-YUKOEDYMD   >=  SPA-SYSYMD )
                   IF     (SYS-1013-STYUKYMD   <=  SPA-SYSYMD) AND
                          (SYS-1013-EDYUKYMD   >=  SPA-SYSYMD)
                       MOVE    SYS-1013-JIHINAME   TO  WRK-JIHINAME
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
      *            有効期間外は終了日が有効期間内
                   IF     (SYS-1013-STYUKYMD   <=  SPA-NAI-YUKOEDYMD)
                   AND    (SYS-1013-EDYUKYMD   >=  SPA-NAI-YUKOEDYMD)
                       MOVE    SYS-1013-JIHINAME   TO  WRK-JIHINAME
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 960-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           .
       30121-JIHINAME-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
      *        複写
               MOVE    SPA-Z98-SRYCD       TO  SPA-GMN-COPYCD
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-GMN-COPYCD
           END-IF 
           .
       320-Z98SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
      *        削除
               WHEN    "0001"
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG1
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        更新
               WHEN    "0002"
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4121-TOUROKU-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG2
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD      
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE  "CHANGE"      TO  MCP-PUTTYPE
                   PERFORM  401-BACK-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        入力コード設定
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-INPUTCD-SEC
      *v47
      *        前回複写
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM  406-ZENCOPY-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE       TO  WRK-INPUTCD
                   PERFORM 409-KENSAKU-SEC
      *
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        複写先コード
               WHEN    "ACTIVATE"       ALSO    "COPYCD"
                   PERFORM 502-COPYCD-CHK-SEC
      *        一括入力分
               WHEN    OTHER
                   PERFORM 2001-ENTRY-SYORI-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       2001-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       2001-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
      *        マスタ更新可
               CONTINUE
           ELSE
               GO   TO  4101-GMN-SPA-SET-EXT
           END-IF
      *
           MOVE    Z02-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z02-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
           MOVE    Z02-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z02-NAME            TO  SPA-GMN-NAME
           MOVE    Z02-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
           MOVE    Z02-COLUMN1         TO  SPA-GMN-COLUMN1
           MOVE    Z02-DIGIT1          TO  SPA-GMN-DIGIT1
           MOVE    Z02-COLUMN2         TO  SPA-GMN-COLUMN2
           MOVE    Z02-DIGIT2          TO  SPA-GMN-DIGIT2
           MOVE    Z02-COLUMN3         TO  SPA-GMN-COLUMN3
           MOVE    Z02-DIGIT3          TO  SPA-GMN-DIGIT3
           MOVE    Z02-COLUMN4         TO  SPA-GMN-COLUMN4
           MOVE    Z02-DIGIT4          TO  SPA-GMN-DIGIT4
           MOVE    Z02-TEN             TO  SPA-GMN-TEN
           MOVE    Z02-HKNTEKKBN       TO  SPA-GMN-HKNTEKKBN-G
           MOVE    Z02-GAITENTTLKBN    TO  SPA-GMN-GAITENTTLKBN
           MOVE    Z02-NYUTENTTLKBN    TO  SPA-GMN-NYUTENTTLKBN
           MOVE    Z02-BUIKBN          TO  SPA-GMN-BUIKBN-G
     ***** MOVE    Z02-INPCHKKBN       TO  SPA-GMN-INPCHKKBN
      *
           MOVE    Z02-GAIJIBAIKBN     TO  SPA-GMN-GAIJIBAIKBN-G
           MOVE    Z02-NYUJIBAIKBN     TO  SPA-GMN-NYUJIBAIKBN-G
      *
           MOVE    Z02-TANICD          TO  SPA-GMN-TANICD-G
           MOVE    Z02-YKZKBN          TO  SPA-GMN-YKZKBN-G
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    有効開始年月日チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 501-YUKOSTYMDCHK-SEC
           END-IF
      *    有効開始年月日チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 502-YUKOEDYMDCHK-SEC
           END-IF
      *    カナ名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 503-KANANAME-CHK-SEC
           END-IF
      *    漢字名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 504-NAME-CHK-SEC
           END-IF
      *    通称名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 505-FORMALNAME-CHK-SEC
           END-IF
      *    カラム位置チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-DIGIT-CHK-SEC
           END-IF
      *    保険適用チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-HKNTEKKBN-CHK-SEC
           END-IF
      *    金額チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-KINGAKU-CHK-SEC
           END-IF
      *H29.2
      *    単位コードチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-TANICD-CHK-SEC
           END-IF
      *H29.2
      *    薬剤区分チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-YKZKBN-CHK-SEC
           END-IF
      *
      *    点数欄集計先チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-GAITENTTLKBN-CHK-SEC
           END-IF
      *    部位チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 507-BUIKBN-CHK-SEC
           END-IF
      *    自費の時、有効開始日により金額区分を修正
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-JIHIKBN     NOT =   ZERO
                   PERFORM 508-JIHI-KBN-SEC
               END-IF
           END-IF
      *    自賠責集計等取扱区分
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SRYCD(1:5)  =   "09593"
                   PERFORM 508-JIBAIKBN-CHK-SEC
               END-IF
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC            SECTION.
      *
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       501-YUKOSTYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       502-YUKOEDYMDCHK-SEC                      SECTION.
      *
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "9999999"           TO  SPA-GMN-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ名称チェック処理
      *****************************************************************
       503-KANANAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    漢字名称チェック処理
      *****************************************************************
       504-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       504-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通称名称チェック処理
      *****************************************************************
       505-FORMALNAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-FORMALNAME  TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       505-FORMALNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額チェック処理
      *****************************************************************
       506-KINGAKU-CHK-SEC          SECTION.
      *
      *    自費コード以外はゼロ
           IF      SPA-JIHIKBN         =   ZERO
               IF      SPA-GMN-TEN     NOT =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    14                  TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
      *H30.5
      *    自費金額は７桁まで
               IF      SPA-GMN-TEN         >   9999999
                   MOVE    "0016"              TO  SPA-ERRCD
                   MOVE    14                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       506-KINGAKU-CHK-EXT.
           EXIT.
      *H29.2
      *****************************************************************
      *    単位コードチェック処理
      *****************************************************************
       506-TANICD-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           IF      SPA-GMN-TANICD      =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TANICD-G
               MOVE    1               TO  FLG-OK
           END-IF
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL  (IDXX    >       SPA-GMN-TANICD-MAX)
                       OR (FLG-OK  =   1    )
               IF      SPA-GMN-TANICD
                                       =   SPA-GMN-TANICDL(IDXX)
                   MOVE    SPA-GMN-TANICD-LST (IDXX)
                                           TO  SPA-GMN-TANICD-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    23                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-TANICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤区分チェック処理
      *****************************************************************
       506-YKZKBN-CHK-SEC          SECTION.
      *
           IF      SPA-NAI-YKZKBN      =   "1"
               MOVE    ZERO                TO  FLG-OK
               IF      SPA-GMN-YKZKBN      =   ZERO
                                           OR  SPACE
                   MOVE    SPACE               TO  SPA-GMN-YKZKBN-G
                   MOVE    1                   TO  FLG-OK
               END-IF
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL  (IDXX    >       SPA-GMN-YKZKBNMAX)
                           OR (FLG-OK  =   1   )
                   IF      SPA-GMN-YKZKBN      =   SPA-GMN-YKZKBNL
                                                               (IDXX)
                       MOVE    SPA-GMN-YKZKBN-LST (IDXX)
                                                   TO  SPA-GMN-YKZKBN-G
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "0014"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN-CUR
               END-IF
      *        金額＝ゼロはエラー
               IF     (SPA-GMN-YKZKBN  NOT =   SPACE )
                AND   (SPA-ERRCD           =   SPACE )
                   IF      SPA-GMN-TEN         =   ZERO
                       MOVE    "0015"              TO  SPA-ERRCD
                       MOVE    24                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-YKZKBN-G
           END-IF
           .
       506-YKZKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適用チェック処理
      *****************************************************************
       506-HKNTEKKBN-CHK-SEC          SECTION.
      *
      *    自費保険は保険適用 ２とする
           IF      SPA-JIHIKBN     NOT =   ZERO
               MOVE    SPA-GMN-HKNTEKKBNLST (03)
                                           TO  SPA-GMN-HKNTEKKBN-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >   SPA-GMN-HKNTEKKBN-MAX
               IF      SPA-GMN-HKNTEKKBN 
                                       =   SPA-GMN-HKNTEKKBNL (IDXX)
                   MOVE    SPA-GMN-HKNTEKKBNLST (IDXX)
                                               TO  SPA-GMN-HKNTEKKBN-G
                   MOVE    SPA-GMN-HKNTEKKBN-MAX
                                               TO  IDXX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
           .
       506-HKNTEKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数欄集計先チェック処理
      *****************************************************************
       506-GAITENTTLKBN-CHK-SEC          SECTION.
      *
           PERFORM 3012-JIHINAME-HEN-SEC
           .
      *
       506-GAITENTTLKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    部位チェック処理
      *****************************************************************
       507-BUIKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >   SPA-GMN-BUIKBN-MAX
               IF      SPA-GMN-BUIKBN      =   SPA-GMN-BUIKBNL (IDXX)
                   MOVE    SPA-GMN-BUIKBNLST (IDXX)
                                               TO  SPA-GMN-BUIKBN-G
                   MOVE    SPA-GMN-BUIKBN-MAX  TO  IDXX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               IF      SPA-GMN-BUIKBN-MAX  <   3
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       507-BUIKBN-CHK-EXT.
           EXIT.
      ****************************************************************
      *    有効開始日により金額区分処理
      ****************************************************************
       508-JIHI-KBN-SEC      SECTION.
      *
           MOVE    ZERO            TO  WRK-JIHIKBN
           IF     (SPA-NAI-YUKOSTYMD   >=  "20070401")  AND
                  (SPA-Z01-SRYCD(1:3)  =   "096"     )
               MOVE    2                   TO  WRK-JIHIKBN
           ELSE
               MOVE    1                   TO  WRK-JIHIKBN
           END-IF
           IF      SPA-JIHIKBN     NOT =   WRK-JIHIKBN
               MOVE    WRK-JIHIKBN         TO  SPA-JIHIKBN
               INITIALIZE                  SPA-GMN-BUIKBNLST-G
               MOVE    "0"                 TO  SPA-GMN-BUIKBNL    (1)
               MOVE    "税抜き"            TO  SPA-GMN-BUIKBNMEIL (1)
               IF      SPA-JIHIKBN         =   2
                   MOVE    "4"             TO  SPA-GMN-BUIKBNL    (2)
                   MOVE    "税込み"        TO  SPA-GMN-BUIKBNMEIL (2)
                   MOVE    2               TO  SPA-GMN-BUIKBN-MAX
               ELSE
                   MOVE    1               TO  SPA-GMN-BUIKBN-MAX
               END-IF
           END-IF
           .
       508-JIHI-KBN-EXT.
           EXIT.
      *
      ****************************************************************
      *    自賠責集計等取扱区分チェック処理
      ****************************************************************
       508-JIBAIKBN-CHK-SEC      SECTION.
      *
           PERFORM 3010-JIBAIKBN-HEN-SEC
           .
       508-JIBAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カラム位置、桁数チェック処理
      *****************************************************************
       506-DIGIT-CHK-SEC          SECTION.
      *
           IF      SPA-Z01-SRYCD (1:2) =   "84"
      *        カラム位置か桁数のみはエラー
               IF     ((SPA-GMN-COLUMN1    =   ZERO )  AND
                       (SPA-GMN-DIGIT1     =   ZERO ))
                 OR   ((SPA-GMN-COLUMN1    >   ZERO )  AND
                       (SPA-GMN-DIGIT1     >   ZERO ))
                   CONTINUE
               ELSE
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
               IF     ((SPA-GMN-COLUMN2    =   ZERO )  AND
                       (SPA-GMN-DIGIT2     =   ZERO ))
                 OR   ((SPA-GMN-COLUMN2    >   ZERO )  AND
                       (SPA-GMN-DIGIT2     >   ZERO ))
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0011"              TO  SPA-ERRCD
                       MOVE    8                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF     ((SPA-GMN-COLUMN3    =   ZERO )  AND
                       (SPA-GMN-DIGIT3     =   ZERO ))
                 OR   ((SPA-GMN-COLUMN3    >   ZERO )  AND
                       (SPA-GMN-DIGIT3     >   ZERO ))
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0011"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF     ((SPA-GMN-COLUMN4    =   ZERO )  AND
                       (SPA-GMN-DIGIT4     =   ZERO ))
                 OR   ((SPA-GMN-COLUMN4    >   ZERO )  AND
                       (SPA-GMN-DIGIT4     >   ZERO ))
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0011"              TO  SPA-ERRCD
                       MOVE    12                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       506-DIGIT-CHK-EXT.
           EXIT.
      *
      *v47
      ****************************************************************
      *    複写先コード処理
      ****************************************************************
       502-COPYCD-CHK-SEC      SECTION.
      *
           MOVE    Z02-COPYCD          TO  SPA-GMN-COPYCD
      *
           IF      SPA-GMN-COPYCD      =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      502-COPYCD-CHK-EXT
           END-IF
      *
           MOVE    22                  TO  SPA-GMN-CUR
      *    全角入力の時、４文字まで
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    Z02-COPYCD          TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-COPYCD
      *
      *    "//" 入力の時、検索画面へ
           IF     SPA-GMN-COPYCD(1:2)  =  "//"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
           ELSE
      *        入力コードチェック
               IF      SPA-GMN-COPYCD      NOT =   SPACE
                   MOVE    SPA-GMN-COPYCD      TO  WRK-INPUTCD
                   PERFORM 4010-SRYCD-CHK-SEC
               END-IF
           END-IF
      *
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-COPYCD  NOT =   SPACE )
      *        複写処理
               PERFORM 5021-COPYCD-SYORI-SEC
           END-IF
           .
       502-COPYCD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    複写チェック処理
      ****************************************************************
       5022-COPYCD-CDCHK-SEC      SECTION.
      *
           EVALUATE    SPA-Z01-SRYCD (1:3)
      *        部位
               WHEN    "002"
                   IF      SPA-GMN-COPYCD (1:3)    =   "002"
                       CONTINUE
                   ELSE
                       MOVE    "0020"          TO  SPA-ERRCD
                   END-IF
      *        コメント
               WHEN    "008"
                   IF     (SPA-GMN-COPYCD (1:3)    =   "008" )
                     OR   (SPA-GMN-COPYCD (1:1)    =   "8"   )
                       CONTINUE
                   ELSE
                       MOVE    "0020"          TO  SPA-ERRCD
                   END-IF
      *        自費
               WHEN    "095"
               WHEN    "096"
                   IF      SPA-GMN-COPYCD (1:3)    =   "095"
                                                   OR  "096"
                       CONTINUE
                   ELSE
                       MOVE    "0020"          TO  SPA-ERRCD
                   END-IF
      *
               WHEN    OTHER
                   MOVE    "0020"          TO  SPA-ERRCD
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    22                  TO  SPA-GMN-CUR
           END-IF
           .
       5022-COPYCD-CDCHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    複写処理処理
      ****************************************************************
       5021-COPYCD-SYORI-SEC      SECTION.
      *
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-GMN-COPYCD      TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 3001-TENSU-SET-SEC
               MOVE    SPACE               TO  SPA-GMN-COPYCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
           .
       5021-COPYCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット
      *****************************************************************
       3001-TENSU-SET-SEC                     SECTION.
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
           EVALUATE    SPA-Z01-SRYCD(1:3)
      *        部位
               WHEN    "002"
                   MOVE    TNS-BUIKBN          TO  SPA-GMN-BUIKBN
                   MOVE    SPACE               TO  SPA-GMN-BUIKBNMEI
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   SPA-GMN-BUIKBN-MAX
                       IF      SPA-GMN-BUIKBN 
                                       =   SPA-GMN-BUIKBNL (IDXX)
                           MOVE    SPA-GMN-BUIKBNLST (IDXX)
                                             TO  SPA-GMN-BUIKBN-G
                           MOVE    SPA-GMN-BUIKBN-MAX
                                             TO  IDXX
                       END-IF
                   END-PERFORM
      *        コメント
               WHEN    "008"
                   IF     (SPA-Z01-SRYCD  (1:4)  =   "0084" )
                      IF      (SPA-GMN-COPYCD (1:4)  =   "0084")  OR
                              (SPA-GMN-COPYCD (1:2)  =   "84"  )
      *                埋め込み値複写
                           MOVE  TNS-SSTKIJUNCD(1)   TO SPA-GMN-COLUMN1
                           MOVE  TNS-SSTKIJUNCD(2)   TO SPA-GMN-DIGIT1
                           MOVE  TNS-SSTKIJUNCD(3)   TO SPA-GMN-COLUMN2
                           MOVE  TNS-SSTKIJUNCD(4)   TO SPA-GMN-DIGIT2
                           MOVE  TNS-SSTKIJUNCD(5)   TO SPA-GMN-COLUMN3
                           MOVE  TNS-SSTKIJUNCD(6)   TO SPA-GMN-DIGIT3
                           MOVE  TNS-SSTKIJUNCD(7)   TO SPA-GMN-COLUMN4
                           MOVE  TNS-SSTKIJUNCD(8)   TO SPA-GMN-DIGIT4
                       ELSE
      *                埋め込み値クリア
                           MOVE  ZERO                TO SPA-GMN-COLUMN1
                           MOVE  ZERO                TO SPA-GMN-DIGIT1
                           MOVE  ZERO                TO SPA-GMN-COLUMN2
                           MOVE  ZERO                TO SPA-GMN-DIGIT2
                           MOVE  ZERO                TO SPA-GMN-COLUMN3
                           MOVE  ZERO                TO SPA-GMN-DIGIT3
                           MOVE  ZERO                TO SPA-GMN-COLUMN4
                           MOVE  ZERO                TO SPA-GMN-DIGIT4
                       END-IF
                   END-IF
      *        自費
               WHEN    "095"
               WHEN    "096"
                   MOVE  TNS-TEN             TO  SPA-GMN-TEN
                   MOVE  TNS-HKNTEKKBN       TO  SPA-GMN-HKNTEKKBN
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   SPA-GMN-HKNTEKKBN-MAX
                       IF      SPA-GMN-HKNTEKKBN 
                                       =   SPA-GMN-HKNTEKKBNL (IDXX)
                           MOVE    SPA-GMN-HKNTEKKBNLST (IDXX)
                                             TO  SPA-GMN-HKNTEKKBN-G
                           MOVE    SPA-GMN-HKNTEKKBN-MAX
                                             TO  IDXX
                       END-IF
                   END-PERFORM
                   MOVE  TNS-GAITENTTLKBN    TO  SPA-GMN-GAITENTTLKBN
                   MOVE  TNS-NYUTENTTLKBN    TO  SPA-GMN-NYUTENTTLKBN
      *            自費コードの時、点数識別とする
                   IF      SPA-GMN-BUIKBN-MAX      >=  2
                       MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-BUIKBN
                       MOVE    SPACE               TO  SPA-GMN-BUIKBNMEI
                   END-IF
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   SPA-GMN-BUIKBN-MAX
                       IF      SPA-GMN-BUIKBN 
                                       =   SPA-GMN-BUIKBNL (IDXX)
                           MOVE    SPA-GMN-BUIKBNLST (IDXX)
                                             TO  SPA-GMN-BUIKBN-G
                           MOVE    SPA-GMN-BUIKBN-MAX
                                             TO  IDXX
                       END-IF
                   END-PERFORM
      *            自賠責集計等取扱
                   IF     (SPA-Z01-SRYCD(1:5)  =   "09593" )
                     AND  (SPA-GMN-COPYCD(1:5) =   "09593" )
                       MOVE    TNS-NAMECHGKBN      TO
                                               SPA-GMN-GAIJIBAIKBN
                       MOVE    TNS-KANANAMECHGKBN  TO
                                               SPA-GMN-NYUJIBAIKBN
                       PERFORM 3010-JIBAIKBN-HEN-SEC
                   END-IF
      *            自費集計先
                   PERFORM 3012-JIHINAME-HEN-SEC
      *H29.2
      *            単位コード、薬剤区分
                   MOVE    SPACE           TO  SPA-GMN-YKZKBN-G
                   MOVE    SPACE           TO  SPA-GMN-TANICD-G
                   IF      SPA-NAI-YKZKBN      =   "1"
                       IF     (SPA-Z01-SRYCD(1:3)  =   "095"
                                                   OR  "096" )
                         AND  (SPA-Z01-SRYCD(4:2)  >=  "21"
                                               AND <=  "27"  )
                           IF      TNS-YKZKBN      NOT =   ZERO
                               MOVE    TNS-YKZKBN  TO  SPA-GMN-YKZKBN
                           END-IF
                           MOVE    TNS-TANICD      TO  SPA-GMN-TANICD
                           MOVE    TNS-TANINAME    TO  SPA-GMN-TANICDMEI
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       3001-TENSU-SET-EXT.
           EXIT.
      *****************************************************************
      *  　入力コードチェック処理
      *****************************************************************
       4010-SRYCD-CHK-SEC                        SECTION.
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-INPUTCD         TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-INPUTCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO    4010-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9    )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (WRK-INPUTCD         NUMERIC  )
               CONTINUE
           ELSE
      *        入力コード検索
               PERFORM 409-KENSAKU-SEC
           END-IF
      *
           .
       4010-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           INITIALIZE                  SPA-Z98GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
      *        その他
               MOVE    "8"                 TO  SPA-Z98-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Z98-INPUTCD
               MOVE    "1"                 TO  SPA-Z98-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Z98-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-Z98-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-Z98-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-Z98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-Z98-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           ELSE
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           END-IF
      *
      *
           MOVE    "Z02"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z02"               TO  SPA-Z98-MOTOPG
      *
           IF      SPA-Z98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "Z98    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-Z98-SET-SEC                           SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z02             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z02
      *
           .
       41011-Z98-SET-EXT.
           EXIT.
      *****************************************************************
      *    前回コード複写処理
      *****************************************************************
       406-ZENCOPY-SEC          SECTION.
      *
           IF      SPA-Z08MAE-SRYCD        =   SPACE
               MOVE    22                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-Z08MAE-SRYCD    TO  SPA-GMN-COPYCD
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
               IF     (FLG-END             =   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )
      *            複写処理
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF 
           END-IF 
           .
       406-ZENCOPY-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >=  SPA-NAI-YUKOEDYMD
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    有効期限範囲チェック
           IF      SPA-Z01-SYORIKBN    =   2
      *        前チェック
               IF      SPA-Z01-SELNUM      >   1
                   MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM - 1)
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      ************ IF      SPA-NAI-YUKOSTYMD   NOT =   WRK-OTYMD
                   IF      SPA-NAI-YUKOSTYMD       <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *        後チェック
               IF      SPA-Z01-SELNUM      <   SPA-Z01-MAX
                   MOVE    SPA-NAI-YUKOEDYMD
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      **********   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
      ********                                 NOT =   WRK-OTYMD
                   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
                                                   <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *    改正チェック
           IF      SPA-Z01-SYORIKBN    =   3
               IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           >=  SPA-NAI-YUKOSTYMD
                   MOVE    "1012"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
               IF  SPA-NAI-YUKOEDYMD    NOT    =  ALL "9"
                   MOVE    "1013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
      *H24.8
               IF      SPA-Z01-SRYCD(1:1)  =   "8"
                   IF      SPA-NAI-KEIFLG3     =   ZERO
                       MOVE    "K005"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *H24.8
      *    コメント期間変更チェック
           IF      SPA-Z01-SELNUM      >=  1
               IF     ( SPA-Z01-SRYCD(1:1)  =   "8"   )
               AND    ((SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOSTYMD)
               OR      (SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOEDYMD))
                   IF      SPA-NAI-KEIFLG3     =   ZERO
                       MOVE    "K004"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-GMN-NAME        =   SPACE  )
               IF     (SPA-Z01-SRYCD       =  "810000001"
                                           OR "008500000"
                                           OR "008600000" )
                   CONTINUE
               ELSE
                   MOVE    "1014"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *    カラム位置チェック
           IF     (SPA-Z01-SRYCD(1:2)  =   "84"  )  OR
                  (SPA-Z01-SRYCD(1:4)  =   "0084")
      *H30.4
      *        調剤分割
              OR  (SPA-Z01-SRYCD(1:7)  =   "0992081" )
               CONTINUE
           ELSE
               PERFORM 41092-COLUMN-CHK-SEC
           END-IF
      *    自費コードの時、コードチェック
           IF      SPA-Z01-SRYCD(1:3)  =  "095"  OR  "096"
               IF      SPA-ERRCD           =   SPACE
                   IF      SPA-SRYCDCHK       =   ZERO
                       PERFORM 41091-SRYCD-CHK-SEC
                   END-IF
               END-IF
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カラム位置チェック処理
      *****************************************************************
       41092-COLUMN-CHK-SEC          SECTION.
      *
      *    84,0084　以外の時入力不可
           IF      (SPA-GMN-COLUMN1    =   ZERO )  AND
                   (SPA-GMN-DIGIT1     =   ZERO )  AND
                   (SPA-GMN-COLUMN2    =   ZERO )  AND
                   (SPA-GMN-DIGIT2     =   ZERO )  AND
                   (SPA-GMN-COLUMN3    =   ZERO )  AND
                   (SPA-GMN-DIGIT3     =   ZERO )  AND
                   (SPA-GMN-COLUMN4    =   ZERO )  AND
                   (SPA-GMN-DIGIT4     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       41092-COLUMN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費コードのコードチェック処理
      *****************************************************************
       41091-SRYCD-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-SRYCDCHK
           EVALUATE    SPA-Z01-SRYCD(4:2)
               WHEN    "00"
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "24"
               WHEN    "25"
               WHEN    "26"
               WHEN    "27"
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
               WHEN    "40"
               WHEN    "50"
               WHEN    "54"
               WHEN    "60"
               WHEN    "64"
               WHEN    "70"
               WHEN    "80"
               WHEN    "83"
               WHEN    "84"
               WHEN    "85"
               WHEN    "89"
               WHEN    "90"
               WHEN    "91"
               WHEN    "92"
               WHEN    "93"
               WHEN    "94"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       41091-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
           MOVE    "Z01"               TO  SPA-SAKIPG
      *
           MOVE    "Z02"               TO  SPA-MOTOPG
      *
      ***  MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       401-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
      *H30.9
      *    新規・改正処理はなし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               GO      TO     403-DEL-EXT
           END-IF
      *
           IF      SPA-NAI-KEIFLG1     =   ZERO
               IF      SPA-Z01-SRYCD(1:1)  =   "8"
                   MOVE    "K003"          TO  SPA-ERRCD
               END-IF
               MOVE    1               TO  SPA-NAI-KEIFLG1
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0001"              TO  SPA-ZIDCD
           END-IF
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
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設定分
               PERFORM 40311-TENSU-USER-DEL-SEC
           ELSE
      *        点数マスタ削除
               PERFORM 40312-TENSU-MAST-DEL-SEC
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               MOVE    SPACE               TO  SPA-Z08MAE-SRYCD
      *
               MOVE     "JOIN"            TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ユーザ点数設定分削除処理
      *****************************************************************
       40311-TENSU-USER-DEL-SEC        SECTION.
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF     MCP-RC           NOT =   ZERO
               DISPLAY "Z05 TENSU DEL ERR!!"     MCP-RC
                       ",KEY:" TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      *    入力コード削除
           IF     (SPA-Z01-SRYCD(1:1)  =   "0"  )  AND
                  (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               INITIALIZE                  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  ICD-SRYCD
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
      *        点数付加情報削除
               INITIALIZE                  TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           END-IF
           .
       40311-TENSU-USER-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ点数設定分削除処理
      *****************************************************************
       40312-TENSU-MAST-DEL-SEC        SECTION.
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z02 TENSU DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      **** 点数付加情報削除
      *    MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
      *    MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
      *                                TO  TNSP-YUKOSTYMD
      *    MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
      *                                TO  TNSP-YUKOEDYMD
      *    MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *    MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *    MOVE    "DBDELETE"          TO  MCP-FUNC
      *    MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                        MCPDATA-REC
      ****                                     SPA-AREA
      *
      *    入力コード削除
           IF     (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               INITIALIZE                  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  ICD-SRYCD
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
      *        点数付加情報削除
               INITIALIZE                  TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           END-IF
           .
       40312-TENSU-MAST-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                              SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z02             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z02
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z02"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z99GMN-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       405-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
           IF      SPA-Z01-SYORIKBN    =   1
      *        新規処理
               IF      SPA-NAI-KEIFLG2     =   ZERO
                   IF      SPA-Z01-SRYCD(1:1)  =   "8"
                       MOVE    "K000"          TO  SPA-ERRCD
                   ELSE
                       IF     (SPA-Z01-SRYCD(1:3)  =   "002"
                                                   OR  "095"
                                                   OR  "096" )  OR
                              (SPA-Z01-SRYCD(1:4)  =   "0082"
                                                   OR  "0083"
                                                   OR  "0084"
                                                   OR  "0085"
                                                   OR  "0086" )
                           CONTINUE
                       ELSE
                           MOVE    "K002"          TO  SPA-ERRCD
                       END-IF
                   END-IF
                   MOVE    1               TO  SPA-NAI-KEIFLG2
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        登録確認メッセージ
               MOVE    "0002"              TO  SPA-ZIDCD
           END-IF
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-TOUROKU-SEC                          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設登録
               PERFORM 41211-TENSU-USER-UPD-SEC
           ELSE
      *        点数マスタ登録
               PERFORM 41212-TENSU-MAST-UPD-SEC
           END-IF
      *
      *    前画面に戻る
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-Z01-SRYCD       TO  SPA-Z08MAE-SRYCD
      *
               MOVE     "JOIN"            TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
           .
       4121-TOUROKU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ユーザ点数設登録処理
      *****************************************************************
       41211-TENSU-USER-UPD-SEC        SECTION.
      *
           MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *
      *
           .
       41211-TENSU-USER-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ登録処理
      *****************************************************************
       41212-TENSU-MAST-UPD-SEC        SECTION.
      *
           IF     (SPA-MENTE-OK        =   1 )
      *    メンテナンスモードのみ、マスタ更新
           MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
           END-IF
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
           .
       41212-TENSU-MAST-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正時処理
      *****************************************************************
       41211-KAISEI-SYORI-SEC                       SECTION.
      *
      *    有効終了日を決定する
      *    今回の有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOSTYMD   TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-YUKOEDYMD
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41211-KAISEI-SYORI-EXT
           END-IF
      *
      *    有効年月日を更新する
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新
               MOVE    WRK-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z02 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41211-KAISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41212-UPDATE-SYORI-SEC                       SECTION.
      *
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新項目編集
               MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
               PERFORM 4121-KOUMOKUISO-SEC
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z02 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41212-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41213-INSERT-SYORI-SEC          SECTION.
      *
      *    データＩＮＳＥＲＴ
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
           PERFORM 4121-KOUMOKUISO-SEC
      *
           MOVE    SPA-OPID            TO  TNS-OPID
           MOVE    SMCNDATE-YMD        TO  TNS-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z02 TENSU INS ERR:" MCP-RC
                       ",KEY:" TNS-KEY
           END-IF
      *
           .
       41213-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新編集処理
      *****************************************************************
       4121-KOUMOKUISO-SEC                       SECTION.
      *
           MOVE  SPA-GMN-KANANAME              TO  TNS-KANANAME
                                                   WRK-STRINGTBL
           MOVE  100                           TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                    TO  TNS-KANAYUKOKETA
      *
           MOVE  SPA-GMN-NAME                  TO  TNS-NAME
                                                   WRK-STRINGTBL
           MOVE  100                           TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                    TO  TNS-YUKOKETA
      *
           MOVE  SPA-GMN-FORMALNAME            TO  TNS-FORMALNAME
                                                   WRK-STRINGTBL
           MOVE  200                           TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                    TO  TNS-FORMALYUKOKETA
      *
           MOVE  SPA-GMN-COLUMN1               TO  TNS-SSTKIJUNCD(1)
           MOVE  SPA-GMN-DIGIT1                TO  TNS-SSTKIJUNCD(2)
           MOVE  SPA-GMN-COLUMN2               TO  TNS-SSTKIJUNCD(3)
           MOVE  SPA-GMN-DIGIT2                TO  TNS-SSTKIJUNCD(4)
           MOVE  SPA-GMN-COLUMN3               TO  TNS-SSTKIJUNCD(5)
           MOVE  SPA-GMN-DIGIT3                TO  TNS-SSTKIJUNCD(6)
           MOVE  SPA-GMN-COLUMN4               TO  TNS-SSTKIJUNCD(7)
           MOVE  SPA-GMN-DIGIT4                TO  TNS-SSTKIJUNCD(8)
           MOVE  SPA-GMN-TEN                   TO  TNS-TEN
           MOVE  SPA-GMN-HKNTEKKBN             TO  TNS-HKNTEKKBN
           MOVE  SPA-GMN-GAITENTTLKBN          TO  TNS-GAITENTTLKBN
           MOVE  SPA-GMN-NYUTENTTLKBN          TO  TNS-NYUTENTTLKBN  
           MOVE  SPA-GMN-BUIKBN                TO  TNS-BUIKBN  
      *    自費コードの時、部位区分は点数識別とする
           IF      SPA-JIHIKBN     NOT =   ZERO
               MOVE    SPA-GMN-BUIKBN          TO  TNS-TENSIKIBETU
               MOVE    ZERO                    TO  TNS-BUIKBN
           END-IF
      *
      *        自賠責集計等取扱
           IF      SPA-Z01-SRYCD(1:5)  =   "09593"
               MOVE   SPA-GMN-GAIJIBAIKBN  TO  TNS-NAMECHGKBN
               MOVE   SPA-GMN-NYUJIBAIKBN  TO  TNS-KANANAMECHGKBN
           ELSE
               MOVE   ZERO                 TO  TNS-NAMECHGKBN
               MOVE   ZERO                 TO  TNS-KANANAMECHGKBN
           END-IF
      *H29.2
      *    薬剤区分
           IF      SPA-NAI-YKZKBN      =   "1"
               IF      SPA-GMN-YKZKBN      =   SPACE
                   MOVE    ZERO                TO  TNS-YKZKBN
               ELSE
                   MOVE    SPA-GMN-YKZKBN      TO  TNS-YKZKBN
               END-IF
           END-IF
      *    単位コード
           MOVE    SPA-GMN-TANICD      TO  TNS-TANICD
           MOVE    SPA-GMN-TANICDMEI   TO  TNS-TANINAME
      *****MOVE  SPA-GMN-INPCHKKBN             TO  TNSP-INPCHKKBN
           .
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       400-ZENCHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
           .
      *
       400-ZENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC             SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *     画面ワークエリアにセット
            INITIALIZE                           Z02.
      *
            MOVE  SPA-GMN-SRYCDH               TO  Z02-SRYCDH.
            MOVE  SPA-GMN-NAMEH                TO  Z02-NAMEH.
            MOVE  SPA-GMN-YUKOSTYMDH           TO  Z02-YUKOSTYMDH.
            MOVE  SPA-GMN-YUKOEDYMDH           TO  Z02-YUKOEDYMDH.
      *
            MOVE  SPA-GMN-YUKOSTYMD            TO  Z02-YUKOSTYMD.
            MOVE  SPA-GMN-YUKOEDYMD            TO  Z02-YUKOEDYMD.
      *
            MOVE  SPA-GMN-KANANAME             TO  Z02-KANANAME.
            MOVE  SPA-GMN-NAME                 TO  Z02-NAME.
            MOVE  SPA-GMN-FORMALNAME           TO  Z02-FORMALNAME.
      *
            MOVE  SPA-GMN-COLUMN1              TO  Z02-COLUMN1.
            MOVE  SPA-GMN-COLUMN2              TO  Z02-COLUMN2.
            MOVE  SPA-GMN-COLUMN3              TO  Z02-COLUMN3.
            MOVE  SPA-GMN-COLUMN4              TO  Z02-COLUMN4.
      *
            MOVE  SPA-GMN-DIGIT1               TO  Z02-DIGIT1.
            MOVE  SPA-GMN-DIGIT2               TO  Z02-DIGIT2.
            MOVE  SPA-GMN-DIGIT3               TO  Z02-DIGIT3.
            MOVE  SPA-GMN-DIGIT4               TO  Z02-DIGIT4.
      *
            MOVE  SPA-GMN-TEN                  TO  Z02-TEN
            MOVE  SPA-GMN-HKNTEKKBN-G          TO  Z02-HKNTEKKBN
            MOVE  SPA-GMN-GAITENTTLKBN         TO  Z02-GAITENTTLKBN
            MOVE  SPA-GMN-NYUTENTTLKBN         TO  Z02-NYUTENTTLKBN
            MOVE  SPA-GMN-BUIKBN-G             TO  Z02-BUIKBN
      *
      ***** MOVE  SPA-GMN-INPCHKKBN            TO  Z02-INPCHKKBN
      *     
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-HKNTEKKBN-MAX
               MOVE    SPA-GMN-HKNTEKKBNLST (IDXX)
                                       TO  Z02-HKNTEKKBNLST   (IDXX)
           END-PERFORM
           MOVE    SPA-GMN-HKNTEKKBN-MAX   TO  Z02-HKNTEKKBN-COUNT
      *     
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-BUIKBN-MAX
               MOVE    SPA-GMN-BUIKBNLST (IDXX)
                                       TO  Z02-BUIKBNLST   (IDXX)
           END-PERFORM
           MOVE    SPA-GMN-BUIKBN-MAX   TO  Z02-BUIKBN-COUNT
      *
           IF      SPA-JIHIKBN         =   ZERO
               MOVE    "部位区分"          TO  Z02-LABEL1
           ELSE
               MOVE    "金額内容"          TO  Z02-LABEL1
           END-IF
      *    自賠責集計等取扱
           MOVE    SPA-GMN-GAIJIBAIKBN-G   TO  Z02-GAIJIBAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-GAIJIBAI-MAX
               MOVE    SPA-GMN-GAIJIBAIKBN-LST (IDX)
                                       TO  Z02-GAIJIBAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-GAIJIBAI-MAX   TO  Z02-GAIJIBAIKBN-COUNT
      *
           MOVE    SPA-GMN-NYUJIBAIKBN-G   TO  Z02-NYUJIBAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-NYUJIBAI-MAX
               MOVE    SPA-GMN-NYUJIBAIKBN-LST (IDX)
                                       TO  Z02-NYUJIBAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUJIBAI-MAX   TO  Z02-NYUJIBAIKBN-COUNT
      *H29.2
      *    単位コード
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-TANICD-MAX
               MOVE    SPA-GMN-TANICD-LST(IDX)
                                           TO  Z02-TANICD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-TANICD-MAX  TO  Z02-TANICD-COUNT
           MOVE    SPA-GMN-TANICD-G    TO  Z02-TANICD
      *    薬剤区分
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-YKZKBNMAX
               MOVE    SPA-GMN-YKZKBN-LST(IDX)
                                           TO  Z02-YKZKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-YKZKBNMAX   TO  Z02-YKZKBN-COUNT
           MOVE    SPA-GMN-YKZKBN-G    TO  Z02-YKZKBN
      *
      *v47
      *    複写元コード
           MOVE    SPA-GMN-COPYCD      TO  Z02-COPYCD
           MOVE    SPA-GMN-JIHINAME1   TO  Z02-JIHINAME1
           MOVE    SPA-GMN-JIHINAME2   TO  Z02-JIHINAME2
           MOVE    "前回複写"          TO  Z02-B06-LABEL
           MOVE    "検索"              TO  Z02-B09-LABEL
      *H29.1
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"  TO  Z02-MIDASHI-DATA
               MOVE    "red"               TO  Z02-MIDASHI-STYLE
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  Z02-COPYCD-STATE
           MOVE    WIDGET-NORMAL       TO  Z02-B06-STATE
           MOVE    WIDGET-NORMAL       TO  Z02-B09-STATE
      *    複写機能不可
           IF     (SPA-NAI-USERCD      =   ZERO )
               MOVE    WIDGET-INSENSITIVE      TO  Z02-COPYCD-STATE
               MOVE    WIDGET-INSENSITIVE      TO  Z02-B06-STATE
               MOVE    WIDGET-INSENSITIVE      TO  Z02-B09-STATE
      *
               MOVE    SPACE                   TO  Z02-B06-LABEL
               MOVE    SPACE                   TO  Z02-B09-LABEL
           END-IF
      *    ユーザー点数識別（2 ：ユーザーマスタ）のみ更新
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               MOVE    WIDGET-NORMAL           TO  Z02-B03-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE      TO  Z02-B03-STATE
           END-IF
      *H30.9
      *    新規・改正処理はなし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               MOVE    WIDGET-INSENSITIVE      TO  Z02-B03-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
               WHEN    2
                   MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
               WHEN    3
                   MOVE    "KANANAME"          TO  MCP-WIDGET
               WHEN    4
                   MOVE    "NAME"              TO  MCP-WIDGET
               WHEN    5
                   MOVE    "FORMALNAME"        TO  MCP-WIDGET
               WHEN    6
                   MOVE    "COLUMN1"           TO  MCP-WIDGET
               WHEN    7
                   MOVE    "DIGIT1"            TO  MCP-WIDGET
               WHEN    8
                   MOVE    "COLUMN2"           TO  MCP-WIDGET
               WHEN    9
                   MOVE    "DIGIT2"            TO  MCP-WIDGET
               WHEN    10
                   MOVE    "COLUMN3"           TO  MCP-WIDGET
               WHEN    11
                   MOVE    "DIGIT3"            TO  MCP-WIDGET
               WHEN    12
                   MOVE    "COLUMN4"           TO  MCP-WIDGET
               WHEN    13
                   MOVE    "DIGIT4"            TO  MCP-WIDGET
               WHEN    14
                   MOVE    "TEN"               TO  MCP-WIDGET
               WHEN    15
                   MOVE    "HKNTEKKBN"         TO  MCP-WIDGET
               WHEN    16
                   MOVE    "GAITENTTLKBN"      TO  MCP-WIDGET
               WHEN    17
                   MOVE    "NYUTENTTLKBN"      TO  MCP-WIDGET
               WHEN    18
                   MOVE    "BUIKBN"            TO  MCP-WIDGET
               WHEN    20
                   MOVE    "GAIJIBAIKBN"       TO  MCP-WIDGET
               WHEN    21
                   MOVE    "NYUJIBAIKBN"       TO  MCP-WIDGET
               WHEN    19
     **登録へ      MOVE    "INPCHKKBN"         TO  MCP-WIDGET
                   MOVE    "B12"               TO  MCP-WIDGET
               WHEN    22
                   MOVE    "COPYCD"            TO  MCP-WIDGET
               WHEN    23
                   MOVE    "TANICD"            TO  MCP-WIDGET
               WHEN    24
                   MOVE    "YKZKBN"            TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルセット処理
      *****************************************************************
       50011-CUR-SET-SEC                            SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOSTYMD"
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    3                   TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    4                   TO  SPA-GMN-CUR
               WHEN    "NAME"
                   MOVE    5                   TO  SPA-GMN-CUR
               WHEN    "FORMALNAME"
      *            入力チェック区分へ
                   MOVE    19                  TO  SPA-GMN-CUR
                   IF      SPA-Z01-SRYCD(1:4)  =   "0084"
                      OR   SPA-Z01-SRYCD(1:2)  =   "84"
      *                カラム位置へ
                       MOVE    6                   TO  SPA-GMN-CUR
                   END-IF
                   IF      SPA-Z01-SRYCD(1:3)  =   "095"  OR
                                                   "096"
      *                金額へ
                       MOVE    14                  TO  SPA-GMN-CUR
                   END-IF
                   IF      SPA-Z01-SRYCD(1:3)  =   "002"
      *                部位へ
                       MOVE    18                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "COLUMN1"
                   MOVE    7                   TO  SPA-GMN-CUR
               WHEN    "DIGIT1 "
                   MOVE    8                   TO  SPA-GMN-CUR
               WHEN    "COLUMN2"
                   MOVE    9                   TO  SPA-GMN-CUR
               WHEN    "DIGIT2 "
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "COLUMN3"
                   MOVE    11                  TO  SPA-GMN-CUR
               WHEN    "DIGIT3 "
                   MOVE    12                  TO  SPA-GMN-CUR
               WHEN    "COLUMN4"
                   MOVE    13                  TO  SPA-GMN-CUR
               WHEN    "DIGIT4 "
                   MOVE    14                  TO  SPA-GMN-CUR
                   IF     (SPA-Z01-SRYCD(1:3)  =   "008" )
                       OR (SPA-Z01-SRYCD(1:1)  =   "8"   )
      *                登録へ
                       MOVE    19                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "TEN      "
                   MOVE    15                  TO  SPA-GMN-CUR
      *??
      *            単位へ
                   IF      SPA-NAI-YKZKBN      =   "1"
                       MOVE    23                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "TANICD"
                   IF      SPA-NAI-YKZKBN      =   "1"
                       MOVE    24                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    15                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "YKZKBN"
                   MOVE    15                  TO  SPA-GMN-CUR
      *
               WHEN    "HKNTEKKBN"
                   MOVE    16                  TO  SPA-GMN-CUR
               WHEN    "GAITENTTLKBN"
                   MOVE    17                  TO  SPA-GMN-CUR
               WHEN    "NYUTENTTLKBN"
                   MOVE    18                  TO  SPA-GMN-CUR
               WHEN    "BUIKBN"
                   IF      SPA-Z01-SRYCD(1:5)  =   "09593"
                       MOVE    20                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    19                  TO  SPA-GMN-CUR
                   END-IF
                   IF      SPA-Z01-SRYCD(1:3)  =   "002"
      *                登録へ
                       MOVE    19                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "GAIJIBAIKBN"
                   MOVE    21                  TO  SPA-GMN-CUR
               WHEN    "NYUJIBAIKBN"
      *****        MOVE    19                  TO  SPA-GMN-CUR
                   MOVE    90                  TO  SPA-GMN-CUR
      ***      WHEN    "INPCHKKBN"
      ************ MOVE    20                  TO  SPA-GMN-CUR
      ***          MOVE    90                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効開始年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効終了年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "カナ名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "漢字名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "正式名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "保険適用がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "部位区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "金額種別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "金額種別が税込みです。金額を入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   STRING  "レセプト編集情報はコメントコードでのみ"
                           "設定可能です。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "カラム位置と桁数の設定をして下さい。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "金額は自費コードでのみ設定可能です。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "単位コードがありません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   STRING  "薬剤区分がありません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0015"
                   STRING  "薬剤区分は金額ゼロでは登録できません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0016"
                   STRING  "自費の金額は７桁まで登録できます。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
      *v47
               WHEN    "0020"
                   STRING  "複写できるコードではありません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0021"
                   STRING  "複写元コードが登録されていません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K001"
                   MOVE    "警告！自費コードの診療区分がありません。"
                                                 TO  SPA-ERRMSG
                   MOVE    "リハビリ・その他に集計されます。"
                                                 TO  SPA-ERRMSG(41:)
               WHEN    "K000"
                   STRING    "警告！厚生労働省から提供される"
                                                   DELIMITED BY  SIZE
                             "コードの範囲です。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING    "警告！ユーザ登録できる範囲のコード"
                                                   DELIMITED BY  SIZE
                             "ではありません。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING   "警告！厚生労働省から提供されたデータです。"
                                                   DELIMITED BY  SIZE
                            "削除しないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H24.8
               WHEN    "K004"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K005"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。改正処理はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *
      *        以下共通
               WHEN    "1010"
                   MOVE
                   "有効開始年月日≧有効終了年月日で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "有効年月日の範囲が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "有効年月日が重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "改正の終了年月日はＡＬＬ９のみ入力できます"
                                                 TO  SPA-ERRMSG
               WHEN    "1014"
                   MOVE    "漢字名称を入力してください"
                                                 TO  SPA-ERRMSG
              WHEN    "1001"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "追加処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "点数マスタ付加情報が更新できません"
                                                 TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                TO  ZERR-ERRMSG
      *
           MOVE    "Z02"                         TO  SPA-MOTOPG
           MOVE    "ZERR"                        TO  SPA-SAKIPG
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE
           MOVE    "ZERR"                        TO  MCP-WINDOW
           MOVE    "B01"                         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z02"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　処理
      *****************************************************************
      *
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .                                       
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       960-SYSKANRI-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
      *
       960-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付変換　処理
      *****************************************************************
      *****************************************************************
      *     対象日＋１日計算処理
      *****************************************************************
       800-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-YMD
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-OTYMD
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-OTYMD
               WHEN    OTHER
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
           END-EVALUATE
           .                                       
      *
           .
       800-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:7)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "9999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
      *
      *----(01.00.01) LINE ADD START --------------------------------
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                       MOVE    ZERO                TO  FLG-ERR
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
           END-EVALUATE
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   文字数カウント処理（全角のみ）
      *****************************************************************
       891-STRCOUNT-SEC                         SECTION.
      *
           PERFORM  UNTIL  WRK-STRING(CNT-STRING)  NOT  =  SPACE
                    OR  CNT-STRING  =  1
               COMPUTE  CNT-STRING  =  CNT-STRING  -  1
           END-PERFORM
      *
           IF  CNT-STRING                        =  1
               MOVE  ZERO                        TO  CNT-STRING
           ELSE
               COMPUTE  CNT-STRING               =  CNT-STRING  /  2
           END-IF
           .                                       
       891-STRCOUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
