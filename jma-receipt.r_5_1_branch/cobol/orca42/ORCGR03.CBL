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
       PROGRAM-ID.             ORCGR03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成−作成指示（Ｒ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/07/16  日付画面用チェックサブ追加
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-小豆沢   01/11/28  SPA-TERMIDの桁数変更に伴う
      *                                     部分参照に変更      
      *  01.02.01    MCC-藤原     02.01.31  ジョブ管理ＤＢの追加
      *                                     Ｆ８の追加
      *  01.02.02    MCC-藤原     02/02/19  処理中でも処理を終了できるように
      *                                     修正
      *  01.02.03    MCC-藤原     02/02/21  処理中のときのメッセージに
      *                                     プログラム名も表示
      *  01.02.04    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.03.01    MCC-藤原     02/05/30  情報削除ボタン追加
      *  01.03.02    MCC-藤原     02/06/06  処理結果画面追加
      *  01.03.03    MCC-藤原     02/06/13  労災・自賠責の項目を追加
      *  01.03.04    MCC-藤原     02/07/09  生活保護まとめ入力画面追加
      *  01.03.05    NACL-藤原    02/07/18  確定のとき自動的に
      *                                     情報確認画面を表示
      *  01.03.06    NACL-藤原    02.08.30  タイマー機能の追加
      *
      *  01.04.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.04.02    NACL-藤原    02/09/24  ２００２．１０改正対応
      *  01.04.03    NACL-藤原    02/11/25  自賠責処理後の初画面の表示を
      *                                     修正
      *　入院機能追加　
      *  01.05.01    NACL-藤原    02/12/11  入外区分追加 
      *  01.05.02    NACL-藤原    03/02/27  入院用県単用項目編集プログラ
      *                                     ム名の追加
      *  01.05.03    NACL-藤原     03/03/03  再印刷ボタン追加
      *
      *  ２００３．４改正対応
      *  01.03.01    NACL-藤原    03/04/17  対象がないときエラーを表示する
      *  01.03.02    NACL-藤原    03/05/22  県番号をパラメタに追加
      *
      *  労災・自賠責入院機能追加
      *  01.04.01    NACL-藤原    03/06/05  労災・自賠責の入院処理追加
      *  01.04.02    NACL-藤原    03/08/05  外来レセ電処理の追加
      *  01.04.03    NACL-藤原    03/11/05  県番号を診療年月から取得
      *  01.04.04    NACL-藤原    04/05/07  処理開始前にジョブ管理の
      *                                     チェックを再度行なう
      *  01.04.05    NACL-藤原    04/07/09  印刷画面への自動遷移
      *  中止処理追加
      *  01.05.01    NACL-藤原    04/09/28 中止ボタンを追加
      *  01.05.02    NACL-藤原    04/12/20 再印刷選択時の処理結果表示中止
      *  01.05.03    NACL-藤原    05/01/13 受診日指定
      *
      *  02.06.01    NACL-藤原    05/06/22 自賠責レセプトの様式追加
      *  02.06.02    NACL-藤原    05/08/30 定期請求との排他制御処理
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  03.02.01    NACL-藤原    06/07/26  自賠責の枠印刷選択機能対応
      *  03.02.02    NACL-藤原    06/08/01  国保主科対応
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.02    NACL-藤原    06/12/22  自賠責健保準拠対応
      *  03.04.03    NACL-藤原    07/03/01  国保レセプト編綴順詳細設定対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/19  自賠責の様式選択対応
      *                                     （平成１９年４月改正様式）
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.02    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.03    NACL-藤原    08/08/07  労災の枠印刷選択機能対応
      *  04.03.04    NACL-藤原    08/08/11  コメント記載箇所選択機能対応
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *  04.05.02    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *  04.05.03    NACL-小豆沢  10/02/18  スクリプト起動パスサーチ対応
      *  04.05.04    NACL-藤原    11/11/07  入外区分表示切替ボタン押下時の修正
      *  04.05.05    NACL-藤原    11/08/02  労災新様式対応
      *  04.05.06    NACL-藤原    11/10/24  「請求しない」設定の請求管理
      *                                     の削除処理の有無対応
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     不明分ありのとき赤字でエラー
      *                                     表示を行う
      *  04.06.02    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.03    NACL-藤原    12/08/20  点検用・提出用初期表示設定対応
      *
      *  04.07.01    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.02    NACL-藤原    12/11/20  リアルタイムプレビュー時の
      *                                     一画面対応
      *  04.07.03    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.04    NACL-藤原    12/11/21  労災レセ電作成対応
      *  04.07.05    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.06    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.07    NACL-藤原    13/12/04  自賠責（労災準拠）のレセプト
      *                                     様式追加対応
      *  04.07.08    NACL-藤原    14/04/22  ＥＦファイル作成対応
      *  04.07.09    NACL-藤原    15/05/14  「自賠責・様式選択」の初期表
      *                                     示変更対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    16/08/24  持参薬対応
      *  04.08.04    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *
      *  05.00.01    NACL-藤原    16/12/12  ＨＡＯＲＩ対応
      *  05.00.02    NACL-藤原    19/02/15  一括作成時の診療年月が一年以
      *                                     上前の確認メッセージ対応
      *                                     Warning 対応
      *  05.00.03    NACL-藤原    19/03/25  元号改正対応
      *  05.00.04    NACL-藤原    20/03/27  令和２年４月改正未対応メッ
      *                                     セージ対応
      *  05.00.05    NACL-藤原    20/04/09  令和２年４月改正対応メッセー
      *                                     ジ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル名称
           COPY    "CPTEMPFL.INC"   REPLACING  //TEMPFLPARA//
                                    BY         //RECEERR//.
      *
      *    エラーファイル名称領域 
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "R03SCR-SPA".
      *
       01  SPA-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //SPA-RECE07//.
           03  SPA-RECE07-SRYKA-KOKUHO
                                   PIC X(02).
           03  SPA-RECE07-SRYKA-KOUIKI
                                   PIC X(02).
           03  SPA-RECE07-RECE-PUTH-T   OCCURS 2.
               05  SPA-RECE07-ZAITAKU-KBN-T
                                   PIC  X(01).
               05  SPA-RECE07-CHOUKI-KBN-T
                                   PIC  X(01).
               05  SPA-RECE07-SRT-LAST-T
                                   PIC  X(01).
               05  SPA-RECE07-KOUGAKU-TEN-T
                                   PIC  9(07).
           03  SPA-RECE08-DRCD-SRYKA
                                   PIC X(02).
           03  SPA-RECE08-SORT-1   PIC X(10).
           03  SPA-RECE08-SORT-2   PIC X(50).
           03  SPA-RECE08-RECESYUBETU-SRT
                                   PIC X(03).
           03  SPA-RECE07-FUMEI-PAGE
                                   PIC 9(05).
           03  SPA-RECE07-HKNJANUM-SYAHO-T.
               05  SPA-RECE07-HKNJANUM-SRT-SYAHO
                                   PIC X(01).
               05  SPA-RECE07-HKNJANUM-SYAHO-MAX
                                   PIC  9(02).
               05  SPA-RECE07-HKNJANUM-SYAHO
                                   PIC X(08)   OCCURS  10.
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
      *
           03  FLG-4001-JIBAIKBN   PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-SRYYM           PIC X(06).
           03  WRK-CREYMD          PIC X(08).
           03  WRK-RIDMSG          PIC X(70).
           03  WRK-YM              PIC X(07). 
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENFLG          PIC 9(01).    
      *  
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(03).
      *     
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC  X(06).
             05  WRK-SHELL-TERMID  PIC  X(16).
             05  WRK-SHELL-SYSYMD  PIC  X(08).  
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *                             
       01  WRK-SHELL-NAME.
      **     05  FILLER             PIC X(16) VALUE
      **                                       "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *              
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "TEIKI1".
           03  WRK-CONS-JOB-JOBID1 PIC 9(07)   VALUE   1.
      *
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPT2".
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR042ERR".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK2001.INC".
      *
           COPY    "CPSK2003.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
           COPY    "CPSK200505.INC".
      *
           COPY    "CPSK2006.INC".
      *
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK9101.INC".
      *
           COPY    "CPSK9102.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    定期請求ジョブチェックサブ
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
            COPY    "CPORCSDAY.INC".
            COPY    "CPORCSLNK.INC".
      *----(01.00.01) LINE ADD START ----------------------------------
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
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
           COPY    "ORCA42SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R03
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
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
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R03         TO  SPA-FREE
      *
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
           IF      SPA-MOTOPG          =   "RERR"
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
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-RIDCD       NOT =   SPACE
                   PERFORM 520-RIDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "R03    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XA01"  OR  "XB01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-R03
                   INITIALIZE              SPA-R01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
               INITIALIZE              SPA-R01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R04"
               WHEN    "R02"
               WHEN    "R01"
               WHEN    "R96"
               WHEN    "R97"
               WHEN    "R98"
                   PERFORM 350-R02SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R99"
                   PERFORM 340-RID2-SET-SEC
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
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-R03-AREA
      *
      *    マシン日付の一年前取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    SMCNDATE-YMD    TO  LNK-DAY6-KIJUN 
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           MOVE    -12             TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN (1:6)
                                   TO  SPA-NAI-SMCNDATE-12
      *
      *    医療情報連携基盤用情報
           INITIALIZE                  SYS-9101-REC
           MOVE    "9101"          TO  SYS-9101-KANRICD
           MOVE    "*"             TO  SYS-9101-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-9101-STYUKYMD
                                       SYS-9101-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-9101-HOSPNUM
           MOVE    SYS-9101-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-9101-REC
           ELSE
               MOVE   SPACE        TO  SYS-9101-REC
           END-IF
           IF      SYS-9101-COOPBASE   =   SPACE
               MOVE    "0"             TO  SYS-9101-COOPBASE
           END-IF
           MOVE    SYS-9101-COOPBASE
                                   TO  SPA-NAI-9101-COOPBASE
      *
      *    ＥＦファイルデータ情報
           INITIALIZE                  SYS-9102-REC
           MOVE    "9102"          TO  SYS-9102-KANRICD
           MOVE    "*"             TO  SYS-9102-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-9102-STYUKYMD
                                       SYS-9102-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-9102-HOSPNUM
           MOVE    SYS-9102-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-9102-REC
           ELSE
               MOVE   SPACE        TO  SYS-9102-REC
           END-IF
           IF      SYS-9102-EFOUT   =   SPACE
               MOVE    "0"             TO  SYS-9102-EFOUT
           END-IF
           MOVE    SYS-9102-EFOUT  TO  SPA-NAI-9102-EFOUT
      *
           IF      SPA-BEDSU       >   ZERO
               MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (2)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL    (2)
               MOVE    2               TO  SPA-GMN-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    1               TO  SPA-GMN-NYUGAIKBN-MAX
           END-IF
           MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
      * 
           MOVE    "1"                 TO  SPA-GMN-PRTKBNL       (1)
           MOVE    "提出用レセプト"    TO  SPA-GMN-PRTKBNMEIL    (1)
           MOVE    "2"                 TO  SPA-GMN-PRTKBNL       (2)
           MOVE    "点検用レセプト（院外処方含）"
                                       TO  SPA-GMN-PRTKBNMEIL    (2)
           MOVE    2                   TO  SPA-GMN-PRTKBN-MAX
           IF      SPA-NAI-9101-COOPBASE
                                       =   "1" OR  "2"
               MOVE    "6"                 TO  SPA-GMN-PRTKBNL   (3)
               MOVE    "連携基盤用"        TO  SPA-GMN-PRTKBNMEIL(3)
               MOVE    3                   TO  SPA-GMN-PRTKBN-MAX
           END-IF
           IF      SPA-NAI-9102-EFOUT  =   "1"
               ADD     1                   TO  SPA-GMN-PRTKBN-MAX
               MOVE    "7"                 TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
               MOVE    "ＥＦファイル用"    TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
           END-IF
           MOVE    SPA-GMN-PRTKBNLST (1)
                                       TO  SPA-GMN-PRTKBN-G
      *
           MOVE    "F"                 TO  SPA-GMN-IKKATU
           MOVE    "F"                 TO  SPA-GMN-KOBETU
           MOVE    "F"                 TO  SPA-DATAKBN1-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN2-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN3-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN4-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN5-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN11-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN21-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN22-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN23-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN24-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN25-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN26-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN31-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN32-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN33-VALUE
      *
      *        レセプト指示コード
           INITIALIZE                  SYS-2001-REC
           MOVE    "2001"              TO  SYS-2001-KANRICD
           MOVE    "*"                 TO  SYS-2001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-2001-STYUKYMD
                                           SYS-2001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2001-HOSPNUM
           MOVE    SYS-2001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2001-REC
               EVALUATE    SYS-2001-SYOKBN
                   WHEN    "1"
                       MOVE    "T"                 TO  SPA-GMN-IKKATU
                       MOVE    "F"                 TO  SPA-GMN-KOBETU
                   WHEN    "2"
                       MOVE    "F"                 TO  SPA-GMN-IKKATU
                       MOVE    "T"                 TO  SPA-GMN-KOBETU
               END-EVALUATE
               EVALUATE    SYS-2001-SYOKBN ALSO    SYS-2001-RECEKBN
                   WHEN    "1"             ALSO    " "             
                   WHEN    "1"             ALSO    "0"             
                       MOVE    "T"             TO  SPA-DATAKBN1-VALUE
                   WHEN    "1"             ALSO    "1"             
                       MOVE    "T"             TO  SPA-DATAKBN2-VALUE
                   WHEN    "1"             ALSO    "2"             
                       MOVE    "T"             TO  SPA-DATAKBN3-VALUE
                   WHEN    "1"             ALSO    "7"             
                       MOVE    "T"             TO  SPA-DATAKBN5-VALUE
                   WHEN    "1"             ALSO    "3"             
                       MOVE    "T"             TO  SPA-DATAKBN11-VALUE
                   WHEN    "1"             ALSO    "4"             
                       MOVE    "T"             TO  SPA-DATAKBN31-VALUE
                   WHEN    "1"             ALSO    "5"             
                       MOVE    "T"             TO  SPA-DATAKBN32-VALUE
                   WHEN    "1"             ALSO    "8"             
                       MOVE    "T"             TO  SPA-DATAKBN33-VALUE
                   WHEN    "1"             ALSO    "6"             
                       MOVE    "T"             TO  SPA-DATAKBN4-VALUE
                   WHEN    "2"             ALSO    " "             
                   WHEN    "2"             ALSO    "0"             
                       MOVE    "T"             TO  SPA-DATAKBN21-VALUE
                   WHEN    "2"             ALSO    "3"             
                       MOVE    "T"             TO  SPA-DATAKBN22-VALUE
                   WHEN    "2"             ALSO    "4"             
                       MOVE    "T"             TO  SPA-DATAKBN23-VALUE
                   WHEN    "2"             ALSO    "5"             
                       MOVE    "T"             TO  SPA-DATAKBN24-VALUE
                   WHEN    "2"             ALSO    "6"             
                       MOVE    "T"             TO  SPA-DATAKBN25-VALUE
                   WHEN    "2"             ALSO    "8"             
                       MOVE    "T"             TO  SPA-DATAKBN26-VALUE
               END-EVALUATE
               IF      SYS-2001-SYOKBN     =   "1"
                   MOVE    SYS-2001-SRYYM      TO  SPA-NAI-SRYYM
                                                   WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-SRYYMH
               END-IF     
           ELSE
               MOVE    "T"                 TO  SPA-GMN-IKKATU
               MOVE    "F"                 TO  SPA-GMN-KOBETU
               MOVE    "T"                 TO  SPA-DATAKBN1-VALUE
           END-IF
      *
           MOVE    "T"                 TO  SPA-GMN-SYOKBN
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "レセプト新規作成" TO  SPA-GMN-SYOKBN-LABEL
               WHEN    "F"
                   MOVE    "作成済レセプト再印刷"
                                              TO  SPA-GMN-SYOKBN-LABEL
                   MOVE    SPACE              TO  SPA-GMN-PRTKBN-G
           END-EVALUATE      
      *
      *    自動遷移区分
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"          TO  SYS-2005-KANRICD
           MOVE    "*"             TO  SYS-2005-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-2005-STYUKYMD
                                       SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-2005-REC
           ELSE
               MOVE   SPACE        TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-AUTODSPKBN =   SPACE
               MOVE    ZERO            TO  SYS-2005-AUTODSPKBN      
           END-IF
           MOVE    SYS-2005-AUTODSPKBN 
                                   TO  SPA-NAI-2005-AUTODSPKBN
      *
      *    区分コード０１のデータ読み込み
           INITIALIZE                  SYS-25-REC
           MOVE    "2005"          TO  SYS-25-KANRICD
           MOVE    "01"            TO  SYS-25-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-25-STYUKYMD
                                       SYS-25-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC      TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-25-REC
           ELSE
               INITIALIZE              SYS-25-REC
               MOVE    SYS-2005-TERMID (1:4)
                                   TO  SYS-25-TBL (1:4)
           END-IF
           IF      SYS-25-INSPECT-ZERO-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-25-INSPECT-ZERO-KBN
           END-IF
           IF      SYS-25-RECEIPT-MAKE-KBN
                                   =   SPACE
               MOVE    "1"             TO  SYS-25-RECEIPT-MAKE-KBN
           END-IF
           IF      SYS-25-INSPECT-INCLUDE-KBN
                                   =   SPACE
               MOVE    "0"             TO
                                       SYS-25-INSPECT-INCLUDE-KBN
           END-IF
           MOVE    SYS-25-INSPECT-ZERO-KBN
                                   TO  SPA-NAI-200501-INSPECT-ZERO-KBN
           MOVE    SYS-25-INSPECT-INCLUDE-KBN
                                   TO
                                   SPA-NAI-200501-INSPECT-INCLUDE
           MOVE    SYS-25-RECEIPT-MAKE-KBN
                                   TO  SPA-NAI-200501-RECEIPT-MAKE-KBN
           EVALUATE    SPA-NAI-200501-RECEIPT-MAKE-KBN
               WHEN    "1"
                   MOVE    SPA-GMN-PRTKBNLST (1)
                                           TO  SPA-GMN-PRTKBN-G
               WHEN    "2"
                   MOVE    SPA-GMN-PRTKBNLST (2)
                                           TO  SPA-GMN-PRTKBN-G
           END-EVALUATE
      *
      *    レセプト・総括編集情報（区分コード０５のデータ読込）
           INITIALIZE                  SYS-200505-REC
           MOVE    "2005"          TO  SYS-200505-KANRICD
           MOVE    "05"            TO  SYS-200505-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-200505-STYUKYMD
                                       SYS-200505-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-200505-HOSPNUM
           MOVE    SYS-200505-REC  TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-200505-REC
           ELSE
               INITIALIZE                  SYS-200505-REC
           END-IF
           MOVE    SYS-200505-HKNJANUM (1)
                                   TO  SPA-NAI-200505-HKNJANUM
      *
           IF      SPA-BEDSU           >   ZERO
               MOVE    3                   TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-IKKATU          =   "T" 
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE 
                   MOVE    7               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               IF      SPA-RIDCD       =   "6666"
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               ELSE
                   IF  SPA-RIDCD           =   "1030"
                       MOVE    SPACE           TO  SPA-RID1-FLG
                                                   SPA-R99-FLG
                       MOVE    SPACE           TO  SPA-RIDCD
                       MOVE    SPACE           TO  SPA-RIDCD2
      *
      *                ガイドメッセージセット処理
                       PERFORM 4501-MESSEGE-SEC
                       GO  TO  330-RID1-SET-EXT
                   END-IF
      *            ジョブ管理チェック処理
                   PERFORM 4501-JOBKANRI-CHK-SEC
                   IF      SPA-ERRCD      =   SPACE
                       PERFORM 450-KOUSHIN-SEC
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "F"
                   MOVE    SPACE       TO  SPA-GMN-PRTKBN-G
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
                                       SPA-R99-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           .
       330-RID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ２）ＯＫ処理
      *****************************************************************
       340-RID2-SET-SEC            SECTION.
      *
           IF      SPA-R99-FLG     =   "OK"
               PERFORM 4501-NEXT-GMN-SEC
           END-IF    
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
                                       SPA-R99-FLG
                                       SPA-R99-CANCEL
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           MOVE    SPACE           TO  SPA-RIDCD3
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           .
       340-RID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻り画面処理
      *****************************************************************
       350-R02SPASET-SEC          SECTION.
      *
           IF      SPA-R01KYOUTU    =   SPACE
               GO  TO  350-R02SPASET-EXT
           END-IF
           MOVE    SPACE           TO  SPA-R01-MOTOPG
      *
           INITIALIZE              SPA-R03-AREA
      *
      *    マシン日付の一年前取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    SMCNDATE-YMD    TO  LNK-DAY6-KIJUN 
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           MOVE    -12             TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN (1:6)
                                   TO  SPA-NAI-SMCNDATE-12
      *
      *    医療情報連携基盤用情報
           INITIALIZE                  SYS-2005-REC
           MOVE    "9101"          TO  SYS-2005-KANRICD
           MOVE    "*"             TO  SYS-2005-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-2005-STYUKYMD
                                       SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-2005-HOSPNUM
           MOVE    SYS-9101-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-9101-REC
           ELSE
               MOVE   SPACE        TO  SYS-9101-REC
           END-IF
           IF      SYS-9101-COOPBASE   =   SPACE
               MOVE    "0"             TO  SYS-9101-COOPBASE
           END-IF
           MOVE    SYS-9101-COOPBASE
                                   TO  SPA-NAI-9101-COOPBASE
      *
      *    ＥＦファイルデータ情報
           INITIALIZE                  SYS-9102-REC
           MOVE    "9102"          TO  SYS-9102-KANRICD
           MOVE    "*"             TO  SYS-9102-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-9102-STYUKYMD
                                       SYS-9102-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-9102-HOSPNUM
           MOVE    SYS-9102-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-9102-REC
           ELSE
               MOVE   SPACE        TO  SYS-9102-REC
           END-IF
           IF      SYS-9102-EFOUT   =   SPACE
               MOVE    "0"             TO  SYS-9102-EFOUT
           END-IF
           MOVE    SYS-9102-EFOUT  TO  SPA-NAI-9102-EFOUT
      *
           IF      SPA-BEDSU       >   ZERO
               MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (2)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL    (2)
               MOVE    2               TO  SPA-GMN-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    1               TO  SPA-GMN-NYUGAIKBN-MAX
           END-IF
           EVALUATE    SPA-R01-NYUGAIKBN 
               WHEN    "1"
                   MOVE    "1"             TO  SPA-GMN-NYUGAIKBN
                   MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEI
               WHEN    "2"    
                   MOVE    "2"             TO  SPA-GMN-NYUGAIKBN
                   MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEI
           END-EVALUATE        
      * 
           MOVE    "1"                 TO  SPA-GMN-PRTKBNL       (1)
           MOVE    "提出用レセプト"    TO  SPA-GMN-PRTKBNMEIL    (1)
           MOVE    "2"                 TO  SPA-GMN-PRTKBNL       (2)
           MOVE    "点検用レセプト（院外処方含）"
                                       TO  SPA-GMN-PRTKBNMEIL    (2)
           MOVE    2                   TO  SPA-GMN-PRTKBN-MAX
           IF      SPA-NAI-9101-COOPBASE
                                       =   "1" OR  "2"
               MOVE    "6"                 TO  SPA-GMN-PRTKBNL   (3)
               MOVE    "連携基盤用"        TO  SPA-GMN-PRTKBNMEIL(3)
               MOVE    3                   TO  SPA-GMN-PRTKBN-MAX
           END-IF
           IF      SPA-NAI-9102-EFOUT  =   "1"
               ADD     1                   TO  SPA-GMN-PRTKBN-MAX
               MOVE    "7"                 TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
               MOVE    "ＥＦファイル用"    TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
           END-IF
           EVALUATE    SPA-R01-PRTKBN
               WHEN    "1"
                   MOVE    SPA-GMN-PRTKBNLST (1)
                                               TO  SPA-GMN-PRTKBN-G
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    SPA-GMN-PRTKBNLST (2)
                                               TO  SPA-GMN-PRTKBN-G
               WHEN    "9"
                   MOVE    SPA-GMN-PRTKBNLST (3)
                                               TO  SPA-GMN-PRTKBN-G
               WHEN    "8"
                   IF      SPA-NAI-9101-COOPBASE
                                           =   "1" OR  "2"
                       MOVE    SPA-GMN-PRTKBNLST (4)
                                               TO  SPA-GMN-PRTKBN-G
                   ELSE
                       MOVE    SPA-GMN-PRTKBNLST (3)
                                               TO  SPA-GMN-PRTKBN-G
                   END-IF
           END-EVALUATE        
      *   
           EVALUATE    SPA-R01-FILEKBN
               WHEN    "1"
                   MOVE    "T"                 TO  SPA-GMN-IKKATU
                   MOVE    "F"                 TO  SPA-GMN-KOBETU
               WHEN    "2"
                   MOVE    "F"                 TO  SPA-GMN-IKKATU
                   MOVE    "T"                 TO  SPA-GMN-KOBETU
           END-EVALUATE
      *
           MOVE    "F"                 TO  SPA-DATAKBN1-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN2-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN3-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN4-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN5-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN11-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN21-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN22-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN23-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN24-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN25-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN26-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN31-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN32-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN33-VALUE
      *
           EVALUATE    SPA-R01-FILEKBN ALSO    SPA-R01-RECEKBN
               WHEN    "1"             ALSO    " "             
               WHEN    "1"             ALSO    "0"             
                   MOVE    "T"             TO  SPA-DATAKBN1-VALUE
               WHEN    "1"             ALSO    "1"             
                   MOVE    "T"             TO  SPA-DATAKBN2-VALUE
               WHEN    "1"             ALSO    "2"             
                   MOVE    "T"             TO  SPA-DATAKBN3-VALUE
               WHEN    "1"             ALSO    "7"             
                   MOVE    "T"             TO  SPA-DATAKBN5-VALUE
               WHEN    "1"             ALSO    "3"             
                   MOVE    "T"             TO  SPA-DATAKBN11-VALUE
               WHEN    "1"             ALSO    "4"             
                   MOVE    "T"             TO  SPA-DATAKBN31-VALUE
               WHEN    "1"             ALSO    "5"             
                   MOVE    "T"             TO  SPA-DATAKBN32-VALUE
               WHEN    "1"             ALSO    "8"             
                   MOVE    "T"             TO  SPA-DATAKBN33-VALUE
               WHEN    "1"             ALSO    "6"             
                   MOVE    "T"             TO  SPA-DATAKBN4-VALUE
               WHEN    "2"             ALSO    " "             
               WHEN    "2"             ALSO    "0"             
                   MOVE    "T"             TO  SPA-DATAKBN21-VALUE
               WHEN    "2"             ALSO    "3"             
                   MOVE    "T"             TO  SPA-DATAKBN22-VALUE
               WHEN    "2"             ALSO    "4"             
                   MOVE    "T"             TO  SPA-DATAKBN23-VALUE
               WHEN    "2"             ALSO    "5"             
                   MOVE    "T"             TO  SPA-DATAKBN24-VALUE
               WHEN    "2"             ALSO    "6"             
                   MOVE    "T"             TO  SPA-DATAKBN25-VALUE
               WHEN    "2"             ALSO    "8"             
                   MOVE    "T"             TO  SPA-DATAKBN26-VALUE
           END-EVALUATE
      *
           IF      SPA-R01-FILEKBN     =   "1"        
               MOVE    SPA-R01-SRYYM       TO  SPA-NAI-SRYYM
               IF      SPA-R01-SRYYM       =   ZERO
                   INITIALIZE                  SPA-GMN-SRYYM
                                               SPA-NAI-SRYYMH
               ELSE   
                   MOVE    SPA-R01-SRYYM       TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-SRYYMH
               END-IF
           ELSE        
               INITIALIZE                  SPA-GMN-SRYYM
                                           SPA-NAI-SRYYM
                                           SPA-NAI-SRYYMH
           END-IF    
      *
           MOVE    SPA-R01-AUTODSPKBN  TO  SPA-NAI-2005-AUTODSPKBN
           MOVE    SPA-R01-INSPECT-ZERO-KBN
                                       TO
                                       SPA-NAI-200501-INSPECT-ZERO-KBN
           MOVE    SPA-R01-INSPECT-INCLUDE-KBN
                                       TO
                                   SPA-NAI-200501-INSPECT-INCLUDE
      *
           MOVE    SPA-R01-SYOKBN      TO  SPA-GMN-SYOKBN
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "レセプト新規作成"  TO  SPA-GMN-SYOKBN-LABEL
                   MOVE    SPACE               TO  SPA-GMN-PRTKBN-LABEL
                   IF      SPA-R01-PRTKBN      =   "1" OR  "8" OR  "9"
                     CONTINUE
                   ELSE
                     EVALUATE    SPA-NAI-200501-INSPECT-ZERO-KBN
                               ALSO   SPA-NAI-200501-INSPECT-INCLUDE
                       WHEN    "1"     ALSO     "0"
                           MOVE    "０点公費記載設定"
                                               TO  SPA-GMN-PRTKBN-LABEL
                       WHEN    "0"     ALSO    "1"
                           MOVE    "包括診療分・持参薬記載設定"
                                               TO  SPA-GMN-PRTKBN-LABEL
                       WHEN    "1"     ALSO    "1"
                           MOVE
                               "０点公費・包括診療分・持参薬記載設定"
                                               TO  SPA-GMN-PRTKBN-LABEL
      ******           WHEN    "0"     ALSO    "0"
      ******               MOVE    "０点公費を記載しない"
      ******                                   TO  SPA-GMN-PRTKBN-LABEL
                     END-EVALUATE
                   END-IF
               WHEN    "F"
                   MOVE    "作成済レセプト再印刷"
                                              TO  SPA-GMN-SYOKBN-LABEL
                   MOVE    SPACE              TO  SPA-GMN-PRTKBN-G
                   MOVE    SPACE              TO  SPA-GMN-PRTKBN-LABEL
           END-EVALUATE      
      *
           IF      SPA-GMN-IKKATU          =   "T" 
               MOVE    2               TO  SPA-GMN-CUR
           ELSE 
               MOVE    7               TO  SPA-GMN-CUR
           END-IF
           .
       350-R02SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *     再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-REPRINT-SEC
      *     主科設定
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 490-SYUKA-ENTRY-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
      *     生保まとめ入力
               WHEN    "CLICKED"       ALSO    "B09"
                    PERFORM 460-SEH-ENTRY-SEC
      *     印刷区分
               WHEN    "CLICKED"       ALSO    "B07"
               WHEN    "CLICKED"       ALSO    "SYOKBN"
                   PERFORM 410-SYOKBN-CHK-SEC
      *    未請求患者設定
               WHEN    "CLICKED"       ALSO    "B22"
                   PERFORM 490-MISEIKYU-ENTRY-SEC
      *    入外区分表示切替え処理
               WHEN    "CLICKED"       ALSO    "B23"
                   PERFORM 430-HNYUGAIKBN-CHANGE-SEC
      *     作成区分
               WHEN    "CLICKED"       ALSO    "B08"
                   MOVE    "F"                 TO  SPA-DATAKBN1-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN2-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN3-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN4-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN5-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN11-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN21-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN22-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN23-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN24-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN25-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN26-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN31-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN32-VALUE
                   MOVE    "F"                 TO  SPA-DATAKBN33-VALUE
                   IF      SPA-GMN-IKKATU      =   "T"
                       MOVE    "F"             TO  SPA-GMN-IKKATU
                       MOVE    "T"             TO  SPA-GMN-KOBETU
                       MOVE    "T"             TO  SPA-DATAKBN21-VALUE
                   ELSE
                       MOVE    "T"             TO  SPA-GMN-IKKATU
                       MOVE    "F"             TO  SPA-GMN-KOBETU
                       MOVE    "T"             TO  SPA-DATAKBN1-VALUE 
                   END-IF
                   IF      SPA-BEDSU           >   ZERO
                       MOVE    3                   TO  SPA-GMN-CUR
                   ELSE 
                       IF      SPA-GMN-IKKATU      =   "T"
                           MOVE    2                   TO  SPA-GMN-CUR
                       ELSE
                           MOVE    7                   TO  SPA-GMN-CUR    
                       END-IF
                   END-IF             
      *    一括帳票区分
               WHEN    "CLICKED"       ALSO    "DATAKBN1"
               WHEN    "CLICKED"       ALSO    "DATAKBN2"
               WHEN    "CLICKED"       ALSO    "DATAKBN3"
               WHEN    "CLICKED"       ALSO    "DATAKBN4"
               WHEN    "CLICKED"       ALSO    "DATAKBN5"
               WHEN    "CLICKED"       ALSO    "DATAKBN31"
               WHEN    "CLICKED"       ALSO    "DATAKBN32"
               WHEN    "CLICKED"       ALSO    "DATAKBN33"
               WHEN    "CLICKED"       ALSO    "DATAKBN11"
                   PERFORM 400-DATAKBN11-SEC
      *    個別帳票区分
               WHEN    "CLICKED"       ALSO    "DATAKBN21"
               WHEN    "CLICKED"       ALSO    "DATAKBN22"
               WHEN    "CLICKED"       ALSO    "DATAKBN23"
               WHEN    "CLICKED"       ALSO    "DATAKBN24"
               WHEN    "CLICKED"       ALSO    "DATAKBN25"
               WHEN    "CLICKED"       ALSO    "DATAKBN26"
                   PERFORM 400-DATAKBN21-SEC
      *    一括作成
               WHEN    "CLICKED"       ALSO    "IKKATU"
                   PERFORM 400-IKKATU-SEC
      *    個別作成
               WHEN    "CLICKED"       ALSO    "KOBETU"
                   PERFORM 400-KOBETU-SEC
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
           PERFORM 410-ENTRY-SYORI-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       410-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       410-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *   入力チェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
      *
                   IF      SPA-GMN-IKKATU         =   "F"
                       MOVE    "0007"             TO  SPA-ERRCD
                       MOVE    3                  TO  SPA-GMN-CUR
                       GO  TO  410-INPUT-CHK-EXT
                   END-IF
           END-EVALUATE
      * 
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
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
           MOVE    R03-SRYYM           TO  SPA-GMN-SRYYM
           MOVE    R03-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
           MOVE    R03-PRTKBN          TO  SPA-GMN-PRTKBN-G
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
           PERFORM 41021-SRYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41022-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41023-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月（一括）チェック処理
      *****************************************************************
       41021-SRYYM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-IKKATU         =   "F"
               GO  TO  41021-SRYYM-CHK-EXT
           END-IF
      *
           MOVE    ZERO               TO  SPA-NAI-SRYYM
           MOVE    SPACE              TO  SPA-NAI-SRYYMH
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-SRYYM
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SGDAY-SDAY         TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG3(1:16)   TO  SPA-NAI-SRYYMH
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF 
      *
           .
       41021-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       41022-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    ZERO               TO  WRK-HENFLG
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-NYUGAIKBN-MAX
                   OR      WRK-HENFLG =   1
               IF      SPA-GMN-NYUGAIKBN 
                               =   SPA-GMN-NYUGAIKBNL (IDY)
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDY)
                                      TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1          TO  WRK-HENFLG
               END-IF
           END-PERFORM
           IF      WRK-HENFLG         =   ZERO
               MOVE    "0006"               TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF 
           .
       41022-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成区分  入力処理
      *****************************************************************
       41023-PRTKBN-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-PRTKBN-LABEL
      *
           IF      SPA-GMN-PRTKBN      =   SPACE
               IF      SPA-GMN-SYOKBN      =   "T"
                   EVALUATE    SPA-NAI-200501-RECEIPT-MAKE-KBN
                       WHEN    "1"
                           MOVE    SPA-GMN-PRTKBNLST (1)
                                                   TO  SPA-GMN-PRTKBN-G
                       WHEN    "2"
                           MOVE    SPA-GMN-PRTKBNLST (2)
                                                   TO  SPA-GMN-PRTKBN-G
                   END-EVALUATE
               END-IF
           ELSE
               MOVE    ZERO                TO  WRK-HENFLG
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTKBN-MAX
                       OR      WRK-HENFLG  =   1
                   IF      SPA-GMN-PRTKBN 
                                   =   SPA-GMN-PRTKBNL (IDY)
                       MOVE    SPA-GMN-PRTKBNLST (IDY)
                                           TO  SPA-GMN-PRTKBN-G
                       MOVE    1           TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   EVALUATE    SPA-NAI-200501-RECEIPT-MAKE-KBN
                       WHEN    "1"
                           MOVE    SPA-GMN-PRTKBNLST (1)
                                                   TO  SPA-GMN-PRTKBN-G
                       WHEN    "2"
                           MOVE    SPA-GMN-PRTKBNLST (2)
                                                   TO  SPA-GMN-PRTKBN-G
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF    (  SPA-NAI-SRYYM       <   200804 )
           AND   (  SPA-GMN-IKKATU      =   "T"    )
           AND   (  SPA-GMN-PRTKBN      =   "2" 
                                        OR  "6"    )
               MOVE    "0012"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
           ELSE
               IF    ( SPA-GMN-PRTKBN      =   "2" )
                   EVALUATE    SPA-NAI-200501-INSPECT-ZERO-KBN
                               ALSO   SPA-NAI-200501-INSPECT-INCLUDE
                       WHEN    "1"    ALSO    "0"
                           MOVE    "０点公費記載設定"
                                               TO  SPA-GMN-PRTKBN-LABEL
                       WHEN    "0"    ALSO    "1"
                           MOVE    "包括診療分・持参薬記載設定"
                                               TO  SPA-GMN-PRTKBN-LABEL
                       WHEN    "1"    ALSO    "1"
                           MOVE
                               "０点公費・包括診療分・持参薬記載設定"
                                               TO  SPA-GMN-PRTKBN-LABEL
      ******           WHEN    "0"    ALSO    "0"
      ******               MOVE    "０点公費を記載しない"
      ******                                   TO  SPA-GMN-PRTKBN-LABEL
                   END-EVALUATE
               ELSE
                   MOVE    SPACE           TO   SPA-GMN-PRTKBN-LABEL
               END-IF
           END-IF
           .
       41023-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *   
      *    医保分のみ連携基盤・ＥＦファイルを選択可能
           IF      SPA-GMN-PRTKBN   =   "6" OR  "7"
               EVALUATE    TRUE
                   WHEN    SPA-DATAKBN1-VALUE  =   "T"
                       CONTINUE
                   WHEN    SPA-DATAKBN2-VALUE  =   "T"
                   WHEN    SPA-DATAKBN3-VALUE  =   "T"
                   WHEN    SPA-DATAKBN5-VALUE  =   "T"
                       MOVE    "0011"          TO  SPA-ERRCD
                   WHEN    SPA-DATAKBN21-VALUE =   "T"
                           CONTINUE
                   WHEN    SPA-DATAKBN11-VALUE =   "T"
                   WHEN    SPA-DATAKBN31-VALUE =   "T"
                   WHEN    SPA-DATAKBN32-VALUE =   "T"
                   WHEN    SPA-DATAKBN4-VALUE  =   "T"
                   WHEN    SPA-DATAKBN22-VALUE =   "T"
                   WHEN    SPA-DATAKBN23-VALUE =   "T"
                   WHEN    SPA-DATAKBN24-VALUE =   "T"
                   WHEN    SPA-DATAKBN25-VALUE =   "T"
                       MOVE    "0011"          TO  SPA-ERRCD
               END-EVALUATE
          END-IF
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
      *********AND     JOB-TERMID      =   SPA-TERMID
                   MOVE    "6666"          TO  SPA-RIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "R03"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
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
      *    処理区分  入力処理
      *****************************************************************
       410-SYOKBN-CHK-SEC                SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "B07"
                   EVALUATE    R03-SYOKBN-VALUE
                       WHEN    "T"
                           MOVE    "F"             TO  R03-SYOKBN-VALUE
                       WHEN    "F"
                           MOVE    "T"             TO  R03-SYOKBN-VALUE
                   END-EVALUATE
           END-EVALUATE
      *
           MOVE    R03-SYOKBN-VALUE    TO  SPA-GMN-SYOKBN
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "レセプト新規作成" TO  SPA-GMN-SYOKBN-LABEL
                   MOVE    SPA-GMN-PRTKBNLST (1)
                                              TO  R03-PRTKBN
               WHEN    "F"
                   MOVE    "作成済レセプト再印刷"
                                              TO  SPA-GMN-SYOKBN-LABEL
                   MOVE    SPACE              TO  R03-PRTKBN
           END-EVALUATE 
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-SYOKBN-CHK-EXT
           END-IF
           .
       410-SYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括帳票区分選択処理
      *****************************************************************
       400-DATAKBN11-SEC          SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "CLICKED"       ALSO    "DATAKBN1"
                   MOVE    R03-DATAKBN1  TO  SPA-DATAKBN1-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN2"
                   MOVE    R03-DATAKBN2  TO  SPA-DATAKBN2-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN3"
                   MOVE    R03-DATAKBN3  TO  SPA-DATAKBN3-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN4"
                   MOVE    R03-DATAKBN4  TO  SPA-DATAKBN4-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN5"
                   MOVE    R03-DATAKBN5  TO  SPA-DATAKBN5-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN31"
                   MOVE    R03-DATAKBN31 TO  SPA-DATAKBN31-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN32"
                   MOVE    R03-DATAKBN32 TO  SPA-DATAKBN32-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN33"
                   MOVE    R03-DATAKBN33 TO  SPA-DATAKBN33-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN11"
                   MOVE    R03-DATAKBN11  TO  SPA-DATAKBN11-VALUE
           END-EVALUATE
      *
           MOVE    "T"           TO  SPA-GMN-IKKATU
           MOVE    "F"           TO  SPA-GMN-KOBETU
      *
           MOVE    2             TO  SPA-GMN-CUR     
           .
       400-DATAKBN11-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別帳票区分選択処理
      *****************************************************************
       400-DATAKBN21-SEC          SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "CLICKED"       ALSO    "DATAKBN21"
                   MOVE    R03-DATAKBN21  TO  SPA-DATAKBN21-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN22"
                   MOVE    R03-DATAKBN22  TO  SPA-DATAKBN22-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN23"
                   MOVE    R03-DATAKBN23  TO  SPA-DATAKBN23-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN24"
                   MOVE    R03-DATAKBN24  TO  SPA-DATAKBN24-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN25"
                   MOVE    R03-DATAKBN25  TO  SPA-DATAKBN25-VALUE
               WHEN    "CLICKED"       ALSO    "DATAKBN26"
                   MOVE    R03-DATAKBN26  TO  SPA-DATAKBN26-VALUE
           END-EVALUATE
      *
           MOVE    "F"           TO  SPA-GMN-IKKATU
           MOVE    "T"           TO  SPA-GMN-KOBETU
      *
           MOVE    7             TO  SPA-GMN-CUR     
           .
       400-DATAKBN21-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括作成選択処理
      *****************************************************************
       400-IKKATU-SEC          SECTION.
      *
           MOVE    R03-IKKATU          TO  SPA-GMN-IKKATU
      *     
           MOVE    "T"                 TO  SPA-DATAKBN1-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN2-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN3-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN4-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN5-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN11-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN21-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN22-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN23-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN24-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN25-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN26-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN31-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN32-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN33-VALUE
      *
           MOVE    2                   TO  SPA-GMN-CUR     
           .
       400-IKKATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別作成選択処理
      *****************************************************************
       400-KOBETU-SEC          SECTION.
      *
           MOVE    R03-KOBETU          TO  SPA-GMN-KOBETU
      *     
           MOVE    "F"                 TO  SPA-DATAKBN1-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN2-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN3-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN4-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN5-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN11-VALUE
           MOVE    "T"                 TO  SPA-DATAKBN21-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN22-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN23-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN24-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN25-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN26-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN31-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN32-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN33-VALUE
      *
           MOVE    7                   TO  SPA-GMN-CUR     
           .
       400-KOBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    生活保護まとめ入力処理
      *****************************************************************
       460-SEH-ENTRY-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  460-SEH-ENTRY-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-RECEERR
                   STRING  "処理中です【"    DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING                          
                   GO  TO  460-SEH-ENTRY-EXT
               END-IF
           END-IF    
      * 
           MOVE    "R03"               TO  SPA-MOTOPG
           MOVE    "R98"               TO  SPA-SAKIPG
      *
      *    画面移行用のパラメタ変更
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       460-SEH-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科入力処理
      *****************************************************************
       490-SYUKA-ENTRY-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-SYUKA-ENTRY-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-RECEERR
                   STRING  "処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING                          
                   GO  TO  490-SYUKA-ENTRY-EXT
               END-IF
           END-IF    
      * 
           MOVE    "R03"               TO  SPA-MOTOPG
           MOVE    "R97"               TO  SPA-SAKIPG
      *
      *    画面移行用のパラメタ変更
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-SYUKA-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定処理
      *****************************************************************
       490-MISEIKYU-ENTRY-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-MISEIKYU-ENTRY-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-RECEERR
                   STRING  "処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING                          
                   GO  TO  490-MISEIKYU-ENTRY-EXT
               END-IF
           END-IF    
      * 
           MOVE    "R03"               TO  SPA-MOTOPG
           MOVE    "R96"               TO  SPA-SAKIPG
      *
      *    画面移行用のパラメタ変更
           PERFORM 45011-PARA-HENSYU-SEC
           MOVE    "R03"               TO  SPA-R01-MOTOPG
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-MISEIKYU-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替処理
      *****************************************************************
       430-HNYUGAIKBN-CHANGE-SEC          SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-GMN-NYUGAIKBN
               WHEN    "1"
                   MOVE    SPA-GMN-NYUGAIKBNLST (2)
                                               TO  R03-NYUGAIKBN
               WHEN    "2"
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                               TO  R03-NYUGAIKBN
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  430-HNYUGAIKBN-CHANGE-EXT
           END-IF
      *
           IF      SPA-GMN-IKKATU          =   "T" 
               MOVE    2               TO  SPA-GMN-CUR
           ELSE 
               MOVE    7               TO  SPA-GMN-CUR
           END-IF
           .
       430-HNYUGAIKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               IF      SPA-R01-JOB-PARAM  NOT =   SPACE
      *            ＨＡＯＲＩのとき画面移行用のパラメタ設定
                   PERFORM 45011-PARA-HENSYU-SEC
               END-IF
               GO  TO  450-TOUROKU-EXT
           END-IF    
      *
      *    画面移行用のパラメタ変更
           PERFORM 45011-PARA-HENSYU-SEC
      *
      *    自賠責（新様式）のとき健保準拠か労災準拠のチェック
      *    平成２９年７月診療分以降または個別作成時はエラーとしない
           IF    ( SPA-R01-RECEKBN     =   "4" )
               EVALUATE    SPA-R01-FILEKBN
                   WHEN    "1"
                       INITIALIZE                  SYS-4001-REC
                       MOVE    "4001"          TO  SYS-4001-KANRICD
                       MOVE    "*"             TO  SYS-4001-KBNCD
                       MOVE    SPA-NAI-SRYYM   TO 
                                               SYS-4001-STYUKYMD(1:6)
                       MOVE    "01"            TO 
                                               SYS-4001-STYUKYMD(7:2)
                       MOVE    SYS-4001-STYUKYMD
                                               TO  SYS-4001-EDYUKYMD
grpsys                 MOVE    SPA-HOSPNUM     TO  SYS-4001-HOSPNUM
                       MOVE    SYS-4001-REC    TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-4001-REC
                       ELSE
                           INITIALIZE                  SYS-4001-REC
                       END-IF
                       IF    ( SYS-4001-JIBAIKBN   =   "2"    )
                       AND   ( SPA-NAI-SRYYM       <   201707 )
                           MOVE    "0009"              TO  SPA-ERRCD
                           GO  TO  450-TOUROKU-EXT
                       END-IF
                   WHEN    "2"
                       MOVE    ZERO                TO  FLG-4001-JIBAIKBN
      *
                       INITIALIZE                      SYS-4001-REC
                       MOVE    "4001"              TO  SYS-4001-KANRICD
                       MOVE    "*"                 TO  SYS-4001-KBNCD
grpsys                 MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
                       MOVE    SYS-4001-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
grpsys                 PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM  900-SYSKANRI-READ-SEC
                           PERFORM  UNTIL   FLG-SYSKANRI   =    1
                               MOVE    MCPDATA-REC     TO  SYS-4001-REC
                               IF      SYS-4001-JIBAIKBN   =   "2"
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-4001-JIBAIKBN
                                   MOVE    1       TO  FLG-SYSKANRI
                               END-IF
      *
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key8"          TO  MCP-PATHNAME
                               PERFORM 900-SYSKANRI-READ-SEC
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
      *****            IF      FLG-4001-JIBAIKBN   =   ZERO
      *****                MOVE    "0010"              TO  SPA-ERRCD
      *****                GO  TO  450-TOUROKU-EXT
      *****            END-IF
               END-EVALUATE           
           END-IF
      *
      *****主科設定済かチェック
      *****IF    ( SPA-SYUKA-KBN               =   "1" )
      *****AND   ( SPA-GMN-IKKATU              =   "T" )
      *****AND   ( SPA-GMN-SYOKBN              =   "T" )
      *****    IF    ( SPA-DATAKBN1-VALUE          =   "T" )
      *****    OR    ( SPA-DATAKBN2-VALUE          =   "T" ) 
      *****    OR    ( SPA-DATAKBN3-VALUE          =   "T" ) 
      *****        INITIALIZE                  SYUKA-REC
      *****        MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
      *****        MOVE    SPA-NAI-SRYYM   TO  SYUKA-SRYYM
      *****        MOVE    SPA-GMN-NYUGAIKBN
      *****                                TO  SYUKA-NYUGAIKBN
      *****        EVALUATE    TRUE
      *****            WHEN    SPA-DATAKBN1-VALUE  =   "T"
      *****                MOVE    "%"         TO  SYUKA-TEISYUTUSAKI
      *****            WHEN    SPA-DATAKBN1-VALUE  =   "T"
      *****                MOVE    "1"         TO  SYUKA-TEISYUTUSAKI
      *****            WHEN    SPA-DATAKBN1-VALUE  =   "T"
      *****                MOVE    "2"         TO  SYUKA-TEISYUTUSAKI
      *****        END-EVALUATE
      *****        MOVE    SYUKA-REC       TO  MCPDATA-REC
      *****        MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
      *****        MOVE    "key12"         TO  WRK-MCP-PATHNAME
      *****        PERFORM 900-SYUKA-READ-SEC
      *****        IF      FLG-SYUKA       =   1 
      *****            MOVE    "0100"              TO  SPA-ERRCD
      *****            GO  TO  450-TOUROKU-EXT
      *****        END-IF
      *****    END-IF
      *****END-IF
      *   
      *    再印刷のときのチェック
           EVALUATE    SPA-GMN-SYOKBN     
               WHEN    "F"
                   PERFORM 4501-REPRINT-CHK-SEC
      *
                   IF      SPA-RECE07-SRYYM    =   SPACE
                       MOVE    "0002"              TO  SPA-ERRCD
                       GO  TO  450-TOUROKU-EXT
                   ELSE                    
                       MOVE    SPA-RECE07-KEY
                                          TO  SPA-R01-RECE07-KEY
                       MOVE    SPA-RECE07-RECEKBN
                                          TO  SPA-R01-RECE07-RECEKBN
                       MOVE    SPA-RECE07-SRYYM
                                          TO  SPA-R01-RECE07-SRYYM
                       MOVE    SPA-RECE07-NYUGAIKBN
                                          TO  SPA-R01-RECE07-NYUGAIKBN
                       MOVE    SPA-RECE07-SYORISBT
                                          TO  SPA-R01-RECE07-SYORISBT
                       MOVE    SPA-RECE07-STYMD
                                          TO  SPA-R01-RECE07-STYMD
                       MOVE    SPA-RECE07-EDYMD
                                          TO  SPA-R01-RECE07-EDYMD
                       MOVE    SPA-RECE07-HSEQNUM
                                          TO  SPA-R01-RECE07-HSEQNUM
                       MOVE    SPA-RECE07-HKNJANUM
                                          TO  SPA-R01-RECE07-HKNJANUM
                       MOVE    SPA-RECE07-SRYKA-KOKUHO
                                          TO SPA-R01-RECE07-SRYKA-KOKUHO
                       MOVE    SPA-RECE07-SRYKA-KOUIKI
                                          TO SPA-R01-RECE07-SRYKA-KOUIKI
                       MOVE    SPA-RECE07-RECE-PUTH-T (1)
                                          TO SPA-R01-RECE07-RECE-PUTH(1)
                       MOVE    SPA-RECE07-RECE-PUTH-T (2)
                                          TO SPA-R01-RECE07-RECE-PUTH(2)
                       MOVE    SPA-RECE08-DRCD-SRYKA
                                          TO SPA-R01-RECE08-DRCD-SRYKA
                       MOVE    SPA-RECE08-SORT-1
                                          TO SPA-R01-RECE08-SORT-1
                       MOVE    SPA-RECE08-SORT-2
                                          TO SPA-R01-RECE08-SORT-2
                       MOVE    SPA-RECE08-RECESYUBETU-SRT
                                          TO
                                          SPA-R01-RECE08-RECESYUBETU-SRT
                       MOVE    SPA-RECE07-FUMEI-PAGE
                                          TO  SPA-R01-RECE07-FUMEI-PAGE
                       MOVE    SPA-RECE07-HKNJANUM-SYAHO-T
                                          TO
                                       SPA-R01-RECE07-HKNJANUM-SYAHO-T
      *
                       EVALUATE    SPA-RECE07-PRTKBN
                           WHEN    SPACE
                               MOVE    SPA-GMN-PRTKBNLST (1)
                                              TO  SPA-GMN-PRTKBN-G
                               MOVE    "1"    TO  SPA-R01-PRTKBN
                           WHEN    "1"
                               MOVE    SPA-GMN-PRTKBNLST (2)
                                              TO  SPA-GMN-PRTKBN-G
                               MOVE    "2"    TO  SPA-R01-PRTKBN
                           WHEN    "2"
                               MOVE    SPA-GMN-PRTKBNLST (2)
                                              TO  SPA-GMN-PRTKBN-G
                               MOVE    "3"    TO  SPA-R01-PRTKBN
                           WHEN    "3"
                               MOVE    SPA-GMN-PRTKBNLST (2)
                                              TO  SPA-GMN-PRTKBN-G
                               MOVE    "4"    TO  SPA-R01-PRTKBN
                           WHEN    "4"
                               MOVE    SPA-GMN-PRTKBNLST (2)
                                              TO  SPA-GMN-PRTKBN-G
                               MOVE    "5"    TO  SPA-R01-PRTKBN
                       END-EVALUATE
                   END-IF    
           END-EVALUATE
      *
      *    ＨＡＯＲＩ以外の一括作成時の診療年月のチェック
      *    システム日付の一年前以前は警告表示を行う
           IF    ( SPA-GMN-SYOKBN  =   "T"   )
           AND   ( SPA-GMN-IKKATU  =   "T"   )
           AND   ( SPA-R01-JOB-PARAM
                                   =   SPACE )
               IF      SPA-NAI-SRYYM   <=  SPA-NAI-SMCNDATE-12
                   MOVE    SPACE           TO  SPA-RID1-FLG
                                               SPA-R99-FLG
                   MOVE    SPACE           TO  SPA-RIDCD
                   MOVE    SPACE           TO  SPA-RIDCD2
      *
                   MOVE    "1030"          TO  SPA-RIDCD
                   GO  TO  450-TOUROKU-EXT
               END-IF 
           END-IF
      *
      *    ガイドメッセージセット処理
           PERFORM 4501-MESSEGE-SEC
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージセット処理
      *****************************************************************
       4501-MESSEGE-SEC            SECTION.
      *
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "4"
               WHEN    "5"
               WHEN    "8"
                   EVALUATE    SPA-GMN-SYOKBN  ALSO    SPA-R01-FILEKBN
                       WHEN    "T"             ALSO    "1" 
                           MOVE    "1009"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "1"
                           MOVE    "1010"              TO  SPA-RIDCD
                       WHEN    "T"             ALSO    "2" 
                           MOVE    "1011"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "2"
                           MOVE    "1012"              TO  SPA-RIDCD
                   END-EVALUATE
               WHEN    "3"    
                   EVALUATE    SPA-GMN-SYOKBN  ALSO    SPA-R01-FILEKBN
                       WHEN    "T"             ALSO    "1" 
                           MOVE    "1005"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "1"
                           MOVE    "1006"              TO  SPA-RIDCD
                       WHEN    "T"             ALSO    "2" 
                           MOVE    "1007"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "2"
                           MOVE    "1008"              TO  SPA-RIDCD
                   END-EVALUATE
               WHEN    "6"    
                   EVALUATE    SPA-GMN-SYOKBN  ALSO    SPA-R01-FILEKBN
                       WHEN    "T"             ALSO    "1" 
                           MOVE    "1013"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "1"
                           MOVE    "1014"              TO  SPA-RIDCD
                       WHEN    "T"             ALSO    "2" 
                           MOVE    "1015"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "2"
                           MOVE    "1016"              TO  SPA-RIDCD
                   END-EVALUATE
               WHEN    OTHER
                   EVALUATE    SPA-GMN-SYOKBN  ALSO    SPA-R01-FILEKBN
                       WHEN    "T"             ALSO    "1" 
                           MOVE    "1001"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "1"
                           MOVE    "1002"              TO  SPA-RIDCD
                       WHEN    "T"             ALSO    "2" 
                           MOVE    "1003"              TO  SPA-RIDCD
                       WHEN    "F"             ALSO    "2"
                           MOVE    "1004"              TO  SPA-RIDCD
                   END-EVALUATE
           END-EVALUATE
           .
       4501-MESSEGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-RECEERR
                   STRING  "レセプト作成処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING                          
               END-IF
           END-IF    
      *
      *    入院の処理のとき定期請求のチェックをする
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-NYUGAIKBN   
                                   =   "1"   )
               INITIALIZE                  STEIKIJOB-AREA
               MOVE    "1"             TO  STEIKIJOB-RECEKBN
               MOVE    SPA-GMN-NYUGAIKBN
                                       TO  STEIKIJOB-NYUGAIKBN
               IF    ( SPA-GMN-IKKATU  =   "T" )
                   MOVE    "1"         TO  STEIKIJOB-SYOKBN
                   MOVE    SPA-NAI-SRYYM
                                       TO  STEIKIJOB-SRYYM
               ELSE
                   MOVE    "2"         TO  STEIKIJOB-SYOKBN
               END-IF
      *
               CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                                   SPA-AREA
               IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    STEIKIJOB-ERRCD     TO  SPA-ERRCD
                   MOVE    STEIKIJOB-ERRMSG    TO  WRK-RECEERR
               END-IF
           END-IF        
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷チェック処理
      *****************************************************************
       4501-REPRINT-CHK-SEC         SECTION.
      *
           INITIALIZE              SPA-RECE07-REC
      *
           INITIALIZE                  RECE07-REC
grpsys     MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2" OR  "7"
               MOVE    "HC06"              TO  RECE07-PRTID
           ELSE
               IF      SPA-R01-RECEKBN     =   "3"
                   MOVE    "HC21"              TO  RECE07-PRTID
               END-IF
               IF      SPA-R01-RECEKBN     =   "4" OR  "5" OR  "8"
                   MOVE    "HC23"              TO  RECE07-PRTID
               END-IF
               IF      SPA-R01-RECEKBN     =   "6"
                   MOVE    "HC25"              TO  RECE07-PRTID
               END-IF
           END-IF            
           IF      SPA-R01-FILEKBN         =   "1"
               MOVE    SPA-R01-SRYYM       TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    SPA-R01-RECEKBN     TO  RECE07-RECEKBN
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "2"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    RECE07-REC          TO  MCPDATA-REC
           PERFORM 900-RECE07-INV-SEC
           IF      FLG-RECE07          =   ZERO
      *        自賠責で個別作成時に選択した様式と違う場合エラーとする
               IF    ( SPA-R01-FILEKBN         =   "2"                 )
               AND   ( SPA-R01-RECEKBN         =   "4" OR  "5" OR  "8" )
                   IF      SPA-R01-RECEKBN     NOT =   RECE07-RECEKBN
                       GO  TO   4501-REPRINT-EXT
                   END-IF
               END-IF
      * 
               MOVE    RECE07-REC          TO  SPA-RECE07-REC
               MOVE    "99999999"          TO  SPA-RECE07-HKNJANUM 
               MOVE    "00"                TO  SPA-RECE07-SRYKA 
                                               SPA-RECE07-SRYKA-KOKUHO
                                               SPA-RECE07-SRYKA-KOUIKI
               INITIALIZE                      SPA-RECE07-RECE-PUTH-T(1)
                                               SPA-RECE07-RECE-PUTH-T(2)
               MOVE    SPACE               TO  SPA-RECE08-DRCD-SRYKA
                                               SPA-RECE08-SORT-1
                                               SPA-RECE08-SORT-2
                                              SPA-RECE08-RECESYUBETU-SRT
               MOVE    ZERO                TO  SPA-RECE07-FUMEI-PAGE
               INITIALIZE                  SPA-RECE07-HKNJANUM-SYAHO-T
               MOVE    "%%%%%%%%"      TO  SPA-RECE07-HKNJANUM-SYAHO (1)
               MOVE    1               TO  SPA-RECE07-HKNJANUM-SYAHO-MAX
               MOVE    "9"             TO  SPA-RECE07-HKNJANUM-SRT-SYAHO
      *
      *        一括作成で社保処理のとき編綴順設定の有無チェック
               IF    ( SPA-R01-FILEKBN         =   "1"         )
               AND   ( SPA-R01-RECEKBN         =   "0" OR  "1" )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key6"              TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-RECESYUBETU-SRT
                                                   TO
                                           SPA-RECE08-RECESYUBETU-SRT
                   END-IF
               END-IF  
      *
      *        一括作成で国保処理のとき編綴順設定の有無チェック
               IF    ( SPA-R01-FILEKBN         =   "1"         )
               AND   ( SPA-R01-RECEKBN         =   "0" OR  "2" )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key6"              TO  WRK-MCP-PATHNAME
                   MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-HKNJANUM TO  SPA-RECE07-HKNJANUM
                       MOVE    RECE07-HSEQNUM  TO  SPA-RECE07-HSEQNUM
      *                編綴順設定のとき詳細情報を取得
                       IF    ( SPA-RECE07-HKNJANUM
                                               NOT =   "99999999" )
                       AND   ( SPA-RECE07-HSEQNUM  =   "99999999" )
                           MOVE    "tbl_recekanri" TO  WRK-MCP-TABLE
                           MOVE    "key12"         TO  WRK-MCP-PATHNAME
                           MOVE    "1"             TO  RECE07-PREFKBN
                           MOVE    "2"             TO
                                                   RECE07-TEISYUTUSAKI
                           MOVE    RECE07-REC      TO  MCPDATA-REC
                           PERFORM 900-RECE07-INV-SEC
                           IF      FLG-RECE07      =   ZERO
                               EVALUATE    RECE07-PREFKBN-SRT (1:1)
                                   WHEN    "1"
                                       MOVE    1           TO  IDXX
                                   WHEN    "2"
                                       MOVE    2           TO  IDXX
                               END-EVALUATE    
                               MOVE    RECE07-ZAITAKU-KBN  TO
                                         SPA-RECE07-ZAITAKU-KBN-T (IDXX)
                               MOVE    RECE07-CHOUKI-KBN   TO
                                         SPA-RECE07-CHOUKI-KBN-T  (IDXX)
                               MOVE    RECE07-SRT-LAST     TO
                                         SPA-RECE07-SRT-LAST-T    (IDXX)
                               MOVE    RECE07-KOUGAKU-TEN  TO
                                         SPA-RECE07-KOUGAKU-TEN-T (IDXX)
                           END-IF
                           MOVE    "tbl_recekanri" TO  WRK-MCP-TABLE
                           MOVE    "key12"         TO  WRK-MCP-PATHNAME
                           MOVE    "2"             TO  RECE07-PREFKBN
                           MOVE    "2"             TO
                                                   RECE07-TEISYUTUSAKI
                           MOVE    RECE07-REC      TO  MCPDATA-REC
                           PERFORM 900-RECE07-INV-SEC
                           IF      FLG-RECE07      =   ZERO
                               EVALUATE    RECE07-PREFKBN-SRT (1:1)
                                   WHEN    "1"
                                       MOVE    1           TO  IDXX
                                   WHEN    "2"
                                       MOVE    2           TO  IDXX
                               END-EVALUATE    
                               MOVE    RECE07-ZAITAKU-KBN  TO
                                         SPA-RECE07-ZAITAKU-KBN-T (IDXX)
                               MOVE    RECE07-CHOUKI-KBN   TO
                                         SPA-RECE07-CHOUKI-KBN-T  (IDXX)
                               MOVE    RECE07-SRT-LAST     TO
                                         SPA-RECE07-SRT-LAST-T    (IDXX)
                               MOVE    RECE07-KOUGAKU-TEN  TO
                                         SPA-RECE07-KOUGAKU-TEN-T (IDXX)
                           END-IF
                       END-IF
                   END-IF
               END-IF  
      *
      *        一括作成のとき主科設定の有無チェック
               IF    ( SPA-SYUKA-KBN           =   "1"         )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO  SPA-RECE07-SRYKA
                   END-IF
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO
                                               SPA-RECE07-SRYKA-KOKUHO
                   END-IF
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "6"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO
                                               SPA-RECE07-SRYKA-KOUIKI
                   END-IF
               END-IF  
      *
      *        一括作成のとき医師別設定、病棟別設定、
      *                      ターミナルデジット設定の有無チェック
               IF    ( SPA-R01-FILEKBN         =   "1"         )
                   INITIALIZE                      RECE08-REC
                   MOVE    RECE07-HOSPNUM      TO  RECE08-HOSPNUM
                   MOVE    RECE07-PRTID        TO  RECE08-PRTID
                   MOVE    RECE07-SRYYM        TO  RECE08-RECEYM
                   MOVE    RECE07-CREYMD       TO  RECE08-CREYMD
                   MOVE    RECE07-CREHMS       TO  RECE08-CREHMS
                   MOVE    RECE07-NYUGAIKBN    TO  RECE08-NYUGAIKBN
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE08-INV-SEC
                   IF      FLG-RECE08          =   ZERO
                       MOVE    RECE08-DRCD-SRYKA   TO
                                               SPA-RECE08-DRCD-SRYKA
                       MOVE    RECE08-SORT-1       TO
                                               SPA-RECE08-SORT-1
                       MOVE    RECE08-SORT-2       TO
                                               SPA-RECE08-SORT-2
                   END-IF
               END-IF
      *
      *        一括作成で社保処理のとき直接請求する保険者のチェックと取得
               IF    ( SPA-R01-FILEKBN         =   "1"         )
               AND   ( SPA-R01-RECEKBN         =   "0" OR  "1" )
                   MOVE    "99999999"          TO
                                           SPA-RECE07-HKNJANUM-SYAHO (1)
                   MOVE    "9"                 TO
                                           SPA-RECE07-HKNJANUM-SRT-SYAHO
                   MOVE    1                   TO  IDXX
      *
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key14"             TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO
                                               SPA-RECE07-TEISYUTUSAKI
                   MOVE    SPA-RECE07-REC      TO  MCPDATA-REC
                   PERFORM 900-RECE07-SELECT-SEC
                   PERFORM             UNTIL   FLG-RECE07  =   1
                                       OR      IDXX        =   10
                                       OR      RECE07-HKNJANUM-SRT
                                                           =   "9"
                       IF      IDXX            =   1
                           MOVE    RECE07-HKNJANUM-SRT TO
                                       SPA-RECE07-HKNJANUM-SRT-SYAHO
                       END-IF                                        
                       IF      RECE07-HKNJANUM NOT =   "99999999"
                           ADD     1                   TO  IDXX
                           MOVE    RECE07-HKNJANUM     TO
                                       SPA-RECE07-HKNJANUM-SYAHO (IDXX)
                       END-IF
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-RECE07-FETCH-SEC
                   END-PERFORM
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      IDXX                =   1
                       MOVE    "%%%%%%%%"          TO  
                                           SPA-RECE07-HKNJANUM-SYAHO (1)
                   END-IF
      *
                   MOVE    IDXX                TO
                                           SPA-RECE07-HKNJANUM-SYAHO-MAX
               END-IF  
      *
      *        不明分の有無チェック
               MOVE    SPA-RECE07-KEY      TO  RECE07-KEY
               MOVE    "%"                 TO  RECE07-TEISYUTUSAKI
               MOVE    "0000"              TO  RECE07-RECESYUBETU
               MOVE    "%%"                TO  RECE07-SRYKA 
               MOVE    RECE07-REC          TO  MCPDATA-REC                             
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
               MOVE    RECE07-REC          TO  MCPDATA-REC
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-PAGE         TO  SPA-RECE07-FUMEI-PAGE
               ELSE
                   MOVE    ZERO                TO  SPA-RECE07-FUMEI-PAGE
               END-IF
           END-IF
      *
           .
       4501-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       450-KOUSHIN-SEC                 SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    フラグ初期化
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-R99-CANCEL
      *
      *    新規分作成のときレセプト指示コード更新
      *
           EVALUATE    SPA-GMN-SYOKBN    ALSO    SPA-GMN-IKKATU
               WHEN    "T"               ALSO    "T"
                   INITIALIZE                  SYS-2001-REC
                   MOVE    "2001"              TO  SYS-2001-KANRICD
                   MOVE    "*"                 TO  SYS-2001-KBNCD
                   MOVE    SPA-SYSYMD          TO  SYS-2001-STYUKYMD
                                                   SYS-2001-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-2001-HOSPNUM
                   MOVE    SYS-2001-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-2001-REC
                   ELSE
                       INITIALIZE                  SYS-2001-REC
grpsys                 MOVE    SPA-HOSPNUM     TO  SYS-2001-HOSPNUM
                       MOVE    "2001"          TO  SYS-2001-KANRICD
                       MOVE    "*"             TO  SYS-2001-KBNCD
                       MOVE    ZERO            TO  SYS-2001-STYUKYMD
                       MOVE    ALL "9"         TO  SYS-2001-EDYUKYMD
                   END-IF 
                   INITIALIZE                      SYS-2001-TBL 
                   MOVE    "1"                 TO  SYS-2001-SYOKBN
                   MOVE    SPA-R01-RECEKBN     TO  SYS-2001-RECEKBN
                   MOVE    SPA-GMN-NYUGAIKBN   TO  SYS-2001-NYUGAIKBN
                   MOVE    SPA-NAI-SRYYM       TO  SYS-2001-SRYYM
                   IF      SPA-R01-PRTKBN      =   "1"
                       MOVE    SPACE               TO  SYS-2001-PRTKBN
                   ELSE
                       IF      SPA-R01-PRTKBN      =   "8" OR  "9"
                           MOVE    SPA-R01-PRTKBN  TO  SYS-2001-PRTKBN
                       ELSE
                           MOVE    "1"             TO  SYS-2001-PRTKBN
                       END-IF
                   END-IF
                   MOVE    SYS-2001-REC        TO  MCPDATA-REC
                   IF      FLG-SYSKANRI        =   1
                       MOVE    "DBINSERT"          TO  MCP-FUNC
                   ELSE
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                   END-IF
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1001"              TO  SPA-ERRCD
                       DISPLAY "2001 SYSKANRI-KEY =>" SYS-2001-KEY 
                       GO  TO  450-KOUSHIN-EXT
                   END-IF
      *
      *            パラメタＤＢ更新
                   INITIALIZE                  BTPARA-REC
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  BTPARA-SHELLID
grpsys             MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
                   MOVE    BTPARA-REC      TO  MCPDATA-REC
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "del1"          TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *
                   INITIALIZE                      BTPARA-REC
                   MOVE    WRK-CONS-JOB-SHELLID
                                               TO  BTPARA-SHELLID
                   MOVE    WRK-CONS-JOB-JOBID  TO  BTPARA-JOBID
                   MOVE    "00"                TO  BTPARA-INFO-KBN
                   MOVE    SPA-TERMID          TO  BTPARA-TERMID
                   MOVE    SPA-OPID            TO  BTPARA-OPID
                   MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
                   MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
                   MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
grpsys             MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
                   MOVE    BTPARA-REC          TO  MCPDATA-REC
                   MOVE    "tbl_btpara"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "1002"          TO  SPA-ERRCD
                       GO  TO  450-KOUSHIN-EXT
                    END-IF
      *
      *            ジョブ開始セット
                   MOVE    "DEL"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
      *
                   MOVE    "JBS"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-TERMID      TO  JOB-TERMID                        
                   MOVE    SPA-R01-NYUGAIKBN
                                           TO  JOB-NYUGAIKBN
                   MOVE    "1"             TO  JOB-SYOKBN
                   MOVE    SPA-R01-SRYYM   TO  JOB-SRYYM
                   MOVE    SPA-R01-JOB-KARTE-UID
                                           TO  JOB-KARTE-UID
                   MOVE    SPA-R01-JOB-ORCA-UID
                                           TO  JOB-ORCA-UID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
      *
      *            印刷用スクリプト処理
                   MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
                   MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
                   MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
                   MOVE    SPACE               TO  RECEERR
                   STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                           "/"                 DELIMITED   BY  SIZE
                           ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                               INTO        RECEERR
                   END-STRING
      *
                   OPEN    OUTPUT   RECEERR-FILE
                   CLOSE   RECEERR-FILE
      *
      *            県番号の取得
                   INITIALIZE                  SYS-1001-REC
                   MOVE    "1001"          TO  SYS-1001-KANRICD
                   MOVE    "*"             TO  SYS-1001-KBNCD
                   MOVE    SPA-NAI-SRYYM   TO  SYS-1001-STYUKYMD (1:6)
                   MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
                   MOVE    SYS-1001-STYUKYMD 
                                           TO  SYS-1001-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC    TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE   MCPDATA-REC  TO  SYS-1001-REC
                   ELSE
                       MOVE   SPACE        TO  SYS-1001-REC
                   END-IF
      *
      *            カスタマイズＰＧ・レセ電情報の設定
                   INITIALIZE                  SYS-2005-REC
                   MOVE    "2005"          TO  SYS-2005-KANRICD
                   MOVE    "*"             TO  SYS-2005-KBNCD
                   MOVE    SPA-SYSYMD      TO  SYS-2005-STYUKYMD
                                               SYS-2005-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM     TO  SYS-2005-HOSPNUM
                   MOVE    SYS-2005-REC    TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE   MCPDATA-REC  TO  SYS-2005-REC
                   ELSE
                       MOVE   SPACE        TO  SYS-2005-REC
                   END-IF
      *
      *            区分コード０１のデータ読み込み
                   INITIALIZE                  SYS-25-REC
                   MOVE    "2005"          TO  SYS-25-KANRICD
                   MOVE    "01"            TO  SYS-25-KBNCD
                   MOVE    SPA-SYSYMD      TO  SYS-25-STYUKYMD
                                               SYS-25-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM     TO  SYS-25-HOSPNUM
                   MOVE    SYS-25-REC  TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE   MCPDATA-REC  TO  SYS-25-REC
                   ELSE
                       INITIALIZE              SYS-25-REC
                       MOVE    SYS-2005-TERMID (1:4)
                                           TO  SYS-25-TBL (1:4)
                   END-IF
      *
                   IF      SYS-2005-PREFPRGID (1) (1:1)
                                           NOT =  "S"
                           MOVE  "NOPG"    TO  SYS-2005-PREFPRGID (1)
                   END-IF
                   IF      SYS-2005-PREFPRGID (2) (1:1)
                                           NOT =  "S"
                           MOVE  "NOPG"    TO  SYS-2005-PREFPRGID (2)
                   END-IF
                   IF      SYS-2005-KKHRECEKBN =   SPACE
                       MOVE    ZERO            TO  SYS-2005-KKHRECEKBN      
                   END-IF
                   IF      SYS-2005-SYARECEKBN =   SPACE
                       MOVE    ZERO            TO  SYS-2005-SYARECEKBN      
                   END-IF
                   IF      SYS-2005-FRAME-JIBAIPRTKBN
                                               =   SPACE
                       MOVE    ZERO            TO
                                           SYS-2005-FRAME-JIBAIPRTKBN
                   END-IF
                   IF      SYS-2005-FRAME-JIBAIPRTKBN-N
                                               =   SPACE
                       MOVE    ZERO            TO
                                           SYS-2005-FRAME-JIBAIPRTKBN-N
                   END-IF
                   IF      SYS-25-JIBAIPRTKBN
                                               =   SPACE   OR  "0"
                       MOVE    "1"         TO  SYS-25-JIBAIPRTKBN
                   END-IF
                   IF      SYS-25-JIBAIPRTKBN-N
                                               =   SPACE   OR  "0"
                       MOVE    "1"             TO
                                               SYS-25-JIBAIPRTKBN-N
                   END-IF
                   IF      SYS-25-JIBAIPRTKBN1
                                               =   SPACE   OR  "0"
                       MOVE    "2"             TO
                                               SYS-25-JIBAIPRTKBN1
                   END-IF
                   IF      SYS-25-JIBAIPRTKBN1-N
                                               =   SPACE   OR  "0"
                       MOVE    "2"             TO
                                               SYS-25-JIBAIPRTKBN1-N
                   END-IF
                   IF      SYS-25-KOUIKIRECEKBN
                                               =   SPACE
                       MOVE    ZERO        TO  SYS-25-KOUIKIRECEKBN
                   END-IF
                   IF      SYS-25-FRAME-ROUPRTKBN
                                               =   SPACE
                       MOVE    ZERO            TO
                                           SYS-25-FRAME-ROUPRTKBN
                   END-IF
                   IF      SYS-25-FRAME-ROUPRTKBN-N
                                               =   SPACE
                       MOVE    ZERO            TO
                                           SYS-25-FRAME-ROUPRTKBN-N
                   END-IF
                   IF      SYS-25-COMMENT-ROU-KSIKBN
                                               =   SPACE
                       MOVE    ZERO            TO
                                   SYS-25-COMMENT-ROU-KSIKBN
                   END-IF
                   IF      SYS-25-ROUPRTKBN
                                               =   SPACE   OR  "1"
                       MOVE    ZERO            TO
                                           SYS-25-ROUPRTKBN
                   END-IF
                   IF      SYS-25-ROUPRTKBN-N
                                               =   SPACE   OR  "1"
                       MOVE    ZERO            TO
                                           SYS-25-ROUPRTKBN-N
                   END-IF
                   IF      SYS-25-ROURECEKBN
                                               =   SPACE
                       MOVE    "0"             TO  SYS-25-ROURECEKBN
                   END-IF
                   IF      SYS-25-RSI-RECEIPT-KBN
                                               =   SPACE
                       MOVE    "0"             TO
                                               SYS-25-RSI-RECEIPT-KBN
                   END-IF
      *
      *            労災指定医療機関の有無
      *           （平成２５年６月以前は指定なしとする）
                   INITIALIZE                  SYS-4001-REC
                   IF      SPA-NAI-SRYYM   <   201307
                       MOVE    "0"             TO  SYS-4001-RSIIRYOKBN
                   ELSE 
                       MOVE    SPA-HOSPNUM     TO  SYS-4001-HOSPNUM
                       MOVE    "4001"          TO  SYS-4001-KANRICD
                       MOVE    "*"             TO  SYS-4001-KBNCD
                       MOVE    SPA-NAI-SRYYM   TO  SYS-4001-STYUKYMD
                                                                   (1:6)
                       MOVE    "01"            TO  SYS-4001-STYUKYMD
                                                                   (7:2)
                       MOVE    SYS-4001-STYUKYMD
                                               TO  SYS-4001-EDYUKYMD
                       MOVE    SYS-4001-REC    TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-4001-REC
                       ELSE
                           INITIALIZE                  SYS-4001-REC
                       END-IF
                   END-IF
      *
                   MOVE    SPACE           TO  SHELLTBL
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "0"
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "7"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    "recept9.sh"
                                                   TO  WRK-SHELL-SHELL
                           ELSE
                               MOVE    "recept1.sh"
                                                   TO  WRK-SHELL-SHELL
                           END-IF                        
                       WHEN    "3"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    "recept10.sh"
                                                   TO  WRK-SHELL-SHELL
                           ELSE
                               MOVE    "recept6.sh"
                                                   TO  WRK-SHELL-SHELL
                           END-IF
                       WHEN    "4"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    "recept11.sh"
                                                   TO  WRK-SHELL-SHELL
                           ELSE
                               MOVE    "recept8.sh"
                                                   TO  WRK-SHELL-SHELL
                           END-IF
                       WHEN    "5"
                       WHEN    "8"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    "recept13.sh"
                                                   TO  WRK-SHELL-SHELL
                           ELSE
                               MOVE    "recept12.sh"
                                                   TO  WRK-SHELL-SHELL
                           END-IF
                       WHEN    "6"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    "recept15.sh"
                                                   TO  WRK-SHELL-SHELL
                           ELSE
                               MOVE    "recept14.sh"
                                                   TO  WRK-SHELL-SHELL
                           END-IF
                   END-EVALUATE        
      *            ファイルデイレクトリ位置指定
      **             INITIALIZE              ORCSMKPASSAREA
      **             MOVE    WRK-SHELL-NAME  TO  MKPASS-IN-01
      **             CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
      **             MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *
                   MOVE    WRK-SHELL-NAME  TO  SHELLTBL-NAME
      *     
                   MOVE    SPA-NAI-SRYYM   TO  WRK-SHELL-SRYYM
                   MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
      *************MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
                   MOVE    SMCNDATE-YMD    TO  WRK-SHELL-SYSYMD
                   MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG1
                   MOVE    "1"             TO  SHELLTBL-ARG2
                   MOVE    SPA-R01-RECEKBN TO  SHELLTBL-ARG3
                   MOVE    ERRFLPARA-BASENAME
                                           TO  SHELLTBL-ARG4
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  SHELLTBL-ARG5
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG6
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    SYS-2005-PREFPRGID (2)
                                               TO  SHELLTBL-ARG7
                   ELSE
                       MOVE    SYS-2005-PREFPRGID (1)
                                               TO  SHELLTBL-ARG7
                   END-IF
                   MOVE    SPA-NAI-SRYYM   TO  SHELLTBL-ARG8
                   MOVE    SMCNDATE-YMD    TO  SHELLTBL-ARG9
                   MOVE    SMCNDATE-HMS    TO  SHELLTBL-ARG10 (1:6)
                   MOVE    "00"            TO  SHELLTBL-ARG10 (7:2)
                   MOVE    SYS-1001-PREFNUM
                                           TO  SHELLTBL-ARG11
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "0"
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "7"
                           IF      SYS-2005-SYARECEKBN
                                                   =   ZERO
                               MOVE    "0"         TO  SHELLTBL-ARG12
                           ELSE
                               MOVE    SYS-2005-SYARECEKBN
                                                   TO  SHELLTBL-ARG12
                           END-IF
                           IF      SYS-2005-KKHRECEKBN
                                                   =   ZERO
                               MOVE    "0"         TO  SHELLTBL-ARG13
                           ELSE
                               MOVE    SYS-2005-KKHRECEKBN
                                                   TO  SHELLTBL-ARG13
                           END-IF
                       WHEN    "3"
                           MOVE    SYS-25-ROURECEKBN
                                                   TO  SHELLTBL-ARG12
                           MOVE    SYS-4001-RSIIRYOKBN
                                                   TO  SHELLTBL-ARG13
                       WHEN    OTHER
                           MOVE    "0"             TO  SHELLTBL-ARG12
                                                       SHELLTBL-ARG13
                   END-EVALUATE
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "3"
                           MOVE    SYS-25-COMMENT-ROU-KSIKBN
                                                   TO  SHELLTBL-ARG14
                       WHEN    OTHER
                           MOVE    "00"            TO  SHELLTBL-ARG14
                   END-EVALUATE
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "4"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    SYS-2005-FRAME-JIBAIPRTKBN-N
                                                   TO  SHELLTBL-ARG15
                           ELSE
                               MOVE    SYS-2005-FRAME-JIBAIPRTKBN
                                                   TO  SHELLTBL-ARG15
                           END-IF
                       WHEN    "3"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    SYS-25-FRAME-ROUPRTKBN-N
                                                   TO  SHELLTBL-ARG15
                           ELSE
                               MOVE    SYS-25-FRAME-ROUPRTKBN
                                                   TO  SHELLTBL-ARG15
                           END-IF
                   END-EVALUATE
grpsys             MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG16
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "4"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    SYS-25-JIBAIPRTKBN1-N
                                                   TO  SHELLTBL-ARG17
                           ELSE
                               MOVE    SYS-25-JIBAIPRTKBN1
                                                   TO  SHELLTBL-ARG17
                           END-IF
                       WHEN    "5"
                       WHEN    "8"
                           IF      SPA-GMN-NYUGAIKBN   =   "1"
                               MOVE    SYS-25-JIBAIPRTKBN-N
                                                   TO  SHELLTBL-ARG17
                           ELSE
                               MOVE    SYS-25-JIBAIPRTKBN
                                                   TO  SHELLTBL-ARG17
                           END-IF
                       WHEN    "3"
                           IF      SPA-R01-NYUGAIKBN   =   "1"
                               MOVE    SYS-25-ROUPRTKBN-N
                                                   TO  SHELLTBL-ARG17
                           ELSE
                               MOVE    SYS-25-ROUPRTKBN
                                                   TO  SHELLTBL-ARG17
                           END-IF
                   END-EVALUATE
                   DISPLAY "====  TEST DISPLAY ==============="
                   DISPLAY "SHELLTBL-NAME = " SHELLTBL-NAME
                   DISPLAY "====  TEST SET ==============="
                   MOVE     SPA-HOSPNUM    TO   SHELLTBL-HOSPNUM
                   MOVE     SHELLTBL-ARG5  TO   SHELLTBL-JOBID
                   MOVE     SHELLTBL-ARG6  TO   SHELLTBL-SHELLID
      *
                   MOVE    SPA-R01-PRTKBN  TO  SHELLTBL-ARG18
                   MOVE    "0"             TO  SHELLTBL-ARG19
                                               SHELLTBL-ARG20
                   IF      SPA-R01-RECEKBN =   "3"
                           MOVE        SYS-25-RSI-RECEIPT-KBN
                                               TO  SHELLTBL-ARG20
                   END-IF
      *
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "monthly"       TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *            情報確認画面を表示                                                   
                   PERFORM 490-KAKUNIN-SEC
                WHEN    OTHER
                    PERFORM 4501-NEXT-GMN-SEC
           END-EVALUATE
      *
           .
       450-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.       
      * 
           MOVE    "R03"               TO  SPA-MOTOPG
      *
           IF      SPA-RID1-FLG        =   "OK"
               IF      SPA-GMN-SYOKBN      =   "T"     AND
                       SPA-GMN-KOBETU      =   "T"
                   MOVE    "R01"               TO  SPA-SAKIPG
               ELSE
                   IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2"
                                                       OR  "7"
                       MOVE    "R02"               TO  SPA-SAKIPG
                   ELSE
                       MOVE    "R04"               TO  SPA-SAKIPG
                   END-IF        
               END-IF 
           ELSE
               IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2"
                                                   OR  "7"
                   MOVE    "R02"               TO  SPA-SAKIPG
               ELSE
                   MOVE    "R04"               TO  SPA-SAKIPG
               END-IF        
           END-IF
      *
           MOVE   "JOIN"              TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面移行用のパラメタ変更処理
      *****************************************************************
       45011-PARA-HENSYU-SEC            SECTION.       
      *    
           MOVE    SPA-GMN-SYOKBN      TO  SPA-R01-SYOKBN
      *
           MOVE    "0"                 TO  SPA-R01-RECEKBN
           EVALUATE    TRUE
               WHEN    SPA-DATAKBN1-VALUE  =   "T"
                   MOVE    "0"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN2-VALUE  =   "T"
                   MOVE    "1"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN3-VALUE  =   "T"
                   MOVE    "2"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN5-VALUE  =   "T"
                   MOVE    "7"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN11-VALUE =   "T"
                   MOVE    "3"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN31-VALUE =   "T"
                   MOVE    "4"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN32-VALUE =   "T"
                   MOVE    "5"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN33-VALUE =   "T"
                   MOVE    "8"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN4-VALUE  =   "T"
                   MOVE    "6"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN21-VALUE  =   "T"
                   MOVE    "0"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN22-VALUE  =   "T"
                   MOVE    "3"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN23-VALUE  =   "T"
                   MOVE    "4"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN24-VALUE  =   "T"
                   MOVE    "5"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN25-VALUE  =   "T"
                   MOVE    "6"                 TO  SPA-R01-RECEKBN
               WHEN    SPA-DATAKBN26-VALUE  =   "T"
                   MOVE    "8"                 TO  SPA-R01-RECEKBN
           END-EVALUATE
      *
           IF      SPA-GMN-IKKATU          =   "T"
               MOVE    "1"                 TO  SPA-R01-FILEKBN
               MOVE    SPA-NAI-SRYYM       TO  SPA-R01-SRYYM
           ELSE
               MOVE    "2"                 TO  SPA-R01-FILEKBN
               MOVE    ZERO                TO  SPA-R01-SRYYM
           END-IF
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-R01-NYUGAIKBN
      *
           MOVE    SPA-GMN-PRTKBN      TO  SPA-R01-PRTKBN
      *    点検用レセ（０点公費あり、包括診療分あり）のとき
           IF     SPA-R01-PRTKBN      =   "2"
      *        医保分
               IF    ( SPA-R01-RECEKBN     =   "0" OR  "1" OR
                                               "2" OR  "7"    )
                   EVALUATE    SPA-NAI-200501-INSPECT-ZERO-KBN
                               ALSO   SPA-NAI-200501-INSPECT-INCLUDE
                       WHEN    "1" ALSO    "0"
                           MOVE    "3"             TO  SPA-R01-PRTKBN
                       WHEN    "0" ALSO    "1"
                           MOVE    "4"             TO  SPA-R01-PRTKBN
                       WHEN    "1" ALSO    "1"
                          MOVE    "5"              TO  SPA-R01-PRTKBN
                   END-EVALUATE
               ELSE
                   EVALUATE    SPA-NAI-200501-INSPECT-ZERO-KBN
                               ALSO   SPA-NAI-200501-INSPECT-INCLUDE
                       WHEN    ANY ALSO    "1"
                           MOVE    "4"             TO  SPA-R01-PRTKBN
                   END-EVALUATE
               END-IF
           END-IF
      *    医保分で連携基盤用のとき
           IF    ( SPA-R01-RECEKBN     =   "0" OR  "1" OR
                                           "2" OR  "7"    )
           AND   ( SPA-R01-PRTKBN      =   "6"            )
               MOVE    "9"                 TO  SPA-R01-PRTKBN
           END-IF
      *    医保分でＥＦファイル作成のとき
           IF    ( SPA-R01-RECEKBN     =   "0" OR  "1" OR
                                           "2" OR  "7"    )
           AND   ( SPA-R01-PRTKBN      =   "7"            )
               MOVE    "8"                 TO  SPA-R01-PRTKBN
           END-IF
     *
           MOVE    SPACE               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-NAI-2005-AUTODSPKBN
                                       TO  SPA-R01-AUTODSPKBN
           MOVE    SPA-NAI-200501-INSPECT-ZERO-KBN
                                       TO  SPA-R01-INSPECT-ZERO-KBN
           MOVE    SPA-NAI-200501-INSPECT-INCLUDE
                                       TO  SPA-R01-INSPECT-INCLUDE-KBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK
      *
           .
       45011-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R03             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R03"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
      *    再印刷を選択時は表示しない
           IF      SPA-GMN-SYOKBN  =   "F"
               GO  TO  490-KAKUNIN-EXT
           END-IF
      *                                     
      *    画面移行用のパラメタ
           PERFORM 45011-PARA-HENSYU-SEC
      *
           INITIALIZE                      R99
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
           IF      SPA-R01-PRTKBN  =   "9" OR  "8"
               CONTINUE
           ELSE
               MOVE    "F12:印    刷"      TO  R99-B12-LABEL
           END-IF
           MOVE    "black"             TO  R99-MSG-STYLE
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                     
               MOVE    JOB-JOBID       TO  R99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  R99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  R99-ERRMSG   (IDX)
               COMPUTE R99-PROGRESSBAR-VALUE  =
                           JOB-STEPCNT  /   13     *   100
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                     IF      JOB-UPDCNT      =   ZERO    
                         MOVE    "対象データがありませんでした"
                                                       TO  R99-MSG
                     ELSE
                       IF      SPA-R01-PRTKBN  =   "9" OR  "8"
                         CONTINUE
                       ELSE
                         PERFORM 4501-REPRINT-CHK-SEC
                         IF      SPA-RECE07-PRTID    =   SPACE
                           MOVE    "対象のレセプトが作成されていません"
                                               TO  R99-MSG
                         ELSE                    
                           MOVE    SPA-RECE07-KEY
                                          TO  SPA-R01-RECE07-KEY
                           MOVE    SPA-RECE07-RECEKBN
                                          TO  SPA-R01-RECE07-RECEKBN
                           MOVE    SPA-RECE07-SRYYM
                                          TO  SPA-R01-RECE07-SRYYM
                           MOVE    SPA-RECE07-NYUGAIKBN
                                          TO  SPA-R01-RECE07-NYUGAIKBN
                           MOVE    SPA-RECE07-SYORISBT
                                          TO  SPA-R01-RECE07-SYORISBT
                           MOVE    SPA-RECE07-STYMD
                                          TO  SPA-R01-RECE07-STYMD
                           MOVE    SPA-RECE07-EDYMD
                                          TO  SPA-R01-RECE07-EDYMD
                           MOVE    SPA-RECE07-HSEQNUM
                                          TO  SPA-R01-RECE07-HSEQNUM
                           MOVE    SPA-RECE07-HKNJANUM
                                          TO  SPA-R01-RECE07-HKNJANUM
                           MOVE    SPA-RECE07-SRYKA-KOKUHO
                                          TO SPA-R01-RECE07-SRYKA-KOKUHO
                           MOVE    SPA-RECE07-SRYKA-KOUIKI
                                          TO SPA-R01-RECE07-SRYKA-KOUIKI
                           MOVE    SPA-RECE07-RECE-PUTH-T (1)
                                          TO SPA-R01-RECE07-RECE-PUTH(1)
                           MOVE    SPA-RECE07-RECE-PUTH-T (2)
                                          TO SPA-R01-RECE07-RECE-PUTH(2)
                           MOVE    SPA-RECE08-DRCD-SRYKA
                                          TO SPA-R01-RECE08-DRCD-SRYKA
                           MOVE    SPA-RECE08-SORT-1
                                          TO SPA-R01-RECE08-SORT-1
                           MOVE    SPA-RECE08-SORT-2
                                          TO SPA-R01-RECE08-SORT-2
                           MOVE    SPA-RECE07-FUMEI-PAGE
                                          TO  SPA-R01-RECE07-FUMEI-PAGE
                           MOVE    SPA-RECE07-HKNJANUM-SYAHO-T
                                          TO
                                       SPA-R01-RECE07-HKNJANUM-SYAHO-T
                         END-IF
                       END-IF
                     END-IF 
                   ELSE
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  R99-MSG
                       END-STRING 
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  R99-COUNT    
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE   TO  R99-B21-STATE
           MOVE    WIDGET-INSENSITIVE   TO  R99-B12-STATE
           IF      R99-MSG              =   SPACE
               IF      R99-COUNT            >   ZERO    
                 IF      SPA-R01-PRTKBN  =   "9" OR  "8"
                    MOVE    "処理は正常に終了しました"
                                                TO  R99-MSG
                 ELSE
                   MOVE    WIDGET-NORMAL        TO  R99-B12-STATE
                   MOVE    "B12"                TO  MCP-WIDGET
                   IF    ( SPA-R01-RECE07-SRYKA =   "XX" )
                   OR    ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                =   "XX" )
                   OR    ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                =   "XX" )
                   OR    ( SPA-R01-RECE07-FUMEI-PAGE
                                                >   ZERO )
                       STRING  "主科未設定分又は種別不明分がありま"
                                                DELIMITED   BY  SIZE
                               "すので印刷画面で確認して下さい"
                                                DELIMITED   BY  SIZE
                                                INTO    R99-MSG
                       END-STRING
                       MOVE    "red"        TO  R99-MSG-STYLE 
                   ELSE
                       MOVE    "処理は正常に終了しました"
                                                TO  R99-MSG
                   END-IF
                 END-IF  
                 MOVE   100                 TO  R99-PROGRESSBAR-VALUE
               ELSE
                   MOVE    "B01"                TO  MCP-WIDGET
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE
               IF      JOB-STOPFLG          =   ZERO
                   MOVE    WIDGET-NORMAL        TO  R99-B21-STATE
               END-IF
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "R03"                TO  SPA-MOTOPG
           MOVE    "R99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R03             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R03"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YM   (1:5)  NOT NUMERIC
               INSPECT     WRK-YM       REPLACING   ALL " " BY  "0"
               MOVE    WRK-YM(1:1)      TO  WRK-WGO
               MOVE    WRK-YM(2:2)      TO  WRK-WYY
               MOVE    WRK-YM(5:2)      TO  WRK-WMM
           ELSE
               MOVE    WRK-YM(1:5)      TO  WRK-WYMD (1:5)
           END-IF
           MOVE    1                TO  WRK-WDD
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
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
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
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-RIDCD2      NOT =   SPACE
               PERFORM 530-RIDSET2-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R03    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  R03
      *
           INITIALIZE                      R03
           MOVE    SPA-GMN-SRYYM       TO  R03-SRYYM
      *
           MOVE    SPA-GMN-SYOKBN         TO  R03-SYOKBN-VALUE
           MOVE    SPA-GMN-SYOKBN-LABEL   TO  R03-SYOKBN-LABEL
           MOVE    SPA-GMN-IKKATU         TO  R03-IKKATU
           MOVE    SPA-GMN-KOBETU         TO  R03-KOBETU
           MOVE    SPA-DATAKBN1-VALUE     TO  R03-DATAKBN1
           MOVE    SPA-DATAKBN2-VALUE     TO  R03-DATAKBN2
           MOVE    SPA-DATAKBN3-VALUE     TO  R03-DATAKBN3
           MOVE    SPA-DATAKBN4-VALUE     TO  R03-DATAKBN4
           MOVE    SPA-DATAKBN5-VALUE     TO  R03-DATAKBN5
           MOVE    SPA-DATAKBN11-VALUE    TO  R03-DATAKBN11
           MOVE    SPA-DATAKBN31-VALUE    TO  R03-DATAKBN31
           MOVE    SPA-DATAKBN32-VALUE    TO  R03-DATAKBN32
           MOVE    SPA-DATAKBN33-VALUE    TO  R03-DATAKBN33
           MOVE    SPA-DATAKBN21-VALUE    TO  R03-DATAKBN21
           MOVE    SPA-DATAKBN22-VALUE    TO  R03-DATAKBN22
           MOVE    SPA-DATAKBN23-VALUE    TO  R03-DATAKBN23
           MOVE    SPA-DATAKBN24-VALUE    TO  R03-DATAKBN24
           MOVE    SPA-DATAKBN25-VALUE    TO  R03-DATAKBN25
           MOVE    SPA-DATAKBN26-VALUE    TO  R03-DATAKBN26
      *
           MOVE    SPA-GMN-NYUGAIKBN-G    TO  R03-NYUGAIKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-NYUGAIKBN-MAX
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                           TO  R03-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUGAIKBN-MAX   TO  R03-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-PRTKBN-G    TO  R03-PRTKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-PRTKBN-MAX
               MOVE    SPA-GMN-PRTKBNLST (IDX)
                                           TO  R03-PRTKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-PRTKBN-MAX   TO  R03-PRTKBN-COUNT
      *
           MOVE    SPA-GMN-PRTKBN-LABEL TO  R03-PRTKBN-LABEL
           MOVE    "blue"               TO  R03-PRTKBN-LABEL-STYLE
      * 
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    WIDGET-NORMAL       TO  R03-PRTKBN-STATE
               WHEN    "F"
                   MOVE    WIDGET-INSENSITIVE  TO  R03-PRTKBN-STATE
           END-EVALUATE
      *
           IF      SPA-BEDSU           =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  R03-B23-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  R03-B23-STATE
           END-IF
           IF      SPA-SYUKA-KBN       =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  R03-B10-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  R03-B10-STATE
           END-IF
      *
           STRING  "＜令和２年度記載要領対応プログラム適用済＞"
                                       INTO    R03-MSG-LABEL
           END-STRING
           MOVE    "blue"              TO  R03-MSG-STYLE
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
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    1
                    MOVE  "SYOKBN"     TO  MCP-WIDGET
             WHEN    2
                    MOVE  "SRYYM"      TO  MCP-WIDGET
             WHEN    3
                    MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    4
                    MOVE  "PRTKBN"     TO  MCP-WIDGET
             WHEN    7
                    MOVE  "B12"        TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SRYYM"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "PRTKBN"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "SYOKBN"
               WHEN    "B07"
                   MOVE    7           TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療年月入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE
                       "作成済のデータがないので再印刷は指定できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "入外区分入力エラー"       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "個別作成のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   STRING  "該当診療年月は健保準拠の設定ですので"
                                           DELIMITED   BY  SIZE  
                           "自賠責（新様式）は選択できません"
                                           DELIMITED   BY  SIZE  
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "労災準拠の設定がありませんので"
                                           DELIMITED   BY  SIZE  
                           "自賠責（新様式）は選択できません"
                                           DELIMITED   BY  SIZE  
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "ＥＦファイル作成、連携基盤用は医保分"
                                           DELIMITED   BY  SIZE  
                           "のときに選択できます"
                                           DELIMITED   BY  SIZE  
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   MOVE    "レセプト作成区分入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE    "主科が設定されていません"  TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "レセプト指示コードが更新できません"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "パラメタＤＢが更新できません"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           INITIALIZE                       RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R03"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示処理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   STRING  SPA-NAI-SRYYMH           DELIMITED  BY  SIZE
                           "　"                     DELIMITED  BY  SIZE
                           SPA-GMN-NYUGAIKBNMEI     DELIMITED  BY  SPACE
                           "分新規作成処理を行います"
                                                    DELIMITED  BY  SIZE 
                                                    INTO  WRK-RIDMSG
                   END-STRING
               WHEN    "1002"
                   STRING  SPA-NAI-SRYYMH           DELIMITED  BY  SIZE
                           "　"                     DELIMITED  BY  SIZE
                           SPA-GMN-NYUGAIKBNMEI     DELIMITED  BY  SPACE
                           "分再印刷処理を行います"
                                                    DELIMITED  BY  SIZE 
                                                    INTO  WRK-RIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  SPA-GMN-NYUGAIKBNMEI     DELIMITED  BY  SPACE
                           "個別作成分の新規作成処理を行います" 
                                                    DELIMITED  BY  SIZE 
                                                    INTO  WRK-RIDMSG
                   END-STRING
               WHEN    "1004"
                   STRING  SPA-GMN-NYUGAIKBNMEI     DELIMITED  BY  SPACE
                           "個別作成分の再印刷処理を行います"
                                                    DELIMITED  BY  SIZE 
                                                    INTO  WRK-RIDMSG
                   END-STRING
               WHEN    "1005"
                   MOVE    SPA-NAI-SRYYMH   TO  WRK-RIDMSG
                   MOVE    "分新規作成処理（労災）を行います"
                                            TO  WRK-RIDMSG (17:)
               WHEN    "1006"
                   MOVE    SPA-NAI-SRYYMH   TO  WRK-RIDMSG
                   MOVE    "分再印刷処理（労災）を行います"
                                            TO  WRK-RIDMSG (17:)
               WHEN    "1007"
                   MOVE    "個別作成分の新規作成処理（労災）を行います" 
                                            TO  WRK-RIDMSG
               WHEN    "1008"
                   MOVE    "個別作成分の再印刷処理（労災）を行います"
                                            TO  WRK-RIDMSG
               WHEN    "1009"
                   MOVE    SPA-NAI-SRYYMH   TO  WRK-RIDMSG
                   MOVE    "分新規作成処理（自賠責）を行います"
                                            TO  WRK-RIDMSG (17:)
               WHEN    "1010"
                   MOVE    SPA-NAI-SRYYMH   TO  WRK-RIDMSG
                   MOVE    "分再印刷処理（自賠責）を行います"
                                            TO  WRK-RIDMSG (17:)
               WHEN    "1011"
                   MOVE  "個別作成分の新規作成処理（自賠責）を行います" 
                                            TO  WRK-RIDMSG
               WHEN    "1012"
                   MOVE    "個別作成分の再印刷処理（自賠責）を行います"
                                            TO  WRK-RIDMSG
               WHEN    "1013"
                   MOVE    SPA-NAI-SRYYMH   TO  WRK-RIDMSG
                   MOVE    "分新規作成処理（公害）を行います"
                                            TO  WRK-RIDMSG (17:)
               WHEN    "1014"
                   MOVE    SPA-NAI-SRYYMH   TO  WRK-RIDMSG
                   MOVE    "分再印刷処理（公害）を行います"
                                            TO  WRK-RIDMSG (17:)
               WHEN    "1015"
                   MOVE    "個別作成分の新規作成処理（公害）を行います"
                                            TO  WRK-RIDMSG
               WHEN    "1016"
                   MOVE    "個別作成分の再印刷処理（公害）を行います"
                                            TO  WRK-RIDMSG
               WHEN    "1017"
                   MOVE
                   "個別作成分の新規作成処理（ＥＦファイル）を行います"
                                            TO  WRK-RIDMSG
               WHEN    "1030"
                   STRING  "診療年月が一年以上前ですが、"
                                                    DELIMITED  BY  SIZE
                           "このまま処理を行いますか"
                                                    DELIMITED  BY  SIZE 
                                                    INTO  WRK-RIDMSG
                   END-STRING
               WHEN    "6666"
                   MOVE    "レセプト作成処理中です"
                                            TO  WRK-RIDMSG
               WHEN    "8888"
                   MOVE    "レセプト作成が正常終了しました"
                                            TO  WRK-RIDMSG
           END-EVALUATE
      *
           INITIALIZE                       RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID1-ID1MSG
      *
           MOVE    "blue"               TO  RID1-LABEL-STYLE
           IF    ( SPA-GMN-PRTKBN       =   "2" )
           AND   ( SPA-RIDCD (1:1)      =   "1" )
               MOVE    "【点検用レセプト】"     TO  RID1-LABEL
           END-IF
           IF    ( SPA-GMN-PRTKBN       =   "6" )
           AND   ( SPA-RIDCD (1:1)      =   "1" )
               IF      SPA-NAI-9101-COOPBASE    =   "2"
                   MOVE    "【連携基盤用レセプト（全件：未同意分含）】"
                                                    TO  RID1-LABEL
               ELSE
                   MOVE    "【連携基盤用レセプト】" TO  RID1-LABEL
               END-IF
           END-IF
           IF    ( SPA-GMN-PRTKBN       =   "7" )
           AND   ( SPA-RIDCD (1:1)      =   "1" )
               MOVE    "【ＥＦファイル用データ】" TO  RID1-LABEL
           END-IF
      *
           MOVE    "戻る"               TO  RID1-B01-LABEL
           EVALUATE    SPA-RIDCD
               WHEN    "1030"
                   MOVE    "B01"                TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "R03"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       530-RIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD2
               WHEN    "8888"
                   MOVE    "レセプト作成が正常終了しました"
                                                       TO  WRK-RIDMSG
           END-EVALUATE
      *
           INITIALIZE                       RID2
           MOVE    SPA-RIDCD2           TO  RID2-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID2-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R03"                TO  SPA-MOTOPG
           MOVE    "RID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-RIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込（キー）
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理（ＳＥＬＥＣＴ）
      *****************************************************************
       900-RECE07-SELECT-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECE07-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理（ＦＥＴＣＨ）
      *****************************************************************
       900-RECE07-FETCH-SEC            SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE07
               MOVE    MCPDATA-REC         TO  RECE07-REC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYUKA
                   MOVE    MCPDATA-REC     TO  SYUKA-REC
               ELSE
                   MOVE    1               TO  FLG-SYUKA
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷読込処理
      *****************************************************************
       900-RECE08-INV-SEC           SECTION.
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key44"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE08
                   MOVE    MCPDATA-REC         TO  RECE08-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE08
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE08
           END-IF
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE08-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
