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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBNYUINCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 入院会計再作成（システム管理変更時の置き換え）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/05/15    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-小豆沢  07/06/15  グループ診療対応
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/06  療養病棟（病床）入院患者についても
      *                                     処理可能となるように改修
      *  04.02.00    NACL-小豆沢  08/03/07  シス管病棟設定の加算判定変更
      *  04.02.00    NACL-小豆沢  08/06/26  他院（特別な関係の医療機関）通算日数
      *                                     カウント変更
      *  04.03.00    NACL-小豆沢  08/07/08  褥瘡患者管理加算対応
      *  04.03.00    NACL-小豆沢  08/08/11  特定入院料コード取得をサブプロに変更
      *  04.05.00    NACL-太田    10/01/29  拡張漢字対応
      *平成３０年４月改正対応
      *  05.00.00    NACL-小豆沢  18/03/05  療養病棟の読み替えコード追加
      *  05.00.00    NACL-小豆沢  20/04/03  病棟有効期間切れエラー対応
      *  05.00.00    NACL-小豆沢  20/04/03  特定入院料算定上限日数後の
      *                                     入院基本料取得エラー対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *
           COPY    "CPRECEERR.INC".
      *
      *    一覧
           COPY    "HCMSL55.INC".
      *
      *
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-NYUINACCTARI            PIC 9(01).
           03  FLG-NYUINACCT-MAE-CHK       PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-NG                      PIC 9(01).
           03  FLG-WWK-PTNYUINRRK          PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
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
           03  TN-IDX                      PIC 9(02).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  IDX-GAIHAKU                 PIC 9(02).
           03  IDX-HKN                     PIC 9(02).
           03  IDX-KSN                     PIC 9(02).
           03  KIKAN-IDX1                  PIC 9(04).
           03  KIKAN-IDX2                  PIC 9(04).
           03  IDX-SKJ-TAIHI               PIC 9(02).
           03  SC-IDX                      PIC 9(02).
           03  SC-IDX2                     PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID              PIC 9(07).
           03  WRK-PARA-SHELLID            PIC X(08).
           03  WRK-PARA-HOSPNUM            PIC 9(02).
           03  WRK-PARA-SRYYM              PIC X(06).
           03  WRK-PARA-BTUNUM             PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
           03  WRK-ERRNO                   PIC 9(2).
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-TNS-YMD                 PIC X(08).
           03  WRK-NAIYO                   PIC X(100).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
      *
      *    プレイルーム、保育士加算加算退避領域
           03  WRK-BTU-HIKSISUKBN          PIC X(01).
           03  WRK-BTU-KNGTEK-KBN          PIC X(01).
           03  WRK-BTU-DRTEK-KBN           PIC X(01).
      *
      *    入院会計作成用ワーク
           03  WRK-YMD                     PIC X(10). 
           03  WRK-NAI-SRYYM-41.
               05  WRK-NAI-SRYYM-41-YY     PIC 9(04).
               05  WRK-NAI-SRYYM-41-MM     PIC 9(02).
           03  WRK-TSUSAN                  PIC 9(05).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU                   PIC 9(05).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-SENTEI-STYMD            PIC X(08).
           03  WRK-BIRTHDAY-15             PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y        PIC 9(04).
               05  WRK-BIRTHDAY-15MD       PIC 9(04).
           03  WRK-SANTEISTYMD             PIC X(08).
           03  WRK-TOKUTEI-NYUIN           PIC X(02).
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9     REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9    REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-TAIHI-SKJ18             PIC 9(02)   OCCURS  31.
           03  WRK-TAIHI-SKJ19             PIC 9(02)   OCCURS  31.
           03  WRK-TAIHI-SKJ20             PIC 9(02)   OCCURS  31.
           03  WRK-TAIHI-GAIHAKU           PIC 9(01)   OCCURS  31.
           03  WRK-TAIHI-HKN               PIC 9(04)   OCCURS  31.
           03  WRK-TAIHI-KSN-SRYCD         PIC X(09).
           03  WRK-TAIHI-KSN-DAY           PIC 9(01)   OCCURS  31.
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
      **** 03  WRK-TAIHI-SRYYM-41          PIC X(06).
      *    医療機関設定の入院料加算チェック用
      *****03  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 20.
           03  WRK-NYUKHNKSN-AREA.
               05  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 60.
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-NYUINYM                 PIC X(06).
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
           03  WRK-TOKUTEI-TENNYUYMD  PIC X(08).
      *    特定入院料の会計作成後に一般の入院料で会計を作成するための
      *    領域
           03  WK-TOKUTEI-DAY              PIC 9(04).
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *
           03  WRK-CHK-SRYYM               PIC X(06).
           03  WRK-CHK-SRYYMR      REDEFINES  WRK-CHK-SRYYM.
               05  WRK-CHK-SRYYR           PIC 9(04).
               05  WRK-CHK-SRYMR           PIC 9(02).
           03  WRK-ERRCD                   PIC X(04).
      *
           03  WRK-CHK-PTID                PIC 9(10).
      *
           03  WRK-NENREIXXX               PIC 9(03).
      *
      *    出生児体重
           03  WRK-WEIGHT              PIC 9(04).
      *
      *    医療機関設定の入院料加算チェック用
           03  WRK-BTU-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-BTU-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-KSN-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-KSN-CDKBN-KBNNUM-EDA    PIC 9(02).
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                  PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1                PIC 9(04).
               05  WRK-MMS1                PIC 9(02).
               05  WRK-DDS1                PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2                PIC 9(04).
               05  WRK-MMS2                PIC 9(02).
               05  WRK-DDS2                PIC 9(02).
      *
       01  LST-AREA.
           03  LST-HEAD1.
               05                      PIC X(40)   VALUE   " ".
               05                      PIC X(30)   VALUE   
                             "入院会計一括置換結果リスト".
               05                      PIC X(30)   VALUE   " ".
               05  LST-HEAD-PAGE       PIC Z,ZZ9.
               05                      PIC X(05)   VALUE   "頁".
           03  LST-HEAD.
               05                      PIC X(04)   VALUE   "番号".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(20)   VALUE   "患者番号".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(16)   VALUE   "病棟".
               05                      PIC X(05)   VALUE   " ".
               05                      PIC X(26)   VALUE   "氏名".
               05                      PIC X(05)   VALUE   " ".
               05                      PIC X(08)   VALUE   "対象年月".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(30)   VALUE   "処理結果".
      *
           03  LST-MEISAI-G.
               05  LST-MEISAI-OCC      OCCURS      50.
                   07  LST-NO          PIC Z(04).
                   07  LST-FIL1        PIC X(01).
                   07  LST-PTNUM       PIC X(20).
                   07  LST-FIL2        PIC X(01).
                   07  LST-BYOTO       PIC X(20).
                   07  LST-FIL3        PIC X(01).
                   07  LST-NAME        PIC X(30).
                   07  LST-FIL4        PIC X(01).
                   07  LST-SRYYM       PIC X(08).
                   07  LST-FIL5        PIC X(01).
                   07  LST-NAIYO       PIC X(52).
      *
       01  CONST-AREA.
           03  CONST-LST-HEAD-LINE     PIC 9(05)   VALUE   2.
           03  CONST-LST-MEISAI-MAX    PIC 9(05)   VALUE   50.
      *
      *    入院履歴退避用ワーク
       01  WK-PTNYU-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WK-PTNYU-//.
      *    入院履歴退避用ワーク
       01  WWK-PTNYU-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WWK-PTNYU-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5002.INC".
           COPY    "CPSK2009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    初期加算算定サブ
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    入院行為照会用テーブル作成
           COPY  "CPORCSS010.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    医療観察入院料コード取得パラメタ
           COPY  "CPORCSIRYOKNS.INC".
      *
      *    亜急性期入院医療管理料判定パラメタ
           COPY  "CPORCSAKYUSEI.INC".
      *
      *    特定入院料算定期間取得パラメタ
           COPY  "CPORCSTOKNYUIN2.INC".
      *
      *    入院料加算取得パラメタ
           COPY  "CPORCSNYUKSNGET.INC".
      *
      *    夜勤時間超過減算入院料コード取得サブ
           COPY "CPORCSYAKINGEN.INC".
      *
      *    夜勤時間特別入院基本料コード取得サブ
           COPY "CPORCSYAKINTOKU.INC".
      *
      *    看護職員数等入院基本料コード取得サブ
           COPY "CPORCSKANGOCHG.INC".
      *
      *    患者番号変換サブ
           COPY "CPORCSPTID.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  PARA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-BTUNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBNYUINCHG"      TO  JOB-PGID
           MOVE    "入院会計一括置換"  TO  JOB-SHELLMSG
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      **     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      **     MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-PREFNUM TO  SPA-PREFNUM
                   ELSE
                         MOVE  "医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         GO  TO  100-INIT-EXT
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    病棟検索
           IF   WRK-PARA-BTUNUM        NOT =   SPACE
             INITIALIZE                      SYS-5001-REC
             MOVE    "5001"              TO  SYS-5001-KANRICD
             MOVE    WRK-PARA-BTUNUM     TO  SYS-5001-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-5001-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
             MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
             MOVE    WRK-PARA-HOSPNUM    TO  SYS-5001-HOSPNUM
             MOVE    SYS-5001-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 910-SYSKANRI-SELECT-SEC
             IF      FLG-SYSKANRI        =   ZERO
                 MOVE    "tbl_syskanri"     TO  MCP-TABLE
                 MOVE    "key10"            TO  MCP-PATHNAME
                 PERFORM     910-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI  NOT =   ZERO
                           MOVE  "病棟情報が取得できませんでした。"
                                             TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  100-INIT-EXT
                 END-IF
             ELSE
                 MOVE    "病棟情報が取得できませんでした。"
                                         TO  WRK-RECEERR
                 PERFORM 500-ERR-HENSYU-SEC
                 GO  TO  100-INIT-EXT
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
      *    主科情報取得
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
               IF      SYS-2009-SYUKA-KBN  =  "1"
                   MOVE   "1"          TO  SPA-SYUKA-KBN
                   MOVE   SYS-2009-SYUKA-NYUIN
                                       TO  SPA-SYUKA-NYUIN
                   MOVE   SYS-2009-SYUKA-MODE
                                       TO  SPA-SYUKA-MODE
                   MOVE   SYS-2009-SYUKA-SELECT
                                       TO  SPA-SYUKA-SELECT
               ELSE
                   MOVE   "0"          TO  SPA-SYUKA-KBN
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票初期処理
           PERFORM 2001-HCMSL55-INIT-SEC
      *
      *    作成対象患者検索
           MOVE    ZERO                 TO      FLG-PTNYUINRRK
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    SYS-1001-HOSPNUM     TO      PTNYUINRRK-HOSPNUM
      *    月末日
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY7-AREA
           MOVE    "71"                 TO      LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM       TO      LNK-DAY7-YM
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI      TO      PTNYUINRRK-TENNYUYMD
      *    月初日
           MOVE    WRK-PARA-SRYYM       TO      PTNYUINRRK-TENSTUYMD
                                                                (1:6)
           MOVE    "01"                 TO      PTNYUINRRK-TENSTUYMD
                                                                (7:2)
           MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
           MOVE    "key50"              TO      MCP-PATHNAME
           PERFORM   910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK       =       ZERO
               MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
               MOVE    "key50"              TO      MCP-PATHNAME
               PERFORM  910-PTNYUINRRK-READ-SEC
               MOVE     ZERO            TO  WRK-CHK-PTID
               PERFORM  UNTIL  FLG-PTNYUINRRK    =    1
      *                 DISPLAY "WRK-PARA-BTUNUM = " WRK-PARA-BTUNUM
                   IF   PTNYUINRRK-PTID     NOT =  WRK-CHK-PTID
                       MOVE   PTNYUINRRK-PTID  TO  WRK-CHK-PTID
                       IF   WRK-PARA-BTUNUM      =    SPACE
                           MOVE    PTNYUINRRK-PTID   TO  WRK-PTID
      *                    入院会計存在チェック
                           PERFORM    2001-NYUINACCT-CHK-SEC
      *                    入院会計更新処理
                           PERFORM     2001-NYUINACCT-UPD-SEC
                       ELSE
                         IF   PTNYUINRRK-BTUNUM  =   WRK-PARA-BTUNUM
                             MOVE    PTNYUINRRK-PTID   TO  WRK-PTID
      *                      入院会計存在チェック
                             PERFORM    2001-NYUINACCT-CHK-SEC
      *                      入院会計更新処理
                             PERFORM     2001-NYUINACCT-UPD-SEC
                         END-IF
                       END-IF
                   END-IF
                   MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
                   MOVE    "key50"              TO      MCP-PATHNAME
                   PERFORM  910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
           MOVE    "key50"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF    ( CNT-LINE            >   ZERO )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票初期処理
      *****************************************************************
       2001-HCMSL55-INIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
                                           CNT-PAGE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2001-HCMSL55-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計存在チェック処理（食事と保険の退避処理も併せて処理）
      *****************************************************************
       2001-NYUINACCT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCTARI
           MOVE    SPACE               TO  WRK-TAIHI-KSN-SRYCD
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           DISPLAY "NACCT-HOSPNUM = " NACCT-HOSPNUM
           DISPLAY "NACCT-NYUGAIKBN = " NACCT-NYUGAIKBN
           DISPLAY "NACCT-PTID = " NACCT-PTID
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      ***                  OR   ( FLG-NYUINACCTARI    NOT =   ZERO )
      *
                   IF    ( NACCT-ZAISKBKBN     =    "1"    )
                       MOVE    1           TO  FLG-NYUINACCTARI
                   END-IF
      *            食事の退避
                   IF    ( NACCT-ZAISKBKBN     =    "4"    )   AND
                         ( NACCT-ZAITEN        =    ZERO   )
                       PERFORM VARYING IDX-SKJ FROM 1 BY 1
                          UNTIL (IDX-SKJ    >    31)
                              EVALUATE   NACCT-SRYCDTOTAL
      *                           朝食
                                  WHEN   099999918
                                      MOVE  NACCT-DAY(IDX-SKJ)
                                             TO  WRK-TAIHI-SKJ18
                                                     (IDX-SKJ)
      *                           昼食
                                  WHEN   099999919
                                      MOVE  NACCT-DAY(IDX-SKJ)
                                             TO  WRK-TAIHI-SKJ19
                                                     (IDX-SKJ)
      *                           夕食
                                  WHEN   099999920
                                      MOVE  NACCT-DAY(IDX-SKJ)
                                             TO  WRK-TAIHI-SKJ20
                                                     (IDX-SKJ)
                              END-EVALUATE
                       END-PERFORM
                   END-IF
      *            外泊の退避
                   IF    ( NACCT-ZAISKBKBN     =    "2"    )
                       PERFORM VARYING IDX-GAIHAKU  FROM 1 BY 1
                          UNTIL (IDX-GAIHAKU   >    31)
                           MOVE    NACCT-DAY(IDX-GAIHAKU)
                                           TO  WRK-TAIHI-GAIHAKU
                                                       (IDX-GAIHAKU)
                       END-PERFORM
                   END-IF
      *            保険組合せの退避
                   IF    ( NACCT-ZAISKBKBN     =    "5"    )
                       PERFORM VARYING IDX-HKN  FROM 1 BY 1
                          UNTIL (IDX-HKN       >    31)
                           MOVE    NACCT-DAY(IDX-HKN)
                                           TO  WRK-TAIHI-HKN(IDX-HKN)
                       END-PERFORM
                   END-IF
      *
      *            入退院登録画面で設定する加算の退避
      *              有床診療所一般病床初期加算（有床診療所入院基本料）
      *              190135470
      *              救急・在宅等支援療養病床初期加算（有床診療所療養病床入院基本料）
      *              190135670
      *              救急・在宅等支援療養病床初期加算
      *              190134270
      *              救急・在宅等支援療養病床初期加算（療養病棟入院基本料１）
      *              190143370
      *              救急・在宅等支援病床初期加算（一般病棟入院基本料）
      *              190142970
      *              救急・在宅等支援病床初期加算（特定一般病棟入院料）
      *              190152470
      *              救急・在宅等支援病床初期加算（地域包括ケア病棟入院料）
      *              190177170
      **             DISPLAY   "PTNYUINRRK-PTID = " PTNYUINRRK-PTID
                   IF    ( NACCT-ZAISKBKBN     =    "6"    )  AND
                         ( NACCT-SRYCDTOTAL    =    190135470 OR
                                                    190135670 OR
                                                    190134270 OR
                                                    190143370 OR
                                                    190142970 OR
                                                    190152470 OR
                                                    190177170 )
      **                 DISPLAY "NACCT-SRYCDTOTAL(6:9) = " 
      **                          NACCT-SRYCDTOTAL(6:9)
      *                該当入院加算が前月以前に存在するかチェックし、
      *                存在した場合のみ加算セットを行うようにする
                       MOVE    ZERO    TO  FLG-NYUINACT
                       INITIALIZE                        NYUINACT-REC
                       MOVE    PTNYUINRRK-HOSPNUM    TO  NSRY-HOSPNUM
                       MOVE    "1"                   TO  NSRY-NYUGAIKBN
                       MOVE    PTNYUINRRK-PTID       TO  NSRY-PTID
                       MOVE    NACCT-SRYCDTOTAL(6:9) TO  NSRY-SRYCD(1)
                                                         NSRY-SRYCD(2)
                                                         NSRY-SRYCD(3)
                                                         NSRY-SRYCD(4)
                                                         NSRY-SRYCD(5)
                       MOVE    "tbl_nyuinact"        TO  MCP-TABLE
                       MOVE    "key6"                TO  MCP-PATHNAME
                       PERFORM 910-NYUINACT-SELECT-SEC
                       IF      FLG-NYUINACT          =   ZERO
      *
                          MOVE    "tbl_nyuinact"     TO  MCP-TABLE
                          MOVE    "key6"             TO  MCP-PATHNAME
                          PERFORM 910-NYUINACT-READ-SEC
                          PERFORM UNTIL  FLG-NYUINACT  NOT =   ZERO
                             DISPLAY  "NACCT-SRYCDTOTAL(6:9) = "
                                       NACCT-SRYCDTOTAL(6:9)
                             DISPLAY  "NSRY-SRYYM = " NSRY-SRYYM
                             IF    NSRY-SRYYM      <  WRK-PARA-SRYYM
                                MOVE    NACCT-SRYCDTOTAL(6:9)
                                               TO   WRK-TAIHI-KSN-SRYCD
                                PERFORM VARYING IDX-KSN  FROM 1 BY 1
                                  UNTIL (IDX-KSN       >    31)
                                   MOVE    NACCT-DAY(IDX-KSN)
                                         TO  WRK-TAIHI-KSN-DAY(IDX-KSN)
                                END-PERFORM
                             END-IF
                             MOVE    "tbl_nyuinact"   TO  MCP-TABLE
                             MOVE    "key6"           TO  MCP-PATHNAME
                             PERFORM 910-NYUINACT-READ-SEC
                          END-PERFORM
                       END-IF
      *
                       MOVE    "tbl_nyuinact"      TO  MCP-TABLE
                       MOVE    "key6"              TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-NYUINACCT-SONZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計更新処理
      *****************************************************************
       2001-NYUINACCT-UPD-SEC          SECTION.
      *
           MOVE    SPACE          TO        WRK-ERRCD
      *
      *    入院月が当月でないことのチェック
           IF  (WRK-PARA-SRYYM    =    PTNYUINRRK-NYUINYMD(1:6)) AND
               (WRK-PARA-SRYYM    =    PTNYUINRRK-TENNYUYMD(1:6))
               MOVE   "2007"      TO        WRK-ERRCD
           END-IF
      *
      *    退院済分でないことのチェック
           IF   WRK-ERRCD         =     SPACE
               IF  (PTNYUINRRK-TAIINYMD    NOT =  "99999999")
                   MOVE   "2008"      TO        WRK-ERRCD
               END-IF
           END-IF
      *
      *    定期請求済分でないことのチェック
           IF   WRK-ERRCD         =     SPACE
               INITIALIZE                      SYUNOU-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  SYU-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  SYU-PTID
               MOVE    WRK-PARA-SRYYM      TO  SYU-SRYYMD(1:6)
               MOVE    "%"                 TO  SYU-SRYYMD(7:1)
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key60"             TO  MCP-PATHNAME
               PERFORM 910-SYUNOU-SELECT-SEC
               IF      FLG-SYUNOU          =   ZERO
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "key60"         TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
                   IF    FLG-SYUNOU        =   ZERO
                       MOVE   "2009"       TO  WRK-ERRCD
                   END-IF
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key60"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    月内で異動処理分のデータでないことのチェック
           IF   WRK-ERRCD           =    SPACE
               IF  (WRK-PARA-SRYYM  =    PTNYUINRRK-TENNYUYMD(1:6)) OR
                   (WRK-PARA-SRYYM  =    PTNYUINRRK-TENSTUYMD(1:6))
                   MOVE   "2010"    TO   WRK-ERRCD
               END-IF
           END-IF
      *
      *   処理対象月以降に異動がないことのチェック
           IF   WRK-ERRCD           =    SPACE
               IF (PTNYUINRRK-TENSTUYMD       NOT = "99999999")  AND
                  (PTNYUINRRK-TENSTUYMD(1:6)  >   WRK-PARA-SRYYM)
                   MOVE   "2013"    TO   WRK-ERRCD
               END-IF
           END-IF
      *
      *    特定入院料算定患者でないことのチェック
      *    (2013.11.15-MEMO)このチェックは外す
      **     IF   WRK-ERRCD           =    SPACE
      **        DISPLAY "PTNYUINRRK-TOKUTEI-KBN ="
      **                 PTNYUINRRK-TOKUTEI-KBN
      **        DISPLAY "PTNYUINRRK-TOKUTEI-NYUIN ="
      **                 PTNYUINRRK-TOKUTEI-NYUIN
      **        IF        PTNYUINRRK-TOKUTEI-KBN  =  SPACE
      **            CONTINUE
      **        ELSE
      **          IF  (PTNYUINRRK-TOKUTEI-KBN    =   "3")     AND
      **             ((PTNYUINRRK-TOKUTEI-NYUIN  >=   "25")   AND
      **              (PTNYUINRRK-TOKUTEI-NYUIN  <=   "48"))
      **                   CONTINUE
      **          ELSE
      **              MOVE   "2011"    TO   WRK-ERRCD
      **          END-IF
      **        END-IF
      **     END-IF
      *
           IF        (FLG-NYUINACCTARI   =    1)   AND
                     (WRK-ERRCD          =    SPACE)
      *        既に作成済の入院会計が存在すれば
      *        削除を行う
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM   900-ORCDBMAIN-SEC
      *
               INITIALIZE                      NYUINACT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NSRY-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NSRY-PTID
               MOVE    WRK-PARA-SRYYM      TO  NSRY-SRYYM
               MOVE    NYUINACT-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "del13"             TO  MCP-PATHNAME
               PERFORM   900-ORCDBMAIN-SEC
      *
      *        会計再作成
               PERFORM     410191-KAIKEI-SAKUSEI-SEC
      *
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-NYUINACCT-UPD-EXT.
           EXIT.
      ******************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-HCMSL55-HEN-SEC            SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
      *    患者情報検索
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    患者番号検索
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           ADD     1                   TO  CNT-JOB-UPDCNT
           MOVE    CNT-JOB-UPDCNT      TO  LST-NO    (CNT-LINE)
           MOVE    PTNUM-PTNUM         TO  LST-PTNUM (CNT-LINE)
      *
           MOVE    PTINF-NAME          TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           IF    ( FLG-ENTRYCHK        =   2 )
               MOVE    ALL "　"        TO  LST-NAME  (CNT-LINE)
               STRING  PTINF-NAME          DELIMITED   BY  SPACE
               INTO    LST-NAME  (CNT-LINE)
               END-STRING
           ELSE
               MOVE    PTINF-NAME      TO  LST-NAME  (CNT-LINE)
           END-IF
      *
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK2-SYORI
               MOVE    LST-NAME  (CNT-LINE)
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING
                                       ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  LST-NAME  (CNT-LINE)
           END-IF
      *
      *    病棟名編集
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                       MOVE    ALL "　"        TO  LST-BYOTO
                                                     (CNT-LINE)
                       STRING  SYS-5001-BTU-NAME  DELIMITED  BY  SPACE
                       INTO    LST-BYOTO  (CNT-LINE)
                       END-STRING
                   END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    WRK-PARA-SRYYM      TO  WRK-SRYYM
           MOVE    WRK-SRYYM           TO  LST-SRYYM (CNT-LINE)
           IF      FLG-NYUINACCTARI   =   ZERO
               MOVE  "更新対象入院会計がありませんでした" TO  WRK-NAIYO
           ELSE
               EVALUATE    WRK-ERRCD
                   WHEN    SPACE
                       MOVE  "入院会計を更新しました" TO  WRK-NAIYO
                   WHEN    "2003"
                       MOVE  "通算日数の取得に失敗しました"
                                                      TO  WRK-NAIYO
                   WHEN    "2006"
                       MOVE  "入院期間加算の取得に失敗しました"
                                                      TO  WRK-NAIYO
                   WHEN    "2007"
                       MOVE  "対象月入院登録分のため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2008"
                       MOVE  "退院済み患者のため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2009"
                       MOVE  "定期請求済みのため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2010"
                    MOVE  "対象月での異動処理があるため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2011"
                      MOVE  "特定入院料患者のため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2012"
                      MOVE  "入院行為照会用テーブルの更新に失敗"
                                                      TO  WRK-NAIYO
                   WHEN    "2013"
                    MOVE  "対象月以降で異動処理があるため処理できません"
                                                      TO  WRK-NAIYO
                   END-EVALUATE
           END-IF
           MOVE    WRK-NAIYO           TO  LST-NAIYO (CNT-LINE)
      *
           IF    ( CNT-LINE            >=  CONST-LST-MEISAI-MAX )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2002-HCMSL55-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2002-HCMSL55-PRT-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  LST-HEAD-PAGE
           MOVE    LST-HEAD1           TO  HCMSL55-MOJI (1)
           MOVE    LST-HEAD            TO  HCMSL55-MOJI (2)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LST-MEISAI-MAX )
      *
               MOVE    LST-MEISAI-OCC (IDX0)
                                       TO  HCMSL55-MOJI
                                          (IDX0 + CONST-LST-HEAD-LINE)
      *
           END-PERFORM
      *
           PERFORM 290-PRINT-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2002-HCMSL55-PRT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       290-PRINT-SEC                   SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE   "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "入院会計一括置換"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       290-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
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
      *
      *    全角文字の場合、間に半角スペースがなかったかチェック
           IF    ( KANACHK-SYUBETU     =   2  )
            AND  ( KANACHK-RC2         =   8  )
               MOVE    9               TO  FLG-ENTRYCHK
           ELSE
               MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   会計作成処理
      *****************************************************************
       410191-KAIKEI-SAKUSEI-SEC            SECTION.
      *
      *    入院会計テーブル作成処理
           PERFORM 410191-NYUACCT-SAKUSEI-SEC
           IF      IPN-NYUKAIKEI-SAKUSEI-FLG   =   "1"
               PERFORM 410191-IPNNYUACCT-SAKUSEI-SEC
           END-IF
      *
           .
      *
       410191-KAIKEI-SAKUSEI-EXT.
           EXIT.
      *
      *021118
      *****************************************************************
      *    入院会計テーブル作成処理                                    
      *****************************************************************
       410191-NYUACCT-SAKUSEI-SEC   SECTION.
      *
           MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    WRK-PARA-SRYYM      TO  WRK-NAI-SRYYM-41.
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID            TO  PTNYUINRRK-PTID
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM  910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key62"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                           TO  WRK-TOKUTEI-NYUIN
      *        老人性認知症疾患療養病棟入院料は平成１８年３月迄
               IF     (PTNYUINRRK-TOKUTEI-KBN   =   "2")   AND
                      (PTNYUINRRK-TOKUTEI-NYUIN =   "06")  AND
                      (WRK-PARA-SRYYM           >=  "200604")
                       MOVE      SPACE     TO  WRK-TOKUTEI-NYUIN
               END-IF
               MOVE    PTNYUINRRK-REC      TO  WK-PTNYU-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD       NOT =  SPACE )
               GO  TO  410191-NYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYU-REC        TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-PARA-SRYYM      TO  SYOKIKSN-IN-SANTEISTYMD(1:6)
           MOVE    "01"                TO  SYOKIKSN-IN-SANTEISTYMD(7:2)
      *
      *     DISPLAY "====AZU  KAIKEI-SAKUSEIL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *
           IF      WRK-TOKUTEI-NYUIN           NOT =   SPACE
               PERFORM 4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
      *        以下入院料を追加（平成２８年改正で対応）
      *        ・療養病棟・一般病棟７対１入院基本料
      *        ・療養病棟・一般病棟１０対１入院基本料
      *        ・療養病棟・一般病棟１３対１入院基本料
      *        ・療養病棟・一般病棟１５対１入院基本料
      *        以下入院料を追加（平成３０年改正で対応）
      *        ・療養病棟・急性期一般入院料２
      *        ・療養病棟・急性期一般入院料３
      *        ・療養病棟・急性期一般入院料４
      *        ・療養病棟・急性期一般入院料５
      *        ・療養病棟・急性期一般入院料６
      *        ・療養病棟・地域一般入院料１
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"   OR
      *                                         平成２８年改正対応
                                                "190198110"   OR
                                                "190198210"   OR
                                                "190198310"   OR
                                                "190198410"   OR
      *                                         平成３０年改正対応
                                                "190201910"   OR
                                                "190202010"   OR
                                                "190202110"   OR
                                                "190202210"   OR
                                                "190202310"   OR
                                                "190202410"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"      TO  SPA-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM 4801-BTUSEL-SEC
               IF      SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   PERFORM 4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  WRK-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  WRK-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し（処理区分９により次月分のみ作成）
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                      TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-EDSRYYMD
           PERFORM VARYING IDX6        FROM    1   BY   1
                   UNTIL  (IDX6            >     7 )    OR
                          (SYOKIKSN-OT-KGNSRYCD(IDX6)   =  SPACE)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (IDX6)
           END-PERFORM
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    特定入院で入院料が変更になるコードの対応
      *
      *    ===  TEST ===
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "2"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   SYOKIKSN-IN-SANTEISTYMD  TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-SHONUM     TO  LNK-TOKNYUIN2-SHONUM
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-TOKUTEI-NYUIN
           IF    LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-FLG(1)
           END-IF
      *    療養病棟（病床）入院のフラグセット
           INITIALIZE                       SYS-5001-REC
           MOVE    "5001"               TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM    TO  SYS-5001-KBNCD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                        TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                 TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD    TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM          TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC         TO  MCPDATA-REC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_syskanri"   TO  MCP-TABLE
               MOVE    "key10"          TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI       =   ZERO
                   IF    SYS-5001-BTU-SBT    =   "04"  OR  "09"
                       MOVE    "1"     TO
                                  LNK-TOKNYUIN2-TOKUBETSU-FLG(2)
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                IF    LNK-TOKNYUIN2-RC  =   1
                    MOVE    SPACE         TO  WRK-TOKUTEI-NYUIN
                                              SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                END-IF
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
      *
      *    入院料加算診療コード取り込み
           PERFORM    410191-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      WRK-TOKUTEI-NYUIN   =   SPACE
               MOVE     "0"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
      *            特定入院基本料（一般病棟入院基本料）はマスタが
      *            平成２６年９月３０日までの為、参照時の日付を変更
                   IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                       MOVE    "20140930"   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   END-IF
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"      TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"      TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE    0                   TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI        =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570" TO  LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1           TO  LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    0               TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE   "00"             TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      *
      *     DISPLAY   "****4801SENTEI-SET-SEC MAE **** "
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1         TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO      TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
      *     DISPLAY   "********ORCSNYUACCT MAE **********"
           MOVE    WRK-PARA-HOSPNUM TO  SPA-HOSPNUM
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           DISPLAY "SPA-HOSPNUM = " SPA-HOSPNUM
           DISPLAY "SPA-PTID = " SPA-PTID
           MOVE     PTNYUINRRK-BTUNUM
                                   TO  LNK-SCNYUACCT-BTU-NUM
           CALL    "ORCSNYUACCT"       USING   SPA-AREA
                                               ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA      TO  SPA-SRYKA
      *
      *
      *    システム管理５０００の外泊区分を取得
           INITIALIZE                      SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    WRK-PARA-SRYYM      TO  SYS-5000-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5000-STYUKYMD (7:2)
           MOVE    SYS-5000-STYUKYMD   TO  SYS-5000-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-5000-HOSPNUM
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           DISPLAY "===== GAIHAKU-KBN = " SYS-5000-NACCT-GAIHAKUKBN
      *
      *    入院会計作成後に退避データの食事区分を復元
           IF WRK-PARA-SRYYM        >= "200604"
      *       （更新月分）
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                 MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
      *              食事を復元
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM 1 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TAIHI-SKJ18(IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                               IF  (SYS-5000-NACCT-GAIHAKUKBN = "2") AND
                                   (WRK-TAIHI-GAIHAKU(IDX-SKJ) =  1 OR
                                                                  2 OR
                                                                  3)
                                    MOVE  ZERO  TO   NACCT-DAY(IDX-SKJ)
                               END-IF
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM 1 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TAIHI-SKJ19(IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                               IF  (SYS-5000-NACCT-GAIHAKUKBN = "2") AND
                                   (WRK-TAIHI-GAIHAKU(IDX-SKJ) =  1 OR
                                                                  2 OR
                                                                  3)
                                    MOVE  ZERO  TO   NACCT-DAY(IDX-SKJ)
                               END-IF
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM 1 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TAIHI-SKJ20(IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                               IF  (SYS-5000-NACCT-GAIHAKUKBN = "2") AND
                                   (WRK-TAIHI-GAIHAKU(IDX-SKJ) =  1 OR
                                                                  2 OR
                                                                  3)
                                    MOVE  ZERO  TO   NACCT-DAY(IDX-SKJ)
                               END-IF
                          END-PERFORM
                     END-EVALUATE
      *              剤回数再セット
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM
      *              入院会計更新
                     MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                     MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                     MOVE     "key"              TO  MCP-PATHNAME
                     PERFORM 910-DBUPDATE-SEC
                 END-IF
      *          入院会計マスタクローズ
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 900-CLOSE-SEC
              END-PERFORM
           END-IF
      *
      *    入院会計作成後に食堂加算に外泊区分を反映
      *    （更新月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
      *        外泊を反映
               PERFORM   VARYING   IDX   FROM  1  BY  1
                         UNTIL     IDX    >   31
                 IF  (SYS-5000-NACCT-GAIHAKUKBN = "2") AND
                     (WRK-TAIHI-GAIHAKU(IDX)     =  1 OR
                                                    2 OR
                                                    3)
                     MOVE  ZERO  TO   NACCT-DAY(IDX)
                 END-IF
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX   FROM  1  BY  1
                        UNTIL     IDX  >   31
                   IF   NACCT-DAY(IDX)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    入院会計作成後に入院料加算に外泊区分を反映
      *    （更新月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "6"                 TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           PERFORM  UNTIL   FLG-NYUINACCT     NOT =   ZERO
      *        外泊を反映
               PERFORM   VARYING   IDX   FROM  1  BY  1
                         UNTIL     IDX    >   31
                 IF  (SYS-5000-NACCT-GAIHAKUKBN = "2") AND
                     (WRK-TAIHI-GAIHAKU(IDX)     =  1 OR
                                                    2 OR
                                                    3)
                     MOVE  ZERO  TO   NACCT-DAY(IDX)
                 END-IF
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX   FROM  1  BY  1
                        UNTIL     IDX  >   31
                   IF   NACCT-DAY(IDX)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM  910-NYUINACCT-READ-SEC
           END-PERFORM
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    入院会計作成後に退避データの外泊区分を復元
      *    （更新月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
      *        外泊を復元
               PERFORM   VARYING   IDX-GAIHAKU FROM  1  BY  1
                         UNTIL     IDX-GAIHAKU   >   31
                     MOVE   WRK-TAIHI-GAIHAKU(IDX-GAIHAKU)
                                       TO  NACCT-DAY(IDX-GAIHAKU)
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-GAIHAKU FROM  1  BY  1
                        UNTIL     IDX-GAIHAKU   >   31
                   IF   NACCT-DAY(IDX-GAIHAKU)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    入院会計作成後に退避データの保険組合せを復元
      *    （更新月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
      *        保険組合せを復元
               PERFORM   VARYING   IDX-HKN FROM  1  BY  1
                         UNTIL     IDX-HKN   >   31
                     MOVE   WRK-TAIHI-HKN(IDX-HKN)
                                       TO  NACCT-DAY(IDX-HKN)
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-HKN FROM  1  BY  1
                        UNTIL     IDX-HKN   >   31
                   IF   NACCT-DAY(IDX-HKN)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    PTNYUINRRK-PTID     TO  SS010-PTID
            MOVE    WRK-PARA-SRYYM      TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  WRK-ERRCD
            END-IF
      *
           .
      *
       410191-NYUACCT-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計テーブル作成処理（特定入院料後の一般入院料）
      *****************************************************************
       410191-IPNNYUACCT-SAKUSEI-SEC   SECTION.
      *
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    IPN-SANTEISTYMD(1:6)  TO WRK-NAI-SRYYM-41(1:6)
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =  ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key62"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-REC  TO  WK-PTNYU-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD    NOT =  SPACE )
               GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYU-REC        TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    IPN-SANTEISTYMD     TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    SPACE               TO  WRK-TOKUTEI-NYUIN
           IF      WRK-TOKUTEI-NYUIN   NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   PERFORM 4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  WRK-ERRCD
                       GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
      *            患者番号取得
                   INITIALIZE                      ORCSPTIDAREA
                   MOVE    WRK-PARA-HOSPNUM    TO  SPTID-HOSPNUM
                   MOVE    WRK-PTID            TO  SPTID-PTID
                   CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                       SPA-AREA
      *
                   STRING  "特定入院料算定上限以降の入院基本料が"
                                               DELIMITED BY SIZE
                           "システム管理から取得できませんでした。"
                                               DELIMITED BY SIZE
                           "患者番号＝"        DELIMITED BY SIZE
                            SPTID-PTNUM        DELIMITED BY SIZE
                                               INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE      SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
                                           LNK-SCNYUACCT-EDSRYYMD
           PERFORM VARYING     IDX6    FROM    1   BY   1
               UNTIL          (IDX6        >   7   )   OR
                              (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE)
               MOVE    SYOKIKSN-OT-KGNSRYCD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                             (IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (IDX6)
           END-PERFORM
      *    通算算定済日数
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    入院料加算診療コード取り込み
           PERFORM 410191-NYUKSN-GET-SEC
      *    特定入院料区分のセット
           MOVE    "0"             TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *    外泊
           MOVE    "099999911"     TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE    "099999912"     TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE    WRK-TAIHI-SKJ18(1)  TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI           =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570" TO  LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1           TO  LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    WRK-TAIHI-HKN(1)
                                   TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE    "00"            TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      **   
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1         TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO      TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
           MOVE    WRK-PARA-HOSPNUM TO  SPA-HOSPNUM
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"    USING   SPA-AREA
                                           ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA  TO  SPA-SRYKA
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    PTNYUINRRK-PTID     TO  SS010-PTID
            MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                        TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  WRK-ERRCD
            END-IF
      *
      *
           .
      *
       410191-IPNNYUACCT-SAKUSEI-EXT.
           EXIT.
      *021118
      *****************************************************************
      *    通算日数取得処理
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
           MOVE    SPACE           TO  WRK-ERRCD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    WRK-PARA-HOSPNUM  TO  STUSAN-HOSPNUM
           MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
           MOVE    WRK-PARA-SRYYM  TO  STUSAN-KJNYMD(1:6)
           IF   IPN-NYUKAIKEI-SAKUSEI-FLG  =   "1"
               MOVE   IPN-SANTEISTYMD(7:2) TO  STUSAN-KJNYMD(7:2)
           ELSE
               MOVE    "01"                TO  STUSAN-KJNYMD(7:2)
           END-IF
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC       NOT =   ZERO )
               MOVE    "2003"      TO  WRK-ERRCD
               GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
      *    他院（特別な関係にある医療機関）の日数を加算
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
           COMPUTE WRK-TSUSAN      =   STUSAN-NISSU03  -   1
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    STUSAN-KJNYMD   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN       =   180 - ( STUSAN-NISSU04 )
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *
      *****************************************************************
      *     選定療養費診療行為コード取得処理
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE                  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                       TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                               TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD  NOT =  SPACE
               MOVE    "099999916"     TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                       TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
           MOVE    WRK-SENTEI-STYMD    TO  LNK-SCNYUACCT-SENTEI-STYMD
      *    １５才未満の患者は選定入院対象外
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
           MOVE     PTINF-BIRTHDAY     TO  WRK-BIRTHDAY-15
                                           SPA-BIRTHDAY
           ADD      15                 TO  WRK-BIRTHDAY-15Y
           IF       WRK-SENTEI-STYMD   <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       410191-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       410191-KIKAN-CALC-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    特定入院料コードをサブプロより取得
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
           MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                        TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                   MOVE   LNK-TOKNYUIN-SRYCD   TO  SYOKIKSN-IN-KHNSRYCD
               ELSE
                   MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       410191-NYUKSN-GET-SEC         SECTION.
      *
      *    入院料加算取得サブ
           INITIALIZE                       ORCSNYUKSNGET-AREA
           MOVE   WRK-PARA-HOSPNUM      TO  LNK-NYUKSNGET-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-NYUKSNGET-PTID
           MOVE   WRK-PARA-SRYYM        TO  LNK-NYUKSNGET-SANTEIYM
           MOVE   PTNYUINRRK-BTUNUM     TO  LNK-NYUKSNGET-BTUNUM
           MOVE   PTNYUINRRK-BRMNUM     TO  LNK-NYUKSNGET-BRMNUM
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1)   NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           END-IF
      *
      *    特定入院料算定要件外で療養病棟入院基本料Ｉを算定時に、入院料
      *    加算を特別判定する場合の判定区分セット
           IF      LNK-TOKNYUIN-GAI-NYUKSNKBN   =    "1"
               MOVE   "1"               TO  LNK-NYUKSNGET-GAI-NYUKSNKBN
           END-IF
      *
           CALL    "ORCSNYUKSNGET"      USING  ORCSNYUKSNGET-AREA
                                               SPA-AREA
      *    入院料加算取得サブからの返却値セット
           PERFORM    VARYING    IDX8   FROM   1   BY   1
             UNTIL    (LNK-NYUKSNGET-KSNSRYCD(IDX8)  =  SPACE)  OR
                      (IDX8   >     20)
                 MOVE   LNK-NYUKSNGET-NYUKSNKBN(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX8)
                 MOVE   LNK-NYUKSNGET-KSNSRYCD(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
           END-PERFORM
           MOVE    LNK-NYUKSNGET-TOKUBETSU-NYUIN
                                        TO  WRK-TOKUBETSU-NYUIN
      *
      *    事前に退避していた入院料加算のセット
           MOVE    SPACE     TO  LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX8)
           DISPLAY  "!!!!! WRK-TAIHI-KSN-SRYCD SET = "
                      WRK-TAIHI-KSN-SRYCD
           IF      WRK-TAIHI-KSN-SRYCD    NOT =   SPACE
      *         加算と入院料の表用番号が一致するかチェック
                PERFORM   48011-TENSU-SEL-SEC
                PERFORM   48012-TENSU-SEL-SEC
                DISPLAY "WRK-BTU-CDKBN-KBNNUM = "
                         WRK-BTU-CDKBN-KBNNUM
                DISPLAY "WRK-BTU-CDKBN-KBNNUM-EDA = "
                         WRK-BTU-CDKBN-KBNNUM-EDA
                DISPLAY "WRK-KSN-CDKBN-KBNNUM = "
                         WRK-KSN-CDKBN-KBNNUM
                DISPLAY "WRK-KSN-CDKBN-KBNNUM-EDA = "
                         WRK-KSN-CDKBN-KBNNUM-EDA
      *        表用番号が同一であることの判定
               IF   WRK-KSN-CDKBN-KBNNUM    NOT =    ZERO
                 IF  (WRK-BTU-CDKBN-KBNNUM  = WRK-KSN-CDKBN-KBNNUM)  AND
                     (WRK-BTU-CDKBN-KBNNUM-EDA
                                            = WRK-KSN-CDKBN-KBNNUM-EDA)
                    MOVE    WRK-TAIHI-KSN-SRYCD
                             TO  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
                 END-IF
              END-IF
           END-IF
           .
      *
       410191-NYUKSN-GET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスタ検索処理（入院料）
      *****************************************************************
       48011-TENSU-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM-EDA
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  TNS-HOSPNUM
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1)   NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                        TO  TNS-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                        TO  TNS-SRYCD
           END-IF
           MOVE    WRK-PARA-SRYYM  TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
               MOVE    ZERO        TO  FLG-TENSU
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48011-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-CDKBN-KBNNUM
                                   TO WRK-BTU-CDKBN-KBNNUM
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                   TO WRK-BTU-CDKBN-KBNNUM-EDA
      *    療養病床以外・有床診療所療養病床入院基本料のとき、
      *    表用番号の置き換え（平成２４年改正）
           IF     (TNS-CDKBN-KBNNUM  = 108)   AND
                  (TNS-CDKBN-KOUBAN  =  10)   AND
                 ((TNS-YUKOSTYMD    >=  "20120401")   AND
                  (TNS-YUKOSTYMD    <=  "20140331"))
               MOVE   109           TO WRK-BTU-CDKBN-KBNNUM
               MOVE   ZERO          TO WRK-BTU-CDKBN-KBNNUM-EDA
           END-IF
      *    療養病床以外・有床診療所療養病床入院基本料のとき、
      *    表用番号の置き換え（平成２６年改正）
           IF     (TNS-CDKBN-KBNNUM  = 108)   AND
                  (TNS-CDKBN-KOUBAN  =  13)   AND
                  (TNS-YUKOSTYMD    >=  "20040401")
               MOVE   109           TO WRK-BTU-CDKBN-KBNNUM
               MOVE   ZERO          TO WRK-BTU-CDKBN-KBNNUM-EDA
           END-IF
      *
           .
      *
       48011-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理（入院料加算）
      *****************************************************************
       48012-TENSU-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM-EDA
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  TNS-HOSPNUM
      *
           MOVE    WRK-TAIHI-KSN-SRYCD
                                   TO  TNS-SRYCD
           MOVE    WRK-PARA-SRYYM  TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
               MOVE    ZERO        TO  FLG-TENSU
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48012-TENSU-SEL-EXT
           END-IF
      *
      *    入院料（Ａ１００番台）、特定入院料（Ａ３００番台）に含まれる
      *    注加算のみが判定対象となる
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   100)     AND
                   (TNS-CDKBN-KBNNUM   <=   199)
               MOVE    TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                        TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
      *
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   300)     AND
                   (TNS-CDKBN-KBNNUM   <=   399)
               MOVE    TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                        TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
           .
      *
       48012-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           INITIALIZE              SYS-5001-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD(1:6)
           MOVE    "01"            TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO   SYS-5001-REC
           ELSE
               MOVE    1          TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    FLG-SYSKANRI      NOT =   ZERO
      *        患者番号取得
               INITIALIZE                      ORCSPTIDAREA
               MOVE    WRK-PARA-HOSPNUM    TO  SPTID-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
      *
               STRING  "病棟情報が取得できませんでした。対象年月＝"
                                           DELIMITED BY SIZE
                        WRK-PARA-SRYYM     DELIMITED BY SIZE
                       "患者番号＝"       DELIMITED BY SIZE
                        SPTID-PTNUM        DELIMITED BY SIZE
                                           INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    夜勤時間超過減算コードへの置き換え処理
      *    夜勤時間特別入院基本料コードへの置き換え処理
      *    看護職員数等入院基本料コードへの置き換え処理
      *****************************************************************
       4801-YAKINGEN-CHG-SEC   SECTION.
      *
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20140401"
      *        夜勤時間超過減算入院料コード取得
               INITIALIZE    ORCSYAKINGEN-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINGEN-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINGEN-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINGEN-SRYCD
               CALL   "ORCSYAKINGEN"    USING   ORCSYAKINGEN-AREA
                                                SPA-AREA
               IF   LNK-YAKINGEN-RC     =       ZERO
                    MOVE LNK-YAKINGEN-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20160401"
      *        夜勤時間特別入院基本料コード取得
               INITIALIZE    ORCSYAKINTOKU-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINTOKU-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINTOKU-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINTOKU-SRYCD
               CALL   "ORCSYAKINTOKU"    USING  ORCSYAKINTOKU-AREA
                                                SPA-AREA
               IF   LNK-YAKINTOKU-RC     =       ZERO
                    MOVE LNK-YAKINTOKU-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
      *        看護職員数等入院基本料コード取得
               INITIALIZE    ORCSKANGOCHG-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-KANGOCHG-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-KANGOCHG-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-KANGOCHG-SRYCD
               CALL   "ORCSKANGOCHG"    USING   ORCSKANGOCHG-AREA
                                                SPA-AREA
               IF   LNK-KANGOCHG-RC     =       ZERO
                    MOVE LNK-KANGOCHG-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-YAKINGEN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料一般コードから老人コードへの置き換え処理
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    WRK-PARA-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    WRK-PTID            TO  AGECHK-PTID
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  AGECHK-SRYYMD
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           MOVE    "1"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    老人対象患者であれば入院診療コードの置き換え（労災除く）
           IF     (AGECHK-HKNKBN       =   1  OR  2 )  OR
                  (AGECHK-ROUJIN2      =   ZERO     )
               CONTINUE
           ELSE
      *        平成１８年４月改定対応
      *        老人コード置き換えは療養病棟、療養病床のみ
               IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=  "200604"
                   IF   SYOKIKSN-IN-KHNSRYCD  =  "190080610" OR
                                                 "190080710" OR
                                                 "190081310" OR
                                                 "190081410" OR
                                                 "190098210" OR
                                                 "190098310"
                       INITIALIZE              SRYCDCHG-REC
                       MOVE    SYOKIKSN-IN-KHNSRYCD
                                               TO   CHG-IPNSRYCD
                       MOVE    WRK-PARA-HOSPNUM TO   CHG-HOSPNUM
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "tbl_srycdchg"  TO   WRK-MCP-TABLE
                       MOVE    "key2"          TO   WRK-MCP-PATHNAME
                       PERFORM 900-TBL-SELECT-SEC 
                       IF    ( FLG-DBRC    =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_srycdchg"  TO   MCP-TABLE
                       MOVE    "key2"          TO   MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    WRK-PARA-HOSPNUM TO   CHG-HOSPNUM
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "tbl_srycdchg"  TO  WRK-MCP-TABLE
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
                   PERFORM 900-TBL-SELECT-SEC 
                   IF    ( FLG-DBRC    =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_srycdchg"  TO   MCP-TABLE
                   MOVE    "key2"          TO   MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO              TO  FLG-DBRC
           MOVE    WRK-MCP-TABLE     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME  TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC            =   ZERO )
               MOVE    WRK-MCP-TABLE     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME  TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-SELECT-SEC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-READ-SEC
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *      
      *****************************************************************
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター検索
      *****************************************************************
       910-PTNUM-SELECT-SEC        SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込
      *****************************************************************
       910-PTNUM-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター検索
      *****************************************************************
       910-PTINF-SELECT-SEC        SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター読込
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       980-PTNYUINRRK-SELECT-SEC     SECTION.
      *
           MOVE    WWK-PTNYU-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO             TO  FLG-WWK-PTNYUINRRK
           ELSE
               MOVE    1                TO  FLG-WWK-PTNYUINRRK
               INITIALIZE                   WWK-PTNYU-REC
           END-IF
      *
           .
      *
       980-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       980-PTNYUINRRK-READ-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WWK-PTNYU-REC
               MOVE    ZERO            TO  FLG-WWK-PTNYUINRRK
           ELSE
               INITIALIZE                  WWK-PTNYU-REC
               MOVE    1               TO  FLG-WWK-PTNYUINRRK
           END-IF.
      *
       980-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTCONF-READ-EXT.
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
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM TO  JOB-HOSPNUM
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
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *      
