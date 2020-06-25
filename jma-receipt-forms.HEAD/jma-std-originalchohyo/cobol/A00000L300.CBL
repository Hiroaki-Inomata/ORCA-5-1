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
       PROGRAM-ID.             A00000L300.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 未収金・預り金個人別月計表
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/01/05    @@@@-半田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者        日付      内容
      *  01.00.01    @@@@-門間     11/01/07  コメント履歴削除
      *                                      open-cobol1.0対応
      *  01.00.02    @@@@-門間     11/05/26  処理速度の改善
      *  01.00.03    @@@@-門間     11/08/04  預り金が正しく集計されるように修正
      *  01.00.04    @@@@-門間     12/05/25  CSVファイル作成機能追加
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    未収金／預り金対象者抽出ファイル
           SELECT  L300T-FILE      ASSIGN  L300TP
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     L300T-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    未収金／預り金対象者抽出ファイル
       FD  L300T-FILE.
           COPY    "A00000L300TMP.INC".
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
       01  L300IPARA.
           03  FILLER              PIC X(256) VALUE  SPACE.
      *
      *    一時ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //L300TP//.
      *
           COPY    "A00000L300.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-INVALID         PIC 9(01).
           03  FLG-ATEND           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SRYKA-ALL       PIC 9(01).
           03  INP-STS             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1              PIC 9(04).
           03  IDX2              PIC 9(04).
           03  IDX3              PIC 9(04).
           03  IDX4              PIC 9(04).
           03  IDX5              PIC 9(04).
           03  IDX6              PIC 9(04).
           03  IDX7G.
              05  IDX7           PIC 9(02).
           03  IDX               PIC 9(04).
           03  IDY               PIC 9(04).
           03  IDZ               PIC 9(04).
           03  IDX-S             PIC 9(07).
           03  IDX-N             PIC 9(07).
           03  IDX-MAX           PIC 9(07).
      *
           03  JIHIIDX           PIC 9(04).
           03  NG                PIC 9(04).
      *
           03  AZU-IDX           PIC 9(02).
      *
           03  IDX8              PIC 9(04).
           03  IDX10             PIC 9(04).
           03  IDX-ST            PIC 9(03).
           03  IDX-ED            PIC 9(03).
           03  IDXP              PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SRYYM      PIC X(06).
           03  WRK-PARA-SRYLYMD    PIC X(08).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-SRYKA      PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
           03  WRK-RECEERR           PIC X(200).
           03  WRK-PARA-SRYYMWH      PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
           03  WRK-CHK-SRYKA         PIC X(02).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-4001-TENTANKA PIC 9(02)V9(01).
      *
           03  WRK-Z9-7             PIC Z,ZZZ,ZZ9.
           03  WRK-Z9-8             PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z9-9             PIC ZZZ,ZZZ,ZZ9.
           03  WRK--9-8             PIC --,---,--9.
           03  WRK--9-9             PIC ---,---,--9.
           03  WRK-RENNUM-X         PIC ZZ9.
           03  WRK-PAGE             PIC ZZ9.
           03  WRK-Z3               PIC ZZ9.
           03  WRK-Z5               PIC ZZZZ9.
           03  WRK-Z9               PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ               PIC ZZZZZZZZZZZZ.
           03  WRK-S9               PIC -----------9.
           03  WRK-SZ               PIC -----------Z.
           03  WRK-PR-NYUGAIKBN     PIC X(01).
           03  WRK-PR-SRYKA         PIC X(02).
           03  WRK-KOH1HKNNUM       PIC X(03).
           03  WRK-KOH2HKNNUM       PIC X(03).
           03  WRK-KOH3HKNNUM       PIC X(03).
           03  WRK-KOH4HKNNUM       PIC X(03).
           03  WRK-KINGAKU          PIC S9(13).
           03  WRK-MISYU            PIC S9(09).
           03  WRK-PRDTLCNT         PIC 9(05).
           03  DEBUG-CNT            PIC 9(03).
           03  WRK-SYS-BEDSU        PIC 9(04).
           03  WRK-SYS-BEDSUIPN     PIC 9(04).
           03  WRK-LPRSRYKA         PIC 9(01).
           03  WRK-NEXT-PRSRYKA     PIC 9(03).
           03  WRK-BIRTHDAY.
            05 WRK-BIRTHY           PIC 9(04).
            05 WRK-BIRTHM           PIC 9(02).
            05 WRK-BIRTHD           PIC 9(02).
           03  WRK-NENREI           PIC 9(03).
           03  WRK-INI-AZUKARICD    PIC X(09).
           03  WRK-L-SRYYMD         PIC X(08).
           03  WRK-LN-SRYYMD        PIC X(08).
           03  WRK-LG-SRYYMD        PIC X(08).
           03  WRK-F-SRYYMD         PIC X(08).
           03  WRK-FN-SRYYMD        PIC X(08).
           03  WRK-FG-SRYYMD        PIC X(08).
      *
       01  WRK-AZU-AREA.
           03  WRK-AZU-KAISU         PIC 9(03).
           03  WRK-AZU               PIC 9(08).
           03  WRK-AZU-G             PIC 9(11).
      *
       01  WRK-SRYKA-AERA.
           03  WRK-SRYKAMAX        PIC    9(03).
           03  WRK-SRYKA-TBL       OCCURS 100.
               05  WRK-SRYKA       PIC  X(02).
               05  WRK-SRYKANAME   PIC  X(20).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-RENNUM     PIC X(50) VALUE "連番".
               07  CSV-HEAD-PTNUM      PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-PTKANANAME PIC X(50) VALUE "カナ氏名".
               07  CSV-HEAD-PTNAME     PIC X(50) VALUE "患者氏名".
               07  CSV-HEAD-HKNJANUM   PIC X(50) VALUE "保険者番号".
               07  CSV-HEAD-HKNJANAME  PIC X(50) VALUE "保険者名称".
               07  CSV-HEAD-BIRTHDAY   PIC X(50) VALUE "生年月日".
               07  CSV-HEAD-AGE        PIC X(50) VALUE "年齢".
               07  CSV-HEAD-ZOKUGARA   PIC X(50) VALUE "続柄".
               07  CSV-HEAD-KOHI       PIC X(50) VALUE "公費".
               07  CSV-HEAD-ATAMAGAKI  PIC X(50) VALUE "頭書登録年月日".
               07  CSV-HEAD-MISYU      PIC X(50) VALUE "未収金（円）".
               07  CSV-HEAD-AZUKARI    PIC X(50) VALUE "預り金（円）".
               07  CSV-HEAD-LRAIIN     PIC X(50) VALUE "最終来院日".
      *
             05  CSV-HEAD-R            REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM       PIC X(50) OCCURS 14.
             05  CSV-HEAD-MAX          PIC 9(05) VALUE  14.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY  "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY  "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災情報取得
           COPY  "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
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
           IF  INP-STS   =   ZERO
             PERFORM 200-MAIN-SEC
           ELSE
             PERFORM 210-ERR-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-SRYKA-AERA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  WRK-AZU-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
      *
                                               WRK-PARA-SRYYM
                                               WRK-PARA-SRYKA
                                               WRK-PARA-NYUGAIKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000L300"    TO  JOB-PGID
           MOVE    "未収金・預り金個人別月計表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AL300TP"                TO  L300TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  L300TP-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  L300TP-HOSPNUM
      *
           MOVE    L300TP-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  L300TP-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH
      *
      *    集計日付セット
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM(1:6) TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PARA-SRYYMWH
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO  WRK-PARA-SRYLYMD
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SKYYMD(1:6)
                                       TO  SYS-1001-STYUKYMD(1:6)
                                           SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
               MOVE    SYS-1001-BEDSU      TO  WRK-SYS-BEDSU
               MOVE    SYS-1001-BEDSUIPN   TO  WRK-SYS-BEDSUIPN
           ELSE
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    LNK-PRTKANRI-SKYYMD(1:6)
                                        TO  SYS-1005-STYUKYMD(1:6)
                                            SYS-1005-EDYUKYMD(1:6)
           MOVE    "01"                 TO  SYS-1005-STYUKYMD(7:2)
                                            SYS-1005-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
               IF   FLG-SYSKANRI3 = ZERO
                   MOVE  SYS-1005-KBNCD       TO  WRK-SRYKA(IDX1)
                   MOVE  SYS-1005-SRYKANAME1  TO  WRK-SRYKANAME(IDX1)
                   ADD   1                    TO  IDX1
               END-IF
               PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT  1        FROM IDX1
           MOVE  IDX1         TO   WRK-SRYKAMAX
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *
           IF   WRK-PARA-SRYKA  =    SPACE
      *      全診療印刷フラグON
             MOVE   1                 TO  FLG-SRYKA-ALL
           ELSE
      *      特定診療科のインデクスを取得
             MOVE   WRK-PARA-SRYKA    TO  WRK-CHK-SRYKA
             PERFORM 320-SRYKA-GROUPING-SEC
             IF     IDX2  NOT  =      ZERO
      *        診療科入力値エラーの場合
               MOVE  1                  TO  INP-STS
             END-IF
           END-IF
      *
           IF   WRK-PARA-NYUGAIKBN  NOT =  SPACE  AND  "1"  AND  "2"
                                                  AND  "3"
             MOVE    2         TO   INP-STS
           END-IF
      *
      *    プログラムオプション取込み（預り金コード）
           INITIALIZE                      ORCSPRGOPTIONAREA
           MOVE    "A00000L300"        TO  ORCSPRGOPTION-PRGID
           MOVE    "AZUKARICD"         TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA(1:9)  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA(1:9)  TO  WRK-INI-AZUKARICD
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処  理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT      L300T-FILE
           CLOSE               L300T-FILE
           OPEN    I-O         L300T-FILE
      *
      *    未収金対象者抽出
           PERFORM   910-DBSELECT-SEC
           PERFORM   320-MISYU-GET-SEC
      *
      *    預り金対象者抽出
           PERFORM   911-DBSELECT-SEC
           PERFORM   320-AZUKARI-GET-SEC
      *
           CLOSE               L300T-FILE
           OPEN    INPUT       L300T-FILE
      *
      *    帳票編集印刷処理
           PERFORM   325-CTRL-PRT-SEC
      *
           CLOSE               L300T-FILE
      *
           MOVE    1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE     L300
      *    エラーメッセージ印刷処理
           PERFORM 330-TERM-HEN-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金対象者抽出者処理
      *****************************************************************
       320-MISYU-GET-SEC      SECTION.
      *
           PERFORM  UNTIL  FLG-SYUNOU  =  1
             PERFORM   320-MISYU-GET-SUB-SEC
             PERFORM   910-DBFETCH-SEC
           END-PERFORM
           PERFORM   910-DBCLOSE-SEC
      *
           .
       320-MISYU-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    預り金対象者抽出者処理
      *****************************************************************
       320-AZUKARI-GET-SEC      SECTION.
      *
           PERFORM  UNTIL  FLG-SRYACT  =  1
             PERFORM   320-AZUKARI-GET-SUB-SEC
             PERFORM   911-DBFETCH-SEC
           END-PERFORM
           PERFORM   911-DBCLOSE-SEC
      *
           .
       320-AZUKARI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金対象者抽出サブ処理
      *****************************************************************
       320-MISYU-GET-SUB-SEC      SECTION.
      *
      *    患者マスタ情報抽出
           INITIALIZE                        PTINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTINF-HOSPNUM
           MOVE     SYU-PTID           TO    PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
      *
      *    患者番号情報抽出
           INITIALIZE                        PTNUM-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTNUM-HOSPNUM
           MOVE     SYU-PTID           TO    PTNUM-PTID
           PERFORM  940-PTNUM-READ-SEC
      *
      *    患者保険情報抽出
           INITIALIZE                        PTHKNINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTHKN-HOSPNUM
           MOVE     SYU-PTID           TO    PTHKN-PTID
           PERFORM  950-PTHKNINF-SELECT-SEC
           PERFORM  UNTIL ( PTHKN-HKNJANUM   NOT =  SPACE
                        AND PTHKN-SAKJOKBN   NOT =  "1" )
                       OR   FLG-PTHKNINF         =   1
                    PERFORM  950-PTHKNINF-FETCH-SEC
           END-PERFORM
           PERFORM  950-PTHKNINF-CLOSE-SEC
      *
      *    患者公費情報抽出
           INITIALIZE                        PTKOHINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTKOH-HOSPNUM
           MOVE     SYU-PTID           TO    PTKOH-PTID
           PERFORM  950-PTKOHINF-SELECT-SEC
           PERFORM  UNTIL ( PTKOH-KOHNUM     NOT =  SPACE
                        AND PTKOH-SAKJOKBN   NOT =  "1" )
                       OR   FLG-PTKOHINF         =   1
                    PERFORM  950-PTKOHINF-FETCH-SEC
           END-PERFORM
           PERFORM  950-PTKOHINF-CLOSE-SEC
      *
      *    保険者情報抽出
           INITIALIZE                        HKNJAINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    HKNJA-HOSPNUM
           MOVE     PTHKN-HKNJA-G      TO    HKNJA-HKNJANUM
           PERFORM  960-HKNJA-READ-SEC
      *
      *    頭書登録年月日取得
           PERFORM  321-GET-F-JYURRK-SEC
      *
      *    最終来院日取得
           PERFORM  321-GET-LASTDAY-SEC
      *
      *    入外別レコード
           INITIALIZE                  L300T
           INITIALIZE                  FLG-INVALID
           MOVE    SYU-NYUGAIKBN    TO    L300T-NYUGAIKBN
           MOVE    SYU-SRYKA        TO    L300T-SRYKA
           MOVE    PTNUM-PTNUM      TO    L300T-PTNUM
           READ L300T-FILE
                KEY      IS      L300T-KEY
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      *
           PERFORM  320-MISYU-GET-SUB2-SEC
      *
           IF   WRK-PARA-NYUGAIKBN  NOT =  "3"
              AND  ( WRK-PARA-NYUGAIKBN  =  L300T-NYUGAIKBN
              OR  WRK-PARA-NYUGAIKBN  =  SPACE )
             IF   FLG-INVALID      =    ZERO
               INITIALIZE       FLG-INVALID
               REWRITE L300T
                       INVALID  KEY
                       MOVE  1  TO FLG-INVALID
               END-REWRITE
             ELSE
               INITIALIZE       FLG-INVALID
               WRITE   L300T
                       INVALID  KEY
                       MOVE  1  TO FLG-INVALID
               END-WRITE
             END-IF
           END-IF
      *
           IF   WRK-PARA-NYUGAIKBN  =  "3"  OR  SPACE
      *    入院／外来レコード
             INITIALIZE                 L300T
             INITIALIZE                 FLG-INVALID
             MOVE    "9"                TO   L300T-NYUGAIKBN
             MOVE    SYU-SRYKA          TO   L300T-SRYKA
             MOVE    PTNUM-PTNUM        TO   L300T-PTNUM
             READ  L300T-FILE
                  KEY      IS      L300T-KEY
                  INVALID  KEY     MOVE  1  TO FLG-INVALID
             END-READ
      *
             PERFORM  320-MISYU-GET-SUB2-SEC
      *
             IF   FLG-INVALID      =    ZERO
               INITIALIZE       FLG-INVALID
               REWRITE L300T
                     INVALID  KEY
                     MOVE  1  TO FLG-INVALID
               END-REWRITE
             ELSE
               INITIALIZE       FLG-INVALID
               WRITE L300T
                     INVALID  KEY
                     MOVE  1  TO FLG-INVALID
               END-WRITE
             END-IF
           END-IF
      *
           .
       320-MISYU-GET-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金対象者抽出サブ２処理
      *****************************************************************
       320-MISYU-GET-SUB2-SEC      SECTION.
      *
      *    患者カナ氏名
           MOVE PTINF-KANANAME   TO    L300T-KANANAME
      *
      *    患者氏名
           MOVE PTINF-NAME       TO    L300T-NAME
      *
      *    保険者番号
           MOVE PTHKN-HKNJANUM   TO    L300T-HKNJANUM
      *
      *    保険者名
           MOVE HKNJA-HKNJANAME  TO    L300T-HKNJANAME
      *
      *    患者生年月日
           MOVE PTINF-BIRTHDAY   TO    L300T-BIRTHDAY
      *
      *    続柄
           MOVE PTINF-ZOKUGARA   TO    L300T-ZOKU
      *
      *    公費番号
           MOVE PTKOH-KOHNUM     TO    L300T-KOHNUM
      *
      *    頭書登録年月日
           MOVE WRK-F-SRYYMD     TO    L300T-ATAMAYMD
      *
      *    最終来院日
           MOVE WRK-L-SRYYMD     TO    L300T-LASTYMD
      *
      *    請求金額
           ADD  SYU-SKYMONEY     TO    L300T-SKYMONEY
      *
      *    入金合計額
           ADD  SYU-NYUKIN-TOTAL TO    L300T-NYUKIN-TOTAL
      *
           .
       320-MISYU-GET-SUB2-EXT.
           EXIT.
      *
      *****************************************************************
      *    預り金対象者抽出サブ処理
      *****************************************************************
       320-AZUKARI-GET-SUB-SEC      SECTION.
      *
      *    患者マスタ情報抽出
           INITIALIZE                        PTINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTINF-HOSPNUM
           MOVE     SRY-PTID           TO    PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
      *
      *    患者番号情報抽出
           INITIALIZE                        PTNUM-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTNUM-HOSPNUM
           MOVE     SRY-PTID           TO    PTNUM-PTID
           PERFORM  940-PTNUM-READ-SEC
      *
      *    患者保険情報抽出
           INITIALIZE                        PTHKNINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTHKN-HOSPNUM
           MOVE     SRY-PTID           TO    PTHKN-PTID
           PERFORM  950-PTHKNINF-SELECT-SEC
           PERFORM  UNTIL ( PTHKN-HKNJANUM   NOT =  SPACE
                        AND PTHKN-SAKJOKBN   NOT =  "1" )
                       OR   FLG-PTHKNINF         =   1
                    PERFORM  950-PTHKNINF-FETCH-SEC
           END-PERFORM
           PERFORM  950-PTHKNINF-CLOSE-SEC
      *
      *    患者公費情報抽出
           INITIALIZE                        PTKOHINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    PTKOH-HOSPNUM
           MOVE     SRY-PTID           TO    PTKOH-PTID
           PERFORM  950-PTKOHINF-SELECT-SEC
           PERFORM  UNTIL ( PTKOH-KOHNUM     NOT =  SPACE
                        AND PTKOH-SAKJOKBN   NOT =  "1" )
                       OR   FLG-PTKOHINF         =   1
                    PERFORM  950-PTKOHINF-FETCH-SEC
           END-PERFORM
           PERFORM  950-PTKOHINF-CLOSE-SEC
      *
      *    保険者情報抽出
           INITIALIZE                        HKNJAINF-REC
           MOVE     WRK-PARA-HOSPNUM   TO    HKNJA-HOSPNUM
           MOVE     PTHKN-HKNJA-G      TO    HKNJA-HKNJANUM
           PERFORM  960-HKNJA-READ-SEC
      *
      *    頭書登録年月日取得
           PERFORM  321-GET-F-JYURRK-SEC
      *
      *    最終来院日取得
           PERFORM  321-GET-LASTDAY-SEC
      *
      *    入外別レコード
           INITIALIZE                  L300T
           INITIALIZE                  FLG-INVALID
           MOVE    SRY-NYUGAIKBN       TO   L300T-NYUGAIKBN
           MOVE    SRY-SRYKA           TO   L300T-SRYKA
           MOVE    PTNUM-PTNUM         TO   L300T-PTNUM
           READ L300T-FILE
                KEY      IS      L300T-KEY
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      *
           INITIALIZE  WRK-AZU-AREA
      *
           PERFORM  320-AZUKARI-GET-SUB2-SEC
      *
           IF   WRK-PARA-NYUGAIKBN  NOT =  "3"
               AND  ( WRK-PARA-NYUGAIKBN  =  L300T-NYUGAIKBN
               OR  WRK-PARA-NYUGAIKBN  =  SPACE )
             IF   FLG-INVALID      =    ZERO
               INITIALIZE       FLG-INVALID
               REWRITE L300T
                      INVALID  KEY
                      MOVE  1  TO FLG-INVALID
               END-REWRITE
             ELSE
               INITIALIZE       FLG-INVALID
               WRITE   L300T
                       INVALID  KEY
                       MOVE  1  TO FLG-INVALID
               END-WRITE
             END-IF
           END-IF
      *
      *    入院／外来レコード
           INITIALIZE                  L300T
           INITIALIZE                  FLG-INVALID
           MOVE    "9"                 TO   L300T-NYUGAIKBN
           MOVE    SRY-SRYKA           TO   L300T-SRYKA
           MOVE    PTNUM-PTNUM         TO   L300T-PTNUM
           READ   L300T-FILE
                KEY      IS      L300T-KEY
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      *
           INITIALIZE  WRK-AZU-AREA
      *
           PERFORM  320-AZUKARI-GET-SUB2-SEC
      *
           IF   WRK-PARA-NYUGAIKBN  =  "3"  OR  SPACE
             IF   FLG-INVALID  =  ZERO
               INITIALIZE       FLG-INVALID
               REWRITE L300T
                       INVALID  KEY
                       MOVE  1  TO FLG-INVALID
               END-REWRITE
             ELSE
               INITIALIZE       FLG-INVALID
               WRITE L300T
                     INVALID  KEY
                     MOVE  1  TO FLG-INVALID
               END-WRITE
             END-IF
           END-IF
      *
           .
       320-AZUKARI-GET-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    預り金対象者抽出サブ２処理
      *****************************************************************
       320-AZUKARI-GET-SUB2-SEC      SECTION.
      *
      *    患者カナ氏名
           MOVE PTINF-KANANAME   TO    L300T-KANANAME
      *
      *    患者氏名
           MOVE PTINF-NAME       TO    L300T-NAME
      *
      *    保険者番号
           MOVE PTHKN-HKNJANUM   TO    L300T-HKNJANUM
      *
      *    保険者名
           MOVE HKNJA-HKNJANAME  TO    L300T-HKNJANAME
      *
      *    患者生年月日
           MOVE PTINF-BIRTHDAY   TO    L300T-BIRTHDAY
      *
      *    続柄
           MOVE PTINF-ZOKUGARA   TO    L300T-ZOKU
      *
      *    公費番号
           MOVE PTKOH-KOHNUM     TO    L300T-KOHNUM
      *
      *    頭書登録年月日
           MOVE WRK-F-SRYYMD     TO    L300T-ATAMAYMD
      *
      *    最終来院日
           MOVE WRK-L-SRYYMD     TO    L300T-LASTYMD
      *
      *    預り金
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     5
                    OR       WRK-AZU  NOT =  ZERO
             IF  SRY-SRYCD(IDX3)  =  WRK-INI-AZUKARICD
               MOVE  SRY-JIHIMONEY(IDX3)  TO  WRK-AZU
             END-IF
           END-PERFORM
      *
           PERFORM  980-SRYACCT-READ-SEC
           PERFORM  VARYING  AZU-IDX  FROM  1  BY  1
                    UNTIL    AZU-IDX  >    31
             IF  ACCT-DAY(1 AZU-IDX)  NOT =  ZERO
               ADD  ACCT-DAY(1 AZU-IDX)  TO  WRK-AZU-KAISU
             END-IF
           END-PERFORM
      *
           COMPUTE  WRK-AZU-G  =  WRK-AZU  *  WRK-AZU-KAISU
           ADD  WRK-AZU-G        TO    L300T-AZUKARI
      *
           .
       320-AZUKARI-GET-SUB2-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴(頭書登録年月日)チェック処理
      *****************************************************************
       321-GET-F-JYURRK-SEC      SECTION.
      *
           INITIALIZE                 WRK-FN-SRYYMD
      *
      *    外来受診患者読み込み
           INITIALIZE                 JYURRK-REC
           MOVE WRK-PARA-HOSPNUM  TO  JYURRK-HOSPNUM
           MOVE PTINF-PTID        TO  JYURRK-PTID
           MOVE "1"               TO  JYURRK-NYUGAIKBN
           MOVE "00000000"        TO  JYURRK-SRYYMD
           MOVE "99999999"        TO  JYURRK-UPSRYYMD
           PERFORM  970-JYURRK-SELECT-SEC
           PERFORM  970-JYURRK-FETCH-SEC
      *
           PERFORM  UNTIL  FLG-JYURRK  =     1
               IF   JYURRK-HOSPNUM      =     WRK-PARA-HOSPNUM
                    MOVE  JYURRK-SRYYMD  TO  WRK-FN-SRYYMD
                    MOVE  1              TO  FLG-JYURRK
               END-IF
               PERFORM  970-JYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   970-JYURRK-CLOSE-SEC
      *
           INITIALIZE                 WRK-FG-SRYYMD
      *
      *    外来受診患者読み込み
           INITIALIZE                 JYURRK-REC
           MOVE WRK-PARA-HOSPNUM  TO  JYURRK-HOSPNUM
           MOVE PTINF-PTID        TO  JYURRK-PTID
           MOVE "2"               TO  JYURRK-NYUGAIKBN
           MOVE "00000000"        TO  JYURRK-SRYYMD
           MOVE "99999999"        TO  JYURRK-UPSRYYMD
           PERFORM  970-JYURRK-SELECT-SEC
           PERFORM  970-JYURRK-FETCH-SEC
      *
           PERFORM  UNTIL  FLG-JYURRK  =     1
               IF   JYURRK-HOSPNUM      =     WRK-PARA-HOSPNUM
                    MOVE  JYURRK-SRYYMD  TO  WRK-FG-SRYYMD
                    MOVE  1              TO  FLG-JYURRK
               END-IF
               PERFORM  970-JYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   970-JYURRK-CLOSE-SEC
      *
           IF    WRK-FN-SRYYMD      =    SPACE
                 MOVE WRK-FG-SRYYMD TO   WRK-F-SRYYMD
           ELSE
            IF   WRK-FG-SRYYMD      =    SPACE
                 MOVE WRK-FN-SRYYMD TO   WRK-F-SRYYMD
            ELSE
             IF  WRK-FN-SRYYMD      <    WRK-FG-SRYYMD
                 MOVE WRK-FN-SRYYMD TO   WRK-F-SRYYMD
             ELSE
                 MOVE WRK-FG-SRYYMD TO   WRK-F-SRYYMD
             END-IF
            END-IF
           END-IF
      *
           .
       321-GET-F-JYURRK-EXT.
           EXIT.
      *****************************************************************
      *    最終来院日チェック処理
      *****************************************************************
       321-GET-LASTDAY-SEC      SECTION.
      *
           INITIALIZE                     WRK-LN-SRYYMD
      *
      *    外来受診患者読み込み
           INITIALIZE                     PTNYUINRRK-REC
           MOVE WRK-PARA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE PTINF-PTID            TO  PTNYUINRRK-PTID
           PERFORM  980-PTNYUINRRK-READ-SEC
           MOVE PTNYUINRRK-TAIINYMD   TO  WRK-LN-SRYYMD
      *
           INITIALIZE                     WRK-LG-SRYYMD
      *
      *    外来受診患者読み込み
           INITIALIZE                     JYURRK-REC
           MOVE WRK-PARA-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE PTINF-PTID            TO  JYURRK-PTID
           MOVE "2"                   TO  JYURRK-NYUGAIKBN
           PERFORM  980-JYURRK-READ-SEC
           MOVE JYURRK-SRYYMD         TO  WRK-LG-SRYYMD
      *
           IF    WRK-LN-SRYYMD      =    SPACE
                 MOVE WRK-LG-SRYYMD TO   WRK-L-SRYYMD
           ELSE
            IF   WRK-LG-SRYYMD      =    SPACE
                 MOVE WRK-LN-SRYYMD TO   WRK-L-SRYYMD
            ELSE
             IF  WRK-LN-SRYYMD      >    WRK-LG-SRYYMD
                 MOVE WRK-LN-SRYYMD TO   WRK-L-SRYYMD
             ELSE
                 MOVE WRK-LG-SRYYMD TO   WRK-L-SRYYMD
             END-IF
            END-IF
           END-IF
      *
           .
       321-GET-LASTDAY-EXT.
           EXIT.
      *****************************************************************
      *    印刷制御処理
      *****************************************************************
       325-CTRL-PRT-SEC                SECTION.
      *
           INITIALIZE      FLG-ATEND
           INITIALIZE      L300
      *
      *    抽出患者情報読み込み
           INITIALIZE                 L300T
           START   L300T-FILE
                   KEY IS          >= L300T-KEY
           END-START
      *
           PERFORM 910-L300T-READ-SEC
           MOVE    L300T-NYUGAIKBN    TO  WRK-PR-NYUGAIKBN
           MOVE    L300T-SRYKA        TO  WRK-PR-SRYKA
           MOVE    1                  TO  IDX2
      *
      *    CSV ヘッダー処理
           MOVE    1                   TO  IDXP
           PERFORM  800-CSV-HEAD-SEC
      *
           PERFORM UNTIL    FLG-ATEND =   1
               IF  FLG-ATEND NOT = 1
                   PERFORM  325-DTL-HEN-SEC
               END-IF
               PERFORM 910-L300T-READ-SEC
           END-PERFORM
      *
           PERFORM  330-TERM-HEN-SEC
      *
           .
       325-CTRL-PRT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為回数明細転送
      *****************************************************************
       325-DTL-HEN-SEC                SECTION.
      *
      *    入外区分のブレイクチェック
           IF    L300T-NYUGAIKBN   NOT =   WRK-PR-NYUGAIKBN
                 PERFORM  330-TERM-HEN-SEC
      *
      *          CSV ヘッダー処理
                 INITIALIZE  WRK-ITEM
                 PERFORM  810-CSVDATA-EDIT-SEC
                 PERFORM  800-CSV-HEAD-SEC
      *
                 MOVE     1            TO  IDX2
                 INITIALIZE                L300
                 MOVE  L300T-NYUGAIKBN TO  WRK-PR-NYUGAIKBN
                 MOVE  L300T-SRYKA     TO  WRK-PR-SRYKA
           END-IF
      *
           IF    L300T-SRYKA       NOT =   WRK-PR-SRYKA
                 PERFORM  330-TERM-HEN-SEC
      *
      *          CSV ヘッダー処理
                 INITIALIZE  WRK-ITEM
                 PERFORM  810-CSVDATA-EDIT-SEC
                 PERFORM  800-CSV-HEAD-SEC
      *
                 MOVE     1            TO  IDX2
                 INITIALIZE                L300
                 MOVE  L300T-SRYKA     TO  WRK-PR-SRYKA
           END-IF
      *
      *    明細数オーバーのチェック
           IF    IDX2     >          20
                 PERFORM  330-TERM-HEN-SEC
                 MOVE     1          TO  IDX2
                 INITIALIZE              L300
           END-IF
      *
      *    患者番号
           MOVE  L300T-PTNUM         TO  L300-PTNUM (IDX2)
      *    患者カナ氏名
           MOVE  L300T-KANANAME      TO  L300-KANANAME (IDX2)
      *    患者氏名
           MOVE  L300T-NAME          TO  L300-NAME (IDX2)
      *
      *    保険者番号
           MOVE  L300T-HKNJANUM      TO  L300-HKNJANUM (IDX2)
      *
      *    保険者名FLG-HKNJAINF
           MOVE  L300T-HKNJANAME     TO  L300-HKNJANAME (IDX2)
      *
      *    生年月日
           MOVE  L300T-BIRTHDAY(1:4) TO  L300-BIRTHDAY (IDX2)(1:4)
           MOVE  "/"                 TO  L300-BIRTHDAY (IDX2)(5:1)
           MOVE  L300T-BIRTHDAY(5:2) TO  L300-BIRTHDAY (IDX2)(6:2)
           MOVE  "/"                 TO  L300-BIRTHDAY (IDX2)(8:1)
           MOVE  L300T-BIRTHDAY(7:2) TO  L300-BIRTHDAY (IDX2)(9:2)
      *    年齢
           MOVE  LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
           MOVE  L300T-BIRTHDAY      TO  WRK-BIRTHDAY
           COMPUTE WRK-NENREI        =   WRK-SYSYY
                                     -   WRK-BIRTHY
           IF    WRK-SYSMM           <   WRK-BIRTHM
                 COMPUTE WRK-NENREI  =   WRK-NENREI   -   1
           END-IF
           MOVE    WRK-NENREI        TO  WRK-Z3
           MOVE    WRK-Z3            TO  L300-AGE (IDX2)
      *    続柄
           MOVE    L300T-ZOKU        TO  L300-ZOKU (IDX2)
      *    公費番号
           MOVE    L300T-KOHNUM      TO  L300-KOHNUM (IDX2)
      *    頭書登録年月日
           IF  L300T-ATAMAYMD     NOT =  SPACE
               MOVE  L300T-ATAMAYMD(1:4) TO  L300-ATAMAYMD (IDX2)(1:4)
               MOVE  "/"                 TO  L300-ATAMAYMD (IDX2)(5:1)
               MOVE  L300T-ATAMAYMD(5:2) TO  L300-ATAMAYMD (IDX2)(6:2)
               MOVE  "/"                 TO  L300-ATAMAYMD (IDX2)(8:1)
               MOVE  L300T-ATAMAYMD(7:2) TO  L300-ATAMAYMD (IDX2)(9:2)
           END-IF
      *    未収金
           COMPUTE  WRK-MISYU        =
                    L300T-SKYMONEY   -   L300T-NYUKIN-TOTAL
           MOVE  WRK-MISYU           TO  WRK--9-9
           MOVE  WRK--9-9            TO  L300-MISYU (IDX2)
      *    預り金
           MOVE  L300T-AZUKARI       TO  WRK--9-9
           MOVE  WRK--9-9            TO  L300-AZUKARI (IDX2)
      *    最終来院日
           IF  L300T-LASTYMD      NOT =  SPACE
            IF L300T-LASTYMD          =  "99999999"
               MOVE  "  入院中"          TO  L300-LASTYMD (IDX2)
            ELSE
               MOVE  L300T-LASTYMD(1:4)  TO  L300-LASTYMD (IDX2)(1:4)
               MOVE  "/"                 TO  L300-LASTYMD (IDX2)(5:1)
               MOVE  L300T-LASTYMD(5:2)  TO  L300-LASTYMD (IDX2)(6:2)
               MOVE  "/"                 TO  L300-LASTYMD (IDX2)(8:1)
               MOVE  L300T-LASTYMD(7:2)  TO  L300-LASTYMD (IDX2)(9:2)
            END-IF
           END-IF
      *    連番
           ADD   1                   TO  WRK-PRDTLCNT
           MOVE  WRK-PRDTLCNT        TO  WRK-Z5
           MOVE  WRK-Z5              TO  L300-NO (IDX2)
      *
           PERFORM  400-CSV-DATA-SEC
      *
      *    印字明細数カウントアップ
           ADD   1                   TO  IDX2
      *
           .
       325-DTL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科グループ判別処理
      *****************************************************************
       320-SRYKA-GROUPING-SEC      SECTION.
      *
           MOVE  1               TO  IDX1
           MOVE  WRK-SRYKAMAX    TO  IDX2
           PERFORM  UNTIL  IDX1   >  IDX2
             IF  WRK-SRYKA(IDX1)  =  WRK-CHK-SRYKA
               MOVE  ZERO            TO  IDX2
             ELSE
               ADD   1               TO  IDX1
             END-IF
           END-PERFORM
      *
           .
       320-SRYKA-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           MOVE    WRK-PARA-SRYYMWH        TO  L300-SRYYM
           MOVE    WRK-SYSYMDWH            TO  L300-PRTYMD
      *
      *    ページ数
           ADD     1                       TO  CNT-PAGE
           MOVE    CNT-PAGE                TO  WRK-PAGE
           MOVE    WRK-PAGE                TO  L300-PAGE
      *
      *    メッセージ
           IF   INP-STS  NOT =  ZERO
             EVALUATE  INP-STS
               WHEN  1
                 MOVE    "入力された診療科コードに誤りがあります"
                                    TO   L300-JOBMSG
               WHEN  2
                 MOVE    "入力された入外区分に誤りがあります"
                                    TO   L300-JOBMSG
             END-EVALUATE
           ELSE
             IF  WRK-PRDTLCNT =      ZERO
               MOVE "＊＊＊  対象データが存在しません  ＊＊＊"
                                     TO   L300-JOBMSG
             END-IF
           END-IF
      *
      *    入外区分
           EVALUATE  WRK-PR-NYUGAIKBN
             WHEN  "1"
               MOVE  "入院"        TO  L300-NYUGAIKBN
             WHEN  "2"
               MOVE  "外来"        TO  L300-NYUGAIKBN
             WHEN  "9"
               MOVE  "入院／外来"  TO  L300-NYUGAIKBN
           END-EVALUATE
      *
      *    診療科情報
           MOVE   WRK-PR-SRYKA          TO  WRK-CHK-SRYKA
           PERFORM   320-SRYKA-GROUPING-SEC
           IF  IDX2  =  ZERO
             MOVE   WRK-SRYKANAME(IDX1) TO  L300-SRYKA
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "A00000L300.red"         TO  SPRT-PRTID
           MOVE    "未収金・預り金個人別月計表"
                                       TO  SPRT-TITLE
           MOVE    L300                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               CONTINUE
           END-IF
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           INITIALIZE  WRK-ITEM
           STRING  "対象年月："          DELIMITED  BY  SIZE
                   WRK-PARA-SRYYMWH      DELIMITED  BY  SPACE
                                         INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分
           INITIALIZE  WRK-ITEM
           EVALUATE  L300T-NYUGAIKBN
             WHEN  "1"
               MOVE    "入外区分：入院"        TO  WRK-ITEM
             WHEN  "2"
               MOVE    "入外区分：外来"        TO  WRK-ITEM
             WHEN  "9"
               MOVE    "入外区分：入院／外来"  TO  WRK-ITEM
           END-EVALUATE
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科
           MOVE   L300T-SRYKA              TO  WRK-CHK-SRYKA
           PERFORM   320-SRYKA-GROUPING-SEC
           INITIALIZE  WRK-ITEM
           IF  IDX2  =  ZERO
             STRING  "診療科："            DELIMITED  BY  SIZE
                     WRK-SRYKANAME(IDX1)   DELIMITED  BY  SPACE
                                           INTO  WRK-ITEM
             END-STRING
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
           MOVE    1                  TO  IDX2
      *
           INITIALIZE  WRK-ITEM
           STRING  WRK-SYSYMDWH        DELIMITED  BY  SPACE
                   "  発行"            DELIMITED  BY  SIZE
                                       INTO  WRK-ITEM
           END-STRING
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称・明細情報セット
           PERFORM  VARYING  IDX8  FROM  1  BY  1
                    UNTIL    IDX8  >  CSV-HEAD-MAX
      *
             INITIALIZE  WRK-ITEM
             MOVE    CSV-HEAD-ITEM(IDX8)     TO  WRK-ITEM
      *
             IF  IDX8  =  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
           END-PERFORM
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV明細編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    連番
           INITIALIZE  WRK-ITEM
           MOVE    L300-NO(IDX2)             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           INITIALIZE  WRK-ITEM
           MOVE    L300-PTNUM(IDX2)          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者カナ氏名
           INITIALIZE  WRK-ITEM
           MOVE    L300-KANANAME(IDX2)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者氏名
           INITIALIZE  WRK-ITEM
           MOVE    L300-NAME(IDX2)           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者番号
           INITIALIZE  WRK-ITEM
           MOVE    L300-HKNJANUM(IDX2)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者名称
           INITIALIZE  WRK-ITEM
           MOVE    L300-HKNJANAME(IDX2)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    生年月日
           INITIALIZE  WRK-ITEM
           MOVE    L300-BIRTHDAY(IDX2)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    年齢
           INITIALIZE  WRK-ITEM
           MOVE    L300-AGE(IDX2)            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    続柄
           INITIALIZE  WRK-ITEM
           MOVE    L300-ZOKU(IDX2)           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    公費
           INITIALIZE  WRK-ITEM
           MOVE    L300-KOHNUM(IDX2)         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    頭書登録年月日
           INITIALIZE  WRK-ITEM
           MOVE    L300-ATAMAYMD(IDX2)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    未収金
           INITIALIZE  WRK-ITEM
           MOVE    WRK-MISYU                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    預り金
           INITIALIZE  WRK-ITEM
           MOVE    L300T-AZUKARI             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    最終来院日
           INITIALIZE  WRK-ITEM
           MOVE    L300-LASTYMD(IDX2)        TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
      *
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX10  FROM  1  BY  1
                    UNTIL  IDX10  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX10:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX10:1)  NOT =  SPACE
                 MOVE    IDX10      TO  IDX-ST
                 MOVE    1          TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID      TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM        TO  STOUKEICSV-SRYYM
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "misyu_azukari_"          DELIMITED  BY  SIZE
                   WRK-PARA-SRYYM            DELIMITED  BY  SPACE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "未収金・預り金個人別月計表"
                                             TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"           USING
                                             ORCSTOUKEICSVAREA
                                             SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                        WRK-CSVDATA
           MOVE    1                         TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"            TO      LNK-DAY7-IRAI
           MOVE    WRK-SYMD        TO      LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI    TO      WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC                  SECTION.
      *
           INITIALIZE                    SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO  SYU-HOSPNUM
           IF  WRK-PARA-NYUGAIKBN  =  SPACE  OR  "3"
             MOVE    "%"                 TO  SYU-NYUGAIKBN
           ELSE
             MOVE    WRK-PARA-NYUGAIKBN  TO  SYU-NYUGAIKBN
           END-IF
           MOVE    WRK-PARA-SRYLYMD      TO  SYU-SRYYMD
           IF  FLG-SRYKA-ALL  =  1
             MOVE    "%"                 TO  SYU-SRYKA
           ELSE
             MOVE    WRK-PARA-SRYKA      TO  SYU-SRYKA
           END-IF
      *
           MOVE    SYUNOU-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key81"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM 910-DBFETCH-SEC
           ELSE
             MOVE    1                   TO  FLG-SYUNOU
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                   SECTION.
      *
           MOVE    "DBFETCH"             TO  MCP-FUNC
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key81"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    ZERO                TO  FLG-SYUNOU
             MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
             MOVE    1                   TO  FLG-SYUNOU
           END-IF
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                   SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key81"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       911-DBSELECT-SEC                  SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    SPA-HOSPNUM           TO  SRY-HOSPNUM
           IF  WRK-PARA-NYUGAIKBN  =  SPACE  OR  "3"
             MOVE    "%"                 TO  SRY-NYUGAIKBN
           ELSE
             MOVE    WRK-PARA-NYUGAIKBN  TO  SRY-NYUGAIKBN
           END-IF
           MOVE    WRK-PARA-SRYYM        TO  SRY-SRYYM
           IF  FLG-SRYKA-ALL  =  1
             MOVE    "%"                 TO  SRY-SRYKA
           ELSE
             MOVE    WRK-PARA-SRYKA      TO  SRY-SRYKA
           END-IF
           MOVE    WRK-INI-AZUKARICD     TO  SRY-SRYCD(1)
                                             SRY-SRYCD(2)
                                             SRY-SRYCD(3)
                                             SRY-SRYCD(4)
                                             SRY-SRYCD(5)
      *
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key18"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM 911-DBFETCH-SEC
           ELSE
             MOVE    1                   TO  FLG-SRYACT
           END-IF
           .
       911-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       911-DBFETCH-SEC                   SECTION.
      *
           MOVE    "DBFETCH"             TO  MCP-FUNC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key18"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    ZERO                TO  FLG-SRYACT
             MOVE    MCPDATA-REC         TO  SRYACT-REC
           ELSE
             MOVE    1                   TO  FLG-SRYACT
           END-IF
           .
       911-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       911-DBCLOSE-SEC                   SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key18"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       911-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金／預り金対象者抽出ファイルREAD
      *****************************************************************
       910-L300T-READ-SEC               SECTION.
      *
           READ L300T-FILE
                NEXT AT  END  MOVE  1  TO  FLG-ATEND
           END-READ
      *
           .
       910-L300T-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
          IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                 SYUNOU-REC
           MOVE    SMEI-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID      TO  SYU-PTID
           MOVE    SMEI-DENPNUM   TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO                    TO  FLG-SYUNOU
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号情報読み込み
      *****************************************************************
       940-PTNUM-READ-SEC         SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       940-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       950-PTHKNINF-SELECT-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
                   MOVE    ZERO        TO  FLG-PTHKNINF
           ELSE
                   MOVE    1           TO  FLG-PTHKNINF
           END-IF
      *
           .
       950-PTHKNINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報 FETCH
      *****************************************************************
       950-PTHKNINF-FETCH-SEC         SECTION.
      *
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"       TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
      *
           .
       950-PTHKNINF-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報 CLOSE
      *****************************************************************
       950-PTHKNINF-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       950-PTHKNINF-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       950-PTKOHINF-SELECT-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
                   MOVE    ZERO        TO  FLG-PTKOHINF
           ELSE
                   MOVE    1           TO  FLG-PTKOHINF
           END-IF
      *
           .
       950-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報 FETCH
      *****************************************************************
       950-PTKOHINF-FETCH-SEC         SECTION.
      *
               MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
      *
           .
       950-PTKOHINF-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報 CLOSE
      *****************************************************************
       950-PTKOHINF-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       950-PTKOHINF-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読み込み
      *****************************************************************
       960-HKNJA-READ-SEC         SECTION.
      *
      *    保険者番号　検索チェック
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  HKNJAINF-REC
                   MOVE    ZERO         TO  FLG-HKNJAINF
               ELSE
                   INITIALIZE               HKNJAINF-REC
                   MOVE    1            TO  FLG-HKNJAINF
               END-IF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       960-HKNJA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴情報検索
      *****************************************************************
       970-JYURRK-SELECT-SEC               SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key13"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-JYURRK
           ELSE
               MOVE    1           TO  FLG-JYURRK
           END-IF
           .
       970-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴情報FETCH
      *****************************************************************
       970-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key13"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-JYURRK
               MOVE    MCPDATA-REC  TO  JYURRK-REC
           ELSE
               MOVE    1            TO  FLG-JYURRK
               INITIALIZE               JYURRK-REC
           END-IF
           .
       970-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴情報 ＤＢ　クローズ処理
      *****************************************************************
       970-JYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key13"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       970-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       980-SRYACCT-READ-SEC         SECTION.
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"         TO  MCP-FUNC
               MOVE    "tbl_sryacct"     TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC       TO  SRYACCT-REC
                   MOVE    ZERO              TO  FLG-SRYACCT
               ELSE
                   MOVE    1                 TO  FLG-SRYACCT
                   INITIALIZE                SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       980-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報読み込み
      *****************************************************************
       980-JYURRK-READ-SEC         SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  JYURRK-REC
                   MOVE    ZERO            TO  FLG-JYURRK
               ELSE
                   MOVE    1               TO  FLG-JYURRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       980-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴情報読み込み
      *****************************************************************
       980-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
              MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
              MOVE    "key3"               TO  MCP-PATHNAME
              CALL    "ORCDBMAIN"          USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       980-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
