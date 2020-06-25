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
       PROGRAM-ID.             A00000C112.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : (医師別)外来患者数集計表
      *  管理者            :
      *  作成日付    作業者       記述
      *  04/02/05    @@@@-有村    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     初診、再診、その他の判定条件を変更
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/10/18  システム管理のDB検索処理を変更(key→key10)
      *  01.00.03    @@@@-田中    12/01/18  CSVファイル作成機能追加
      *  01.00.04    @@@@-門間    12/07/18  初診、再診、その他の判定条件を再変更
      *  01.00.05    @@@@-門間    12/11/12  処理が終了しない不具合の修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    門間         16/02/01  一時ディレクトリ対応(エラーファイル)
      *  02.00.03    門間         16/07/20  CSVファイル作成機能修正
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000C100.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKA-ALL           PIC 9(01).
           03  FLG-GET                 PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYSKANRI2           PIC 9(01).
           03  FLG-SYSKANRI3           PIC 9(01).
           03  FLG-INPUT               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-JYURRK2             PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  INP-STS                 PIC 9(04).
           03  FLG-11DUMMY             PIC 9(01).
           03  FLG-12DUMMY             PIC 9(01).
           03  FLG-ACCTEND             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(06).
           03  CNT-DAY                 PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
             05  SYS-YY                PIC 9(02).
             05  SYS-MM                PIC 9(02).
             05  SYS-DD                PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  SELKA-IDX               PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDX4                    PIC 9(04).
           03  IDX5                    PIC 9(04).
           03  IDX6                    PIC 9(04).
           03  IDX7G.
             05  IDX7                  PIC 9(02).
           03  IDX8G.
             05  IDX8                  PIC 9(02).
           03  DIDXG.
             05  DIDX                  PIC 9(02).
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-S                   PIC 9(07).
           03  IDX-N                   PIC 9(07).
           03  IDX-MAX                 PIC 9(07).
           03  JIHIIDX                 PIC 9(04).
           03  NG                      PIC 9(04).
           03  IDX10                   PIC 9(02).
           03  IDX11                   PIC 9(02).
           03  IDX-LINE                PIC 9(05).
           03  IDX-DR                  PIC 9(04).
           03  IDX-ST                  PIC 9(03).
           03  IDX-ED                  PIC 9(03).
           03  IDX-LEN                 PIC 9(03).
           03  IDXP                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                PIC X(64).
           03  WRK-RECEERR             PIC X(200).
           03  WRK-SYSYMDWH            PIC X(22).
           03  WRK-YMD1WH              PIC X(22).
           03  WRK-YMD2WH              PIC X(22).
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTNUM6              PIC 9(06).
           03  WRK-SYUHKNNUM           PIC X(03).
           03  WRK-SYSYMD.
             05  WRK-SYSYY             PIC 9(04).
             05  WRK-SYSMM             PIC 9(02).
             05  WRK-SYSDD             PIC 9(02).
           03  WRK-SYMD.
             05  WRK-SYY               PIC 9(04).
             05  WRK-SMM               PIC 9(02).
             05  WRK-SDD               PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-MONEY               PIC 9(08).
           03  WRK-JIHI                PIC 9(08).
           03  WRK-SYUEKI              PIC 9(08).
           03  WRK-JIHI2               PIC 9(08).
           03  WRK-TJIHI-G             OCCURS 5.
             05  WRK-TJIHI             PIC X(08).
             05  WRK-TJIHI-FLG         PIC 9(01).
           03  WRK-TJIHIN-G            OCCURS 5.
             05  WRK-TJIHIN            PIC X(08).
             05  WRK-TJIHIN-FLG        PIC 9(01).
           03  WRK-4001-TENTANKA       PIC 9(02)V9(01).
           03  WRK-RENNUM-X            PIC ZZ9.
           03  WRK-PAGE                PIC ZZ9.
           03  WRK-S9-3                PIC --9.
           03  WRK-Z9-5                PIC ZZ,ZZ9.
           03  WRK-Z9-5-2              PIC ZZZZ9.
           03  WRK-Z9                  PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                  PIC ZZZZZZZZZZZZ.
           03  WRK-S9                  PIC -----------9.
           03  WRK-SZ                  PIC -----------Z.
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-SRYYM               PIC X(06).
           03  DEBUG-CNT               PIC 9(03).
           03  WRK-PR-SRYYM            PIC X(06).
      *
           03  WRK-TMP-YMD.
             05  WRK-TMP-YY            PIC 9(04).
             05  WRK-TMP-MM            PIC 9(02).
             05  WRK-TMP-DD            PIC 9(02).
           03  WRK-TMP-LYMD.
             05  WRK-TMP-LYY           PIC 9(04).
             05  WRK-TMP-LMM           PIC 9(02).
             05  WRK-TMP-LDD           PIC 9(02).
           03  WRK-DENPNUM             PIC 9(07).
      *
       01  SUM-AREA                    VALUE  ZERO.
           03  SUM-DRCDMAX             PIC 9(04).
           03  SUM-SRYYMDIMAX          PIC 9(05).
           03  SUM-SRYYMD-TBL          OCCURS 1000.
             05  SUM-SRYYMD            PIC  X(08).
           03  SUM-SRYKA-TBL           OCCURS 1000.
             05  SUM-DRCD              PIC X(05).
             05  SUM-DRNAME            PIC X(20).
             05  SUM-SRYKA-ZAN         PIC S9(05).
             05  SUM-SYOSHIN-G.
               07  SUM-SYOSHIN-TBL     OCCURS 1000.
                 09  SUM-SYOSHIN       PIC 9(05).
             05  SUM-SAISHIN-G.
               07  SUM-SAISHIN-TBL     OCCURS 1000.
                 09  SUM-SAISHIN       PIC 9(05).
             05  SUM-ETC-G.
               07  SUM-ETC-TBL         OCCURS 1000.
                 09  SUM-ETC           PIC 9(05).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)  VALUE "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)  VALUE "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)  VALUE "973".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    CSV領域
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE          PIC 9(03)  VALUE 100.
           03  WRK-ITEM               PIC X(100)  VALUE SPACE.
           03  WRK-CSVDATA            PIC X(20000) VALUE SPACE.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    自費管理情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災情報取得
           COPY    "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
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
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為情報
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    初再診取得サブ
           COPY    "CPORCSS003.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000C100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000C100-LNK.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           IF  INP-STS  =  ZERO
      *    正常主処理
             PERFORM  200-MAIN-SEC
           ELSE
      *    エラー処理
             PERFORM  210-ERR-SEC
           END-IF
      *
           PERFORM  300-END-SEC
      *
           STOP  RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                 FLG-AREA
           INITIALIZE                 STS-AREA
           INITIALIZE                 WRK-AREA
           INITIALIZE                 CNT-AREA
           INITIALIZE                 SUM-AREA
           INITIALIZE                 SPA-AREA
      *
           PERFORM  100-DBOPEN-SEC
           MOVE    C100LNK-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO  SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
           MOVE    C100LNK-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    C100LNK-JOBID      TO  JOB-JOBID
           MOVE    C100LNK-SHELLID    TO  JOB-SHELLID
           MOVE    "A00000C112"       TO  JOB-PGID
           MOVE    "(医師別)外来患者数集計表"
                                      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
      *    初診／再診判定サブ初期処理
           INITIALIZE                      SS003-AREA
           MOVE    ZERO            TO      SS003-SYORIKBN
           CALL    "ORCSS003"      USING   SS003-AREA
                                           SPA-AREA
      *
           INITIALIZE                     SGETTEMP-AREA
           MOVE    RECEERR                TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"          USING  SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)  TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付取得／和暦変換
           MOVE    C100LNK-PRTKANRI-SKYYMD   TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG               TO  WRK-SYSYMDWH
      *
      *    集計対象期間開始日取得／和暦変換
           MOVE    C100LNK-YMD1        TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YMD1WH
      *
      *    集計対象期間開始日取得／和暦変換
           MOVE    C100LNK-YMD2        TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YMD2WH
      *
      *    画面入力値のチェック
      *    集計区分のチェック
           IF   C100LNK-MDKBN  NOT =  SPACE
               AND   C100LNK-MDKBN  NOT =  "0"
             MOVE    10         TO  INP-STS
             GO                 TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    集計対象か期間のチェック
           IF   C100LNK-MDKBN  =  SPACE
             IF  C100LNK-YMD1  >  C100LNK-YMD2
               MOVE    20      TO  INP-STS
               GO              TO  110-PARA-HENSYU-EXT
             END-IF
           ELSE
             IF   C100LNK-YMD1(1:6)  >  C100LNK-YMD2(1:6)
               MOVE    20      TO  INP-STS
               GO              TO  110-PARA-HENSYU-EXT
             END-IF
           END-IF
      *
           IF   C100LNK-YMD1(1:4)  NOT =  C100LNK-YMD2(1:4)
             MOVE    21        TO  INP-STS
             GO                TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    入外区分のチェック
           IF   C100LNK-NYUGAIKBN  NOT =  "1"
               AND  C100LNK-NYUGAIKBN  NOT =  "2"
             MOVE    30        TO  INP-STS
             GO                TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    C100LNK-YMD2          TO  SYS-1001-STYUKYMD
           MOVE    C100LNK-YMD1          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  C100LNK-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    職員情報編集
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SYS-1001-KBNCD      TO  SYS-1010-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    C100LNK-YMD2        TO  SYS-1010-STYUKYMD
           MOVE    C100LNK-YMD1        TO  SYS-1010-EDYUKYMD
           PERFORM  820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
      *    医者はKBNCDの頭１桁が"１"
             IF   FLG-SYSKANRI3 = ZERO
                 AND  SYS-1010-KBNCD(1:1)  =  "1"
               MOVE    SYS-1010-KBNCD       TO  SUM-DRCD(IDX1)
               IF   SYS-1010-NAME NOT  = SPACE
                 MOVE    SYS-1010-NAME      TO  SUM-DRNAME(IDX1)
               ELSE
                 MOVE    SYS-1010-KANANAME  TO  SUM-DRNAME(IDX1)
               END-IF
               ADD  1         TO  IDX1
             END-IF
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
           MOVE    "9999"            TO  SUM-DRCD(IDX1)
           MOVE    "全医師合計"      TO  SUM-DRNAME(IDX1)
           MOVE    IDX1              TO  SUM-DRCDMAX
      *
           IF  C100LNK-MDKBN  =  SPACE
      *    印刷年月日取得
             MOVE    1               TO  IDX1
             MOVE    C100LNK-YMD1    TO  WRK-TMP-YMD
             INITIALIZE  WRK-SYMD
             MOVE    WRK-TMP-YMD     TO  WRK-SYMD(1:6)
             PERFORM  31012-LASTDAY-GET-SEC
             MOVE    WRK-SYMD        TO   WRK-TMP-LYMD
      *
             PERFORM  UNTIL  WRK-TMP-YMD  >  C100LNK-YMD2
                      OR  IDX1  >  1000
              IF   WRK-TMP-YMD  >  WRK-TMP-LYMD
                IF   WRK-TMP-MM  >=  12
                  ADD  1             TO  WRK-TMP-YY
                  MOVE    1          TO  WRK-TMP-MM
                  MOVE    1          TO  WRK-TMP-DD
                ELSE
                  ADD  1             TO  WRK-TMP-MM
                  MOVE    1          TO  WRK-TMP-DD
                END-IF
                INITIALIZE  WRK-SYMD
                MOVE    WRK-TMP-YMD  TO  WRK-SYMD(1:6)
                PERFORM 31012-LASTDAY-GET-SEC
                MOVE    WRK-SYMD     TO  WRK-TMP-LYMD
              END-IF
              MOVE    WRK-TMP-YMD    TO  SUM-SRYYMD(IDX1)
              ADD  1                 TO  WRK-TMP-DD
              ADD  1                 TO  IDX1
             END-PERFORM
             MOVE    "計"            TO  SUM-SRYYMD(IDX1)
             MOVE    IDX1            TO  SUM-SRYYMDIMAX
           ELSE
      *    印刷年月取得
             MOVE    1               TO  IDX1
             MOVE    C100LNK-YMD1(1:6)    TO  WRK-TMP-YMD(1:6)
             MOVE    "01"            TO  WRK-TMP-YMD(7:2)
      *
             PERFORM  UNTIL  WRK-TMP-YMD  >  C100LNK-YMD2
                      OR  IDX1  >  1000
               IF   WRK-TMP-YMD  >  WRK-TMP-LYMD
                 IF   WRK-TMP-MM  >  12
                   ADD   1           TO  WRK-TMP-YY
                   MOVE    1         TO  WRK-TMP-MM
                 END-IF
               END-IF
               MOVE    WRK-TMP-YMD(1:6)   TO  SUM-SRYYMD(IDX1)
               ADD  1                TO  WRK-TMP-MM
               ADD  1                TO  IDX1
             END-PERFORM
             MOVE     "計"           TO  SUM-SRYYMD(IDX1)
             MOVE     IDX1           TO  SUM-SRYYMDIMAX
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF  C100LNK-MDKBN  =  SPACE
      *    (日別)初診数、再診数、その他集計処理
             PERFORM 320-JYURRK-CNT-SEC
      *
      *    (日別)初診数、再診数、その他印刷処理
             PERFORM  330-TERM-HEN-SEC
           ELSE
      *    (月別)初診数、再診数、その他集計処理
             PERFORM  320-JYURRK-M-CNT-SEC
      *
      *    (月別)初診数、再診数、その他印刷処理
             PERFORM  330-TERM-M-HEN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値阿エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE             C100
      *
           MOVE    WRK-SYSYMDWH   TO  C100-PRTYMD
      *
           ADD  1                 TO  CNT-PAGE
           MOVE    CNT-PAGE       TO  WRK-PAGE
           MOVE    WRK-PAGE       TO  C100-PAGE
      *
           EVALUATE INP-STS
           WHEN 10
             MOVE    "入力された集計区分の値に誤りがあります"
                                  TO  C100-JOBMSG
           WHEN 20
             MOVE    "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO  C100-JOBMSG
           WHEN 21
             MOVE
                 "入力する集計対象期間の範囲は同年内で指定をして下さい"
                                  TO  C100-JOBMSG
           WHEN 30
             MOVE    "入力された入外区分の値に誤りがあります"
                                  TO  C100-JOBMSG
           END-EVALUATE
      *
           PERFORM  340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    (日別)初診数、再診数、その他集計処理
      *****************************************************************
       320-JYURRK-CNT-SEC      SECTION.
      *
           MOVE    SUM-DRCDMAX        TO  IDX3
           MOVE    SUM-SRYYMDIMAX     TO  IDX5
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    C100LNK-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    C100LNK-YMD1        TO  SYU-SRYYMD
           MOVE    C100LNK-YMD2        TO  SYU-UPSRYYMD
      *
           PERFORM  900-DBSELECT-SEC
           PERFORM  UNTIL  FLG-SYUNOU  =  1
      *
      *      初診／再診判定サブ
             INITIALIZE                          SS003-AREA
             MOVE    "1"                 TO      SS003-SYORIKBN
             MOVE    SYU-HOSPNUM         TO      SS003-HOSPNUM
             MOVE    SYU-NYUGAIKBN       TO      SS003-NYUGAIKBN
             MOVE    SYU-PTID            TO      SS003-PTID
             MOVE    SYU-DENPNUM         TO      SS003-DENPNUM
             CALL    "ORCSS003"          USING   SS003-AREA
                                                 SPA-AREA
      *
             MOVE    "1"                 TO  WRK-DRCD(1:1)
             MOVE    SYU-DRCD            TO  WRK-DRCD(2:4)
             PERFORM  320-DRCD-GET-IDX-SEC
             MOVE    SYU-SRYYMD          TO  WRK-SRYYMD
             PERFORM  320-SRYYMD-GET-IDX-SEC
      *
             EVALUATE  SS003-SYOSAISIN-KBN
      *      初診件数カウント
             WHEN    "1"
               ADD    1                    TO  SUM-SYOSHIN(IDX1 IDX6)
               ADD    1                    TO  SUM-SYOSHIN(IDX1 IDX5)
               ADD    1                    TO  SUM-SYOSHIN(IDX3 IDX6)
               ADD    1                    TO  SUM-SYOSHIN(IDX3 IDX5)
      *      再診件数カウント
             WHEN    "2"
               ADD    1                    TO  SUM-SAISHIN(IDX1 IDX6)
               ADD    1                    TO  SUM-SAISHIN(IDX1 IDX5)
               ADD    1                    TO  SUM-SAISHIN(IDX3 IDX6)
               ADD    1                    TO  SUM-SAISHIN(IDX3 IDX5)
      *      その他件数カウント
             WHEN    OTHER
               ADD    1                    TO  SUM-ETC(IDX1 IDX6)
               ADD    1                    TO  SUM-ETC(IDX1 IDX5)
               ADD    1                    TO  SUM-ETC(IDX3 IDX6)
               ADD    1                    TO  SUM-ETC(IDX3 IDX5)
             END-EVALUATE
      *
             PERFORM 900-DBFETCH-SEC
           END-PERFORM
           PERFORM 900-DBCLOSE-SEC
      *
           .
       320-JYURRK-CNT-EXT.
           EXIT.
      *****************************************************************
      *    (月別)初診数、再診数、その他集計処理
      *****************************************************************
       320-JYURRK-M-CNT-SEC      SECTION.
      *
           MOVE    SUM-DRCDMAX        TO  IDX3
           MOVE    SUM-SRYYMDIMAX     TO  IDX5
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    C100LNK-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    C100LNK-YMD1(1:6)   TO  SYU-SRYYMD(1:6)
           MOVE    "01"                TO  SYU-SRYYMD(7:2)
           MOVE    C100LNK-YMD2(1:6)   TO  SYU-UPSRYYMD(1:6)
           MOVE    "31"                TO  SYU-UPSRYYMD(7:2)
      *
           PERFORM  900-DBSELECT-SEC
           PERFORM  UNTIL  FLG-SYUNOU  =  1
      *
      *      初診／再診判定サブ
             INITIALIZE                          SS003-AREA
             MOVE    "1"                 TO      SS003-SYORIKBN
             MOVE    SYU-HOSPNUM         TO      SS003-HOSPNUM
             MOVE    SYU-NYUGAIKBN       TO      SS003-NYUGAIKBN
             MOVE    SYU-PTID            TO      SS003-PTID
             MOVE    SYU-DENPNUM         TO      SS003-DENPNUM
             CALL    "ORCSS003"          USING   SS003-AREA
                                                 SPA-AREA
      *
             MOVE    "1"                 TO  WRK-DRCD(1:1)
             MOVE    SYU-DRCD            TO  WRK-DRCD(2:4)
             PERFORM  320-DRCD-GET-IDX-SEC
             MOVE    SYU-SRYYMD(1:6)     TO  WRK-SRYYM
             PERFORM  320-SRYYM-GET-IDX-SEC
      *
             EVALUATE  SS003-SYOSAISIN-KBN
      *      初診件数カウント
             WHEN    "1"
               ADD    1                    TO  SUM-SYOSHIN(IDX1 IDX6)
               ADD    1                    TO  SUM-SYOSHIN(IDX1 IDX5)
               ADD    1                    TO  SUM-SYOSHIN(IDX3 IDX6)
               ADD    1                    TO  SUM-SYOSHIN(IDX3 IDX5)
      *      再診件数カウント
             WHEN    "2"
               ADD    1                    TO  SUM-SAISHIN(IDX1 IDX6)
               ADD    1                    TO  SUM-SAISHIN(IDX1 IDX5)
               ADD    1                    TO  SUM-SAISHIN(IDX3 IDX6)
               ADD    1                    TO  SUM-SAISHIN(IDX3 IDX5)
      *      その他件数カウント
             WHEN    OTHER
               ADD    1                    TO  SUM-ETC(IDX1 IDX6)
               ADD    1                    TO  SUM-ETC(IDX1 IDX5)
               ADD    1                    TO  SUM-ETC(IDX3 IDX6)
               ADD    1                    TO  SUM-ETC(IDX3 IDX5)
             END-EVALUATE
      *
             PERFORM 900-DBFETCH-SEC
           END-PERFORM
           PERFORM 900-DBCLOSE-SEC
      *
           .
       320-JYURRK-M-CNT-EXT.
           EXIT.
      *****************************************************************
      *    医師別INDEX判別処理
      *****************************************************************
       320-DRCD-GET-IDX-SEC      SECTION.
      *
           MOVE    1             TO  IDX1
           MOVE    SUM-DRCDMAX   TO  IDX2
           PERFORM  UNTIL  IDX1  >  IDX2
             IF   SUM-DRCD(IDX1)  =  WRK-DRCD
               MOVE    ZERO      TO  IDX2
             ELSE
              ADD  1             TO  IDX1
             END-IF
           END-PERFORM
      *
           .
       320-DRCD-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日INDEX判定処理
      *****************************************************************
       320-SRYYMD-GET-IDX-SEC      SECTION.
      *
           MOVE    1               TO  IDX6
           MOVE    SUM-SRYYMDIMAX  TO  IDX2
           PERFORM  UNTIL  IDX6  >  IDX2
             IF   SUM-SRYYMD(IDX6)  =  WRK-SRYYMD
               MOVE    ZERO        TO  IDX2
             ELSE
               ADD  1              TO  IDX6
             END-IF
           END-PERFORM
      *
           .
       320-SRYYMD-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療月INDEX判定処理
      *****************************************************************
       320-SRYYM-GET-IDX-SEC      SECTION.
      *
           MOVE    1               TO  IDX6
           MOVE    SUM-SRYYMDIMAX  TO  IDX2
           PERFORM  UNTIL  IDX6   >  IDX2
             IF    SUM-SRYYMD(IDX6)(1:6)  = WRK-SRYYM
               MOVE    ZERO        TO  IDX2
             ELSE
               ADD  1              TO  IDX6
             END-IF
           END-PERFORM
      *
           .
       320-SRYYM-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    (日別)初診数、再診数、その他印刷処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           MOVE    1          TO  IDX5
           PERFORM  UNTIL  IDX5  >  SUM-SRYYMDIMAX
             MOVE    1        TO  IDX2
             PERFORM  UNTIL  IDX2  >  SUM-DRCDMAX
      *
               INITIALIZE     C100
      *
      *    帳票タイトル
               MOVE "(医師別)  外  来  患  者  数  集  計  表"
                                         TO  C100-MIDASHI
      *    出力年月日
               MOVE    WRK-SYSYMDWH      TO  C100-PRTYMD
      *    集計対象期間開始日
               MOVE    WRK-YMD1WH        TO  C100-SRYMD-FR
      *    集計対象期間終了日
               MOVE    WRK-YMD2WH(11:)   TO  C100-SRMD-TO
      *    集計単位
               MOVE    "月  日"          TO  C100-TANI
      *
               ADD     1                 TO  CNT-PAGE
               MOVE    CNT-PAGE          TO  WRK-PAGE
               MOVE    WRK-PAGE          TO  C100-PAGE
      *
               MOVE    1                 TO  IDX3
               PERFORM  UNTIL  IDX3  >  7
                         OR  IDX2  >  SUM-DRCDMAX
      *    診療科名称を集計エリアに転送
                MOVE    SUM-DRNAME(IDX2) TO  C100-SRYKA(IDX3)
      *    件数名称を転送
                MOVE    "初  診"         TO  C100-KENSU-1(IDX3)
                MOVE    "再  診"         TO  C100-KENSU-2(IDX3)
                MOVE    "その他"         TO  C100-KENSU-3(IDX3)
      *
      *    日別集計数の集計値を転送
                MOVE    1                TO  IDX6
                MOVE    IDX5             TO  IDX4
                MOVE    SUM-SRYYMD(IDX4)(1:6)  TO    WRK-PR-SRYYM
                PERFORM  UNTIL  IDX4  >  SUM-SRYYMDIMAX
                         OR  IDX6  >  32
                         OR  (SUM-SRYYMD(IDX4)  NOT =  "計"
                         AND  SUM-SRYYMD(IDX4)(1:6) NOT = WRK-PR-SRYYM)
                 IF   IDX4  =  SUM-SRYYMDIMAX
                   MOVE    "計"        TO  C100-MD(32)
                   MOVE    SUM-SYOSHIN(IDX2 IDX4)   TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-11-CNT(IDX3 32)
                   MOVE    SUM-SAISHIN(IDX2 IDX4)   TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-12-CNT(IDX3 32)
                   MOVE    SUM-ETC(IDX2 IDX4)       TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-99-CNT(IDX3 32)
                 ELSE
                   MOVE    SUM-SRYYMD(IDX4)(5:2)
                                       TO  C100-MD(IDX6)(1:2)
                   MOVE    "/"         TO  C100-MD(IDX6)(3:1)
                   MOVE    SUM-SRYYMD(IDX4)(7:2)
                                       TO  C100-MD(IDX6)(4:2)
                   MOVE    SUM-SYOSHIN(IDX2 IDX4)   TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-11-CNT(IDX3 IDX6)
                   MOVE    SUM-SAISHIN(IDX2 IDX4)   TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-12-CNT(IDX3 IDX6)
                   MOVE    SUM-ETC(IDX2 IDX4)       TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-99-CNT(IDX3 IDX6)
                 END-IF
                 ADD  1          TO  IDX4
                 ADD  1          TO  IDX6
                END-PERFORM
      *
                ADD  1           TO  IDX2
                ADD  1           TO  IDX3
               END-PERFORM
      *
               PERFORM 340-PRINT-OUT-SEC
      *
             END-PERFORM
             COMPUTE  IDX5  =  IDX5  +  (IDX6  -  1)
           END-PERFORM
      *
      *    CSVヘッダ編集処理
           PERFORM  400-CSV-HEAD-SEC
      *    CSVデータ編集処理
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    (月別)初診数、再診数、その他印刷処理
      *****************************************************************
       330-TERM-M-HEN-SEC         SECTION.
      *
           MOVE    1         TO  IDX5
           PERFORM  UNTIL  IDX5  >  SUM-SRYYMDIMAX
             MOVE    1       TO  IDX2
             PERFORM  UNTIL  IDX2  >  SUM-DRCDMAX
      *
               INITIALIZE     C100
      *
      *    帳票タイトル
               MOVE "(医師別)  外  来  患  者  数  集  計  表"
                                       TO  C100-MIDASHI
      *    出力年月日
               MOVE    WRK-SYSYMDWH    TO  C100-PRTYMD
      *    集計対象期間開始日
               MOVE    WRK-YMD1WH(1:16)     TO  C100-SRYMD-FR
      *    集計対象期間終了日
               MOVE    WRK-YMD2WH(11:6)     TO  C100-SRMD-TO
      *    集計単位
               MOVE    "  月"          TO  C100-TANI
      *
               ADD     1               TO  CNT-PAGE
               MOVE    CNT-PAGE        TO  WRK-PAGE
               MOVE    WRK-PAGE        TO  C100-PAGE
      *
               MOVE    1               TO  IDX3
               PERFORM  UNTIL  IDX3  >  7
                        OR  IDX2  >  SUM-DRCDMAX
      *    診療科名称を転送
                MOVE    SUM-DRNAME(IDX2)  TO  C100-SRYKA(IDX3)
      *    件数名称を転送
                MOVE    "初  診"       TO  C100-KENSU-1(IDX3)
                MOVE    "再  診"       TO  C100-KENSU-2(IDX3)
                MOVE    "その他"       TO  C100-KENSU-3(IDX3)
      *
      *    日別集計数の集計値を転送
                MOVE    1              TO  IDX6
                MOVE    IDX5           TO  IDX4
                PERFORM  UNTIL  IDX4  >  SUM-SRYYMDIMAX
                         OR  IDX6  >  32
                 IF  IDX4  =  SUM-SRYYMDIMAX
                   MOVE    "計"        TO  C100-MD(32)
                   MOVE    SUM-SYOSHIN(IDX2 IDX4)    TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-11-CNT(IDX3 32)
                   MOVE    SUM-SAISHIN(IDX2 IDX4)    TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-12-CNT(IDX3 32)
                   MOVE    SUM-ETC(IDX2 IDX4)        TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-99-CNT(IDX3 32)
                 ELSE
                   MOVE    SUM-SRYYMD(IDX4)(5:2)  TO  C100-MD(IDX6)(4:2)
                   MOVE    SUM-SYOSHIN(IDX2 IDX4)    TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-11-CNT(IDX3 IDX6)
                   MOVE    SUM-SAISHIN(IDX2 IDX4)    TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-12-CNT(IDX3 IDX6)
                   MOVE    SUM-ETC(IDX2 IDX4)        TO  WRK-Z9-5
                   MOVE    WRK-Z9-5    TO  C100-99-CNT(IDX3 IDX6)
                 END-IF
                 ADD  1          TO  IDX4
                 ADD  1          TO  IDX6
                END-PERFORM
      *
                ADD  1           TO  IDX2
                ADD  1           TO  IDX3
               END-PERFORM
      *
               PERFORM 340-PRINT-OUT-SEC
      *
             END-PERFORM
             COMPUTE  IDX5  =  IDX5  +  (IDX6  -  1)
           END-PERFORM
      *
      *    CSVヘッダ編集処理
           PERFORM  400-CSV-HEAD-SEC
      *    CSVデータ編集処理
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-M-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    C100LNK-PRTKANRI-RENNUM      TO  SPRT-RENNUM
           MOVE    C100LNK-PRTKANRI-TBL-KEY     TO  SPRT-TBL-KEY
           MOVE    C100LNK-PRTKANRI-TBL-GROUP   TO  SPRT-TBL-GROUP
           MOVE    C100LNK-PRTKANRI-SRYYM       TO  SPRT-SRYYM
           MOVE    C100LNK-PRTKANRI-SKYYMD      TO  SPRT-SKYYMD
           MOVE    C100LNK-PRTKANRI-SHELLID     TO  SPRT-SHELLID
           MOVE    C100LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    C100LNK-PRTKANRI-PRIORITY    TO  SPRT-PRIORITY
           MOVE    "A00000C100.red"             TO  SPRT-PRTID
           MOVE    "(医師別)外来患者数集計表"   TO  SPRT-TITLE
           MOVE    C100                         TO  SPRT-PRTDATA
           MOVE    C100LNK-PRTKANRI-TERMID      TO  SPRT-TERMID
           MOVE    C100LNK-PRTKANRI-OPID        TO  SPRT-OPID
           MOVE    C100LNK-PRTKANRI-PRTNM       TO  SPRT-PRTNM
           CALL    "ORCSPRT"                    USING
                                                ORCSPRTAREA
                                                SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF   STS-RECEERR  =  ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"             TO  SJOBKANRI-MODE
             INITIALIZE                JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
             MOVE    C100LNK-JOBID     TO  JOB-JOBID
             MOVE    C100LNK-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR       TO  JOB-YOBI
             MOVE    "9999"            TO  JOB-ERRCD
             CALL    "ORCSJOB"         USING
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
      *    CSVヘッダ編集処理
      *****************************************************************
       400-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                 TO  IDXP
      *    対象年月
           INITIALIZE                WRK-ITEM
           STRING    C100-SRYMD-FR   DELIMITED  BY  SIZE
                     "〜"            DELIMITED  BY  SIZE
                     C100-SRMD-TO    DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    作成日セット
           INITIALIZE                WRK-ITEM
           MOVE    C100-PRTYMD       TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    診療科名称セット
           INITIALIZE                   WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-DR  FROM  1  BY  1
                    UNTIL  IDX-DR  >  SUM-DRCDMAX
             INITIALIZE            WRK-ITEM
             STRING    SUM-DRNAME(IDX-DR)    DELIMITED  BY  SPACE
                       ",,"                  DELIMITED  BY  SIZE
                                             INTO  WRK-ITEM
             END-STRING
             IF  IDX-DR  =  SUM-DRCDMAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    初診、再診、その他セット
           IF  C100LNK-MDKBN  =  SPACE
             MOVE    "月日"         TO  WRK-ITEM
           ELSE
             MOVE    "月"           TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-DR  FROM  1  BY  1
                    UNTIL  IDX-DR  >  SUM-DRCDMAX
             MOVE    "初  診"       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
             MOVE    "再  診"       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
             MOVE    "その他"       TO  WRK-ITEM
             IF  IDX-DR  =  SUM-DRCDMAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    CSV出力処理
           PERFORM  230-TOUKEICSV-SEC
      *
           .
      *
       400-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  SUM-SRYYMDIMAX
      *
             MOVE    1                 TO  IDXP
      *
      *    日付
             IF   IDX-LINE  =  SUM-SRYYMDIMAX
               MOVE    "計"           TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             ELSE
               IF  C100LNK-MDKBN  =  SPACE
                 INITIALIZE            WRK-ITEM
                 STRING   SUM-SRYYMD(IDX-LINE)(5:2)  DELIMITED  BY  SIZE
                           "/"                       DELIMITED  BY  SIZE
                          SUM-SRYYMD(IDX-LINE)(7:2)  DELIMITED  BY  SIZE
                                                     INTO  WRK-ITEM
                 END-STRING
               ELSE
                 MOVE    SUM-SRYYMD(IDX-LINE)(5:2)    TO  WRK-ITEM
               END-IF
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
             PERFORM  VARYING  IDX-DR  FROM  1  BY  1
                      UNTIL  IDX-DR  >  SUM-DRCDMAX
      *    初診数
               MOVE    SUM-SYOSHIN(IDX-DR IDX-LINE)  TO  WRK-Z9-5-2
               MOVE    WRK-Z9-5-2          TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
      *    再診数
               MOVE    SUM-SAISHIN(IDX-DR IDX-LINE)  TO  WRK-Z9-5-2
               MOVE    WRK-Z9-5-2          TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
      *    その他
               MOVE    SUM-ETC(IDX-DR IDX-LINE)      TO  WRK-Z9-5-2
               MOVE    WRK-Z9-5-2          TO  WRK-ITEM
               IF  IDX-DR  =  SUM-DRCDMAX
                 PERFORM  810-CSVDATA-EDIT-SEC
               ELSE
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
      *
             END-PERFORM
      *
      *      CSV出力処理
             PERFORM  230-TOUKEICSV-SEC
      *
           END-PERFORM
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-CR-SEC
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
           STRING    ","           DELIMITED  BY  SIZE
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
           STRING    X"0D"         DELIMITED  BY  SIZE
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
           PERFORM  VARYING  IDX-LEN  FROM  1  BY  1
                    UNTIL  IDX-LEN  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX-LEN:)  =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX-LEN:1)  NOT =  SPACE
                 MOVE    IDX-LEN      TO  IDX-ST
                 MOVE    1            TO  IDX-ED
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
           MOVE    C100LNK-PRTKANRI-TBL-KEY  TO  STOUKEICSV-TBL-KEY
           MOVE    C100LNK-PRTKANRI-RENNUM   TO  STOUKEICSV-RENNUM
           MOVE    C100LNK-PRTKANRI-TBL-GROUP  TO  STOUKEICSV-TBL-GROUP
           MOVE    C100LNK-PRTKANRI-SHORI-RENNUM
                                             TO  STOUKEICSV-SHORI-RENNUM
           MOVE    C100LNK-PRTKANRI-SHELLID  TO  STOUKEICSV-SHELLID
           MOVE    C100LNK-PRTKANRI-SRYYM    TO  STOUKEICSV-SRYYM
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    C100LNK-NYUGAIKBN         TO  STOUKEICSV-NYUGAIKBN
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "kanjyasyukei_g_dr_"      DELIMITED  BY  SIZE
                   C100LNK-YMD1              DELIMITED  BY  SIZE
                   "_"                       DELIMITED  BY  SIZE
                   C100LNK-YMD2              DELIMITED  BY  SIZE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "医師別外来患者数集計表"  TO  STOUKEICSV-TITLE
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
           INITIALIZE                  WRK-CSVDATA
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    C100LNK-JOBID       TO  JOB-JOBID
           MOVE    C100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
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
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT  WRK-HENYMDG   REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
             ELSE
               MOVE    1               TO  FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(職員情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC        SECTION.
      *
           MOVE    SYS-1010-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(職員情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-SYSKANRI3
             MOVE    MCPDATA-REC        TO  SYS-1010-REC
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI3
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
          MOVE    "tbl_ptinf"          TO  MCP-TABLE
          MOVE    "key"                TO  MCP-PATHNAME
          CALL    "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
             INITIALIZE                PTINF-REC
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
      *    収納情報検索
      *****************************************************************
       900-DBSELECT-SEC             SECTION.
      *
           MOVE    SYUNOU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    "st_key1"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    "st_key1"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SYUNOU
               MOVE    MCPDATA-REC  TO  SYUNOU-REC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
           END-IF  
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
           .
       900-DBCLOSE-EXT.
           EXIT.
