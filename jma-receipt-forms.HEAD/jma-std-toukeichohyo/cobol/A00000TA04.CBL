      ******************************************************************
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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000TA04.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 来院・在院状況分析表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/01/20  新規作成
      *   01.00.02    門間    2013/07/24  発行区分が空白の場合、在院数が集計されるように修正
      *   01.00.03    門間    2014/03/12  テスト患者を対象外とする
      *   02.00.01    門間    2014/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *   02.00.02    門間    2014/11/14  在院延べ日数算出処理の修正
      *                                   入退院数のカウント処理修正
      *   02.00.03    門間    2014/12/16  在院延べ日数(過去異動分)の修正
      *   02.00.04    門間    2016/02/01  一時ディレクトリ対応(エラーファイル)
      *   02.00.05    門間    2016/05/30  年齢算出処理を修正
      *                                   項目（初診、新患、再診）処理を変更
      *   02.00.06    門間    2018/08/31  CSVファイル出力処理を修正
      *   02.00.07    井上    2020/04/20  転科・転棟・転室の処理を修正
      *******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    対象患者ファイル
           SELECT  TA04PT-FILE     ASSIGN  TA04PT
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     TA04P-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    対象患者ファイル
       FD  TA04PT-FILE.
       01  TA04P. 
           03    TA04P-KEY.
             05  TA04P-SRYKA       PIC X(02).
             05  TA04P-SRYYMD      PIC X(08).
             05  TA04P-PTID        PIC 9(10).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    診療日別受信患者ファイル
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA04PT//.
      *
           COPY    "A00000TA041.INC".
           COPY    "A00000TA042.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-PRINT               PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-KENSU               PIC 9(01).
           03  FLG-TAIIN               PIC 9(01).
           03  FLG-TEIKI               PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-GAITOU              PIC 9(01).
           03  FLG-NZAIIN              PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-INVALID             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(02).
           03  CNT-GPAGE               PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(01).
           03  IDX2                    PIC 9(01).
           03  IDX3                    PIC 9(02).
           03  IDX4                    PIC 9(02).
           03  IDX5                    PIC 9(04).
           03  IDX-DAY                 PIC 9(02).
           03  IDX-KBN                 PIC 9(02).
           03  IDX-LINE                PIC 9(02).
           03  IDX-ST                  PIC 9(03).
           03  IDX-ED                  PIC 9(03).
           03  IDX-LEN                 PIC 9(03).
           03  IDXP                    PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-PRTYM          PIC X(08).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-PRTKBN         PIC X(01).
           03  WRK-PARA-PRTSTYMD       PIC X(08).
           03  WRK-PARA-PRTEDYMD       PIC X(08).
           03  WRK-PARA-ZENPRTYMD.
             05  WRK-PARA-ZENPRTY      PIC 9(04).
             05  WRK-PARA-ZENPRTM      PIC 9(02).
             05  WRK-PARA-ZENPRTD      PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-Z2                  PIC Z9.
           03  WRK-Z5                  PIC Z,ZZ9.
           03  WRK-Z6                  PIC ZZ,ZZ9.
           03  WRK-V5                  PIC ZZ9.9.
           03  WRK-V6                  PIC ZZZ9.9.
           03  WRK-Z4                  PIC ZZZ9.
           03  WRK-Z5-2                PIC ZZZZ9.
           03  WRK-Z3                  PIC ZZZ.
           03  WRK-SYMD.
             05  WRK-SYY               PIC 9(04).
             05  WRK-SMM               PIC 9(02).
             05  WRK-SDD               PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-KEY                 PIC X(05).
           03  WRK-AMARI               PIC 9(02).
           03  WRK-MAX                 PIC 9(03).
           03  WRK-ZENPTID             PIC 9(20).
           03  WRK-ZENSRYKA            PIC 9(02).
           03  WRK-LASTDAY             PIC 9(02).
           03  WRK-KANJASU             PIC 9(05).
           03  WRK-GKANJASU            PIC 9(06).
           03  WRK-KANJASU-AVG         PIC 9(03)V9(01).
           03  WRK-GKANJASU-AVG        PIC 9(03)V9(01).
           03  WRK-GNINZU              PIC 9(04).
           03  WRK-NYUINSTARTDAY       PIC 9(02).
           03  WRK-NYUINENDDAY         PIC 9(02).
           03  WRK-SRYYMD              PIC 9(08).
           03  WRK-STARTDAY            PIC 9(02).
           03  WRK-ENDDAY              PIC 9(02).
           03  WRK-KBNCD               PIC X(05).
           03  WRK-ZENKBNCD-TBL.
             05  WRK-ZENKBNCD          OCCURS 99 TIMES  PIC X(05).
           03  WRK-PTID2               PIC X(20).
           03  WRK-AGE                 PIC 9(03).
           03  WRK-AGE-ST              PIC 9(03).
           03  WRK-AGE-ED              PIC 9(03).
           03  WRK-SYSYMD.
             05  WRK-SYSYY             PIC 9(04).
             05  WRK-SYSMM             PIC 9(02).
             05  WRK-SYSDD             PIC 9(02).
           03  WRK-SYOSINYMD           PIC X(08).
      *
      *    集計領域
       01  WRK-SYUKEI-AREA.
           03  WRK-MEISAI-TBL          OCCURS 100 TIMES.
             05  WRK-NAME              PIC X(20).
             05  WRK-NAMEKBN           PIC X(05).
             05  WRK-YUKOSTYMD         PIC X(08).
             05  WRK-YUKOEDYMD         PIC X(08).
             05  WRK-KANAME            PIC X(20).
             05  WRK-SYOSIN            PIC 9(05).
             05  WRK-SINKAN            PIC 9(05).
             05  WRK-SYOJKNGAI         PIC 9(05).
             05  WRK-SAISIN            PIC 9(05).
             05  WRK-SAIJKNGAI         PIC 9(05).
             05  WRK-NNINZU            PIC 9(05).
             05  WRK-JNINZU            PIC 9(05).
             05  WRK-KANJA-AVG         PIC 9(05).
             05  WRK-SYOSIN-AVG        PIC 9(05).
             05  WRK-TUUIN-AVG         PIC 9(05).
             05  WRK-KZAIIN            PIC 9(05).
             05  WRK-NYUIN             PIC 9(05).
             05  WRK-TAIIN             PIC 9(05).
             05  WRK-SIBOU             PIC 9(05).
             05  WRK-ZAIIN             PIC 9(05).
             05  WRK-NZAIIN            PIC 9(05).
             05  WRK-NGAIHAKU          PIC 9(05).
             05  WRK-NINZU             PIC 9(04).
      *
      *    合計集計領域
       01  SUM-G-AREA.
           03  SUM-G-GSYOSIN           PIC 9(06).
           03  SUM-G-GSINKAN           PIC 9(06).
           03  SUM-G-GSYOJKNGAI        PIC 9(06).
           03  SUM-G-GSAISIN           PIC 9(06).
           03  SUM-G-GSAIJKNGAI        PIC 9(06).
           03  SUM-G-GNNINZU           PIC 9(06).
           03  SUM-G-GJNINZU           PIC 9(06).
           03  SUM-G-GKZAIIN           PIC 9(06).
           03  SUM-G-GNYUIN            PIC 9(06).
           03  SUM-G-GTAIIN            PIC 9(06).
           03  SUM-G-GSIBOU            PIC 9(06).
           03  SUM-G-GZAIIN            PIC 9(06).
           03  SUM-G-GNZAIIN           PIC 9(06).
           03  SUM-G-GNGAIHAKU         PIC 9(06).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    CSV領域
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)  VALUE 100.
           03  WRK-ITEM                PIC X(100)  VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000)  VALUE SPACE.
      *
       01  WRK-JNISSU-AREA.
           03  WRK-JNISSU-SEC          OCCURS 3000.
             05  WRK-PTID              PIC X(10).
             05  WRK-KEYKBN            PIC X(05).
             05  WRK-NYUINYMD          PIC X(08).
      *
      *    プログラムオプション
       01  PGOPTION-AREA.
           03  STX-DATA-TBL            OCCURS 30.
               05  STX-AGE             PIC X(07). 
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
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    医師情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災自賠責医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受信履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院行為
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
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
      *    初再診取得サブ
           COPY    "CPORCSS003.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療科履歴マスタ
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
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
      *
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＰＧオプション
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE            SECTION.
       01  COMMAND-PARAM.
           02  FILLER            PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           DISPLAY "*** START ***"
           PERFORM  100-INIT-SEC
      *
           DISPLAY "FLG-END   =  "FLG-END 
           IF  FLG-END  NOT =  1
             PERFORM  300-MAIN-SEC
           END-IF
      *
           PERFORM  600-END-SEC
           DISPLAY "*** END ***"
      *
           STOP  RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-G-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-SYUKEI-AREA
           INITIALIZE                  WRK-JNISSU-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM  100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO   LNK-PRTKANRI-RENNUM
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
                                              WRK-PARA-PRTYM
                                              WRK-PARA-NYUGAIKBN
                                              WRK-PARA-PRTKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "A00000TA04"        TO  JOB-PGID
           MOVE    "来院・在院状況分析表"   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "TA04TP"                 TO  TA04PT-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA04PT-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA04PT-HOSPNUM
      *
           MOVE    TA04PT-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA04PT-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    初診／再診判定サブ初期処理
           INITIALIZE                      SS003-AREA
           MOVE    ZERO            TO      SS003-SYORIKBN
           CALL    "ORCSS003"      USING   SS003-AREA
                                           SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM  200-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       200-PARA-HENSYU-SEC            SECTION.
      *
           MOVE    ZERO                  TO  FLG-END
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD   TO  SYS-1001-EDYUKYMD
                                             SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM   900-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入外区分チェック
           IF  WRK-PARA-NYUGAIKBN  NOT =  "1"
               AND  WRK-PARA-NYUGAIKBN  NOT =  "2"
             MOVE    "入外区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    処理区分チェック
           IF  WRK-PARA-PRTKBN  NOT =  SPACE
               AND   WRK-PARA-PRTKBN  NOT =  "1"
               AND   WRK-PARA-PRTKBN  NOT =  "2"
               AND   WRK-PARA-PRTKBN  NOT =  "3"
             MOVE    "処理区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    対象年月最初日取得
           MOVE    WRK-PARA-PRTYM        TO  WRK-PARA-PRTSTYMD(1:6)
           MOVE    "01"                  TO  WRK-PARA-PRTSTYMD(7:2)
      *    対象年月最終日取得
           INITIALIZE                    WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO  WRK-SYMD(1:6)
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"                  TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD              TO  LNK-DAY7-YM
           CALL    "ORCSDAY"             USING
                                         STS-AREA-DAY
                                         LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI       TO  WRK-SYMD
           MOVE    WRK-SYMD              TO  WRK-PARA-PRTEDYMD
      *
      *    対象年月の前月最終日取得
           MOVE    WRK-PARA-PRTYM        TO  WRK-PARA-ZENPRTYMD
           IF   WRK-PARA-ZENPRTM  >=  2
               AND  WRK-PARA-ZENPRTM  <=  12
             COMPUTE  WRK-PARA-ZENPRTM  =  WRK-PARA-ZENPRTM  -  1
           ELSE
             COMPUTE  WRK-PARA-ZENPRTY  =  WRK-PARA-ZENPRTY  -  1
             MOVE    12          TO  WRK-PARA-ZENPRTM
           END-IF
           INITIALIZE                    WRK-SYMD
           MOVE    WRK-PARA-ZENPRTYMD    TO  WRK-SYMD(1:6)
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"                  TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD              TO  LNK-DAY7-YM
           CALL    "ORCSDAY"             USING
                                         STS-AREA-DAY
                                         LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI       TO  WRK-SYMD
           MOVE    WRK-SYMD              TO  WRK-PARA-ZENPRTYMD
      *
      *    集計名称取得
           MOVE    1           TO  IDX-KBN
           EVALUATE  WRK-PARA-PRTKBN
             WHEN  "1"
             WHEN  SPACE
      *    診療科別
               MOVE    SPACE                 TO  SYS-1005-REC
               INITIALIZE                    SYS-1001-REC
               MOVE    "1005"                TO  SYS-1005-KANRICD
               MOVE    "%"                   TO  SYS-1005-KBNCD
               MOVE    SPA-HOSPNUM           TO  SYS-1005-HOSPNUM
               MOVE    WRK-PARA-PRTYM        TO  SYS-1005-EDYUKYMD(1:6)
               MOVE    "01"                  TO  SYS-1005-EDYUKYMD(7:2)
               MOVE    WRK-PARA-PRTEDYMD     TO  SYS-1005-STYUKYMD
               MOVE    SYS-1005-REC          TO  MCPDATA-REC
               PERFORM   910-SYSKANRI-SELECT-SEC
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               PERFORM  UNTIL  FLG-SYSKANRI  =  1
                 MOVE    SYS-1005-SRYKANAME  TO  WRK-NAME(IDX-KBN)
                 MOVE    SYS-1005-KBNCD      TO  WRK-NAMEKBN(IDX-KBN)
                 MOVE    SYS-1005-STYUKYMD   TO  WRK-YUKOSTYMD(IDX-KBN)
                 MOVE    SYS-1005-EDYUKYMD   TO  WRK-YUKOEDYMD(IDX-KBN)
                 ADD   1                     TO  IDX-KBN
                 MOVE    IDX-KBN             TO  WRK-MAX
                 PERFORM   910-SYSKANRI-FETCH-SEC
                 MOVE    MCPDATA-REC         TO  SYS-1005-REC
               END-PERFORM
               PERFORM   910-SYSKANRI-CLOSE-SEC
               MOVE    "該当診療科なし"      TO  WRK-NAME(WRK-MAX)
               MOVE    "99999"               TO  WRK-NAMEKBN(WRK-MAX)
               MOVE    "00000000"            TO  WRK-YUKOSTYMD(WRK-MAX)
               MOVE    "99999999"            TO  WRK-YUKOEDYMD(WRK-MAX)
             WHEN  "2"
      *    医師別
               MOVE    SPACE                 TO  SYS-1010-REC
               INITIALIZE                    SYS-1001-REC
               MOVE    "1010"                TO  SYS-1010-KANRICD
               MOVE    "1%"                  TO  SYS-1010-KBNCD
               MOVE    SPA-HOSPNUM           TO  SYS-1010-HOSPNUM
               MOVE    WRK-PARA-PRTYM        TO  SYS-1010-EDYUKYMD(1:6)
               MOVE    "01"                  TO  SYS-1010-EDYUKYMD(7:2)
               MOVE    WRK-PARA-PRTEDYMD     TO  SYS-1010-STYUKYMD

               MOVE    SYS-1010-REC          TO  MCPDATA-REC
               PERFORM   910-SYSKANRI-SELECT-SEC
               MOVE    MCPDATA-REC           TO  SYS-1010-REC
               PERFORM  UNTIL  FLG-SYSKANRI  =  1
                 MOVE    SYS-1010-NAME       TO  WRK-NAME(IDX-KBN)
                 MOVE    SYS-1010-KBNCD      TO  WRK-NAMEKBN(IDX-KBN)
                 MOVE    SYS-1010-STYUKYMD   TO  WRK-YUKOSTYMD(IDX-KBN)
                 MOVE    SYS-1010-EDYUKYMD   TO  WRK-YUKOEDYMD(IDX-KBN)
                 ADD   1                     TO  IDX-KBN
                 MOVE    IDX-KBN             TO  WRK-MAX
                 PERFORM   910-SYSKANRI-FETCH-SEC
                 MOVE    MCPDATA-REC         TO  SYS-1010-REC
               END-PERFORM
               PERFORM   910-SYSKANRI-CLOSE-SEC
               MOVE    "該当医師なし"        TO  WRK-NAME(WRK-MAX)
               MOVE    "99999"               TO  WRK-NAMEKBN(WRK-MAX)
               MOVE    "00000000"            TO  WRK-YUKOSTYMD(WRK-MAX)
               MOVE    "99999999"            TO  WRK-YUKOEDYMD(WRK-MAX)
             WHEN  "3"
      *    年齢別
      *    プログラムオプション取込み
               INITIALIZE                    ORCSPRGOPTIONAREA
                                             PGOPTION-AREA
               MOVE    "A00000TA04"          TO  ORCSPRGOPTION-PRGID
               MOVE    "AGE"                 TO  ORCSPRGOPTION-IN-DATA
               PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                        UNTIL  IDX-KBN  >  30
                 MOVE    IDX-KBN         TO  ORCSPRGOPTION-IN-DATA(4:2)
                 CALL    "ORCSPRGOPTION"     USING
                                             SPA-AREA
                                             ORCSPRGOPTIONAREA
                 IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
                   UNSTRING  ORCSPRGOPTION-OT-PARA  DELIMITED  BY  "-"
                                         INTO  WRK-YUKOSTYMD(IDX-KBN)
                                               WRK-YUKOEDYMD(IDX-KBN)
                   END-UNSTRING
                   MOVE    IDX-KBN       TO  WRK-NAMEKBN(IDX-KBN)
                   IF  WRK-YUKOEDYMD(IDX-KBN)  =  999
                     STRING  WRK-YUKOSTYMD(IDX-KBN) DELIMITED  BY  SPACE
                             "歳 〜"
                                             DELIMITED  BY  SIZE
                                                 INTO  WRK-NAME(IDX-KBN)
                     END-STRING
                   ELSE
                     STRING  WRK-YUKOSTYMD(IDX-KBN) DELIMITED  BY  SPACE
                             "歳 〜 "               DELIMITED  BY  SIZE
                             WRK-YUKOEDYMD(IDX-KBN) DELIMITED  BY  SPACE
                             "歳"                   DELIMITED  BY  SIZE
                                                 INTO  WRK-NAME(IDX-KBN)
                     END-STRING
                   END-IF
                 ELSE
                   MOVE    IDX-KBN           TO  WRK-MAX
                   MOVE    99                TO  IDX-KBN
                 END-IF
               END-PERFORM
               MOVE    "該当なし"            TO  WRK-NAME(WRK-MAX)
               MOVE    "99999"               TO  WRK-NAMEKBN(WRK-MAX)
               MOVE    "99999999"            TO  WRK-YUKOSTYMD(WRK-MAX)
               MOVE    "99999999"            TO  WRK-YUKOEDYMD(WRK-MAX)
           END-EVALUATE
      *
           .
       200-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       300-MAIN-SEC            SECTION.
      *
           IF  WRK-PARA-NYUGAIKBN  =  "1"
      *    入院履歴情報
             PERFORM  900-PTNYUINRRK-SELECT-SEC
             PERFORM  UNTIL  FLG-PTNYUINRRK  =  1
      *    入院カウント
               PERFORM  400-NYUIN-CNT-SEC
      *
               PERFORM  900-PTNYUINRRK-FETCH-SEC
             END-PERFORM
             PERFORM  900-PTNYUINRRK-CLOSE-SEC
      *
           ELSE
      *    診療行為情報
             PERFORM  900-SRYACT-SELECT-SEC
             PERFORM  UNTIL  FLG-SRYACT  =  1
      *
      *    初診・再診カウント
               PERFORM  400-SYOSAISHIN-CNT-SEC
      *
               PERFORM  900-SRYACT-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYACT-CLOSE-SEC
      *
      *    受信履歴情報
             PERFORM  910-JYURRK-SELECT-SEC
             PERFORM  UNTIL  FLG-JYURRK  =  1
      *
      *    延べ人数、実人数集計カウント
               PERFORM  400-NINZU-CNT-SEC
      *
               PERFORM  910-JYURRK-FETCH-SEC
             END-PERFORM
             PERFORM  910-JYURRK-CLOSE-SEC
      *
      *      対象患者ファイル初期化
             OPEN  OUTPUT TA04PT-FILE
             CLOSE        TA04PT-FILE
      *
             OPEN  I-O TA04PT-FILE
      *
      *      収納情報
             PERFORM  910-DBSELECT-SEC
             PERFORM  UNTIL      FLG-SYUNOU  = 1
      *
      *        初診、再診件数を取得する。
               PERFORM 420-SYOSAISHIN-CNT-SUB-SEC
      *
               PERFORM 910-DBFETCH-SEC
             END-PERFORM
             PERFORM 910-DBCLOSE-SEC
      *
             CLOSE     TA04PT-FILE
      *
           END-IF
      *
           IF  FLG-KENSU  =  1
             IF  WRK-PARA-NYUGAIKBN  =  "1"
               PERFORM  400-PRINT-SEC
             ELSE
               PERFORM  410-PRINT-SEC
             END-IF
           ELSE
             MOVE    "データがありません"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       300-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入院カウント
      *****************************************************************
       400-NYUIN-CNT-SEC           SECTION.
      *
      *    繰越在院数
           IF  PTNYUINRRK-NYUINYMD  <=  WRK-PARA-ZENPRTYMD
               AND  PTNYUINRRK-TAIINYMD  >  WRK-PARA-ZENPRTYMD
               AND  PTNYUINRRK-TENNYUYMD <  WRK-PARA-PRTSTYMD
               AND  PTNYUINRRK-TENSTUYMD >=  WRK-PARA-PRTSTYMD
      *
      *    診療科、医師セット
             MOVE     WRK-PARA-PRTSTYMD      TO  WRK-SRYYMD
             PERFORM  500-NYUIN-KBNSET-SEC
      *
             ADD  1         TO  WRK-KZAIIN(IDX-KBN)
             MOVE    1      TO  FLG-KENSU
           END-IF
      *
      *    入院数
           IF  PTNYUINRRK-NYUINYMD(1:6)   =  WRK-PARA-PRTYM(1:6)
               AND  PTNYUINRRK-NYUINYMD   =  PTNYUINRRK-TENNYUYMD
               AND  PTNYUINRRK-RRKEDANUM  =  1
      *
      *    診療科、医師セット
             MOVE     PTNYUINRRK-NYUINYMD    TO  WRK-SRYYMD
             PERFORM  500-NYUIN-KBNSET-SEC
      *
             ADD  1         TO  WRK-NYUIN(IDX-KBN)
             MOVE    1      TO  FLG-KENSU
           END-IF
      *
      *    退院数
           IF   PTNYUINRRK-TAIINYMD(1:6)  =  WRK-PARA-PRTYM(1:6)
               AND  PTNYUINRRK-TAIINYMD   =  PTNYUINRRK-TENSTUYMD
               AND  PTNYUINRRK-RRKEDANUM  =  PTNYUINRRK-MAXEDANUM
      *
      *    診療科、医師セット
             MOVE     PTNYUINRRK-TAIINYMD    TO  WRK-SRYYMD
             PERFORM  500-NYUIN-KBNSET-SEC
      *
             ADD  1         TO  WRK-TAIIN(IDX-KBN)
             MOVE    1      TO  FLG-KENSU
             MOVE    PTNYUINRRK-PTID         TO  WRK-PTID2
             PERFORM  900-PTINF-READ-SEC
             IF  PTINF-DEATHKBN  =  "1"
      *    [内]死亡
               ADD  1       TO  WRK-SIBOU(IDX-KBN)
             END-IF
           END-IF
      *
           INITIALIZE  FLG-NZAIIN
      *
      *    在院数
           IF  PTNYUINRRK-NYUINYMD  <=  WRK-PARA-PRTEDYMD
               AND  PTNYUINRRK-TAIINYMD  >=  WRK-PARA-PRTSTYMD
      *
      *    診療科、医師セット
             MOVE     WRK-PARA-PRTEDYMD      TO  WRK-SRYYMD
             PERFORM  500-NYUIN-KBNSET-SEC
      *
      *       ADD  1         TO  WRK-ZAIIN(IDX-KBN)
      *       MOVE    1      TO  FLG-KENSU
             MOVE    ZERO    TO  FLG-LOOPEND
      *    実人数
             PERFORM  VARYING  IDX5  FROM  1  BY  1
                      UNTIL  IDX5  >  3000
                      OR  FLG-LOOPEND  =  1
               EVALUATE  WRK-PARA-PRTKBN
                 WHEN  "1"
                 WHEN  SPACE
      *    診療科別
                   IF  ( WRK-PTID(IDX5)  =  PTNYUINRRK-PTID
                       AND  WRK-KEYKBN(IDX5)(1:2)  =  PTNYUINRRK-NYUINKA
                       AND  WRK-NYUINYMD(IDX5)  =  PTNYUINRRK-NYUINYMD )
                     MOVE    1           TO  FLG-LOOPEND
                   END-IF
      *
                   IF  ( WRK-PTID(IDX5)  =  SPACE
                       AND  WRK-KEYKBN(IDX5)(1:2)  =  SPACE
                       AND  WRK-NYUINYMD(IDX5)  =  SPACE )
                       AND  FLG-LOOPEND  NOT =  1
                     ADD  1              TO  WRK-ZAIIN(IDX-KBN)
                     MOVE    PTNYUINRRK-NYUINKA
                                         TO  WRK-KEYKBN(IDX5)(1:2)
                     MOVE    PTNYUINRRK-PTID      TO  WRK-PTID(IDX5)
                     MOVE    PTNYUINRRK-NYUINYMD  TO  WRK-NYUINYMD(IDX5)
                     MOVE    9999        TO  IDX5
                   END-IF
      *    医師別
                 WHEN  "2"
                   IF  ( WRK-PTID(IDX5)  =  PTNYUINRRK-PTID
                       AND  WRK-KEYKBN(IDX5)  =  PTNYUINRRK-DRCD1
                       AND  WRK-NYUINYMD(IDX5)  =  PTNYUINRRK-NYUINYMD )
                     MOVE    1           TO  FLG-LOOPEND
                   END-IF
      *
                   IF  ( WRK-PTID(IDX5)  =  SPACE
                       AND  WRK-KEYKBN(IDX5)  =  SPACE
                       AND  WRK-NYUINYMD(IDX5)  =  SPACE )
                       AND  FLG-LOOPEND  NOT =  1
                     ADD  1              TO  WRK-ZAIIN(IDX-KBN)
                     MOVE    PTNYUINRRK-DRCD1     TO  WRK-KEYKBN(IDX5)
                     MOVE    PTNYUINRRK-PTID      TO  WRK-PTID(IDX5)
                     MOVE    PTNYUINRRK-NYUINYMD  TO  WRK-NYUINYMD(IDX5)
                     MOVE    1           TO  FLG-LOOPEND
                   END-IF
                 WHEN  "3"
      *    年齢別
                   IF  ( WRK-PTID(IDX5)  =  PTNYUINRRK-PTID
                       AND  WRK-NYUINYMD(IDX5)  =  PTNYUINRRK-NYUINYMD )
                     MOVE    1           TO  FLG-LOOPEND
                   END-IF
      *
                   IF  ( WRK-PTID(IDX5)  =  SPACE
                       AND  WRK-KEYKBN(IDX5)  =  SPACE
                       AND  WRK-NYUINYMD(IDX5)  =  SPACE )
                       AND  FLG-LOOPEND  NOT =  1
                     ADD  1              TO  WRK-ZAIIN(IDX-KBN)
                     MOVE    IDX5                 TO  WRK-KEYKBN(IDX5)
                     MOVE    PTNYUINRRK-PTID      TO  WRK-PTID(IDX5)
                     MOVE    PTNYUINRRK-NYUINYMD  TO  WRK-NYUINYMD(IDX5)
                     MOVE    1           TO  FLG-LOOPEND
                   END-IF
               END-EVALUATE
             END-PERFORM
      *
             IF  (     PTNYUINRRK-TENNYUYMD(1:6)  =  WRK-PARA-PRTYM
                   AND  (      PTNYUINRRK-JTIKBN  =  "2"
                           OR  PTNYUINRRK-JTIKBN  =  "3"
                           OR  PTNYUINRRK-JTIKBN  =  "4"  
                           OR  PTNYUINRRK-JTIKBN  =  "7"  ) )
               MOVE    1         TO  FLG-NZAIIN
             END-IF
      *
           END-IF
      *
      *    在院延べ日数
           IF  PTNYUINRRK-TENSTUYMD  >  WRK-PARA-PRTEDYMD
             MOVE    WRK-PARA-PRTEDYMD(7:2)     TO  WRK-NYUINENDDAY
           ELSE
             MOVE    PTNYUINRRK-TENSTUYMD(7:2)  TO  WRK-NYUINENDDAY
           END-IF
           IF  PTNYUINRRK-TENNYUYMD  <  WRK-PARA-PRTSTYMD
             MOVE    1           TO  WRK-NYUINSTARTDAY
           ELSE
             MOVE    PTNYUINRRK-TENNYUYMD(7:2)  TO  WRK-NYUINSTARTDAY
             PTNYUINRRK-TENNYUYMD(7:2)
           END-IF
           COMPUTE  WRK-KANJASU  =  WRK-NYUINENDDAY
                                 -  WRK-NYUINSTARTDAY  +  1
           IF  FLG-NZAIIN  =  1
             ADD    -1           TO  WRK-KANJASU
           END-IF
           ADD  WRK-KANJASU      TO  WRK-NZAIIN(IDX-KBN)
      *
      *    外泊
           PERFORM  900-NYUINACCT-SELECT-SEC
           PERFORM  UNTIL  FLG-NYUINACCT  =  1
      *
             IF  NACCT-ZAIKAISU  >  ZERO
               IF  PTNYUINRRK-TENNYUYMD(1:6)  =  WRK-PARA-PRTYM
                 MOVE    PTNYUINRRK-TENNYUYMD(7:2)    TO  WRK-STARTDAY
               ELSE
                 IF    PTNYUINRRK-TENNYUYMD(1:6)  <  WRK-PARA-PRTYM
                   MOVE    WRK-PARA-PRTSTYMD(7:2)     TO  WRK-STARTDAY
                 END-IF
               END-IF
               IF     PTNYUINRRK-TENSTUYMD(1:6)  =  WRK-PARA-PRTYM
                 MOVE    PTNYUINRRK-TENSTUYMD(7:2)    TO  WRK-ENDDAY
               ELSE
                 IF    PTNYUINRRK-TENNYUYMD(1:6)  >  WRK-PARA-PRTYM
                   MOVE    WRK-PARA-PRTEDYMD(7:2)     TO  WRK-STARTDAY
                 END-IF
               END-IF
               PERFORM  VARYING  IDX3  FROM  WRK-STARTDAY  BY  1
                        UNTIL  IDX3  >  WRK-ENDDAY
                        OR  IDX3  >  31
                 IF  NACCT-DAY(IDX3)  >  ZERO
                   ADD  1            TO  WRK-NGAIHAKU(IDX-KBN)
                   MOVE    1         TO  FLG-KENSU
                   MOVE    99        TO  IDX3
                 END-IF
               END-PERFORM
             END-IF
      *
             PERFORM  900-NYUINACCT-FETCH-SEC
           END-PERFORM
           PERFORM  900-NYUINACCT-CLOSE-SEC
      *
           .
       400-NYUIN-CNT-EXT.
           EXIT.
      *****************************************************************
      *    初診再診カウント
      *****************************************************************
       400-SYOSAISHIN-CNT-SEC            SECTION.
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL  IDX2  >  5
                    OR  SRY-SRYCD(IDX2)  =  SPACE
      *
             IF  SRY-SRYKBN  =  "11"  OR  "12"  OR  "13"
      *    診療会計情報取得
               PERFORM  900-SRYACCT-READ-SEC
      *
               PERFORM  VARYING  IDX-DAY  FROM  1  BY  1
                        UNTIL  IDX-DAY  >  31
      *
                 IF  ACCT-DAY(1 IDX-DAY)  NOT =  ZERO
      *    診療科、医師セット
                   PERFORM  510-GAIRAI-KBNSET-SEC
      *
      *    [内]診察時間外
                   IF  ( SRY-SRYCD(IDX2)  =  "111000570"
                       OR  SRY-SRYCD(IDX2)  =  "111000670"
                       OR  SRY-SRYCD(IDX2)  =  "111000770"
                       OR  SRY-SRYCD(IDX2)  =  "111000870"
                       OR  SRY-SRYCD(IDX2)  =  "111011670"
                       OR  SRY-SRYCD(IDX2)  =  "111011770"
                       OR  SRY-SRYCD(IDX2)  =  "111011970"
                       OR  SRY-SRYCD(IDX2)  =  "111012070"
                       OR  SRY-SRYCD(IDX2)  =  "111012170"
                       OR  SRY-SRYCD(IDX2)  =  "111012270"
                       OR  SRY-SRYCD(IDX2)  =  "113007170"
                       OR  SRY-SRYCD(IDX2)  =  "111010770"
                       OR  SRY-SRYCD(IDX2)  =  "113009670"
                       OR  SRY-SRYCD(IDX2)  =  "113007270" )
                     ADD  1          TO  WRK-SYOJKNGAI(IDX-KBN)
                     MOVE    1       TO  FLG-KENSU
                   END-IF
      *
      *      [内]診察時間外
                   IF  ( SRY-SRYCD(IDX2)  =  "112001110"
                       OR  SRY-SRYCD(IDX2)  =  "112001210"
                       OR  SRY-SRYCD(IDX2)  =  "112001310"
                       OR  SRY-SRYCD(IDX2)  =  "112001410"
                       OR  SRY-SRYCD(IDX2)  =  "112014270"
                       OR  SRY-SRYCD(IDX2)  =  "112014370"
                       OR  SRY-SRYCD(IDX2)  =  "112014770"
                       OR  SRY-SRYCD(IDX2)  =  "112014870"
                       OR  SRY-SRYCD(IDX2)  =  "112014970"
                       OR  SRY-SRYCD(IDX2)  =  "112015070"
                       OR  SRY-SRYCD(IDX2)  =  "112006070"
                       OR  SRY-SRYCD(IDX2)  =  "113007470"
                       OR  SRY-SRYCD(IDX2)  =  "113009770"
                       OR  SRY-SRYCD(IDX2)  =  "113007570" )
                     ADD  1          TO  WRK-SAIJKNGAI(IDX-KBN)
                     MOVE    1       TO  FLG-KENSU
                   END-IF
      *
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM
      *
           .
       400-SYOSAISHIN-CNT-EXT.
           EXIT.
      *****************************************************************
      *    延べ数、実人数集計
      *****************************************************************
       400-NINZU-CNT-SEC         SECTION.
      *
      *    名称インデックスセット
           MOVE    ZERO           TO  FLG-LOOPEND
           PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                    UNTIL  IDX-KBN  >  WRK-MAX
                    OR  FLG-LOOPEND  =  1
             EVALUATE  WRK-PARA-PRTKBN
               WHEN  "1"
               WHEN  SPACE
      *    診療科別
                 IF  JYURRK-SRYKA  =  WRK-NAMEKBN(IDX-KBN)(1:2)
                     AND  JYURRK-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                     AND  JYURRK-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                   MOVE    1      TO  FLG-LOOPEND
                 END-IF
               WHEN  "2"
      *    医師別
                 IF  JYURRK-DRCD  =  WRK-NAMEKBN(IDX-KBN)
                     AND  JYURRK-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                     AND  JYURRK-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                   MOVE    1      TO  FLG-LOOPEND
                 END-IF
               WHEN  "3"
      *    年齢別
                 MOVE    JYURRK-PTID           TO  WRK-PTID2
                 PERFORM  900-PTINF-READ-SEC
                 MOVE    JYURRK-SRYYMD         TO  WRK-SYSYMD
                 MOVE    PTINF-BIRTHDAY        TO  WRK-SYMD
                 COMPUTE  WRK-AGE  =  WRK-SYSYY  -  WRK-SYY
                 IF  JYURRK-SRYYMD(5:4)  <  WRK-SYMD(5:4)
                   COMPUTE  WRK-AGE  =  WRK-AGE  -  1
                 END-IF
      *    文字型から数値型へ移項
                 MOVE    WRK-YUKOSTYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                 MOVE    WRK-Z3       TO  WRK-AGE-ST
                 MOVE    WRK-YUKOEDYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                 MOVE    WRK-Z3       TO  WRK-AGE-ED
                 IF  WRK-AGE  >=  WRK-AGE-ST
                     AND  WRK-AGE  <=  WRK-AGE-ED
                   MOVE    1      TO  FLG-LOOPEND
                 END-IF
             END-EVALUATE
           END-PERFORM
           ADD  -1         TO  IDX-KBN
      *
      *    延べ数
           IF  JYURRK-EDANUM  =  1
             ADD  1        TO  WRK-NNINZU(IDX-KBN)
           END-IF
      *
           EVALUATE  WRK-PARA-PRTKBN
             WHEN  SPACE
             WHEN  "1"
      *    診療科別
               MOVE    JYURRK-SRYKA   TO  WRK-KBNCD
             WHEN  "2"
      *    医師別
               MOVE    JYURRK-DRCD    TO  WRK-KBNCD
             WHEN  "3"
      *    年齢別
               MOVE    SPACE          TO  WRK-KBNCD
           END-EVALUATE
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  WRK-MAX
      *
             IF  ( JYURRK-PTID  NOT =  WRK-ZENPTID )
               INITIALIZE       WRK-ZENKBNCD-TBL
             END-IF
      *
             IF  ( JYURRK-PTID  NOT =  WRK-ZENPTID
                 OR  WRK-KBNCD  NOT =  WRK-ZENKBNCD(IDX4) )
                 AND  WRK-ZENKBNCD(IDX4)  =  SPACE
      *
      *    実人数
               MOVE    1        TO  FLG-KENSU
               ADD  1           TO  WRK-JNINZU(IDX-KBN)
               MOVE    JYURRK-PTID    TO  WRK-ZENPTID
               MOVE    WRK-KBNCD      TO  WRK-ZENKBNCD(IDX4)
               MOVE    99       TO  IDX4
      *
      *    診察料を診療した患者数
               IF  JYURRK-SRYKBN1  =  "01"
                 ADD  1         TO  WRK-NINZU(IDX-KBN)
                 ADD  1         TO  WRK-GNINZU
               END-IF
             ELSE
               IF  ( JYURRK-PTID  =  WRK-ZENPTID
                   AND  WRK-KBNCD  =  WRK-ZENKBNCD(IDX4) )
                 MOVE    99     TO  IDX4
               END-IF
             END-IF
           END-PERFORM
      *
           .
       400-NINZU-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       420-SYOSAISHIN-CNT-SUB-SEC      SECTION.
      *
      *    初診／再診判定サブ
           INITIALIZE                          SS003-AREA
           MOVE    "1"                 TO      SS003-SYORIKBN
           MOVE    SYU-HOSPNUM         TO      SS003-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO      SS003-NYUGAIKBN
           MOVE    SYU-PTID            TO      SS003-PTID
           MOVE    SYU-DENPNUM         TO      SS003-DENPNUM
           CALL    "ORCSS003"          USING   SS003-AREA
                                               SPA-AREA
      *
      *    診療科、医師セット
           PERFORM  520-GAIRAI-KBNSET-SEC
      *
           EVALUATE  SS003-SYOSAISIN-KBN
      *    初診患者数
           WHEN    "1"
             ADD     1                   TO  WRK-SYOSIN(IDX-KBN)
      *
             INITIALIZE            FLG-INVALID
             MOVE    SYU-SRYYMD   TO  TA04P-SRYYMD
             MOVE    SYU-PTID     TO  TA04P-PTID
      *
             IF  ( WRK-PARA-PRTKBN  =  SPACE  OR  "1" )
               MOVE    SYU-SRYKA    TO  TA04P-SRYKA
             ELSE
               INITIALIZE  TA04P-SRYKA
             END-IF
      *
             READ TA04PT-FILE
               KEY      IS      TA04P-KEY
               INVALID  KEY
                 MOVE    1            TO  FLG-INVALID
             END-READ
      *
             IF   FLG-INVALID      =     1 
      *        未処理のデータは患者数を集計処理を行う。
               INITIALIZE       FLG-INVALID
               WRITE  TA04P
                 INVALID  KEY
                 MOVE    1              TO  FLG-INVALID
               END-WRITE
      *
      *        [内]新患者数
               PERFORM  920-JYURRK-READ-SEC
      *
                 IF  (    ( WRK-PARA-PRTKBN  =  "2"  OR  "3" )
                       AND  FLG-JYURRK       =  ZERO
                       AND  JYURRK-SRYYMD    =  SYU-SRYYMD )
                 OR  (    ( WRK-PARA-PRTKBN  =  SPACE  OR  "1" )
                       AND  FLG-JYURRK       =  ZERO
                       AND  JYURRK-SRYYMD    =  SYU-SRYYMD
                       AND  JYURRK-SRYKA     =  TA04P-SRYKA )
      *
                 INITIALIZE  FLG-SRYKARRK
                 MOVE    SYU-SRYYMD         TO  WRK-SYOSINYMD
      *
                 PERFORM  920-SRYKARRK-SELECT-SEC
                 PERFORM  UNTIL  FLG-SRYKARRK  =  1
      *
                   IF  (      KARRK-SYOSINYMD1      <  WRK-SYOSINYMD
                         AND  KARRK-SYOSINYMD1  NOT =  SPACE          )
                     MOVE    KARRK-SYOSINYMD1    TO   WRK-SYOSINYMD
                   END-IF
      *
                   PERFORM  920-SRYKARRK-FETCH-SEC
                 END-PERFORM
                 PERFORM  920-SRYKARRK-CLOSE-SEC
      *
                 IF   WRK-SYOSINYMD  =  SYU-SRYYMD
                   ADD     1                 TO  WRK-SINKAN(IDX-KBN)
                 END-IF
      *
               END-IF
      *
             END-IF
      *
             MOVE    1                   TO  FLG-KENSU
      *    再診患者数
           WHEN    "2"
             ADD     1                   TO  WRK-SAISIN(IDX-KBN)
             MOVE    1                   TO  FLG-KENSU
      *    その他件数カウント
           WHEN    OTHER
             CONTINUE
           END-EVALUATE
      *
           .           
       420-SYOSAISHIN-CNT-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科、医師セット（入院）
      *****************************************************************
       500-NYUIN-KBNSET-SEC            SECTION.
      *
      *    名称インデックスセット
           MOVE    ZERO         TO  FLG-LOOPEND
           PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                    UNTIL  IDX-KBN  >  WRK-MAX
                    OR  FLG-LOOPEND  =  1
             EVALUATE  WRK-PARA-PRTKBN
               WHEN  "1"
               WHEN  SPACE
      *    診療科別
                 IF  PTNYUINRRK-NYUINKA  =  WRK-NAMEKBN(IDX-KBN)(1:2)
                     AND  WRK-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                     AND  WRK-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                   MOVE    1      TO  FLG-LOOPEND
                 END-IF
               WHEN  "2"
      *    医師別
                 IF  PTNYUINRRK-DRCD1  =  WRK-NAMEKBN(IDX-KBN)
                     AND  WRK-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                     AND  WRK-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                   MOVE    1      TO  FLG-LOOPEND
                 END-IF
               WHEN  "3"
      *    年齢別
                 MOVE    PTNYUINRRK-PTID       TO  WRK-PTID2
                 PERFORM  900-PTINF-READ-SEC
                 IF  PTNYUINRRK-TENSTUYMD  NOT =  "99999999"
                   MOVE    PTNYUINRRK-TENSTUYMD  TO  WRK-SYSYMD
                 ELSE
                   MOVE    WRK-PARA-PRTEDYMD     TO  WRK-SYSYMD
                 END-IF
                 MOVE    PTINF-BIRTHDAY        TO  WRK-SYMD
                 COMPUTE  WRK-AGE  =  WRK-SYSYY  -  WRK-SYY
                 IF  WRK-SYSYMD(5:4)  <  WRK-SYMD(5:4)
                   COMPUTE  WRK-AGE  =  WRK-AGE  -  1
                 END-IF
      *    文字型から数値型へ移項
                 MOVE    WRK-YUKOSTYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                 MOVE    WRK-Z3       TO  WRK-AGE-ST
                 MOVE    WRK-YUKOEDYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                 MOVE    WRK-Z3       TO  WRK-AGE-ED
                 IF  WRK-AGE  >=  WRK-AGE-ST
                     AND  WRK-AGE  <=  WRK-AGE-ED
                   MOVE    1      TO  FLG-LOOPEND
                 END-IF
             END-EVALUATE
      *
           END-PERFORM
           ADD  -1              TO  IDX-KBN
      *
      *    診療科、医師該当なし判定
           IF  IDX-KBN  =  WRK-MAX
             MOVE    1     TO  FLG-GAITOU
           END-IF
           .
       500-NYUIN-KBNSET-EXT.
           EXIT.
      *****************************************************************
      *    診療科、医師セット（外来）
      *****************************************************************
       510-GAIRAI-KBNSET-SEC            SECTION.
      *
           PERFORM  910-JYURRK-READ-SEC
      *
           IF  JYURRK-SRYYMD(1:6)  =  WRK-PARA-PRTYM
      *
      *    名称インデックスセット
             MOVE    ZERO         TO  FLG-LOOPEND
             PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                      UNTIL  IDX-KBN  >  WRK-MAX
                      OR  FLG-LOOPEND  =  1
               EVALUATE  WRK-PARA-PRTKBN
                 WHEN  "1"
                 WHEN  SPACE
      *    診療科別
                   IF  ACCT-SRYKA  =  WRK-NAMEKBN(IDX-KBN)(1:2)
                       AND  JYURRK-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                       AND  JYURRK-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                     MOVE    1      TO  FLG-LOOPEND
                   END-IF
                 WHEN  "2"
      *    医師別
                   IF  JYURRK-DRCD  =  WRK-NAMEKBN(IDX-KBN)
                       AND  JYURRK-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                       AND  JYURRK-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                     MOVE    1      TO  FLG-LOOPEND
                   END-IF
                 WHEN  "3"
      *    年齢別
                   MOVE    JYURRK-PTID          TO  WRK-PTID2
                   PERFORM  900-PTINF-READ-SEC
                   MOVE    JYURRK-SRYYMD        TO  WRK-SYSYMD
                   MOVE    PTINF-BIRTHDAY       TO  WRK-SYMD
                   COMPUTE  WRK-AGE  =  WRK-SYSYY  -  WRK-SYY
                   IF  WRK-SYSYMD(5:4)  <  WRK-SYMD(5:4)
                     COMPUTE  WRK-AGE  =  WRK-AGE  -  1
                   END-IF
      *    文字型から数値型へ移項
                   MOVE    WRK-YUKOSTYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                   MOVE    WRK-Z3       TO  WRK-AGE-ST
                   MOVE    WRK-YUKOEDYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                   MOVE    WRK-Z3       TO  WRK-AGE-ED
                   IF  WRK-AGE  >=  WRK-AGE-ST
                       AND  WRK-AGE  <=  WRK-AGE-ED
                     MOVE    1          TO  FLG-LOOPEND
                   END-IF
               END-EVALUATE
      *
             END-PERFORM
             ADD  -1              TO  IDX-KBN
      *    診療科、医師該当なし判定
             IF  IDX-KBN  =  WRK-MAX
               MOVE    1     TO  FLG-GAITOU
             END-IF
           END-IF
      *
           .
       510-GAIRAI-KBNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科、医師セット（外来）  収納
      *****************************************************************
       520-GAIRAI-KBNSET-SEC            SECTION.
      *
           IF  SYU-SRYYMD(1:6)  =  WRK-PARA-PRTYM
      *
      *      名称インデックスセット
             MOVE    ZERO         TO  FLG-LOOPEND
             PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                      UNTIL  IDX-KBN  >  WRK-MAX
                      OR  FLG-LOOPEND  =  1
               EVALUATE  WRK-PARA-PRTKBN
                 WHEN  "1"
                 WHEN  SPACE
      *            診療科別
                   IF  SYU-SRYKA  =  WRK-NAMEKBN(IDX-KBN)(1:2)
                       AND  SYU-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                       AND  SYU-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                     MOVE    1      TO  FLG-LOOPEND
                   END-IF
                 WHEN  "2"
      *            医師別
                   IF  SYU-DRCD  =  WRK-NAMEKBN(IDX-KBN)(2:4)
                       AND  SYU-SRYYMD  >=  WRK-YUKOSTYMD(IDX-KBN)
                       AND  SYU-SRYYMD  <=  WRK-YUKOEDYMD(IDX-KBN)
                     MOVE    1      TO  FLG-LOOPEND
                   END-IF
                 WHEN  "3"
      *            年齢別
                   MOVE    SYU-PTID             TO  WRK-PTID2
                   PERFORM  900-PTINF-READ-SEC
                   MOVE    SYU-SRYYMD           TO  WRK-SYSYMD
                   MOVE    PTINF-BIRTHDAY       TO  WRK-SYMD
                   COMPUTE  WRK-AGE  =  WRK-SYSYY  -  WRK-SYY
                   IF  WRK-SYSYMD(5:4)  <  WRK-SYMD(5:4)
                     COMPUTE  WRK-AGE  =  WRK-AGE  -  1
                   END-IF
      *            文字型から数値型へ移項
                   MOVE    WRK-YUKOSTYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                   MOVE    WRK-Z3       TO  WRK-AGE-ST
                   MOVE    WRK-YUKOEDYMD(IDX-KBN)(1:3)  TO  WRK-Z3
                   MOVE    WRK-Z3       TO  WRK-AGE-ED
                   IF  WRK-AGE  >=  WRK-AGE-ST
                       AND  WRK-AGE  <=  WRK-AGE-ED
                     MOVE    1          TO  FLG-LOOPEND
                   END-IF
               END-EVALUATE
      *
             END-PERFORM
      *
             ADD  -1              TO  IDX-KBN
      *    診療科、医師該当なし判定
             IF  IDX-KBN  =  WRK-MAX
               MOVE    1     TO  FLG-GAITOU
             END-IF
      *
           END-IF
      *
           .
       520-GAIRAI-KBNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理(入院)
      *****************************************************************
       400-PRINT-SEC            SECTION.
      *
           INITIALIZE                      TA041
      *    科、医師該当なしの患者が存在しない場合、「該当なし」の区分は印字しない
           IF  FLG-GAITOU  NOT =  1
             ADD  -1             TO  WRK-MAX
           END-IF
      *
           MOVE    1             TO  IDX
           PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                    UNTIL  IDX-KBN  >  WRK-MAX
                    OR  WRK-NAMEKBN(IDX-KBN)  =  SPACE
      *
      *    科名／医師名
             MOVE    WRK-NAME(IDX-KBN)      TO  TA041-KANAME(IDX)
      *
      *    繰越在院数
             MOVE    WRK-KZAIIN(IDX-KBN)    TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-KZAIIN(IDX)
             ADD  WRK-KZAIIN(IDX-KBN)       TO  SUM-G-GKZAIIN
      *
      *    入院数
             MOVE    WRK-NYUIN(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-NYUIN(IDX)
             ADD  WRK-NYUIN(IDX-KBN)        TO  SUM-G-GNYUIN
      *
      *    退院数
             MOVE    WRK-TAIIN(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-TAIIN(IDX)
             ADD  WRK-TAIIN(IDX-KBN)        TO  SUM-G-GTAIIN
      *
      *    [内]死亡
             MOVE    WRK-SIBOU(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-SIBOU(IDX)
             ADD  WRK-SIBOU(IDX-KBN)        TO  SUM-G-GSIBOU
      *
      *    在院数
             MOVE    WRK-ZAIIN(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-ZAIIN(IDX)
             ADD  WRK-ZAIIN(IDX-KBN)        TO  SUM-G-GZAIIN
      *
      *    在院延べ数
             MOVE    WRK-NZAIIN(IDX-KBN)    TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-NZAIIN(IDX)
             ADD  WRK-NZAIIN(IDX-KBN)       TO  SUM-G-GNZAIIN
      *
      *    [内]外泊延べ数
             MOVE    WRK-NGAIHAKU(IDX-KBN)  TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA041-NGAIHAKU(IDX)
             ADD  WRK-NGAIHAKU(IDX-KBN)     TO  SUM-G-GNGAIHAKU
      *
      *    １日平均入院数
             MOVE    WRK-PARA-PRTEDYMD(7:2) TO  WRK-LASTDAY
             COMPUTE  WRK-KANJASU-AVG  =  WRK-NYUIN(IDX-KBN)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG        TO  WRK-V5
             MOVE    WRK-V5      TO  TA041-NYUIN-AVG(IDX)
      *
      *    １日平均退院数
             COMPUTE  WRK-KANJASU-AVG  =  WRK-TAIIN(IDX-KBN)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG        TO  WRK-V5
             MOVE    WRK-V5      TO  TA041-TAIIN-AVG(IDX)
      *
      *    １日平均在院数
             COMPUTE  WRK-KANJASU-AVG  =  WRK-ZAIIN(IDX-KBN)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG        TO  WRK-V5
             MOVE    WRK-V5      TO  TA041-ZAIIN-AVG(IDX)
      *
             IF  IDX  >=  7
                 AND ( ( WRK-NAMEKBN(IDX-KBN + 1)  NOT =  "99999"
                 AND  WRK-NAMEKBN(IDX-KBN + 1)  NOT =  SPACE )
                 OR  ( WRK-NAMEKBN(IDX-KBN + 1)  =  "99999"
                 AND  FLG-GAITOU  =  1 ) )
      *    印刷処理
               PERFORM  500-NYUIN-PRINT-OUT-SEC
               MOVE    ZERO      TO  IDX
             END-IF
             ADD  1              TO  IDX
      *
           END-PERFORM
      *
      *    合計集計処理
           PERFORM  500-NYUIN-GOUKEI-HEN-SEC
      *
      *    印刷処理
           PERFORM  500-NYUIN-PRINT-OUT-SEC
      *
      *    CSV編集
           PERFORM  400-CSV-HEAD-SEC
           .
       400-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理(外来)
      *****************************************************************
       410-PRINT-SEC            SECTION.
      *
           INITIALIZE                      TA042
      *    科、医師該当なしの患者が存在しない場合、「該当なし」の区分は印字しない
           IF   FLG-GAITOU  NOT =  1
             ADD  -1          TO  WRK-MAX
           END-IF
      *
           MOVE   1        TO  IDX
           PERFORM  VARYING  IDX-KBN  FROM  1  BY  1
                    UNTIL  IDX-KBN  >  WRK-MAX
                    OR  WRK-NAMEKBN(IDX-KBN)  =  SPACE
      *
      *    科名／医師名
             MOVE    WRK-NAME(IDX-KBN)       TO  TA042-KANAME(IDX)
      *
      *    初診患者数
             MOVE    WRK-SYOSIN(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-SYOSIN(IDX)
             ADD  WRK-SYOSIN(IDX-KBN)        TO  SUM-G-GSYOSIN
      *
      *    [内]新患者数
             MOVE    WRK-SINKAN(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-SINKAN(IDX)
             ADD  WRK-SINKAN(IDX-KBN)        TO  SUM-G-GSINKAN
      *
      *    [内]診察時間外
             MOVE    WRK-SYOJKNGAI(IDX-KBN)  TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-SYOJKNGAI(IDX)
             ADD  WRK-SYOJKNGAI(IDX-KBN)     TO  SUM-G-GSYOJKNGAI
      *
      *    再診患者数
             MOVE    WRK-SAISIN(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-SAISIN(IDX)
             ADD  WRK-SAISIN(IDX-KBN)        TO  SUM-G-GSAISIN
      *
      *    [内]診察時間外
             MOVE    WRK-SAIJKNGAI(IDX-KBN)  TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-SAIJKNGAI(IDX)
             ADD  WRK-SAIJKNGAI(IDX-KBN)     TO  SUM-G-GSAIJKNGAI
      *
      *    患者延べ数
             MOVE    WRK-NNINZU(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-NNINZU(IDX)
             ADD  WRK-NNINZU(IDX-KBN)        TO  SUM-G-GNNINZU
      *
      *    実人数
             MOVE    WRK-JNINZU(IDX-KBN)     TO  WRK-Z5
             MOVE    WRK-Z5      TO  TA042-JNINZU(IDX)
             ADD  WRK-JNINZU(IDX-KBN)        TO  SUM-G-GJNINZU
      *
      *    １日平均患者数
             MOVE    WRK-PARA-PRTEDYMD(7:2)  TO  WRK-LASTDAY
             COMPUTE  WRK-KANJASU-AVG  =  WRK-NNINZU(IDX-KBN)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG         TO  WRK-V5
             MOVE    WRK-V5      TO  TA042-KANJA-AVG(IDX)
      *
      *    [内]初診患者数
             COMPUTE  WRK-KANJASU-AVG  =  WRK-SYOSIN(IDX-KBN)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG         TO  WRK-V5
             MOVE    WRK-V5      TO  TA042-SYOSIN-AVG(IDX)
      *
      *    平均通院回数
             COMPUTE  WRK-KANJASU-AVG  =  ( WRK-SAISIN(IDX-KBN)
                                       +  WRK-SYOSIN(IDX-KBN) )
                                       /  WRK-NINZU(IDX-KBN)
             MOVE    WRK-KANJASU-AVG         TO  WRK-V5
             MOVE    WRK-V5      TO  TA042-TUUIN-AVG(IDX)
      *
             IF  IDX  >=  7
                 AND ( ( WRK-NAMEKBN(IDX-KBN + 1)  NOT =  "99999"
                 AND  WRK-NAMEKBN(IDX-KBN + 1)  NOT =  SPACE )
                 OR  ( WRK-NAMEKBN(IDX-KBN + 1)  =  "99999"
                 AND  FLG-GAITOU  =  1 ) )
      *    印刷処理
               PERFORM  510-GAIRAI-PRINT-OUT-SEC
               MOVE    ZERO      TO  IDX
             END-IF
             ADD  1           TO  IDX
      *
           END-PERFORM
      *
      *    合計集計処理
           PERFORM  510-GAIRAI-GOUKEI-HEN-SEC
      *
      *    印刷処理
           PERFORM  510-GAIRAI-PRINT-OUT-SEC
      *
      *    CSV編集
           PERFORM  400-CSV-HEAD-SEC
           .
       410-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    合計集計処理（入院）
      *****************************************************************
       500-NYUIN-GOUKEI-HEN-SEC      SECTION.
      *
      *    繰越在院数
           MOVE    SUM-G-GKZAIIN      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GKZAIIN
      *
      *    入院数
           MOVE    SUM-G-GNYUIN       TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GNYUIN
      *
      *    退院数
           MOVE    SUM-G-GTAIIN       TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GTAIIN
      *
      *    [内]死亡
           MOVE    SUM-G-GSIBOU       TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GSIBOU
      *
      *    在院数
           MOVE    SUM-G-GZAIIN       TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GZAIIN
      *
      *    在院延べ数
           MOVE    SUM-G-GNZAIIN      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GNZAIIN
      *
      *    [内]外泊延べ数
           MOVE    SUM-G-GNGAIHAKU    TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA041-GNGAIHAKU
      *
      *    １日平均入院数
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GNYUIN  /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG   TO  WRK-V6
           MOVE    WRK-V6             TO  TA041-GNYUIN-AVG
      *
      *    １日平均退院数
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GTAIIN  /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG   TO  WRK-V6
           MOVE    WRK-V6             TO  TA041-GTAIIN-AVG
      *
      *    １日平均在院数
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GZAIIN  /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG   TO  WRK-V6
           MOVE    WRK-V6             TO  TA041-GZAIIN-AVG
      *
            .
       500-NYUIN-GOUKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    合計集計処理（外来）
      *****************************************************************
       510-GAIRAI-GOUKEI-HEN-SEC      SECTION.
      *
      *    初診患者数
           MOVE    SUM-G-GSYOSIN      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GSYOSIN
      *
      *    [内]新患者数
           MOVE    SUM-G-GSINKAN      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GSINKAN
      *
      *    [内]診察時間外
           MOVE    SUM-G-GSYOJKNGAI   TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GSYOJKNGAI
      *
      *    再診患者数
           MOVE    SUM-G-GSAISIN      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GSAISIN
      *
      *    [内]診察時間外
           MOVE    SUM-G-GSAIJKNGAI   TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GSAIJKNGAI
      *
      *    患者延べ数
           MOVE    SUM-G-GNNINZU      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GNNINZU
      *
      *    実人数
           MOVE    SUM-G-GJNINZU      TO  WRK-Z6
           MOVE    WRK-Z6             TO  TA042-GJNINZU
      *
      *    １日平均患者数
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GNNINZU  /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG   TO  WRK-V6
           MOVE    WRK-V6             TO  TA042-GKANJA-AVG
      *
      *    [内]初診患者数
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GSYOSIN  /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG   TO  WRK-V6
           MOVE    WRK-V6             TO  TA042-GSYOSIN-AVG
      *
      *    平均通院回数
           COMPUTE  WRK-GKANJASU-AVG  =  ( SUM-G-GSYOSIN
                                      +  SUM-G-GSAISIN )  /  WRK-GNINZU
           MOVE    WRK-GKANJASU-AVG   TO  WRK-V6
           MOVE    WRK-V6             TO  TA042-GTUUIN-AVG
      *
            .
       510-GAIRAI-GOUKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       500-HEAD-HEN-SEC            SECTION.
      *
      *    頁数編集
           ADD  1                      TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-Z2
           MOVE    WRK-Z2              TO  TA041-PAGE
                                           TA042-PAGE
      *
      *    合計頁数計算
           DIVIDE  WRK-MAX    BY  7
                   GIVING  CNT-GPAGE
                   REMAINDER  WRK-AMARI
           IF   WRK-AMARI  NOT =  ZERO
             ADD  1           TO  CNT-GPAGE
           END-IF
      *
           MOVE    CNT-GPAGE           TO  WRK-Z2
           MOVE    WRK-Z2              TO  TA041-GPAGE
                                           TA042-GPAGE
      *
      *    対象年月日編集
           MOVE    WRK-PARA-PRTYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  TA041-SRYYM
                                           TA042-SRYYM
      *
      *    作成年月日編集
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  TA041-CREATEYMD
                                           TA042-CREATEYMD
      *
      *    帳票タイトル・ラベル編集
           EVALUATE  WRK-PARA-PRTKBN
             WHEN  "1"
             WHEN  SPACE
               MOVE    "［科別］"      TO  TA041-GRPTITLE
                                           TA042-GRPTITLE
             WHEN  "2"
               MOVE    "［医師別］"    TO  TA041-GRPTITLE
                                           TA042-GRPTITLE
             WHEN  "3"
               MOVE    "［年齢別］"    TO  TA041-GRPTITLE
                                           TA042-GRPTITLE
           END-EVALUATE
      *
      *    入外区分編集
           IF  WRK-PARA-NYUGAIKBN  =  "1"
             MOVE    "入院"            TO  TA041-NYUGAI
           ELSE
             MOVE    "外来"            TO  TA042-NYUGAI
           END-IF
      *
           .
       500-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       500-NYUIN-PRINT-OUT-SEC            SECTION.
      *
      *    ヘッダー編集
           PERFORM  500-HEAD-HEN-SEC
      *
      *    印刷処理
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO  SPRT-PRIORITY
           MOVE    "A00000TA041.red"       TO  SPRT-PRTID
           MOVE    "来院・在院状況分析表"  TO  SPRT-TITLE
           MOVE    TA041                   TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO  SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                      TA041
      *
           .
       500-NYUIN-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       510-GAIRAI-PRINT-OUT-SEC            SECTION.
      *
      *    ヘッダー編集
           PERFORM  500-HEAD-HEN-SEC
      *
      *    印刷処理
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO  SPRT-PRIORITY
           MOVE    "A00000TA042.red"       TO  SPRT-PRTID
           MOVE    "来院・在院状況分析表"  TO  SPRT-TITLE
           MOVE    TA042                   TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO  SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                      TA042
      *
           .
       510-GAIRAI-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       400-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                  TO  IDXP
      *    対象年月
           INITIALIZE                 WRK-ITEM
           MOVE    WRK-PARA-PRTYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分編集
           IF  WRK-PARA-NYUGAIKBN  =  "1"
             MOVE    "入院"            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           ELSE
             MOVE    "外来"            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           END-IF
      *
      *    作成日セット
           INITIALIZE                WRK-ITEM
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    診療科名称セット
           INITIALIZE                   WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE            WRK-ITEM
             MOVE    WRK-NAME(IDX-LINE)    TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計
           MOVE    "合計"                TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    最終日
           MOVE    WRK-PARA-PRTEDYMD(7:2)   TO  WRK-LASTDAY
      *
      *    詳細内容セット
           IF   WRK-PARA-NYUGAIKBN  =  "1"
             PERFORM  400-CSV-DATA-SEC
           ELSE
             PERFORM  410-CSV-DATA-SEC
           END-IF
      *
           .
      *
       400-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理（入院）
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    繰越在院数
           MOVE    "繰越在院数"        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-KZAIIN(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：繰越在院数
           MOVE    SUM-G-GKZAIIN       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    入院数
           MOVE    "入院数"            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-NYUIN(IDX-LINE)   TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：入院数
           MOVE    SUM-G-GNYUIN        TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    退院数
           MOVE    "退院数"            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-TAIIN(IDX-LINE)   TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：退院数
           MOVE    SUM-G-GTAIIN        TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    [内]死亡
           MOVE    "[内]死亡"          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-SIBOU(IDX-LINE)   TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：[内]死亡
           MOVE    SUM-G-GSIBOU        TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    在院数
           MOVE    "在院数"            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-ZAIIN(IDX-LINE)   TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：在院数
           MOVE    SUM-G-GZAIIN        TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    在院延べ数
           MOVE    "在院延べ数"        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-NZAIIN(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：在院延べ数
           MOVE    SUM-G-GNZAIIN       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    [内]外泊延べ数
           MOVE    "[内]外泊延べ数"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-NGAIHAKU(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2                TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：[内]外泊延べ数
           MOVE    SUM-G-GNGAIHAKU     TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    １日平均入院数
           MOVE    "１日平均入院数"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE  WRK-KANJASU-AVG
             COMPUTE  WRK-KANJASU-AVG  =  WRK-NYUIN(IDX-LINE)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG   TO  WRK-V5
             MOVE    WRK-V5            TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：１日平均入院数
           INITIALIZE  WRK-GKANJASU-AVG
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GNYUIN
                                      /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG  TO  WRK-V6
           MOVE    WRK-V6            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    １日平均退院数
           MOVE    "１日平均退院数"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE  WRK-KANJASU-AVG
             COMPUTE  WRK-KANJASU-AVG  =  WRK-TAIIN(IDX-LINE)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG   TO  WRK-V5
             MOVE    WRK-V5            TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：１日平均退院数
           INITIALIZE  WRK-GKANJASU-AVG
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GTAIIN
                                      /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG  TO  WRK-V6
           MOVE    WRK-V6            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    １日平均在院数
           MOVE    "１日平均在院数"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE  WRK-KANJASU-AVG
             COMPUTE  WRK-KANJASU-AVG  =  WRK-ZAIIN(IDX-LINE)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG   TO  WRK-V5
             MOVE    WRK-V5            TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：１日平均在院数
           INITIALIZE  WRK-GKANJASU-AVG
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GZAIIN
                                      /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG  TO  WRK-V6
           MOVE    WRK-V6            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    CSV出力処理
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理（外来）
      *****************************************************************
       410-CSV-DATA-SEC            SECTION.
      *
      *    初診患者数
           MOVE    "初診患者数"        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-SYOSIN(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：初診患者数
           MOVE    SUM-G-GSYOSIN       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    [内]新患者数
           MOVE    "[内]新患者数"      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-SINKAN(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：[内]新患者数
           MOVE    SUM-G-GSINKAN       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    [内]診察時間外
           MOVE    "[内]診察時間外"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-SYOJKNGAI(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2                 TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：[内]診察時間外
           MOVE    SUM-G-GSYOJKNGAI    TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    再診患者数
           MOVE    "再診患者数"        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-SAISIN(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：再診患者数
           MOVE    SUM-G-GSAISIN       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    [内]診察時間外
           MOVE    "[内]診察時間外"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-SAIJKNGAI(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2                 TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：[内]診察時間外
           MOVE    SUM-G-GSAIJKNGAI    TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    患者延べ数
           MOVE    "患者延べ数"        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-NNINZU(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：患者延べ数
           MOVE    SUM-G-GNNINZU       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    実人数
           MOVE    "実人数"            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             MOVE    WRK-JNINZU(IDX-LINE)  TO  WRK-Z5-2
             MOVE    WRK-Z5-2              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：実人数
           MOVE    SUM-G-GJNINZU       TO  WRK-Z5-2
           MOVE    WRK-Z5-2            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    １日平均患者数
           MOVE    "１日平均患者数"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE  WRK-KANJASU-AVG
             COMPUTE  WRK-KANJASU-AVG  =  WRK-NNINZU(IDX-LINE)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG     TO  WRK-V5
             MOVE    WRK-V5              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：１日平均患者数
           INITIALIZE  WRK-GKANJASU-AVG
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GNNINZU
                                      /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG    TO  WRK-V6
           MOVE    WRK-V6              TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    [内]初診平均数
           MOVE    "[内]初診平均数"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE  WRK-KANJASU-AVG
             COMPUTE  WRK-KANJASU-AVG  =  WRK-SYOSIN(IDX-LINE)
                                       /  WRK-LASTDAY
             MOVE    WRK-KANJASU-AVG     TO  WRK-V5
             MOVE    WRK-V5              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：[内]初診平均数
           INITIALIZE  WRK-GKANJASU-AVG
           COMPUTE  WRK-GKANJASU-AVG  =  SUM-G-GSYOSIN
                                      /  WRK-LASTDAY
           MOVE    WRK-GKANJASU-AVG    TO  WRK-V6
           MOVE    WRK-V6              TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    平均通院回数
           MOVE    "平均通院回数"      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  WRK-MAX
             INITIALIZE  WRK-KANJASU-AVG
             COMPUTE  WRK-KANJASU-AVG  =  ( WRK-SAISIN(IDX-LINE)
                                       +    WRK-SYOSIN(IDX-LINE) )
                                       /  WRK-NINZU(IDX-LINE)
             MOVE    WRK-KANJASU-AVG     TO  WRK-V5
             MOVE    WRK-V5              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    合計：平均通院回数
           INITIALIZE  WRK-GKANJASU-AVG
           COMPUTE  WRK-GKANJASU-AVG  =  ( SUM-G-GSAISIN
                                      +    SUM-G-GSYOSIN )
                                      /  WRK-GNINZU
           MOVE    WRK-GKANJASU-AVG    TO  WRK-V6
           MOVE    WRK-V6              TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    CSV出力処理
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       410-CSV-DATA-EXT.
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
           STRING    ","          DELIMITED  BY  SIZE
                                  INTO  WRK-CSVDATA
                                  WITH  POINTER   IDXP
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
           STRING    X"0D"        DELIMITED  BY  SIZE
                                  INTO  WRK-CSVDATA
                                  WITH  POINTER   IDXP
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
                    OR  WRK-ITEM(IDX-LEN:)    =  SPACE
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
           INITIALIZE                     ORCSTOUKEICSVAREA
           MOVE    "INS"                  TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                          TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
           MOVE    ZERO                   TO  STOUKEICSV-PTID
           MOVE    WRK-PARA-NYUGAIKBN     TO  STOUKEICSV-NYUGAIKBN
           MOVE    "/var/tmp/"            TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM            DELIMITED  BY  SIZE
                   "raiinzaiin_"          DELIMITED  BY  SIZE
                   WRK-PARA-PRTYM(1:6)    DELIMITED  BY  SIZE
                   ".csv"                 DELIMITED  BY  SIZE
                                          INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                    TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA            TO  STOUKEICSV-CSVDATA
           MOVE    "来院・在院状況分析票" TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  WRK-CSVDATA
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT. 
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       800-ERR-HENSYU-SEC            SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT            RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"             TO  SJOBKANRI-MODE
             INITIALIZE                JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID    TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID  TO  JOB-SHELLID
             MOVE    WRK-RECEERR       TO  JOB-YOBI
             MOVE    "9999"            TO  JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1           TO  FLG-END
      *
           .
       800-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       700-SEIWA-HEN-SEC            SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       600-END-SEC            SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       600-END-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
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
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-SELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  910-SYSKANRI-FETCH-SEC
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO              TO  FLG-SYSKANRI
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-SYSKANRI-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴SELECT
      *****************************************************************
       900-PTNYUINRRK-SELECT-SEC         SECTION.
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PARA-PRTEDYMD   TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-PARA-PRTSTYMD   TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  900-PTNYUINRRK-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH
      *****************************************************************
       900-PTNYUINRRK-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  PTNYUINRRK-REC
             MOVE    ZERO              TO  FLG-PTNYUINRRK
      *
             INITIALIZE                  PTINF-REC
             MOVE    PTNYUINRRK-HOSPNUM  TO  PTINF-HOSPNUM
             MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
             PERFORM  910-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  900-PTNYUINRRK-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1                 TO  FLG-PTNYUINRRK
           END-IF
           .
       900-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴CLOSE
      *****************************************************************
       900-PTNYUINRRK-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTNYUINRRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為SELECT
      *****************************************************************
       900-NYUINACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-PRTYM      TO  NACCT-SRYYM
           MOVE    "2"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  900-NYUINACCT-FETCH-SEC
           ELSE
             MOVE    1                 TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為FETCH
      *****************************************************************
       900-NYUINACCT-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  NYUINACCT-REC
             MOVE    ZERO              TO  FLG-NYUINACCT
           ELSE
             MOVE    1                 TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為CLOSE
      *****************************************************************
       900-NYUINACCT-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-NYUINACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC           SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    WRK-PARA-PRTYM      TO  SRY-SRYYM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "key14"             TO  MCP-PATHNAME
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-SRYACT-FETCH-SEC
           ELSE
             MOVE    1                 TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  SRYACT-REC
             MOVE    ZERO              TO  FLG-SRYACT
      *
             INITIALIZE                  PTINF-REC
             MOVE    SRY-HOSPNUM       TO  PTINF-HOSPNUM
             MOVE    SRY-PTID          TO  PTINF-PTID
             PERFORM  910-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  900-SRYACT-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1                 TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索
      *****************************************************************
       900-PTINF-READ-SEC            SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID2           TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索
      *****************************************************************
       910-PTINF-READ-SEC            SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       900-JYURRK-READ-SEC            SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    ACCT-HOSPNUM        TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID           TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN      TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  JYURRK-SRYKA
           MOVE    "00000000"          TO  JYURRK-SRYYMD
           MOVE    ACCT-SRYYM          TO  JYURRK-UPSRYYMD
           MOVE    "00"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_jyurrk"      TO  MCP-TABLE
             MOVE    "key12"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
             ELSE
               MOVE    1               TO  FLG-JYURRK
             END-IF
           ELSE
             MOVE    1                 TO  FLG-JYURRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       910-JYURRK-READ-SEC            SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    ACCT-HOSPNUM        TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID           TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN      TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  JYURRK-SRYKA
           MOVE    ACCT-HKNCOMBI       TO  JYURRK-HKNCOMBINUM
           MOVE    ACCT-ZAINUM         TO  JYURRK-ZAINUM1
                                           JYURRK-ZAINUM2
                                           JYURRK-ZAINUM3
                                           JYURRK-ZAINUM4
                                           JYURRK-ZAINUM5
                                           JYURRK-ZAINUM6
                                           JYURRK-ZAINUM7
                                           JYURRK-ZAINUM8
                                           JYURRK-ZAINUM9
                                           JYURRK-ZAINUM10
                                           JYURRK-ZAINUM11
                                           JYURRK-ZAINUM12
                                           JYURRK-ZAINUM13
                                           JYURRK-ZAINUM14
                                           JYURRK-ZAINUM15
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_jyurrk"      TO  MCP-TABLE
             MOVE    "key20"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
             ELSE
               MOVE    1               TO  FLG-JYURRK
             END-IF
           ELSE
             MOVE    1                 TO  FLG-JYURRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       910-JYURRK-SELECT-SEC            SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTYM      TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    WRK-PARA-PRTEDYMD   TO  JYURRK-UPSRYYMD
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  910-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       910-JYURRK-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  JYURRK-REC
             MOVE    ZERO              TO  FLG-JYURRK
      *
             INITIALIZE                PTINF-REC
             MOVE    JYURRK-HOSPNUM    TO  PTINF-HOSPNUM
             MOVE    JYURRK-PTID       TO  PTINF-PTID
             PERFORM  910-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  910-JYURRK-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1                 TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       910-JYURRK-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-JYURRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報検索
      *****************************************************************
       900-SRYACCT-READ-SEC            SECTION.
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_sryacct"     TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
             ELSE
               MOVE    1               TO  FLG-SRYACCT
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC             SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTSTYMD    TO  SYU-SRYYMD(1:6)
           MOVE    WRK-PARA-PRTEDYMD    TO  SYU-UPSRYYMD
      *
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF        
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
               MOVE    MCPDATA-REC        TO  SYUNOU-REC
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF  
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       920-JYURRK-READ-SEC            SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SYU-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SYU-PTID            TO  JYURRK-PTID
           MOVE    SYU-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    "00000000"          TO  JYURRK-SRYYMD
           MOVE    SYU-SRYYMD          TO  JYURRK-UPSRYYMD
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_jyurrk"      TO  MCP-TABLE
             MOVE    "key14"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
             ELSE
               MOVE    1               TO  FLG-JYURRK
             END-IF
           ELSE
             MOVE    1                 TO  FLG-JYURRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       920-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       920-SRYKARRK-SELECT-SEC            SECTION.
      *
           INITIALIZE               SRYKARRK-REC
      *
           MOVE    SYU-HOSPNUM      TO   KARRK-HOSPNUM
           MOVE    SYU-PTID         TO   KARRK-PTID
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM  920-SRYKARRK-FETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       920-SRYKARRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       920-SRYKARRK-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"         TO   MCP-FUNC
           MOVE    "tbl_srykarrk"    TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO   SRYKARRK-REC 
             MOVE    ZERO            TO   FLG-SRYKARRK
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       920-SRYKARRKA-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       920-SRYKARRK-CLOSE-SEC            SECTION.
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBCLOSECURSOR"  TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       920-SRYKARRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC            SECTION.
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
      *****************************************************************
      *    ＤＢクローズ処理
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
