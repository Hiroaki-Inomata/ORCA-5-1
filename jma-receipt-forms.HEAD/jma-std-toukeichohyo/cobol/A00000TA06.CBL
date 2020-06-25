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
       PROGRAM-ID.             A00000TA06.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 請求収納チェック表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付         内容
      *   01.00.00    門間    2011/08/31   新規作成
      *   01.00.01    門間    2013/10/03   CSVファイルが20kBを超える場合、
      *                                    正しく出力されない不具合を修正
      *   02.00.01    門間    2014/10/31   ver4.8.0対応（一時ディレクトリ変更）
      *   02.00.02    吉川    2015/03/24   医療機関情報の適用期間を区切った場合
      *                                    帳票が出力されない不具合を修正
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    未収金対象患者一時ファイル
           SELECT  TA06-FILE       ASSIGN          TA06P
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TA06TMP-KEY
                                   FILE    STATUS  IS  STS-TA06TMP.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    未収金対象患者一時ファイル
       FD  TA06-FILE.
           COPY    "A00000TA06TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA06P//.
      *
           COPY    "A00000TA06.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-RECEERR             PIC X(02).
           03  STS-TA06TMP             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PRINT               PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-INVALID             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(02).
           03  CNT-RENNUM              PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
      *
      *    ＣＳＶ用
           03  IDX2                    PIC 9(03).
           03  IDX3                    PIC 9(03).
           03  IDX-ST                  PIC 9(03).
           03  IDX-ED                  PIC 9(03).
           03  IDXP                    PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-TABLEKBN       PIC X(01).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-HKNKBN         PIC X(01).
           03  WRK-PARA-YMKBN          PIC X(01).
           03  WRK-PARA-PRTYM          PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-Z2                  PIC Z9.
           03  WRK-Z6                  PIC ZZZZZ9.
           03  WRK-Z10                 PIC ZZ,ZZZ,ZZZ.
           03  WRK-SYMD                PIC 9(08).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-HENYMD              PIC X(09).
           03  WRK-DATA                PIC 9(04).
           03  WRK-SKYSTYMD            PIC 9(08).
           03  WRK-SKYEDYMD            PIC 9(08).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-TEISYUTUSAKI        PIC 9(01).
           03  WRK-TEISYUTUSAKI2       PIC 9(01).
           03  WRK-HKN                 PIC 9(01).
           03  WRK-NYUGAI              PIC 9(01).
           03  WRK-KEY                 PIC X(05).
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
               07  CSV-HEAD-RENNUM        PIC X(50) VALUE "連番".
               07  CSV-HEAD-PTNUM         PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-KANANAME      PIC X(50) VALUE "カナ".
               07  CSV-HEAD-NAME          PIC X(50) VALUE "氏名".
               07  CSV-HEAD-HKN           PIC X(50) VALUE "保険".
               07  CSV-HEAD-NYUGAI        PIC X(50) VALUE "入外".
               07  CSV-HEAD-SEIKYUTEN     PIC X(50) VALUE "請求点数".
               07  CSV-HEAD-SYUNOUTEN     PIC X(50) VALUE "収納点数".
               07  CSV-HEAD-BIKO          PIC X(50) VALUE "備考".
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 9.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 9.
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
      *    医師情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    請求管理テーブル
           COPY    "CPRCF010.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
           PERFORM  100-INIT-SEC
      *
           IF  FLG-END  NOT =  1
             PERFORM  300-MAIN-SEC
           END-IF
      *
           PERFORM  600-END-SEC
      *
           STOP  RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           INITIALIZE                    FLG-AREA
                                         STS-AREA
                                         WRK-AREA
                                         CNT-AREA
                                         SPA-AREA
                                         IDX-AREA
           INITIALIZE                    RECEERR
      *
           PERFORM  100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM      DELIMITED  BY  ","
                                         INTO  LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-YMKBN
                                               WRK-PARA-PRTYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HKNKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM          TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"                     TO  SJOBKANRI-MODE
           INITIALIZE                        JOBKANRI-REC
           MOVE    SPA-HOSPNUM               TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID            TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID          TO  JOB-SHELLID
           MOVE    "A00000TA06"                 TO  JOB-PGID
           MOVE    "請求収納チェック表（月次）"
                                             TO  JOB-SHELLMSG
           CALL    "ORCSJOB"                 USING
                                             ORCSJOBKANRIAREA
                                             JOBKANRI-REC
                                             SPA-AREA
      *+
      *    サブプログラムより一時ファイル名編集
           MOVE    "ATA06TP"                TO  TA06P-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA06P-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA06P-HOSPNUM
      *
           MOVE    TA06P-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA06P-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    対象年月初日
           MOVE    WRK-PARA-PRTYM            TO  WRK-SKYSTYMD(1:6)
           MOVE    "01"                      TO  WRK-SKYSTYMD(7:2)
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           PERFORM 700-LASTDAY-GET-SEC
           MOVE    WRK-SYMD                  TO  WRK-SKYEDYMD
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
           INITIALIZE                        SYS-1001-REC
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
           IF     WRK-PARA-NYUGAIKBN  NOT =  "1"
             AND  WRK-PARA-NYUGAIKBN  NOT =  "2"
             AND  WRK-PARA-NYUGAIKBN  NOT =  SPACE
             MOVE    "入外区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           IF  WRK-PARA-YMKBN  =  "1"
             MOVE    "key60"    TO  WRK-KEY
           ELSE
             MOVE    "key19"    TO  WRK-KEY
           END-IF
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
           OPEN    OUTPUT   TA06-FILE
           CLOSE   TA06-FILE
           OPEN    I-O      TA06-FILE
      *
      *    帳票編集処理
           PERFORM    400-BODY-SEC
      *
           CLOSE   TA06-FILE
      *
      *    印刷処理
           IF  WRK-DATA  NOT =  ZERO
      *      印字データ編集処理
             PERFORM  400-PRINT-HEN-SEC
           ELSE
             MOVE    "データがありません"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       300-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<請求管理・収納：明細>
      *****************************************************************
       400-BODY-SEC            SECTION.
      *
           EVALUATE  WRK-PARA-HKNKBN
           WHEN  "0"
             MOVE    1        TO    WRK-TEISYUTUSAKI
             PERFORM  400-SEIKYU-MAIN-SEC
             PERFORM  400-SYUNOU-MAIN-SEC
           WHEN  "1"
             MOVE    2        TO    WRK-TEISYUTUSAKI
             PERFORM  400-SEIKYU-MAIN-SEC
             PERFORM  400-SYUNOU-MAIN-SEC
           WHEN  "2"
             MOVE    6        TO    WRK-TEISYUTUSAKI
             PERFORM  400-SEIKYU-MAIN-SEC
             PERFORM  400-SYUNOU-MAIN-SEC
           WHEN  "3"
           WHEN  SPACE
             MOVE    1        TO    WRK-TEISYUTUSAKI
             PERFORM  400-SEIKYU-MAIN-SEC
             MOVE    2        TO    WRK-TEISYUTUSAKI
             PERFORM  400-SEIKYU-MAIN-SEC
             MOVE    6        TO    WRK-TEISYUTUSAKI
             PERFORM  400-SEIKYU-MAIN-SEC
             PERFORM  400-SYUNOU-MAIN-SEC
           END-EVALUATE
      *
           .
       400-BODY-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<請求管理：メイン>
      *****************************************************************
       400-SEIKYU-MAIN-SEC            SECTION.
      *
           PERFORM  900-SEIKYU-SELECT-SEC
           PERFORM  UNTIL  FLG-END   =   1
      *
             IF   ( RECE10-NYUGAIKBN     =  WRK-PARA-NYUGAIKBN
               OR   WRK-PARA-NYUGAIKBN   =  SPACE  )
               AND  RECE10-TEISYUTUSAKI  =  WRK-TEISYUTUSAKI
      *
               IF  WRK-PARA-YMKBN  =  "1"
      *          請求年月、診療年月
                 IF   ( WRK-PARA-PRTYM  =  RECE10-SRYYM
                   OR   WRK-PARA-PRTYM  =  RECE10-SKYYM )
                   PERFORM  400-SEIKYU-SUB-SEC
                 END-IF
               ELSE
      *          診療年月
                 IF  WRK-PARA-PRTYM  =  RECE10-SRYYM
                   PERFORM  400-SEIKYU-SUB-SEC
                 END-IF
               END-IF
      *
             END-IF
             PERFORM  900-SEIKYU-READ-SEC
           END-PERFORM
           PERFORM  900-SEIKYU-CLOSE-SEC
      *
           .
       400-SEIKYU-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<請求管理：サブ>
      *****************************************************************
       400-SEIKYU-SUB-SEC            SECTION.
      *
           INITIALIZE    TA06TMP-REC
      *
      *    入外区分
           MOVE    RECE10-NYUGAIKBN     TO  TA06TMP-NYUGAIKBN
      *    審査支払機関（1：社保、2：国保、6：後期）
           IF  RECE10-HKNNUM  NOT =  SPACE
             MOVE    RECE10-TEISYUTUSAKI  TO  TA06TMP-HKN
           ELSE
      *    公費単独の場合
             MOVE    "9"                  TO  TA06TMP-HKN
           END-IF
      *    患者番号
           MOVE    RECE10-PTNUM         TO  TA06TMP-PTNUM
      *    カナ
           MOVE    PTINF-KANANAME       TO  TA06TMP-KANA-NAME
      *    氏名
           MOVE    PTINF-NAME           TO  TA06TMP-NAME
      *    医療機関識別番号
           MOVE    RECE10-HOSPNUM       TO  TA06TMP-HOSPNUM
      *    患者ＩＤ
           MOVE    RECE10-PTID          TO  TA06TMP-PTID
      *    診療年月
           MOVE    RECE10-SRYYM         TO  TA06TMP-SRYYM
      *    請求年月
           MOVE    RECE10-SKYYM         TO  TA06TMP-SKYYM
      *
      *    一時データ読み込み
           PERFORM  700-TMP-READ-SEC
      *
      *    請求点数加算
           IF  TA06TMP-HKN  NOT =  "9"
             ADD     RECE10-TOTALTEN(1)   TO  TA06TMP-SKY-TEN
           ELSE
             ADD     RECE10-TOTALTEN(2)   TO  TA06TMP-SKY-TEN
             ADD     RECE10-TOTALTEN(3)   TO  TA06TMP-SKY-TEN
             ADD     RECE10-TOTALTEN(4)   TO  TA06TMP-SKY-TEN
             ADD     RECE10-TOTALTEN(5)   TO  TA06TMP-SKY-TEN
           END-IF
      *
      *    データ書き込み
           IF  FLG-INVALID  =  1
             WRITE    TA06TMP-REC
           ELSE
             REWRITE  TA06TMP-REC
           END-IF
      *
           .
       400-SEIKYU-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<収納：メイン>
      *****************************************************************
       400-SYUNOU-MAIN-SEC            SECTION.
      *
           OPEN    OUTPUT   TA06-FILE
           CLOSE   TA06-FILE
           OPEN    I-O      TA06-FILE
      *
           INITIALIZE          TA06TMP-REC
      *
      *    一時ファイル読込
           PERFORM  710-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
             PERFORM  400-SYUNOU-SUB-SEC
      *      一時ファイル読込
             PERFORM  710-TMP-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   TA06-FILE
      *
           .
       400-SYUNOU-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<収納：サブ>
      *****************************************************************
       400-SYUNOU-SUB-SEC            SECTION.
      *
      *    収納点数を合計する
           PERFORM  900-SYUNOU-SELECT-SEC
           PERFORM  UNTIL  FLG-SYUNOU   =   1
      *
             PERFORM  500-HKNSET-SEC
      *
      *      収納点数加算
             IF  TA06TMP-HKN  =  WRK-TEISYUTUSAKI2
               ADD    SYU-TOTAL-HKNTEN      TO  TA06TMP-SYU-TEN
             END-IF
      *
             PERFORM  900-SYUNOU-READ-SEC
           END-PERFORM
           PERFORM  900-SYUNOU-CLOSE-SEC
      *
           IF  WRK-PARA-YMKBN  =  "1"
             IF   ( TA06TMP-SRYYM  <  TA06TMP-SKYYM
               AND  "99999999"    NOT =  TA06TMP-SKYYM )
      *        請求月日指定の場合、診療月日を備考へ
               MOVE    TA06TMP-SRYYM        TO  WRK-SYMD(1:6)
               MOVE    "01"                 TO  WRK-SYMD(7:2)
               PERFORM  700-SEIWA-HEN-SEC
               MOVE    "診療年月  "         TO  TA06TMP-BIKO(1:10)
               MOVE    WRK-HENYMDG(1:16)    TO  TA06TMP-BIKO(11:16)
             END-IF
           END-IF
      *
      *    データ書き込み
           REWRITE  TA06TMP-REC
      *
           IF  TA06TMP-SKY-TEN  NOT =  TA06TMP-SYU-TEN
             ADD     1                  TO  WRK-DATA
           END-IF
      *
           .
       400-SYUNOU-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険区分編集
      *****************************************************************
       500-HKNSET-SEC                SECTION.
      *
           EVALUATE  SYU-SYUHKNNUM
             WHEN  "001" THRU "034"
             WHEN  "063"
             WHEN  "072" THRU "075"
               MOVE    1                 TO  WRK-TEISYUTUSAKI2
             WHEN  "060"
             WHEN  "067" THRU "069"
               MOVE    2                 TO  WRK-TEISYUTUSAKI2
             WHEN  "039" THRU "040"
               MOVE    6                 TO  WRK-TEISYUTUSAKI2
             WHEN  SPACE
               MOVE    9                 TO  WRK-TEISYUTUSAKI2
           END-EVALUATE
      *
           .
       500-HKNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       400-PRINT-HEN-SEC            SECTION.
      *
           OPEN    INPUT       TA06-FILE
           MOVE    ZERO        TO  FLG-PRINT
      *
           INITIALIZE          TA06TMP-REC
           INITIALIZE          TA06-REC
      *
           MOVE    ZERO        TO  CNT-RENNUM
           MOVE    ZERO        TO  IDX
      *
      *    CSVヘッダ編集処理
           PERFORM  800-CSV-HEAD-SEC
      *
      *    一時ファイル読込
           PERFORM  710-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
             IF  TA06TMP-SKY-TEN  NOT =  TA06TMP-SYU-TEN
      *
               ADD     1                      TO  IDX
      *
      *        連番
               ADD     1                      TO  CNT-RENNUM
               MOVE    CNT-RENNUM             TO  WRK-Z6
               MOVE    WRK-Z6                 TO  TA06-RENNUM(IDX)
      *        患者番号
               IF  TA06TMP-PTNUM(11:1)  =  SPACE
      *          10桁
                 MOVE    TA06TMP-PTNUM     TO  TA06-PTNUM(IDX)
               ELSE
      *          20桁
                 MOVE    TA06TMP-PTNUM(1:10)
                                              TO  TA06-PTNUM2-1(IDX)
                 MOVE    TA06TMP-PTNUM(11:10)
                                              TO  TA06-PTNUM2-2(IDX)
               END-IF
      *        カナ
               MOVE    TA06TMP-KANA-NAME   TO  TA06-KANA-NAME(IDX)
      *        氏名
               MOVE    TA06TMP-NAME        TO  TA06-NAME(IDX)
      *        保険
               EVALUATE  TA06TMP-HKN
               WHEN  "1"
                 MOVE    "社保"               TO  TA06-HKN(IDX)
               WHEN  "2"
                 MOVE    "国保"               TO  TA06-HKN(IDX)
               WHEN  "6"
                 MOVE    "後期"               TO  TA06-HKN(IDX)
               WHEN  "9"
                 MOVE    "公単"               TO  TA06-HKN(IDX)
               END-EVALUATE
      *        入院／外来
               IF  TA06TMP-NYUGAIKBN  =  "1"
                 MOVE    "入院"               TO  TA06-NYUGAI(IDX)
               ELSE
                 MOVE    "外来"               TO  TA06-NYUGAI(IDX)
               END-IF
      *        請求点数
               MOVE    TA06TMP-SKY-TEN     TO  WRK-Z10
               MOVE    WRK-Z10                TO  TA06-SKY-TEN(IDX)
      *        収納点数
               MOVE    TA06TMP-SYU-TEN     TO  WRK-Z10
               MOVE    WRK-Z10                TO  TA06-SYU-TEN(IDX)
      *        備考
               MOVE    TA06TMP-BIKO        TO  TA06-BIKO(IDX)
      *
               PERFORM  400-CSV-DATA-SEC
             END-IF
      *
      *      一時ファイル読込
             PERFORM  710-TMP-READ-SEC
      *
             IF    IDX        =  24
               OR  FLG-PRINT  =  1
               MOVE    ZERO        TO  IDX
      *        印刷処理
               PERFORM  500-PRINT-OUT-SEC
             END-IF
           END-PERFORM
      *
           CLOSE   TA06-FILE
      *
           .
       400-PRINT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       400-HEAD-HEN-SEC            SECTION.
      *
      *    対象年月初日取得
           IF  WRK-PARA-YMKBN  =  "1"
             MOVE    "請求年月："           TO  TA06-SRYYM(1:10)
           ELSE
             MOVE    "診療年月："           TO  TA06-SRYYM(1:10)
           END-IF
      *
           MOVE    WRK-SKYSTYMD           TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)      TO  TA06-SRYYM(11:16)
      *
      *    作成年月日編集
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  TA06-CREATEYMD
      *
      *    頁数編集
           ADD     1                      TO  CNT-PAGE
           MOVE    CNT-PAGE               TO  WRK-Z2
           MOVE    WRK-Z2                 TO  TA06-PAGE
      *
           .
       400-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       500-PRINT-OUT-SEC            SECTION.
      *
      *    ヘッダー編集処理
           PERFORM  400-HEAD-HEN-SEC
      *
      *    印刷処理
           INITIALIZE                         ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE    "A00000TA06.red"              TO  SPRT-PRTID
           MOVE    "請求収納チェック表（月次）"
                                              TO  SPRT-TITLE
           MOVE    TA06-REC                   TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
                                              SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                      TA06-REC
      *
           .
       500-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1               TO  IDXP
      *
      *    対象年月
           INITIALIZE  WRK-ITEM
           IF  WRK-PARA-YMKBN  =  "1"
             MOVE    "請求年月："           TO  WRK-ITEM(1:10)
           ELSE
             MOVE    "診療年月："           TO  WRK-ITEM(1:10)
           END-IF
      *
           MOVE    WRK-SKYSTYMD           TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)      TO  WRK-ITEM(11:16)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称セット
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL  IDX2  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX2)     TO  WRK-ITEM
             IF   IDX2  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           MOVE    1               TO  IDXP
      *
      *    連番
           MOVE    TA06-RENNUM(IDX)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    TA06-PTNUM(IDX)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    カナ
           MOVE    TA06-KANA-NAME(IDX)  TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名
           MOVE    TA06-NAME(IDX)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険
           MOVE    TA06-HKN(IDX)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入院／外来
           MOVE    TA06-NYUGAI(IDX)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    請求点数
           MOVE    TA06TMP-SKY-TEN      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    収納点数
           MOVE    TA06TMP-SYU-TEN      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    備考
           MOVE    TA06-BIKO(IDX)       TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
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
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL  IDX3  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX3:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX3:1)  NOT =  SPACE
                 MOVE    IDX3      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
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
           INITIALIZE                         ORCSTOUKEICSVAREA
           MOVE    "INS"                      TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM        TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID       TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM         TO  STOUKEICSV-SRYYM
           MOVE    ZERO                       TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"                TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM                DELIMITED  BY  SIZE
                   "seikyusyunou_check_"      DELIMITED  BY  SIZE
                   WRK-PARA-PRTYM             DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "請求収納チェック表（月次）"
                                              TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
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
           INITIALIZE                         WRK-CSVDATA
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       800-ERR-HENSYU-SEC            SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT             RECEERR-FILE
      *
             MOVE    WRK-RECEERR        TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                 JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
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
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       700-SEIWA-HEN-SEC            SECTION.
      *
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY2-AREA
           MOVE    "21"                 TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"            USING
                                        STS-AREA-DAY
                                        LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3     TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1     TO  WRK-HENYMD
           INSPECT WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       700-LASTDAY-GET-SEC          SECTION.
      *
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY7-AREA
           MOVE    "71"                 TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-PRTYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"            USING
                                        STS-AREA-DAY
                                        LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI      TO  WRK-SYMD
           .
       700-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       600-END-SEC            SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"                TO  SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE             TO  JOB-UPDCNT
           CALL    "ORCSJOB"            USING
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
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"        TO  MCP-FUNC
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    ZERO           TO  FLG-SYSKANRI
             ELSE
               MOVE    1              TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-SELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM  910-SYSKANRI-FETCH-SEC 
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           INITIALIZE                   RECE10-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO   RECE10-HOSPNUM
           IF  WRK-PARA-YMKBN  =  "1"
             MOVE    WRK-PARA-PRTYM       TO   RECE10-SKYYM
             MOVE    WRK-TEISYUTUSAKI     TO   RECE10-TEISYUTUSAKI
           ELSE
             MOVE    WRK-PARA-PRTYM       TO   RECE10-SRYYM
             MOVE    WRK-PARA-NYUGAIKBN   TO   RECE10-NYUGAIKBN
           END-IF
      *
           MOVE    RECE10-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_seikyu"         TO   MCP-TABLE
           MOVE    WRK-KEY              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                 TO   FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
           MOVE    "tbl_seikyu"         TO   MCP-TABLE
           MOVE    WRK-KEY              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC    =   ZERO
             MOVE    ZERO                 TO   FLG-END
             MOVE    MCPDATA-REC          TO   RECE10-REC
      *
      *    テスト患者のときは読み飛ばす
             MOVE    RECE10-HOSPNUM       TO  WRK-PARA-HOSPNUM
             MOVE    RECE10-PTID          TO  WRK-PTID
             PERFORM  920-PTINF-READ-SEC
             IF      FLG-PTINF           =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-SEIKYU-READ-SEC
             END-IF
           ELSE
             MOVE    1                    TO   FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理情報クローズ処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_seikyu"         TO   MCP-TABLE
           MOVE    WRK-KEY              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索(SELECT)
      *****************************************************************
       900-SYUNOU-SELECT-SEC             SECTION.
      *
           INITIALIZE                    SYUNOU-REC
      *
           MOVE    TA06TMP-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    TA06TMP-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    TA06TMP-PTID       TO  SYU-PTID
           MOVE    TA06TMP-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE    "%%"                  TO  SYU-SRYYMD(7:2)
      *
           MOVE    SYUNOU-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key37"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             PERFORM  900-SYUNOU-READ-SEC
           ELSE
             MOVE    1                     TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    収納情報検索(FETCH)
      *****************************************************************
       900-SYUNOU-READ-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    ZERO                TO  FLG-SYUNOU
             MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
             MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納情報検索(FETCH)
      *****************************************************************
       900-SYUNOU-CLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       920-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                    PTINF-REC
           MOVE    WRK-PARA-HOSPNUM      TO   PTINF-HOSPNUM
           MOVE    WRK-PTID              TO   PTINF-PTID
           MOVE    PTINF-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptinf"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTINF-REC
               MOVE    ZERO              TO   FLG-PTINF
             ELSE
               MOVE    1                 TO   FLG-PTINF
               INITIALIZE                PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(KEY READ)
      *****************************************************************
       700-TMP-READ-SEC              SECTION.
      *
           INITIALIZE                FLG-INVALID
           READ   TA06-FILE
               INVALID    KEY
                 MOVE    1           TO  FLG-INVALID
               NOT  INVALID  KEY
                 MOVE    ZERO        TO  FLG-INVALID
           END-READ
           .
       700-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(NEXT READ)
      *****************************************************************
       710-TMP-READ-SEC              SECTION.
      *
           READ    TA06-FILE  NEXT
               AT  END
                 MOVE    1           TO  FLG-PRINT
               NOT  AT END
                 MOVE    ZERO        TO  FLG-PRINT
           END-READ
           .
       710-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC            SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
