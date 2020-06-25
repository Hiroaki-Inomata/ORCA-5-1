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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000YL01.
      ******************************************************************
      *  システム名          : ＯＲＣＡ
      *  サブシステム名      : 日次帳票
      *  コンポーネント名    : 医薬品名・一般名対応リスト
      *  管理者              : 
      *  Maj/Min/Rev  修正者   日付       内容
      *   01.00.01    門間     12/05/01   新規作成
      *   01.00.02    門間     12/05/18   薬価基準コード１２桁対応
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE     ASSIGN  RECEERR
                                    FILE    STATUS  IS  STS-RECEERR.
      *
      *    一時ファイル
           SELECT  A00000YL01-FILE  ASSIGN          AYL01T
                                    ORGANIZATION    IS  INDEXED
                                    ACCESS  MODE    IS  DYNAMIC
                                    RECORD  KEY     IS  YL01TMP-KEY
                                    FILE    STATUS  IS  STS-YL01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    一時ファイル
       FD  A00000YL01-FILE.
           COPY    "A00000YL01TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE         SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    一時ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //AYL01T//.
      *
           COPY    "A00000YL01.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-RECEERR             PIC X(02).
           03  STS-YL01                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
           03  FLG-TENSUP              PIC 9(01).
           03  FLG-GENERIC             PIC 9(01).
           03  FLG-GENERICP            PIC 9(01).
           03  FLG-PRINT               PIC 9(01).
           03  FLG-KPAGE               PIC 9(01).
           03  FLG-W-INP               PIC 9(01).
           03  FLG-W-IPN               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(04).
           03  CNT-GPAGE               PIC 9(04)  OCCURS 4.
           03  CNT-AMARI               PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDX-YKZ1                PIC 9(01).
           03  IDX-YKZ2                PIC 9(01).
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
      *
           03  WRK-PARA-KEY1           PIC 9(01).
           03  WRK-PARA-KEY2           PIC 9(01).
           03  WRK-PARA-KEY3           PIC 9(01).
           03  WRK-PARA-KEY4           PIC 9(01).
           03  WRK-PARA-KEY5           PIC 9(01).
           03  WRK-PARA-YKZKBN         PIC X(01).
           03  WRK-PARA-INPUTCD        PIC X(01).
           03  WRK-PARA-IPNKBN         PIC X(01).
           03  WRK-PARA-KPAGE          PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-Z4                  PIC ZZZZ.
           03  WRK-Z9                  PIC ZZZZZZZZZ.
           03  WRK-YAKKA               PIC ZZZZZ9.99.
           03  WRK-SYMD                PIC 9(08).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-HENYMD              PIC X(09).
           03  WRK-DATA                PIC 9(05)  OCCURS 4.
      *
           03  WRK-YKZ-KEY             PIC X(01)  OCCURS 3.
           03  WRK-YKZKBN              PIC X(02).
      *
           03  WRK-KEY-TBL             OCCURS 5.
             05  WRK-KEY                 PIC X(20).
           03  WRK-TITLE-TBL           OCCURS 2.
             05  WRK-IYHKBN              PIC X(24).
             05  WRK-INPUTCD             PIC X(24).
             05  WRK-IPNKBN              PIC X(24).
             05  WRK-KPAGE               PIC X(24).
      *
      *    一時領域：ヘッダー
       01  WRK-HEAD-AREA.
           02  WRK-HEAD-TITLE1         PIC X(90).
           02  WRK-HEAD-TITLE2         PIC X(100).
           02  WRK-HEAD-PRTYMD         PIC X(22).
           02  WRK-HEAD-GPAGE          PIC X(4).
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
               07  CSV-HEAD-SRYCD        PIC X(50)  VALUE "診療行為CD".
               07  CSV-HEAD-INPUTCD      PIC X(50)  VALUE "入力CD".
               07  CSV-HEAD-YKZKBN       PIC X(50)  VALUE "区分".
               07  CSV-HEAD-KISAIKBN     PIC X(50)  VALUE "【般】".
               07  CSV-HEAD-NAME         PIC X(50)  VALUE "医薬品名".
               07  CSV-HEAD-GNAME        PIC X(50)  VALUE "一般名".
               07  CSV-HEAD-YAKKA        PIC X(50)  VALUE "薬価".
               07  CSV-HEAD-SAIYAKKA     PIC X(50)  VALUE "最低薬価".
             05  CSV-HEAD-R            REDEFINES  CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM         PIC X(50)  OCCURS 8.
             05  CSV-HEAD-MAX          PIC 9(05)  VALUE 8.
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
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入力マスタ
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    点数付加マスタ
       01  TENSUP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    一般名マスタ
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    最低薬価マスタ
       01  GENERICPRICE-REC.
           COPY    "CPGENERICPRICE.INC".
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
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER            PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
           USING
           COMMAND-PARAM.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           IF  FLG-TENSU  =  ZERO
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
       100-INIT-SEC                      SECTION.
      *
           INITIALIZE                    FLG-AREA
                                         STS-AREA
                                         WRK-AREA
                                         CNT-AREA
                                         SPA-AREA
                                         IDX-AREA
                                         WRK-HEAD-AREA
                                         RECEERR
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
                                               WRK-PARA-KEY1
                                               WRK-PARA-KEY2
                                               WRK-PARA-KEY3
                                               WRK-PARA-KEY4
                                               WRK-PARA-KEY5
                                               WRK-PARA-YKZKBN
                                               WRK-PARA-INPUTCD
                                               WRK-PARA-IPNKBN
                                               WRK-PARA-KPAGE
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM          TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD       TO  SPA-SRYYMD
      *
      *    ステップ管理開始処理
           MOVE    "STS"                     TO  SJOBKANRI-MODE
           INITIALIZE                        JOBKANRI-REC
           MOVE    SPA-HOSPNUM               TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID            TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID          TO  JOB-SHELLID
           MOVE    "A00000YL01"              TO  JOB-PGID
           MOVE    "医薬品名・一般名対応リスト"
                                             TO  JOB-SHELLMSG
           CALL    "ORCSJOB"                 USING
                                             ORCSJOBKANRIAREA
                                             JOBKANRI-REC
                                             SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AYL01TP"                TO  AYL01T-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  AYL01T-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  AYL01T-HOSPNUM
      *
           MOVE    AYL01T-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  AYL01T-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM  200-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       200-PARA-HENSYU-SEC               SECTION.
      *
      *    医療機関ＩＤ編集
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
      *    パラメータ１：項目１（入力コード）
           IF  ( WRK-PARA-KEY1  NOT =  1  AND  2  AND  3  AND
                                       4  AND  5  AND  ZERO )
             MOVE    "項目１（入力コード）の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    空白はスルー
           IF  WRK-PARA-KEY1  NOT =  ZERO
      *      パラメータ２、３，４、５と重複している場合はエラーを表示
             EVALUATE  WRK-PARA-KEY1
               WHEN  WRK-PARA-KEY2
                 STRING  "項目１（入力コード）と項目２（薬剤区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY3
                 STRING  "項目１（入力コード）と項目３（一般名区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY4
                 STRING  "項目１（入力コード）と項目４（医薬品名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY5
                 STRING  "項目１（入力コード）と項目５（一般名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
             END-EVALUATE
           END-IF
      *
      *    パラメータ２：項目２（薬剤区分）
           IF  ( WRK-PARA-KEY2  NOT =  1  AND  2  AND  3  AND
                                       4  AND  5  AND  ZERO )
             MOVE    "項目２（薬剤区分）の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    空白はスルー
           IF  WRK-PARA-KEY2  NOT =  ZERO
      *      パラメータ１、３，４、５と重複している場合はエラーを表示
             EVALUATE  WRK-PARA-KEY2
               WHEN  WRK-PARA-KEY1
                 STRING  "項目２（薬剤区分）と項目１（入力コード）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY3
                 STRING  "項目２（薬剤区分）と項目３（一般名区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY4
                 STRING  "項目２（薬剤区分）と項目４（医薬品名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY5
                 STRING  "項目２（薬剤区分）と項目５（一般名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
             END-EVALUATE
           END-IF
      *
      *    パラメータ３：項目３（一般名区分）
           IF  ( WRK-PARA-KEY3  NOT =  1  AND  2  AND  3  AND
                                       4  AND  5  AND  ZERO )
             MOVE    "項目３（一般名区分）の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    空白はスルー
           IF  WRK-PARA-KEY3  NOT =  ZERO
      *      パラメータ１、２，４、５と重複している場合はエラーを表示
             EVALUATE  WRK-PARA-KEY3
               WHEN  WRK-PARA-KEY1
                 STRING  "項目３（一般名区分）と項目１（入力コード）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY2
                 STRING  "項目３（一般名区分）と項目２（薬剤区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY4
                 STRING  "項目３（一般名区分）と項目４（医薬品名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY5
                 STRING  "項目３（一般名区分）と項目５（一般名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
             END-EVALUATE
           END-IF
      *
      *    パラメータ４：項目４（医薬品名）
           IF  ( WRK-PARA-KEY4  NOT =  1  AND  2  AND  3  AND
                                       4  AND  5  AND  ZERO )
             MOVE    "項目４（医薬品名）の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    空白はスルー
           IF  WRK-PARA-KEY4  NOT =  ZERO
      *      パラメータ１、２，３、５と重複している場合はエラーを表示
             EVALUATE  WRK-PARA-KEY4
               WHEN  WRK-PARA-KEY1
                 STRING  "項目４（医薬品名）と項目１（入力コード）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY2
                 STRING  "項目４（医薬品名）と項目２（薬剤区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY3
                 STRING  "項目４（医薬品名）と項目３（一般名区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY5
                 STRING  "項目４（医薬品名）と項目５（一般名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
             END-EVALUATE
           END-IF
      *
      *    パラメータ５：項目５（一般名）
           IF  ( WRK-PARA-KEY5  NOT =  1  AND  2  AND  3  AND
                                       4  AND  5  AND  ZERO )
             MOVE    "項目５（一般名）の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    空白はスルー
           IF  WRK-PARA-KEY5  NOT =  ZERO
      *      パラメータ１、２，３、４と重複している場合はエラーを表示
             EVALUATE  WRK-PARA-KEY5
               WHEN  WRK-PARA-KEY1
                 STRING  "項目５（一般名）と項目１（入力コード）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY2
                 STRING  "項目５（一般名）と項目２（薬剤区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY3
                 STRING  "項目５（一般名）と項目３（一般名区分）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
               WHEN  WRK-PARA-KEY4
                 STRING  "項目５（一般名）と項目４（医薬品名）"
                                                DELIMITED  BY  SIZE
                         "の値が重複しています"
                                                DELIMITED  BY  SIZE
                                                INTO  WRK-RECEERR
                 END-STRING
                 PERFORM  800-ERR-HENSYU-SEC
             END-EVALUATE
           END-IF
      *
      *    パラメータ６：薬剤区分
           IF  ( WRK-PARA-YKZKBN  NOT =  "0" AND "1" AND "2" AND "3" )
             MOVE    "薬剤区分の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    パラメータ７：入力コード
           IF  ( WRK-PARA-INPUTCD   NOT =  "0" AND "1" AND "2" )
             MOVE    "入力コードの値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    パラメータ８：一般名区分
           IF  ( WRK-PARA-IPNKBN  NOT =  "0" AND "1" AND "2" AND
                                         "3" AND "4" )
             MOVE    "一般名区分の値が間違っています"
                                                TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    パラメータ９：改頁区分
           IF  WRK-PARA-KEY2  =  1
             IF  ( WRK-PARA-KPAGE   NOT = "0" AND "1" )
               MOVE    "改頁区分の値が間違っています"
                                                TO  WRK-RECEERR
               PERFORM  800-ERR-HENSYU-SEC
             END-IF
           END-IF
      *
      *    薬剤区分集計用
           EVALUATE  WRK-PARA-YKZKBN
             WHEN  "0"
               MOVE    3           TO  IDX-YKZ1
               MOVE    "1"         TO  WRK-YKZ-KEY(1)
               MOVE    "6"         TO  WRK-YKZ-KEY(3)
               MOVE    "4"         TO  WRK-YKZ-KEY(2)
             WHEN  "1"
               MOVE    1           TO  IDX-YKZ1
               MOVE    "1"         TO  WRK-YKZ-KEY(1)
             WHEN  "2"
               MOVE    1           TO  IDX-YKZ1
               MOVE    "6"         TO  WRK-YKZ-KEY(1)
             WHEN  "3"
               MOVE    1           TO  IDX-YKZ1
               MOVE    "4"         TO  WRK-YKZ-KEY(1)
           END-EVALUATE
      *
      *    改頁フラグ
           IF  ( WRK-PARA-KEY2   =  1  AND  WRK-PARA-KPAGE  =  1 )
             MOVE    1           TO  FLG-KPAGE
           END-IF
      *
           .
       200-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       300-MAIN-SEC            SECTION.
      *
           OPEN   OUTPUT  A00000YL01-FILE
           CLOSE  A00000YL01-FILE
           OPEN   I-O     A00000YL01-FILE
      *
      *    帳票データ  集計処理
           PERFORM  400-BODY-SEC
      *
           CLOSE  A00000YL01-FILE
      *
      *    印刷処理
           IF  WRK-DATA(1)  NOT =  ZERO
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
      *****************************************************************
      *    帳票データ  集計処理  <点数マスタ読込>
      *****************************************************************
       400-BODY-SEC            SECTION.
      *
      *    薬剤区分にて設定した薬剤を集計
           PERFORM  VARYING  IDX-YKZ2  FROM  1  BY  1
                    UNTIL    IDX-YKZ2  >  IDX-YKZ1
      *
             PERFORM  900-TENSU-SELECT-SEC
             PERFORM  UNTIL  FLG-TENSU  =  1
      *
               PERFORM  400-BODY-SUB-SEC
      *
               PERFORM  900-TENSU-READ-SEC
             END-PERFORM
             PERFORM  900-TENSU-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       400-BODY-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ  集計処理
      *****************************************************************
       400-BODY-SUB-SEC                 SECTION.
      *
           INITIALIZE  YL01TMP
      *
      *    優先順編集
           PERFORM  410-ORDER-SEC
      *
      *    診療行為コード
           MOVE    TNS-SRYCD            TO  YL01TMP-SRYCD
      *
      *    入力コード
           MOVE    ICD-INPUTCD          TO  YL01TMP-INPUTCD
      *
      *    薬剤区分
           EVALUATE  TNS-YKZKBN
             WHEN  1
               MOVE    "内"                 TO  YL01TMP-YKZKBN
             WHEN  6
               MOVE    "外"                 TO  YL01TMP-YKZKBN
             WHEN  4
               MOVE    "注"                 TO  YL01TMP-YKZKBN
           END-EVALUATE
      *
      *    一般名区分
           EVALUATE  TNSP-IPN-KISAIKBN
      *      0：記載しない
             WHEN  ZERO
               MOVE    "銘"                 TO  YL01TMP-KISAIKBN
               MOVE    GENERIC-GENERICNAME  TO  YL01TMP-GNAME
      *      1：一般名を記載する
             WHEN  1
               MOVE    "般"                 TO  YL01TMP-KISAIKBN
               MOVE    GENERIC-GENERICNAME  TO  YL01TMP-GNAME
      *      2：処方名称を記載する
      *      3：処方名称（一般名）を記載する
             WHEN  2
             WHEN  3
               MOVE    "処"                 TO  YL01TMP-KISAIKBN
               MOVE    TNSP-SHONAME         TO  YL01TMP-GNAME
           END-EVALUATE
      *
      *    医薬品名（漢字）
           MOVE    TNS-NAME             TO  YL01TMP-NAME
      *
      *    薬価
           MOVE    TNS-TEN              TO  WRK-YAKKA
           MOVE    WRK-YAKKA            TO  YL01TMP-YAKKA
      *
      *    最低薬価
           PERFORM  910-GENERICPRICE-READ-SEC
           MOVE    GENERICPRICE-PRICE   TO  WRK-YAKKA
           MOVE    WRK-YAKKA            TO  YL01TMP-SAIYAKKA
      *
      *    データ書込み判定
           PERFORM  420-BODY-WRITE-SEC
      *
           .
       400-BODY-SUB-EXT.
           EXIT.
      *****************************************************************
      *    優先順編集
      *****************************************************************
       410-ORDER-SEC                SECTION.
      *
      *    入力コード
           PERFORM  910-INPUTCD-READ-SEC
           IF  WRK-PARA-KEY1  NOT =  ZERO
             MOVE    ICD-INPUTCD     TO   YL01TMP-KEY-D(WRK-PARA-KEY1)
           END-IF
      *
      *    薬剤区分
           IF  WRK-PARA-KEY2  NOT =  ZERO
             EVALUATE  TNS-YKZKBN
               WHEN  1
                 MOVE    "1"         TO  YL01TMP-KEY-D(WRK-PARA-KEY2)
               WHEN  6
                 MOVE    "2"         TO  YL01TMP-KEY-D(WRK-PARA-KEY2)
               WHEN  4
                 MOVE    "3"         TO  YL01TMP-KEY-D(WRK-PARA-KEY2)
             END-EVALUATE
           END-IF
      *
      *    一般名区分
           PERFORM  910-TENSUPLUS-READ-SEC
           IF  WRK-PARA-KEY3  NOT =  ZERO
             MOVE    TNSP-IPN-KISAIKBN
                                     TO   YL01TMP-KEY-D(WRK-PARA-KEY3)
           END-IF
      *
      *    医薬品名（カナ）
           IF  WRK-PARA-KEY4  NOT =  ZERO
             MOVE    TNS-KANANAME    TO   YL01TMP-KEY-D(WRK-PARA-KEY4)
           END-IF
      *
      *    一般名
           PERFORM  910-GENERICNAME-READ-SEC
           IF  WRK-PARA-KEY5  NOT =  ZERO
             MOVE    GENERIC-GENERICNAME
                                     TO   YL01TMP-KEY-D(WRK-PARA-KEY5)
           END-IF
      *
           .
       410-ORDER-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ  書込処理
      *****************************************************************
       420-BODY-WRITE-SEC            SECTION.
      *
      *    入力コード：書込み判定
           EVALUATE  WRK-PARA-INPUTCD
             WHEN  "0"
               IF  ICD-INPUTCD  NOT =  SPACE
                 MOVE    1    TO  FLG-W-INP
               END-IF
             WHEN  "1"
               IF  ICD-INPUTCD  =  SPACE
                 MOVE    1    TO  FLG-W-INP
               END-IF
             WHEN  "2"
                 MOVE    1    TO  FLG-W-INP
           END-EVALUATE
      *
      *    一般名区分：書込み判定
           EVALUATE  WRK-PARA-IPNKBN
             WHEN  "0"
                 MOVE    1    TO  FLG-W-IPN
             WHEN  "1"
               IF  TNSP-IPN-KISAIKBN  =  ZERO
                 MOVE    1    TO  FLG-W-IPN
               END-IF
             WHEN  "2"
               IF  TNSP-IPN-KISAIKBN  =  1
                 MOVE    1    TO  FLG-W-IPN
               END-IF
             WHEN  "3"
             WHEN  "4"
               IF  ( TNSP-IPN-KISAIKBN  =  2  OR  3 )
                 MOVE    1    TO  FLG-W-IPN
               END-IF
           END-EVALUATE
      *
      *    データ書込み判定
           IF  ( FLG-W-INP  =  1  AND  FLG-W-IPN  =  1 )
      *      データ書き込み
             WRITE  YL01TMP
      *      件数をカウント
             ADD    1  TO  WRK-DATA(1)
      *      改頁フラグがONの場合は、薬剤区分毎の件数をカウント
             IF  FLG-KPAGE  =  1
               EVALUATE  TNS-YKZKBN
                 WHEN  1
                   ADD    1  TO  WRK-DATA(2)
                 WHEN  6
                   ADD    1  TO  WRK-DATA(3)
                 WHEN  4
                   ADD    1  TO  WRK-DATA(4)
               END-EVALUATE
             END-IF
           END-IF
      *
      *    判定フラグ初期化
           INITIALIZE  FLG-W-INP
                       FLG-W-IPN
      *
           .
       420-BODY-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       400-PRINT-HEN-SEC            SECTION.
      *
           OPEN  INPUT  A00000YL01-FILE
           MOVE  ZERO   TO  FLG-PRINT
      *
           INITIALIZE  YL01TMP
                       YL01
                       IDX
      *
      *    CSV用一時領域  初期化
           INITIALIZE  WRK-ITEM
                       WRK-CSVDATA
      *
      *    一時ファイル読込
           PERFORM  700-TMP-READ-SEC
      *
      *    合計頁算出処理
           PERFORM  400-GPAGE-SEC
      *
      *    ヘッダー編集処理
           PERFORM  400-HEAD-HEN-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
      *      CSV用一時領域  初期化
             INITIALIZE  WRK-ITEM
      *
             ADD     1                      TO  IDX
      *
      *      診療行為コード
             MOVE    YL01TMP-SRYCD          TO  YL01-SRYCD(IDX)
      *
      *      CSV(診療行為コード)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      入力コード
             MOVE    YL01TMP-INPUTCD        TO  YL01-INPUTCD(IDX)
      *
      *      CSV(入力コード)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      薬剤区分
             MOVE    YL01TMP-YKZKBN         TO  YL01-YKZKBN(IDX)
      *
      *      CSV(薬剤区分)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      一般名区分
             MOVE    YL01TMP-KISAIKBN       TO  YL01-KISAIKBN(IDX)
      *
      *      CSV(一般名区分)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      医薬品名
             MOVE    YL01TMP-NAME           TO  YL01-NAME(IDX)
      *
      *      CSV(医薬品名)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      一般名
             MOVE    YL01TMP-GNAME          TO  YL01-GNAME(IDX)
      *
      *      CSV(一般名)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      薬価
             MOVE    YL01TMP-YAKKA          TO  WRK-YAKKA
             MOVE    WRK-YAKKA              TO  YL01-YAKKA(IDX)
      *
      *      CSV(薬価)
                                                WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      最低薬価
             MOVE    YL01TMP-SAIYAKKA       TO  WRK-YAKKA
             MOVE    WRK-YAKKA              TO  YL01-SAIYAKKA(IDX)
      *
      *      CSV(最低薬価)
                                                WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
      *
      *      CSV印字
             PERFORM  230-TOUKEICSV-SEC
      *
      *      薬剤区分を退避
             MOVE    YL01TMP-YKZKBN         TO  WRK-YKZKBN
      *
      *      一時ファイル読込
             PERFORM  700-TMP-READ-SEC
      *
      *      最終行、印字情報無し又は
      *      改頁フラグONかつ薬剤区分が前回と異なる場合、
      *      印刷を行う。
             IF (   IDX                 =  34  OR
                    FLG-PRINT           =   1  OR
                  ( YL01TMP-YKZKBN  NOT =  WRK-YKZKBN  AND
                    FLG-KPAGE           =   1     )         )
      *
               MOVE    ZERO        TO  IDX
      *
      *        印刷処理
               PERFORM  500-PRINT-OUT-SEC
      *
      *        改頁有、印字情報有の場合、薬剤区分を印字（CSV）
               IF  ( FLG-PRINT       NOT =  1  AND
                     FLG-KPAGE           =  1  AND
                     YL01TMP-YKZKBN  NOT =  WRK-YKZKBN)
      *
                 PERFORM  410-CSV-HEAD-KPAGE-SEC
      *
               END-IF
      *
             END-IF
           END-PERFORM
      *
           CLOSE   A00000YL01-FILE
      *
           .
       400-PRINT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    合計頁  算出処理
      *****************************************************************
       400-GPAGE-SEC            SECTION.
      *
      *    カウント一時領域  初期化
           INITIALIZE  CNT-AREA
      *
           IF  FLG-KPAGE  =  1
      *
      *      内服薬
             INITIALIZE  CNT-AMARI
             DIVIDE  WRK-DATA(2)   BY  34
             GIVING  CNT-GPAGE(2)  REMAINDER  CNT-AMARI
      *
             IF  CNT-AMARI  =  ZERO
               CONTINUE
             ELSE
               COMPUTE  CNT-GPAGE(2)  =  CNT-GPAGE(2) + 1
             END-IF
      *
      *      外用薬
             INITIALIZE  CNT-AMARI
             DIVIDE  WRK-DATA(3)   BY  34
             GIVING  CNT-GPAGE(3)  REMAINDER  CNT-AMARI
      *
             IF  CNT-AMARI  =  ZERO
               COMPUTE  CNT-GPAGE(3)  =  CNT-GPAGE(2) + CNT-GPAGE(3)
             ELSE
               COMPUTE  CNT-GPAGE(3)  =  CNT-GPAGE(2) + CNT-GPAGE(3) + 1
             END-IF
      *
      *      注射薬
             INITIALIZE  CNT-AMARI
             DIVIDE  WRK-DATA(4)   BY  34
             GIVING  CNT-GPAGE(4)  REMAINDER  CNT-AMARI
      *
             IF  CNT-AMARI  =  ZERO
               COMPUTE  CNT-GPAGE(1)  =  CNT-GPAGE(3) + CNT-GPAGE(4)
             ELSE
               COMPUTE  CNT-GPAGE(1)  =  CNT-GPAGE(3) + CNT-GPAGE(4) + 1
             END-IF
      *
           ELSE
      *
      *      内服薬/注射薬/外用薬
             INITIALIZE  CNT-AMARI
             DIVIDE  WRK-DATA(1)      BY  34
             GIVING  CNT-GPAGE(1)     REMAINDER  CNT-AMARI
      *
             IF  CNT-AMARI  NOT =  ZERO
               COMPUTE  CNT-GPAGE(1)  =  CNT-GPAGE(1) + 1
             END-IF
           END-IF
      *
           .
       400-GPAGE-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー処理
      *****************************************************************
       400-HEAD-HEN-SEC                   SECTION.
      *
      *    タイトル１
           IF  WRK-PARA-KEY1  NOT =  ZERO
             MOVE    "入力コード"         TO  WRK-KEY(WRK-PARA-KEY1)
           END-IF
           IF  WRK-PARA-KEY2  NOT =  ZERO
             MOVE    "薬剤区分"           TO  WRK-KEY(WRK-PARA-KEY2)
           END-IF
           IF  WRK-PARA-KEY3  NOT =  ZERO
             MOVE    "一般名区分"         TO  WRK-KEY(WRK-PARA-KEY3)
           END-IF
           IF  WRK-PARA-KEY4  NOT =  ZERO
             MOVE    "医薬品名"           TO  WRK-KEY(WRK-PARA-KEY4)
           END-IF
           IF  WRK-PARA-KEY5  NOT =  ZERO
             MOVE    "一般名"             TO  WRK-KEY(WRK-PARA-KEY5)
           END-IF
      *
      *    タイトル１編集
           STRING  "（１）"               DELIMITED  BY  SIZE
                   WRK-KEY(1)             DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   "（２）"               DELIMITED  BY  SIZE
                   WRK-KEY(2)             DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   "（３）"               DELIMITED  BY  SIZE
                   WRK-KEY(3)             DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   "（４）"               DELIMITED  BY  SIZE
                   WRK-KEY(4)             DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   "（５）"               DELIMITED  BY  SIZE
                   WRK-KEY(5)             DELIMITED  BY  SPACE
                                          INTO  WRK-HEAD-TITLE1
           END-STRING
      *
      *    タイトル２
      *    薬剤区分編集
           EVALUATE  WRK-PARA-YKZKBN
             WHEN  "0"
               MOVE    "すべて"           TO  WRK-IYHKBN(1)
             WHEN  "1"
               MOVE    "内服薬"           TO  WRK-IYHKBN(1)
             WHEN  "2"
               MOVE    "外用薬"           TO  WRK-IYHKBN(1)
             WHEN  "3"
               MOVE    "注射薬"           TO  WRK-IYHKBN(1)
           END-EVALUATE
      *
           STRING  "薬剤区分："           DELIMITED  BY  SIZE
                   WRK-IYHKBN(1)          DELIMITED  BY  SPACE
                                          INTO  WRK-IYHKBN(2)
           END-STRING
      *
      *    入力コード編集
           EVALUATE  WRK-PARA-INPUTCD
             WHEN  "0"
               MOVE    "あり"             TO  WRK-INPUTCD(1)
             WHEN  "1"
               MOVE    "なし"             TO  WRK-INPUTCD(1)
             WHEN  "2"
               MOVE    "すべて"           TO  WRK-INPUTCD(1)
           END-EVALUATE
      *
           STRING  "入力コード："         DELIMITED  BY  SIZE
                   WRK-INPUTCD(1)         DELIMITED  BY  SPACE
                                          INTO  WRK-INPUTCD(2)
           END-STRING
      *
      *    一般名区分編集
           EVALUATE  WRK-PARA-IPNKBN
             WHEN  "0"
               MOVE    "すべて"           TO  WRK-IPNKBN(1)
             WHEN  "1"
               MOVE    "銘柄名"           TO  WRK-IPNKBN(1)
             WHEN  "2"
               MOVE    "一般名"           TO  WRK-IPNKBN(1)
             WHEN  "3"
             WHEN  "4"
               MOVE    "処方名"           TO  WRK-IPNKBN(1)
           END-EVALUATE
      *
           STRING  "一般名区分："         DELIMITED  BY  SIZE
                   WRK-IPNKBN(1)          DELIMITED  BY  SPACE
                                          INTO  WRK-IPNKBN(2)
           END-STRING
      *
      *    改頁編集
           EVALUATE  WRK-PARA-KPAGE
             WHEN  "0"
               MOVE    "なし"             TO  WRK-KPAGE(1)
             WHEN  "1"
               MOVE    "あり"             TO  WRK-KPAGE(1)
           END-EVALUATE
      *
           STRING  "改ページ："           DELIMITED  BY  SIZE
                   WRK-KPAGE(1)           DELIMITED  BY  SPACE
                                          INTO  WRK-KPAGE(2)
           END-STRING
      *
      *    タイトル２編集
           STRING  WRK-IYHKBN(2)          DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   WRK-INPUTCD(2)         DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   WRK-IPNKBN(2)          DELIMITED  BY  SPACE
                   "  "                   DELIMITED  BY  SIZE
                   WRK-KPAGE(2)           DELIMITED  BY  SPACE
                                          INTO  WRK-HEAD-TITLE2
           END-STRING
      *
      *    合計頁
           MOVE    CNT-GPAGE(1)           TO  WRK-Z4
           MOVE    WRK-Z4                 TO  WRK-HEAD-GPAGE
      *
      *    作成年月日編集
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  WRK-HEAD-PRTYMD
      *
      *    CSVヘッダ編集処理
           PERFORM  800-CSV-HEAD-SEC
      *
           .
       400-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       500-PRINT-OUT-SEC                      SECTION.
      *
      *    ヘッダー処理
           MOVE    WRK-HEAD-TITLE1            TO  YL01-TITLE1
           MOVE    WRK-HEAD-TITLE2            TO  YL01-TITLE2
           MOVE    WRK-HEAD-PRTYMD            TO  YL01-PRTYMD
           MOVE    WRK-HEAD-GPAGE             TO  YL01-GPAGE
      *
      *    頁カウント
           ADD     1                          TO  CNT-PAGE
           MOVE    CNT-PAGE                   TO  WRK-Z4
           MOVE    WRK-Z4                     TO  YL01-PAGE
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
           MOVE    "A00000YL01.red"           TO  SPRT-PRTID
           MOVE    "医薬品名・一般名対応リスト"
                                              TO  SPRT-TITLE
           MOVE    YL01                       TO  SPRT-PRTDATA
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
           INITIALIZE  YL01
      *
           .
       500-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダー処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1               TO  IDXP
      *
      *    CSV用一時領域  初期化
           INITIALIZE  WRK-ITEM
      *
      *    タイトル１編集
           STRING  "1"                    DELIMITED  BY  SIZE
                   WRK-KEY(1)             DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   "2"                    DELIMITED  BY  SIZE
                   WRK-KEY(2)             DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   "3"                    DELIMITED  BY  SIZE
                   WRK-KEY(3)             DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   "4"                    DELIMITED  BY  SIZE
                   WRK-KEY(4)             DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   "5"                    DELIMITED  BY  SIZE
                   WRK-KEY(5)             DELIMITED  BY  SPACE
                                          INTO  WRK-ITEM
           END-STRING
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    CSV用一時領域  初期化
           INITIALIZE  WRK-ITEM
      *
      *    タイトル２編集
           STRING  WRK-IYHKBN(2)          DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   WRK-INPUTCD(2)         DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   WRK-IPNKBN(2)          DELIMITED  BY  SPACE
                   ","                    DELIMITED  BY  SIZE
                   WRK-KPAGE(2)           DELIMITED  BY  SPACE
                                          INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付
           MOVE    LNK-PRTKANRI-SKYYMD    TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG            TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    改頁有の場合、薬剤区分を印字
           IF  FLG-KPAGE  =  1
             EVALUATE  YL01TMP-YKZKBN
               WHEN  "内"
                 MOVE    "薬剤区分：内服薬"     TO  WRK-ITEM
               WHEN  "外"
                 MOVE    "薬剤区分：外用薬"     TO  WRK-ITEM
               WHEN  "注"
                 MOVE    "薬剤区分：注射薬"     TO  WRK-ITEM
             END-EVALUATE
             PERFORM  810-CSVDATA-EDIT-SEC
           END-IF
      *
      *    項目名称セット
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL  IDX2  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX2)    TO  WRK-ITEM
             IF   IDX2  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    CSV印字
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダー処理  改頁
      *****************************************************************
       410-CSV-HEAD-KPAGE-SEC                   SECTION.
      *
      *    CSV用一時領域  初期化
           INITIALIZE  WRK-ITEM
      *
      *    一行空ける
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    薬剤区分を印字
           EVALUATE  YL01TMP-YKZKBN
             WHEN  "内"
               MOVE    "薬剤区分：内服薬"     TO  WRK-ITEM
             WHEN  "外"
               MOVE    "薬剤区分：外用薬"     TO  WRK-ITEM
             WHEN  "注"
               MOVE    "薬剤区分：注射薬"     TO  WRK-ITEM
           END-EVALUATE
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称セット
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL  IDX2  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX2)    TO  WRK-ITEM
             IF   IDX2  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    CSV印字
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       410-CSV-HEAD-KPAGE-EXT.
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
       800-CSVDATA-EDIT-EXT.
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
                   "iyakuhinmei_ippanmei_"
                                              DELIMITED  BY  SIZE
                   LNK-PRTKANRI-SKYYMD        DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "医薬品名・一般名対応リスト"
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
           INITIALIZE  WRK-CSVDATA
           MOVE    1               TO  IDXP
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
           MOVE    1           TO  FLG-TENSU
      *
           .
       800-ERR-HENSYU-EXT.
           EXIT.
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
      *****************************************************************
      *    点数情報読込
      *****************************************************************
       900-TENSU-SELECT-SEC                  SECTION.
      *
           INITIALIZE                        TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM          TO  TNS-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD       TO  TNS-YUKOSTYMD
                                                 TNS-YUKOEDYMD
           MOVE    "600000000"               TO  TNS-SRYCD
           MOVE    "699999999"               TO  TNSUP-SRYCD
           MOVE    WRK-YKZ-KEY(IDX-YKZ2)     TO  TNS-YKZKBN
      *
           MOVE    TNS-TENSU-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_tensu"               TO  MCP-TABLE
           MOVE    "key11s"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             PERFORM 900-TENSU-READ-SEC
           ELSE
             MOVE    1                         TO  FLG-TENSU
           END-IF
           .
       900-TENSU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数情報読込
      *****************************************************************
       900-TENSU-READ-SEC                    SECTION.
      *
           MOVE    "DBFETCH"                 TO  MCP-FUNC
           MOVE    "tbl_tensu"               TO  MCP-TABLE
           MOVE    "key11s"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
           IF   MCP-RC    =   ZERO
             MOVE    ZERO                      TO  FLG-TENSU
             MOVE    MCPDATA-REC               TO  TNS-TENSU-REC
           ELSE
             MOVE    1                         TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数情報クローズ処理
      *****************************************************************
       900-TENSU-CLOSE-SEC                   SECTION.
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_tensu"               TO  MCP-TABLE
           MOVE    "key11s"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           .
       900-TENSU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    入力コード情報読込
      *****************************************************************
       910-INPUTCD-READ-SEC           SECTION.
      *
           INITIALIZE                   INPUTCD-REC
           MOVE    WRK-PARA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    TNS-SRYCD            TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_inputcd"        TO  MCP-TABLE
             MOVE    "key4"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC        TO  INPUTCD-REC
               MOVE    ZERO               TO  FLG-INPUTCD
             ELSE
               INITIALIZE                     INPUTCD-REC
               MOVE    1                  TO  FLG-INPUTCD
             END-IF
           ELSE
             MOVE    1                  TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数付加情報読込
      *****************************************************************
       910-TENSUPLUS-READ-SEC             SECTION.
      *
           INITIALIZE                   TENSUP-REC
           MOVE    WRK-PARA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNS-SRYCD            TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD           TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD           TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUP-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_tensuplus"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_tensuplus"      TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
             IF   MCP-RC    =   ZERO
               MOVE    ZERO                 TO  FLG-TENSUP
               MOVE    MCPDATA-REC          TO  TENSUP-REC
             ELSE
               MOVE    1                    TO  FLG-TENSUP
             END-IF
           ELSE
             MOVE    1                  TO  FLG-TENSUP
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_tensuplus"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    一般名情報読込
      *****************************************************************
       910-GENERICNAME-READ-SEC             SECTION.
      *
           INITIALIZE                   GENERICNAME-REC
           MOVE    TNS-YAKKAKJNCD       TO  GENERIC-YAKKAKJNCD
      *
           MOVE    GENERICNAME-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_genericname"    TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_genericname"    TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
             IF   MCP-RC    =   ZERO
               MOVE    ZERO                 TO  FLG-GENERIC
               MOVE    MCPDATA-REC          TO  GENERICNAME-REC
             ELSE
               MOVE    1                    TO  FLG-GENERIC
             END-IF
           ELSE
             MOVE    1                  TO  FLG-GENERIC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_genericname"    TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-GENERICNAME-READ-EXT.
           EXIT.
      *****************************************************************
      *    最低薬価情報読込
      *****************************************************************
       910-GENERICPRICE-READ-SEC             SECTION.
      *
           INITIALIZE                   GENERICPRICE-REC
           MOVE    GENERIC-YAKKAKJNCD   TO  GENERICPRICE-YAKKAKJNCD
           MOVE    TNS-YUKOEDYMD        TO  GENERICPRICE-YUKOSTYMD
           MOVE    TNS-YUKOSTYMD        TO  GENERICPRICE-YUKOEDYMD
      *
           MOVE    GENERICPRICE-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_generic_price"  TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_generic_price"  TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
             IF   MCP-RC    =   ZERO
               MOVE    ZERO                 TO  FLG-GENERICP
               MOVE    MCPDATA-REC          TO  GENERICPRICE-REC
             ELSE
               MOVE    1                    TO  FLG-GENERICP
             END-IF
           ELSE
             MOVE    1                  TO  FLG-GENERICP
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_generic_price"  TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-GENERICPRICE-READ-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込(NEXT READ)
      *****************************************************************
       700-TMP-READ-SEC              SECTION.
      *
           READ    A00000YL01-FILE  NEXT
               AT  END
                 MOVE    1           TO  FLG-PRINT
               NOT  AT END
                 MOVE    ZERO        TO  FLG-PRINT
           END-READ
           .
       700-TMP-READ-EXT.
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
