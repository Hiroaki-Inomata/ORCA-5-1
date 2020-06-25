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
       PROGRAM-ID.             ORCBCHKKNS1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : チェックマスタ２設定
      *                      適応病名マスタ->チェックマスタ２
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/12/20    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.01.00    NACL-太田    06.07.27  MONFUNC対応
      *  03.05.00    NACL-太田    07.06.07  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    診療行為コードワーク
           SELECT  SRYCD-FILE      ASSIGN   BCHKKNS101PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS   MODE   IS  DYNAMIC
                                   RECORD   KEY    IS  SRYCD-KEY
                                   FILE     STATUS IS  STS-SRYCD.
      *
      *    並べ替えファイル
           SELECT  SORT-FILE       ASSIGN   BCHKKNS102PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS   MODE   IS  DYNAMIC
                                   RECORD   KEY    IS  SORT-KEY
                                   FILE     STATUS IS  STS-SORT.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為コードワーク
       FD  SRYCD-FILE.
       01  SRYCD-REC.
           03  SRYCD-KEY           PIC X(09).
           03  SRYCD-FLG           PIC X(01).
           03  SRYCD-YUKOSTYMD     PIC X(08).
           03  SRYCD-YUKOEDYMD     PIC X(08).
      *
      *    並べ替えファイル
       FD  SORT-FILE.
       01  SORT-REC.
           03  SORT-KEY.
               05  SORT-LISTKBN    PIC X(01).
               05  SORT-KANANAME   PIC X(100).
               05  SORT-SRYCD      PIC X(09).
           03  SORT-YUKOSTYMD      PIC X(08).
           03  SORT-YUKOEDYMD      PIC X(08).
           03  SORT-NAME           PIC X(100).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    診療行為コードワーク
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //BCHKKNS101//.
      *
      *    並べ替えファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //BCHKKNS102//.
      *
           COPY    "CPRECEERR.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
           03  STS-SRYCD                   PIC X(02).
           03  STS-SORT                    PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-INPUTCD                         PIC 9(01).
           03  FLG-SRYACT                          PIC 9(01).
           03  FLG-CHKSND                          PIC 9(01).
           03  FLG-TEKBYO                          PIC 9(01).
           03  FLG-SRYCD                           PIC 9(01).
           03  FLG-SORT                            PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-END                             PIC 9(01).
           03  FLG-LIST1                           PIC 9(01).
           03  FLG-LIST2                           PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCD                           PIC 9(08).
           03  CNT-LINE                            PIC 9(08).
           03  CNT-PAGE                            PIC 9(08).
           03  CNT-RENNUM                          PIC 9(08).
           03  CNT-LIST1                           PIC 9(08).
           03  CNT-LIST2                           PIC 9(08).
           03  CNT-SEQ                             PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDXI                                PIC 9(05).
           03  IDXE                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-DUMMY                      PIC X(1)
                                                   OCCURS 9.
           03  WRK-PARA-CLEAR                      PIC X(6).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ZZZZ9-G.
               05  WRK-ZZZZ9                       PIC ZZZZ9.
           03  WRK-YUKOSTYMD                       PIC X(10).
           03  WRK-YUKOEDYMD                       PIC X(10).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-YMD                         PIC X(08).
           03  WRK-BYOMEICD                        PIC X(07).
           03  WRK-MEMO                            PIC X(500).
           03  WRK-TITLE                           PIC X(100).
           03  WRK-MSG                             PIC X(100).
           03  WRK-STR                             PIC X(200).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05  WRK-STR-LEFT-FL                 PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
      *        全角編集用項目
           03  WRK-N.
               05  WRK-KNS-N                       PIC X(10).
               05  WRK-TIME-N                      PIC X(16).
           03  WRK-CHKKBNMEI                       PIC X(20).
           03  WRK-KEY-CD-G.
               05  WRK-KEY-CD-OCC                  OCCURS  21.
                   07  WRK-KEY-CD                  PIC X(07).
                   07  WRK-KEY-CDSEP               PIC X(01).
           03  WRK-IKOCD                           PIC X(07).
           03  WRK-TBYOMEICD-G.
               05  WRK-TBYOMEICD                   PIC X(07)
                                                   OCCURS  21.
           03  WRK-LINE                            PIC 9(03).
      *
       01  KEY-AREA.
           03  KEY-LISTKBN                         PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-YUKOSTYMD         PIC X(08)   VALUE "00000000".
           03  CONST-YUKOEDYMD         PIC X(08)   VALUE "99999999".
           03  CONST-MKCD-MAX          PIC 9(05)   VALUE 21.
           03  CONST-LINE-MAX          PIC 9(05)   VALUE 80.
           03  CONST-BODY-START        PIC 9(05)   VALUE 3.
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "HCMSL80.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
      *    入力コード
       01  ICD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    チェックマスター２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    適応病名マスタ
       01  TEKIOUBYOMEI-REC.
           COPY    "CPTEKIOUBYOMEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    チェックマスタ病名編集サブ
           COPY    "CPORCSS012.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               IF    ( WRK-PARA-CLEAR  =   "999999" )
                   PERFORM 201-MAIN-SEC
               ELSE
                   PERFORM 200-MAIN-SEC
               END-IF
           END-IF
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           INITIALIZE                  WRK-PARA
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
                                       WRK-PARA-DUMMY (1)
                                       WRK-PARA-DUMMY (2)
                                       WRK-PARA-DUMMY (3)
                                       WRK-PARA-DUMMY (4)
                                       WRK-PARA-DUMMY (5)
                                       WRK-PARA-DUMMY (6)
                                       WRK-PARA-DUMMY (7)
                                       WRK-PARA-DUMMY (8)
                                       WRK-PARA-DUMMY (9)
                                       WRK-PARA-CLEAR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "適応病名情報更新（検査）"
                                       TO  WRK-TITLE
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBCHKKNS1"       TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  BCHKKNS101PARA-HOSPNUM
           MOVE    "BCHKK01"           TO  BCHKKNS101PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  BCHKKNS101PARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  BCHKKNS102PARA-HOSPNUM
           MOVE    "BCHKK02"           TO  BCHKKNS102PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  BCHKKNS102PARA-TERMID
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BCHKKNS101PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    BCHKKNS102PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (2)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (3)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BCHKKNS101PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  BCHKKNS102PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECEERR
      *
           OPEN    OUTPUT              SRYCD-FILE
           CLOSE   SRYCD-FILE
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF    ( WRK-PARA-CLEAR  NOT =   SPACE )
               IF    ( WRK-PARA-CLEAR  =   "999999" )
                   MOVE    "【診療行為と病名】を初期化します"
                                               TO  WRK-MSG
                   PERFORM 800-JOB-STM-SEC
               ELSE
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "初期化を行う場合、初期化区分に"
                                               DELIMITED BY SIZE
                           "９９９９９９を設定してください"
                                               DELIMITED BY SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    "登録内容より検査情報を取得しています"
                                   TO  WRK-MSG
           PERFORM 800-JOB-STM-SEC
      *
      *    入力コード有を対象にチェックマスタ２情報登録
           PERFORM 2001-ICD-TO-CHKSND-SEC
      *
      *    診療行為マスタより対象診療コード抽出
           PERFORM 2002-SRYACT-OUT-SEC
      *
      *    対象診療コードよりチェックマスタ２情報登録
           PERFORM 2002-SRYACT-TO-CHKSND-SEC
      *
      *    並べ替えファイル編集処理
           PERFORM 2003-SRYCD-SORT-SEC
      *
      *    確認リスト編集処理
           PERFORM 2004-LIST-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力コード対象処理
      *****************************************************************
       2001-ICD-TO-CHKSND-SEC      SECTION.
      *
           OPEN    I-O                 SRYCD-FILE
      *
           PERFORM 900-ICD-ALL-SEL-SEC
      *
           PERFORM UNTIL ( FLG-INPUTCD NOT =   ZERO )
      *
               IF  ( ICD-SRYCD(1:2)    =   "16" )
                   MOVE    ICD-SRYCD   TO  WRK-SRYCD
                   PERFORM 900-SRYCD-KREAD-SEC
                   IF   ( FLG-SRYCD    =   1 )
      *
                       INITIALIZE          SRYCD-REC
                       MOVE  CONST-YUKOSTYMD
                                           TO  SRYCD-YUKOSTYMD
                       MOVE  CONST-YUKOEDYMD
                                           TO  SRYCD-YUKOEDYMD
                       MOVE  WRK-SRYCD     TO  SRYCD-KEY
      *
                       PERFORM 900-TEKIOUBYOMEI-KEY2-SEL-SEC
                       IF  ( FLG-TEKBYO        =   ZERO )
                           PERFORM 900-CHKSND-KEY30-SEL-SEC
                           IF  ( FLG-CHKSND    =  ZERO  )
                               MOVE  "2"   TO  SRYCD-FLG
                               WRITE SRYCD-REC
                               MOVE   1    TO  FLG-LIST2
                           ELSE
                               MOVE  "1"   TO  SRYCD-FLG
                               WRITE SRYCD-REC
                               MOVE   1    TO  FLG-LIST1
                           END-IF
                       ELSE
                           MOVE  "0"       TO  SRYCD-FLG
                           WRITE SRYCD-REC
                       END-IF
      *
                       MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
                       MOVE    "key2"              TO   MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-ICD-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
      *
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   SRYCD-FILE
      *
           .
       2001-ICD-TO-CHKSND-EXT.
           EXIT.
      *****************************************************************
      *    対象診療コードファイル作成
      *****************************************************************
       2002-SRYACT-OUT-SEC             SECTION.
      *
           OPEN    I-O                 SRYCD-FILE
      *
           PERFORM 900-SRYACT-KEY13-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =  ZERO )
      *
               IF  SRY-SRYKBN          =  "60" OR "64"
                   PERFORM VARYING IDX0  FROM  1  BY  1
                           UNTIL ( IDX0  >  5 )
                            OR   ( SRY-SRYCD (IDX0) =  SPACE )
                       IF  SRY-SRYCD(IDX0)(1:2)  =  "16"
                           MOVE  SRY-SRYCD(IDX0)  TO WRK-SRYCD
                           PERFORM 900-SRYCD-KREAD-SEC
                           IF    FLG-SRYCD   =   1
                                 MOVE  WRK-SRYCD
                                             TO  SRYCD-KEY
                                 MOVE  SPACE TO  SRYCD-FLG
                                 WRITE SRYCD-REC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
               PERFORM 900-SRYACT-KEY13-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   SRYCD-FILE
           .
       2002-SRYACT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ情報移動処理
      *****************************************************************
       2002-SRYACT-TO-CHKSND-SEC       SECTION.
      *
           OPEN    I-O                 SRYCD-FILE
      *
           PERFORM 900-SRYCD-READ-SEC
      *
           PERFORM UNTIL ( FLG-SRYCD   NOT =   ZERO )
      *
               MOVE    SRYCD-KEY       TO   WRK-SRYCD
               PERFORM 900-TEKIOUBYOMEI-KEY2-SEL-SEC
               IF    ( FLG-TEKBYO      =   ZERO )
                   PERFORM 900-CHKSND-KEY30-SEL-SEC
                   IF    ( FLG-CHKSND  =   ZERO )
                       MOVE  CHKSND-YUKOSTYMD
                                           TO  SRYCD-YUKOSTYMD
                       MOVE  CHKSND-YUKOEDYMD
                                           TO  SRYCD-YUKOEDYMD
                       MOVE    "2"         TO  SRYCD-FLG
                       REWRITE SRYCD-REC
                       MOVE    1           TO  FLG-LIST2
                   ELSE
                       MOVE    "1"         TO  SRYCD-FLG
                       MOVE  CONST-YUKOSTYMD
                                           TO  SRYCD-YUKOSTYMD
                       MOVE  CONST-YUKOEDYMD
                                           TO  SRYCD-YUKOEDYMD
                       REWRITE SRYCD-REC
                       MOVE    1           TO  FLG-LIST1
                   END-IF
               END-IF
      *
               MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
               MOVE    "key2"              TO   MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               PERFORM 900-SRYCD-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   SRYCD-FILE
           .
       2002-SRYACT-TO-CHKSND-EXT.
           EXIT.
      *****************************************************************
      *    診療コード並べ替え処理
      *****************************************************************
       2003-SRYCD-SORT-SEC             SECTION.
      *
           OPEN    INPUT   SRYCD-FILE
           OPEN    OUTPUT  SORT-FILE
      *
           PERFORM 901-SRYCD-READ-SEC
      *
           PERFORM UNTIL ( FLG-SRYCD   NOT =   ZERO )
      *
               INITIALIZE              SORT-REC
               MOVE    SRYCD-KEY       TO  WRK-TNS-SRYCD
               MOVE    SMCNDATE-YMD    TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
      *
                   MOVE    SRYCD-FLG       TO   SORT-LISTKBN
                   MOVE    TNS-KANANAME    TO   SORT-KANANAME
                   MOVE    TNS-SRYCD       TO   SORT-SRYCD
                   MOVE    SRYCD-YUKOSTYMD TO   SORT-YUKOSTYMD
                   MOVE    SRYCD-YUKOEDYMD TO   SORT-YUKOEDYMD
                   MOVE    TNS-NAME        TO   SORT-NAME
                   WRITE   SORT-REC
      *
               END-IF
      *
               PERFORM 901-SRYCD-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   SRYCD-FILE
                   SORT-FILE
      *
           .
       2003-SRYCD-SORT-EXT.
           EXIT.
      *****************************************************************
      *    確認リスト編集処理
      *****************************************************************
       2004-LIST-EDIT-SEC           SECTION.
      *
           OPEN    INPUT            SORT-FILE
      *
           PERFORM 900-SORT-READ-SEC
      *
           PERFORM UNTIL ( FLG-SORT    NOT =   ZERO )
      *
               MOVE    SORT-LISTKBN    TO  KEY-LISTKBN
               MOVE    ZERO            TO  CNT-SEQ
      *
               PERFORM 2101-HEAD-SEC
      *
               PERFORM UNTIL ( FLG-SORT    NOT =   ZERO )
                        OR   ( KEY-LISTKBN NOT =   SORT-LISTKBN )
      *
                   IF    ( SORT-LISTKBN    =   "1" )
                       PERFORM 2101-BODY-SEC
                   ELSE
                       PERFORM 2102-BODY-SEC
                   END-IF
      *
                   PERFORM 900-SORT-READ-SEC
      *
               END-PERFORM
      *
               IF    ( CNT-LINE        >   CONST-BODY-START )
                   PERFORM 2101-PRINT-SEC
               END-IF
      *
           END-PERFORM
      *
           CLOSE   SORT-FILE
      *
           .
       2004-HCMSL80-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダ情報編集処理
      *****************************************************************
       2101-HEAD-SEC                   SECTION.
      *
           COMPUTE CNT-PAGE            =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  WRK-ZZZZ9
      *
           INITIALIZE                  HCMSL80
      *
           MOVE    SPACE               TO  WRK-MEMO
           IF    ( SORT-LISTKBN    =   "1" )
               MOVE    "（チェックマスタを登録した検査）"
                                       TO  WRK-MEMO
           ELSE
               MOVE    "（登録済みのため更新を行わなかった検査）"
                                       TO  WRK-MEMO
           END-IF
      *
           STRING
               "＊＊　適用病名情報更新確認リスト"
                                               DELIMITED   BY  SIZE
               WRK-MEMO                        DELIMITED   BY  SPACE
               "＊＊＊"                        DELIMITED   BY  SIZE
               "（作成日："                    DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(1:4)        DELIMITED   BY  SIZE
               "."                             DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(5:2)        DELIMITED   BY  SIZE
               "."                             DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(7:2)        DELIMITED   BY  SIZE
               "）"                            DELIMITED   BY  SIZE
               "PAGE "                         DELIMITED   BY  SIZE
               WRK-ZZZZ9-G                     DELIMITED   BY  SIZE
               INTO    HCMSL80-MOJI (1)
           END-STRING
      *
           MOVE    "＜チェック区分【診療行為と病名】＞"
                                       TO  HCMSL80-MOJI (2)
      *
           MOVE    CONST-BODY-START    TO  CNT-LINE
      *
           .
       2101-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細情報編集処理
      *****************************************************************
       2101-BODY-SEC                   SECTION.
      *
           DIVIDE  10  INTO        CNT-SEQ
           GIVING      WRK-SHO
           REMAINDER   WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI       =   ZERO )
            AND  ( CNT-SEQ         >   ZERO )
      *
               MOVE    CNT-SEQ         TO  WRK-STR-LEFT-ZX99
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-KNS-N
               MOVE    SPACE       TO  WRK-MSG
               STRING  "チェックマスタを登録しています　"
                                           DELIMITED   BY  SIZE
                       WRK-KNS-N           DELIMITED   BY  SPACE
                       "件"                DELIMITED   BY  SIZE
               INTO    WRK-MSG
               END-STRING
               PERFORM 800-JOB-STM-SEC
           END-IF
      *
           MOVE    ZERO                TO  CNT-RENNUM
      *
           MOVE    SORT-SRYCD      TO  WRK-SRYCD
           PERFORM 900-TEKIOUBYOMEI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TEKBYO  NOT =   ZERO )
                    OR   ( CNT-RENNUM      >   ZERO )
      *
               INITIALIZE              SS012-AREA
               MOVE    LNK-PRTKANRI-SKYYMD (1:6)
                               TO  SS012-SRYYM
      *
               MOVE    TEKBYO-BYOMEI
                               TO  SS012-I-BYOMEI
               CALL    "ORCSS012"      USING
                                       SS012-AREA
                                       SPA-AREA
      *
               IF    ( SS012-HAISHI    =   ZERO )
      *
                   INITIALIZE              CHKSND-REC
      *
                   COMPUTE CNT-RENNUM      =   CNT-RENNUM  +   1
      *
                   MOVE    WRK-PARA-HOSPNUM TO CHKSND-HOSPNUM
                   MOVE    "2"             TO  CHKSND-CHKKBN
                   MOVE    WRK-SRYCD       TO  CHKSND-SRYCD
                   MOVE    "00000000"      TO  CHKSND-YUKOSTYMD
                   MOVE    "99999999"      TO  CHKSND-YUKOEDYMD
                   MOVE    "0"             TO  CHKSND-CDKBN
                   MOVE    CNT-RENNUM      TO  CHKSND-RENNUM
                   MOVE    "1"             TO  CHKSND-TEKBYOKBN
      *
                   MOVE    SMCNDATE-YMD    TO  CHKSND-CREYMD
                                               CHKSND-UPYMD
                   MOVE    SMCNDATE-HMS    TO  CHKSND-UPHMS
      *
                   MOVE    CHKSND-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"    TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
      *
                   PERFORM 2104-CHKSND-HEN-SEC
      *
               END-IF
      *
               PERFORM 900-TEKIOUBYOMEI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( CNT-RENNUM          >   ZERO )
               PERFORM 2104-LINE-CNT-SEC
           END-IF
      *
           .
      *
       2101-BODY-EXT.
           EXIT.
      *****************************************************************
      *    明細情報編集処理
      *****************************************************************
       2102-BODY-SEC                   SECTION.
      *
           PERFORM 2104-CHKSND-HEN-SEC
      *
           PERFORM 2104-LINE-CNT-SEC
      *
           .
      *
       2102-BODY-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ内容編集処理
      *****************************************************************
       2104-CHKSND-HEN-SEC         SECTION.
      *
           COMPUTE CNT-SEQ =   CNT-SEQ +   1
           MOVE    CNT-SEQ         TO  WRK-ZZZZ9
      *
           MOVE    SPACE           TO  WRK-YUKOSTYMD
                                       WRK-YUKOEDYMD
           IF    ( SORT-YUKOSTYMD  =   "00000000" )
               MOVE    SORT-YUKOSTYMD  TO  WRK-YUKOSTYMD
           ELSE
               MOVE    SORT-YUKOSTYMD  TO  WRK-YUKOSTYMD
               STRING  SORT-YUKOSTYMD (1:4)    DELIMITED   BY  SIZE
                       "."                     DELIMITED   BY  SIZE
                       SORT-YUKOSTYMD (5:2)    DELIMITED   BY  SIZE
                       "."                     DELIMITED   BY  SIZE
                       SORT-YUKOSTYMD (7:2)    DELIMITED   BY  SIZE
               INTO    WRK-YUKOSTYMD
               END-STRING
           END-IF
      *
           IF    ( SORT-YUKOEDYMD  =   "99999999" )
               MOVE    SORT-YUKOEDYMD  TO  WRK-YUKOEDYMD
           ELSE
               MOVE    SORT-YUKOEDYMD  TO  WRK-YUKOEDYMD
               STRING  SORT-YUKOEDYMD (1:4)    DELIMITED   BY  SIZE
                       "."                     DELIMITED   BY  SIZE
                       SORT-YUKOEDYMD (5:2)    DELIMITED   BY  SIZE
                       "."                     DELIMITED   BY  SIZE
                       SORT-YUKOEDYMD (7:2)    DELIMITED   BY  SIZE
               INTO    WRK-YUKOEDYMD
               END-STRING
           END-IF
      *
           STRING  WRK-ZZZZ9-G             DELIMITED   BY  SIZE
                   " "                     DELIMITED   BY  SIZE
                   SORT-SRYCD              DELIMITED   BY  SIZE
                   " "                     DELIMITED   BY  SIZE
                   SORT-NAME               DELIMITED   BY  SPACE
                   INTO    HCMSL80-MOJI (CNT-LINE)
           END-STRING
      *
      *    PERFORM 2104-LINE-CNT-SEC
      *
           .
       2104-CHKSND-HEN-EXT.
           EXIT.
      *****************************************************************
      *    行カウント処理
      *****************************************************************
       2104-LINE-CNT-SEC               SECTION.
      *
           IF    ( CNT-LINE       >=   CONST-LINE-MAX )
               PERFORM 2101-PRINT-SEC
               PERFORM 2101-HEAD-SEC
           ELSE
               COMPUTE CNT-LINE
                   =   CNT-LINE        +   1
           END-IF
      *
           .
       2104-LINE-CNT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2101-PRINT-SEC                  SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
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
           MOVE   "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    "適用病名情報更新確認リスト（検査）"
                                       TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               IF    ( KEY-LISTKBN     =   "1" )
                   COMPUTE CNT-LIST1   =   CNT-LIST1   +   1
               ELSE
                   COMPUTE CNT-LIST2   =   CNT-LIST2   +   1
               END-IF
           ELSE
               DISPLAY "ORCSPRT ERR!!"
           END-IF
      *
           .
       2101-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理（初期化処理）
      *****************************************************************
       201-MAIN-SEC                SECTION.
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    "2"                 TO  CHKSND-CHKKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "【診療行為と病名】の初期化に失敗しました"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       201-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    ファイル削除
           PERFORM 800-FILE-DEL-SEC
      *
           IF    ( WRK-PARA-CLEAR      =   "999999" )
               IF    ( FLG-END         =   ZERO )
                   MOVE    "【診療行為と病名】を初期化しました"
                                               TO  WRK-MSG
                   PERFORM 800-JOB-STM-SEC
               END-IF
           ELSE
               MOVE    SPACE       TO  WRK-MSG
               PERFORM 800-JOB-STM-SEC
      *
               MOVE    SPACE       TO  WRK-MEMO
      *
               EVALUATE    TRUE
               WHEN  ( CNT-LIST1   >   ZERO )
                AND  ( CNT-LIST2   >   ZERO )
                   MOVE    "（登録した検査と登録済みの検査一覧）"
                                   TO  WRK-MEMO
               WHEN  ( CNT-LIST1   >   ZERO )
                   MOVE    "（登録した検査一覧）"
                                   TO  WRK-MEMO
               WHEN  ( CNT-LIST2   >   ZERO )
                   MOVE    "（登録済みの検査一覧）"
                                   TO  WRK-MEMO
               END-EVALUATE
      *
               IF    ( WRK-MEMO    NOT =   SPACE )
      *
                   MOVE    SPACE               TO  WRK-MSG
                   STRING  "確認リスト"        DELIMITED BY SIZE
                           WRK-MEMO            DELIMITED BY SPACE
                           "を作成しました"    DELIMITED BY SIZE
                   INTO    WRK-MSG
                   END-STRING
                   PERFORM 800-JOB-STM-SEC
               END-IF
      *
               IF    ( CNT-LIST1           =   ZERO )
                   MOVE    "今回登録した検査はありませんでした"
                                               TO  WRK-MSG
                   PERFORM 800-JOB-STM-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           COMPUTE JOB-UPDCNT  =   CNT-LIST1   +   CNT-LIST2
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
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
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    途中経過編集処理
      *****************************************************************
       800-JOB-STM-SEC                 SECTION.
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-MSG             TO  JOB-YOBI
           MOVE    ZERO                TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBCHKKNS1"       TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           .
      *
       800-JOB-STM-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX     TO  WRK-OUT-MAX
           END-IF
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    WRK-STR-LEFT        TO  IDXE
           PERFORM VARYING IDXE        FROM    1   BY  1
                   UNTIL ( IDXE        >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                       TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                       TO  WRK-STR-LEFT
                   MOVE    500         TO  IDXE
               END-IF
      *
           END-PERFORM
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ファイル削除処理
      *****************************************************************
       800-FILE-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BCHKKNS101PARA  TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BCHKKNS102PARA  TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-FILE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードワークファイル読込み処理
      *****************************************************************
       900-SRYCD-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOPEND
                                       FLG-SRYCD
      *
           PERFORM UNTIL ( FLG-SRYCD   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    SRYCD-FILE          NEXT
               AT  END
                   MOVE    1       TO  FLG-SRYCD
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( SRYCD-FLG   NOT =   SPACE )
      *
                       MOVE    ZERO    TO  FLG-LOOPEND
      *
                   END-IF
      *
                   COMPUTE CNT-SRYCD   =   CNT-SRYCD  +  1
      *
               END-READ
      *
           END-PERFORM
      *
           .
       900-SRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードワークファイル読込み処理
      *****************************************************************
       901-SRYCD-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOPEND
                                       FLG-SRYCD
      *
           PERFORM UNTIL ( FLG-SRYCD   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    SRYCD-FILE          NEXT
               AT  END
                   MOVE    1       TO  FLG-SRYCD
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( SRYCD-FLG   =   "1" OR "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               END-READ
      *
           END-PERFORM
      *
           .
       901-SRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    並べ替えファイル読込み処理
      *****************************************************************
       900-SORT-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-SORT
      *
           READ    SORT-FILE           NEXT
           AT  END
               MOVE    1           TO  FLG-SORT
           END-READ
      *
           .
       900-SORT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードワークファイル読込み処理
      *****************************************************************
       900-SRYCD-KREAD-SEC             SECTION.
      *
           MOVE    WRK-SRYCD           TO  SRYCD-KEY
      *
           READ    SRYCD-FILE
               INVALID
                   MOVE    1           TO  FLG-SRYCD
               NOT INVALID
                   MOVE    ZERO        TO  FLG-SRYCD
           END-READ
           .
       900-SRYCD-KREAD-EXT.
           EXIT.
      *****************************************************************
      *    入力コード　抽出
      *****************************************************************
       900-ICD-ALL-SEL-SEC             SECTION.
      *
           INITIALIZE                  ICD-REC
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           MOVE    WRK-PARA-HOSPNUM    TO  ICD-HOSPNUM
           MOVE    ICD-REC             TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ICD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           .
       900-ICD-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *     入力コード　読み込み
      *****************************************************************
       900-ICD-ALL-FET-SEC             SECTION.
      *
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ICD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           .
       900-ICD-ALL-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ　抽出
      *****************************************************************
       900-SRYACT-KEY13-SEL-SEC          SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ZERO                TO  FLG-SRYACT
           MOVE    "160000000"         TO  SRY-SRYCD (1)
           MOVE    "169999999"         TO  SRY-SRYCD (2)
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-KEY13-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ　読み込み
      *****************************************************************
       900-SRYACT-KEY13-FET-SEC          SECTION.
      *
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-KEY13-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２情報検索
      *****************************************************************
       900-CHKSND-KEY30-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                  CHKSND-REC
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           MOVE    "2"                 TO  CHKSND-CHKKBN
           MOVE    WRK-SRYCD           TO  CHKSND-SRYCD
           MOVE    ZERO                TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO   MCP-TABLE
           MOVE    "key30"             TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"        TO   MCP-TABLE
           MOVE    "key30"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY30-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ情報検索(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TEKBYO
      *
           INITIALIZE                  TEKIOUBYOMEI-REC
           MOVE    "2"                 TO  TEKBYO-CHKKBN
           MOVE    WRK-SRYCD           TO  TEKBYO-SRYCD
           MOVE    TEKIOUBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1               TO  FLG-TEKBYO
           END-IF
           .
       900-TEKIOUBYOMEI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ情報FETCH(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1               TO  FLG-TEKBYO
           END-IF
           .
      *
       900-TEKIOUBYOMEI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-TNS-YMD         TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY24-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO   MCP-TABLE
           MOVE    "key24"             TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO   MCP-TABLE
           MOVE    "key24"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-TENSU-KEY24-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.

