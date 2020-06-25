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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR1210.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  コンポーネント名  : 公害レセプト（入院）
      *                      診療実日数編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-門脇    08/06/17  入院料未算定日対応
      *  04.02.02    NACL-門脇    08/09/24  入院略称編集追加
      *
      *  04.04.01    NACL-門脇    09/01/23  入院料算定保険以外の診療分実日数算定対応
      *  04.04.02    NACL-門脇    09/09/04  小入管略称修正
      *  04.04.03    NACL-門脇    10/03/23  改正対応（外泊区分５の対応）
      *  04.04.04    NACL-門脇    10/06/01  他医療機関受診対応
      *
      *  04.05.01    NACL-門脇    10/01/27  治験対応
      *  04.05.02    NACL-門脇    11/11/04  救急搬送診療料算定対応
      *  04.05.03    NACL-門脇    11/11/09  同日再入院食事対応
      *  04.05.04    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     外泊区分６．７の対応
      *                                     入院略称の文字数拡張対応
      *  04.05.05    NACL-門脇    12/09/18  改正対応（外泊区分８，９の対応）
      *
      *  04.07.01    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.02    NACL-門脇    14/04/08  平成２６年４月改正対応
      *  04.07.03    NACL-門脇    14/06/17  栄養管理体制減算規定該当対応
      *  04.07.04    NACL-門脇    15/01/14  同日入院時のフラグ編集修正
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.03    NACL-門脇    16/08/23  持参薬対応
      *  04.08.04    NACL-門脇    18/03/01  平成３０年４月改正対応
      *  04.08.05    NACL-門脇    18/03/27  平成３０年４月改正対応
      *  05.00.01    NACL-門脇    20/03/16  他医療機関受診＋標欠の計算修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE200-FILE    ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC. 
           COPY    "CPRCF200.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    レセプト明細ファイル名称
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
      *    エラーファイル名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名称領域
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE200                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-SHOSAISIN-GEN                       PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-RECE200                             PIC 9(01).
           03  FLG-NYUKHN                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-NYURYAKUSHO                         PIC 9(01).
           03  FLG-GAIHAKU                             PIC 9(01).
           03  FLG-NYURYAKUWRK                         PIC 9(01).
           03  FLG-NISSUSET                            PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-MOD                                 PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDY                                     PIC 9(03).
           03  NYUINACT-IDX                            PIC 9(03).
           03  NYUINACT-IDX2                           PIC 9(03).
           03  ACCT-IDX                                PIC 9(03).
           03  SRYACT-IDX                              PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-DAY                          PIC X(01)
                                                       OCCURS   31.
      *
      *    実日数減カウント用領域
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-DAY-GEN                      PIC X(01)
                                                       OCCURS   31.
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
               05  WRK-DAY6                            PIC 9(04).
               05  WRK-DAY6-ZAITEN                     PIC S9(08).
               05  WRK-ACCT90                          PIC S9(08).
      *
       01  WRK-KOGAI-TABLE.
           03  WRK-KOGAI-T                             OCCURS    4.
               05  WRK-KOGAI-TEN                       PIC 9(09)V99.
               05  WRK-KOGAI-ZAIKAISU1                 PIC 9(04).
               05  WRK-KOGAI-ZAIKAISU2                 PIC 9(04).
               05  WRK-KOGAI-ZAIKAISU3                 PIC 9(04).
               05  WRK-KOGAI-ZAIKAISU4                 PIC 9(04).
               05  WRK-KOGAI-GOKEI1                    PIC 9(08).
               05  WRK-KOGAI-GOKEI2                    PIC 9(08).
               05  WRK-KOGAI-GOKEI3                    PIC 9(08).
               05  WRK-KOGAI-GOKEI4                    PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-HYOKETU-TEN                         PIC 9(09)V99.
      *
           03  WRK-HOSPSBT                             PIC 9(01).
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-TOKUREI-SRYCD                       PIC X(09).
           03  WRK-TOKUREI-SRYCD2                      PIC X(09).
           03  WRK-TOKUREI-FLG                         PIC 9(01).
      *
           03  WRK-TOKUREI-RYAKUSHO                    PIC X(20).
      *
      *    入院略称退避ワーク
           03  WRK-NYURYAKU-OCC                       OCCURS  200.
               05  WRK-NYURYAKU-SRYCD                 PIC X(09).
               05  WRK-NYURYAKU-YUKOSTYMD             PIC X(09).
               05  WRK-NYURYAKU-YUKOEDYMD             PIC X(09).
               05  WRK-NYURYAKU-DATA                  PIC X(400).
           03  WRK-NYUYAKU-CD                         PIC X(09).
           03  WRK-NYURYAKU-IDX                       PIC 9(03).
           03  WRK-NYURYAKU-MAX                       PIC 9(03).
      *
           03  WRK-KEISAN2                            PIC 9(08).
           03  WRK-TENSU-KEISAN2                      PIC 9(08)V9(5).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001                        PIC X(610).
      *
       01  WRK-CONS-AREA.
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *    入院略称読み込みワーク用
           03  WRK-NYURYAKU-CONST-MAX             PIC 9(03) VALUE 200.
      *
           03  WRK-RECEERR                             PIC X(200). 
      *
      *    入院点数算定用ワークエリア
       01  WRK-NYUIN-TENSU-AREA.
           03  WRK-KHNTEN                              PIC 9(08)V9(5).
           03  WRK-KHNTEN-MOTO                         PIC 9(08).
           03  WRK-KHNTEN-GEN                          PIC 9(08)V9(5).
           03  WRK-KHN-ZAITEN                          PIC 9(08)V9(5).
           03  WRK-KHN-ZAITEN-MOTO                     PIC 9(08).
           03  WRK-KHN-ZAITEN-GEN                      PIC 9(08)V9(5).
           03  WRK-KSNTEN                              PIC S9(08).
           03  WRK-NYUTENTTLKBN                        PIC 9(03).
           03  WRK-NYUKHNRYO                           PIC 9(09).
           03  WRK-NYUTOKRYO                           PIC 9(09).
           03  WRK-SRYCD                               PIC X(09).
           03  WRK-RYAKUSHO                            PIC X(20).
           03  WRK-SRYYMD                              PIC X(08).       
           03  WRK-SRYYMD-TAIHI                        PIC X(08).   
           03  WRK-GAIHAKU-SRYCD                       PIC X(09).
           03  WRK-GAIHAKU-KHNTEN                      PIC 9(08).
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    入院略称
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    栄養管理経過措置入院料コード取得サブ
           COPY    "CPORCSEIYOUKKS.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    夜間看護体制特定日減算取得サブ
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE700"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE200PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1210"      TO  JOB-PGID
               MOVE    "診療実日数編集"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
      *    システム管理情報編集処理
           IF      FLG-END             =   ZERO
               PERFORM 1001-SYSKANRI-HENSYU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    システム管理情報編集処理
      ******************************************************************
       1001-SYSKANRI-HENSYU-SEC          SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key10"             TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key10"          TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 IF      FLG-SYSKANRI     =   ZERO
                    MOVE   SYS-1001-HOSPSBT  TO  WRK-HOSPSBT
                 END-IF
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key10"             TO  MCP-PATHNAME
              PERFORM  900-CLOSE-SEC
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
                 PERFORM  900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
                   ADD     1              TO  SYSKAN-IDX
                   MOVE    SYS-1001-REC   TO  TBL-SYS1001(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri" TO  MCP-TABLE
                   MOVE    "key8"         TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM  900-CLOSE-SEC
           END-IF
      *
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    I-O                 RECE200-FILE
      *
           PERFORM 900-RECE200-READ-SEC
      *
           PERFORM             UNTIL   FLG-RECE200 =   1
                               OR      FLG-END     =   1
               PERFORM 20011-HENSYU-SEC
      *
               PERFORM 900-RECE200-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE200-FILE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       20011-HENSYU-SEC                   SECTION.
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
           MOVE       SPACE             TO     TBL-JNISSU-GEN
           INITIALIZE                          TBL-JNISSU-GEN
      *
           MOVE       SPACE             TO     WRK-DAY-TABLE
           INITIALIZE                          WRK-DAY-TABLE
      *
           MOVE       SPACE             TO     WRK-KOGAI-TABLE
           INITIALIZE                          WRK-KOGAI-TABLE
      *
      *    夜間看護体制特定日減算取得
           IF   RECE200-SRYYM       >=  "201804"
             INITIALIZE                 ORCSKANGOGEN-AREA
             MOVE  RECE200-HOSPNUM  TO  LNK-KANGOGEN-HOSPNUM
             MOVE  RECE200-PTID     TO  LNK-KANGOGEN-PTID
             MOVE  RECE200-SRYYM    TO  LNK-KANGOGEN-SANTEIYM
             CALL  "ORCSKANGOGEN"   USING  ORCSKANGOGEN-AREA
                                           SPA-AREA
             IF    LNK-KANGOGEN-RC  =   ZERO
               PERFORM  VARYING  IDY  FROM  1  BY  1
                         UNTIL   IDY   >   31
                 MOVE  LNK-KANGOGEN-SRYCD(IDY)
                                    TO  RECE200-KANGOGEN-SRYCD(IDY)
               END-PERFORM
             END-IF
           END-IF
      *
           PERFORM     200111-RECE200-HENSYU-SEC
           PERFORM     200112-RECE200-HENSYU2-SEC
           PERFORM     200113-RECE200-HENSYU3-SEC
      *
           PERFORM     200116-JNISSU-SET-SEC
      *
           PERFORM     200117-RECE200-REWRITE-SEC
           .
       20011-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200111-RECE200-HENSYU-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR
                                                "5"  OR  "6"
                       PERFORM    2001111-NYUIN-DAY-TAIHI-SEC
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key7"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
                 IF        WRK-DAY5(IDY)   =   RECE200-HKNCOMBI
                      IF  (WRK-DAY1(IDY)    NOT =    ZERO )
                      OR  (RECE200-NYUINDAY-KBN(IDY) = "1")
                          MOVE   "1"    TO   TBL-JNISSU-DAY(IDY)
                      END-IF
                 END-IF
      *
           END-PERFORM
      *
      *+++同日再入院
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
      *
                 IF   RECE200-HKNCOMBI        =
                                   RECE200-DOUJITSU-HKNCOMBI(IDY)
      ***             MOVE    SPACE     TO   RECE200-NYUINRYOFLG
                      IF   RECE200-HKNCOMBI = WRK-DAY5(IDY)
                        CONTINUE
                      ELSE
                        MOVE  SPACE     TO   RECE200-NYUINRYOFLG
                      END-IF
                      IF  (WRK-DAY1(IDY)    NOT =    ZERO )
                      OR  (RECE200-NYUINDAY-KBN(IDY) = "1")
                          MOVE   "1"    TO   TBL-JNISSU-DAY(IDY)
                      END-IF
                 END-IF
      *
           END-PERFORM
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE200-HOSPNUM      TO      ACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      ACCT-NYUGAIKBN
           MOVE    RECE200-PTID         TO      ACCT-PTID
           MOVE    RECE200-SRYYM        TO      ACCT-SRYYM
           MOVE    RECE200-HKNCOMBI     TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN        =    ZERO)
                        MOVE    ZERO     TO    FLG-SHOSAISIN
                                               FLG-SHOSAISIN-GEN
      ***               IF   ACCT-SRYKBN  =  "11"  OR
      ***                                    "13"  OR  "14"
      ***                   PERFORM 2001113-SHOSAISIN-HANTEI-SEC
      ***               END-IF
                        IF   ACCT-SRYKBN  =  "95"  OR  "96"
                            CONTINUE
                        ELSE
                            MOVE    1    TO    FLG-SHOSAISIN
                        END-IF
                        PERFORM   2001114-RECE200-TENSOU12-SEC
                   END-IF
                   IF   (ACCT-ZAITEN    =        ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN        =    ZERO)
                        MOVE    ZERO     TO    FLG-SHOSAISIN
                                               FLG-SHOSAISIN-GEN
                        IF   ACCT-SRYKBN  =  "11"  OR  "99" 
                            PERFORM 2001113-SHOSAISIN-HANTEI-SEC
                        END-IF
                        PERFORM   2001114-RECE200-TENSOU12-SEC
                   END-IF
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)    AND
                        (ACCT-ZAIKBN        =    ZERO)
                        IF   ACCT-SRYKBN  =  "90" OR "92"
                            PERFORM   2001115-NYUINRYO-TAIHI-SEC
                        END-IF
                   END-IF
                   MOVE    "tbl_sryacct"     TO  MCP-TABLE
                   MOVE    "key22"           TO  MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"    TO  MCP-TABLE
           MOVE    "key22"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RECE200-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避処理                                          *
      ******************************************************************
       2001111-NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"
                  IF     NACCT-SRYCDTOTAL    =    099999916
                    CONTINUE
                  ELSE
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1(IDY) 
                    IF  ( NACCT-SRYKBN     = "90"      )
                    OR  ( NACCT-SRYCDTOTAL = 190799410 )
                    OR  ( NACCT-SRYCDTOTAL = 190799510 )
                       MOVE   903  TO  WRK-DAY1-NYUTENTTLKBN(IDY)
                       MOVE   "90" TO  RECE200-NYUDAY-SRYKBN(IDY)
                    ELSE
                       MOVE   920  TO  WRK-DAY1-NYUTENTTLKBN(IDY)
                       MOVE   "92" TO  RECE200-NYUDAY-SRYKBN(IDY)
                    END-IF
                  END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "2"
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY2(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "5"
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY5(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "6"
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY6(IDY) 
                    IF   NACCT-SRYCDTOTAL  =  190182590   OR
                                              190182690
                      COMPUTE  WRK-DAY6-ZAITEN(IDY) =
                               WRK-DAY6-ZAITEN(IDY) -
                               NACCT-ZAITEN
                    ELSE
                      ADD  NACCT-ZAITEN     TO  WRK-DAY6-ZAITEN(IDY)
                    END-IF
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       2001111-NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       2001113-SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                              UNTIL (SRYACT-IDX            >   5  ) OR
                                    (SRY-SRYCD(SRYACT-IDX) = SPACE)
      **               MOVE    SPACE         TO   TNS-TENSU-REC
      **               INITIALIZE                 TNS-TENSU-REC
      **               MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
      **               MOVE    ACCT-SRYYM         TO  TNS-YUKOSTYMD(1:6)
      **               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
      **               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
      **               PERFORM  900-TENSU-READ-SEC
      **               IF       FLG-TENSU      =    ZERO
      **                 IF ( (TNS-JITUDAY    =     2)    AND
      **                      (TNS-DAYCNT     =     1   OR   2) ) OR
      **                    (  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
      **                                                "099999902" OR
      **                                                "099110001" OR
      **                                                "099120001" OR
      **                                                "111011810")
      **                     MOVE    1      TO    FLG-SHOSAISIN
      **                 END-IF
      **                 IF    SRY-SRYCD(SRYACT-IDX) =  "099409903"
      **                     MOVE    1      TO    FLG-SHOSAISIN-GEN
      **                 END-IF
      **               END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
                                                    "099999902" OR
                                                    "099110001" OR
                                                    "099120001"
                             MOVE    1       TO   FLG-SHOSAISIN
                       END-IF
                       IF  SRY-SRYCD(SRYACT-IDX) =  "099409903"
                             MOVE    1       TO   FLG-SHOSAISIN-GEN
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO  MCP-TABLE
           MOVE    "key9"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       2001113-SHOSAISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001114-RECE200-TENSOU12-SEC                 SECTION.
      *
           IF   FLG-SHOSAISIN        =       1
              PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                          UNTIL    ACCT-IDX    >    31
                IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                  MOVE    "1"   TO   TBL-JNISSU-DAY(ACCT-IDX)
                END-IF
              END-PERFORM
           END-IF
           IF   FLG-SHOSAISIN-GEN    =       1
              PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                          UNTIL    ACCT-IDX    >    31
                IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                  MOVE    "1"   TO   TBL-JNISSU-DAY-GEN(ACCT-IDX)
                END-IF
              END-PERFORM
           END-IF
           .
       2001114-RECE200-TENSOU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計の入院料等の退避処理
      *****************************************************************
       2001115-NYUINRYO-TAIHI-SEC                 SECTION.
      *
           PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                       UNTIL    ACCT-IDX    >    31
             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
              IF   ACCT-SRYCDTOTAL  =  190182590  OR  190182690
               COMPUTE  WRK-ACCT90(ACCT-IDX) =
                        WRK-ACCT90(ACCT-IDX) -
                       (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
              ELSE
               COMPUTE  WRK-ACCT90(ACCT-IDX) =
                        WRK-ACCT90(ACCT-IDX) +
                       (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
              END-IF
             END-IF
           END-PERFORM
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                              UNTIL (SRYACT-IDX            >   5  ) OR
                                    (SRY-SRYCD(SRYACT-IDX) = SPACE)
                     IF  SRY-SRYCD(SRYACT-IDX) =  "102900010" OR
                                                  "102900020" OR
                                                  "102900030" OR
                                                  "102900040"
                       MOVE    SPACE         TO   TNS-TENSU-REC
                       INITIALIZE                 TNS-TENSU-REC
                       MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
                       MOVE    ACCT-SRYYM         TO  TNS-YUKOSTYMD(1:6)
                       MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
                       MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
                       PERFORM  900-TENSU-READ-SEC
                       IF       FLG-TENSU      =    ZERO
      *
                         PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                  UNTIL  ACCT-IDX  >  31
      *
                          IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                             COMPUTE  WRK-ACCT90(ACCT-IDX) =
                                      WRK-ACCT90(ACCT-IDX) -
                                     (TNS-TEN * ACCT-DAY(1 ACCT-IDX))
                          END-IF
      *
                          IF  ((WRK-DAY1(ACCT-IDX)   NOT =  ZERO    ) OR
                               (RECE200-NYUINDAY-KBN(ACCT-IDX) = "1"))
                          AND ( WRK-DAY5(ACCT-IDX) = RECE200-HKNCOMBI)
                          AND ( ACCT-DAY(1 ACCT-IDX) NOT =  ZERO     )
      *
                            EVALUATE  SRY-SRYCD(SRYACT-IDX)
                              WHEN     "102900010"
                                MOVE  TNS-TEN  TO  WRK-KOGAI-TEN(1)
                                EVALUATE WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                                  WHEN   903
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU1(1)
                                  WHEN   920
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU2(1)
                                END-EVALUATE
                                IF  (WRK-DAY1(ACCT-IDX)         = ZERO)
                                AND (RECE200-NYUINDAY-KBN(ACCT-IDX)
                                                                = "1" )
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU3(1)
                                END-IF
                              WHEN     "102900020"
                                MOVE  TNS-TEN  TO  WRK-KOGAI-TEN(2)
                                EVALUATE WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                                  WHEN   903
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU1(2)
                                  WHEN   920
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU2(2)
                                END-EVALUATE
                                IF  (WRK-DAY1(ACCT-IDX)         = ZERO)
                                AND (RECE200-NYUINDAY-KBN(ACCT-IDX)
                                                                = "1" )
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU3(2)
                                END-IF
                              WHEN     "102900030"
                                MOVE  TNS-TEN  TO  WRK-KOGAI-TEN(3)
                                EVALUATE WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                                  WHEN   903
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU1(3)
                                  WHEN   920
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU2(3)
                                END-EVALUATE
                                IF  (WRK-DAY1(ACCT-IDX)         = ZERO)
                                AND (RECE200-NYUINDAY-KBN(ACCT-IDX)
                                                                = "1" )
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU3(3)
                                END-IF
                              WHEN     "102900040"
                                MOVE  TNS-TEN  TO  WRK-KOGAI-TEN(4)
                                EVALUATE WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                                  WHEN   903
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU1(4)
                                  WHEN   920
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU2(4)
                                END-EVALUATE
                                IF  (WRK-DAY1(ACCT-IDX)         = ZERO)
                                AND (RECE200-NYUINDAY-KBN(ACCT-IDX)
                                                                = "1" )
                                    ADD   ACCT-DAY(1 ACCT-IDX)
                                            TO  WRK-KOGAI-ZAIKAISU3(4)
                                END-IF
                            END-EVALUATE
      *
                          END-IF
      *
                         END-PERFORM
      *
                       END-IF
                     END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO  MCP-TABLE
           MOVE    "key9"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001115-NYUINRYO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200112-RECE200-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
      *            入院基本料と加減算点数の取得
                   PERFORM   2001123-NYUINKHN-GET-SEC
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >     31
      *
                      MOVE     ZERO    TO   FLG-NISSUSET
                      IF  WRK-DAY5(IDY)  =  RECE200-HKNCOMBI
                         IF   NACCT-ZAITEN    NOT =    ZERO
                         AND  NACCT-DAY(IDY)  NOT =    ZERO
                              PERFORM 2001124-RECE200-TENSOU21-SEC
                         END-IF
                      END-IF
      *
                   END-PERFORM
      *
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "6"
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >     31
      *
                       IF   (WRK-DAY1(IDY)   NOT  =  ZERO   )
                       OR   (RECE200-NYUINDAY-KBN(IDY) = "1")
                         IF   WRK-DAY5(IDY)   =  RECE200-HKNCOMBI
                              IF   (NACCT-ZAITEN      NOT =    ZERO)
                              AND  (NACCT-DAY(IDY)    NOT =    ZERO)
                              AND  ((WRK-DAY2(IDY)    =    ZERO   ) OR
                                    (WRK-DAY2(IDY)   >=     4     ))
                                   PERFORM 2001125-RECE200-TENSOU22-SEC
                              END-IF
                         END-IF
                       END-IF
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key7"              TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE200-HOSPNUM      TO      ACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      ACCT-NYUGAIKBN
           MOVE    RECE200-PTID         TO      ACCT-PTID
           MOVE    RECE200-SRYYM        TO      ACCT-SRYYM
           MOVE    RECE200-HKNCOMBI     TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)
                   AND  (ACCT-ZAIKAISU  NOT =    ZERO)
                   AND  (ACCT-JIHOKBN       =   SPACE)
                     IF   ACCT-SRYKBN  =  "90" OR "92"
                         PERFORM 2001126-RECE200-TENSOU23-SEC
                     END-IF
                   END-IF
                   MOVE    "tbl_sryacct"     TO   MCP-TABLE
                   MOVE    "key22"           TO   MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200112-RECE200-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001124-RECE200-TENSOU21-SEC                 SECTION.
      *
           IF    RECE200-KANGOGEN-SRYCD(IDY)  =  SPACE
             MOVE  WRK-KHNTEN-MOTO         TO  WRK-KHNTEN
             MOVE  WRK-KHN-ZAITEN-MOTO     TO  WRK-KHN-ZAITEN
           ELSE
             MOVE  WRK-KHNTEN-GEN          TO  WRK-KHNTEN
             MOVE  WRK-KHN-ZAITEN-GEN      TO  WRK-KHN-ZAITEN
           END-IF
      *
      *    入院料と外泊を比較して点数算定
      *
           EVALUATE   WRK-NYUTENTTLKBN
               WHEN   903
                  MOVE      ZERO   TO  WRK-NYUKHNRYO
                  EVALUATE   WRK-DAY2(IDY)
                    WHEN   ZERO
                      COMPUTE  WRK-KEISAN2     =
                               WRK-KHN-ZAITEN  +  0.5
                      MOVE     WRK-KEISAN2     TO   WRK-NYUKHNRYO
                    WHEN   1
                      COMPUTE  WRK-NYUKHNRYO = (WRK-KHNTEN  *  0.15)
                                                            +  0.5
                    WHEN   2
                      COMPUTE  WRK-NYUKHNRYO = (WRK-KHNTEN  *  0.3)
                                                            +  0.5
                    WHEN   4
                    WHEN   5
                    WHEN   6
                    WHEN   7
                    WHEN  14
                    WHEN  15
                    WHEN  16
                      EVALUATE  WRK-DAY2(IDY)
                        WHEN       4
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.6 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.3 )
                         END-IF
                        WHEN       5
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.9 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.7 )
                         END-IF
                        WHEN       6
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.8 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.45)
                         END-IF
                        WHEN       7
                         IF    RECE200-SRYYM  >=  "201604"
                          CONTINUE
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.85)
                         END-IF
                        WHEN      14
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.95)
                        WHEN      15
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.65)
                        WHEN      16
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.85)
                      END-EVALUATE
                      COMPUTE   WRK-TENSU-KEISAN2 =
                                WRK-TENSU-KEISAN2 +
                                WRK-KSNTEN
                      IF  WRK-HYOKETU-TEN  NOT =  ZERO
                        COMPUTE WRK-NYUKHNRYO     =
                              ( WRK-TENSU-KEISAN2 -
                               (WRK-TENSU-KEISAN2 *
                               (WRK-HYOKETU-TEN   / 100)))
                                                  + 0.5
                      ELSE
                        COMPUTE WRK-NYUKHNRYO     =
                                WRK-TENSU-KEISAN2 + 0.5
                      END-IF
                    WHEN   8
                    WHEN   9
      ***           WHEN  11
                    WHEN  13
                    WHEN  17
                      EVALUATE  WRK-DAY2(IDY)
                        WHEN       8
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92)
                        WHEN       9
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92)
      ***               WHEN      11
      ***                 COMPUTE  WRK-TENSU-KEISAN2 =
      ***                         (WRK-KHNTEN * 0.92 * 0.7 )
                        WHEN      13
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92 * 0.9 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92 * 0.7 )
                         END-IF
                        WHEN      17
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92 * 0.95)
                      END-EVALUATE
                      COMPUTE   WRK-TENSU-KEISAN2 =
                                WRK-TENSU-KEISAN2 +
                                WRK-KSNTEN
                      IF  WRK-HYOKETU-TEN  NOT =  ZERO
                        COMPUTE WRK-NYUKHNRYO     =
                              ( WRK-TENSU-KEISAN2 -
                               (WRK-TENSU-KEISAN2 *
                               (WRK-HYOKETU-TEN   / 100)))
                                                  + 0.5
                      ELSE
                        COMPUTE WRK-NYUKHNRYO     =
                                WRK-TENSU-KEISAN2 + 0.5
                      END-IF
                  END-EVALUATE
                  IF   WRK-NYUKHNRYO  NOT = ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                              UNTIL (ID1                      > 20  )
                                OR  (RECE200-NYUKHNNISSU(ID1) = ZERO)
                                OR  (FLG-ARI                  = 1   )
                      IF  RECE200-NYUKHNRYO(ID1)  =  WRK-NYUKHNRYO
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE200-NYUKHNNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            ADD   WRK-NYUKHNRYO
                                      TO   RECE200-NYUKHNKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO
                      ADD    1             TO  RECE200-NYUKHNNISSU(ID1)
                      MOVE   1             TO  FLG-NISSUSET
                      MOVE  WRK-NYUKHNRYO  TO  RECE200-NYUKHNRYO(ID1)
                      ADD   WRK-NYUKHNRYO  TO  RECE200-NYUKHNKEI(ID1)
                    END-IF
                  END-IF
               WHEN   920
                  MOVE      ZERO   TO  WRK-NYUTOKRYO
                  MOVE      ZERO   TO  FLG-GAIHAKU
                  EVALUATE   WRK-DAY2(IDY)
                    WHEN   ZERO
                      COMPUTE  WRK-KEISAN2     =
                               WRK-KHN-ZAITEN  +  0.5
                      MOVE     WRK-KEISAN2     TO   WRK-NYUTOKRYO
                    WHEN   1
                      COMPUTE  WRK-NYUTOKRYO = (WRK-KHNTEN  *  0.15)
                                                            +  0.5
                      MOVE   1   TO   FLG-GAIHAKU
                    WHEN   2
                      COMPUTE  WRK-NYUTOKRYO = (WRK-KHNTEN  *  0.3)
                                                            +  0.5
                      MOVE   1   TO   FLG-GAIHAKU
                    WHEN   4
                    WHEN   5
                    WHEN   6
                    WHEN   7
                    WHEN  14
                    WHEN  15
                    WHEN  16
                      EVALUATE  WRK-DAY2(IDY)
                        WHEN       4
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.6 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.3 )
                         END-IF
                        WHEN       5
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.9 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.7 )
                         END-IF
                        WHEN       6
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.8 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.45)
                         END-IF
                        WHEN       7
                         IF    RECE200-SRYYM  >=  "201604"
                          CONTINUE
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.85)
                         END-IF
                        WHEN      14
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.95)
                        WHEN      15
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.65)
                        WHEN      16
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.85)
                      END-EVALUATE
                      COMPUTE   WRK-TENSU-KEISAN2 =
                                WRK-TENSU-KEISAN2 +
                                WRK-KSNTEN
                      IF  WRK-HYOKETU-TEN  NOT =  ZERO
                        COMPUTE WRK-NYUTOKRYO     =
                              ( WRK-TENSU-KEISAN2 -
                               (WRK-TENSU-KEISAN2 *
                               (WRK-HYOKETU-TEN   / 100)))
                                                  + 0.5
                      ELSE
                        COMPUTE WRK-NYUTOKRYO     =
                                WRK-TENSU-KEISAN2 + 0.5
                      END-IF
                      MOVE   1   TO   FLG-GAIHAKU
                    WHEN   8
                    WHEN   9
      ***           WHEN  11
                    WHEN  13
                    WHEN  17
                      EVALUATE  WRK-DAY2(IDY)
                        WHEN       8
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92)
                        WHEN       9
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92)
      ***               WHEN      11
      ***                 COMPUTE  WRK-TENSU-KEISAN2 =
      ***                         (WRK-KHNTEN * 0.92 * 0.7 )
                        WHEN      13
                         IF    RECE200-SRYYM  >=  "201604"
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92 * 0.9 )
                         ELSE
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92 * 0.7 )
                         END-IF
                        WHEN      17
                          COMPUTE  WRK-TENSU-KEISAN2 =
                                  (WRK-KHNTEN * 0.92 * 0.95)
                      END-EVALUATE
                      COMPUTE   WRK-TENSU-KEISAN2 =
                                WRK-TENSU-KEISAN2 +
                                WRK-KSNTEN
                      IF  WRK-HYOKETU-TEN  NOT =  ZERO
                        COMPUTE WRK-NYUTOKRYO     =
                              ( WRK-TENSU-KEISAN2 -
                               (WRK-TENSU-KEISAN2 *
                               (WRK-HYOKETU-TEN   / 100)))
                                                  + 0.5
                      ELSE
                        COMPUTE WRK-NYUTOKRYO     =
                                WRK-TENSU-KEISAN2 + 0.5
                      END-IF
                      MOVE   1   TO   FLG-GAIHAKU
                  END-EVALUATE
                  IF  (WRK-DAY6(IDY)          NOT =  ZERO)
                  AND (WRK-DAY6-ZAITEN(IDY)   NOT =  ZERO)
                  AND ((WRK-DAY2(IDY)         =    ZERO ) OR
                       (WRK-DAY2(IDY)        >=     4   ))
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-DAY6-ZAITEN(IDY)
                  END-IF
                  IF  WRK-ACCT90(IDY)        NOT =  ZERO
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-ACCT90(IDY)
                  END-IF
                  IF   WRK-NYUTOKRYO  NOT = ZERO
                   IF     FLG-GAIHAKU  =  ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 20    )
                          OR  ( RECE200-NYUTOKNISSU(ID1)     = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE200-NYUTOKRYO-RYAKU(ID1)  =  WRK-RYAKUSHO
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE200-NYUTOKNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE200-NYUTOKKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO
                      ADD     1            TO  RECE200-NYUTOKNISSU(ID1)
                      MOVE    1            TO  FLG-NISSUSET
                      MOVE   WRK-RYAKUSHO  TO  RECE200-NYUTOKRYO-RYAKU
                                                                  (ID1)
      ***             略称が空白の場合の為の処置
                      MOVE   999999999     TO  RECE200-NYUTOKRYO(ID1)
      *
                      ADD    WRK-NYUTOKRYO TO  RECE200-NYUTOKKEI(ID1)
                    END-IF
                   ELSE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 20    )
                          OR  ( RECE200-NYUTOKNISSU(ID1)     = ZERO  )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE200-NYUTOKRYO(ID1)  =  WRK-NYUTOKRYO
                            IF   FLG-NISSUSET   =  ZERO
                              ADD   1   TO   RECE200-NYUTOKNISSU(ID1)
                              MOVE  1   TO   FLG-NISSUSET
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE200-NYUTOKKEI(ID1)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO  
                      ADD        1         TO  RECE200-NYUTOKNISSU(ID1)
                      MOVE       1         TO  FLG-NISSUSET
      ***             点数がゼロの場合の為の処置
                      MOVE  "９９９９９９９９９９"
                                           TO  RECE200-NYUTOKRYO-RYAKU
                                                                  (ID1)
      *
                      MOVE  WRK-NYUTOKRYO  TO  RECE200-NYUTOKRYO(ID1)
                      ADD   WRK-NYUTOKRYO  TO  RECE200-NYUTOKKEI(ID1)
                    END-IF
                   END-IF
                  END-IF
           END-EVALUATE
      *
           .
       2001124-RECE200-TENSOU21-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001125-RECE200-TENSOU22-SEC                 SECTION.
      *
           IF    NACCT-SRYCDTOTAL   =   190182590  OR  190182690
             EVALUATE   WRK-DAY1-NYUTENTTLKBN(IDY)
              WHEN   903
                    COMPUTE  RECE200-NYUKHNKEI2  =
                             RECE200-NYUKHNKEI2  -  NACCT-ZAITEN
             END-EVALUATE
           ELSE
             EVALUATE   WRK-DAY1-NYUTENTTLKBN(IDY)
              WHEN   903
                    COMPUTE  RECE200-NYUKHNKEI2  =
                             RECE200-NYUKHNKEI2  +  NACCT-ZAITEN
             END-EVALUATE
           END-IF
      *
           IF  (WRK-DAY1(IDY)             = ZERO)
           AND (RECE200-NYUINDAY-KBN(IDY) = "1" )
                    COMPUTE  RECE200-NYUTOKKEI2  =
                             RECE200-NYUTOKKEI2  +  NACCT-ZAITEN
           END-IF
      *
           .
       2001125-RECE200-TENSOU22-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と加減算点数の取得
      **************************************************************
       2001123-NYUINKHN-GET-SEC           SECTION.
      *
           IF  ( NACCT-SRYKBN     = "90"      )
           OR  ( NACCT-SRYCDTOTAL = 190799410 )
           OR  ( NACCT-SRYCDTOTAL = 190799510 )
                 MOVE   903   TO  WRK-NYUTENTTLKBN                      
           ELSE                                                         
                 MOVE   920   TO  WRK-NYUTENTTLKBN                      
           END-IF                                                       
      *
           MOVE    ZERO               TO   WRK-KHNTEN
           MOVE    ZERO               TO   WRK-KHNTEN-MOTO
           MOVE    ZERO               TO   WRK-KHNTEN-GEN
           MOVE    ZERO               TO   WRK-KHN-ZAITEN
           MOVE    ZERO               TO   WRK-KHN-ZAITEN-MOTO
           MOVE    ZERO               TO   WRK-KHN-ZAITEN-GEN
           MOVE    SPACE              TO   WRK-RYAKUSHO
           MOVE    ZERO               TO   NYUINACT-IDX2
           MOVE    ZERO               TO   WRK-KSNTEN
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
           MOVE    ZERO               TO   WRK-HYOKETU-TEN
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =    ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
      *
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE      ZERO          TO  FLG-NYUKHN
               PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =   1)    OR
                        (NYUINACT-IDX       >   5)    OR
                        (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    NSRY-SRYCD(NYUINACT-IDX) TO   TNS-SRYCD
                     MOVE    NACCT-SRYYM        TO  TNS-YUKOSTYMD(1:6)
                     MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
                     MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
                     PERFORM  900-TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
                         IF   TNS-DATAKBN    =    1
                              MOVE   TNS-TEN   TO   WRK-KHNTEN-MOTO
      *                       夜間看護体制特定日減算
                              COMPUTE  WRK-KHNTEN-GEN   =
                                      (WRK-KHNTEN-MOTO  * 0.95)
      *                       看護比率基準未達時の入院基本点数の取得
                              IF    RECE200-SRYYM  <=  "201203"
                               PERFORM 20011231-GAIHAKU-NYUKHN-SET-SEC
                              END-IF
                              IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                               TO   WRK-KHNTEN-MOTO
                              END-IF
                              MOVE   NYUINACT-IDX
                                               TO   NYUINACT-IDX2
      ***                     MOVE   1         TO   FLG-NYUKHN
                         ELSE
                              EVALUATE  TNS-TENSIKIBETU
                                WHEN     3
                                    MOVE  TNS-TEN  TO  WRK-KSNTEN
                                WHEN     8
                                    COMPUTE  WRK-KSNTEN = TNS-TEN * -1
                                WHEN     6
                                    MOVE  TNS-TEN  TO  WRK-HYOKETU-TEN
                              END-EVALUATE
                         END-IF
                     END-IF
               END-PERFORM
      *
               COMPUTE  WRK-KHN-ZAITEN-MOTO     =
                        NACCT-ZAITEN
               COMPUTE  WRK-KHN-ZAITEN-GEN      =
                        WRK-KHNTEN-GEN          +
                        WRK-KSNTEN
               IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *          標欠：％減算
                 COMPUTE    WRK-KEISAN2             =
                          ( WRK-KHN-ZAITEN-GEN      -
                           (WRK-KHN-ZAITEN-GEN      *
                            (WRK-HYOKETU-TEN   /  100))) + 0.5
                 MOVE   WRK-KEISAN2   TO   WRK-KHN-ZAITEN-GEN
               END-IF
      *
               IF    WRK-NYUTENTTLKBN     =   920
               AND   NYUINACT-IDX2    NOT =   ZERO
      *
                 MOVE    ZERO          TO   WRK-TOKUREI-FLG
                 MOVE    SPACE         TO   NYURYAKUSHO-REC
                 INITIALIZE                 NYURYAKUSHO-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2) 
                                       TO   NYURYAKUSHO-SRYCD
                                            WRK-TOKUREI-SRYCD2
                 MOVE    SPACE         TO   WRK-TOKUREI-SRYCD
                 PERFORM  500-RYAKUSYO-KEISO-CHECK-SEC
                 IF   WRK-TOKUREI-SRYCD  NOT = SPACE
                   MOVE  WRK-TOKUREI-SRYCD
                                       TO   NYURYAKUSHO-SRYCD
                 END-IF
      *
                 MOVE    NACCT-SRYYM   TO   NYURYAKUSHO-YUKOSTYMD(1:6)
                 MOVE    "01"          TO   NYURYAKUSHO-YUKOSTYMD(7:2)
                 MOVE    NYURYAKUSHO-YUKOSTYMD
                                       TO   NYURYAKUSHO-YUKOEDYMD
                 PERFORM  900-NYURYAKUSHO-READ-SEC
                 IF       FLG-NYURYAKUSHO    =    ZERO
                   MOVE     NYURYAKUSHO-RYAKUSHO
                                       TO  WRK-RYAKUSHO
                 END-IF
      *
               END-IF
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001123-NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    外泊時の入院基本料取得
      **************************************************************
       20011231-GAIHAKU-NYUKHN-SET-SEC           SECTION.
      *
           EVALUATE  TNS-SRYCD
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        診療所老人医療管理料
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        老人性認知症疾患治療病棟入院料１
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        老人性認知症疾患治療病棟入院料２
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (RECE200-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE    WRK-GAIHAKU-SRYCD  TO  TNS-SRYCD
               MOVE    NACCT-SRYYM        TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD      TO  TNS-YUKOEDYMD
               PERFORM  900-TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                        MOVE   TNS-TEN    TO  WRK-GAIHAKU-KHNTEN
               END-IF                  
           END-IF
      *
           .
       20011231-GAIHAKU-NYUKHN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       2001126-RECE200-TENSOU23-SEC                 SECTION.
      *
           PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                       UNTIL    ACCT-IDX    >    31
      *
              IF  ((WRK-DAY1(ACCT-IDX)   NOT =  ZERO    ) OR
                   (RECE200-NYUINDAY-KBN(ACCT-IDX) = "1"))
              AND ( WRK-DAY5(ACCT-IDX) = RECE200-HKNCOMBI)
              AND ( ACCT-DAY(1 ACCT-IDX) NOT =  ZERO     )
      *
                 IF   ACCT-ZAIKBN  =    2
                   COMPUTE  RECE200-NYUTOKKEI2  =
                            RECE200-NYUTOKKEI2  -
                           (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                 ELSE
                  IF   ACCT-SRYCDTOTAL  =  190182590  OR  190182690
                   EVALUATE   WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                      WHEN   903
                       COMPUTE  RECE200-NYUKHNKEI2  =
                                RECE200-NYUKHNKEI2  -
                               (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                   END-EVALUATE
                  ELSE
                   EVALUATE   WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                      WHEN   903
                       COMPUTE  RECE200-NYUKHNKEI2  =
                                RECE200-NYUKHNKEI2  +
                               (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                   END-EVALUATE
                  END-IF
                 END-IF
      *
                 IF  (WRK-DAY1(ACCT-IDX)             = ZERO)
                 AND (RECE200-NYUINDAY-KBN(ACCT-IDX) = "1" )
                      IF ACCT-ZAIKBN = 2
                       COMPUTE  RECE200-NYUTOKKEI2  =
                                RECE200-NYUTOKKEI2  -
                               (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                      ELSE
                       COMPUTE  RECE200-NYUTOKKEI2  =
                                RECE200-NYUTOKKEI2  +
                               (ACCT-ZAITEN * ACCT-DAY(1 ACCT-IDX))
                      END-IF
                 END-IF
      *
              END-IF
      *
           END-PERFORM
      *
           .
       2001126-RECE200-TENSOU23-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       200113-RECE200-HENSYU3-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >     31
                         IF  WRK-DAY5(IDY)  =  RECE200-HKNCOMBI
                              IF   WRK-DAY1-NYUTENTTLKBN(IDY) = 903
                              AND  NACCT-ZAITEN    NOT =    ZERO
                              AND  NACCT-DAY(IDY)  NOT =    ZERO
                                   PERFORM   2001131-NYUINKHN-GET3-SEC
                                   MOVE    31    TO    IDY
                              END-IF
                         END-IF
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key7"              TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
              END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院略称の取得（夜間看護体制特定日減算）
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
                IF   (WRK-DAY1(IDY)               NOT = ZERO )
                AND  (RECE200-KANGOGEN-SRYCD(IDY) NOT = SPACE)
                  IF  WRK-DAY5(IDY) =  RECE200-HKNCOMBI
                     MOVE      RECE200-KANGOGEN-SRYCD(IDY)
                                 TO    WRK-SRYCD
                     PERFORM   2001134-YAGEN-GET3-SEC
                  END-IF
                END-IF
           END-PERFORM
      *
      *    入院略称の取得（特定時間退院減算・特定曜日入退院減算）
           PERFORM  VARYING  IDY    FROM    1   BY  1
                     UNTIL   IDY     >     31
                IF   (WRK-DAY1(IDY)   NOT  =  ZERO                    )
      ***       AND  (WRK-DAY2(IDY)        =  8 OR 9 OR 11 OR 13 OR 17)
                AND  (WRK-DAY2(IDY)        =  8 OR 9 OR 13 OR 17      )
                  IF  WRK-DAY5(IDY) =  RECE200-HKNCOMBI
                     PERFORM   2001133-GENSAN-GET3-SEC
                  END-IF
                END-IF
           END-PERFORM
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE200-HOSPNUM       TO      NACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN     TO      NACCT-NYUGAIKBN
           MOVE    RECE200-PTID          TO      NACCT-PTID
           MOVE    RECE200-SRYYM         TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "6"
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                             UNTIL   IDY     >     31
      *
                       IF   (WRK-DAY1(IDY)    NOT  =  ZERO  )
                       OR   (RECE200-NYUINDAY-KBN(IDY) = "1")
                         IF   WRK-DAY5(IDY)  =  RECE200-HKNCOMBI
                              IF   (NACCT-ZAITEN    NOT =    ZERO   )
                              AND  (NACCT-DAY(IDY)  NOT =    ZERO   )
                              AND  ((WRK-DAY2(IDY)      =    ZERO  ) OR
                                    (WRK-DAY2(IDY)     >=     4    ))
                              AND  (WRK-DAY1-NYUTENTTLKBN(IDY) = 903)
                                   PERFORM   2001131-NYUINKHN-GET3-SEC
                                   MOVE    31    TO    IDY
                              END-IF
                         END-IF
                       END-IF
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                 MOVE    "key7"               TO  MCP-PATHNAME
                 PERFORM        900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE200-HOSPNUM      TO      ACCT-HOSPNUM
           MOVE    RECE200-NYUGAIKBN    TO      ACCT-NYUGAIKBN
           MOVE    RECE200-PTID         TO      ACCT-PTID
           MOVE    RECE200-SRYYM        TO      ACCT-SRYYM
           MOVE    RECE200-HKNCOMBI     TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   IF   (ACCT-ZAITEN    NOT =    ZERO)  AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)  AND
                        (ACCT-JIHOKBN       =   SPACE)  AND
                        (ACCT-ZAIKBN        =    ZERO)
                     IF   ACCT-SRYKBN  =  "90" OR "92"
                        PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                 UNTIL  ACCT-IDX  >  31
                          IF  ((WRK-DAY1(ACCT-IDX) NOT = ZERO ) OR
                               (RECE200-NYUINDAY-KBN(ACCT-IDX)
                                                         = "1"))
                          AND ( WRK-DAY5(ACCT-IDX) =
                                               RECE200-HKNCOMBI)
                          AND ( ACCT-DAY(1 ACCT-IDX) NOT = ZERO)
                          AND ( WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
                                                          = 903)
                                PERFORM 2001132-ACCTKHN-GET3-SEC
                                MOVE  31  TO  ACCT-IDX
                          END-IF
                        END-PERFORM
                     END-IF
                   END-IF
                   MOVE    "tbl_sryacct"     TO      MCP-TABLE
                   MOVE    "key22"           TO      MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key22"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    特別な関係にある他院
      *
           IF   (RECE200-TAINFLG         =  "1"     )
           AND  (RECE200-SRYYM          >=  "201804")
             MOVE     ZERO     TO    FLG-ARI
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL  (ID1                     >  20  )
                        OR    (RECE200-NYUSYUBETU(ID1) = SPACE)
                        OR    (FLG-ARI                 =   1  )
               IF  RECE200-NYUSYUBETU(ID1)(1:4) = "特別"
                 MOVE  1   TO   FLG-ARI
               END-IF
             END-PERFORM
             IF    FLG-ARI  =  ZERO
             AND   ID1     <=   20
               MOVE   "特別"   TO   RECE200-NYUSYUBETU(ID1)(1:4)
             END-IF
           END-IF
      *
           .
       200113-RECE200-HENSYU3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得
      **************************************************************
       2001131-NYUINKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "tbl_nyuinact"   TO   MCP-TABLE
               MOVE    "key9"           TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
      *
                 PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                      UNTIL  (NYUINACT-IDX       >   5)    OR
                             (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
      *
                       MOVE    ZERO           TO  WRK-TOKUREI-FLG
                       MOVE    NSRY-SRYCD(NYUINACT-IDX) 
                                              TO  WRK-SRYCD
                                                  WRK-TOKUREI-SRYCD2
                       MOVE    SPACE          TO  WRK-TOKUREI-SRYCD
                       IF   NACCT-ZAISKBKBN    =  "1"
                       AND  NYUINACT-IDX       =   1
                         PERFORM  500-RYAKUSYO-KEISO-CHECK-SEC
                       END-IF
                       IF   WRK-TOKUREI-SRYCD  NOT = SPACE
                         MOVE     WRK-TOKUREI-SRYCD
                                              TO  WRK-SRYCD
                       END-IF
      *
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD-TAIHI(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD-TAIHI(7:2)
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   500-RYAKUSYO-HENSYU-SEC
      *
                 END-PERFORM
      *
                 MOVE    "tbl_nyuinact"     TO   MCP-TABLE
                 MOVE    "key9"             TO   MCP-PATHNAME
                 PERFORM        900-NYUINACT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001131-NYUINKHN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（診療会計）
      **************************************************************
       2001132-ACCTKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      *
                       MOVE    ZERO               TO  WRK-TOKUREI-FLG
                       MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-SRYCD
                       MOVE    ACCT-SRYYM         TO  WRK-SRYYMD(1:6)
                       MOVE    "01"               TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   500-RYAKUSYO-HENSYU-SEC
      *
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO   MCP-TABLE
                   MOVE    "key9"            TO   MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001132-ACCTKHN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（特定時間退院減算・特定曜日入退院減算）
      **************************************************************
       2001133-GENSAN-GET3-SEC           SECTION.
      *
           EVALUATE    WRK-DAY2(IDY)
              WHEN          8
      ***     WHEN         11
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL  (ID1                     >  20  )
                            OR    (RECE200-NYUSYUBETU(ID1) = SPACE)
                            OR    (FLG-ARI                 =   1  )
                   IF  RECE200-NYUSYUBETU(ID1)(1:6)  =  "午前減"
                     MOVE     1      TO   FLG-ARI
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI  =  ZERO
                 AND   ID1     <=   20
                     MOVE  "午前減"  TO   RECE200-NYUSYUBETU(ID1)(1:6)
                 END-IF
              WHEN          9
              WHEN         13
              WHEN         17
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL  (ID1                     >  20  )
                            OR    (RECE200-NYUSYUBETU(ID1) = SPACE)
                            OR    (FLG-ARI                 =   1  )
                   IF  RECE200-NYUSYUBETU(ID1)(1:6)  =  "土日減"
                     MOVE     1      TO   FLG-ARI
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI  =  ZERO
                 AND   ID1     <=   20
                     MOVE  "土日減"  TO   RECE200-NYUSYUBETU(ID1)(1:6)
                 END-IF
           END-EVALUATE
      *
           .
       2001133-GENSAN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（夜間看護体制特定日減算）
      **************************************************************
       2001134-YAGEN-GET3-SEC           SECTION.
      *
           MOVE     ZERO     TO    FLG-ARI
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL  (ID1                     >  20  )
                      OR    (RECE200-NYUSYUBETU(ID1) = SPACE)
                      OR    (FLG-ARI                 =   1  )
             EVALUATE   WRK-SRYCD
      *        夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190200970"
                 IF  RECE200-NYUSYUBETU(ID1)(1:14)
                                             = "一般夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        療養病棟・夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190214470"
                 IF  RECE200-NYUSYUBETU(ID1)(1:14)
                                             = "一般夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（結核病棟入院基本料）
               WHEN     "190205270"
                 IF  RECE200-NYUSYUBETU(ID1)(1:12)
                                             = "結夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（精神病棟入院基本料）
               WHEN     "190205370"
                 IF  RECE200-NYUSYUBETU(ID1)(1:12)
                                             = "精夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（専門病院入院基本料）
               WHEN     "190205470"
                 IF  RECE200-NYUSYUBETU(ID1)(1:12)
                                             = "専夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（障害者施設等入院基本料）
               WHEN     "190205870"
                 IF  RECE200-NYUSYUBETU(ID1)(1:12)
                                             = "障夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
      *        夜間看護体制特定日減算（地域包括ケア病棟入院料）
               WHEN     "190212670"
                 IF  RECE200-NYUSYUBETU(ID1)(1:12)
                                             = "包夜看特定減"
                          MOVE  1   TO   FLG-ARI
                 END-IF
             END-EVALUATE
           END-PERFORM
           IF    FLG-ARI  =  ZERO
           AND   ID1     <=   20
             EVALUATE   WRK-SRYCD
      *        夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190200970"
                 MOVE   "一般夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:14)
      *        療養病棟・夜間看護体制特定日減算（一般病棟入院基本料）
               WHEN     "190214470"
                 MOVE   "一般夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:14)
      *        夜間看護体制特定日減算（結核病棟入院基本料）
               WHEN     "190205270"
                 MOVE   "結夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（精神病棟入院基本料）
               WHEN     "190205370"
                 MOVE   "精夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（専門病院入院基本料）
               WHEN     "190205470"
                 MOVE   "専夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（障害者施設等入院基本料）
               WHEN     "190205870"
                 MOVE   "障夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:12)
      *        夜間看護体制特定日減算（地域包括ケア病棟入院料）
               WHEN     "190212670"
                 MOVE   "包夜看特定減"
                            TO   RECE200-NYUSYUBETU(ID1)(1:12)
             END-EVALUATE
           END-IF
      *
           .
       2001134-YAGEN-GET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数の項目転送（Ｆ００２）
      *****************************************************************
       200116-JNISSU-SET-SEC                    SECTION.
      *
      *    実日数から減する
           PERFORM    VARYING    IDY    FROM    1   BY   1
                       UNTIL     IDY    >      31
             IF  (TBL-JNISSU-DAY(IDY)      NOT =  SPACE)
             AND (TBL-JNISSU-DAY-GEN(IDY)  NOT =  SPACE)
                  MOVE  SPACE  TO  TBL-JNISSU-DAY(IDY)
             END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                       UNTIL     IDY    >      31
             IF   TBL-JNISSU-DAY(IDY)      NOT =  SPACE
                  ADD    1     TO   RECE200-JNISSU
                  MOVE  "1"    TO   RECE200-JNISSUTBL(IDY)
             END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM   1   BY   1
                       UNTIL     IDY    >     20
              IF   RECE200-NYUTOKRYO(IDY)  =  999999999
                    MOVE    ZERO    TO   RECE200-NYUTOKRYO(IDY)
              END-IF
              IF   RECE200-NYUTOKRYO-RYAKU(IDY)  
                                           =  "９９９９９９９９９９"
                    MOVE    SPACE   TO   RECE200-NYUTOKRYO-RYAKU(IDY)
              END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM   1   BY   1
                       UNTIL     IDY    >      4
              COMPUTE  WRK-KOGAI-GOKEI1(IDY) = WRK-KOGAI-TEN(IDY)
                                             * WRK-KOGAI-ZAIKAISU1(IDY)
              COMPUTE  WRK-KOGAI-GOKEI2(IDY) = WRK-KOGAI-TEN(IDY)
                                             * WRK-KOGAI-ZAIKAISU2(IDY)
              COMPUTE  WRK-KOGAI-GOKEI3(IDY) = WRK-KOGAI-TEN(IDY)
                                             * WRK-KOGAI-ZAIKAISU3(IDY)
              COMPUTE  WRK-KOGAI-GOKEI4(IDY) = WRK-KOGAI-TEN(IDY)
                                             * WRK-KOGAI-ZAIKAISU4(IDY)
      *
              COMPUTE  RECE200-NYUKHNKEI2    = RECE200-NYUKHNKEI2
                                             - WRK-KOGAI-GOKEI1(IDY)
              COMPUTE  RECE200-NYUTOKKEI2    = RECE200-NYUTOKKEI2
                                             - WRK-KOGAI-GOKEI3(IDY)
              COMPUTE  RECE200-NYUKHNKEI2    = RECE200-NYUKHNKEI2
                                             - WRK-KOGAI-GOKEI4(IDY)
           END-PERFORM
      *
      *    公害入院療養指導
           IF    (WRK-KOGAI-TEN(1)  NOT = ZERO)
           OR    (WRK-KOGAI-TEN(2)  NOT = ZERO)
             IF   WRK-KOGAI-TEN(1)  NOT = ZERO
                MOVE     WRK-KOGAI-TEN(1)    TO  RECE200-KOGAISIDORYO(1)
                COMPUTE  RECE200-KOGAISIDONISSU(1)
                                  =  WRK-KOGAI-ZAIKAISU1(1)
                                  +  WRK-KOGAI-ZAIKAISU2(1)
                                  +  WRK-KOGAI-ZAIKAISU3(1)
                                  +  WRK-KOGAI-ZAIKAISU4(1)
                COMPUTE  RECE200-KOGAISIDOKEI(1)
                                  =  WRK-KOGAI-GOKEI1(1)
                                  +  WRK-KOGAI-GOKEI2(1)
                                  +  WRK-KOGAI-GOKEI3(1)
                                  +  WRK-KOGAI-GOKEI4(1)
                IF   WRK-KOGAI-TEN(2)  NOT = ZERO
                  MOVE     WRK-KOGAI-TEN(2)  TO  RECE200-KOGAISIDORYO(2)
                  COMPUTE  RECE200-KOGAISIDONISSU(2)
                                  =  WRK-KOGAI-ZAIKAISU1(2)
                                  +  WRK-KOGAI-ZAIKAISU2(2)
                                  +  WRK-KOGAI-ZAIKAISU3(2)
                                  +  WRK-KOGAI-ZAIKAISU4(2)
                  COMPUTE  RECE200-KOGAISIDOKEI(2)
                                  =  WRK-KOGAI-GOKEI1(2)
                                  +  WRK-KOGAI-GOKEI2(2)
                                  +  WRK-KOGAI-GOKEI3(2)
                                  +  WRK-KOGAI-GOKEI4(2)
                END-IF
             ELSE
                MOVE     WRK-KOGAI-TEN(2)    TO  RECE200-KOGAISIDORYO(1)
                COMPUTE  RECE200-KOGAISIDONISSU(1)
                                  =  WRK-KOGAI-ZAIKAISU1(2)
                                  +  WRK-KOGAI-ZAIKAISU2(2)
                                  +  WRK-KOGAI-ZAIKAISU3(2)
                                  +  WRK-KOGAI-ZAIKAISU4(2)
                COMPUTE  RECE200-KOGAISIDOKEI(1)
                                  =  WRK-KOGAI-GOKEI1(2)
                                  +  WRK-KOGAI-GOKEI2(2)
                                  +  WRK-KOGAI-GOKEI3(2)
                                  +  WRK-KOGAI-GOKEI4(2)
             END-IF
           ELSE
             IF   WRK-KOGAI-TEN(3)  NOT = ZERO
                MOVE     WRK-KOGAI-TEN(3)    TO  RECE200-KOGAISIDORYO(1)
                COMPUTE  RECE200-KOGAISIDONISSU(1)
                                  =  WRK-KOGAI-ZAIKAISU1(3)
                                  +  WRK-KOGAI-ZAIKAISU2(3)
                                  +  WRK-KOGAI-ZAIKAISU3(3)
                                  +  WRK-KOGAI-ZAIKAISU4(3)
                COMPUTE  RECE200-KOGAISIDOKEI(1)
                                  =  WRK-KOGAI-GOKEI1(3)
                                  +  WRK-KOGAI-GOKEI2(3)
                                  +  WRK-KOGAI-GOKEI3(3)
                                  +  WRK-KOGAI-GOKEI4(3)
             END-IF
           END-IF
      *
      *    清浄空気室管理料
           IF   WRK-KOGAI-TEN(4)  NOT = ZERO
             MOVE     WRK-KOGAI-TEN(4)    TO  RECE200-SEIKANRYO
             COMPUTE  RECE200-SEIKANNISSU  =  WRK-KOGAI-ZAIKAISU1(4)
                                           +  WRK-KOGAI-ZAIKAISU2(4)
                                           +  WRK-KOGAI-ZAIKAISU3(4)
                                           +  WRK-KOGAI-ZAIKAISU4(4)
             COMPUTE  RECE200-SEIKANKEI    =  WRK-KOGAI-GOKEI1(4)
                                           +  WRK-KOGAI-GOKEI2(4)
                                           +  WRK-KOGAI-GOKEI3(4)
                                           +  WRK-KOGAI-GOKEI4(4)
           END-IF
      *
           .
       200116-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       200117-RECE200-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE200-REC
      *
           ADD     1                   TO  CNT-MOD
      *
           .
       200117-RECE200-REWRITE-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM   900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
                       PERFORM   900-CALL-ORCSJOB-SEC
                   END-IF
      *
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE200 IN  :" CNT-IN
           DISPLAY "        MOD :" CNT-MOD
           DISPLAY "*** ORCR1210 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院略称の編集
      *****************************************************************
       500-RYAKUSYO-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE         TO   NYURYAKUSHO-REC
           INITIALIZE                 NYURYAKUSHO-REC
      *
           MOVE    WRK-SRYCD     TO   NYURYAKUSHO-SRYCD
           MOVE    WRK-SRYYMD    TO   NYURYAKUSHO-YUKOSTYMD
                                      NYURYAKUSHO-YUKOEDYMD
           PERFORM  900-NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
              MOVE     ZERO     TO    FLG-ARI
              PERFORM  VARYING  ID1    FROM    1   BY  1
                    UNTIL   ID1                     > 20
                      OR    RECE200-NYUSYUBETU(ID1) = SPACE
                      OR    FLG-ARI                 = 1
                 IF  RECE200-NYUSYUBETU(ID1)
                            =  NYURYAKUSHO-RYAKUSHO
                     MOVE  1   TO   FLG-ARI
                 END-IF
              END-PERFORM
              IF  FLG-ARI  =  ZERO
              AND ID1     <=   20
                     MOVE   NYURYAKUSHO-RYAKUSHO
                               TO   RECE200-NYUSYUBETU(ID1)
              END-IF
           ELSE
              IF   ( RECE200-SRYYM  >= "200604"    )
              AND  ( WRK-SRYCD       = "190123250" OR
                                       "190123350" OR
                                       "190123450" OR
                                       "190123550" )
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                      UNTIL   ID1                     > 20
                        OR    RECE200-NYUSYUBETU(ID1) = SPACE
                        OR    FLG-ARI                 = 1 
                   EVALUATE   WRK-SRYCD
                     WHEN     "190123250"
                      IF  RECE200-NYUSYUBETU(ID1)(1:8) = "一般経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123350"
                      IF  RECE200-NYUSYUBETU(ID1)(1:8) = "結核経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123450"
                      IF  RECE200-NYUSYUBETU(ID1)(1:8) = "精神経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190123550"
                      IF  RECE200-NYUSYUBETU(ID1)(1:8) = "障害経過"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-EVALUATE
                END-PERFORM  
                IF  FLG-ARI  =  ZERO
                AND ID1     <=   20
                   EVALUATE   WRK-SRYCD
                     WHEN     "190123250"
                      MOVE   "一般経過"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123350"
                      MOVE   "結核経過"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123450"
                      MOVE   "精神経過"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190123550"
                      MOVE   "障害経過"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:8)
                   END-EVALUATE
                END-IF
              END-IF
              IF   ( RECE200-SRYYM  >= "200807"    )
              AND  ( WRK-SRYCD       = "190130810" OR
                                       "190131010" OR
                                       "190131110" )
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                      UNTIL   ID1                     > 20
                        OR    RECE200-NYUSYUBETU(ID1) = SPACE
                        OR    FLG-ARI                 = 1 
                   EVALUATE   WRK-SRYCD
                     WHEN     "190130810"
                      IF  RECE200-NYUSYUBETU(ID1)(1:8) = "一般１０"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190131010"
                      IF  RECE200-NYUSYUBETU(ID1)(1:6) = "結１０"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                     WHEN     "190131110"
                      IF  RECE200-NYUSYUBETU(ID1)(1:6) = "専１０"
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-EVALUATE
                END-PERFORM  
                IF  FLG-ARI  =  ZERO
                AND ID1     <=   20
                   EVALUATE   WRK-SRYCD
                     WHEN     "190130810"
                      MOVE   "一般１０"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:8)
                     WHEN     "190131010"
                      MOVE   "結１０"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:6)
                     WHEN     "190131110"
                      MOVE   "専１０"
                                 TO   RECE200-NYUSYUBETU(ID1)(1:6)
                   END-EVALUATE
                END-IF
              END-IF
           END-IF
      *
           .
       500-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院略称（経措）チェック
      *****************************************************************
       500-RYAKUSYO-KEISO-CHECK-SEC                SECTION.
      *
           IF   (RECE200-SRYYM    >=  "201404")
           AND  (RECE200-SRYYM    <=  "201406")
           AND  (WRK-HOSPSBT       =   1      )
      *
             INITIALIZE    ORCSEIYOUKKS-AREA
             MOVE   RECE200-SRYYM         TO  LNK-EIYOUKKS-SANTEIYM
             MOVE   WRK-TOKUREI-SRYCD2    TO  LNK-EIYOUKKS-SRYCD
             CALL   "ORCSEIYOUKKS" USING  ORCSEIYOUKKS-AREA
                                          SPA-AREA
             IF     LNK-EIYOUKKS-RECE-CHGSRYCD  NOT = SPACE
               MOVE LNK-EIYOUKKS-RECE-CHGSRYCD
                                          TO  WRK-TOKUREI-SRYCD
             END-IF
             IF     LNK-EIYOUKKS-RC  =  9
               IF   WRK-TOKUREI-SRYCD2(1:3)  =  "188"
                  CONTINUE
               ELSE
                  MOVE    1               TO  WRK-TOKUREI-FLG
               END-IF
             END-IF
      *
           END-IF
      *
           .
       500-RYAKUSYO-KEISO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    入院診療行為マスター読込
      **************************************************************
       900-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    入院略称マスター読込（主キー）
      *****************************************************************
       900-NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE     ZERO       TO    FLG-NYURYAKUSHO
      *
           PERFORM  910-NYURYAKUWRK-CHK-SEC
      *
           IF      FLG-NYURYAKUWRK     =   ZERO
               MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-DBFETCH-SEC
                   IF      MCP-RC               =   ZERO
                       MOVE    MCPDATA-REC      TO  NYURYAKUSHO-REC
                       IF  WRK-TOKUREI-FLG  =  1
                          MOVE    SPACE   TO   WRK-TOKUREI-RYAKUSHO
                          STRING  NYURYAKUSHO-RYAKUSHO
                                    DELIMITED   BY  SPACE
                                  "経措"
                                    DELIMITED   BY  SIZE
                                  INTO  WRK-TOKUREI-RYAKUSHO
                          END-STRING
                          MOVE    SPACE   TO   NYURYAKUSHO-RYAKUSHO
                          MOVE    WRK-TOKUREI-RYAKUSHO
                                          TO   NYURYAKUSHO-RYAKUSHO
                       END-IF
                       MOVE    ZERO             TO  FLG-NYURYAKUSHO
                       PERFORM  910-NYURYAKUWRK-HEN-SEC
                   ELSE
                       MOVE    1                TO  FLG-NYURYAKUSHO
                       INITIALIZE                   NYURYAKUSHO-REC
                   END-IF
               ELSE
                   MOVE    1                    TO  FLG-NYURYAKUSHO
                   INITIALIZE                       NYURYAKUSHO-REC
               END-IF
      *
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       900-NYURYAKUSHO-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院略称マスター退避ワークチェック処理
      *****************************************************************
       910-NYURYAKUWRK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-NYURYAKUWRK
      *
           PERFORM VARYING WRK-NYURYAKU-IDX  FROM    1   BY  1
             UNTIL ( WRK-NYURYAKU-IDX  >   WRK-NYURYAKU-CONST-MAX ) OR
                   ( FLG-NYURYAKUWRK   =     1 )      OR
                   ( WRK-NYURYAKU-IDX  >   200 )
               IF    ( WRK-NYURYAKU-SRYCD (WRK-NYURYAKU-IDX)
                                       =   NYURYAKUSHO-SRYCD )
                 AND ( WRK-NYURYAKU-YUKOSTYMD (WRK-NYURYAKU-IDX)
                                       <=  NYURYAKUSHO-YUKOSTYMD)
                 AND ( WRK-NYURYAKU-YUKOEDYMD (WRK-NYURYAKU-IDX)
                                       >=  NYURYAKUSHO-YUKOSTYMD)
                   MOVE    1           TO  FLG-NYURYAKUWRK
                   MOVE    WRK-NYURYAKU-DATA (WRK-NYURYAKU-IDX)
                                       TO  NYURYAKUSHO-REC
               END-IF
           END-PERFORM
      *
           .
       910-NYURYAKUWRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院略称マスター退避ワーク編集処理
      *****************************************************************
       910-NYURYAKUWRK-HEN-SEC               SECTION.
      *
           IF    ( WRK-NYURYAKU-CONST-MAX  >   WRK-NYURYAKU-MAX )
               COMPUTE WRK-NYURYAKU-MAX    =  WRK-NYURYAKU-MAX   +   1
               MOVE    NYURYAKUSHO-SRYCD   TO  WRK-NYURYAKU-SRYCD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-YUKOSTYMD
                                           TO  WRK-NYURYAKU-YUKOSTYMD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-YUKOEDYMD
                                           TO  WRK-NYURYAKU-YUKOEDYMD
                                                   (WRK-NYURYAKU-MAX)
               MOVE    NYURYAKUSHO-REC     TO  WRK-NYURYAKU-DATA
                                                   (WRK-NYURYAKU-MAX)
           END-IF
      *
           .
       910-NYURYAKUWRK-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-1001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE200-READ-SEC            SECTION.
      *
           READ    RECE200-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE200
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE200 "  " PTNUM=" RECE200-PTNUM
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE200-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ）
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               CONTINUE
           ELSE
      *        医療機関ＩＤ編集
               MOVE  ZERO           TO  WRK-HOSPSBT
               MOVE  ZERO           TO  FLG-SET
               MOVE  RECE200-SRYYM  TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                     UNTIL  ( SYSKAN-IDX              >  100  )
                      OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                      OR    ( FLG-SET                 =   1   )
                 MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                 IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                 AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1               TO    FLG-SET
                    MOVE   SYS-1001-HOSPSBT  TO    WRK-HOSPSBT
                 END-IF
               END-PERFORM
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
       100-DBOPEN-SEC                SECTION.
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
       900-DBDISCONNECT-SEC            SECTION.
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
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
