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
       PROGRAM-ID.             ORCR0400.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                                対象患者抽出（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    02.09.09  自賠責対応
      *  01.00.02    NACL-藤原    03.02.24  最新のレセプトのみＤＢに残す
      *  01.00.03    NACL-門脇    03.03.10  診療科１０から２０へ
      *  01.00.04    NACL-門脇    04.01.14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *                                     診療会計テーブルの読み込みを
      *                                     グループ化に変更
      *                                     SRYACCT-KEY29 -> SRYACCT-KEY36
      *                                     SRYACCT-KEY30 -> SRYACCT-KEY37
      *  01.01.02    NACL-藤原    05/01/20  受診日指定対応
      *  01.01.03    NACL-藤原    05/02/10  訂正日指定対応
      *
      *  03.03.01    NACL-藤原    06/09/19  月途中での氏名変更対応
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *  03.04.02    NACL-門脇    06/12/01  自賠責もこのプログラムで抽出
      *  03.04.03    NACL-門脇    06/12/20  自賠責健保準拠対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.05.01    NACL-門脇    12/02/13  アフターケア診療科情報編集修正
      *
      *  04.07.01    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE40-FILE     ASSIGN  RECE40PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE40-KEY
                                   FILE    STATUS  IS  STS-RECE40.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE40-FILE.
       01  RECE40-REC. 
           COPY    "CPRCF040.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE40//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE40                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01). 
           03  FLG-RECE40                              PIC 9(01).
           03  FLG-PTRSIINF                            PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-BTPARA                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                                     PIC 9(03). 
           03  IDX                                     PIC 9(03). 
           03  IDX1                                    PIC 9(03). 
           03  IDY                                     PIC 9(03). 
           03  IDZ                                     PIC 9(03).
           03  TBL-MAX                                 PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
               05  KEY-N-SRYKA                         PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
               05  KEY-O-SRYKA                         PIC X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC                         OCCURS  100.
               05  TBL-SYS4001                         PIC X(610).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-SYOKBN                     PIC  X(01).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
               05  WRK-PARA-PTID                       PIC  X(10).
               05  WRK-PARA-SRYYM-O                    PIC  X(06).
           03  WRK-PARA-JIBAIPRTKBN                    PIC  X(01).
           03  WRK-PARA-PRTKBN                         PIC  X(01).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-SRYACCT-PATHNAME    PIC X(64).
      *
           03  WRK-HKNNUM              PIC 9(03).     
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
           03  WRK-SYOKBN              PIC X(01).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
           03  WRK-UPDKBN              PIC X(01).
           03  WRK-TEISEIKBN           PIC X(01).
           03  WRK-ROUSAIKBN           PIC X(01).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).     
      *
           03  WRK-CONS-HKNNUM         PIC X(03).
      *
           03  WRK-JIBAIKBN            PIC X(01).
      *
       01  WRK-ERRMSG-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      * 
       01  WRK-RECE40-REC. 
           COPY    "CPRCF040.INC"  REPLACING  //RECE40//
                                   BY         //WRK-RECE40//.
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-201707-X.
               05  WRK-CONS-SRYYM-201707
                                   PIC 9(06)   VALUE   201707.
      *
           COPY    "MCPAREA".
      *     
           COPY    "ORCA-DBMETA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
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
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM    =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN     =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM      =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN     =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-RECEKBN    =" WRK-PARA-RECEKBN
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           EVALUATE  WRK-PARA-RECEKBN
             WHEN      "3"
                 MOVE    "971"         TO  WRK-CONS-HKNNUM
             WHEN      "4"
             WHEN      "5"
                 MOVE    "973"         TO  WRK-CONS-HKNNUM
             WHEN      "8"
                 MOVE    "970"         TO  WRK-CONS-HKNNUM
           END-EVALUATE
      *
           MOVE    SPACE               TO  RECE40PARA
           INITIALIZE                      RECE40PARA
           MOVE    "RECE400"           TO  RECE40PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE40PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE40PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE40PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE40PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0400"      TO  JOB-PGID
               MOVE    "対象患者抽出（労災・自賠責）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
              INITIALIZE                  SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC         TO  SYS-1001-REC
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                            TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           IF      WRK-PARA-RECEKBN    =   "8"
               MOVE    "2"                 TO    WRK-JIBAIKBN
           ELSE
               IF      WRK-PARA-SRYYM  NOT =   ZERO
      *            システム管理マスタ（４００１）から自賠責区分取得
                   INITIALIZE                       SYS-4001-REC
                   MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
                   MOVE    "4001"              TO  SYS-4001-KANRICD
                   MOVE    "*"                 TO  SYS-4001-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO  SYS-4001-STYUKYMD
                                                                   (1:6)
                   MOVE    "01"                TO  SYS-4001-STYUKYMD
                                                                   (7:2)
                   MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
                   MOVE    SYS-4001-REC        TO  MCPDATA-REC
                   PERFORM 910-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-4001-REC
                       IF     SYS-4001-JIBAIKBN    =   SPACE
                           MOVE    "1"                 TO  WRK-JIBAIKBN
                       ELSE
                           MOVE SYS-4001-JIBAIKBN      TO  WRK-JIBAIKBN
                       END-IF
      *
                       DISPLAY "WRK-JIBAIKBN=" WRK-JIBAIKBN
                   ELSE
                       MOVE
                          "労災自賠医療機関情報が取得できませんでした"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1                   TO  FLG-END
                   END-IF
               ELSE
                   MOVE     ZERO               TO  SYSKAN-IDX
                   MOVE     SPACE              TO  TBL-SYS4001-T
                   INITIALIZE                      TBL-SYS4001-T
      *
                   INITIALIZE                      SYS-4001-REC
                   MOVE    "4001"              TO  SYS-4001-KANRICD
                   MOVE    "*"                 TO  SYS-4001-KBNCD
                   MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
                   MOVE    SYS-4001-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM UNTIL ( FLG-SYSKANRI    =   1   )
                               OR    ( SYSKAN-IDX      >=  100 )
                           MOVE    MCPDATA-REC         TO  SYS-4001-REC
                           ADD     1                   TO  SYSKAN-IDX
                           MOVE    SYS-4001-REC        TO
                                                TBL-SYS4001 (SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
                       END-PERFORM
                   ELSE
                       MOVE
                           "労災自賠医療機関情報が取得できませんでした"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1                   TO  FLG-END
                  END-IF
      *
                  MOVE    "tbl_syskanri"       TO  MCP-TABLE
                  MOVE    "key8"               TO  MCP-PATHNAME
                  PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           IF        WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        パラメタＤＢ読み込み
               INITIALIZE                  BTPARA-REC
               MOVE    WRK-PARA-SHELLID
                                       TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID  TO  BTPARA-JOBID
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA      NOT =   ZERO
                   MOVE    "パラメタ情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           OPEN    OUTPUT              RECE40-FILE
           CLOSE                       RECE40-FILE
      *
           OPEN    I-O                 RECE40-FILE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   PERFORM 2001-IKKATU-SYORI-SEC
               WHEN    "2"
                   PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
           IF      FLG-END             =   ZERO 
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "2"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key60"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key36"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1 
                   PERFORM 20011-IKKATU-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    一括編集処理
      *****************************************************************
       20011-IKKATU-HENSYU-SEC              SECTION.
      *
      *    テスト患者のときは読み飛ばす
grpsys     MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR          FLG-END     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR          FLG-END     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-UPDKBN
                                                   WRK-TEISEIKBN
                                                   WRK-ROUSAIKBN
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       EVALUATE    WRK-PARA-RECEKBN
                           WHEN    "3"
                           WHEN    "4"
                           WHEN    "5"
                               IF      COMB-HKNNUM =   WRK-CONS-HKNNUM
                                   IF      COMB-TEKSTYMD   <=  WRK-STYMD
                                   AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                       MOVE    SPACE   TO  WRK-UPDKBN
                                   ELSE
                                       MOVE    "1"     TO  WRK-UPDKBN
                                   END-IF
                               ELSE
                                   MOVE    "1"     TO  WRK-ROUSAIKBN
                               END-IF    
                           WHEN    "8"
                               IF      COMB-KOHHKNNUM (1)
                                                   =   WRK-CONS-HKNNUM
                                   IF      COMB-TEKSTYMD   <=  WRK-STYMD
                                   AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                       MOVE    SPACE   TO  WRK-UPDKBN
                                   ELSE
                                       MOVE    "1"     TO  WRK-UPDKBN
                                   END-IF
                               ELSE
                                   MOVE    "1"     TO  WRK-ROUSAIKBN
                               END-IF    
                       END-EVALUATE
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN    
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       IF      WRK-ROUSAIKBN           =   SPACE            
                           PERFORM 20011-FILE-SYORI-SEC 
                       END-IF    
      *
      *                処理対象以外かアフターケア以外か日付変更がないとき
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       IF    ( WRK-ROUSAIKBN         =   "1"           )  
                       OR    (     ( WRK-UPDKBN      =   SPACE       )         
                               AND ( PTRSI-HKNKBN    =   "1" OR  "2" 
                                                     OR  "4" OR  "5"
                                                     OR  "6"         ) )
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-END     =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       20011-FILE-SYORI-SEC              SECTION.
      *
      *    労災保険情報読み込み
           INITIALIZE              PTRSIINF-REC
grpsys     MOVE    SPA-HOSPNUM     TO  PTRSI-HOSPNUM
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    COMB-HKNID      TO  PTRSI-HKNID
               WHEN    "8"
                   MOVE    COMB-KOHID (1)  TO  PTRSI-KOHID
           END-EVALUATE
           MOVE    COMB-PTID       TO  PTRSI-PTID
           MOVE    PTRSIINF-REC    TO  MCPDATA-REC
           PERFORM 900-PTRSIINF-INV-SEC
           IF      FLG-PTRSIINF    =   "1"
               MOVE    "1"             TO  WRK-ROUSAIKBN
           END-IF
      *
           DISPLAY "WRK-ROUSAIKBN="     WRK-ROUSAIKBN
                   " PTRSI-JIBAIKBN="   PTRSI-JIBAIKBN
      *
           IF      PTRSI-JIBAIKBN  =   SPACE   OR  "0"
               MOVE    WRK-JIBAIKBN    TO  PTRSI-JIBAIKBN
           END-IF
      *
      *
      *    労災・自賠責保険以外のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  20011-FILE-SYORI-EXT
           END-IF
      *
      *    新様式で健保準拠のときは処理しない
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201707-X )
           AND   ( WRK-PARA-RECEKBN    =   "4"                     )
           AND   ( PTRSI-JIBAIKBN      =   "2"                     )
               GO  TO  20011-FILE-SYORI-EXT
           END-IF
      *
           IF      WRK-UPDKBN          =   "1"
      *    保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF    
               END-IF    
           END-IF
      *
      *    ファイル編集処理
           PERFORM 20011-FILE-HENSYU-SEC
           .
       20011-FILE-SYORI-EXT.
           EXIT. 
      *            
      *            
      *****************************************************************
      *    ファイル編集処理
      *****************************************************************
       20011-FILE-HENSYU-SEC              SECTION.
      *     
           INITIALIZE                      WRK-RECE40-REC
grpsys     MOVE    SPA-HOSPNUM         TO  WRK-RECE40-HOSPNUM
           MOVE    WRK-STYMD (1:6)     TO  WRK-RECE40-SRYYM
           MOVE    ACCT-NYUGAIKBN      TO  WRK-RECE40-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE40-PTID
           MOVE    SPACE               TO  WRK-RECE40-RECEKA
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    COMB-HKNID      TO  WRK-RECE40-HKNID
               WHEN    "8"
                   MOVE    COMB-KOHID (1)  TO  WRK-RECE40-HKNID
           END-EVALUATE
           IF      PTRSI-HKNKBN        =   "3"
               PERFORM VARYING IDW FROM    1   BY  1
                       UNTIL   IDW         >   31
                   IF      ACCT-DAY (1 IDW)    >   ZERO
                       MOVE    ACCT-SRYYM      TO  WRK-SRYYM
                       MOVE    IDW             TO  WRK-SRYDD
                       MOVE    WRK-SRYYMD      TO  WRK-RECE40-TEKSTYMD  
                       MOVE    WRK-RECE40-KEY      TO  RECE40-KEY
                       PERFORM 900-RECE40-READ-SEC
                       IF      FLG-RECE40          =   ZERO
                           MOVE    RECE40-REC  TO  WRK-RECE40-REC 
                           PERFORM 20011-RECE40-REWRITE-SEC
                       ELSE
                           INITIALIZE              WRK-RECE40-SRYKAINF
                           MOVE    WRK-SRYYMD  TO  WRK-RECE40-TEKSTYMD  
                           PERFORM 20011-RECE40-WRITE-SEC
                       END-IF
                   END-IF
               END-PERFORM
            ELSE   
               MOVE    COMB-TEKSTYMD       TO  WRK-RECE40-TEKSTYMD
               MOVE    WRK-RECE40-KEY      TO  RECE40-KEY 
               PERFORM 900-RECE40-READ-SEC
               IF      FLG-RECE40          =   ZERO
                   MOVE    RECE40-REC          TO  WRK-RECE40-REC 
                   PERFORM 20011-RECE40-REWRITE-SEC
               ELSE
                   PERFORM 20011-RECE40-WRITE-SEC
               END-IF
           END-IF    
      *   
           .
       20011-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE40-WRITE-SEC              SECTION.
      *
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    COMB-HKNNUM         TO  WRK-RECE40-HKNNUM
               WHEN    "8"
                   MOVE    COMB-KOHHKNNUM (1)  TO  WRK-RECE40-HKNNUM
           END-EVALUATE
      *
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "8"
                   MOVE    COMB-HKNNUM         TO  WRK-RECE40-HKNNUM-X
                   MOVE    COMB-HKNID          TO  WRK-RECE40-HKNID-X
           END-EVALUATE
      *
           MOVE    COMB-HKNCOMBINUM    TO  WRK-RECE40-HKNCOMBI
           MOVE    ACCT-SRYKA          TO  WRK-RECE40-SRYKA (1)
           MOVE    WRK-TEISEIKBN       TO  WRK-RECE40-TEISEIKBN
           MOVE    WRK-RECE40-TEKSTYMD TO  WRK-RECE40-TEKSTYMD-X
           IF      PTRSI-HKNKBN        =   "3"
               MOVE    WRK-RECE40-TEKSTYMD-X   TO  WRK-RECE40-TEKEDYMD-X
           ELSE
               MOVE    COMB-TEKEDYMD           TO  WRK-RECE40-TEKEDYMD-X
           END-IF
      *
      *    自賠責区分（患者別設定）
           IF      WRK-RECE40-SRYYM    >=  WRK-CONS-SRYYM-201707
               IF     WRK-PARA-RECEKBN     =   "8"
                   CONTINUE
               ELSE
                   MOVE    PTRSI-JIBAIKBN      TO  WRK-RECE40-JIBAIKBN
               END-IF
           END-IF
      *
           DISPLAY "WRK-RECE40-JIBAIKBN=" WRK-RECE40-JIBAIKBN "#"
      *
           MOVE    WRK-RECE40-REC      TO  RECE40-REC
           WRITE   RECE40-REC
      *
           IF      STS-RECE40          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE40PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE40          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       20011-RECE40-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE40-REWRITE-SEC            SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
               IF      WRK-RECE40-SRYKA    (IDX)       =   SPACE
                   MOVE    ACCT-SRYKA      TO  WRK-RECE40-SRYKA (IDX)
                   MOVE    20              TO  IDX
               ELSE
                   IF      WRK-RECE40-SRYKA    (IDX)   =   ACCT-SRYKA
                       MOVE    20                  TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-TEISEIKBN           =   "1"
               MOVE    "1"                 TO  WRK-RECE40-TEISEIKBN     
           END-IF          
      *
           MOVE    WRK-RECE40-REC      TO  RECE40-REC
           REWRITE RECE40-REC
      *
           IF      STS-RECE40          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE40PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE40          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           .
       20011-RECE40-REWRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
                   PERFORM 20021-KOBETU-REAL-SEC
                   MOVE    "1"   TO  PTINF-TSTPTNUMKBN
                   GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
           EVALUATE    BTPARA-INFO-KBN
               WHEN    "01"
               WHEN    "04"
      *            レセプト指示（個別指示）コード読み込み
                   INITIALIZE                      SYS-2002-REC
                   MOVE    "2002"              TO  SYS-2002-KANRICD
                   MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                                   SYS-2002-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
                   MOVE    SYS-2002-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
      *
                   PERFORM     UNTIL       FLG-SYSKANRI    =   1
                               OR          FLG-END         =   1
                       PERFORM 20021-KOBETU-HENSYU-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               WHEN    "02"
      *            該当診療年月の医療機関情報取得
                   MOVE  BTPARA-INFO-PARA(17:6)
                                               TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM 20029-SYSKANRI-HENSYU-SEC
      *
      *                診療年月編集
                       MOVE     BTPARA-INFO-PARA(17:6)
                                               TO  WRK-STYMD (1:6)
                       MOVE    "01"            TO  WRK-STYMD (7:2)
                       INITIALIZE                      STS-AREA-DAY
                       INITIALIZE                      LNK-DAY7-AREA
                       MOVE    "71"            TO  LNK-DAY7-IRAI
                       MOVE    WRK-STYMD (1:6) TO  LNK-DAY7-YM
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                       MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *                対象患者のよる抽出処理
                       INITIALIZE                      PTINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD    
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       EVALUATE  WRK-PARA-RECEKBN
                         WHEN      "3"
                              MOVE    "1"          TO  PTINF-HKNKBN
                              MOVE    "5"          TO  PTINF-HKNKBN2
                         WHEN      "4"
                              MOVE    "2"          TO  PTINF-HKNKBN
                         WHEN      "5"
                              MOVE    "2"          TO  PTINF-HKNKBN
                              MOVE    "4"          TO  PTINF-HKNKBN2
                        WHEN      "8"
                              MOVE    "7"          TO  PTINF-HKNKBN
                       END-EVALUATE
                       MOVE    BTPARA-INFO-PARA(17:6) 
                                                   TO  PTINF-SRYYM
                       MOVE    BTPARA-INFO-PARA(17:6) 
                                                   TO  PTINF-SRYYMD(1:6)
                       MOVE    "__"                TO  PTINF-SRYYMD(7:2)
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       EVALUATE  WRK-PARA-RECEKBN
      ***                WHEN      "3"
                         WHEN      "4"
                         WHEN      "8"
                           MOVE    "tbl_ptinf"     TO  WRK-MCP-TABLE
                           MOVE    "key14"         TO  WRK-MCP-PATHNAME
                         WHEN      "3"
                         WHEN      "5"
                           MOVE    "tbl_ptinf"     TO  WRK-MCP-TABLE
                           MOVE    "key19"         TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                       PERFORM 20022-JYUSINYMD-HENSYU-SEC
               WHEN    "03"
      *            該当診療年月の医療機関情報取得
                   MOVE  BTPARA-INFO-PARA(1:6) TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM 20029-SYSKANRI-HENSYU-SEC
      *
      *                診療年月編集
                       MOVE     BTPARA-INFO-PARA(1:6)
                                               TO  WRK-STYMD (1:6)
                       MOVE    "01"            TO  WRK-STYMD (7:2)
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY7-AREA
                       MOVE    "71"            TO  LNK-DAY7-IRAI
                       MOVE    WRK-STYMD (1:6) TO  LNK-DAY7-YM
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                       MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *                対象患者のよる抽出処理
                       INITIALIZE                      PTINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD    
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       EVALUATE  WRK-PARA-RECEKBN
                         WHEN      "3"
                              MOVE    "1"          TO  PTINF-HKNKBN
                              MOVE    "5"          TO  PTINF-HKNKBN2
                         WHEN      "4"
                              MOVE    "2"          TO  PTINF-HKNKBN
                         WHEN      "5"
                              MOVE    "2"          TO  PTINF-HKNKBN
                              MOVE    "4"          TO  PTINF-HKNKBN2
                        WHEN      "8"
                              MOVE    "7"          TO  PTINF-HKNKBN
                       END-EVALUATE
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       EVALUATE  WRK-PARA-RECEKBN
      ***                WHEN      "3"
                         WHEN      "4"
                         WHEN      "8"
                            MOVE    "tbl_ptinf"    TO  WRK-MCP-TABLE
                            MOVE    "key12"        TO  WRK-MCP-PATHNAME
                         WHEN      "3"
                         WHEN      "5"
                            MOVE    "tbl_ptinf"    TO  WRK-MCP-TABLE
                            MOVE    "key17"        TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                       PERFORM 20022-JYUSINYMD-HENSYU-SEC
           END-EVALUATE
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ）
      *****************************************************************
       20029-SYSKANRI-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SET
      *
      *    該当診療年月の医療機関情報
           PERFORM VARYING SYSKAN-IDX  FROM  1  BY  1
                   UNTIL ( SYSKAN-IDX  >   100   )
                   OR    ( TBL-SYS1001 (SYSKAN-IDX)
                                       =   SPACE )
                   OR    ( FLG-SET     =   1     )
               MOVE    TBL-SYS1001 (SYSKAN-IDX)
                                           TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD   <=  WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD      <=  SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET         =   ZERO
               STRING
                   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                    WRK-SYS-SRYYMD(1:6)    DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               MOVE    WRK-RECEERR     TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    該当診療年月の労災自賠医療機関情報
           IF     WRK-PARA-RECEKBN     =   "8"
               MOVE    "2"                 TO  WRK-JIBAIKBN
           ELSE
               MOVE    ZERO            TO  FLG-SET
      *
               PERFORM VARYING SYSKAN-IDX  FROM  1  BY  1
                       UNTIL ( SYSKAN-IDX  >   100   )
                       OR    ( TBL-SYS4001 (SYSKAN-IDX)
                                           =   SPACE )
                       OR    ( FLG-SET     =   1     )
                   MOVE    TBL-SYS4001 (SYSKAN-IDX)
                                               TO  SYS-4001-REC
                   IF    ( SYS-4001-STYUKYMD   <=  WRK-SYS-SRYYMD    )
                   AND   ( WRK-SYS-SRYYMD      <=  SYS-4001-EDYUKYMD )
                       MOVE     1                  TO  FLG-SET
                       IF      SYS-4001-JIBAIKBN   =   SPACE
                           MOVE    "1"                 TO  WRK-JIBAIKBN
                       ELSE
                           MOVE    SYS-4001-JIBAIKBN   TO  WRK-JIBAIKBN
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-SET         =   ZERO
                   STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                              DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD(1:6)    DELIMITED  BY  SIZE
                                              INTO    WRK-RECEERR
                   END-STRING
                   MOVE    WRK-RECEERR     TO  WRK-ERRMSG
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-IF
      *
           DISPLAY "WRK-JIBAIKBN=" WRK-JIBAIKBN
           .
      *
       20029-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "2"              TO  SYS-2002-NYUGAIKBN
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      *
      *    該当診療年月の医療機関情報取得
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM 20029-SYSKANRI-HENSYU-SEC
      *
      *    患者情報取得
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
      *
      *
           IF      FLG-END             =   ZERO
      *        診療会計読み込み
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  ACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
               MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1
                   PERFORM 20011-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *        MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *        PERFORM 900-SYSKANRI-READ-SEC
           END-IF
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID 
                   " SRYYM="   SYS-2002-SRYYM
      *
      *    該当診療年月の医療機関情報取得
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM 20029-SYSKANRI-HENSYU-SEC
      *
      *    患者情報取得
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-END             =   ZERO
      *        診療会計読み込み
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  ACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
               MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1
                   PERFORM 20011-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-IF
      *
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日指定編集処理
      *****************************************************************
       20022-JYUSINYMD-HENSYU-SEC              SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                  TO  FLG-PTINF
           END-IF
      *
           PERFORM     UNTIL       FLG-PTINF       =   1
                       OR          FLG-END         =   1
      *
               DISPLAY "JYUSIN PTID=" PTINF-PTID 
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
      *
               IF      FLG-END             =   ZERO
      *            診療会計読み込み
                   INITIALIZE                  SRYACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    PTINF-PTID          TO  ACCT-PTID
                   EVALUATE    BTPARA-INFO-KBN
                       WHEN    "02"
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  ACCT-SRYYM
                       WHEN    "03"
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  ACCT-SRYYM
                   END-EVALUATE
                   MOVE    "2"                 TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                       MOVE    "key61"         TO  WRK-SRYACCT-PATHNAME
                   ELSE
                       MOVE    "key37"         TO  WRK-SRYACCT-PATHNAME
                   END-IF
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
      *
                   PERFORM UNTIL       FLG-SRYACCT     =   1
                           OR          FLG-END         =   1
                       PERFORM 20011-HENSYU-SEC
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-PTINF-READ-N-SEC
               END-IF
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-CLOSE-SEC
      *
           .
       20022-JYUSINYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-SRYACCT     =   1 
                               OR          FLG-END     =   1 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-UPDKBN
                                               WRK-TEISEIKBN
                                               WRK-ROUSAIKBN
      *
               IF      FLG-HKNCOMBI    =   ZERO
                   EVALUATE    WRK-PARA-RECEKBN
                       WHEN    "3"
                       WHEN    "4"
                       WHEN    "5"
                           IF      COMB-HKNNUM =   WRK-CONS-HKNNUM
                               IF      COMB-TEKSTYMD   <=  WRK-STYMD
                               AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                   MOVE    SPACE   TO  WRK-UPDKBN
                               ELSE
                                   MOVE    "1"     TO  WRK-UPDKBN
                               END-IF
                           ELSE
                               MOVE    "1"     TO  WRK-ROUSAIKBN
                           END-IF    
                       WHEN    "8"
                           IF      COMB-KOHHKNNUM (1)
                                               =   WRK-CONS-HKNNUM
                               IF      COMB-TEKSTYMD   <=  WRK-STYMD
                               AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                                   MOVE    SPACE   TO  WRK-UPDKBN
                               ELSE
                                   MOVE    "1"     TO  WRK-UPDKBN
                               END-IF
                           ELSE
                               MOVE    "1"     TO  WRK-ROUSAIKBN
                           END-IF    
                   END-EVALUATE
               ELSE
                   MOVE    "1"             TO  WRK-TEISEIKBN    
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1 
                               OR      KEY-NEW     NOT =   KEY-OLD
                   IF      WRK-ROUSAIKBN           =   SPACE            
                       PERFORM 20011-FILE-SYORI-SEC
                   END-IF    
      *
                   IF    ( WRK-ROUSAIKBN           =   "1"           )  
                   OR    (     ( WRK-UPDKBN        =   SPACE       )         
                           AND ( PTRSI-HKNKBN      =   "1" OR  "2" 
                                                   OR  "4" OR  "5"
                                                   OR  "6"         ) )
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       20011-HENSYU-EXT.
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
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0400 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
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
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
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
           CLOSE   RECE40-FILE
      *
           IF      CNT-OUT             =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *     
           DISPLAY "RECE40  CNT " CNT-OUT 
           DISPLAY "*** ORCR0400 END ***"
           .
       300-TERM-EXT.
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1               TO  FLG-PTRSIINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
                   INITIALIZE                      PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTRSIINF
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE40-READ-SEC             SECTION.
      *
           READ    RECE40-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE40
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE40
           END-READ
           .
       900-RECE40-READ-EXT.
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
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0400 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
