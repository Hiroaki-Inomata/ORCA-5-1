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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBPRT1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 帳票ＤＢの印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/16    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/05/20  再印刷画面からの印刷による修正  
      *  01.00.02    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.01.01    NACL-藤原    02/10/10  印刷用コンスタント領域の追加
      *                                     印刷用ファイルにサイト区分を追加
      *  01.01.02    NACL-藤原    03/01/20  プレビューＤＢ処理追加
      *  01.01.03    NACL-藤原    04/11/29  引数にエラーファイル名を追加
      *  01.01.04    NACL-伊藤    06/03/07  オフセット対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.05.00    NACL-太田    10/01/28  拡張漢字対応
      *  04.05.01    NACL-藤原    11/09/02  起動方法の変更
      *
      *  04.07.00    NACL-竹田    11/03/28  クライアント印刷対応
      *****************************************************************
      *
      *  PRTFLG
      *  1: 1頁毎印刷。PDFを残さない。
      *  2: 2頁毎印刷。PDFを残す。
      *  3: 1頁毎PDF作成
      *  4: 3と同じ
      *  5: クライアント印刷を考慮する。
      *  6: 帳票毎印刷
      *  7: 帳票毎PDF作成 (印刷しない) 複数帳票も1pdfにまとめる。
      *  8: 帳票毎印刷(ginbee用)
      *  9: 帳票毎PDF作成 (印刷しない) 帳票にpdfを作成
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    PUSH用データ
           SELECT  PUSH-FILE       ASSIGN  PUSHPARA
                                   ORGANIZATION    IS  SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PUSH.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "CPPRTALL.INC".
      *
      *    PUSH用データ
       FD  PUSH-FILE.
           COPY    "CPPUSHPRINT001.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
           COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                       BY        //PUSHPARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
           03  STS-PUSH            PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-PRTDATA         PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
           03  FLG-PRVKANRI        PIC 9(01).
           03  FLG-NODATA          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *  01.01.02  ADD START 
      *    添字領域
       01  SOEJI-AREA.
           03  IDX                 PIC 9(05).
      *  01.01.02  ADD END   
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(10).
           03  CNT-GROUP           PIC 9(05).
      *  01.01.02  ADD START 
           03  CNT-KETA            PIC 9(05).
           03  WRK-MOJISU          PIC 9(05).
      *  01.01.02  ADD END   
           03  CNT-PUSH            PIC 9(05).
      *
       01  SYS-TIME                PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HC01PARA-CNT    PIC  9(03).
      *
           03  WRK-RECEERR         PIC X(200). 
      *
      *  01.01.02  ADD START 
           03  WRK-FILENAME        PIC X(8). 
           03  WRK-NONE            PIC X(256). 
      *  01.01.02  ADD END   
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
           03  WRK-PAGE            PIC 9(05).
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
      *
      *    一時ディレクトリ名
           03  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
           03  WRK-RPLNK-KEY-G.
               05  WRK-RPLNK-KEY   PIC 9(02).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-STPAGE     PIC 9(05).
           03  WRK-PARA-EDPAGE     PIC 9(05).
           03  WRK-PARA-PRTFLG     PIC X(01).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-SHOWDIALOG PIC X(01).
           03  WRK-PARA-KEEP-PDF   PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-PUSH-MAX              PIC 9(05)   VALUE 10.
      *
      *    印刷用コンスタント領域 
           COPY    "CPPRTCONST.INC".          
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  TPRT-REC.
           COPY    "CPPRTKANRI.INC"        REPLACING  //PRTKANRI//
                                           BY         //TPRT//.
      *
           COPY    "CPPUSHPRINT001.INC"    REPLACING  //PUSHP001-//
                                           BY         //TPUSHP001-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    オフセット情報
           COPY    "CPSK1044.INC".
      *
      *    印刷管理ＤＢ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷ＤＢ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *    プレビューＤＢ
      *01  PRVKANRI-REC.
           COPY    "CPPRVKANRI.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ファイル取得関数
       01 ORCFILEGET-AREA.
           03  ORCF-FILE-MODE   PIC X(1).
           03  ORCF-FILE-NAME   PIC X(200).
           03  ORCF-DATA        PIC X(100000).
           03  ORCF-RET         PIC X(2).
       01  ST                      PIC 9(2).
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
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
               DISPLAY "------------- ORCBPRT1 start ----------------"
               DISPLAY "FLG-END = " FLG-END
               DISPLAY "------------- ORCBPRT1 end   ----------------"
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       RECEERR
                                       TPRT-REC
                                       TPUSHP001-REC
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE     SMCNDATE-TIME   TO  SYS-TIME
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
                                               RECEERR
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-STPAGE
                                               WRK-PARA-EDPAGE
                                               WRK-PARA-PRTFLG
                                               WRK-PARA-HOSPNUM
                                               LNK-PRTKANRI-TBL-UUID
                                               LNK-PRTKANRI-GYOUMU-CD
                                               WRK-NONE
                                               WRK-PARA-SHOWDIALOG
                                               WRK-PARA-KEEP-PDF
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
           MOVE    LNK-PRTKANRI-OPID   TO  SPA-OPID
      *
           MOVE    WRK-PARA-SHOWDIALOG TO  SPA-CLIENT-SHOWDIALOG
           IF    ( SPA-CLIENT-SHOWDIALOG   =   "1" )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  SPA-CLIENT-SHOWDIALOG
           END-IF
      *
           IF      LNK-PRTKANRI-GYOUMU-CD NOT NUMERIC
                   MOVE  ZERO          TO
                                       LNK-PRTKANRI-GYOUMU-CD
           END-IF
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    SPA-MWの値を取得
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE     SGETTEMP-MW        TO  SPA-MW
      *
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING    TO      WRK-ENCODING9
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR         TO      SGETTEMP-FULLNAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO      RECEERR
      *
           MOVE    SGETTEMP-TEMPDIR    TO  WRK-SGETTEMP-TEMPDIR
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBPRT1"      TO  JOB-PGID
           MOVE    "帳票印刷処理"  TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    印刷管理ＤＢ読み込み処理
           PERFORM 900-PRTKANRI-SELECT-SEC
      *
      *    プレビュー用前処理
           MOVE    ZERO            TO  WRK-MOJISU
           IF      WRK-PARA-PRTFLG     =   "3"
               PERFORM 900-PRVKANRI-KEY9-SEL-SEC
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
               MOVE    ZERO        TO  CNT-OUT
           END-IF
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
           IF    ( WRK-PARA-PRTFLG     =  "7" )
               PERFORM 2001-COMMON-PRINT-SEC
           ELSE
               IF    ( WRK-PARA-PRTFLG     =  "5" OR "6" OR "9" )
                OR   ( SPA-MW              =  SPA-GINBEE )
                   PERFORM 2001-BIND-PRINT-SEC
               ELSE
                   PERFORM 2001-COMMON-PRINT-SEC
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷以外の印刷処理
      *****************************************************************
       2001-COMMON-PRINT-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    "HC99"          TO  HC01PARA-FORM-ID
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  HC01PARA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    1               TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGNPARA
           MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
      *
           OPEN    OUTPUT              PRT-FILE
      *
           MOVE    ZERO            TO  CNT-GROUP
      *
           PERFORM                     UNTIL   FLG-PRTKANRI =   1
                                       OR      FLG-END      =   1   
      *
               MOVE    PRTKANRI-REC    TO  TPRT-REC
      *
               IF    ( PRTKANRI-SHELLID    =   "API" )
                    PERFORM 20011-PUSH-FORM-SEC
               ELSE
      *
      *            印刷ＤＢ編集処理
                   PERFORM 900-PRTDATA-SELECT-SEC
                   PERFORM             UNTIL   FLG-PRTDATA  =   1
                                       OR      FLG-END      =   1
      *
                       PERFORM 2001-PRTDATA-PRINT-SEC
                       PERFORM 900-PRTDATA-FET-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_prtdata"   TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
               END-IF
      *
               IF    ( TPRT-PRTFLG    =    "D" )
                   PERFORM 2003-PRTKANRI-DEL-SEC
               ELSE
                   PERFORM 2003-PRTKANRI-UPD-SEC
               END-IF
      *
               PERFORM 900-PRTKANRI-FET-SEC
      *
           END-PERFORM
      *
           CLOSE   PRT-FILE
      *
           PERFORM 210-PRINT-SEC
      *
           IF    ( CNT-PUSH        >   ZERO )
      *
               PERFORM 20012-PUSH-FORM-SEC
      *
           END-IF
      *
           .
       2001-COMMON-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票毎印刷処理
      *****************************************************************
       2001-BIND-PRINT-SEC             SECTION.
      *
      *    印刷用ファイル作成
           MOVE    "HC99"          TO  HC01PARA-FORM-ID
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  HC01PARA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ZERO            TO  HC01PARA-CNT
      *
           PERFORM                     UNTIL   FLG-PRTKANRI =   1
                                       OR      FLG-END      =   1   
      *
               MOVE    PRTKANRI-REC    TO  TPRT-REC
      *
               MOVE    ZERO            TO  CNT-GROUP
      *
               IF    ( PRTKANRI-SHELLID    =   "API" )
                    PERFORM 20011-PUSH-FORM-SEC
               ELSE
                    PERFORM 20011-BIND-PRINT-SEC
               END-IF
      *
               IF    ( TPRT-PRTFLG    =    "D" )
                   IF    ( PRTKANRI-SHELLID    NOT =   "API" )
                       PERFORM 2003-PRTKANRI-DEL-SEC
                   END-IF
               ELSE
                   PERFORM 2003-PRTKANRI-UPD-SEC
               END-IF
      *
               PERFORM 900-PRTKANRI-FET-SEC
      *
           END-PERFORM                                
      *
           IF    ( CNT-PUSH        >   ZERO )
      *
               PERFORM 20012-PUSH-FORM-SEC
      *
           END-IF
      *
           .
       2001-BIND-PRINT-EXT.
           EXIT.
      *****************************************************************
      *   PUSH情報編集処理
      *****************************************************************
       20011-PUSH-FORM-SEC             SECTION.
      *
           IF    ( WRK-PARA-PRTFLG     NOT =  "3" )
               IF    ( CNT-PUSH       <   CONST-PUSH-MAX )
                   PERFORM 200111-PUSH-FORM-SEC
               END-IF
           END-IF
      *
           .
       20011-PUSH-FORM-EXT.
           EXIT.
      *****************************************************************
      *   PUSH情報編集処理
      *****************************************************************
       200111-PUSH-FORM-SEC           SECTION.
      *
      *    PUSH分はカウントから除外
      *    ADD     1                   TO  CNT-OUT
      *
           ADD     1                   TO  CNT-GROUP
           ADD     1                   TO  CNT-PUSH
      *
           MOVE    PRTKANRI-API-ID     TO  TPUSHP001-FORMID (CNT-PUSH)
      *
           PERFORM 900-PRTDATA-SELECT-SEC
           IF    ( FLG-PRTDATA     =   ZERO )
              MOVE    PRTDATA-PRTID TO TPUSHP001-CUSTOMID (CNT-PUSH)
           END-IF
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           EVALUATE    TRUE
           WHEN    ( PRTKANRI-TITLE (1:8)  =    "支払日別" )
               MOVE    "支払証明書（日別）"
                                       TO  TPUSHP001-FORMNAME (CNT-PUSH)
           WHEN    ( PRTKANRI-TITLE (1:8)  =    "支払月別" )
               MOVE    "支払証明書（月別）"
                                       TO  TPUSHP001-FORMNAME (CNT-PUSH)
           WHEN    ( PRTKANRI-TITLE (1:8)  =    "支払証明" )
               MOVE    "支払証明書"    TO  TPUSHP001-FORMNAME (CNT-PUSH)
           WHEN    OTHER
               MOVE    PRTKANRI-TITLE  TO  TPUSHP001-FORMNAME (CNT-PUSH)
           END-EVALUATE
      *
           STRING  PRTKANRI-TBL-KEY    DELIMITED BY SPACE
                   "#"                 DELIMITED BY SIZE
                   PRTKANRI-TBL-UUID   DELIMITED BY SPACE
                   "#"                 DELIMITED BY SIZE
                   PRTKANRI-TBL-GROUP  DELIMITED BY SIZE
                   "#"                 DELIMITED BY SIZE
                   PRTKANRI-RENNUM     DELIMITED BY SIZE
                   "#"                 DELIMITED BY SIZE
                   PRTKANRI-SHORI-RENNUM
                                       DELIMITED BY SIZE
           INTO    TPUSHP001-DATAID (CNT-PUSH)
           END-STRING
      *
           PERFORM 20021-PRTDATA-UPD-SEC
      *
           .
       200111-PUSH-FORM-EXT.
           EXIT.
      *****************************************************************
      *   PUSH情報編集処理
      *****************************************************************
       20012-PUSH-FORM-SEC           SECTION.
      *
           COMPUTE HC01PARA-CNT = HC01PARA-CNT + 1
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PUSHPARA
           MOVE    SGETTEMP-ST         TO  PUSHPARA-ST
      *
           OPEN    OUTPUT                  PUSH-FILE
      *
           MOVE    TPUSHP001-REC       TO  PUSHP001-REC
           MOVE    "print001"          TO  PUSHP001-EVENT
           MOVE    LNK-PRTKANRI-OPID   TO  PUSHP001-OPID
      *
           WRITE   PUSHP001-REC
      *
           CLOSE   PUSH-FILE
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "push_form.sh"    TO  SHELLTBL-NAME
           MOVE    PUSHPARA (PUSHPARA-ST:)
                                       TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG2
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       20012-PUSH-FORM-EXT.
           EXIT.
      *****************************************************************
      *    帳票毎印刷処理
      *****************************************************************
       20011-BIND-PRINT-SEC            SECTION.
      *
           COMPUTE HC01PARA-CNT = HC01PARA-CNT + 1
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGNPARA
           MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
      *
           OPEN    OUTPUT              PRT-FILE
      *
      *    印刷ＤＢ編集処理
           PERFORM 900-PRTDATA-SELECT-SEC
           PERFORM                     UNTIL   FLG-PRTDATA  =   1
                                       OR      FLG-END      =   1
               PERFORM 2001-PRTDATA-PRINT-SEC
               PERFORM 900-PRTDATA-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE   PRT-FILE
      *
           PERFORM 210-PRINT-SEC
      *
           .
       20011-BIND-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       210-PRINT-SEC                   SECTION.
      *
           INITIALIZE                      SRPLNK-AREA
      *
      *
           IF    ( WRK-PARA-PRTFLG     =   "9" )
      *
               MOVE    "SEL"           TO  SRPLNK-MODE
               MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  WRK-RPLNK-KEY
               MOVE    WRK-RPLNK-KEY-G TO  SPA-PRT-FLG
               MOVE    PRTKANRI-TBL-UUID
                                       TO  SPA-PRT-UUID
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
               MOVE    SRPLNK-VAL      TO  PRTKANRI-API-ID
      *
           END-IF
      *
      *   印刷用スクリプト処理
      *
           DISPLAY "------------- ORCBPRT1 start ----------------"
           DISPLAY "CNT-GROUP = " CNT-GROUP
           DISPLAY "FLG-ERR   = " FLG-ERR
           DISPLAY "------------- ORCBPRT1 end   ----------------"
           IF      CNT-GROUP           >   ZERO
           AND     FLG-ERR             =   ZERO 
      *
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG1
grpsys         MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
               MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
               IF    ( WRK-PARA-PRTFLG =  "9" )
                   CONTINUE
               ELSE
                   MOVE    "1"         TO  SHELLTBL-ARG4
               END-IF
               IF    ( WRK-PARA-PRTFLG     =  "5" )
                   MOVE    TPRT-TITLE  TO  SHELLTBL-ARG5
                   MOVE    SPA-CLIENT-SHOWDIALOG
                                       TO  SHELLTBL-ARG6
               END-IF
               MOVE    WRK-PARA-KEEP-PDF
                                       TO  SHELLTBL-ARG7
               MOVE    SRPLNK-VAL      TO  SHELLTBL-ARG8
               MOVE    SHELLTBL        TO  MCPDATA-REC
               IF    ( WRK-PARA-PRTFLG     =  "7" OR "9" )
                   MOVE    "EXCOMMAND" TO  MCP-FUNC
               ELSE
                   MOVE    "SHELL"     TO  MCP-FUNC
               END-IF
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "daily"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
           END-IF                                                
      *
           .
       210-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRTDATA-PRINT-SEC          SECTION.
      *
      * プレビュー処理の場合管理DBがあれば以降スキップ
           IF      WRK-PARA-PRTFLG        =    "3"
                   PERFORM 900-PRVKANRI-INV-SEC
             IF    FLG-PRVKANRI           =    ZERO
             AND   FLG-NODATA             =    ZERO
                   ADD     1              TO  CNT-OUT
                   GO      TO          2001-PRTDATA-PRINT-EXT
             END-IF
           END-IF
      *
           MOVE    SPACE               TO  PRT-REC 
           MOVE    PRTDATA-PRTID       TO  PRT-PRTID
           IF      PRTDATA-PRTNM       =   SPACE
               MOVE    PRTKANRI-PRTNM      TO  PRT-PRTNM
           ELSE
               MOVE    PRTDATA-PRTNM       TO  PRT-PRTNM
           END-IF
      *
      *    オフセット
           PERFORM 2002-OFFSET-GET-SEC
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
      *
           EVALUATE    PRTDATA-SITEKBN
               WHEN    "1"
               WHEN    "2"               
                   MOVE    PRTDATA-SITEKBN     TO  PRT-SITEKBN
               WHEN    OTHER
                   SET     PRTCONST-IDX        TO  1
                   SEARCH  PRTCONST-TX VARYING PRTCONST-IDX
                       AT  END
                           MOVE    "2"         TO  PRT-SITEKBN
                       WHEN    PRTCONST-PRTID (PRTCONST-IDX)
                                               =   PRTDATA-PRTID
                           MOVE    "1"         TO  PRT-SITEKBN
                   END-SEARCH
           END-EVALUATE                                                                
      *  01.01.02  SATRT
           MOVE WRK-PARA-PRTFLG                TO  PRT-PRTKBN
           MOVE    PRTDATA-DATA-TYPE           TO  PRT-DATA-TYPE
           MOVE    ZERO                        TO  CNT-KETA
      *****MOVE    "/var/lib/jma-receipt/orca_ps/"        
           MOVE    WRK-SGETTEMP-TEMPDIR        TO  PRT-OUT
           MOVE    LNK-PRTKANRI-TBL-KEY        TO  PRT-TBL-KEY
           MOVE    PRTDATA-RENNUM              TO  PRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO  PRT-TBL-GROUP 
           MOVE    PRTDATA-SHORI-RENNUM        TO  PRT-SHORI-RENNUM
           MOVE    PRTDATA-PAGE                TO  PRT-PAGE  
           MOVE    LNK-PRTKANRI-TBL-UUID       TO  PRT-UUID
      *  01.01.02  END
           MOVE    PRTDATA-PRTDATA TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
      *  01.01.02  SATRT
      *    IF   WRK-PARA-PRTFLG        =   "3"
      *         PERFORM 2004-PREVIEW-INS-SEC
      *    END-IF
      *  01.01.02  END
      *
           ADD     1                   TO  CNT-OUT     
           ADD     1                   TO  CNT-GROUP
      *
           IF    ( TPRT-PRTFLG    =    "D" )
               PERFORM 2002-PRTDATA-DEL-SEC
           ELSE
               PERFORM 2002-PRTDATA-UPD-SEC
           END-IF
           .
       2001-PRTDATA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       2002-OFFSET-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    PRT-PRTID           TO  SYS-1044-FORMID
           STRING  PRT-PRTNM           DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE  MCPDATA-REC   TO  SYS-1044-REC
                   IF   (SYS-1044-FORMID  =   PRT-PRTID)  AND
                        (SYS-1044-PRTNM   =   PRT-PRTNM)
                         CONTINUE
                   ELSE
                         INITIALIZE    SYS-1044-REC
                   END-IF
               ELSE
                   INITIALIZE          SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE              SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002-OFFSET-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷フラグセット処理
      *****************************************************************
       2002-PRTDATA-UPD-SEC          SECTION.
      *
      *    印刷フラグセット
           MOVE    "1"                 TO  PRTDATA-PRTFLG
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢの更新でエラーになりました"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF    
           .
       2002-PRTDATA-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷ＤＢ削除処理
      *****************************************************************
       2002-PRTDATA-DEL-SEC            SECTION.
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢの更新でエラーになりました"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF    
           .
       2002-PRTDATA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷フラグセット処理
      *****************************************************************
       20021-PRTDATA-UPD-SEC          SECTION.
      *
      *    印刷フラグセット
           INITIALIZE                      PRTDATA-REC
           MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
           MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    PRTKANRI-TBL-UUID   TO  PRTDATA-TBL-UUID
           MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
      *
           MOVE    "1"                 TO  PRTDATA-PRTFLG
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢの更新でエラーになりました"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF
      *
           .
      *
       20021-PRTDATA-UPD-EXT.
           EXIT.
      *****************************************************************
      *    印刷フラグセット処理
      *****************************************************************
       2003-PRTKANRI-UPD-SEC          SECTION.
      *
           MOVE    "1"                 TO  PRTKANRI-PRTFLG
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷管理ＤＢの更新でエラーになりました"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF    
           .
       2003-PRTKANRI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    印刷管理削除処理
      *****************************************************************
       2003-PRTKANRI-DEL-SEC          SECTION.
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷管理ＤＢの更新でエラーになりました"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF    
           .
       2003-PRTKANRI-DEL-EXT.
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
                                           FLG-ERR
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
           MOVE    CNT-OUT         TO  JOB-UPDCNT                        
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBPRT1 IN  "  CNT-IN
           DISPLAY "*** ORCBPRT1 OUT "  CNT-OUT
      **** DISPLAY "*** ORCBPRT1 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     プレビュー管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-INV-SEC             SECTION.
      *
           MOVE    LNK-PRTKANRI-TBL-KEY        TO  PRVKANRI-TBL-KEY
           MOVE    PRTDATA-RENNUM              TO  PRVKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO  PRVKANRI-TBL-GROUP 
           MOVE    PRTDATA-SHORI-RENNUM        TO  PRVKANRI-SHORI-RENNUM
           MOVE    PRTDATA-PAGE                TO  PRVKANRI-PAGE  
           MOVE    LNK-PRTKANRI-TBL-UUID       TO  PRVKANRI-TBL-UUID
grpsys     MOVE    SPA-HOSPNUM                 TO  PRVKANRI-HOSPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRVKANRI-FET1-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRVKANRI
                                               FLG-END
               DISPLAY "ORCBPRT1 SYSTEM ERR!!"
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PRVKANRI-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     プレビュー管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-KEY9-SEL-SEC          SECTION.
      *
           INITIALIZE                      PRVKANRI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRVKANRI-TBL-GROUP
           MOVE    LNK-PRTKANRI-RENNUM
                                       TO  PRVKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  PRVKANRI-TBL-UUID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  PRVKANRI-SHORI-RENNUM
           MOVE    WRK-PARA-STPAGE     TO  PRVKANRIXX-STPAGE
           MOVE    WRK-PARA-EDPAGE     TO  PRVKANRIXX-EDPAGE
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_prvkanri"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-PRVKANRI
                   MOVE    MCPDATA-REC TO  PRVKANRI-REC
      *
                   IF    ( WRK-PARA-STPAGE  NOT =   ZERO )
                       MOVE    WRK-PARA-STPAGE
                                       TO   WRK-STPAGE
                   ELSE
                       MOVE    PRVKANRIXX-STPAGE
                                       TO   WRK-STPAGE
                   END-IF
                   IF    ( WRK-PARA-EDPAGE  NOT =   99999 )
                       MOVE    WRK-PARA-EDPAGE
                                       TO   WRK-EDPAGE
                   ELSE
                       MOVE    PRVKANRIXX-EDPAGE
                                       TO   WRK-EDPAGE
                   END-IF
      *
                   COMPUTE WRK-PAGE = WRK-EDPAGE - WRK-STPAGE + 1
      *
      D            DISPLAY "PAGE COUNT   =" PRVKANRI-PAGE
      D            DISPLAY "PAGE MIN     =" PRVKANRIXX-STPAGE
      D            DISPLAY "PAGE MAX     =" PRVKANRIXX-EDPAGE
      D            DISPLAY "PRINT PAGES  =" WRK-PAGE
      *
                   IF    ( PRVKANRI-PAGE   <   WRK-PAGE )
                       MOVE    1       TO  FLG-PRVKANRI
                   END-IF
      *
               ELSE
                   MOVE    SPACE       TO  PRVKANRI-REC
                   MOVE    1           TO  FLG-PRVKANRI
               END-IF
           ELSE 
               MOVE    1               TO  FLG-PRVKANRI
                                           FLG-END
               DISPLAY "ORCBPRT1 SYSTEM ERR!!"
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-PRVKANRI        =   1 
               MOVE    1               TO  FLG-NODATA
               GO      TO             900-PRVKANRI-KEY9-SEL-EXT
           END-IF
      *
           DISPLAY "ORCBPRT1 start hit ok!!"
      *
               DISPLAY "ORCBPRT1 next hit ok!!"
      *    プレビュー以外の時
           IF WRK-PARA-PRTFLG         =    "3"
               PERFORM VARYING IDX    FROM    WRK-STPAGE BY 1
                       UNTIL ( IDX    >       WRK-EDPAGE )
                        OR   ( FLG-NODATA     NOT =   ZERO )
      *
                  MOVE    IDX            TO  PRVKANRI-PAGE
      *
                  MOVE    SPACE          TO  ORCF-FILE-NAME
                  STRING  WRK-SGETTEMP-TEMPDIR
                                          DELIMITED  BY  SPACE
                      PRVKANRI-HOSPNUM    DELIMITED  BY  SPACE
                      PRVKANRI-TBL-KEY    DELIMITED  BY  SPACE
                      PRVKANRI-RENNUM     DELIMITED  BY  SPACE
                      PRVKANRI-TBL-GROUP  DELIMITED  BY  SPACE
                      PRVKANRI-SHORI-RENNUM
                                          DELIMITED  BY  SPACE
                      PRVKANRI-PAGE       DELIMITED  BY  SPACE
                      PRVKANRI-TBL-UUID   DELIMITED  BY  SPACE
                      ".pdf"              DELIMITED  BY  SIZE
                                          INTO   ORCF-FILE-NAME
                  END-STRING
                  MOVE    "r"             TO  ORCF-FILE-MODE
                  CALL    "orcfileio"     USING ST  ORCFILEGET-AREA
                  IF      ORCF-RET        =   ZERO
                          MOVE    1       TO  FLG-END
                          ADD     1       TO  CNT-OUT
                  ELSE
                          MOVE    ZERO    TO  FLG-END
                          MOVE    1       TO  FLG-NODATA
                  END-IF
               END-PERFORM
           END-IF
           .
       900-PRVKANRI-KEY9-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *     プレビュー管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FET1-SEC           SECTION.
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PRVKANRI
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
           ELSE
               MOVE    SPACE               TO  PRVKANRI-REC
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
           .
       900-PRVKANRI-FET1-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FET-SEC            SECTION.
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
               IF      LNK-PRTKANRI-RENNUM  =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-RENNUM   NOT = 
                                           PRVKANRI-RENNUM
                       GO  TO  900-PRVKANRI-FET-SEC
                   END-IF
               END-IF                                             
               IF      LNK-PRTKANRI-SHORI-RENNUM   =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-SHORI-RENNUM   NOT = 
                                           PRVKANRI-SHORI-RENNUM
                       GO  TO  900-PRVKANRI-FET-SEC
                   END-IF
               END-IF                                             
               MOVE    ZERO                TO  FLG-PRVKANRI
                                               FLG-END
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
                                               FLG-END 
           END-IF
           .
       900-PRVKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRTKANRI-SELECT-SEC          SECTION.
      *
           INITIALIZE                      PRTKANRI-REC
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRTKANRI-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  PRTKANRI-TBL-UUID
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
                                               FLG-END
           END-IF
           .
       900-PRTKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRTKANRI-FET-SEC            SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
               IF      LNK-PRTKANRI-RENNUM  =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-RENNUM   NOT = 
                                           PRTKANRI-RENNUM
                       GO  TO  900-PRTKANRI-FET-SEC
                   END-IF
               END-IF                                             
               IF      LNK-PRTKANRI-SHORI-RENNUM   =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-SHORI-RENNUM   NOT = 
                                           PRTKANRI-SHORI-RENNUM
                       GO  TO  900-PRTKANRI-FET-SEC
                   END-IF
               END-IF                                             
               MOVE    ZERO                TO  FLG-PRTKANRI
                                               FLG-END
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
                                               FLG-END 
           END-IF
           .
       900-PRTKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTDATA-SELECT-SEC          SECTION.
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
           MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    PRTKANRI-TBL-UUID   TO  PRTDATA-TBL-UUID
           MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
           MOVE    WRK-PARA-STPAGE     TO  PRTDATAXX-STPAGE
           MOVE    WRK-PARA-EDPAGE     TO  PRTDATAXX-EDPAGE
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTDATA-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTDATA
           END-IF
           .
       900-PRTDATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTDATA-FET-SEC            SECTION.
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTDATA-REC
               ADD     1                   TO  CNT-IN
               MOVE    ZERO                TO  FLG-PRTDATA
           ELSE  
               MOVE    1                   TO  FLG-PRTDATA
           END-IF 
           .
       900-PRTDATA-FET-EXT.
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
