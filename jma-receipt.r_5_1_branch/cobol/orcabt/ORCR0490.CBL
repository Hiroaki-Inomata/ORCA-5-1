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
       PROGRAM-ID.             ORCR0490.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災・自賠責
      *                                明細書ファイル作成（印刷データ抽出）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/09/20  自賠責の追加
      *  01.00.02    NACL-藤原    02/09/27  パラメタに入外区分の追加
      *
      *  中止処理追加
      *  01.03.01    NACL-藤原    04/12/01  印刷中止処理の追加
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  03.02.01    NACL-藤原    06/07/11  労災・自賠責のプリンタ指定対応
      *                                     （入院外・入院別）
      *  03.02.02    NACL-藤原    06/08/01  国保主科対応
      *                                     主科用出力順識別番号追加
      *
      *  03.03.01    NACL-藤原    06/09/19  月途中での氏名変更対応
      *                                     MONFUNC対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *
      *  04.02.01    NACL-藤原    08/05/09  エラー時終了処理の追加
      *
      *  04.07.01    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
      ***     COPY    "CPCOMMONSHELL.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-MOJISU1         PIC 9(02).
           03  WRK-MOJISU2         PIC 9(02).
      *
           03  WRK-PARA.
               05  WRK-PARA1.  
                   07  WRK-PARA-SRYYM            PIC X(06).
                   07  WRK-PARA-TERMID           PIC X(16).
                   07  WRK-PARA-SYSYMD           PIC X(08).
               05  WRK-PARA-RECE09-REC. 
                   07  WRK-PARA-RECE09-KEY.
                       09  WRK-PARA-RECE09-PRTID     PIC X(04).
                       09  WRK-PARA-RECE09-SRYYM     PIC X(06).
                       09  WRK-PARA-RECE09-CREYMD    PIC X(08).
                       09  WRK-PARA-RECE09-CREHMS    PIC X(06).
                       09  WRK-PARA-RECE09-PREFKBN   PIC X(01).
                       09  WRK-PARA-RECE09-TEISYUTUSAKI
                                                     PIC X(01).
                       09  WRK-PARA-RECE09-RECESYUBETU
                                                     PIC X(04).
                       09  WRK-PARA-RECE09-NYUGAIKBN PIC X(01).
                   07  WRK-PARA-RECE09-PRTJYUN      PIC X(02).
               05  WRK-PARA-RECE09-STDATA           PIC X(100).
               05  WRK-PARA-RECE09-EDDATA           PIC X(100).
               05  WRK-PARA-PRTJYUN           PIC 9(02).
               05  WRK-PARA-SHELLID           PIC X(08).
               05  WRK-PARA-JOBID             PIC 9(07).
               05  WRK-PARA-PRTNM             PIC X(16).
               05  WRK-PARA-PRTNM-NEXT        PIC X(16).
               05  WRK-PARA-HOSPNUM    PIC  9(02).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-RECE08-PRTRENNUM    PIC 9(06).
      *
           03  WRK-RECEERR             PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
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
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END    =   1
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO  WRK-PARA-JOBID
                                             WRK-PARA-SHELLID
                                             WRK-PARA1
                                             WRK-PARA-RECE09-REC
                                             WRK-PARA-RECE09-STDATA
                                             WRK-PARA-RECE09-EDDATA
                                             WRK-PARA-PRTJYUN
                                             WRK-PARA-PRTNM
                                             WRK-PARA-PRTNM-NEXT
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
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0490"      TO  JOB-PGID
               MOVE    "対象レセプト抽出"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       130-SYOKI-PRINT-SEC             SECTION.
      *
           INITIALIZE                      RECE08-REC
grpsys     MOVE    SPA-HOSPNUM                  TO  RECE08-HOSPNUM
           MOVE    WRK-PARA-RECE09-PRTID        TO  RECE08-PRTID
           MOVE    WRK-PARA-RECE09-SRYYM        TO  RECE08-RECEYM
           MOVE    WRK-PARA-RECE09-CREYMD       TO  RECE08-CREYMD
           MOVE    WRK-PARA-RECE09-CREHMS       TO  RECE08-CREHMS
           MOVE    "%"                          TO  RECE08-PREFKBN
           MOVE    "%"                          TO  RECE08-TEISYUTUSAKI
           MOVE    WRK-PARA-RECE09-RECESYUBETU  TO  RECE08-RECESYUBETU
           MOVE    WRK-PARA-RECE09-NYUGAIKBN    TO  RECE08-NYUGAIKBN
           MOVE    "%%"                         TO  RECE08-SRYKA
           MOVE    "%%%%%%%%"                   TO  RECE08-HKNJANUM-S
      *
           MOVE    WRK-PARA-RECE09-STDATA       TO  RECE08-STKANANAME
           MOVE    WRK-PARA-RECE09-EDDATA       TO  RECE08-EDKANANAME
           MOVE    1                            TO  RECE08-STRENNUM
           MOVE    99                           TO  RECE08-EDRENNUM
           PERFORM 900-RECEPRT-SELECT-SEC
      *
           .
       130-SYOKI-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-RECE08-PRTRENNUM     
      *
      *    帳票印刷開始処理
           PERFORM 130-SYOKI-PRINT-SEC
      *
           PERFORM              UNTIL   FLG-END =   1        
               ADD     1                    TO  WRK-RECE08-PRTRENNUM
      *
               MOVE    WRK-PARA-PRTJYUN     TO  RECE08-PRTJYUN-SYUKA
               MOVE    WRK-RECE08-PRTRENNUM TO  RECE08-PRTRENNUM      
               IF      RECE08-RENNUM    =   1
                   MOVE    WRK-PARA-PRTNM       TO  RECE08-PRTNM
               ELSE
                   MOVE    WRK-PARA-PRTNM-NEXT  TO  RECE08-PRTNM
               END-IF
      *
               DISPLAY "PTNUM=" RECE08-PTNUM "#" RECE08-PRTJYUN-SYUKA
                                "#" RECE08-PRTRENNUM
      *
               MOVE    RECE08-REC           TO  MCPDATA-REC
               MOVE    "tbl_receprt"        TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               MOVE    "DBUPDATE"           TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
      *        エラーチェック
               IF      MCP-RC             =   ZERO
                   ADD     1                  TO  CNT-OUT
                   PERFORM 900-RECEPRT-FET-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL       =   50
                       MOVE    ZERO                TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
               ELSE
                   MOVE    MCP-RC          TO  WRK-MCP-RC
                   STRING  "レセプト明細ＤＢ更新エラー MCP-RC="
                                                   DELIMITED  BY  SIZE
                           WRK-MCP-RC              DELIMITED  BY  SIZE
                                                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF    
           END-PERFORM    
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
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
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCR0490 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
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
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     >   ZERO )
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
                       IF      JOB-STOPFLG     =   1
      *                    ジョブ管理終了処理
                           MOVE    "CA2"           TO  SJOBKANRI-MODE
                           INITIALIZE                  JOBKANRI-REC
                           MOVE    WRK-RECEERR     TO  JOB-YOBI
                           MOVE    "8888"          TO  JOB-ERRCD
                           MOVE    2               TO  JOB-STOPFLG
grpsys                     PERFORM 900-CALL-ORCSJOB-SEC
                       END-IF
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCR0490 IN   "  CNT-IN
           DISPLAY "*** ORCR0490 OUT  "  CNT-OUT 
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     レセプト管理を主キーで読み込む
      *****************************************************************
       900-RECEPRT-SELECT-SEC          SECTION.
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_receprt"       TO  WRK-MCP-TABLE
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               IF      WRK-PARA-RECE09-PRTJYUN      =   "03"
                   MOVE    "key14"              TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE    "key15"              TO  WRK-MCP-PATHNAME
               END-IF
           ELSE
               MOVE    "key30"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
      *    エラーチェック
           IF      MCP-RC             =   ZERO
               PERFORM 900-RECEPRT-FET-SEC
           ELSE 
               DISPLAY "*** ORCR0490 SELECT ERR " MCP-RC 
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-RECEPRT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードマスタを主キーより検索処理
      *****************************************************************
       900-RECEPRT-FET-SEC            SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
      *
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE08-REC
               ADD     1               TO  CNT-IN-CANCEL
                                           CNT-IN
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
            .
       900-RECEPRT-FET-EXT.
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
