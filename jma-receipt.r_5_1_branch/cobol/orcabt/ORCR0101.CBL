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
       PROGRAM-ID.             ORCR0101.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト　印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  ファイル名を連番に変更
      *  01.00.02    MCC-藤原     01.07.13  県内・県外別に印刷のため
      *                                     区分追加 
      *  01.00.03    MCC-藤原     01.10.03  出力先プリンタ名編集
      *  01.00.04    MCC-藤原     01.10.24  出力先プリンタ名が取得
      *                                     できないときエラーにしない
      *  01.01.01    MCC-藤原     02.01.23  帳票出力用パラメタファイル、
      *                                     印刷用データ(HC99)を未使用とする
      *  01.01.02    MCC-藤原     02.02.25  不明分の出力の変更
      *  01.01.03    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.01.04    MCC-藤原     02.03.18  端末ＩＤ
      *                                     X(16)からX(32)に変更
      *  01.01.05    MCC-藤原     02/04/30  印刷順の追加
      *  01.01.06    MCC-藤原     02.05.16  個別の時複数診療月で作成するために
      *                                     診療年月を追加
      *
      *  ２００２年１０月改正対応
      *  01.02.01    NACL-藤原    02/09/27  パラメタに入外区分の追加
      *  01.02.02    NACL-藤原    03/08/28  出力内容指定の処理の追加
      *  01.02.03    NACL-藤原    04/06/20  印刷順の追加（個別作成時のみ）
      *
      *  中止処理追加
      *  01.03.01    NACL-藤原    04/12/01  印刷中止処理の追加
      *
      *  02.06.01    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.02    NACL-藤原    05/07/13  国保の県内県外の印刷順の指定
      *                                     「４」の追加
      *
      *  02.07.01    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *  02.08.02    NACL-藤原    06/04/01  対象データ表示
      *
      *  03.00.01    NACL-藤原    06/05/29  印刷時のプリンター指定対応
      *  03.00.02    NACL-藤原    06/06/29  入力順指定のときの続紙のプリ
      *                                     ンター指定
      *
      *  03.02.01    NACL-藤原    06/08/01  国保主科対応
      *                                     主科用出力順識別番号追加
      *
      *  03.04.01    NACL-藤原    07/01/19  全体での出力順の指定
      *                                     「０７」「０８」の追加
      *  03.04.02    NACL-藤原    07/01/24  種別・カナ氏名順、種別・患者
      *                                     番号順の指定
      *                                     「０９」「１０」の追加
      *                                     MONFUNC対応
      *  03.04.03    NACL-藤原    07/03/01  国保レセプト編綴順詳細設定対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/05/09  エラー時終了処理の追加
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *                                     「１１」の追加
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *                                     「１２」の追加
      *  04.05.02    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *
      *  04.07.01    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.02    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.03    NACL-藤原    13/01/07  自費レセプトを印刷しない設定
      *                                     のとき、入力順、ターミナルデジット
      *                                     出力の対象としない
      *  04.07.04    NACL-藤原    13/03/01  保険者番号の条件設定変更
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
                   07  WRK-PARA-RECE09-PRTJYUN       PIC X(02).
                   07  WRK-PARA-RECE09-DATA.
                       09  WRK-PARA-RECE09-HKNJANUM  PIC X(08).
                       09  WRK-PARA-RECE09-CHOUKI    PIC X(01).
                       09  WRK-PARA-RECE09-ZAITAKU   PIC X(01).
                       09  WRK-PARA-RECE09-KOUGAKU   PIC X(01).
                       09  WRK-PARA-RECE09-SRT-LAST  PIC X(01).
                   07  WRK-PARA-RECE09-DATA-R        REDEFINES
                                                 WRK-PARA-RECE09-DATA.
                       09  WRK-PARA-RECE09-DRCD      PIC X(05).
                       09  WRK-PARA-RECE09-DRCD-PRTJYUNKBN
                                                     PIC X(01).
                       09  FILLER                    PIC X(06).
                   07  WRK-PARA-RECE09-DATA-R1       REDEFINES
                                                 WRK-PARA-RECE09-DATA.
                       09  WRK-PARA-RECE09-BTUNUM    PIC X(02).
                       09  WRK-PARA-RECE09-BTUNUM-PRTJYUNKBN
                                                     PIC X(01).
                       09  FILLER                    PIC X(09).
               05  WRK-PARA-RECE09-STDATA            PIC X(100).
               05  WRK-PARA-RECE09-EDDATA            PIC X(100).
               05  WRK-PARA-PRTJYUN           PIC 9(04).
               05  WRK-PARA-SHELLID           PIC X(08).
               05  WRK-PARA-JOBID             PIC 9(07).
               05  WRK-PARA-PRTKBN            PIC X(01).
               05  WRK-PARA-PREFKBN           PIC X(01).
               05  WRK-PARA-SRYKA             PIC X(02).
               05  WRK-PARA-PRTNM             PIC X(16).
               05  WRK-PARA-PRTNM-N           PIC X(16). 
               05  WRK-PARA-KKHPRTNM          PIC X(16). 
               05  WRK-PARA-KKHPRTNM-N        PIC X(16). 
               05  WRK-PARA-KOUIKIPRTNM       PIC X(16). 
               05  WRK-PARA-KOUIKIPRTNM-N     PIC X(16). 
               05  WRK-PARA-HOSPNUM    PIC  9(02).
      *
           03  WRK-PARA-ARG25          PIC X(100).
           03  WRK-PARA-ARG26          PIC X(100).
      *
           03  WRK-RECE08-PRTRENNUM    PIC 9(06).
      *
           03  WRK-RECEERR             PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      ***  COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
           DISPLAY "*** ORCR0101 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO  WRK-PARA-PRTKBN
                                             WRK-PARA-PREFKBN
                                             WRK-PARA-SRYKA
                                             WRK-PARA-JOBID
                                             WRK-PARA-SHELLID
                                             WRK-PARA1
                                             WRK-PARA-RECE09-REC
                                             WRK-PARA-RECE09-STDATA
                                             WRK-PARA-RECE09-EDDATA
                                             WRK-PARA-PRTJYUN
                                             WRK-PARA-PRTNM
                                             WRK-PARA-PRTNM-N
                                             WRK-PARA-ARG25
                                             WRK-PARA-ARG26
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
           UNSTRING    WRK-PARA-ARG25  DELIMITED  BY  "#/"
                                       INTO  WRK-PARA-KKHPRTNM
                                             WRK-PARA-KOUIKIPRTNM
           END-UNSTRING                                  
           UNSTRING    WRK-PARA-ARG26  DELIMITED  BY  "#/"
                                       INTO  WRK-PARA-KKHPRTNM-N
                                             WRK-PARA-KOUIKIPRTNM-N
           END-UNSTRING
           INSPECT WRK-PARA-RECE09-HKNJANUM  REPLACING   ALL "#"
                                                         BY  " "
      *
      *    リアルタイムプレビューのとき
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
               MOVE    "0"                 TO  WRK-PARA-PREFKBN
           END-IF
           DISPLAY "KKHPRTNM   =" WRK-PARA-KKHPRTNM " "
                                  WRK-PARA-KKHPRTNM-N
           DISPLAY "KOUIKIPRTNM=" WRK-PARA-KOUIKIPRTNM " "
                                  WRK-PARA-KOUIKIPRTNM-N
           DISPLAY "PRTJYUN    =" WRK-PARA-RECE09-PRTJYUN " "
                                  WRK-PARA-PREFKBN
           DISPLAY "HKNJANUM   =" WRK-PARA-RECE09-HKNJANUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0101"      TO  JOB-PGID
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
           MOVE    WRK-PARA-RECE09-PREFKBN      TO  RECE08-PREFKBN
           MOVE    WRK-PARA-RECE09-TEISYUTUSAKI TO  RECE08-TEISYUTUSAKI
           MOVE    WRK-PARA-RECE09-RECESYUBETU  TO  RECE08-RECESYUBETU
           MOVE    WRK-PARA-RECE09-NYUGAIKBN    TO  RECE08-NYUGAIKBN
           IF      WRK-PARA-RECE09-PRTJYUN      =   "11"    OR
                                                    "12"
               CONTINUE
           ELSE
               MOVE    WRK-PARA-RECE09-HKNJANUM
                                            TO  RECE08-HKNJANUM-S
           END-IF
           MOVE    WRK-PARA-SRYKA               TO  RECE08-SRYKA
      *
           MOVE    WRK-PARA-RECE09-STDATA       TO  RECE08-STKANANAME
           MOVE    WRK-PARA-RECE09-EDDATA       TO  RECE08-EDKANANAME
      *
      *    出力内容による頁設定
           EVALUATE    WRK-PARA-PRTKBN
               WHEN    "1"
                   MOVE    1                    TO  RECE08-STRENNUM
                   MOVE    99                   TO  RECE08-EDRENNUM
               WHEN    "2"
                   MOVE    1                    TO  RECE08-STRENNUM
                   MOVE    1                    TO  RECE08-EDRENNUM
               WHEN    "3"
                   MOVE    2                    TO  RECE08-STRENNUM
                   MOVE    99                   TO  RECE08-EDRENNUM
           END-EVALUATE
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
               EVALUATE    WRK-PARA-RECE09-PRTJYUN
                   WHEN    "05"
                       IF    ( RECE08-RECESYUBETU   =   "0000"  OR
                                                        "6666"  OR
                                                        "8888"     )
                       OR    ( RECE08-TEISYUTUSAKI  =   "1"        )
                           IF      RECE08-RENNUM    =   1
                               MOVE    WRK-PARA-PRTNM
                                                    TO  RECE08-PRTNM
                           ELSE
                               MOVE    WRK-PARA-PRTNM-N
                                                    TO  RECE08-PRTNM
                           END-IF
                       ELSE
                           IF      RECE08-TEISYUTUSAKI  =   "2"
                               IF      RECE08-RENNUM    =   1
                                   MOVE    WRK-PARA-KKHPRTNM
                                                        TO  RECE08-PRTNM
                               ELSE
                                   MOVE    WRK-PARA-KKHPRTNM-N
                                                        TO  RECE08-PRTNM
                               END-IF
                           ELSE
                               IF      RECE08-RENNUM    =   1
                                   MOVE    WRK-PARA-KOUIKIPRTNM
                                                        TO  RECE08-PRTNM
                               ELSE
                                   MOVE    WRK-PARA-KOUIKIPRTNM-N
                                                        TO  RECE08-PRTNM
                               END-IF
                           END-IF
                       END-IF
                   WHEN    OTHER
                       IF      RECE08-RENNUM    =   1
                           MOVE    WRK-PARA-PRTNM   TO  RECE08-PRTNM 
                       ELSE
                           MOVE    WRK-PARA-PRTNM-N TO  RECE08-PRTNM 
                       END-IF
               END-EVALUATE
      *
               DISPLAY "PTNUM=" RECE08-PTNUM "#" RECE08-PRTJYUN-SYUKA
                                "#" RECE08-PRTRENNUM "#" RECE08-PRTNM
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
           STRING  "ORCR0101 "         DELIMITED  BY  SIZE
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
           DISPLAY "*** ORCR0101 IN   "  CNT-IN
           DISPLAY "*** ORCR0101 OUT  "  CNT-OUT 
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
           MOVE    "tbl_receprt"       TO  WRK-MCP-TABLE
      *
           EVALUATE    WRK-PARA-RECE09-PRTJYUN ALSO    WRK-PARA-PREFKBN
               WHEN    "06"                    ALSO    ANY
      *            編綴順印刷
                   PERFORM 9001-RECEPRT-KEY-HENSYU-SEC
               WHEN    "05"                    ALSO    ANY
      *            入力順印刷（個別作成時）
                   MOVE    "key30"             TO  WRK-MCP-PATHNAME
               WHEN    "11"                    ALSO    ANY
      *            医師別順印刷
                   PERFORM 9002-DRCD-RECEPRT-KEY-HENSYU-SEC
               WHEN    "12"                    ALSO    ANY
      *            病棟別順印刷
                   PERFORM 9003-BTUNUM-RECEPRT-KEY-HENSYU-SEC
               WHEN    "13"                    ALSO    ANY
      *            ターミナルデジット
                   MOVE    "key52"             TO  WRK-MCP-PATHNAME
               WHEN    ANY                     ALSO    "4"
                   IF      WRK-PARA-RECE09-RECESYUBETU  =   "9999"  
                       EVALUATE    WRK-PARA-RECE09-PRTJYUN
                           WHEN    "03"
                           WHEN    "07"
                               MOVE    "key35"      TO  WRK-MCP-PATHNAME
                           WHEN    "04"
                           WHEN    "08"
                               MOVE    "key36"      TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                   ELSE 
                       EVALUATE    WRK-PARA-RECE09-PRTJYUN
                           WHEN    "01"
                               MOVE    "key31"      TO  WRK-MCP-PATHNAME
                           WHEN    "02"
                               MOVE    "key32"      TO  WRK-MCP-PATHNAME
                           WHEN    "03"
                           WHEN    "07"
                               MOVE    "key38"      TO  WRK-MCP-PATHNAME
                           WHEN    "04"
                           WHEN    "08"
                               MOVE    "key39"      TO  WRK-MCP-PATHNAME
                           WHEN    "09"
                               MOVE    "key33"      TO  WRK-MCP-PATHNAME
                           WHEN    "10"
                               MOVE    "key34"      TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                   END-IF
               WHEN    ANY                     ALSO    "1"
               WHEN    ANY                     ALSO    "2"
               WHEN    ANY                     ALSO    "3"
                   IF      WRK-PARA-RECE09-RECESYUBETU  =   "9999"  
                       EVALUATE    WRK-PARA-RECE09-PRTJYUN
                           WHEN    "03"
                               MOVE    "key16"      TO  WRK-MCP-PATHNAME
                           WHEN    "04"
                               MOVE    "key17"      TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                   ELSE 
                       EVALUATE    WRK-PARA-RECE09-PRTJYUN
                           WHEN    "01"
                           WHEN    "03"
                               MOVE    "key10"      TO  WRK-MCP-PATHNAME
                           WHEN    "02"
                           WHEN    "04"
                               MOVE    "key11"      TO  WRK-MCP-PATHNAME
                           WHEN    "09"
                               MOVE    "key14"      TO  WRK-MCP-PATHNAME
                           WHEN    "10"
                               MOVE    "key15"      TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                   END-IF
           END-EVALUATE
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
      *
      *    エラーチェック
           IF      MCP-RC             =   ZERO
               PERFORM 900-RECEPRT-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-RECEPRT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴順印刷処理
      *****************************************************************
       9001-RECEPRT-KEY-HENSYU-SEC    SECTION.
      *
           MOVE    WRK-PARA-RECE09-CHOUKI  TO  RECE08-CHOUKI
           MOVE    WRK-PARA-RECE09-ZAITAKU TO  RECE08-ZAITAKU
           MOVE    WRK-PARA-RECE09-KOUGAKU TO  RECE08-KOUGAKU
      *
           IF      WRK-PARA-RECE09-RECESYUBETU =   "0000"  OR
                                                   "6666"  OR
                                                   "8888"
               EVALUATE    WRK-PARA-RECE09-SRT-LAST
                   WHEN    "1"
                       MOVE    "key10"      TO  WRK-MCP-PATHNAME
                   WHEN    "2"
                       MOVE    "key11"      TO  WRK-MCP-PATHNAME
               END-EVALUATE
               MOVE    "key11"      TO  WRK-MCP-PATHNAME
           ELSE
               EVALUATE    WRK-PARA-RECE09-TEISYUTUSAKI
                   WHEN    "1"
                       EVALUATE    WRK-PARA-RECE09-SRT-LAST
                           WHEN    "1"
                               MOVE    "key51" TO  WRK-MCP-PATHNAME
                           WHEN    "2"
                               MOVE    "key51" TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                   WHEN    "2" 
                       EVALUATE    WRK-PARA-RECE09-SRT-LAST
                           WHEN    "0"
                               MOVE    "key37"     TO  WRK-MCP-PATHNAME
                           WHEN    "1"
                               MOVE    "key40"     TO  WRK-MCP-PATHNAME
                           WHEN    "2"
                               MOVE    "key41"     TO  WRK-MCP-PATHNAME
                       END-EVALUATE
               END-EVALUATE
           END-IF
      *
           .
       9001-RECEPRT-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師別順印刷処理
      *****************************************************************
       9002-DRCD-RECEPRT-KEY-HENSYU-SEC    SECTION.
      *
           IF      WRK-PARA-RECE09-DRCD =   "XXXXX"
               MOVE    SPACE                TO  RECE08-DRCD
           ELSE
               MOVE    WRK-PARA-RECE09-DRCD TO  RECE08-DRCD
           END-IF
           MOVE    WRK-PARA-SRYKA       TO  RECE08-DRCD-SRYKA
           MOVE    "%%%%%%%%"           TO  RECE08-HKNJANUM-S
      *
           EVALUATE    WRK-PARA-RECE09-DRCD-PRTJYUNKBN
                               ALSO    WRK-PARA-RECE09-RECESYUBETU
               WHEN    "1"     ALSO    "9999"
                   MOVE    "key45"             TO  WRK-MCP-PATHNAME
               WHEN    "2"     ALSO    "9999"
                   MOVE    "key46"             TO  WRK-MCP-PATHNAME
               WHEN    "3"     ALSO    "9999"
                   MOVE    "key47"             TO  WRK-MCP-PATHNAME
               WHEN    "4"     ALSO    "9999"
                   MOVE    "key48"             TO  WRK-MCP-PATHNAME
               WHEN    "1"     ALSO    ANY
               WHEN    "3"     ALSO    ANY
                   MOVE    "key38"             TO  WRK-MCP-PATHNAME
               WHEN    "2"     ALSO    ANY
               WHEN    "4"     ALSO    ANY
                   MOVE    "key39"             TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *
           .
       9002-DRCD-RECEPRT-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟別順印刷処理
      *****************************************************************
       9003-BTUNUM-RECEPRT-KEY-HENSYU-SEC    SECTION.
      *
           IF      WRK-PARA-RECE09-BTUNUM   =   "XX"
               MOVE    SPACE                TO  RECE08-SORT-1
           ELSE
               MOVE    WRK-PARA-RECE09-BTUNUM
                                            TO  RECE08-SORT-1
           END-IF
           MOVE    WRK-PARA-SRYKA       TO  RECE08-SRYKA
           MOVE    "%%%%%%%%"           TO  RECE08-HKNJANUM-S
      *
           EVALUATE    WRK-PARA-RECE09-BTUNUM-PRTJYUNKBN
                               ALSO    WRK-PARA-RECE09-RECESYUBETU
               WHEN    "1"     ALSO    "9999"
                   MOVE    "key49"             TO  WRK-MCP-PATHNAME
               WHEN    "2"     ALSO    "9999"
                   MOVE    "key50"             TO  WRK-MCP-PATHNAME
               WHEN    "1"     ALSO    ANY
                   MOVE    "key38"             TO  WRK-MCP-PATHNAME
               WHEN    "2"     ALSO    ANY
                   MOVE    "key39"             TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *
           .
       9003-BTUNUM-RECEPRT-KEY-HENSYU-EXT.
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
               MOVE    MCPDATA-REC     TO  RECE08-REC
      *
      *        入力順、ターミナルデジットで自費レセプトを印刷しない設定のとき
               IF    ( WRK-PARA-RECE09-PRTJYUN =   "05"    OR  "13" )
               AND   ( WRK-PARA-PREFKBN        =   "1"              )
                   IF      RECE08-RECESYUBETU      =    "8888"
                       GO  TO  900-RECEPRT-FET-SEC
                   END-IF
               END-IF
      *
               MOVE    ZERO            TO  FLG-END
               ADD     1               TO  CNT-IN-CANCEL
                                           CNT-IN
      *
               DISPLAY "PTNUM=" RECE08-PTNUM 
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
