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
       PROGRAM-ID.             ORCR0091.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *                                     KEYが長すぎるので医療機関ＩＤ・
      *                                     診療年月を削除
      *  01.00.02    MCC-小豆沢   02.01.09  診療区分＝９９(コメント等)の時
      *                                     適用欄の先頭に印字するよう変更
      *  01.00.03    MCC-小豆沢   02.02.20  診療区分＝２１(内服)の時
      *                                     入力順で印字するように修正
      *  01.00.04    MCC-小豆沢   02.02.22  診療区分＝９５と９６(自費)の時
      *                                     適用欄の印字しない
      *  01.00.05    MCC-小豆沢   02.03.19  診療区分＝１２を摘要の印字対象
      *                                     に修正（同日再診対応）
      *  01.00.06    MCC-小豆沢   02.04.20  診療区分＝８０の摘要欄印字順を
      *                                     変更
      *  01.00.07    NACL-藤原    02.09.04  検査のときは表用番号を編集しない
      *                                     検査のときZAITENを編集する
      *  01.00.08    NACL-藤原    02.09.06  初診のとき編集する
      *  01.00.09    NACL-小豆沢  02.09.28  検査の摘要欄印刷順を修正
      *  01.00.10    NACL-藤原    02.11.07  医療機関ＩＤ・診療年月を追加
      *  01.00.11    NACL-門脇    02.11.26  検診コードの診療区分編集
      *  01.00.12    NACL-藤原    03.03.12  補助区分の追加
      *  01.00.13    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.14    NACL-門脇    03.03.06  初再診のコメント編集対応
      *  01.00.15    NACL-門脇    03.11.26  自費レセプトの自費分
      *                                     摘要欄記載対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/09/28  中止処理の追加
      *  01.01.02    NACL-門脇    05/01/06  自費レセプトの出力条件修正
      *  01.01.03    NACL-門脇    05/02/28  （サ）記載対応
      *
      *  02.06.01    NACL-門脇    05/05/26  手術・麻酔の記載順変更対応
      *  02.06.02    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.03    NACL-門脇    05/08/31  剤番号編集修正
      *  02.06.04    NACL-門脇    05/11/01  （検選）等コメント対応
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *  04.02.02    NACL-門脇    08/04/21  ８０その他記載順対応
      *
      *  04.03.01    NACL-門脇    08/07/08  診区１４のコメントのみの剤
      *                                     編集対応
      *  04.03.02    NACL-門脇    08/07/09  投薬の記載順変更（日付順）
      *  04.03.03    NACL-門脇    08/07/14  投薬剤点０（院外等）記載対応
      *  04.04.01    NACL-門脇    10/03/29  改正対応
      *                                     （画像診断２回目以降減算編集）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    10/01/12  治験対応
      *  04.05.03    NACL-門脇    11/06/20  公費分離区分区切り編集修正
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-門脇    12/10/15  診療行為入力したコメントのみの剤の
      *                                     レセプト記載対応
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE06-FILE     ASSIGN  RECE06PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE06-KEY
                                   FILE    STATUS  IS  STS-RECE06.
           SELECT  RECE061-FILE    ASSIGN  RECE061PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE061-KEY
                                   FILE    STATUS  IS  STS-RECE061.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE06-FILE.
       01  RECE06-REC. 
           COPY    "CPRCF006.INC".
      *
       FD  RECE061-FILE.
       01  RECE061-REC. 
           COPY    "CPRCF0061.INC".
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE06PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE061PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE06                              PIC X(02).
           03  STS-RECE061                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  SAISHIN-FLG                             PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE060"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE061"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE06PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE061PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0091"      TO  JOB-PGID
               MOVE    "摘要欄編集"    TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE061-FILE
           CLOSE                       RECE061-FILE
      *
           OPEN    INPUT               RECE06-FILE
           IF      STS-RECE06          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE06PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE06          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE061-FILE
      *
           PERFORM    900-RECE06-READ-SEC
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     900-RECE06-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           RECE061-REC
grpsys     MOVE    RECE06-KEY1          TO      RECE061-KEY1
           MOVE    RECE06-KOHBUNRI      TO      RECE061-KOHBUNRI
           MOVE    RECE06-ZAINUM        TO      RECE061-ZAINUM
           IF      RECE06-ZAINUM       =   99999981  OR  99999982  OR
                                           99999983  OR  99999984  OR
                                           99999985
                 MOVE     ZERO          TO      RECE061-ZAINUM
           END-IF
           IF      RECE06-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE061-ZAINUM
           END-IF
           IF      RECE06-SRYKBN        =       "60" OR "64"
              IF   RECE06-KNSJISKBN     =       2
                   MOVE    5            TO      RECE061-KNSJISKBN
              ELSE
                  IF   (RECE06-CDKBN-KBNNUM >=        0)   AND
                       (RECE06-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   (RECE06-CDKBN-KBNNUM >=      400)   AND
                       (RECE06-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   (RECE06-CDKBN-KBNNUM >=      200)   AND
                       (RECE06-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   RECE06-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE061-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE06-SAFLG   NOT =   SPACE )
              OR   (RECE06-ZAIKBN      =   1 OR 2)
                   MOVE    9            TO      RECE061-KNSJISKBN
              END-IF
           END-IF     
      ***     MOVE    RECE06-KNSJISKBN     TO      RECE061-KNSJISKBN
           MOVE    RECE06-LINENUM       TO      RECE061-LINENUM
           MOVE    RECE06-ZAIMEISAISU   TO      RECE061-ZAIMEISAISU
           MOVE    RECE06-PTNUM         TO      RECE061-PTNUM
           MOVE    RECE06-SRYKA         TO      RECE061-SRYKA
           IF     (RECE06-SRYKBN          =  "99" )
           AND    (RECE06-SRYSYUKBN   NOT =  "991")
               MOVE  SPACE              TO      RECE061-SRYKBN
           ELSE
               MOVE  RECE06-SRYKBN      TO      RECE061-SRYKBN
           END-IF
           MOVE    RECE06-ASTER         TO      RECE061-ASTER
           MOVE    RECE06-TEKIYO        TO      RECE061-TEKIYO
           IF      RECE06-SRYKBN        =       "12"
            IF      RECE06-ASTER         =       "＊"
              IF   RECE06-TEKIYO(1:24)  =
                   "再診（診療所）（１回目）"
                   MOVE   "1"           TO      SAISHIN-FLG
              END-IF
            END-IF
            IF   RECE06-TEKIYO(49:2)  =   SPACE
                 CONTINUE
            ELSE     
              IF  SAISHIN-FLG          =      "1"
                 MOVE      SPACE        TO      SAISHIN-FLG
              END-IF
            END-IF 
           END-IF 
           IF      RECE06-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE06-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE06-ZAITEN   TO  RECE061-ZAITEN
               END-IF
               MOVE   RECE06-CDKBN-SYO     TO  RECE061-CDKBN-SYO
               MOVE   RECE06-CDKBN-BU      TO  RECE061-CDKBN-BU
               MOVE   RECE06-CDKBN-KBN     TO  RECE061-CDKBN-KBN
               MOVE   RECE06-CDKBN-KBNNUM  TO  RECE061-CDKBN-KBNNUM
               MOVE   RECE06-CDKBN-KBNNUM-EDA
                                           TO  RECE061-CDKBN-KBNNUM-EDA
               MOVE   RECE06-CDKBN-KOUBAN  TO  RECE061-CDKBN-KOUBAN
               MOVE   RECE06-KOUHYOJUNNUM  TO  RECE061-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE06-SRYSYUKBN   TO   RECE061-SRYSYUKBN
           MOVE   RECE06-DAY         TO   RECE061-DAY
      *
           MOVE   RECE06-SYOHOUFLG   TO   RECE061-SYOHOUFLG
      *
           MOVE   RECE06-ZAIKBN      TO   RECE061-ZAIKBN
      *
           MOVE   RECE06-DAY2        TO   RECE061-DAY2
      *
           MOVE   RECE06-HKNCOMBI    TO   RECE061-HKNCOMBI
      *
           MOVE   RECE06-JIHOKBN     TO   RECE061-JIHOKBN
      *
           IF     RECE06-RECESYUBETU  =  8888
              IF     RECE06-ZAITEN  NOT =  ZERO
                  PERFORM     2101-RECE061-WRITE-SEC
              ELSE
                IF   RECE06-SRYKBN  NOT =  "95" AND "96"
                     PERFORM  2101-RECE061-WRITE-SEC
                END-IF
              END-IF
           ELSE
              IF     RECE06-SRYKBN  NOT =  "95" AND "96"
                  PERFORM     2101-RECE061-WRITE-SEC
              END-IF
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００６１）
      *****************************************************************
       2101-RECE061-WRITE-SEC                SECTION.
      *
           WRITE   RECE061-REC
      *
           IF      STS-RECE061         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE061PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE061         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       2101-RECE061-WRITE-EXT.
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
           STRING  "ORCR0091 "             DELIMITED   BY  SIZE
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
           CLOSE   RECE06-FILE
                   RECE061-FILE
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
           DISPLAY "RECE06  CNT-IN  :" CNT-IN
           DISPLAY "RECE061 CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0091 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE06-READ-SEC            SECTION.
      *
           READ    RECE06-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   IF      STS-RECE06          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE06PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE06      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE06  PTID=" RECE06-PTID
           END-READ
           .
       900-RECE06-READ-EXT.
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
