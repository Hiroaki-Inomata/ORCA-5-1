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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSCNKSNCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 診療行為入力入院料加算チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/05/18    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *平成３０年４月改定対応
      *  05.00.00    NACL-小豆沢  18/03/12  入院料と入院料加算のチェック
      *                                     を入院料加算チェックマスタから
      *                                     電子点数表マスタに変更
      *  05.00.00    NACL-小豆沢  18/03/19  入院基本料等加算の中で注の加算
      *                                     は注加算通番ゼロの加算に読み替えて
      *                                     チェックする
      *              NACL-小豆沢  18/07/04  入退院支援加算のチェック追加
      *                                     （注加算通番ゼロに限る）
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-NYUKSNCHK               PIC 9(01).
           03  FLG-NYUKSN-ARI              PIC 9(01).
           03  FLG-KSN                     PIC 9(01).
           03  FLG-SENTEI-SET              PIC 9(01).
           03  FLG-SENTEICDCHG             PIC 9(01).
           03  FLG-ETENSU1                 PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-INPUT-SRYCD             PIC X(09).
           03  WRK-MM-X                    PIC X(02).
           03  WRK-MM-9     REDEFINES   WRK-MM-X
                                           PIC 9(02).
           03  WRK-KSN-NYUKHNKSNKBN        PIC 9(03).
           03  WRK-KSN-CHUKSNCD            PIC X(04).
           03  WRK-KSN-CHUKSNTUBAN         PIC X(01).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-CHUKSNCD-XXX            PIC X(04).
      *
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
      *    同日再入院処理用
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
           03  WRK-DOUJITSU-KBN            PIC X(01).
      *
      *    入院会計ワーク
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-SRYKBN          PIC X(02).
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-KHN-NYUKHNKSNKBN PIC 9(03).
                   07  WRK-HYOUKETSU       PIC X(09).
                   07  WRK-TEISU           PIC X(09).
                   07  WRK-GAIHAKU-KBN     PIC 9(02).
                   07  WRK-CHUKSNCD        PIC X(04).
      *
      *    外泊ワーク
           03  WRK-GAIHAKU-G.
               05  WRK-GAIHAKU             PIC 9(01)  OCCURS  31.
      *
      *    入院料加算ワーク
           03  WRK-NYUKSN-G.
               05  WRK-NYUKSN              PIC 9(01)  OCCURS  31.
      *
      *     医療観察用退避領域
           03  WRK-IRYOKNS-TABLE.
               05  WRK-IRYOKNS-TBL         OCCURS  31.
                   07  WRK-IRYOKNS-SRYCD1  PIC X(09).
                   07  WRK-IRYOKNS-SRYCD2  PIC X(09).
                   07  WRK-IRYOKNS-SRYCD3  PIC X(09).
                   07  WRK-IRYOKNS-SRYCD4  PIC X(09).
      *
      *    選定療養退避用テーブル
           03  TABLE-SENTEI.
               05  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY   "CPNYUKSNCHK.INC".
      *
      *    電子点数表マスタ
       01  ETENSU1-REC.
           COPY   "CPETENSU1.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為入力入院料加算チェックパラメタ
           COPY    "CPORCSCNKSNCHK.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCNKSNCHKAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY  "== ORCSCNKSNCHK.CBL START =="
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           MOVE    SPACE               TO  ORCSCNKSN-ERRMSG
           MOVE    ZERO                TO  ORCSCNKSN-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
      *
      *    主処理
           IF      ORCSCNKSN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           DISPLAY "ORCSCNKSN-RC = " ORCSCNKSN-RC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    診療行為コードチェック
           IF     ORCSCNKSN-SRYCD      =   SPACE
               MOVE    1               TO  ORCSCNKSN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    診療年月チェック
           IF     ORCSCNKSN-SRYYM      =   SPACE
               MOVE    1               TO  ORCSCNKSN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           DISPLAY  "SPA-HOSPNUM = " SPA-HOSPNUM
           DISPLAY  "SPA-PTID = " SPA-PTID
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  ORCSCNKSN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスターの保険組合せ検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    ORCSCNKSN-SRYYM     TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    3               TO  ORCSCNKSN-RC
           ELSE
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key17"         TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT      NOT =   ZERO
                    MOVE    3          TO  ORCSCNKSN-RC
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           PERFORM   200-MAIN-1-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理（入院料加算チェック）
      *****************************************************************
       200-MAIN-1-SEC            SECTION.
      *
           MOVE    SPACE                   TO  WRK-INPUT-SRYCD
           MOVE    ORCSCNKSN-SRYCD         TO  WRK-INPUT-SRYCD
      *
      *    短期滞在手術基本料２と３の入院料未算定日について取得
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM             TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SPA-PTID                TO  LNK-NYUINDAY-PTID
           MOVE    ORCSCNKSN-SRYYM         TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
      *    点数マスタより注加算か入院基本料等加算かを判定
      *    入院料加算の入院基本加算区分を取得
           MOVE    ZERO                    TO  FLG-KSN
           MOVE    ZERO                    TO  WRK-KSN-NYUKHNKSNKBN
           MOVE    SPACE                   TO  WRK-KSN-CHUKSNCD
                                               WRK-KSN-CHUKSNTUBAN
           INITIALIZE                          TNS-TENSU-REC
           MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
           MOVE    ORCSCNKSN-SRYCD         TO  TNS-SRYCD
      *    平成３０年４月からの入院料加算チェック対応
      *  （入院基本料等加算の中で更に注の加算となっているものを読み替え）
      *  （注加算通番０で単純に読み替えができないものが対象）
           IF    ORCSCNKSN-SRYYM      >=   "201804"
      *(A207-3)夜間３０対１急性期看護補助体制加算（急性期看護補助体制加算の注加算）
               IF    ORCSCNKSN-SRYCD   =  "190173010"
                  MOVE   "190145710"   TO  ORCSCNKSN-SRYCD
                                           TNS-SRYCD
               END-IF
      *(A207-3)夜間５０対１急性期看護補助体制加算（急性期看護補助体制加算の注加算）
               IF    ORCSCNKSN-SRYCD   =  "190145970"
                  MOVE   "190145710"   TO  ORCSCNKSN-SRYCD
                                           TNS-SRYCD
               END-IF
      *(A207-3)夜間１００対１急性期看護補助体制加算（急性期看護補助体制加算の注加算）
               IF    ORCSCNKSN-SRYCD   =  "190146070"
                  MOVE   "190145710"   TO  ORCSCNKSN-SRYCD
                                           TNS-SRYCD
               END-IF
      *(A207-3)夜間看護体制加算（急性期看護補助体制加算の注加算）
               IF    ORCSCNKSN-SRYCD   =  "190190770"
                  MOVE   "190145710"   TO  ORCSCNKSN-SRYCD
                                           TNS-SRYCD
               END-IF
           END-IF
           MOVE    ORCSCNKSN-SRYYM         TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD           TO  TNS-YUKOEDYMD
           PERFORM   910-TENSU-READ-SEC
      *    入院料加算以外は処理をスキップする
           IF     (TNS-SRYKBN              =   "90"  OR  "92")  AND
                  (TNS-TENSIKIBETU         =    3)
               CONTINUE
           ELSE
               GO  TO  200-MAIN-1-EXT
           END-IF
      *    注加算（入院基本料）
           IF   ((TNS-CDKBN-KBNNUM         >=  100)   AND
                 (TNS-CDKBN-KBNNUM         <=  199))
               MOVE      1                 TO  FLG-KSN
           END-IF
      *    注加算（特定入院料）
           IF   ((TNS-CDKBN-KBNNUM         >=  300)   AND
                 (TNS-CDKBN-KBNNUM         <=  399))
               MOVE      1                 TO  FLG-KSN
           END-IF
      *    入院基本料等加算
             IF ((TNS-CDKBN-KBNNUM         >=  200)   AND
                 (TNS-CDKBN-KBNNUM         <=  299))  
               MOVE      2                 TO  FLG-KSN
           END-IF
      *
      *    平成３０年３月までは入院料加算チェックマスタを使用する
           IF     ORCSCNKSN-SRYYM   <=     "201803"
               MOVE    TNS-NYUKHNKSNKBN    TO  WRK-KSN-NYUKHNKSNKBN
           END-IF
      *
           MOVE    SPACE                   TO  WRK-KSN-CHUKSNCD
                                               WRK-KSN-CHUKSNTUBAN
           IF     ORCSCNKSN-SRYYM          >=  "201804"
             IF ((TNS-CDKBN-KBNNUM         >=  200)   AND
                 (TNS-CDKBN-KBNNUM         <=  299))
      *        入院基本料等加算
               MOVE    TNS-CHUKSNCD        TO  WRK-KSN-CHUKSNCD
               MOVE    TNS-CHUKSNTUBAN     TO  WRK-KSN-CHUKSNTUBAN
               IF      WRK-KSN-CHUKSNTUBAN  NOT =  "0"
      *          加算コードを注加算通番０のものに読み替える
      *          （(A246)入退院支援加算の注の加算は読み替え対象外）
                 IF   TNS-CDKBN-KBNNUM         =   246
                     CONTINUE
                 ELSE
                   INITIALIZE                        TNS-TENSU-REC
                   MOVE    SPA-HOSPNUM           TO  TNS-HOSPNUM
                   MOVE    WRK-KSN-CHUKSNCD      TO  TNS-CHUKSNCD
                   MOVE    "0"                   TO  TNS-CHUKSNTUBAN
                   MOVE    ORCSCNKSN-SRYYM       TO  TNS-YUKOSTYMD(1:6)
                   MOVE    "01"                  TO  TNS-YUKOSTYMD(7:2)
                   MOVE    TNS-YUKOSTYMD         TO  TNS-YUKOEDYMD
                   PERFORM   910-TENSU-READ2-SEC
                   IF        FLG-TENSU       =   ZERO
                       MOVE  TNS-SRYCD       TO    ORCSCNKSN-SRYCD
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
      *    (A246)入退院支援加算の注の加算はチェック対象外とする
           IF   (TNS-CDKBN-KBN         =   "A")   AND
                (TNS-CDKBN-KBNNUM      =   246)   AND
                (TNS-CDKBN-KBNNUM-EDA  =   0)     AND
                (TNS-CHUKSNTUBAN   NOT =  "0")
                 MOVE      ZERO                 TO  FLG-KSN
           END-IF
      *
      *    入院料の取得
           INITIALIZE                          NYUINACCT-REC
           MOVE    SPA-HOSPNUM             TO  NACCT-HOSPNUM
           MOVE    SPA-PTID                TO  NACCT-PTID
           MOVE    ORCSCNKSN-SRYYM         TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                該当入院行為テーブルの読み込み
                       PERFORM    210-NYUINACT-GET-SEC
      *
      *                点数マスタより入院料の入院基本加算区分を取得
                       INITIALIZE                   TNS-TENSU-REC
                       MOVE    SPA-HOSPNUM      TO  TNS-HOSPNUM
                       MOVE    NSRY-SRYCD(1)    TO  TNS-SRYCD
                       MOVE    ORCSCNKSN-SRYYM  TO  TNS-YUKOSTYMD(1:6)
                       MOVE    "01"             TO  TNS-YUKOSTYMD(7:2)
                       MOVE    TNS-YUKOSTYMD    TO  TNS-YUKOEDYMD
                       PERFORM   910-TENSU-READ-SEC
      *                入院基本料の退避
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   NACCT-DAY(IDX1)    NOT =  ZERO
                             MOVE    NSRY-SRYCD(1)
                                                TO  WRK-NYUINKHN(IDX1)
                             MOVE    TNS-NYUKHNKSNKBN  TO
                                            WRK-KHN-NYUKHNKSNKBN(IDX1)
      *
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE     4      TO      ORCSCNKSN-RC
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
               GO   TO    200-MAIN-1-EXT
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    入院料加算チェック（入院基本料等加算）
           IF    FLG-KSN      =     2
             PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL     IDX1   >   31
               IF    ORCSCNKSN-SRYYM          <=    "201803"
      *          入院料加算チェックマスタによるチェック
                 IF (ORCSCNKSN-DAY(IDX1)         NOT =   ZERO)  AND
                    (WRK-KHN-NYUKHNKSNKBN(IDX1)  NOT =   ZERO)  AND
                    (WRK-KSN-NYUKHNKSNKBN        NOT =   ZERO)
      *
                    INITIALIZE                 NYUKSNCHK-REC
                    MOVE    WRK-KHN-NYUKHNKSNKBN (IDX1)
                                           TO  NYUKSNCHK-NYUINKBN
                    MOVE    WRK-KSN-NYUKHNKSNKBN
                                           TO  NYUKSNCHK-KSNKBN
                    DISPLAY "NYUKSNCHK-NYUINKBN = " NYUKSNCHK-NYUINKBN
                    DISPLAY "NYUKSNCHK-KSNKBN   = " NYUKSNCHK-KSNKBN
                    MOVE    ORCSCNKSN-SRYYM
                                           TO  NYUKSNCHK-YUKOSTYMD(1:6)
                    MOVE    "01"           TO  NYUKSNCHK-YUKOSTYMD(7:2)
                    MOVE    NYUKSNCHK-YUKOSTYMD
                                           TO  NYUKSNCHK-YUKOEDYMD
                    MOVE    "tbl_nyuksnchk" TO MCP-TABLE
                    MOVE    "key"          TO  MCP-PATHNAME
                    PERFORM 910-NYUKSNCHK-SELECT-SEC
                    MOVE    "tbl_nyuksnchk" TO MCP-TABLE
                    MOVE    "key"          TO  MCP-PATHNAME
                    PERFORM 910-NYUKSNCHK-READ-SEC
                    DISPLAY "FLG-NYUKSNCHK = " FLG-NYUKSNCHK
                    IF      FLG-NYUKSNCHK  NOT =   ZERO
                      MOVE    6          TO  ORCSCNKSN-RC
                      MOVE  "算定入院料では算定できない入院料加算です。"
                                           TO      ORCSCNKSN-ERRMSG
                    END-IF
      *             入院料加算チェッククローズ
                    MOVE    "tbl_nyuksnchk"         TO  MCP-TABLE
                    MOVE    "key"                   TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                 END-IF
               ELSE
      *          平成３０年４月以降は電子点数表マスタによるチェック
                 IF (ORCSCNKSN-DAY(IDX1)   NOT =   ZERO)   AND
                    (WRK-NYUINKHN(IDX1)    NOT =   SPACE)
      *
                    INITIALIZE                 ETENSU1-REC
                    MOVE    WRK-NYUINKHN (IDX1)
                                           TO  ETENSU1-SRYCD
                    MOVE    ORCSCNKSN-SRYCD
                                           TO  ETENSU1-UPSRYCD
                    MOVE    ORCSCNKSN-SRYYM
                                           TO  ETENSU1-YUKOSTYMD(1:6)
                    MOVE    "01"           TO  ETENSU1-YUKOSTYMD(7:2)
                    MOVE    ETENSU1-YUKOSTYMD
                                           TO  ETENSU1-YUKOEDYMD
                    MOVE    "tbl_etensu_1" TO  MCP-TABLE
                    MOVE    "key4"         TO  MCP-PATHNAME
                    PERFORM 910-ETENSU1-SELECT-SEC
                    MOVE    "tbl_etensu_1" TO MCP-TABLE
                    MOVE    "key4"         TO  MCP-PATHNAME
                    PERFORM 910-ETENSU1-READ-SEC
                    DISPLAY "FLG-ETENSU1 = " FLG-ETENSU1
                    IF      FLG-ETENSU1    NOT =   ZERO
                      MOVE    6            TO  ORCSCNKSN-RC
                      MOVE  "算定入院料では算定できない入院料加算です。"
                                           TO      ORCSCNKSN-ERRMSG
                    END-IF
      *             電子点数表マスタクローズ
                    MOVE    "tbl_etensu_1"          TO  MCP-TABLE
                    MOVE    "key4"                  TO  MCP-PATHNAME
                    PERFORM   900-CLOSE-SEC
                 END-IF
               END-IF
             END-PERFORM
      *
             IF     ORCSCNKSN-RC             =   6
                  GO  TO  200-MAIN-1-EXT
             END-IF
      *
           END-IF
      *
      *    入院時の外泊取得
           INITIALIZE                          NYUINACCT-REC
           MOVE    SPA-HOSPNUM             TO  NACCT-HOSPNUM
           MOVE    SPA-PTID                TO  NACCT-PTID
           MOVE    ORCSCNKSN-SRYYM         TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            外泊区分の退避
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                           IF   NACCT-DAY(IDX1)  =   1  OR  2  OR  3
                              MOVE    1    TO  WRK-GAIHAKU(IDX1)
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      **     DISPLAY "===  WRK-GAIHAKU ======"
      **     DISPLAY "      ----5----0----5----0----5----0-"
      **     DISPLAY "GAI = " WRK-GAIHAKU-G
      *
      *    外泊日の加算入力チェック
           PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL     IDX1   >   31
               IF    (ORCSCNKSN-DAY(IDX1)  =   1) 
               AND   (WRK-GAIHAKU(IDX1)    =   1)
                   MOVE     7              TO      ORCSCNKSN-RC
                   MOVE  "外泊日に入院料加算が入力されています。"
                                           TO      ORCSCNKSN-ERRMSG
               END-IF
           END-PERFORM
      *
           IF     ORCSCNKSN-RC             =   7
               GO  TO  200-MAIN-1-EXT
           END-IF
      *
      *
      *    入院料加算取得（入院会計）
           MOVE    ZERO                    TO  FLG-NYUKSN-ARI
           INITIALIZE                          NYUINACT-REC
           MOVE    SPA-HOSPNUM             TO  NSRY-HOSPNUM
           MOVE    "1"                     TO  NSRY-NYUGAIKBN
           MOVE    SPA-PTID                TO  NSRY-PTID
           MOVE    ORCSCNKSN-SRYYM         TO  NSRY-SRYYM
           MOVE    WRK-INPUT-SRYCD         TO  NSRY-SRYCD (1)
                                               NSRY-SRYCD (2)
                                               NSRY-SRYCD (3)
                                               NSRY-SRYCD (4)
                                               NSRY-SRYCD (5)
           MOVE    "tbl_nyuinact"          TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT            =   ZERO
             MOVE     "tbl_nyuinact"       TO  MCP-TABLE
             MOVE     "key7"               TO  MCP-PATHNAME
             PERFORM   910-NYUINACT-READ-SEC
             PERFORM   UNTIL   (FLG-NYUINACT    =   1)
      *
      *        入院会計テーブル検索
               INITIALIZE                      NYUINACCT-REC
               MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    SPA-PTID            TO  NACCT-PTID
               MOVE    NSRY-SRYKA          TO  NACCT-SRYKA
               MOVE    ORCSCNKSN-SRYYM     TO  NACCT-SRYYM
               MOVE    NSRY-ZAINUM         TO  NACCT-ZAINUM
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT       =   ZERO
                   MOVE     "tbl_nyuinacct" TO MCP-TABLE
                   MOVE     "key5"         TO  MCP-PATHNAME
                   PERFORM   910-NYUINACCT-READ-SEC
                   PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *
                       MOVE    1           TO  FLG-NYUKSN-ARI
      *
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF  NACCT-DAY(IDX1)    NOT =  ZERO
                               MOVE   1        TO  WRK-NYUKSN(IDX1)
                           END-IF
                       END-PERFORM
      *
                       MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM  910-NYUINACCT-READ-SEC
                   END-PERFORM
               END-IF
      *
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key5"                  TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
      *
               MOVE     "tbl_nyuinact"     TO  MCP-TABLE
               MOVE     "key7"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    入院行為クローズ
           MOVE    "tbl_nyuinact"          TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      **     DISPLAY "===  WRK-NYUKSN ======"
      **     DISPLAY "      ----5----0----5----0----5----0-"
      **     DISPLAY "KSN = " WRK-NYUKSN-G
      *
      *    入院会計と診療行為の加算重複チェック
           PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL     IDX1   >   31
               IF    (ORCSCNKSN-DAY(IDX1)  =   1) 
               AND   (WRK-NYUKSN(IDX1)     =   1)
                   MOVE     8              TO      ORCSCNKSN-RC
                   MOVE  "入院会計で算定済みの入院料加算です。"
                                           TO      ORCSCNKSN-ERRMSG
               END-IF
           END-PERFORM
      *
           IF     ORCSCNKSN-RC             =   8
               GO  TO  200-MAIN-1-EXT
           END-IF
      *
           IF    FLG-NYUKSN-ARI            =   1
               MOVE     9              TO      ORCSCNKSN-RC
               MOVE  "入院会計で剤が作成済みの入院料加算です。"
                                           TO      ORCSCNKSN-ERRMSG
           END-IF
      *
           .
       200-MAIN-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    入院行為クローズ
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    選定入院料振替マスター検索
      *****************************************************************
       910-SENTEICDCHG-SELECT-SEC        SECTION.
      *
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SENTEICDCHG
           ELSE
               MOVE    1                  TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       910-SENTEICDCHG-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    選定入院料振替マスター読込
      *****************************************************************
       910-SENTEICDCHG-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SENTEICDCHG-REC
               MOVE    ZERO               TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                     SENTEICDCHG-REC
               MOVE    1                  TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       910-SENTEICDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算チェック検索
      *****************************************************************
       910-NYUKSNCHK-SELECT-SEC        SECTION.
      *
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUKSNCHK
           ELSE
               MOVE    1                  TO  FLG-NYUKSNCHK
           END-IF
      *
           .
       910-NYUKSNCHK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算チェック読込
      *****************************************************************
       910-NYUKSNCHK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUKSNCHK-REC
               MOVE    ZERO               TO  FLG-NYUKSNCHK
           ELSE
               INITIALIZE                     NYUKSNCHK-REC
               MOVE    1                  TO  FLG-NYUKSNCHK
           END-IF
      *
           .
       910-NYUKSNCHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    電子点数表検索
      *****************************************************************
       910-ETENSU1-SELECT-SEC        SECTION.
      *
           MOVE    ETENSU1-REC            TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-ETENSU1
           ELSE
               MOVE    1                  TO  FLG-ETENSU1
           END-IF
      *
           .
       910-ETENSU1-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電子点数表読込
      *****************************************************************
       910-ETENSU1-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  ETENSU1-REC
               MOVE    ZERO               TO  FLG-ETENSU1
           ELSE
               INITIALIZE                     ETENSU1-REC
               MOVE    1                  TO  FLG-ETENSU1
           END-IF
      *
           .
       910-ETENSU1-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
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
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（key36）
      *****************************************************************
       910-TENSU-READ2-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key36"             TO  MCP-PATHNAME
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
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ2-EXT.
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
