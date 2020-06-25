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
       PROGRAM-ID.                 ORCSKANGOGEN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 夜間看護体制特定日減算取得サブ（平成３０年度改正）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/10/31    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-BTU                     PIC 9(02).
      *
       01  CONS-AREA.
      *
           03  CONS-KANGOGEN-AREA.
      *    夜間看護体制特定日減算（一般病棟入院基本料）
             05   CONS-KANGOGEN-SRYCD1     PIC X(09) VALUE "190200970".
      *    夜間看護体制特定日減算（結核病棟入院基本料）
             05   CONS-KANGOGEN-SRYCD2     PIC X(09) VALUE "190205270".
      *    夜間看護体制特定日減算（精神病棟入院基本料）
             05   CONS-KANGOGEN-SRYCD3     PIC X(09) VALUE "190205370".
      *    夜間看護体制特定日減算（専門病院入院基本料）
             05   CONS-KANGOGEN-SRYCD4     PIC X(09) VALUE "190205470".
      *    夜間看護体制特定日減算（障害者施設等入院基本料）
             05   CONS-KANGOGEN-SRYCD5     PIC X(09) VALUE "190205870".
      *    夜間看護体制特定日減算（地域包括ケア病棟入院料）
             05   CONS-KANGOGEN-SRYCD6     PIC X(09) VALUE "190212670".
      *    療養病棟・夜間看護体制特定日減算（一般病棟入院基本料）
             05   CONS-KANGOGEN-SRYCD7     PIC X(09) VALUE "190214470".
           03  CONS-KANGOGEN-R     REDEFINES  CONS-KANGOGEN-AREA.
             05   CONS-KANGOGEN-SRYCD      PIC X(09)  OCCURS  7.
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    夜間看護体制特定日減算の注加算コード退避エリア
           03  WRK-CHUKSNCD-AREA.
              05  WRK-CHUKSNCD             PIC X(04)  OCCURS  7.
      *    減算率（％）退避エリア
           03  WRK-KANGOGEN-TEN-AREA.
              05  WRK-KANGOGEN-TEN         PIC 9(05)  OCCURS  7.
      *
      *    病棟設定された夜間看護体制特定日減算の対象日退避エリア
           03  WRK-BTU-TAIHI-AREA.
              05  WRK-BTU-TAIHI            OCCURS  99.
                  07  WRK-BTU-TAIHI-BTUNUM PIC X(02).
                  07  WRK-BTU-TAIHI-GENHIT PIC X(01)   OCCURS  31.
      *
      *    患者入院病棟退避エリア
           03  WRK-PTNYUINRRK-BTUNUM       PIC X(02)   OCCURS  31.
      *
      *    患者減算対象日エリア
           03  WRK-PT-GENHIT-AREA.
               05  WRK-PT-GENHIT           PIC X(01)   OCCURS  31.
      *
      *    入院履歴番号退避エリア
           03  WRK-RRKNUM                  PIC 9(03).
      *
      *    月末日算定領域
           03  WRK-MATSUYMD.
               05  WRK-MATSUYY             PIC 9(04).
               05  WRK-MATSUMM             PIC 9(02).
               05  WRK-MATSUDD             PIC 9(02).
      *
      *    日付変換領域
           03  WRK-YMD                     PIC X(08).
           03  WRK-YMDR                    REDEFINES WRK-YMD.
               05  WRK-YY                  PIC 9(04).
               05  WRK-MM                  PIC 9(02).
               05  WRK-DD                  PIC 9(02).
      *
      *    患者年齢算出用エリア
           03  WRK-NENREI-AREA.
              05  WRK-NENREI               PIC 9(03).
              05  WRK-YMDS1.
                  07  WRK-YYS1             PIC 9(04).
                  07  WRK-MMS1             PIC 9(02).
                  07  WRK-DDS1             PIC 9(02).
              05  WRK-YMDS2.
                  07  WRK-YYS2             PIC 9(04).
                  07  WRK-MMS2             PIC 9(02).
                  07  WRK-DDS2             PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （医療機関情報−基本情報）
           COPY    "CPSK1001.INC".
      *    （医療機関情報）
           COPY    "CPSK5000.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *    （病室情報）
           COPY    "CPSK5002.INC".
      *    （夜間看護体制特定日減算情報）
           COPY    "CPSK5008.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者入院履歴マスタ−
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    電子点数表マスタ
       01  ETENSU1-REC.
           COPY   "CPETENSU1.INC".
      *
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    夜間看護体制特定日減算取得パラメタ
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSKANGOGEN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      LNK-KANGOGEN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-KANGOGEN-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           DISPLAY "============================================="
           DISPLAY "LNK-KANGOGEN-RC = " LNK-KANGOGEN-RC
           DISPLAY "============================================="
           IF    ( LNK-KANGOGEN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
           PERFORM    100-KANGOGEN-GET-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    病院診療所種別取得
           INITIALIZE                        SYS-1001-REC
           MOVE    LNK-KANGOGEN-HOSPNUM  TO  SYS-1001-HOSPNUM
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    LNK-KANGOGEN-SANTEIYM TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                  TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD     TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF       MCP-RC               =   ZERO
               MOVE  MCPDATA-REC         TO  SYS-1001-REC
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    病院チェック
           IF     SYS-1001-HOSPSBT       =   1
               CONTINUE
           ELSE
               MOVE    1               TO  LNK-KANGOGEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    算定年月チェック
           IF     (LNK-KANGOGEN-SANTEIYM     =   SPACE)   OR
                  (LNK-KANGOGEN-SANTEIYM    <=  "201803")
               MOVE    2               TO  LNK-KANGOGEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                        PTINF-REC
           MOVE    LNK-KANGOGEN-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-KANGOGEN-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF            NOT =   ZERO
               MOVE    3                TO  LNK-KANGOGEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-KANGOGEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-KANGOGEN-PTID     TO  NACCT-PTID
           MOVE    LNK-KANGOGEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                   TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
           MOVE    "key17"               TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT         NOT =   ZERO
               MOVE    4                 TO  LNK-KANGOGEN-RC
           ELSE
               MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
               MOVE    "key17"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT       =   ZERO
                    CONTINUE
               ELSE
                    MOVE    4           TO  LNK-KANGOGEN-RC
               END-IF
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   990-DBCLOSE-SEC
      *
           IF    LNK-KANGOGEN-RC     NOT =   ZERO
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    システム管理に減算該当日の入力があるかチェック
      *   （ある場合はワーク領域への退避も行う）
           INITIALIZE                      SYSKANRI-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-KANGOGEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    "5008"              TO  SYS-KANRICD
           MOVE    LNK-KANGOGEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           PERFORM   UNTIL   MCP-RC    NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5008-REC
               IF      SYS-5008-YAKANGEN-AREA   NOT =   SPACE
                   PERFORM   VARYING   IDX  FROM   1   BY  1
                      UNTIL  IDX       >    12
                      IF  (SYS-5008-YAKANGENYMD(IDX) NOT =  SPACE) AND
                          (SYS-5008-YAKANGENYMD(IDX)(1:6)
                                       =   LNK-KANGOGEN-SANTEIYM)
                         MOVE   SYS-5008-KBNCD(1:2)   TO  IDX-BTU
                         MOVE   SYS-5008-KBNCD
                                       TO  WRK-BTU-TAIHI-BTUNUM(IDX-BTU)
                         MOVE   SYS-5008-YAKANGENYMD(IDX)(7:2)
                                       TO  IDX2
                         MOVE    "1"   TO  WRK-BTU-TAIHI-GENHIT
                                                        (IDX-BTU IDX2)
                      END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *
      **     PERFORM  VARYING  IDX1  FROM  1  BY  1
      **      UNTIL   (IDX1  >  99)
      **        IF  WRK-BTU-TAIHI-BTUNUM(IDX1)  NOT =  SPACE
      **          DISPLAY "IDX1 = " IDX1
      **          DISPLAY "WRK-BTU-TAIHI-BTUNUM(IDX1) = "
      **                   WRK-BTU-TAIHI-BTUNUM(IDX1)
      **          PERFORM  VARYING  IDX2  FROM  1  BY  1
      **            UNTIL  IDX2  >  31
      **               IF  WRK-BTU-TAIHI-GENHIT(IDX1 IDX2)  NOT =  SPACE
      **                  DISPLAY "IDX2 = " IDX2
      **                  DISPLAY "WRK-BTU-TAIHI-GENHIT(IDX1 IDX2) = "
      **                           WRK-BTU-TAIHI-GENHIT(IDX1 IDX2)
      **               END-IF
      **          END-PERFORM
      **        END-IF
      **     END-PERFORM
      *
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF   WRK-BTU-TAIHI-AREA   =   SPACE
               MOVE    5           TO  LNK-KANGOGEN-RC
           END-IF
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    夜間看護体制特定日減算取得の取り込み処理
      *****************************************************************
       100-KANGOGEN-GET-SEC            SECTION.
      *
      *    CONS-AREAの夜間看護体制特定日減算について注加算コードの取得
           PERFORM   VARYING  IDX   FROM   1  BY  1
             UNTIL   IDX   >   7
                INITIALIZE                           TNS-TENSU-REC
                MOVE    LNK-KANGOGEN-HOSPNUM     TO  TNS-HOSPNUM
                MOVE    CONS-KANGOGEN-SRYCD(IDX) TO  TNS-SRYCD
                MOVE    LNK-KANGOGEN-SANTEIYM    TO  TNS-YUKOSTYMD(1:6)
                MOVE    "01"                     TO  TNS-YUKOSTYMD(7:2)
                MOVE    TNS-YUKOSTYMD            TO  TNS-YUKOEDYMD
                MOVE    "tbl_tensu"              TO  MCP-TABLE
                MOVE    "key"                    TO  MCP-PATHNAME
                PERFORM 910-TENSU-SELECT-SEC
                MOVE    "tbl_tensu"              TO  MCP-TABLE
                MOVE    "key"                    TO  MCP-PATHNAME
                PERFORM 910-TENSU-READ-SEC
                IF      FLG-TENSU                =   ZERO
                        MOVE    TNS-CHUKSNCD     TO  WRK-CHUKSNCD(IDX)
                        MOVE    TNS-TEN      TO  WRK-KANGOGEN-TEN (IDX)
                END-IF
      *
                MOVE    "tbl_tensu"              TO  MCP-TABLE
                MOVE    "key"                    TO  MCP-PATHNAME
                PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      **     DISPLAY  "WRK-CDKSNCD(1) = " WRK-CHUKSNCD(1)
      **     DISPLAY  "WRK-CDKSNCD(2) = " WRK-CHUKSNCD(2)
      **     DISPLAY  "WRK-CDKSNCD(3) = " WRK-CHUKSNCD(3)
      **     DISPLAY  "WRK-CDKSNCD(4) = " WRK-CHUKSNCD(4)
      **     DISPLAY  "WRK-CDKSNCD(5) = " WRK-CHUKSNCD(5)
      **     DISPLAY  "WRK-CDKSNCD(6) = " WRK-CHUKSNCD(6)
      **     DISPLAY  "WRK-CDKSNCD(7) = " WRK-CHUKSNCD(7)
      *
      *    月末日を取得
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY7-AREA
           MOVE      "71"                   TO  LNK-DAY7-IRAI
           MOVE    LNK-KANGOGEN-SANTEIYM    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"     USING      STS-AREA-DAY
                                            LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI          TO  WRK-MATSUYMD
      *
      *    入院履歴から該当月入院時の病棟番号を取得
      *    （新しい歴から逆読み）
      *    MOVE    ZERO                     TO  WRK-RRKNUM
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-KANGOGEN-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-KANGOGEN-PTID        TO  PTNYUINRRK-PTID
           MOVE    LNK-KANGOGEN-SANTEIYM    TO  PTNYUINRRK-NYUINYMD(1:6)
           MOVE    "99"                     TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    LNK-KANGOGEN-SANTEIYM    TO  PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "01"                     TO  PTNYUINRRK-TAIINYMD(7:2)
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key74"                  TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK           =   ZERO
               MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
               MOVE    "key74"              TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL    FLG-PTNYUINRRK  NOT =  ZERO
      *           開始インデックス
                  IF   PTNYUINRRK-TENNYUYMD(1:6) < LNK-KANGOGEN-SANTEIYM
                      MOVE   1              TO  IDX-ST
                  END-IF
                  IF   PTNYUINRRK-TENNYUYMD(1:6) = LNK-KANGOGEN-SANTEIYM
                      MOVE   PTNYUINRRK-TENNYUYMD(7:2) TO  IDX-ST
                  END-IF
      *           終了インデックス
                  IF   PTNYUINRRK-TENSTUYMD(1:6) > LNK-KANGOGEN-SANTEIYM
                      MOVE   WRK-MATSUDD              TO  IDX-ED
                  END-IF
                  IF   PTNYUINRRK-TENSTUYMD(1:6) = LNK-KANGOGEN-SANTEIYM
                      MOVE   PTNYUINRRK-TENSTUYMD(7:2) TO  IDX-ED
                  END-IF
      *           病棟番号セット
                  PERFORM    VARYING  IDX    FROM  IDX-ST  BY  1
                    UNTIL    IDX   >    IDX-ED
                     IF    WRK-PTNYUINRRK-BTUNUM(IDX)  =   SPACE
                        MOVE   PTNYUINRRK-BTUNUM
                                     TO  WRK-PTNYUINRRK-BTUNUM(IDX)
                     ELSE
                        IF     PTNYUINRRK-RRKNUM  =   WRK-RRKNUM
      *                     同じ入院履歴番号（異動処理等）であれば
      *                     置き換えをしない
                            CONTINUE
                        ELSE
      *                     異なる入院履歴番号（同日再入院）であれば
      *                     置き換えをする
                            MOVE   PTNYUINRRK-BTUNUM
                                     TO  WRK-PTNYUINRRK-BTUNUM(IDX)
                        END-IF
                     END-IF
                  END-PERFORM
      *           院履歴番号入れ替え
                  MOVE   PTNYUINRRK-RRKNUM     TO  WRK-RRKNUM
      *
                  MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                  MOVE    "key74"              TO  MCP-PATHNAME
                  PERFORM   910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *    患者入院履歴クローズ
           MOVE     "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE     "key74"                 TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
      *
      **     PERFORM   VARYING  IDX   FROM   1  BY  1
      **       UNTIL   IDX   >   31
      **           DISPLAY "IDX = " IDX
      **           DISPLAY "WRK-PTNYUINRRK-BTUNUM(IDX) = "
      **                    WRK-PTNYUINRRK-BTUNUM(IDX)
      **     END-PERFORM
      *
      *    入院病棟から減算対象日の特定
           PERFORM     VARYING  IDX1  FROM  1  BY  1
             UNTIL     IDX1   >   31
                IF  WRK-PTNYUINRRK-BTUNUM(IDX1)  NOT =  SPACE
                   MOVE   WRK-PTNYUINRRK-BTUNUM(IDX1)  TO  IDX-BTU
                   IF     WRK-BTU-TAIHI-GENHIT(IDX-BTU IDX1)  =  "1"
                       MOVE   "1"         TO   WRK-PT-GENHIT(IDX1)
                   END-IF
                END-IF
           END-PERFORM
      **     PERFORM     VARYING  IDX1  FROM  1  BY  1
      **       UNTIL     IDX1   >   31
      **             DISPLAY "IDX1 = " IDX1
      **             DISPLAY "WRK-PT-GENHIT(IDX1) = " WRK-PT-GENHIT(IDX1)
      **     END-PERFORM
      *
           IF      WRK-PT-GENHIT-AREA   NOT =   SPACE
               INITIALIZE                           NYUINACCT-REC
               MOVE  LNK-KANGOGEN-HOSPNUM       TO  NACCT-HOSPNUM
               MOVE  LNK-KANGOGEN-PTID          TO  NACCT-PTID
               MOVE  LNK-KANGOGEN-SANTEIYM      TO  NACCT-SRYYM
               MOVE  "1"                        TO  NACCT-ZAISKBKBN
               MOVE  "tbl_nyuinacct"            TO  MCP-TABLE
               MOVE  "key35"                    TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT            =   ZERO
                  MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
                  MOVE    "key35"               TO  MCP-PATHNAME
                  PERFORM   910-NYUINACCT-READ-SEC
                  PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *              該当入院行為テーブルの読み込み
                     PERFORM    910-NYUINACT-GET-SEC
      *              点数マスタから入院料の注加算コード取得
                      INITIALIZE                    TNS-TENSU-REC
                      MOVE    LNK-KANGOGEN-HOSPNUM
                                                TO  TNS-HOSPNUM
                      MOVE    NSRY-SRYCD(1)     TO  TNS-SRYCD
                      MOVE    LNK-KANGOGEN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                      MOVE    "01"              TO  TNS-YUKOSTYMD(7:2)
                      MOVE    TNS-YUKOSTYMD     TO  TNS-YUKOEDYMD
                      MOVE    "tbl_tensu"       TO  MCP-TABLE
                      MOVE    "key"             TO  MCP-PATHNAME
                      PERFORM 910-TENSU-SELECT-SEC
                      MOVE    "tbl_tensu"       TO  MCP-TABLE
                      MOVE    "key"             TO  MCP-PATHNAME
                      PERFORM 910-TENSU-READ-SEC
                      IF      FLG-TENSU         =   ZERO
                         PERFORM   VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >    31
                           IF  (WRK-PT-GENHIT(IDX1)     =   "1")   AND
                               (NACCT-DAY(IDX1)         =    1)
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(1)
                                 MOVE  CONS-KANGOGEN-SRYCD(1)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (1)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(2)
                                 MOVE  CONS-KANGOGEN-SRYCD(2)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (2)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(3)
                                 MOVE  CONS-KANGOGEN-SRYCD(3)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (3)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(4)
                                 MOVE  CONS-KANGOGEN-SRYCD(4)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (4)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(5)
                                 MOVE  CONS-KANGOGEN-SRYCD(5)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (5)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(6)
                                 MOVE  CONS-KANGOGEN-SRYCD(6)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (6)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(7)
                                 MOVE  CONS-KANGOGEN-SRYCD(7)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (7)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                           END-IF
                         END-PERFORM
                      END-IF
      *
                      MOVE    "tbl_tensu"      TO  MCP-TABLE
                      MOVE    "key"           TO  MCP-PATHNAME
                      PERFORM 990-DBCLOSE-SEC
      *
                     MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
                     MOVE    "key35"           TO  MCP-PATHNAME
                     PERFORM 910-NYUINACCT-READ-SEC
                  END-PERFORM
               END-IF
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key35"                 TO  MCP-PATHNAME
               PERFORM   990-DBCLOSE-SEC
           END-IF
      *
      **     PERFORM     VARYING  IDX1  FROM  1  BY  1
      **       UNTIL     IDX1   >   31
      **             DISPLAY "IDX1 = " IDX1
      **             DISPLAY "LNK-KANGOGEN-SRYCD(IDX1) = "
      **                      LNK-KANGOGEN-SRYCD(IDX1)
      **     END-PERFORM
      *
           IF   LNK-KANGOGEN-TBL   =   SPACE
               MOVE    5           TO  LNK-KANGOGEN-RC
           END-IF
      *
           .
      *
       100-KANGOGEN-GET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-DBFETCH-SEC
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
           PERFORM   990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       910-TENSU-SELECT-SEC        SECTION.
      *
           MOVE    TNS-TENSU-REC          TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込
      *****************************************************************
       910-TENSU-READ-SEC        SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TNS-TENSU-REC
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               INITIALIZE                     TNS-TENSU-REC
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
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
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       910-NYUINACT-GET-SEC            SECTION.
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
           PERFORM   990-DBCLOSE-SEC
      *
           .
       910-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
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
           PERFORM   910-DBFETCH-SEC
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
