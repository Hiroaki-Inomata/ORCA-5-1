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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGDAPI01S01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : データチェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SELECTED                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDXE                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXM                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TERM                    PIC X(64).
           03  WRK-WINDOW                  PIC X(64).
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY             PIC 9(04).
               05  WRK-DATE-FL1            PIC X(01).
               05  WRK-DATE-MM             PIC 9(02).
               05  WRK-DATE-FL2            PIC X(01).
               05  WRK-DATE-DD             PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH              PIC 9(02).
               05  WRK-HMS-MM              PIC 9(02).
               05  WRK-HMS-SS              PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH             PIC 9(02).
               05  WRK-TIME-FL1            PIC X(01).
               05  WRK-TIME-MM             PIC 9(02).
               05  WRK-TIME-FL2            PIC X(01).
               05  WRK-TIME-SS             PIC 9(02).
           03  WRK-DAY-X                   PIC X(02).
           03  WRK-DAY                     PIC 9(02).
           03  WRK-EOMYMD.
               05  WRK-EOMYM               PIC X(06).
               05  WRK-EOMDD               PIC 9(02).
           03  WRK-STYMD.
               05  WRK-STYM                PIC X(06).
               05  WRK-STDD                PIC 9(02).
           03  WRK-EDYMD.
               05  WRK-EDYM                PIC X(06).
               05  WRK-EDDD                PIC 9(02).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-KBN           PIC X(01).
               05  WRK-KBNCD-SEQ           PIC 9(07).
           03  WRK-LENGTH                  PIC 9(03).
      *
       01  CONST-AREA.
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD        PIC X(08) VALUE "041".
      *    印刷管理ＤＢのキー値
           03  CONST-TBL-KEY           PIC X(08) VALUE "ORCBSD1".
           03  CONST-TBL-GROUP         PIC X(14) VALUE SPACE.
      *    ジョブ管理ＤＢのキー値
           03  CONST-SHELLID           PIC X(08) VALUE "ORCBSD1".
           03  CONST-JOBID             PIC 9(07) VALUE ZERO.
           03  CONST-PTINF-MAX         PIC 9(05) VALUE 100.
      *
           COPY    "DAPI01SCR-SPA.INC".
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
      *    データチェック確認項目
           COPY    "CPDTCHKKOMOKU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    保険分類取得
           COPY    "CPORCSS001.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
           COPY    "CPCK2001.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1101.INC".
           COPY    "CPSK3003.INC".
      *
      *    API XML用定義体
           COPY    "CPAPIV3REQ.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
           COPY    "MONBLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA41SCRAREA.INC".
           COPY    "CPDCV3REQ.INC".
           COPY    "CPDCV3RES.INC".
           COPY    "COMMON-SPA".
           COPY    "D01COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               DCREQ-AREA
                                               DCRES-AREA
                                               SPA-AREA
                                               SPA-D01KYOUTU.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.

      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-API-AREA
           INITIALIZE                  ORCBSD01PARA
           INITIALIZE                  DCRES-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
           IF    ( SYS-1101-INGAIPROCKBN   =   SPACE )
               MOVE    "1"         TO  SYS-1101-INGAIPROCKBN
           END-IF
           IF    ( SYS-1101-PRTSRTKBN      =   SPACE )
               MOVE    "1"         TO  SYS-1101-PRTSRTKBN
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-DC-RUN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KARTE-UID-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JOBAKANRI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( ORCBSD01-NYUGAIKBN  =   "2" )
                   PERFORM 2001-KIKAN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-HOKEN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-ITEM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INGAIPROCKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-PRTSRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( DCREQ-PTINF-AREA    NOT =   SPACE )
                   PERFORM 2001-PTNUM-CHK-SEC
               END-IF
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カルテＵＩＤ・処理状況チェック
      *****************************************************************
       2001-KARTE-UID-CHK-SEC              SECTION.
      *
           IF    ( DCREQ-KARTE-UID     =   SPACE )
               MOVE    "0203"          TO  SPA-ERRCD
           END-IF
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE  FUNCTION LENGTH(
                              FUNCTION TRIM(DCREQ-KARTE-UID TRAILING))
                                   TO  WRK-LENGTH
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   WRK-LENGTH )
      *
                   EVALUATE  FUNCTION TRIM(DCREQ-KARTE-UID TRAILING)
                                                            (IDX0 : 1)
                   WHEN    "A" THRU    "Z"
                   WHEN    "a" THRU    "z"
                   WHEN    "0" THRU    "9"
                   WHEN    "_"
                   WHEN    "-"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0204"      TO  SPA-ERRCD
                   END-EVALUATE
      *
               END-PERFORM
           END-IF
      *
           .
       2001-KARTE-UID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理状況チェック
      *****************************************************************
       2001-JOBAKANRI-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
      *
      *    ジョブ管理チェック
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "3003"          TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD TO SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC TO SYS-3003-REC
           END-IF
      *
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >      SYS-3003-JOBID )
                    OR   ( SPA-ERRCD   =   "0070" )
      *
      *        ジョブ管理チェック処理
               MOVE    "CH2"       TO  SJOBKANRI-MODE
               INITIALIZE              JOBKANRI-REC
               MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
               MOVE    IDX1        TO  JOB-JOBID
               MOVE    CONST-SHELLID
                                   TO  JOB-SHELLID
               PERFORM 800-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   IF    ( JOB-ENDYMD      =   SPACE )
                       MOVE    "0070"      TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-D01-ERRMSG
                       STRING  "処理中です【"  DELIMITED  BY  SIZE
                               JOB-SHELLMSG    DELIMITED  BY  SPACE
                               "】"            DELIMITED  BY  SIZE
                               FUNCTION TRIM (JOB-YOBI)
                                               DELIMITED  BY  SIZE
                       INTO    SPA-D01-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-JOBAKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック
      *****************************************************************
       2001-SRYYM-CHK-SEC              SECTION.
      *
           IF    ( DCREQ-SRYYM             =   SPACE )
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    DCREQ-SRYYM         TO  WRK-DATE
               MOVE    "-01"               TO  WRK-DATE (8:)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1         =   SPACE )
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD (1:6)  TO  ORCBSD01-SRYYM
               END-IF
           END-IF
      *
           .
       2001-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間指定チェック
      *****************************************************************
       2001-KIKAN-CHK-SEC             SECTION.
      *
           IF    ( DCREQ-STDAY  =    SPACE )
            AND  ( DCREQ-EDDAY  =    SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ORCBSD01-SRYYM      TO  WRK-SYMD
               PERFORM 800-GETUMATU-SEC
               MOVE    WRK-SYMD            TO  WRK-EOMYMD
      *
               MOVE    "1"                 TO  ORCBSD01-SRYYMDKBN
      *
               IF    ( DCREQ-STDAY     =   SPACE )
                   MOVE    1               TO  ORCBSD01-STSRYYMD
               ELSE
                   MOVE    "0003"          TO  WRK-ERRCD
                   MOVE    DCREQ-STDAY
                                           TO  WRK-DAY-X
                   PERFORM 20011-KIKAN-CHK-SEC
                   MOVE    WRK-DAY         TO  ORCBSD01-STSRYYMD
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( DCREQ-EDDAY = SPACE )
                       MOVE    WRK-EOMDD   TO  ORCBSD01-EDSRYYMD
                   ELSE
                       MOVE    "0004"      TO  WRK-ERRCD
                       MOVE    DCREQ-EDDAY
                                           TO  WRK-DAY-X
                       PERFORM 20011-KIKAN-CHK-SEC
                       MOVE    WRK-DAY     TO  ORCBSD01-EDSRYYMD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( ORCBSD01-STSRYYMD   >   ORCBSD01-EDSRYYMD )
                       MOVE    "0005"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       2001-KIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間指定チェック
      *****************************************************************
       20011-KIKAN-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO   WRK-DAY
      *
           IF    ( FUNCTION TRIM(WRK-DAY-X)
                                       IS NOT NUMERIC )
               MOVE    WRK-ERRCD       TO   SPA-ERRCD
           ELSE
               MOVE    FUNCTION NUMVAL(WRK-DAY-X)
                                       TO   WRK-DAY
               IF    ( WRK-DAY    >=   1 )
                AND  ( WRK-DAY    <=   WRK-EOMDD )
                   CONTINUE
               ELSE
                   MOVE    WRK-ERRCD   TO   SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-KIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       2001-NYUGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( DCREQ-NYUGAIKBN     =   SPACE )
               MOVE    "0007"          TO  SPA-ERRCD
           ELSE
               EVALUATE    FUNCTION
                             UPPER-CASE(DCREQ-NYUGAIKBN)
               WHEN  "I"
                   MOVE    "1"         TO  ORCBSD01-NYUGAIKBN
               WHEN  "O"
                   MOVE    "2"         TO  ORCBSD01-NYUGAIKBN
               WHEN  OTHER
                   MOVE    "0006"      TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険選択チェック
      *****************************************************************
       2001-HOKEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SELECTED
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   DTCHKHOKEN-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( DCREQ-HOKENID(IDX1)
                                       NOT =   SPACE )
      *
                   MOVE    1       TO  FLG-SELECTED
      *
                   EVALUATE  FUNCTION UPPER-CASE(DCREQ-HOKENID(IDX1))
      *
                   WHEN    SPACE
                       CONTINUE
      *
                   WHEN    FUNCTION UPPER-CASE(DTCHKHOKEN-SHAHOID)
      *
                       MOVE    "1"     TO  ORCBSD01-SYAHOPROCKBN
      *
                   WHEN    FUNCTION UPPER-CASE(DTCHKHOKEN-KOKUHOID)
      *
                       MOVE    "1"     TO  ORCBSD01-KOKUHOPROCKBN
      *
                   WHEN    FUNCTION UPPER-CASE(DTCHKHOKEN-KOKIID)
      *
                       MOVE    "1"     TO  ORCBSD01-KOUKIPROCKBN
      *
                   WHEN    FUNCTION UPPER-CASE(DTCHKHOKEN-RSIJBIID)
      *
                       MOVE    "1"     TO  ORCBSD01-RSIPROCKBN
                       MOVE    "1"     TO  ORCBSD01-JBIPROCKBN
                       MOVE    "1"     TO  ORCBSD01-DAISANPROCKBN
      *
                   WHEN    FUNCTION UPPER-CASE(DTCHKHOKEN-KOGAIID)
      *
                       MOVE    "1"     TO  ORCBSD01-KGIPROCKBN
      *
                   WHEN    FUNCTION UPPER-CASE(DTCHKHOKEN-JIHIID)
      *
                       MOVE    "1"     TO  ORCBSD01-JIHIPROCKBN
                       MOVE    "1"     TO  ORCBSD01-CHIKENPROCKBN
      *
                   WHEN    OTHER
                       MOVE    "0008"      TO  SPA-ERRCD
      *
                   END-EVALUATE
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( FLG-SELECTED    =   ZERO )
                   MOVE    "0009"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-HOKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目選択チェック
      *****************************************************************
       2001-ITEM-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SELECTED
      *
           MOVE    ZERO            TO  ORCBSD01-CHKKBN-G
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   DTCHKKOMOKU-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( DCREQ-ITEMID (IDX1) NOT =   SPACE )
      *
                   MOVE    1       TO  FLG-SELECTED
      *
                   MOVE    ZERO    TO  IDX3
      *
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   DTCHKKOMOKU-MAX )
                            OR   ( IDX3    >   ZERO )
                            OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                       IF    ( FUNCTION UPPER-CASE(
                                 DCREQ-ITEMID (IDX1))
                           =   FUNCTION UPPER-CASE(
                                 DTCHKKOMOKU-CHKKOMOKUID (IDX2)) )
                           MOVE    IDX2    TO  IDX3
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( IDX3    >    ZERO )
                       MOVE    "2"      TO  ORCBSD01-CHKKBN (IDX3)
                   ELSE
                       MOVE    "0010"   TO  SPA-ERRCD
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( FLG-SELECTED    =   ZERO )
                   MOVE    "0011"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-ITEM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外区分チェック
      *****************************************************************
       2001-INGAIPROCKBN-CHK-SEC           SECTION.
      *
           IF    ( DCREQ-INGAIPROCKBN
                           =   SPACE OR "0" OR "1" OR "2" )
               IF    ( DCREQ-INGAIPROCKBN
                           =   SPACE )
                   MOVE    SYS-1101-INGAIPROCKBN
                                       TO  ORCBSD01-INGAIPROCKBN
               ELSE
                   MOVE    DCREQ-INGAIPROCKBN
                                       TO  ORCBSD01-INGAIPROCKBN
               END-IF
           ELSE
                   MOVE    "0012"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-INGAIPROCKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷順チェック
      *****************************************************************
       2001-PRTSRTKBN-CHK-SEC           SECTION.
      *
           IF    ( DCREQ-PRTSRTKBN      =    SPACE OR "1" OR "2"
                                                   OR "3" OR "4" )
               IF    ( DCREQ-PRTSRTKBN  =    SPACE )
                   MOVE    SYS-1101-PRTSRTKBN
                                       TO  ORCBSD01-PRTSRTKBN
               ELSE
                   MOVE    DCREQ-PRTSRTKBN
                                       TO  ORCBSD01-PRTSRTKBN
               END-IF
           ELSE
                   MOVE    "0013"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-PRTSRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       2001-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-API-PTINF-MAX
           MOVE    ZERO                TO  IDXE
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   CONST-PTINF-MAX )
      *
               MOVE    SPACE           TO  SPA-ERRCD
      *
               IF    ( DCREQ-PTNUM (IDXL)  NOT =   SPACE )
                   PERFORM 20011-PTNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXE                >   ZERO )
               MOVE    IDXE            TO  SPA-API-EPTINF-MAX
               MOVE    "0014"          TO  SPA-ERRCD
               INITIALIZE                  SPA-API-PTINF-AREA
           END-IF
      *
           .
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    DCREQ-PTNUM (IDXL)  TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"     USING   ORCSPTNUMAREA
                                           SPA-AREA
           IF    ( SPTNUM-RC       NOT =   ZERO )
               MOVE    "0015"  TO  SPA-ERRCD
               PERFORM 200111-EPTINF-EDIT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                  SS001-AREA
               MOVE    SPA-HOSPNUM     TO  SS001-HOSPNUM
               MOVE    SPTNUM-PTID     TO  SS001-PTID
               MOVE    ORCBSD01-SRYYM  TO  SS001-SRYYM
               CALL    "ORCSS001"          USING   SS001-AREA
                                                   SPA-AREA
      *
               PERFORM 200111-TAISHO-HOKEN-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 200111-EPTINF-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM VARYING IDXM    FROM    1   BY  1
                       UNTIL ( IDXM    >   SPA-API-PTINF-MAX )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPTNUM-PTID =   SPA-API-PTINF-PTID (IDXM))
                       MOVE    "0017"  TO  SPA-ERRCD
                       PERFORM 200111-EPTINF-EDIT-SEC
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               COMPUTE SPA-API-PTINF-MAX   =   SPA-API-PTINF-MAX   +   1
               MOVE    SPTNUM-PTID     TO  SPA-API-PTINF-PTID
                                              (SPA-API-PTINF-MAX)
           END-IF
      *
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       200111-EPTINF-EDIT-SEC             SECTION.
      *
           COMPUTE IDXE    =   IDXE    +   1
           MOVE    DCREQ-PTNUM (IDXL)  TO  SPA-API-EPTINF-PTNUM (IDXE)
           MOVE    SPA-ERRCD           TO  SPA-API-EPTINF-ERRCD (IDXE)
      *
           .
       200111-EPTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    対象保険チェック処理
      *****************************************************************
       200111-TAISHO-HOKEN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SELECTED
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               MOVE    1               TO  IDX1
           ELSE
               MOVE    2               TO  IDX1
           END-IF
      *
           IF    ( SS001-SYA-FLG (IDX1)    =    1 )
            AND  ( ORCBSD01-SYAHOPROCKBN   =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-KOK-FLG (IDX1)    =    1 )
            AND  ( ORCBSD01-KOKUHOPROCKBN  =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-RSI-FLG (IDX1)    =    1 )
            AND  ( ORCBSD01-RSIPROCKBN     =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-DI3-FLG(IDX1)     =    1 )
            AND  ( ORCBSD01-DAISANPROCKBN  =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-JBI-FLG(IDX1)     =    1 )
            AND  ( ORCBSD01-JBIPROCKBN     =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-KGI-FLG(IDX1)     =    1 )
            AND  ( ORCBSD01-KGIPROCKBN     =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-KOUKI-FLG(IDX1)   =    1 )
            AND  ( ORCBSD01-KOUKIPROCKBN   =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-JHI-FLG(IDX1)     =    1 )
            AND  ( ORCBSD01-JIHIPROCKBN    =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( SS001-CHIKEN-FLG(IDX1)  =    1 )
            AND  ( ORCBSD01-CHIKENPROCKBN  =    "1" )
               MOVE    1               TO  FLG-SELECTED
           END-IF
      *
           IF    ( FLG-SELECTED            =   ZERO )
               MOVE    "0016"          TO  SPA-ERRCD
           END-IF
      *
           .
       200111-TAISHO-HOKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    データチェック実行処理
      *****************************************************************
       2003-DC-RUN-SEC               SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    "A%"            TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4101"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-API-PTINF-MAX       >   ZERO )
                   PERFORM 20031-CK2001-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "MONBLOBNEW"    TO  MCP-FUNC
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "monblob"       TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MONBLOB
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MONBLOB-ID  TO  ORCBSD01-DATA-ID
               ELSE
                   MOVE    "4102"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "MONBLOBNEW"    TO  MCP-FUNC
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "monblob"       TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MONBLOB
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MONBLOB-ID  TO  ORCBSD01-PRINT-ID
               ELSE
                   MOVE    "4103"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 20031-DC-RUN-SEC
           END-IF
      *
           .
       2003-DC-RUN-EXT.
           EXIT.
      *****************************************************************
      *    個別指示患者編集処理
      *****************************************************************
       20031-CK2001-EDIT-SEC           SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-API-PTINF-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    "A"             TO  WRK-KBNCD-KBN
      *
               COMPUTE WRK-KBNCD-SEQ
                   =   WRK-KBNCD-SEQ   +   1
      *
               INITIALIZE                  CHK004-REC
               MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
               MOVE    "2001"          TO  CHK004-KANRICD
               MOVE    WRK-KBNCD-G     TO  CHK004-KBNCD
               MOVE    "00000000"      TO  CHK004-STYUKYMD
               MOVE    "99999999"      TO  CHK004-EDYUKYMD
               MOVE    SPA-API-PTINF-PTID (IDXL)
                                       TO  CHK004-PTID
               MOVE    SMCNDATE-YMD    TO  CHK004-CREYMD
                                           CHK004-UPYMD
               MOVE    SMCNDATE-HMS    TO  CHK004-UPHMS
               MOVE    CHK004-REC      TO  MCPDATA-REC
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4104"      TO  SPA-ERRCD
               END-IF
      *
           END-PERFORM
      *
           .
       20031-CK2001-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    個別指示患者編集処理
      *****************************************************************
       20031-DC-RUN-SEC                    SECTION.
      *
           IF    ( SPA-API-PTINF-MAX       >   ZERO )
               MOVE    "2"                 TO  ORCBSD01-IKTKBT
           ELSE
               MOVE    "1"                 TO  ORCBSD01-IKTKBT
           END-IF
      *
           MOVE    "1"                     TO  ORCBSD01-FROM-API
           MOVE    DCREQ-KARTE-UID         TO  ORCBSD01-KARTE-UID
           MOVE    SPA-TERMID              TO  ORCBSD01-ORCA-UID
      *
           MOVE    SMCNDATE-YMD            TO  SPA-D01-UPYMD
           MOVE    SMCNDATE-HMS            TO  SPA-D01-UPHMS
      *
           INITIALIZE                      STEMPDIR-AREA
           CALL     "ORCSTEMPDIR"          USING
                                           STEMPDIR-AREA
           MOVE     STEMPDIR-TEMPDIR       TO  MCP-TEMPDIR
      *
           CALL    "ORCGDSUB02"    USING
                                   ORCBSD01PARA
                                   SPA-AREA
                                   SPA-D01KYOUTU
      *
           .
       20031-DC-RUN-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC              SECTION.
      *
           MOVE    SPA-TERMID          TO  DCRES-ORCA-UID
           MOVE    ORCBSD01-DATA-ID    TO  DCRES-DATA-ID
           MOVE    ORCBSD01-PRINT-ID   TO  DCRES-PRINT-ID
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                         SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API01S01"      TO  SPA-MOTOPG
           CALL    "ORCGDERRMSG"   USING
                                   SPA-AREA
                                   SPA-D01KYOUTU
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC               SECTION.
      *
           MOVE    DCREQ-REQNUM        TO  DCRES-REQNUM
      *
           IF    ( SPA-ERRCD           =   SPACE OR ZERO )
               MOVE    "02"            TO  DCRES-RESNUM
           ELSE
               MOVE    "01"            TO  DCRES-RESNUM
           END-IF
      *
           MOVE    DCREQ-KARTE-UID     TO  DCRES-KARTE-UID
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  DCRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  DCRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO            TO  DCRES-API-RESULT
           ELSE
               MOVE    "E"             TO  DCRES-API-RESULT
               MOVE    SPA-ERRCD       TO  DCRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-D01-ERRMSG      TO  DCRES-API-RESULT-MSG
      *
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   SPA-API-EPTINF-MAX )
      *
               MOVE    SPA-API-EPTINF-PTNUM (IDXE)
                                   TO  DCRES-ERR-PTNUM (IDXE)
               MOVE    "E"         TO  DCRES-ERR-PTNUM-ERRCD (IDXE)
               MOVE    SPA-API-EPTINF-ERRCD (IDXE)
                                   TO  DCRES-ERR-PTNUM-ERRCD (IDXE)(2:)
               MOVE    SPA-API-EPTINF-ERRCD (IDXE)
                                   TO  SPA-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    SPA-D01-ERRMSG
                                   TO  DCRES-ERR-PTNUM-ERRMSG(IDXE)
           END-PERFORM
           MOVE    WRK-ERRCD       TO  SPA-ERRCD
      *
           MOVE    "xml_data_checkv3res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    DCRES-AREA          TO  APIREQ-AREA
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "data_checkv3res"    TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  DAPI01-BODY
               MOVE    "application/xml"
                                       TO  DAPI01-CONTENT-TYPE
            ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    XMLWRITE処理
      *****************************************************************
       910-XMLWRITE-SEC                 SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLWRITE-EXT.
           EXIT.
