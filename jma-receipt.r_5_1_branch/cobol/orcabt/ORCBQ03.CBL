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
       PROGRAM-ID.             ORCBQ03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 検索結果データ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/10/31    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付       内容
      *  03.05.00    NACL-太田    07.05.23 グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  BQ01-FILE       ASSIGN  PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-BQ01.
      *
      *    診療行為数値検索チェックファイル
           SELECT  TMP01-FILE      ASSIGN  TMP01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TMP01-KEY
                                   FILE    STATUS  IS  STS-TMP01.
      *
      *    同一剤チェックファイル
           SELECT  TMP02-FILE      ASSIGN  TMP02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TMP02-KEY
                                   FILE    STATUS  IS  STS-TMP02.
      *
      *    診療行為月別集計ファイル
           SELECT  TMP03-FILE      ASSIGN  TMP03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TMP03-KEY
                                   FILE    STATUS  IS  STS-TMP03.
      *
      *    診療行為日別集計ファイル
           SELECT  TMP04-FILE      ASSIGN  TMP04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TMP04-KEY
                                   FILE    STATUS  IS  STS-TMP04.
      *
      *
      *    パラメータファイル（LINE SEQUENTIAL にはしないこと!!)
           SELECT  PUSHFIND-FILE   ASSIGN  PUSHNAME
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-PUSHNAME.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
       FD  TMP01-FILE.
       01  TMP01-REC.
           03  TMP01-KEY.
               05  TMP01-HOSPNUM               PIC 9(02).
               05  TMP01-PTID                  PIC 9(10).
           03  TMP01-PTNUM                     PIC X(20).
           03  TMP01-KANANAME                  PIC X(100).
           03  TMP01-SEX                       PIC X(1).
           03  TMP01-BIRTHDAY                  PIC X(8).
           03  TMP01-SRYCD-G.
               05  TMP01-SRYCD-OCC OCCURS 5.
                   07  TMP01-SRYCD             PIC X(09).
                   07  TMP01-SRYSURYO1         PIC 9(05)V9(05).
                   07  TMP01-SRYSURYO2         PIC 9(05)V9(05).
                   07  TMP01-ZAIKAISU          PIC 9(07).
                   07  TMP01-SURYOTOTAL        PIC 9(07)V9(05).
      *
       FD  TMP02-FILE.
       01  TMP02-REC.
           03  TMP02-KEY.
               05  TMP02-HOSPNUM               PIC 9(02).
               05  TMP02-PTID                  PIC 9(10).
               05  TMP02-ZAINUM                PIC 9(09).
               05  TMP02-SRYCD                 PIC X(09).
               05  TMP02-RENNUM                PIC 9(02).
               05  TMP02-SRYCD-IDX             PIC 9(02).
      *
       FD  TMP03-FILE.
       01  TMP03-REC.
           03  TMP03-KEY.
               05  TMP03-HOSPNUM               PIC 9(02).
               05  TMP03-PTID                  PIC 9(10).
               05  TMP03-SRYCD                 PIC X(09).
               05  TMP03-SRYYM                 PIC X(06).
               05  TMP03-SRYSURYO              PIC 9(05)V9(05).
           03  TMP03-ZAIKAISU                  PIC 9(07).
           03  TMP03-SURYOTOTAL                PIC 9(07)V9(05).
           03  TMP03-DAY                       PIC 9(05)
                                               OCCURS  31.
      *
       FD  TMP04-FILE.
       01  TMP04-REC.
           03  TMP04-KEY.
               05  TMP04-HOSPNUM               PIC 9(02).
               05  TMP04-PTID                  PIC 9(10).
               05  TMP04-SRYCD                 PIC X(09).
               05  TMP04-SRYYMD                PIC X(08).
               05  TMP04-SRYSURYO              PIC 9(05)V9(05).
           03  TMP04-ZAIKAISU                  PIC 9(03).
      *
       FD  PUSHFIND-FILE.
       01  PUSHFIND-REC.
           COPY    "CPPUSHFIND.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ワークパラメータファイル
           COPY    "CPBQ01.INC"    REPLACING  //BQ01//
                                   BY         //WKBQ01//.
      *
           COPY    "CPCOMMONDAT3.INC".
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP01PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP02PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP03PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP04PARA//.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PRFNAME//.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PUSHNAME//.
      *
      *    画面SPA(KYOUTU領域を使用)
           COPY    "Q01SCR-SPA".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BQ01                PIC X(02).
           03  STS-TMP01               PIC X(02).
           03  STS-TMP02               PIC X(02).
           03  STS-TMP03               PIC X(02).
           03  STS-TMP04               PIC X(02).
           03  STS-PUSHNAME            PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXM                    PIC 9(05).
           03  IDXN                    PIC 9(05).
           03  IDXR                    PIC 9(05).
           03  IDX-SRY                 PIC 9(05).
      *
           03  IDXD                    PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SRYSRH              PIC 9(01).
           03  FLG-SKYSRH              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-TMP01               PIC 9(01).
           03  FLG-TMP02               PIC 9(01).
           03  FLG-TMP03               PIC 9(01).
           03  FLG-TMP04               PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
           03  FLG-TARGET-SRYCD-G.
               05  FLG-TARGET-SRYCD    PIC 9(01)
                                       OCCURS  5.
           03  FLG-TARGET-ACCT         PIC 9(01).
           03  FLG-DBERR               PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-TMP03               PIC 9(03).
           03  CNT-TMP04               PIC 9(03).
      *
       01  SEL-AREA.
           03  SEL-SRYKA                           PIC X(02).
           03  SEL-NYUGAIKBN                       PIC X(01).
           03  SEL-DRCD                            PIC X(05).
           03  SEL-SRYKBN                          PIC X(02).
           03  SEL-SRYSYUKBN                       PIC X(03).
           03  SEL-STSRYYMD                        PIC X(08).
           03  SEL-EDSRYYMD                        PIC X(08).
           03  SEL-SRYCD                           PIC X(09).
           03  SEL-DATAKBN                         PIC X(01).
           03  SEL-RRKSRH                          PIC 9(01).
           03  SEL-SRYSRH                          PIC 9(01).
           03  SEL-SKYSRH                          PIC 9(01).
           03  SEL-UPYMD                           PIC X(08).
           03  SEL-UPHMS                           PIC X(06).
           03  SEL-TENKENRECE                      PIC X(01).
           03  SEL-SRYYM                           PIC X(06)
                                                   OCCURS 12.
           03  SEL-KOHNUM                          PIC X(03).
      *
       01  TSRYCD-G.
           03  TSRYCD-IDX                          PIC 9(05).
           03  TSRYCD-OCC                          OCCURS 5.
               05  TSRYCD-SRYVAL                   PIC X(20).
               05  TSRYCD-SRYVALNUM                PIC 9(05)V9(05).
               05  TSRYCD-SRYVALCOMP               PIC X(01).
               05  TSRYCD-KANANAME                 PIC X(100).
               05  TSRYCD-NAME                     PIC X(100).
               05  TSRYCD-SRYCD                    PIC X(09).
               05  TSRYCD-SURYO-EDIT               PIC 9(01).
               05  TSRYCD-TANICD                   PIC X(03).
               05  TSRYCD-TANINAME                 PIC X(12).
      *
       01  WRK-AREA.
           03  WRK-YMD                             PIC X(10). 
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
           03  WRK-DELYMDHMS.
               05  WRK-DELYMDHMS-YMD               PIC X(08).
               05  WRK-DELYMDHMS-HMS               PIC X(06).
           03  WRK-ACCT-G.
               05  WRK-ZAIKAISU                    PIC 9(05).
               05  WRK-SURYOTOTAL                  PIC 9(07)V9(05).
               05  WRK-DAY-G.
                   07  WRK-DAY                     PIC 9(03)
                                                   OCCURS 31.
           03  WRK-SRYVAL                          PIC 9(07)V9(05).
           03  WRK-SRYYMD.
               05  WRK-SRYYM                       PIC X(06).
               05  WRK-SRYDD                       PIC 9(02).
           03  WRK-RENNUM                          PIC 9(05).
           03  WRK-KARTE-UID                       PIC X(36).
           03  WRK-COUNT                           PIC 9(10).
           03  WRK-TMP-SRYCD                       PIC X(09).
           03  WRK-TMP-SRYYMD.
               05  WRK-TMP-SRYYM                   PIC X(06).
               05  WRK-TMP-SRYDD                   PIC 9(02).
           03  WRK-TMP-SRYSURYO                    PIC 9(05)V9(05).
           03  WRK-SRYSURYOX                       PIC X(13).
           03  WRK-SRYSURYOZ-G.
               05  WRK-SRYSURYOZ                   PIC ZZZZZZ9.99999.
           03  WRK-NUM03Z                          PIC ZZ9.
           03  WRK-NUM07Z                          PIC ZZZZZZ9.
           03  WRK-GYOUMUID                        PIC X(08).
           03  WRK-NUMZ                            PIC Z(14)9.
      *
       01  COSNT-AREA.
           03  CONST-VALSURYO          PIC X(01) VALUE "1".
           03  CONST-VALKAISU          PIC X(01) VALUE "2".
           03  CONST-VALTOTAL          PIC X(01) VALUE "3".
           03  CONST-SRYACCT           PIC X(01) VALUE "1".
           03  CONST-NYUINACCT         PIC X(01) VALUE "2".
           03  CONST-EQUAL             PIC X(01) VALUE "1".
           03  CONST-GREATER-EQUAL     PIC X(01) VALUE "2".
           03  CONST-LESS-EQUAL        PIC X(01) VALUE "3".
           03  CONST-AND               PIC X(01) VALUE "1".
           03  CONST-NOT               PIC X(01) VALUE "9".
           03  CONST-KOHNUM-VAL.
               05                      PIC X(03) VALUE "946".
               05                      PIC X(03) VALUE "948".
               05                      PIC X(03) VALUE "949".
               05                      PIC X(03) VALUE "950".
               05                      PIC X(03) VALUE "951".
               05                      PIC X(03) VALUE "953".
               05                      PIC X(03) VALUE "954".
               05                      PIC X(03) VALUE "955".
               05                      PIC X(03) VALUE "956".
               05                      PIC X(03) VALUE "957".
               05                      PIC X(03) VALUE "958".
               05                      PIC X(03) VALUE "959".
               05                      PIC X(03) VALUE "960".
               05                      PIC X(03) VALUE "961".
               05                      PIC X(03) VALUE "962".
               05                      PIC X(03) VALUE "963".
               05                      PIC X(03) VALUE "964".
               05                      PIC X(03) VALUE "965".
               05                      PIC X(03) VALUE "966".
               05                      PIC X(03) VALUE "967".
               05                      PIC X(03) VALUE "968".
               05                      PIC X(03) VALUE "969".
               05                      PIC X(03) VALUE "976".
               05                      PIC X(03) VALUE "977".
               05                      PIC X(03) VALUE "978".
               05                      PIC X(03) VALUE "979".
           03  CONST-KOHNUM-R          REDEFINES CONST-KOHNUM-VAL.
               05  CONST-KOHNUM        PIC X(03) OCCURS    26.
           03  CONST-KOHNUM-MAX        PIC 9(03) VALUE     26.
           03  CONST-ACCT-OCC-MAX      PIC 9(03) VALUE     10.
           03  CONST-FINDID            PIC X(08) VALUE   "api013".
           03  CONST-FOUNDID           PIC X(08) VALUE   "api01301".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    受診履歴テーブル
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    請求管理照会テーブル
       01  SKYSRH-REC.
           COPY    "CPSKYSRH.INC".
      *
      *    診療行為照会テーブル
       01  SRYSRH-REC.
           COPY    "CPSRYSRH.INC".
      *
      *    患者情報照会テーブル
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    受診履歴照会テーブル
       01  RRKSRH-REC.
           COPY    "CPRRKSRH.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA013.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
      ******************************************************************
       PROCEDURE                       DIVISION
                                       USING     COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           MOVE    ZERO            TO  RETURN-CODE
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       CNT-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       SEL-AREA
                                       TSRYCD-G
                                       SPA-AREA
                                       SPA-Q01FREE
      *
           PERFORM 800-MCNDATE-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    SPA-TERMID
                                               SPA-HOSPNUM
                                               PRFNAME
                                               SEL-UPYMD
                                               SEL-UPHMS
                                               SPA-OPID
                                               WRK-KARTE-UID
                                               PUSHNAME
           END-UNSTRING
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
           MOVE    SPA-HOSPNUM         TO  TMPFLPARA-HOSPNUM
           MOVE    ALL "0"             TO  TMPFLPARA-TERMID
           STRING  SPA-TERMID  DELIMITED   BY  SPACE
           INTO    TMPFLPARA-TERMID
           END-STRING
      *
           MOVE    TMPFLPARA           TO  TMP01PARA
           MOVE    "BQ03TMP1"          TO  TMP01PARA-FILE-ID
      *
           MOVE    TMPFLPARA           TO  TMP02PARA
           MOVE    "BQ03TMP2"          TO  TMP02PARA-FILE-ID
      *
           MOVE    SPA-HOSPNUM         TO  TMP03PARA-HOSPNUM
           MOVE    "BQ03TMP3"          TO  TMP03PARA-FILE-ID
           MOVE    SPA-TERMID          TO  TMP03PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  TMP04PARA-HOSPNUM
           MOVE    "BQ03TMP4"          TO  TMP04PARA-FILE-ID
           MOVE    SPA-TERMID          TO  TMP04PARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    PRFNAME             TO  SGETTEMP-BASENAMES (1)
           MOVE    TMP01PARA-BASENAME  TO  SGETTEMP-BASENAMES (2)
           MOVE    TMP02PARA-BASENAME  TO  SGETTEMP-BASENAMES (3)
           MOVE    TMP03PARA-BASENAME  TO  SGETTEMP-BASENAMES (4)
           MOVE    TMP04PARA-BASENAME  TO  SGETTEMP-BASENAMES (5)
           MOVE    PUSHNAME            TO  SGETTEMP-BASENAMES (6)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PRFNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TMP01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  TMP02PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  TMP03PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  TMP04PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  PUSHNAME
      *
      *    パラメータファイルオープン
           OPEN    INPUT               BQ01-FILE
      *
      *    パラメータファイル読み込み
           READ    BQ01-FILE
           END-READ
      *
           MOVE    BQ01                TO  WKBQ01
      *
           CLOSE   BQ01-FILE
      *
           MOVE    WKBQ01-Q01KYOUTU    TO  SPA-Q01KYOUTU
      *
           PERFORM 1001-INIT-SEC
      *
           MOVE    SEL-UPYMD           TO  WRK-YMD
           COMPUTE WRK-ZOGEN   =   -1
           MOVE    "1"                 TO  WRK-ZOGENPTN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WRK-DELYMDHMS-YMD
           MOVE    SEL-UPHMS           TO  WRK-DELYMDHMS-HMS
      *
           PERFORM 900-PTSRH-DEL4-SEC
           PERFORM 900-SRYSRH-DEL3-SEC
           PERFORM 900-RRKSRH-DEL3-SEC
           PERFORM 900-SKYSRH-DEL3-SEC
      *
           MOVE    CONST-FINDID        TO  WRK-GYOUMUID
           PERFORM 900-APIPARA-DEL5-SEC
      *
           MOVE    CONST-FOUNDID       TO  WRK-GYOUMUID
           PERFORM 900-APIPARA-DEL5-SEC
      *
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           DISPLAY "SQL[" WKBQ01-SQL "]"
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       1001-INIT-SEC               SECTION.
      *
           IF    ( SPA-Q01-STSRYYMD    =   SPACE )
               MOVE    "00000000"          TO  SEL-STSRYYMD
           ELSE
               MOVE    SPA-Q01-STSRYYMD    TO  SEL-STSRYYMD
               PERFORM VARYING     IDX0    FROM    1   BY  1
                       UNTIL (     IDX0    >   12 )
                   MOVE    SEL-STSRYYMD TO WRK-YMD
                   COMPUTE WRK-ZOGEN   =   IDX0    -   1
                   MOVE    "2"         TO  WRK-ZOGENPTN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD(1:6)
                                           TO  SEL-SRYYM (IDX0)
               END-PERFORM
           END-IF
      *
           IF    ( SPA-Q01-EDSRYYMD    =   SPACE )
               MOVE    "99999999"          TO  SEL-EDSRYYMD
           ELSE
               MOVE    SPA-Q01-EDSRYYMD    TO  SEL-EDSRYYMD
           END-IF
      *
           IF    ( SPA-Q01-SRYKA       =   SPACE )
               MOVE    "%"                 TO  SEL-SRYKA
           ELSE
               MOVE    SPA-Q01-SRYKA       TO  SEL-SRYKA
           END-IF
      *
           IF    ( SPA-Q01-NYUGAIKBN   =   SPACE )
               MOVE    "%"                 TO  SEL-NYUGAIKBN
           ELSE
               MOVE    SPA-Q01-NYUGAIKBN   TO  SEL-NYUGAIKBN
           END-IF
      *
           IF    ( SPA-Q01-DRCD        =   SPACE )
               MOVE    "%"                 TO  SEL-DRCD
           ELSE
               MOVE    SPA-Q01-DRCD        TO  SEL-DRCD
           END-IF
      *
           IF    ( SPA-Q01-SRYKBN       =   SPACE )
               MOVE    "%"                 TO  SEL-SRYKBN
           ELSE
               MOVE    SPA-Q01-SRYKBN(1:1)
                                           TO  SEL-SRYKBN(1:1)
               MOVE    "%"                 TO  SEL-SRYKBN(2:1)
           END-IF
      *
               MOVE    "%"                 TO  SEL-SRYSYUKBN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
      *
               IF    ( SPA-Q01-SRYCD (IDX0)
                               NOT =   SPACE )
      *
                   COMPUTE TSRYCD-IDX  =   TSRYCD-IDX  +   1
                   MOVE    SPA-Q01-SRYVALNUM (IDX0)
                                   TO  TSRYCD-SRYVALNUM (TSRYCD-IDX)
                   MOVE    SPA-Q01-SRYVAL    (IDX0)
                                   TO  TSRYCD-SRYVAL    (TSRYCD-IDX)
                   MOVE    SPA-Q01-SRYVALCOMP(IDX0)
                                   TO  TSRYCD-SRYVALCOMP(TSRYCD-IDX)
                   MOVE    SPA-Q01-SRYCD (IDX0)
                                   TO  TSRYCD-SRYCD     (TSRYCD-IDX)
                   PERFORM 900-TENSU-KEY34-SEL-SEC
                   IF    ( TNS-NAME    NOT =   SPACE )
                       MOVE    TNS-NAME
                                   TO  TSRYCD-NAME      (TSRYCD-IDX)
                   ELSE
                       MOVE    TSRYCD-SRYCD (TSRYCD-IDX)
                                   TO  TSRYCD-NAME      (TSRYCD-IDX)
                   END-IF
                   MOVE    TNS-KANANAME
                                   TO  TSRYCD-KANANAME  (TSRYCD-IDX)
                   MOVE    1       TO  TSRYCD-SURYO-EDIT(TSRYCD-IDX)
      *
                   IF    ( TNS-SRYCD (1:1) =   "6" OR  "7" )
                       INITIALIZE              TNSUP-KEY
                       CALL    "ORCSSETTANI"   USING
                                               TNS-TENSU-REC
                                               SPA-AREA
                   END-IF
      *
                   IF     (TNS-TANICD  =   ZERO
                                       OR  SPACE )
                       IF    ( TNS-SRYCD (1:1)   =   "6" OR  "7" )
                        AND  ( TNS-TEN           >   ZERO )
                        AND  ( TNS-TENSIKIBETU   =   1 OR 2 )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  TSRYCD-SURYO-EDIT
                                                        (TSRYCD-IDX)
                       END-IF
                   END-IF
      *
                   IF    ( TSRYCD-SURYO-EDIT(TSRYCD-IDX)
                                           =   1 )
                       MOVE    TNS-TANICD      TO  TSRYCD-TANICD
                                                        (TSRYCD-IDX)
                       MOVE    TNS-TANINAME    TO  TSRYCD-TANINAME
                                                        (TSRYCD-IDX)
                   END-IF
      *
               END-IF
      *
               IF    ( SEL-SRYCD   =   SPACE )
                   IF    ( SPA-Q01-SRYCD (IDX0)    NOT =   SPACE )
                       MOVE    SPA-Q01-SRYCD (IDX0)    TO  SEL-SRYCD
                   END-IF
                   IF    ( SPA-Q01-XSRYCD (IDX0)   NOT =   SPACE )
                       MOVE    SPA-Q01-XSRYCD(IDX0)    TO  SEL-SRYCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    診察年月日
           IF   ( SPA-Q01-GJYURRK  =   "1" )
               MOVE    1           TO  SEL-RRKSRH
           END-IF
      *
      *    診療行為
           IF   ( SPA-Q01-GSRYACT   =   "1" )
            OR  ( SPA-Q01-GXSRYACT  =   "1" )
               IF    ( SPA-Q01-GJYURRK  =   SPACE )
                   IF    ( SPA-Q01-SRYCD-G  NOT =   SPACE )
                       MOVE    1    TO  SEL-SRYSRH
                   ELSE
                       MOVE    3    TO  SEL-SRYSRH
                   END-IF
               ELSE
                   IF    ( SPA-Q01-SRYCD-G  NOT =   SPACE )
                       MOVE    2    TO  SEL-SRYSRH
                   ELSE
                       MOVE    4    TO  SEL-SRYSRH
                   END-IF
               END-IF
           END-IF
      *
      *    診察年月日
           IF   ( SPA-Q01-GJYURRK  =   "1" )
               MOVE    1           TO  SEL-RRKSRH
           END-IF
      *
           IF    ( SPA-Q01-GSEIKYU =   "1" )
               MOVE    1           TO  SEL-SKYSRH
           END-IF
      *
           IF    ( SPA-Q01-TENKENRECE   =   SPACE )
               MOVE    "%"                 TO  SEL-TENKENRECE
           ELSE
               MOVE    SPA-Q01-TENKENRECE  TO  SEL-TENKENRECE
           END-IF
      *
           .
       1001-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-TERMID      TO  RRKSRH-TERMID
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5)
      *
               MOVE    SPACE       TO  SEL-KOHNUM
      *
               IF  ( SPA-Q01-KOHNUM-JKN    NOT =   CONST-NOT )
                AND( SPA-Q01-KOHNUM (IDX1) NOT =   SPACE )
                   PERFORM VARYING IDXK    FROM    1   BY  1
                           UNTIL ( IDXK    >   CONST-KOHNUM-MAX )
                            OR   ( SEL-KOHNUM  NOT =   SPACE )
                       IF    ( CONST-KOHNUM (IDXK)
                                       =   SPA-Q01-KOHNUM (IDX1) )
                           MOVE    CONST-KOHNUM (IDXK)
                                       TO  SEL-KOHNUM
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF    ( SEL-KOHNUM  NOT =   SPACE )
                   MOVE    SEL-KOHNUM  TO  RRKSRH-KOHNUM (IDX1)
               ELSE
                   MOVE    "@@@"       TO  RRKSRH-KOHNUM (IDX1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "crtv01"        TO  MCP-PATHNAME
           MOVE    "DBCREATE"      TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
      *
           IF   ( SEL-SKYSRH       =   1 )
               PERFORM 900-SKYSRH-DEL1-SEC
               IF   ( SPA-Q01-STTOTALTEN >   ZERO )
                OR  ( SPA-Q01-EDTOTALTEN >   ZERO )
                   PERFORM 2001-SYUNOU-GET-SEC
               END-IF
               PERFORM 900-SKYSRH-INS1-SEC
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           END-IF
      *
           IF   ( SEL-RRKSRH       =   1 )
               PERFORM 900-RRKSRH-INS1-SEC
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           END-IF
      *
           PERFORM 900-PTSRH-DEL6-SEC
      *
           PERFORM 900-SRYSRH-DEL1-SEC
      *
           EVALUATE    SEL-SRYSRH
           WHEN    1
               PERFORM 900-SRYSRH-INS1-SEC
           WHEN    2
               PERFORM 900-SRYSRH-INS2-SEC
           WHEN    3
               PERFORM 900-SRYSRH-INS3-SEC
           WHEN    4
               PERFORM 900-SRYSRH-INS4-SEC
               IF    ( SPA-Q01-XSRYCD-G  NOT =   SPACE )
                   PERFORM 900-SRYSRH-INS2-SEC
               END-IF
           END-EVALUATE
      *
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           IF    ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
               PERFORM 2001-SRYVAL-EDIT-SEC
               PERFORM 2001-SRYVAL-PTSRH-EDIT-SEC
      *
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           ELSE
               PERFORM 900-PTSRH-INS1-SEC
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           END-IF
      *
           PERFORM 900-SKYSRH-DEL1-SEC
           PERFORM 900-RRKSRH-DEL1-SEC
           PERFORM 900-SRYSRH-DEL1-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "0"             TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
           MOVE    SPA-Q01-OUTKBN  TO  PTSRH-OUTKBN
           MOVE    FLG-DBERR       TO  PTSRH-DBERR
           MOVE    BQ01-SQL-ORDER  TO  PTSRH-ORDER
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "upd1"          TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *
          IF    ( WRK-KARTE-UID    NOT =   SPACE )
              PERFORM 2003-STATE-UPD-SEC
              PERFORM 2003-PUSH-SEC
          END-IF
      *
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "drtv01"        TO  MCP-PATHNAME
           MOVE    "DBDROP"        TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索処理
      *****************************************************************
       2001-SRYVAL-EDIT-SEC            SECTION.
      *
           OPEN    OUTPUT  TMP01-FILE
                           TMP02-FILE
                           TMP03-FILE
                           TMP04-FILE
      *
           CLOSE           TMP01-FILE
                           TMP02-FILE
                           TMP03-FILE
                           TMP04-FILE
      *
           OPEN    I-O     TMP01-FILE
                           TMP02-FILE
                           TMP03-FILE
                           TMP04-FILE
      *
           PERFORM 900-SRYSRH-KEY-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYSRH  NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   5 )
                        OR   ( SRYSRH-SRYCD (IDX-SRY)   =   SPACE )
      *
                   PERFORM 20011-SRYVAL-EDIT-SEC
      *
               END-PERFORM
      *
               PERFORM 900-SRYSRH-KEY-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   TMP01-FILE
                   TMP02-FILE
                   TMP03-FILE
                   TMP04-FILE
      *
           .
       2001-SRYVAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索処理
      *****************************************************************
       20011-SRYVAL-EDIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TARGET
      *
           MOVE    ZERO                TO  IDXM
      *
           PERFORM VARYING TSRYCD-IDX  FROM    1   BY  1
                   UNTIL ( TSRYCD-IDX  >   5 )
                    OR   ( TSRYCD-SRYCD (TSRYCD-IDX)
                                       =   SPACE )
      *
               IF    ( SRYSRH-SRYCD (IDX-SRY)
                                       =    TSRYCD-SRYCD (TSRYCD-IDX))
      *
                   MOVE    1           TO   FLG-TARGET
      *
                   IF    ( SPA-Q01-SRYVALITEM
                                       =   CONST-VALSURYO )
      *
                       IF    ( TSRYCD-SRYVAL (TSRYCD-IDX)
                                   NOT =   SPACE )
                           EVALUATE    TSRYCD-SRYVALCOMP (TSRYCD-IDX)
                           WHEN    CONST-GREATER-EQUAL
                               IF    ( SRYSRH-SRYSURYO (IDX-SRY)
                                               >=  TSRYCD-SRYVALNUM
                                                           (TSRYCD-IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-TARGET
                               END-IF
                           WHEN    CONST-LESS-EQUAL
                               IF    ( SRYSRH-SRYSURYO (IDX-SRY)
                                               <=  TSRYCD-SRYVALNUM
                                                           (TSRYCD-IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-TARGET
                               END-IF
                           WHEN    OTHER
                               IF    ( SRYSRH-SRYSURYO (IDX-SRY)
                                                =  TSRYCD-SRYVALNUM
                                                           (TSRYCD-IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-TARGET
                               END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   12 )
                    OR   ( IDXM    >   ZERO  )
      *
               IF    ( SRYSRH-SRYYMD (1:6) =   SEL-SRYYM (IDX2))
                   MOVE    IDX2    TO  IDXM
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXM        =   ZERO )
               MOVE    ZERO    TO  FLG-TARGET
           END-IF
      *
           IF    ( FLG-TARGET  =   1 )
               INITIALIZE              WRK-ACCT-G
               IF    ( SRYSRH-ACCTKBN  =   CONST-SRYACCT )
                   PERFORM 901-TMP02-READ-RND-SEC
                   IF    ( FLG-TMP02   =   ZERO )
                       PERFORM 900-TMP02-READ-RND-SEC
                       IF    ( FLG-TMP02   =   ZERO )
                           CONTINUE
                       ELSE
                           PERFORM 200111-SRYVAL-SUM-SEC
                           COMPUTE WRK-SURYOTOTAL
                               =   SRYSRH-SRYSURYO (IDX-SRY)
                               *   WRK-ZAIKAISU
                           MOVE    ZERO    TO  WRK-ZAIKAISU
                           INITIALIZE          WRK-DAY-G
      *
                           PERFORM 9001-TMP02-KEY-EDIT-SEC
                           WRITE   TMP02-REC
      *
                       END-IF
                   ELSE
      *
                       PERFORM 200111-SRYVAL-SUM-SEC
      *
                       COMPUTE WRK-SURYOTOTAL
                           =   SRYSRH-SRYSURYO (IDX-SRY)
                           *   WRK-ZAIKAISU
      *
                       PERFORM 9001-TMP02-KEY-EDIT-SEC
                       WRITE   TMP02-REC
                       PERFORM 9011-TMP02-KEY-EDIT-SEC
                       WRITE   TMP02-REC
                   END-IF
               ELSE
                   MOVE    SRYSRH-ZAIKAISU
                                       TO  WRK-ZAIKAISU
                   MOVE    SRYSRH-SRYYMD
                                       TO  WRK-SRYYMD
                   MOVE    SRYSRH-ZAIKAISU
                                       TO  WRK-DAY (WRK-SRYDD)
                   COMPUTE WRK-SURYOTOTAL
                       =   SRYSRH-SRYSURYO (IDX-SRY)
                       *   WRK-ZAIKAISU
               END-IF
      *
               PERFORM 900-TMP01-READ-RND-SEC
               IF    ( FLG-TMP01       =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 9001-TMP01-KEY-EDIT-SEC
                   MOVE    SRYSRH-PTNUM    TO  TMP01-PTNUM
                   MOVE    SRYSRH-KANANAME TO  TMP01-KANANAME
                   MOVE    SRYSRH-SEX      TO  TMP01-SEX
                   MOVE    SRYSRH-BIRTHDAY TO  TMP01-BIRTHDAY
               END-IF
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   5 )
                        OR   ( TSRYCD-SRYCD (IDX0)
                                       =   SPACE )
                   IF    ( TSRYCD-SRYCD (IDX0)
                                       =   SRYSRH-SRYCD (IDX-SRY))
                       MOVE    TSRYCD-SRYCD (IDX0)
                                       TO  TMP01-SRYCD (IDX0)
                       COMPUTE TMP01-ZAIKAISU(IDX0)
                           =   TMP01-ZAIKAISU(IDX0)
                           +   WRK-ZAIKAISU
                       COMPUTE TMP01-SURYOTOTAL(IDX0)
                           =   TMP01-SURYOTOTAL(IDX0)
                           +   WRK-SURYOTOTAL
      *
                       IF    ( TMP01-SRYSURYO1 (IDX0)
                                           =   ZERO )
                           MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  TMP01-SRYSURYO1(IDX0)
                                       TMP01-SRYSURYO2(IDX0)
                       ELSE
                           IF    ( TMP01-SRYSURYO2(IDX0)
                                       =   SRYSRH-SRYSURYO (IDX-SRY))
                               CONTINUE
                           ELSE
                               MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  TMP01-SRYSURYO1(IDX0)
                           END-IF
                       END-IF
      *
                       MOVE    TMP01-SRYCD (IDX0)
                                   TO  WRK-TMP-SRYCD
                       MOVE    SEL-SRYYM (IDXM)
                                   TO  WRK-TMP-SRYYM
                       MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  WRK-TMP-SRYSURYO
                       PERFORM 200111-TMP03-EDIT-SEC
                       PERFORM 200111-TMP04-EDIT-SEC
      *
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-TMP01       =   ZERO )
                   REWRITE TMP01-REC
               ELSE
                   WRITE   TMP01-REC
               END-IF
      *
           END-IF
      *
           .
       20011-SRYVAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索処理
      *****************************************************************
       200111-TMP03-EDIT-SEC           SECTION.
      *
           PERFORM 900-TMP03-READ-RND-SEC
      *
           COMPUTE TMP03-ZAIKAISU
               =   TMP03-ZAIKAISU
               +   WRK-ZAIKAISU
           COMPUTE TMP03-SURYOTOTAL
               =   TMP03-SURYOTOTAL
               +   WRK-SURYOTOTAL
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               COMPUTE TMP03-DAY (IDXD)
                   =   TMP03-DAY (IDXD)
                   +   WRK-DAY (IDXD)
           END-PERFORM
      *
           IF    ( FLG-TMP03       =   ZERO )
               REWRITE TMP03-REC
           ELSE
               WRITE   TMP03-REC
           END-IF
      *
           .
       200111-TMP03-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索処理
      *****************************************************************
       200111-TMP04-EDIT-SEC           SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF    ( WRK-DAY (IDXD)      >   ZERO )
                   MOVE    IDXD    TO  WRK-TMP-SRYDD
                   PERFORM 900-TMP04-READ-RND-SEC
      *
                   COMPUTE TMP04-ZAIKAISU
                       =   TMP04-ZAIKAISU
                       +   WRK-DAY (IDXD)
      *
                   IF    ( FLG-TMP04       =   ZERO )
                       REWRITE TMP04-REC
                   ELSE
                       WRITE   TMP04-REC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200111-TMP04-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値集計処理
      *****************************************************************
       200111-SRYVAL-SUM-SEC           SECTION.
      *
           PERFORM 900-SRYACCT-KEY57-SEL-SEC
           IF    ( SPA-Q01-DRCD    NOT =   SPACE )
               MOVE    ACCT-SRYYM      TO  WRK-SRYYMD
               PERFORM VARYING IDXR    FROM    2   BY  1
                       UNTIL ( IDXR    >   CONST-ACCT-OCC-MAX )
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       IF    ( ACCT-DAY (IDXR IDXD)    >   ZERO )
                           PERFORM 2001111-SRYVAL-SUM-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           ELSE
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    1           TO  IDXR
                   PERFORM 2001111-SRYVAL-SUM-SEC
               END-PERFORM
           END-IF
      *
           .
       200111-SRYVAL-SUM-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値集計処理
      *****************************************************************
       2001111-SRYVAL-SUM-SEC          SECTION.
      *
           MOVE    ACCT-SRYYM      TO  WRK-SRYYMD
           MOVE    IDXD            TO  WRK-SRYDD
      *
           MOVE    1               TO  FLG-TARGET-ACCT
      *
           IF    ( WRK-SRYYMD  >=  SEL-STSRYYMD )
            AND  ( WRK-SRYYMD  <=  SEL-EDSRYYMD )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  FLG-TARGET-ACCT
           END-IF
      *
           IF    ( SPA-Q01-DRCD     NOT =   SPACE )
               COMPUTE WRK-RENNUM   =   IDXR   -   1
               PERFORM 900-JYURRK-KEY65-SEL-SEC
               IF    ( SPA-Q01-DRCD NOT =   JYURRK-DRCD )
                   MOVE    ZERO    TO  FLG-TARGET-ACCT
               END-IF
           END-IF
      *
           IF    ( FLG-TARGET-ACCT =   1 )
               COMPUTE WRK-ZAIKAISU
                   =   WRK-ZAIKAISU
                   +   ACCT-DAY (IDXR IDXD)
               MOVE    ACCT-DAY (IDXR IDXD)
                                   TO  WRK-DAY (IDXD)
           END-IF
      *
           .
       2001111-SRYVAL-SUM-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       2001-SRYVAL-PTSRH-EDIT-SEC  SECTION.
      *
           OPEN    INPUT   TMP01-FILE
                           TMP03-FILE
                           TMP04-FILE
      *
           PERFORM 900-TMP01-READ-SEQ-SEC
      *
           PERFORM UNTIL ( FLG-TMP01   NOT =   ZERO )
      *
               MOVE    1       TO  FLG-TARGET
               INITIALIZE          FLG-TARGET-SRYCD-G
      *
               PERFORM VARYING TSRYCD-IDX  FROM    1   BY  1
                       UNTIL ( TSRYCD-IDX  >   5 )
                        OR   ( TSRYCD-SRYCD (TSRYCD-IDX)
                                           =   SPACE )
      *
                   IF    ( TMP01-SRYCD (TSRYCD-IDX)
                                           =   SPACE )
                       MOVE    ZERO    TO  FLG-TARGET
                                           FLG-TARGET-SRYCD (TSRYCD-IDX)
                   ELSE
                       PERFORM 20011-SRYVAL-PTSRH-EDIT-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM VARYING TSRYCD-IDX  FROM    1   BY  1
                       UNTIL ( TSRYCD-IDX  >   5 )
                        OR   ( TSRYCD-SRYCD (TSRYCD-IDX)
                                           =   SPACE )
      *
                   IF    ( SPA-Q01-SRYACT-JKN  =   CONST-AND )
                       IF    ( FLG-TARGET  =   1 )
                           PERFORM 20011-SRYVAL-PTSRH-INS-SEC
                       END-IF
                   ELSE
                       IF    ( FLG-TARGET-SRYCD (TSRYCD-IDX)
                                           =   1 )
                           PERFORM 20011-SRYVAL-PTSRH-INS-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-TMP01-READ-SEQ-SEC
      *
           END-PERFORM
      *
           CLOSE           TMP01-FILE
                           TMP03-FILE
                           TMP04-FILE
      *
           .
       2001-SRYVAL-PTSRH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       20011-SRYVAL-PTSRH-EDIT-SEC SECTION.
      *
           MOVE    1   TO  FLG-TARGET-SRYCD (TSRYCD-IDX)
      *
           IF    ( TSRYCD-SRYVAL (TSRYCD-IDX)  NOT =   SPACE )
               IF    ( SPA-Q01-SRYVALITEM  =   CONST-VALKAISU
                                            OR CONST-VALTOTAL )
                   IF    ( SPA-Q01-SRYVALITEM  =   CONST-VALKAISU )
                       MOVE    TMP01-ZAIKAISU (TSRYCD-IDX)
                                           TO  WRK-SRYVAL
                   ELSE
                       MOVE    TMP01-SURYOTOTAL (TSRYCD-IDX)
                                           TO  WRK-SRYVAL
                   END-IF
      *
                   EVALUATE   TSRYCD-SRYVALCOMP (TSRYCD-IDX)
                   WHEN    CONST-GREATER-EQUAL
                       IF    ( WRK-SRYVAL
                                  >=  TSRYCD-SRYVALNUM
                                                   (TSRYCD-IDX))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TARGET-SRYCD
                                                   (TSRYCD-IDX)
                           MOVE    ZERO    TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-LESS-EQUAL
                       IF    ( WRK-SRYVAL
                                  <=  TSRYCD-SRYVALNUM
                                                   (TSRYCD-IDX))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TARGET-SRYCD
                                                   (TSRYCD-IDX)
                           MOVE    ZERO    TO  FLG-TARGET
                       END-IF
                   WHEN    OTHER
                       IF    ( WRK-SRYVAL
                                   =  TSRYCD-SRYVALNUM
                                                   (TSRYCD-IDX))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TARGET-SRYCD
                                                   (TSRYCD-IDX)
                           MOVE    ZERO    TO  FLG-TARGET
                       END-IF
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       20011-SRYVAL-PTSRH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       20011-SRYVAL-PTSRH-INS-SEC  SECTION.
      *
          IF    ( WRK-KARTE-UID    NOT =   SPACE )
              PERFORM 200111-SRYVAL-PTSRH-INS-SEC
              PERFORM 200112-SRYVAL-PTSRH-INS-SEC
          ELSE
              PERFORM 200111-SRYVAL-PTSRH-INS-SEC
          END-IF
      *
           .
       20011-SRYVAL-PTSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理（画面用）
      *****************************************************************
       200111-SRYVAL-PTSRH-INS-SEC  SECTION.
      *
           MOVE    ZERO            TO  CNT-TMP03
      *
           PERFORM 900-TMP03-START-SEC
      *
           PERFORM UNTIL ( FLG-TMP03   NOT =   ZERO )
      *
               COMPUTE CNT-TMP03   =   CNT-TMP03   +   1
      *
               PERFORM 2001111-SRYVAL-PTSRH-INS-SEC
      *
               PERFORM 900-TMP03-READ-SEQ-SEC
      *
           END-PERFORM
      *
           IF    ( CNT-TMP03       >   1 )
      *
               INITIALIZE                  PTSRH-REC
                                           PTSRH-MEMO-R1
               MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
               MOVE    SPA-TERMID      TO  PTSRH-TERMID
               MOVE    TMP01-PTID      TO  PTSRH-PTID
               MOVE    TMP01-PTNUM     TO  PTSRH-PTNUM
               MOVE    TMP01-KANANAME  TO  PTSRH-KANANAME
               MOVE    TMP01-SEX       TO  PTSRH-SEX
               MOVE    TMP01-BIRTHDAY  TO  PTSRH-BIRTHDAY
               MOVE    TMP01-SRYCD    (TSRYCD-IDX)
                                        TO  PTSRH-SRYCD
      *
               MOVE    TSRYCD-NAME (TSRYCD-IDX)
                                       TO  PTSRH-SRYNAME
               MOVE    0               TO  PTSRH-SURYO-EDIT
               MOVE    TSRYCD-KANANAME (TSRYCD-IDX)
                                       TO  PTSRH-SRYKANANAME
               MOVE    TSRYCD-IDX      TO  PTSRH-SRYSEQ
      *
               MOVE    "999999"        TO  PTSRH-SRYYM
               MOVE    TMP01-ZAIKAISU (TSRYCD-IDX)
                                       TO  PTSRH-ZAIKAISU
               MOVE    TMP01-SURYOTOTAL (TSRYCD-IDX)
                                       TO  PTSRH-SURYOTOTAL
      *
               IF    ( WRK-KARTE-UID    NOT =   SPACE )
                   MOVE    "3"         TO  PTSRH-DATAKBN
               ELSE
                   MOVE    "1"         TO  PTSRH-DATAKBN
               END-IF
               MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                           PTSRH-UPYMD
               MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
      *
               MOVE    PTSRH-REC       TO  MCPDATA-REC
               MOVE    "tbl_ptsrh"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           .
       20011-SRYVAL-PTSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       2001111-SRYVAL-PTSRH-INS-SEC    SECTION.
      *
           INITIALIZE                  PTSRH-REC
                                       PTSRH-MEMO-R1
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    TMP01-PTID      TO  PTSRH-PTID
           MOVE    TMP01-PTNUM     TO  PTSRH-PTNUM
           MOVE    TMP01-KANANAME  TO  PTSRH-KANANAME
           MOVE    TMP01-SEX       TO  PTSRH-SEX
           MOVE    TMP01-BIRTHDAY  TO  PTSRH-BIRTHDAY
           MOVE    TMP01-SRYCD    (TSRYCD-IDX)
                                   TO  PTSRH-SRYCD
      *
           MOVE    TSRYCD-NAME (TSRYCD-IDX)
                                   TO  PTSRH-SRYNAME
           MOVE    TSRYCD-SURYO-EDIT (TSRYCD-IDX)
                                   TO  PTSRH-SURYO-EDIT
           MOVE    TSRYCD-KANANAME (TSRYCD-IDX)
                                   TO  PTSRH-SRYKANANAME
           MOVE    TSRYCD-IDX      TO  PTSRH-SRYSEQ
      *
           MOVE    TMP03-SRYYM     TO  PTSRH-SRYYM
      *
           MOVE    TMP03-SRYSURYO
                                   TO  PTSRH-SRYSURYO
           MOVE    TMP03-ZAIKAISU
                                   TO  PTSRH-ZAIKAISU
           MOVE    TMP03-SURYOTOTAL
                                   TO  PTSRH-SURYOTOTAL
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    TMP03-DAY (IDXD)
                                   TO  PTSRH-DAY (IDXD)
               IF    ( TMP03-DAY (IDXD)    >   ZERO )
                 AND ( PTSRH-FIRSTDAY      =   ZERO )
                   MOVE    IDXD    TO  PTSRH-FIRSTDAY
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-KARTE-UID    NOT =   SPACE )
               MOVE    "3"         TO  PTSRH-DATAKBN
           ELSE
               MOVE    "1"         TO  PTSRH-DATAKBN
           END-IF
      *
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       2001111-SRYVAL-PTSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理（API用）
      *****************************************************************
       200112-SRYVAL-PTSRH-INS-SEC  SECTION.
      *
           MOVE    ZERO                TO  CNT-TMP04
      *
           INITIALIZE                  PTSRH-REC
                                       PTSRH-MEMO-R4
      *
           PERFORM 900-TMP04-START-SEC
      *
           PERFORM UNTIL ( FLG-TMP04   NOT =   ZERO )
      *
               COMPUTE CNT-TMP04   =   CNT-TMP04   +   1
      *
               IF    ( CNT-TMP04   <=  1000 )
                   MOVE    TMP04-SRYYMD
                                   TO  PTSRH-R4-SRYYMD (CNT-TMP04)
                   MOVE    TMP04-SRYSURYO
                                   TO  WRK-SRYSURYOZ
                   PERFORM 800-SRYSURYO-EDIT-SEC
                   MOVE    WRK-SRYSURYOX
                                   TO  PTSRH-R4-SRYSURYO (CNT-TMP04)
                   MOVE    TMP04-ZAIKAISU
                                   TO  WRK-NUM03Z
                   MOVE    WRK-NUM03Z
                                   TO  PTSRH-R4-ZAIKAISU (CNT-TMP04)
               END-IF
      *
               PERFORM 900-TMP04-READ-SEQ-SEC
      *
           END-PERFORM
      *
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    TMP01-PTID      TO  PTSRH-PTID
           MOVE    TMP01-PTNUM     TO  PTSRH-PTNUM
           MOVE    TMP01-KANANAME  TO  PTSRH-KANANAME
           MOVE    TMP01-SEX       TO  PTSRH-SEX
           MOVE    TMP01-BIRTHDAY  TO  PTSRH-BIRTHDAY
           MOVE    TMP01-SRYCD    (TSRYCD-IDX)
                                   TO  PTSRH-SRYCD
      *
           MOVE    TSRYCD-NAME (TSRYCD-IDX)
                                   TO  PTSRH-SRYNAME
           MOVE    TSRYCD-KANANAME (TSRYCD-IDX)
                                   TO  PTSRH-SRYKANANAME
           MOVE    TSRYCD-IDX      TO  PTSRH-SRYSEQ
      *
           MOVE    TSRYCD-SURYO-EDIT (TSRYCD-IDX)
                                   TO  PTSRH-R4-SURYO-EDIT
           MOVE    TSRYCD-TANICD (TSRYCD-IDX)
                                   TO  PTSRH-R4-TANICD
           MOVE    TSRYCD-TANINAME (TSRYCD-IDX)
                                   TO  PTSRH-R4-TANINAME
           MOVE    TMP01-ZAIKAISU (TSRYCD-IDX)
                                   TO  WRK-NUM07Z
           MOVE    WRK-NUM07Z      TO  PTSRH-R4-KAISUTOTAL
           MOVE    TMP01-SURYOTOTAL (TSRYCD-IDX)
                                   TO  WRK-SRYSURYOZ
           PERFORM 800-SRYSURYO-EDIT-SEC
           MOVE    WRK-SRYSURYOX   TO  PTSRH-R4-SURYOTOTAL
      *
           MOVE    "1"             TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       200112-SRYVAL-PTSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    収納データ取得　処理
      *****************************************************************
       2001-SYUNOU-GET-SEC         SECTION.
      *
           MOVE    ZERO            TO  IDXN
      *
           PERFORM 900-SYUNOU-KEY77-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               COMPUTE IDXN = IDXN + 1
      *
               IF    ( SYU-SYUHKNNUM   NOT =   SPACE )
                   PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               ELSE
                   INITIALIZE              HKNCOMBI-REC
               END-IF
      *
               INITIALIZE                  SKYSRH-REC
               MOVE    SYU-HOSPNUM     TO  SKYSRH-HOSPNUM
               MOVE    SPA-TERMID      TO  SKYSRH-TERMID
               MOVE    SYU-PTID        TO  SKYSRH-PTID
               MOVE    SYU-NYUGAIKBN   TO  SKYSRH-NYUGAIKBN
               MOVE    SEL-STSRYYMD(1:6)
                                       TO  SKYSRH-SRYYM
               MOVE    COMB-HKNID      TO  SKYSRH-HKNID
               MOVE    SYU-SYUHKNNUM   TO  SKYSRH-HKNNUM
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   4 )
                   MOVE    COMB-KOHID (IDX0)
                                       TO  SKYSRH-KOHID (IDX0)
                   MOVE    SYU-KOH-HKNNUM (IDX0)
                                       TO  SKYSRH-KOHNUM (IDX0)
               END-PERFORM
               MOVE    SYU-TOTAL-HKNTEN
                                       TO  SKYSRH-TOTALTEN
               MOVE    "2"             TO  SKYSRH-DATAKBN
               MOVE    IDXN            TO  SKYSRH-SEQNUM
               MOVE    SEL-UPYMD       TO  SKYSRH-CREYMD
                                           SKYSRH-UPYMD
               MOVE    SEL-UPHMS       TO  SKYSRH-UPHMS
               MOVE    SKYSRH-REC      TO  MCPDATA-REC
               MOVE    "tbl_skysrh"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
               PERFORM 900-SYUNOU-KEY77-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key77"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SYUNOU-GET-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       2003-STATE-UPD-SEC          SECTION.
      *
           PERFORM 20031-STATE-UPD-SEC
      *
           .
       2003-STATE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       20031-STATE-UPD-SEC          SECTION.
      *
           INITIALIZE                  PARA013-REC
           MOVE    "02"            TO  PARA013-STATE
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api013"        TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID      TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    1               TO  APIPARA-EDANUM
           MOVE    WRK-KARTE-UID   TO  APIPARA-KARTE-UID
           MOVE    PARA013-REC     TO  APIPARA-DATA-REC
           MOVE    SEL-UPYMD       TO  APIPARA-UPYMD
           MOVE    SEL-UPHMS       TO  APIPARA-UPHMS
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *
           .
       20031-STATE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知処理
      *****************************************************************
       2003-PUSH-SEC               SECTION.
      *
           PERFORM 900-PTSRH-KEY2-SEL-SEC
           IF    ( PTSRH-OUTKBN       =   "3" )
               MOVE    PTSRH-CNT-SRYCD
                                       TO  WRK-COUNT
           ELSE
               MOVE    PTSRH-CNT-ALL   TO  WRK-COUNT
           END-IF
      *
           OPEN    OUTPUT  PUSHFIND-FILE
      *
           INITIALIZE                  PUSHFIND-REC
           MOVE    "find"          TO  PUSHFIND-EVENT
           IF    ( WRK-COUNT       =   ZERO )
               MOVE    "01"        TO  PUSHFIND-RESPONSE-NUMBER
           ELSE
               MOVE    "02"        TO  PUSHFIND-RESPONSE-NUMBER
           END-IF
           MOVE    WRK-KARTE-UID   TO  PUSHFIND-KERTE-UID
           MOVE    SPA-TERMID      TO  PUSHFIND-ORCA-UID
           MOVE    WRK-COUNT       TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  PUSHFIND-COUNT
           WRITE   PUSHFIND-REC
      *
           CLOSE   PUSHFIND-FILE
      *
      *    MOVE    "push_find"     TO  MCP-TABLE
      *    MOVE    LOW-VALUE       TO  MCP-PATHNAME
      *    MOVE    "PUSHEVENT"     TO  MCP-FUNC
      *    CALL    "MONFUNC"           USING
      *                                MCPAREA
      *                                PUSHFIND-REC
      *
           DISPLAY "RESULTS PUSHED!!"
      *
           .
       2003-PUSH-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP01PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP02PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP03PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP04PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       800-SRYSURYO-EDIT-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-SRYSURYOX
      *
           PERFORM VARYING IDXA    FROM    13  BY  -1
                   UNTIL ( IDXA    <   1 )
                    OR   ( WRK-SRYSURYOZ-G(IDXA:1)
                               NOT =   "0" )
                   MOVE    SPACE   TO  WRK-SRYSURYOZ-G(IDXA:1)
           END-PERFORM
           IF    ( IDXA    >   0 )
               IF    ( WRK-SRYSURYOZ-G (IDXA:1)
                                    =   "." )
                   MOVE    SPACE   TO  WRK-SRYSURYOZ-G(IDXA:1)
               END-IF
           END-IF
      *
           MOVE    FUNCTION TRIM(WRK-SRYSURYOZ-G)
                                   TO  WRK-SRYSURYOX
      *
           .
       800-SRYSURYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY77-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    SEL-NYUGAIKBN   TO  SYU-NYUGAIKBN
      *
           IF    ( SEL-NYUGAIKBN   =   "1" )
               MOVE    "3"         TO  SYU-CREATEKBN
           ELSE
               MOVE    "%"         TO  SYU-CREATEKBN
           END-IF
      *
           MOVE    SEL-SRYKA       TO  SYU-SRYKA
           MOVE    SEL-STSRYYMD(1:6)
                                   TO  SYU-SRYYMD
           MOVE    "%"             TO  SYU-SRYYMD(7:1)
           MOVE    "9%"            TO  SYU-SYUHKNNUM
           MOVE    SPA-Q01-STTOTALTEN
                                   TO  SYU-TOTAL-HKNTEN
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key77"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SYUNOU-KEY77-FET-SEC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY77-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納FETCH処理
      *****************************************************************
       900-SYUNOU-KEY77-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key77"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( SYU-NYUGAIKBN   =   "1" )
                       IF    ( SYU-CREATEKBN   =   "3" )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   EVALUATE    SYU-SYUHKNNUM
                   WHEN    "900"   THRU "919"
                   WHEN    "971"
                   WHEN    "973"
                   WHEN    "980"   THRU "989"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-EVALUATE
      *
                   IF   ( SPA-Q01-STTOTALTEN   NOT =   ZERO )
                    AND ( SPA-Q01-STTOTALTEN   >   SYU-TOTAL-HKNTEN )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
                   IF   ( SPA-Q01-EDTOTALTEN   NOT =   ZERO )
                    AND ( SPA-Q01-EDTOTALTEN   <   SYU-TOTAL-HKNTEN )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1       TO  FLG-SYUNOU
                   INITIALIZE          SYUNOU-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-SYUNOU-KEY77-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNCOMBI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    SYU-PTID        TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNCOMBI-REC
           ELSE
               MOVE    1           TO  FLG-HKNCOMBI
               INITIALIZE              HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY57-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRYSRH-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    SRYSRH-PTID     TO  ACCT-PTID
           MOVE    SRYSRH-ZAINUM   TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
               INITIALIZE              SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会追加処理
      *****************************************************************
       900-SKYSRH-INS1-SEC             SECTION.
      *
           MOVE    ZERO            TO  IDXN
      *
           INITIALIZE                  SKYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SKYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SKYSRH-TERMID
           MOVE    SEL-NYUGAIKBN   TO  SKYSRH-NYUGAIKBN
           MOVE    SEL-STSRYYMD(1:6)
                                   TO  SKYSRH-STSRYYM
           MOVE    SEL-EDSRYYMD(1:6)
                                   TO  SKYSRH-EDSRYYM
           MOVE    SPA-Q01-STTOTALTEN
                                   TO  SKYSRH-TOTALTEN
           IF    ( SPA-Q01-EDTOTALTEN
                                   >   ZERO )
               MOVE    SPA-Q01-EDTOTALTEN
                                   TO  SKYSRH-TOTALTEN-MAX
           ELSE
               MOVE    99999999    TO  SKYSRH-TOTALTEN-MAX
           END-IF
           IF    ( SPA-Q01-TOKKICD NOT =   SPACE )
               MOVE    SPA-Q01-TOKKICD
                                   TO  SKYSRH-TOKKI (1)
               MOVE    SPACE       TO  SKYSRH-TOKKI (2)
           ELSE
               MOVE    SPACE       TO  SKYSRH-TOKKI (1)
               MOVE    SPACE       TO  SKYSRH-TOKKI (2)
           END-IF
      *
           IF    ( SPA-Q01-TOKKINAME   NOT =   SPACE )
               MOVE    SPA-Q01-TOKKINAME
                                   TO  SKYSRH-TOKKINM (1)
               MOVE    SPACE       TO  SKYSRH-TOKKINM (2)
           ELSE
               MOVE    SPACE       TO  SKYSRH-TOKKINM (1)
               MOVE    SPACE       TO  SKYSRH-TOKKINM (2)
           END-IF
      *
           MOVE    SEL-TENKENRECE  TO  SKYSRH-SKYKBN-RECEDEN
      *
           IF    ( SPA-Q01-SAIGAIKBN   NOT =   SPACE )
               MOVE    1           TO  SKYSRH-ETCCREATEFLG
           END-IF
      *
           MOVE    SEL-UPYMD       TO  SKYSRH-CREYMD
                                       SKYSRH-UPYMD
           MOVE    SEL-UPHMS       TO  SKYSRH-UPHMS
           MOVE    SKYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_skysrh"    TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    "1"             TO  SEL-DATAKBN
           PERFORM 900-SKYSRH-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SKYSRH  NOT =   ZERO )
      *
               COMPUTE IDXN = IDXN +  1
      *
               PERFORM 900-SEIKYU-KEY-SEL-SEC
      *
               MOVE    RECE10-HKNID
                                   TO  SKYSRH-HKNID
               MOVE    RECE10-HKNNUM
                                   TO  SKYSRH-HKNNUM
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   4 )
                   MOVE    RECE10-KOHNUM (IDX0)
                                   TO  SKYSRH-KOHNUM (IDX0)
                   MOVE    RECE10-KOHID  (IDX0)
                                   TO  SKYSRH-KOHID (IDX0)
               END-PERFORM
      *
               MOVE    RECE10-SKYIRYO-TOTALTEN
                                   TO  SKYSRH-TOTALTEN
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   5 )
                   MOVE    RECE10-TOKKI (IDX0)
                                   TO  SKYSRH-TOKKI (IDX0)
                   MOVE    RECE10-TOKKINM (IDX0)
                                   TO  SKYSRH-TOKKINM (IDX0)
               END-PERFORM
      *
               MOVE    RECE10-ETC-YKZFTN (1)
                                   TO  SKYSRH-ETC-YKZFTN1
               MOVE    RECE10-SKYKBN-RECEDEN
                                   TO  SKYSRH-SKYKBN-RECEDEN
      *
               MOVE    IDXN        TO  SKYSRH-SEQNUM
      *
               MOVE    SKYSRH-REC  TO  MCPDATA-REC
               MOVE    "tbl_skysrh"
                                   TO  MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
      *
               PERFORM 900-SKYSRH-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_skysrh"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SKYSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会検索処理
      *****************************************************************
       900-SKYSRH-KEY2-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYSRH
      *
           INITIALIZE                      SKYSRH-REC
      *
           MOVE    SPA-HOSPNUM         TO  SKYSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  SKYSRH-TERMID
           MOVE    SEL-DATAKBN         TO  SKYSRH-DATAKBN
           MOVE    SKYSRH-REC          TO  MCPDATA-REC
           MOVE    "tbl_skysrh"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SKYSRH-REC
           ELSE
               MOVE    1           TO  FLG-SKYSRH
               INITIALIZE              SKYSRH-REC
           END-IF
      *
           .
       900-SKYSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会FETCH処理
      *****************************************************************
       900-SKYSRH-KEY2-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYSRH
      *
           MOVE    "tbl_skysrh"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SKYSRH-REC
           ELSE
               MOVE    1           TO  FLG-SKYSRH
               INITIALIZE              SKYSRH-REC
           END-IF
      *
           .
       900-SKYSRH-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    請求管理検索処理
      *****************************************************************
       900-SEIKYU-KEY-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SEIKYU
      *
           INITIALIZE                      RECE10-REC
      *
           MOVE    SKYSRH-HOSPNUM      TO  RECE10-HOSPNUM
           MOVE    SKYSRH-SRYYM        TO  RECE10-SRYYM
           MOVE    SKYSRH-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    SKYSRH-PTID         TO  RECE10-PTID
           MOVE    SKYSRH-RECEKA       TO  RECE10-RECEKA
           MOVE    SKYSRH-TEISYUTUSAKI TO  RECE10-TEISYUTUSAKI
           MOVE    SKYSRH-RECESYUBETU  TO  RECE10-RECESYUBETU
           MOVE    SKYSRH-HKNJANUM     TO  RECE10-HKNJANUM
           MOVE    SKYSRH-HOJOKBN-KEY  TO  RECE10-HOJOKBN-KEY
           MOVE    SKYSRH-KOHID-KEY    TO  RECE10-KOHID-KEY
           MOVE    SKYSRH-TEKSTYMD     TO  RECE10-TEKSTYMD
           MOVE    SKYSRH-RECEIPT-KBN  TO  RECE10-RECEIPT-KBN
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key "              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RECE10-REC
           ELSE
               MOVE    1           TO  FLG-SEIKYU
               INITIALIZE              RECE10-REC
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key "              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SEIKYU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会削除処理
      *****************************************************************
       900-SKYSRH-DEL1-SEC             SECTION.
      *
           INITIALIZE                  SKYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SKYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SKYSRH-TERMID
           MOVE    SKYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_skysrh"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-SKYSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴照会追加処理
      *****************************************************************
       900-RRKSRH-INS1-SEC             SECTION.
      *
           DISPLAY "RRKSRH-INS1"
      *
           PERFORM 900-RRKSRH-DEL1-SEC
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  RRKSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  RRKSRH-TERMID
           MOVE    SEL-NYUGAIKBN   TO  RRKSRH-NYUGAIKBN
           MOVE    SEL-SRYKA       TO  RRKSRH-SRYKA
           MOVE    SEL-DRCD        TO  RRKSRH-DRCD
           MOVE    SEL-STSRYYMD    TO  RRKSRH-STSRYYMD
           MOVE    SEL-EDSRYYMD    TO  RRKSRH-EDSRYYMD
           MOVE    SEL-UPYMD       TO  RRKSRH-CREYMD
                                       RRKSRH-UPYMD
           MOVE    SEL-UPHMS    TO  RRKSRH-UPHMS
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "ins1_1"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "ins1_2"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-RRKSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴照会削除処理
      *****************************************************************
       900-RRKSRH-DEL1-SEC             SECTION.
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  RRKSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  RRKSRH-TERMID
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-RRKSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS1-SEC             SECTION.
      *
           DISPLAY "SRYSRH-INS1"
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins1_1"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins1_2"        TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins1_3"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS2-SEC             SECTION.
      *
           DISPLAY "SRYSRH-INS2"
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins2_1"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins2_2"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS2-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS3-SEC             SECTION.
      *
           DISPLAY "SRYSRH-INS3"
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins3_1"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins3_2"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS3-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS4-SEC             SECTION.
      *
           DISPLAY "SRYSRH-INS4"
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins4_1"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins4_2"        TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS4-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       9001-SRYSRH-INS-SEC             SECTION.
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SRYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SRYSRH-TERMID
           MOVE    SEL-NYUGAIKBN   TO  SRYSRH-NYUGAIKBN
           MOVE    SEL-SRYKA       TO  SRYSRH-SRYKA
           MOVE    SEL-DRCD        TO  SRYSRH-DRCD
           MOVE    SEL-SRYKBN      TO  SRYSRH-SRYKBN
           MOVE    SEL-SRYSYUKBN   TO  SRYSRH-SRYSYUKBN
           MOVE    SEL-STSRYYMD    TO  SRYSRH-STSRYYMD
           MOVE    SEL-EDSRYYMD    TO  SRYSRH-EDSRYYMD
           MOVE    SEL-STSRYYMD(1:6)
                                   TO  SRYSRH-STSRYYM
           MOVE    SEL-EDSRYYMD(1:6)
                                   TO  SRYSRH-EDSRYYM
           MOVE    ZERO            TO  IDX1
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
               MOVE    SEL-SRYCD   TO  SRYSRH-SRYCD (IDX0)
           END-PERFORM
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
                    OR   ( IDX1    >=  5 )
               IF    ( SPA-Q01-SRYCD (IDX0)
                               NOT =   SPACE )
                   COMPUTE IDX1 =  IDX1    +   1
                   MOVE    SPA-Q01-SRYCD (IDX0)
                                   TO  SRYSRH-SRYCD (IDX1)
               END-IF
           END-PERFORM
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
                    OR   ( IDX1    >=  5 )
               IF    ( SPA-Q01-XSRYCD (IDX0)
                               NOT =   SPACE )
                   COMPUTE IDX1 =  IDX1    +   1
                   MOVE    SPA-Q01-XSRYCD (IDX0)
                                   TO  SRYSRH-SRYCD (IDX1)
               END-IF
           END-PERFORM
           MOVE    SEL-UPYMD    TO  SRYSRH-CREYMD
                                       SRYSRH-UPYMD
           MOVE    SEL-UPHMS    TO  SRYSRH-UPHMS
      *
      *
           .
       9001-SRYSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者照会追加処理
      *****************************************************************
       900-PTSRH-INS1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    WKBQ01-SQL      TO  PTSRH-SQL
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           IF    ( SPA-Q01-OUTKBN  =   "2" )
      *
               MOVE    "0"         TO  SEL-DATAKBN
               PERFORM 900-SKYSRH-KEY2-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SKYSRH  NOT =   ZERO )
      *
                   INITIALIZE                  PTSRH-REC
                   MOVE    SKYSRH-HOSPNUM  TO  PTSRH-HOSPNUM
                   MOVE    SKYSRH-TERMID   TO  PTSRH-TERMID
                   MOVE    SKYSRH-PTID	   TO  PTSRH-PTID
                   MOVE    "1"             TO  PTSRH-DATAKBN
                   MOVE    SKYSRH-SEQNUM   TO  PTSRH-SEQNUM
                   MOVE    SKYSRH-UPYMD    TO  PTSRH-UPYMD
                   MOVE    SKYSRH-UPHMS    TO  PTSRH-UPHMS
      *
                   MOVE    SKYSRH-HKNID    TO  PTSRH-HKNID
                   PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   4 )
                       MOVE    SKYSRH-KOHNUM (IDX0)
                                           TO  PTSRH-KOHNUM (IDX0)
                       MOVE    SKYSRH-KOHID  (IDX0)
                                           TO  PTSRH-KOHID (IDX0)
                   END-PERFORM
      *
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   5 )
                       MOVE    SKYSRH-TOKKI (IDX0)
                                           TO  PTSRH-TOKKI (IDX0)
                       MOVE    SKYSRH-TOKKINM (IDX0)
                                           TO  PTSRH-TOKKINM (IDX0)
                   END-PERFORM
      *
      *
                   MOVE    PTSRH-REC   TO  MCPDATA-REC
                   MOVE    "tbl_ptsrh"
                                       TO  MCP-TABLE
                   MOVE    "upd2"      TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *
                   PERFORM 900-SKYSRH-KEY2-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_skysrh"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-PTSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL6-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del6"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL6-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL4-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    WRK-DELYMDHMS   TO  PTSRH-VSTR (1)
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL4-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY2-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "0"                 TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD           TO  PTSRH-UPYMD
           MOVE    SEL-UPHMS           TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF  (   MCP-RC              =   ZERO    )
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               INITIALIZE                  PTSRH-REC
               MOVE    1               TO  FLG-PTSRH
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会削除処理
      *****************************************************************
       900-SRYSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SRYSRH-HOSPNUM
           MOVE    WRK-DELYMDHMS   TO  SRYSRH-YMDHMS
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-SRYSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴照会削除処理
      *****************************************************************
       900-RRKSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  RRKSRH-HOSPNUM
           MOVE    WRK-DELYMDHMS   TO  RRKSRH-YMDHMS
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-RRKSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会削除処理
      *****************************************************************
       900-SKYSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  SKYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SKYSRH-HOSPNUM
           MOVE    WRK-DELYMDHMS   TO  SKYSRH-YMDHMS
           MOVE    SKYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_skysrh"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-SKYSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩパラメタ削除処理
      *****************************************************************
       900-APIPARA-DEL5-SEC              SECTION.
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    WRK-GYOUMUID    TO  APIPARA-GYOUMUID
           MOVE    WRK-DELYMDHMS   TO  APIPARA-DATA-REC
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del5"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-APIPARA-DEL5-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会削除処理
      *****************************************************************
       900-SRYSRH-DEL1-SEC             SECTION.
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SRYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SRYSRH-TERMID
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-SRYSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会テーブル検索処理
      *****************************************************************
       900-SRYSRH-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYSRH
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    WKBQ01-SQL      TO  SRYSRH-SQL
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYSRH-REC
           ELSE
               INITIALIZE              SRYSRH-REC
               MOVE    1           TO  FLG-SRYSRH
           END-IF
      *
           .
       900-SRYSRH-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会テーブル検索処理
      *****************************************************************
       900-SRYSRH-KEY-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYSRH
      *
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYSRH-REC
           ELSE
               INITIALIZE              SRYSRH-REC
               MOVE    1           TO  FLG-SRYSRH
           END-IF
      *
           .
       900-SRYSRH-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY34-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO FLG-TENSU
      *
           INITIALIZE                 TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO TNS-HOSPNUM
           MOVE    TSRYCD-SRYCD (TSRYCD-IDX)
                                   TO TNS-SRYCD
           MOVE    SEL-STSRYYMD    TO TNS-YUKOEDYMD
           MOVE    SEL-EDSRYYMD    TO TNS-YUKOSTYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key34"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key34"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY34-SEL-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴検索処理
      *****************************************************************
       900-JYURRK-KEY65-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-JYURRK
      *
           INITIALIZE                  JYURRK-REC
           MOVE    ACCT-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID       TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN  TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA      TO  JYURRK-SRYKA
           MOVE    WRK-SRYYMD      TO  JYURRK-SRYYMD
           MOVE    WRK-RENNUM      TO  JYURRK-RENNUM
           MOVE    1               TO  JYURRK-EDANUM
           MOVE    ACCT-HKNCOMBI   TO  JYURRK-HKNCOMBINUM
           MOVE    JYURRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key65"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  JYURRK-REC
           ELSE
               INITIALIZE              JYURRK-REC
               MOVE    1           TO  FLG-JYURRK
           END-IF
      *
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key65"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-JYURRK-KEY65-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索チェックファイル読込処理
      *****************************************************************
       900-TMP01-READ-RND-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP01
           PERFORM 9001-TMP01-KEY-EDIT-SEC
      *
           READ    TMP01-FILE
           INVALID
               MOVE    1           TO  FLG-TMP01
           END-READ
      *
           .
       900-TMP01-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索チェックファイルキー項目編集処理
      *****************************************************************
       9001-TMP01-KEY-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TMP01-REC
           MOVE    SRYSRH-HOSPNUM  TO  TMP01-HOSPNUM
           MOVE    SRYSRH-PTID     TO  TMP01-PTID
      *
           .
       9001-TMP01-COMMON-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索チェックファイル読込処理
      *****************************************************************
       900-TMP01-READ-SEQ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP01
      *
           READ    TMP01-FILE      NEXT
           AT  END
               MOVE    1           TO  FLG-TMP01
           END-READ
      *
           .
       900-TMP01-READ-SEQ-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイル読込処理
      *    同じ剤の中に同じ診療コードが無いかチェック
      *    ある場合は回数はカウントせず、合計のみ集計
      *****************************************************************
       900-TMP02-READ-RND-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP02
           PERFORM 9001-TMP02-KEY-EDIT-SEC
      *
           READ    TMP02-FILE
           INVALID
               MOVE    1           TO  FLG-TMP02
           END-READ
      *
           .
       900-TMP02-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイルキー項目編集処理
      *****************************************************************
       9001-TMP02-KEY-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TMP02-REC
           MOVE    SRYSRH-HOSPNUM  TO  TMP02-HOSPNUM
           MOVE    SRYSRH-PTID     TO  TMP02-PTID
           MOVE    SRYSRH-ZAINUM   TO  TMP02-ZAINUM
           MOVE    SRYSRH-SRYCD(IDX-SRY)
                                   TO  TMP02-SRYCD
           MOVE    SRYSRH-RENNUM   TO  TMP02-RENNUM
           MOVE    IDX-SRY         TO  TMP02-SRYCD-IDX
      *
           .
       9001-TMP02-COMMON-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイル読込処理
      *****************************************************************
       901-TMP02-READ-RND-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP02
           PERFORM 9011-TMP02-KEY-EDIT-SEC
      *
           READ    TMP02-FILE
           INVALID
               MOVE    1           TO  FLG-TMP02
           END-READ
      *
           .
       901-TMP02-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイルキー項目編集処理
      *****************************************************************
       9011-TMP02-KEY-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TMP02-REC
           MOVE    SRYSRH-HOSPNUM  TO  TMP02-HOSPNUM
           MOVE    SRYSRH-PTID     TO  TMP02-PTID
           MOVE    SRYSRH-ZAINUM   TO  TMP02-ZAINUM
           MOVE    SRYSRH-SRYCD(IDX-SRY)
                                   TO  TMP02-SRYCD
           MOVE    ZERO            TO  TMP02-RENNUM
                                       TMP02-SRYCD-IDX
      *
           .
       9011-TMP02-COMMON-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為月別集計ファイル
      *****************************************************************
       900-TMP03-READ-RND-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP03
      *
           INITIALIZE                  TMP03-REC
      *
           MOVE    TMP01-HOSPNUM   TO  TMP03-HOSPNUM
           MOVE    TMP01-PTID      TO  TMP03-PTID
           MOVE    WRK-TMP-SRYCD
                                   TO  TMP03-SRYCD
           MOVE    WRK-TMP-SRYYM
                                   TO  TMP03-SRYYM
           MOVE    WRK-TMP-SRYSURYO
                                   TO  TMP03-SRYSURYO
      *
           READ    TMP03-FILE
           INVALID
               MOVE    1           TO  FLG-TMP03
           END-READ
      *
           .
       900-TMP03-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    診療行為月別集計ファイルSTART処理
      *****************************************************************
       900-TMP03-START-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP03
      *
           INITIALIZE                  TMP03-REC
           MOVE    TMP01-HOSPNUM   TO  TMP03-HOSPNUM
           MOVE    TMP01-PTID      TO  TMP03-PTID
           MOVE    TMP01-SRYCD (TSRYCD-IDX)
                                   TO  TMP03-SRYCD
      *
           START   TMP03-FILE
               KEY IS  >=   TMP03-KEY
           INVALID
               MOVE    1               TO  FLG-TMP03
           NOT INVALID
               PERFORM 900-TMP03-READ-SEQ-SEC
           END-START
      *
           .
       900-TMP03-START-EXT.
           EXIT.
      *****************************************************************
      *    診療行為月別集計ファイル読込処理
      *****************************************************************
       900-TMP03-READ-SEQ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP03
      *
           READ    TMP03-FILE      NEXT
           AT  END
               MOVE    1           TO  FLG-TMP03
           NOT AT  END
      *
               IF  ( TMP03-HOSPNUM NOT =   TMP01-HOSPNUM )
                OR ( TMP03-PTID    NOT =   TMP01-PTID )
                OR ( TMP03-SRYCD   NOT =   TMP01-SRYCD (TSRYCD-IDX))
                   MOVE    1       TO  FLG-TMP03
               END-IF
      *
           END-READ
      *
           .
       900-TMP03-READ-SEQ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為日別集計ファイル
      *****************************************************************
       900-TMP04-READ-RND-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP04
      *
           INITIALIZE                  TMP04-REC
      *
           MOVE    TMP01-HOSPNUM   TO  TMP04-HOSPNUM
           MOVE    TMP01-PTID      TO  TMP04-PTID
           MOVE    WRK-TMP-SRYCD
                                   TO  TMP04-SRYCD
           MOVE    WRK-TMP-SRYYMD
                                   TO  TMP04-SRYYMD
           MOVE    WRK-TMP-SRYSURYO
                                   TO  TMP04-SRYSURYO
      *
           READ    TMP04-FILE
           INVALID
               MOVE    1           TO  FLG-TMP04
           END-READ
      *
           .
       900-TMP04-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    診療行為日別集計ファイルSTART処理
      *****************************************************************
       900-TMP04-START-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP04
      *
           INITIALIZE                  TMP04-REC
           MOVE    TMP01-HOSPNUM   TO  TMP04-HOSPNUM
           MOVE    TMP01-PTID      TO  TMP04-PTID
           MOVE    TMP01-SRYCD (TSRYCD-IDX)
                                   TO  TMP04-SRYCD
      *
           START   TMP04-FILE
               KEY IS  >=   TMP04-KEY
           INVALID
               MOVE    1               TO  FLG-TMP04
           NOT INVALID
               PERFORM 900-TMP04-READ-SEQ-SEC
           END-START
      *
           .
       900-TMP04-START-EXT.
           EXIT.
      *****************************************************************
      *    診療行為日別集計ファイル読込処理
      *****************************************************************
       900-TMP04-READ-SEQ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP04
      *
           READ    TMP04-FILE      NEXT
           AT  END
               MOVE    1           TO  FLG-TMP04
           NOT AT  END
      *
               IF  ( TMP04-HOSPNUM NOT =   TMP01-HOSPNUM )
                OR ( TMP04-PTID    NOT =   TMP01-PTID )
                OR ( TMP04-SRYCD   NOT =   TMP01-SRYCD (TSRYCD-IDX))
                   MOVE    1       TO  FLG-TMP04
               END-IF
      *
           END-READ
      *
           .
       900-TMP04-READ-SEQ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　接続処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           PERFORM 990-DBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           PERFORM 990-DBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
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
           PERFORM 990-DBMAIN-SEC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
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
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
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
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＯＭＭＩＴ処理
      *****************************************************************
       910-DBCOMMIT-SEC                SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
      *
           .
      *
       910-DBCOMMIT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       910-DBSTART-SEC                SECTION.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 990-DBMAIN-SEC
      *
           .
      *
       910-DBSTART-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＭＡＩＮ処理
      *****************************************************************
       990-DBMAIN-SEC                  SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              <       ZERO )
               MOVE    1               TO      FLG-DBERR
           END-IF
      *
           .
      *
       990-DBMAIN-EXT.
           EXIT.
