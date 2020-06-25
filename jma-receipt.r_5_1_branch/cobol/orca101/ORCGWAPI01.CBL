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
       PROGRAM-ID.         ORCGWAPI01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : システム管理（ＡＰＩ）
      *  コンポーネント名  : ユーザ管理
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/07/27    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-SYSBASE             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PASSWD              PIC 9(01).
           03  FLG-SYSUSER             PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-STRCHK              PIC 9(01).
           03  FLG-KATAKANA            PIC 9(01).
           03  FLG-OUT                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXS                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXL                    PIC 9(05).
           03  IDXM                    PIC 9(05).
           03  IDXN                    PIC 9(05).
           03  IDXP                    PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-SYSBASE             PIC 9(05).
           03  CNT-SYSKANRI            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-BASEYMD             PIC X(08).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-KANACHK-INPUT       PIC X(512).
           03  WRK-USER-NUMBER         PIC X(04).
           03  WRK-HOSPNUMX.
               05  WRK-HOSPNUM         PIC 9(02).
           03  WRK-NUMZ01              PIC Z(03).
           03  WRK-LENGTH              PIC 9(03).
           03  WRK-MI-NUMBER           PIC 9(03).
      *
       01  API-AREA.
           03  API-KJNYMD              PIC X(08).
           03  API-ADMIN               PIC X(01).
           03  API-SYS1010             PIC X(1000).
           03  API-UPD1010             PIC X(1000).
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *    API XML読み込み用定義体
           COPY    "CPMANAGEUSERSV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMANAGEUSERSV2RES.INC".
      *
       01  CONST-AREA.
           03  CONST-MI-MAX            PIC 9(05)   VALUE 50.
           03  CONST-USER-MAX          PIC 9(05)   VALUE 600.
           03  CONST-KATAKANA-G.
               05   FILLER             PIC X(08)   VALUE "　ー゛゜".
               05   FILLER             PIC X(10)   VALUE "アイウエオ".
               05   FILLER             PIC X(10)   VALUE "カキクケコ".
               05   FILLER             PIC X(10)   VALUE "サシスセソ".
               05   FILLER             PIC X(10)   VALUE "タチツテト".
               05   FILLER             PIC X(10)   VALUE "ナニヌネノ".
               05   FILLER             PIC X(10)   VALUE "ハヒフヘホ".
               05   FILLER             PIC X(10)   VALUE "マミムメモ".
               05   FILLER             PIC X(06)   VALUE "ヤユヨ".
               05   FILLER             PIC X(10)   VALUE "ラリルレロ".
               05   FILLER             PIC X(06)   VALUE "ワヲン".
               05   FILLER             PIC X(10)   VALUE "ガギグゲゴ".
               05   FILLER             PIC X(10)   VALUE "ザジズゼゾ".
               05   FILLER             PIC X(10)   VALUE "ダヂヅデド".
               05   FILLER             PIC X(10)   VALUE "バビブベボ".
               05   FILLER             PIC X(10)   VALUE "パピプペポ".
               05   FILLER             PIC X(02)   VALUE "ヴ".
               05   FILLER             PIC X(10)   VALUE "ァィゥェォ".
               05   FILLER             PIC X(10)   VALUE "ャュョヮッ".
           03  CONST-KATAKANA-R    REDEFINES   CONST-KATAKANA-G.
               05   CONST-KATAKANA     PIC X(02)   OCCURS  86.
           03  CONST-KATAKANA-MAX      PIC 9(05)   VALUE   86.
           03  CONST-GSRAUTH.
               05    FILLER            PIC X(10)   VALUE   "0 0 0     ".
               05    FILLER            PIC X(10)   VALUE   "     0000 ".
               05    FILLER            PIC X(10)   VALUE   "0000 0000 ".
               05    FILLER            PIC X(10)   VALUE   "00   0000 ".
               05    FILLER            PIC X(10)   VALUE   "0     0   ".
               05    FILLER            PIC X(10)   VALUE   "00   00   ".
               05    FILLER            PIC X(10)   VALUE   "000000000 ".
      *
           COPY    "CPMANAGEUSERSV2REQ.INC"    REPLACING   //MUREQ//
                                               BY          //MUTMP//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    カナチェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＵＩＤ取得用エリア
       01  SUID-AREA.
           03  SUID-RET                PIC X(2).
           03  SUID-UID                PIC X(36).
       01  SUID-ST                     PIC 9(2).
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    パスワード変換
           COPY    "CPORCSAUTH.INC".
      *    メニュー項目番号
           COPY    "CPMENUITEM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *    システムユーザ
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    パスワード
           COPY    "CPPASSWD.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "********************"
           DISPLAY   "* manageusers start*"
           DISPLAY   "********************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  API-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "********************"
           DISPLAY   "* manageusers end  *"
           DISPLAY   "********************"
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
           INITIALIZE                  XML-MUREQ
                                       XML-MURES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
           INITIALIZE                  SUID-AREA
           CALL    "orcuidset" USING   SUID-ST
                                       SUID-AREA
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "8099"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  WRK-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  WRK-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  WRK-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  WRK-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  WRK-ERRCD
               END-EVALUATE
      *
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    SPA-STAFFCD     TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
           IF    ( SYS-1010-KANRIAUTHFLG    =    "T" )
               MOVE    "1"         TO  API-ADMIN
           END-IF
      *
           PERFORM 1001-GROUP-CONFIG-CHK-SEC
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ診療設定チェック処理
      *****************************************************************
       1001-GROUP-CONFIG-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSBASE
           MOVE    ZERO                TO  CNT-SYSBASE
      *
           PERFORM 900-SYSBASE-ALL-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSBASE NOT =   ZERO )
                    OR   ( CNT-SYSBASE     >   1 )
      *
               COMPUTE CNT-SYSBASE = CNT-SYSBASE + 1
      *
               PERFORM 900-SYSBASE-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-GROUP-CONFIG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC         SECTION.
      *
           PERFORM 10011-REQNUM-CHECK-SEC
      *
           PERFORM 10011-BASE-DATE-CHECK-SEC
      *
      *    PERFORM 10011-GET-MI-NAME-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    リクエスト番号チェック処理
      *****************************************************************
       10011-REQNUM-CHECK-SEC          SECTION.
      *
           EVALUATE    MUREQ-REQNUM
           WHEN    "01"
           WHEN    "02"
           WHEN    "03"
           WHEN    "04"
               CONTINUE
           WHEN    OTHER
               MOVE    "0002"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-EVALUATE
      *
           .
       10011-REQNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       10011-BASE-DATE-CHECK-SEC       SECTION.
      *
           IF    ( MUREQ-BASE-DATE     =   SPACE )
               MOVE    SMCNDATE-YMD    TO  API-KJNYMD
           ELSE
               MOVE    MUREQ-BASE-DATE TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0001"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  API-KJNYMD
               END-IF
           END-IF
      *
           .
       10011-BASE-DATE-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    メニュー項目名取得区分
      *****************************************************************
     **10011-GET-MI-NAME-CHECK-SEC     SECTION.
      *
     **    IF    ( MUREQ-GET-MI-NAME   =   SPACE OR "0" OR "1" )
     **        CONTINUE
     **    ELSE
     **        MOVE    "0017"      TO  WRK-ERRCD
     **        PERFORM 990-EXIT-PROGRAM-SEC
     **    END-IF
      *
     **    .
     **10011-GET-MI-NAME-CHECK-EXT.
     **    EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           MOVE    ZERO            TO  FLG-ERR
      *
           EVALUATE    MUREQ-REQNUM
           WHEN    "01"
               PERFORM 2001-LIST-USERS-SEC
           WHEN    "02"
               PERFORM 2001-CREATE-ACCOUNT-SEC
           WHEN    "03"
               PERFORM 2001-UPDATE-ACCOUNT-SEC
           WHEN    "04"
               PERFORM 2001-DELETE-ACCOUNT-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ一覧処理
      *****************************************************************
       2001-LIST-USERS-SEC             SECTION.
      *
           PERFORM 20011-COMMON-RES-SEC
      *
           IF    ( MUREQ-USER-ID   =   SPACE )
               CONTINUE
           ELSE
               MOVE    MUREQ-USER-ID   TO  WRK-KANACHK-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               IF    ( FLG-STRCHK      NOT =   ZERO )
                   MOVE    "0004"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           PERFORM 900-PASSWD-ALL-USERS-SEL-SEC
      *
           MOVE    ZERO                TO  IDX1
      *
           PERFORM UNTIL ( FLG-PASSWD  NOT =   ZERO )
                    OR   ( IDX1            >=  CONST-USER-MAX )
      *
               INITIALIZE                  SYS-1010-REC
               MOVE    "1010"          TO  SYS-1010-KANRICD
               MOVE    PASSWD-USERID   TO  SYS-1010-USERID
               MOVE    "99999999"      TO  SYS-1010-STYUKYMD
               MOVE    "00000000"      TO  SYS-1010-EDYUKYMD
               MOVE    "%"             TO  SYS-1010-TBL (17 : )
               MOVE    SYS-1010-REC    TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY27-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                        OR   ( IDX1               >=  CONST-USER-MAX )
      *
                   MOVE    SYSKANRI-REC TO  SYS-1010-REC
      *
                   PERFORM 20011-LIST-USERS-SEC
      *
                   PERFORM 900-SYSKANRI-KEY27-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               PERFORM 900-PASSWD-ALL-USERS-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_passwd"        TO  MCP-TABLE
           MOVE    "all_users"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( MUREQ-USER-ID   NOT  =   SPACE )
            AND  ( IDX1                 =   ZERO )
               MOVE    "0012"  TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *
           IF    ( API-ADMIN      =   "1" )
      *        IF    ( MUREQ-GET-MI-NAME   =   "1" )
               PERFORM VARYING IDXP    FROM    1   BY  1
                       UNTIL ( IDXP    >   MENU-ITEM-MAX )
      *
                   IF    ( SPA-MW          =   SPA-GINBEE )
                    AND  ( MENU-ITEM-NUMBER (IDXP)
                                       =   3 OR 71 OR 82 )
                       CONTINUE
                   ELSE
                       MOVE    MENU-ITEM-NUMBER (IDXP)
                                       TO  WRK-NUMZ01
                       MOVE    FUNCTION TRIM(WRK-NUMZ01)
                                       TO  MURES-MI-HNUMBER (IDXP)
                       MOVE    MENU-ITEM-NAME (IDXP)
                                       TO  MURES-MI-HNAME   (IDXP)
                   END-IF
      *
               END-PERFORM
      *        END-IF
           END-IF
      *
           .
       2001-LIST-USERS-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ一覧処理
      *****************************************************************
       20011-LIST-USERS-SEC            SECTION.
      *
           COMPUTE IDX1  =  IDX1   +   1
      *
           MOVE    SYS-1010-USERID TO  MURES-USER-ID (IDX1 1)
      *
           IF    ( CNT-SYSBASE     >   1 )
               MOVE    SYS-1010-HOSPNUM
                                   TO  WRK-HOSPNUM
               MOVE    WRK-HOSPNUMX TO MURES-HOSPNUM (IDX1 1)
           END-IF
      *
           MOVE    SYS-1010-KBNCD (1:1)
                                   TO  MURES-GROUP-NUMBER (IDX1 1)
           MOVE    SYS-1010-KBNCD (2:4)
                                   TO  MURES-USER-NUMBER  (IDX1 1)
           MOVE    SYS-1010-NAME   TO  MURES-FULL-NAME (IDX1 1)
           MOVE    SYS-1010-KANANAME
                                   TO  MURES-KANA-NAME (IDX1 1)
      *
           IF    ( SYS-1010-STYUKYMD   NOT =   "00000000" )
            AND  ( SYS-1010-STYUKYMD   NOT =   "99999999" )
               MOVE    SYS-1010-STYUKYMD
                                   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  MURES-STYUKYMD (IDX1 1)
           END-IF
      *
           IF    ( SYS-1010-EDYUKYMD   NOT =   "00000000" )
            AND  ( SYS-1010-EDYUKYMD   NOT =   "99999999" )
      *
               MOVE    SYS-1010-EDYUKYMD
                                   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  MURES-EDYUKYMD (IDX1 1)
      *
            END-IF
      *
            IF    ( API-ADMIN      =   "1" )
                MOVE    IDX1           TO  IDXL
                PERFORM 20012-COMMON-RES-SEC
      *
            END-IF
      *
           .
       20011-LIST-USERS-EXT.
           EXIT.
      *****************************************************************
      *    共通返却項目編集処理
      *****************************************************************
       20011-COMMON-RES-SEC            SECTION.
      *
           MOVE    MUREQ-REQNUM        TO  MURES-REQNUM
      *
           MOVE    API-KJNYMD          TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  MURES-BASE-DATE
      *
           MOVE    1                   TO  IDXP
      *
           .
       20011-COMMON-RES-EXT.
           EXIT.
      *****************************************************************
      *    共通返却項目編集処理２
      *****************************************************************
       20012-COMMON-RES-SEC            SECTION.
      *
           IF    ( SYS-1010-KANRIAUTHFLG    =    "T" )
               MOVE    "1"             TO   MURES-ADMIN-PRIVILEGE (IDXL)
           ELSE
               MOVE    "0"             TO   MURES-ADMIN-PRIVILEGE (IDXL)
           END-IF
      *
           MOVE    ZERO                TO   IDXP
      *
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   MENU-ITEM-MAX )
      *
               EVALUATE    MENU-ITEM-NUMBER (IDXM)
               WHEN    1
               WHEN    91
               WHEN    101
                   IF    ( SYS-1010-KANRIAUTHFLG       =   "T" )
                       PERFORM 200120-COMMON-RES-SEC
                   ELSE
                       PERFORM 200121-COMMON-RES-SEC
                   END-IF
               WHEN    3
                   IF    ( SPA-MW      =   SPA-GINBEE )
                       CONTINUE
                   ELSE
                       IF    ( SYS-1010-KANRIAUTHFLG   =   "T" )
                           PERFORM 200120-COMMON-RES-SEC
                       END-IF
                   END-IF
               WHEN    71
               WHEN    82
                   IF    ( SPA-MW      =   SPA-GINBEE )
                       CONTINUE
                   ELSE
                       PERFORM 200121-COMMON-RES-SEC
                   END-IF
               WHEN    92
                   IF    ( SYS-1010-KANRIAUTHFLG       =   "T" )
                       PERFORM 200120-COMMON-RES-SEC
                   END-IF
               WHEN    OTHER
                   PERFORM 200121-COMMON-RES-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       20012-COMMON-RES-EXT.
           EXIT.
      *****************************************************************
      *    共通返却項目編集処理２
      *****************************************************************
       200120-COMMON-RES-SEC           SECTION.
      *
           COMPUTE IDXP    =   IDXP     +    1
           MOVE    MENU-ITEM-NUMBER (IDXM)
                                       TO  WRK-NUMZ01
           MOVE    FUNCTION TRIM(WRK-NUMZ01)
                                       TO  MURES-MI-NUMBER   (IDXL IDXP)
      *
           IF    ( MUREQ-REQNUM        =   "02" OR "03" )
               MOVE    MENU-ITEM-NAME (IDXM)
                                       TO  MURES-MI-NAME     (IDXL IDXP)
           END-IF
      *
           MOVE    "1"                 TO  MURES-MI-PRIVILEGE(IDXL IDXP)
      *
           .
       200120-COMMON-RES-EXT.
           EXIT.
      *****************************************************************
      *    共通返却項目編集処理２
      *****************************************************************
       200121-COMMON-RES-SEC           SECTION.
      *
           MOVE    MENU-ITEM-GSRAUTH (IDXM)
                                       TO   IDXN
           IF    ( SYS-1010-GSRAUTH(IDXN : 1)
                                       =    "1" )
               PERFORM 200120-COMMON-RES-SEC
           END-IF
      *
           .
       200121-COMMON-RES-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ新規登録
      *****************************************************************
       2001-CREATE-ACCOUNT-SEC         SECTION.
      *
           PERFORM 20011-COMMON-RES-SEC
      *
           MOVE    XML-MUREQ           TO  XML-MUTMP
      *
           PERFORM 20011-CREATE-ACCOUNT-CHK-SEC
      *
           PERFORM 20011-CREATE-ACCOUNT-SEC
      *
           .
       2001-CREATE-ACCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ新規登録時チェック処理
      *****************************************************************
       20011-CREATE-ACCOUNT-CHK-SEC    SECTION.
      *
           IF    ( MUTMP-USER-ID       NOT =   LOW-VALUE )
               PERFORM 2100-USER-ID-CHK-SEC
           END-IF
      *
           IF    ( MUTMP-USER-PASSWORD NOT =   LOW-VALUE )
               PERFORM 2100-USER-PASSWD-CHK-SEC
           END-IF
      *
           IF    ( MUTMP-GROUP-NUMBER  NOT =   LOW-VALUE )
               PERFORM 2100-GROUP-NUMBER-CHK-SEC
           END-IF
      *
           IF    ( MUTMP-FULL-NAME     NOT =   LOW-VALUE )
               PERFORM 2100-FULL-NAME-CHK-SEC
           END-IF
      *
           IF    ( MUTMP-KANA-NAME     NOT =   LOW-VALUE )
               PERFORM 2100-KANA-NAME-CHK-SEC
           END-IF
      *
           PERFORM 2100-PRIVILEGE-CHK-SEC
      *
           .
       20011-CREATE-ACCOUNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザＩＤチェック処理
      *****************************************************************
       2100-USER-ID-CHK-SEC            SECTION.
      *
           IF    ( MUTMP-USER-ID   =   SPACE )
               MOVE    "0003"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 21001-USER-ID-CHK-SEC
      *
           PERFORM 900-PASSWD-KEY-SEL-SEC
           IF    ( FLG-PASSWD  =   ZERO )
               MOVE    "0011"  TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 900-SYSUSER-KEY-SEL-SEC
           IF    ( FLG-SYSUSER =   ZERO )
               MOVE    "0011"  TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2100-USER-ID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザＩＤチェック処理
      *****************************************************************
       21001-USER-ID-CHK-SEC               SECTION.
      *
           MOVE    MUTMP-USER-ID   TO  WRK-KANACHK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-STRCHK      NOT =   ZERO )
               MOVE    "0018"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE  FUNCTION LENGTH(FUNCTION TRIM(MUTMP-USER-ID TRAILING))
                                   TO  WRK-LENGTH
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-LENGTH )
      *
               EVALUATE FUNCTION TRIM(MUTMP-USER-ID TRAILING)(IDX0 : 1)
               WHEN    "A" THRU    "Z"
               WHEN    "a" THRU    "z"
               WHEN    "0" THRU    "9"
               WHEN    "_"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "0018"  TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       21001-USER-ID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザパスワードチェック処理
      *****************************************************************
       2100-USER-PASSWD-CHK-SEC            SECTION.
      *
           IF    ( MUTMP-USER-PASSWORD =   SPACE )
               MOVE    "0005"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    MUTMP-USER-PASSWORD
                                   TO  WRK-KANACHK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-STRCHK      NOT =   ZERO )
               MOVE    "0006"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2100-USER-PASSWD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    グループ番号チェック処理
      *****************************************************************
       2100-GROUP-NUMBER-CHK-SEC       SECTION.
      *
           IF    ( MUTMP-GROUP-NUMBER  =  "0" OR "1" OR "2" OR "3"
                                           OR "4" OR "5" )
               CONTINUE
           ELSE
               MOVE    "0007"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2100-GROUP-NUMBER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    氏名チェック処理
      *****************************************************************
       2100-FULL-NAME-CHK-SEC          SECTION.
      *
           IF    ( MUTMP-FULL-NAME  =  SPACE )
               MOVE    "0008"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    MUTMP-FULL-NAME  TO  WRK-KANACHK-INPUT
           PERFORM 800-ZENKAKU-CHK-SEC
           IF    ( FLG-STRCHK      NOT =   ZERO )
               MOVE    "0009"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2100-FULL-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カナ氏名チェック処理
      *****************************************************************
       2100-KANA-NAME-CHK-SEC          SECTION.
      *
           IF    ( MUTMP-KANA-NAME  =  SPACE )
               CONTINUE
           ELSE
               MOVE    MUTMP-KANA-NAME  TO  WRK-KANACHK-INPUT
               PERFORM 800-KATAKANA-CHK-SEC
               IF    ( FLG-STRCHK      NOT =   ZERO )
                   MOVE    "0010"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           .
       2100-KANA-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    権限チェック処理
      *****************************************************************
       2100-PRIVILEGE-CHK-SEC          SECTION.
      *
           IF    ( MUTMP-ADMIN-PRIVILEGE   =   SPACE )
               CONTINUE
           ELSE
               IF    ( API-ADMIN           =   "1" )
                   IF    ( MUTMP-ADMIN-PRIVILEGE   =   "0" OR "1" )
                       CONTINUE
                   ELSE
                       MOVE    "0019"      TO  WRK-ERRCD
                       PERFORM 990-EXIT-PROGRAM-SEC
                   END-IF
               ELSE
                       MOVE    "0020"      TO  WRK-ERRCD
                       PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDXP    FROM    1   BY  1
                   UNTIL ( IDXP    >   CONST-MI-MAX )
               IF    ( MUTMP-MI-NUMBER (IDXP)  NOT =   SPACE )
                   PERFORM 21001-PRIVILEGE-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       2100-PRIVILEGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    権限チェック処理
      *****************************************************************
       21001-PRIVILEGE-CHK-SEC         SECTION.
      *
           IF    ( API-ADMIN           =   "1" )
               MOVE    MUTMP-MI-NUMBER (IDXP)
                                       TO  WRK-MI-NUMBER
               PERFORM 210011-PRIVILEGE-CHK-SEC
               IF    ( IDXA            =   ZERO )
                   MOVE    "0021"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               IF    ( MUTMP-MI-PRIVILEGE (IDXP)
                                       =   "0" OR "1" )
                   CONTINUE
               ELSE
                   MOVE    "0022"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
           ELSE
               MOVE    "0020"          TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       21001-PRIVILEGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    権限チェック処理
      *****************************************************************
       210011-PRIVILEGE-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   CONST-MI-MAX )
                    OR   ( IDXA    >   ZERO )
      *
               IF    ( MENU-ITEM-NUMBER (IDXM)
                                   =   WRK-MI-NUMBER )
                   MOVE    IDXM    TO  IDXA
               END-IF
      *
           END-PERFORM
      *
           .
       210011-PRIVILEGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ新規登録処理
      *****************************************************************
       20011-CREATE-ACCOUNT-SEC        SECTION.
      *
           PERFORM 2200-EDIT-PASSWD-SEC
      *
           PERFORM 2200-EDIT-SYSUSER-SEC
      *
           PERFORM 2200-EDIT-SYSKANRI-SEC
      *
           PERFORM 2200-EDIT-RESPONSE-SEC
      *
           IF    ( API-ADMIN      =   "1" )
               MOVE    1                   TO  IDXL
               PERFORM 20012-COMMON-RES-SEC
           END-IF
      *
           .
       20011-CREATE-ACCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    パスワード登録処理
      *****************************************************************
       2200-EDIT-PASSWD-SEC            SECTION.
      *
           INITIALIZE                  PASSWD-REC
           MOVE    MUTMP-USER-ID   TO  PASSWD-USERID
      *
           INITIALIZE                  ORCSAUTHAREA
           MOVE    "passwd"        TO  AUTH-COMMAND
           MOVE    1               TO  AUTH-ID
           MOVE    MUTMP-USER-PASSWORD
                                   TO  AUTH-PASSWORD1
           CALL    "orcsauth"          USING
                                       ORCSAUTHAREA
           IF    ( AUTH-RC         =   ZERO )
                MOVE    AUTH-PASSWORD2
                                   TO  PASSWD-PASSWORD
           ELSE
               MOVE    "4001"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    PASSWD-REC      TO  MCPDATA-REC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4002"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-EDIT-PASSWD-EXT.
           EXIT.
      *****************************************************************
      *    パスワード更新処理(ユーザＩＤ）
      *****************************************************************
       2200-UPDATE-PASSWD-SEC          SECTION.
      *
           INITIALIZE                  PASSWD-REC
           MOVE    MUTMP-USER-ID   TO  PASSWD-USERID
           MOVE    MUTMP-NEW-USER-ID
                                   TO  PASSWD-UPD-USERID
      *
           MOVE    PASSWD-REC      TO  MCPDATA-REC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "upd1"          TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4009"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-UPDATE-PASSWD-EXT.
           EXIT.
      *****************************************************************
      *    システムユーザ登録処理
      *****************************************************************
       2200-EDIT-SYSUSER-SEC           SECTION.
      *
           INITIALIZE                  SYSUSER-REC
           MOVE    MUTMP-USER-ID   TO  SYSUSER-USERID
           MOVE    SPA-HOSPNUM     TO  SYSUSER-HOSPNUM
           MOVE    "F"             TO  SYSUSER-SUPERVISOR
           MOVE    SMCNDATE-YMD    TO  SYSUSER-UPYMD
           MOVE    SYSUSER-REC     TO  MCPDATA-REC
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4003"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-EDIT-SYSUSER-EXT.
           EXIT.
      *****************************************************************
      *    システム管理登録処理
      *****************************************************************
       2200-EDIT-SYSKANRI-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-USER-NUMBER
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    MUTMP-GROUP-NUMBER
                                   TO  SYS-KBNCD
           MOVE    "99999999"      TO  SYS-STYUKYMD
           MOVE    "00000000"      TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY28-SEL-SEC
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "4004"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               MOVE    SYS-KBNCD
                                   TO  WRK-USER-NUMBER
           END-IF
      *
           INITIALIZE                  SYS-1010-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    MUTMP-GROUP-NUMBER
                                   TO  SYS-1010-KBNCD(1:1)
           MOVE    WRK-USER-NUMBER
                                   TO  SYS-1010-KBNCD(2:)
           MOVE    "00000000"      TO  SYS-1010-STYUKYMD
           MOVE    "99999999"      TO  SYS-1010-EDYUKYMD
           MOVE    SYS-1010-KBNCD  TO  SYS-1010-USERID
           MOVE    MUTMP-USER-ID   TO  SYS-1010-USERID
           MOVE    MUTMP-KANA-NAME
                                   TO  SYS-1010-KANANAME
           MOVE    MUTMP-FULL-NAME
                                   TO  SYS-1010-NAME
           MOVE    CONST-GSRAUTH   TO  SYS-1010-GSRAUTH
      *
           PERFORM VARYING IDXP    FROM    1   BY  1
                   UNTIL ( IDXP    >   CONST-MI-MAX )
      *
               MOVE    MUTMP-MI-NUMBER (IDXP)
                                   TO  WRK-MI-NUMBER
               PERFORM 210011-PRIVILEGE-CHK-SEC
               IF    ( IDXA        >   ZERO )
                   MOVE    MENU-ITEM-GSRAUTH (IDXA)
                                   TO  IDXN
                   IF    ( MUTMP-MI-PRIVILEGE (IDXP)
                                   =   "1" )
                       MOVE    "1" TO  SYS-1010-GSRAUTH (IDXN : 1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( MUTMP-ADMIN-PRIVILEGE   =   "1" )
               MOVE    "T"         TO  SYS-1010-KANRIAUTHFLG
           ELSE
               MOVE    "F"         TO  SYS-1010-KANRIAUTHFLG
           END-IF
      *
           IF    ( SYS-1010-KANRIAUTHFLG       =   "T" )
               MOVE    "1"         TO  SYS-1010-GSRAUTH
                                              (MENU-ITEM001-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM003-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM091-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM092-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM101-GSRAUTH : 1)
           ELSE
               MOVE    "0"         TO  SYS-1010-GSRAUTH
                                              (MENU-ITEM003-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM092-GSRAUTH : 1)
           END-IF
      *
           IF    ( SPA-BEDFLG      >   ZERO )
               MOVE    "1"         TO  SYS-1010-ORCA21FLG
           ELSE
               MOVE    "2"         TO  SYS-1010-ORCA21FLG
           END-IF
           MOVE    "0"             TO  SYS-1010-CLIENT-SAVE
                                       SYS-1010-USER-KANRI
                                       SYS-1010-CLIENT-PRT
                                       SYS-1010-DIALOGFLG
           MOVE    "api_"          TO  SYS-1010-OPID
           MOVE    SPA-OPID        TO  SYS-1010-OPID (5:)
           MOVE    SMCNDATE-YMD    TO  SYS-1010-CREYMD
                                       SYS-1010-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-1010-UPHMS
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4005"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-EDIT-SYSKANRI-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（登録・削除時）
      *****************************************************************
       2200-EDIT-RESPONSE-SEC          SECTION.
      *
           MOVE    SYS-1010-USERID     TO  MURES-USER-ID (1 1)
      *
           IF    ( SPA-GRPHOSPKBN  =   1 )
               MOVE    SYS-1010-HOSPNUM
                                       TO  WRK-HOSPNUM
               MOVE    WRK-HOSPNUMX
                                       TO  MURES-HOSPNUM (1 1)
           END-IF
      *
           MOVE    SYS-1010-KBNCD (1:1)
                                       TO  MURES-GROUP-NUMBER (1 1)
           MOVE    SYS-1010-KBNCD (2:)
                                       TO  MURES-USER-NUMBER (1 1)
           MOVE    SYS-1010-NAME       TO  MURES-FULL-NAME (1 1)
           MOVE    SYS-1010-KANANAME
                                       TO  MURES-KANA-NAME (1 1)
      *
           .
       2200-EDIT-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（変更時）
      *****************************************************************
       2201-EDIT-RESPONSE-SEC          SECTION.
      *
           MOVE    API-UPD1010         TO  SYS-1010-REC
           PERFORM 2200-EDIT-RESPONSE-SEC
           MOVE    MURES-USER-INF-OCC2 (1 1)
                                       TO  MURES-USER-INF-OCC2 (1 2)
      *
           IF    ( API-ADMIN      =   "1" )
               MOVE    1                   TO  IDXL
               PERFORM 20012-COMMON-RES-SEC
           END-IF
      *
           INITIALIZE                      MURES-USER-INF-OCC2 (1 1)
      *
           MOVE    API-SYS1010         TO  SYS-1010-REC
           PERFORM 2200-EDIT-RESPONSE-SEC
      *
           .
       2201-EDIT-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ削除
      *****************************************************************
       2001-DELETE-ACCOUNT-SEC         SECTION.
      *
           PERFORM 20011-COMMON-RES-SEC
      *
           MOVE    XML-MUREQ           TO  XML-MUTMP
      *
           PERFORM 20011-DELETE-ACCOUNT-CHK-SEC
      *
           PERFORM 20011-DELETE-ACCOUNT-SEC
      *
           .
       2001-DELETE-ACCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ削除時チェック処理
      *****************************************************************
       20011-DELETE-ACCOUNT-CHK-SEC    SECTION.
      *
           IF    ( MUTMP-USER-ID   =   SPACE )
               MOVE    "0003"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    MUTMP-USER-ID   TO  WRK-KANACHK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-STRCHK      NOT =   ZERO )
               MOVE    "0004"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 900-PASSWD-KEY-SEL-SEC
           IF    ( FLG-PASSWD  =   ZERO )
               CONTINUE
           ELSE
               PERFORM 900-SYSUSER-KEY-SEL-SEC
               IF    ( FLG-SYSUSER =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "0012"  TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           PERFORM 200111-SYSKANRI-CHK-SEC
           MOVE    API-SYS1010     TO  SYS-1010-REC
           IF    ( SYS-1010-KBNCD  =   "00001" )
               IF    ( MUTMP-REQNUM    =   "03" )
                   MOVE    "0023"      TO  WRK-ERRCD
               ELSE
                   MOVE    "0016"      TO  WRK-ERRCD
               END-IF
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       20011-DELETE-ACCOUNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ削除時チェック処理
      *****************************************************************
       20011-DELETE-ACCOUNT-SEC    SECTION.
      *
           PERFORM 2200-DELETE-PASSWD-SEC
      *
           PERFORM 2200-DELETE-SYSUSER-SEC
      *
           MOVE    API-SYS1010 TO  SYS-1010-REC
           PERFORM 2200-DELETE-SYSKANRI-SEC
      *
           PERFORM 2200-EDIT-RESPONSE-SEC
      *
           .
       20011-DELETE-ACCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    パスワード削除処理
      *****************************************************************
       2200-DELETE-PASSWD-SEC          SECTION.
      *
           INITIALIZE                  PASSWD-REC
           MOVE    MUTMP-USER-ID   TO  PASSWD-USERID
           MOVE    PASSWD-REC      TO  MCPDATA-REC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4006"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-DELETE-PASSWD-EXT.
           EXIT.
      *****************************************************************
      *    システムユーザ削除処理
      *****************************************************************
       2200-DELETE-SYSUSER-SEC           SECTION.
      *
           INITIALIZE                  SYSUSER-REC
           MOVE    MUTMP-USER-ID   TO  SYSUSER-USERID
           MOVE    SYSUSER-REC     TO  MCPDATA-REC
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4007"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-DELETE-SYSUSER-EXT.
           EXIT.
      *****************************************************************
      *    システム管理削除処理
      *****************************************************************
       2200-DELETE-SYSKANRI-SEC        SECTION.
      *
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4008"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-DELETE-SYSKANRI-EXT.
           EXIT.
      *****************************************************************
      *    システム管理追加処理
      *****************************************************************
       2200-INSERT-SYSKANRI-SEC        SECTION.
      *
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4010"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2200-INSERT-SYSKANRI-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ更新処理
      *****************************************************************
       2001-UPDATE-ACCOUNT-SEC     SECTION.
      *
           PERFORM 20011-COMMON-RES-SEC
      *
           PERFORM 20011-UPDATE-ACCOUNT-CHK-SEC
      *
           PERFORM 20011-UPDATE-ACCOUNT-SEC
      *
           .
       2001-UPDATE-ACCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ更新処理時チェック処理
      *****************************************************************
       20011-UPDATE-ACCOUNT-CHK-SEC    SECTION.
      *
           MOVE    XML-MUREQ           TO  XML-MUTMP
           PERFORM 20011-DELETE-ACCOUNT-CHK-SEC
      *
           PERFORM 200111-EDIT-UPDATE-DATA-SEC
      *
           PERFORM 20011-CREATE-ACCOUNT-CHK-SEC
      *
           .
       20011-UPDATE-ACCOUNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ更新情報編集処理
      *****************************************************************
       200111-EDIT-UPDATE-DATA-SEC     SECTION.
      *
           INITIALIZE                      XML-MUTMP
      *
           IF    ( MUREQ-NEW-USER-ID   =   SPACE )
               MOVE    LOW-VALUE       TO  MUTMP-USER-ID
           ELSE
               MOVE    MUREQ-NEW-USER-ID
                                       TO  MUTMP-USER-ID
           END-IF
      *
           IF    ( MUREQ-NEW-USER-PASSWORD
                                       =   SPACE )
               MOVE    LOW-VALUE       TO  MUTMP-USER-PASSWORD
           ELSE
               MOVE    MUREQ-NEW-USER-PASSWORD
                                       TO  MUTMP-USER-PASSWORD
           END-IF
      *
           IF    ( MUREQ-NEW-FULL-NAME =   SPACE )
               MOVE    LOW-VALUE       TO  MUTMP-FULL-NAME
           ELSE
               MOVE    MUREQ-NEW-FULL-NAME
                                       TO  MUTMP-FULL-NAME
           END-IF
      *
           IF    ( MUREQ-NEW-KANA-NAME =   SPACE )
               MOVE    LOW-VALUE       TO  MUTMP-KANA-NAME
           ELSE
               MOVE    MUREQ-NEW-KANA-NAME
                                       TO  MUTMP-KANA-NAME
           END-IF
      *
           MOVE        LOW-VALUE       TO  MUTMP-GROUP-NUMBER
      *
           MOVE    MUREQ-ADMIN-PRIVILEGE
                                       TO  MUTMP-ADMIN-PRIVILEGE
           MOVE    MUREQ-MI-NUMBER-INFORMATION-G
                                       TO  MUTMP-MI-NUMBER-INFORMATION-G
      *
           .
       200111-EDIT-UPDATE-DATA-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ更新処理
      *****************************************************************
       20011-UPDATE-ACCOUNT-SEC     SECTION.
      *
           INITIALIZE                          XML-MUTMP
           MOVE    API-SYS1010                 TO  SYS-1010-REC
      *
           IF    ( MUREQ-NEW-USER-PASSWORD     NOT =   SPACE )
               MOVE    MUREQ-USER-ID           TO  MUTMP-USER-ID
               PERFORM 2200-DELETE-PASSWD-SEC
               MOVE    MUREQ-NEW-USER-PASSWORD
                                               TO  MUTMP-USER-PASSWORD
               PERFORM 2200-EDIT-PASSWD-SEC
           END-IF
      *
           IF    ( MUREQ-NEW-USER-ID           NOT =   SPACE )
               MOVE    MUREQ-USER-ID           TO  MUTMP-USER-ID
               PERFORM 2200-DELETE-SYSUSER-SEC
               MOVE    MUREQ-NEW-USER-ID       TO  MUTMP-USER-ID
               PERFORM 2200-EDIT-SYSUSER-SEC
               MOVE    MUREQ-USER-ID           TO  MUTMP-USER-ID
               MOVE    MUREQ-NEW-USER-ID       TO  MUTMP-NEW-USER-ID
               PERFORM 2200-UPDATE-PASSWD-SEC
      *
               MOVE    MUREQ-NEW-USER-ID       TO  SYS-1010-USERID
      *
           END-IF
      *
           IF    ( MUREQ-NEW-FULL-NAME NOT =   SPACE )
               MOVE    MUREQ-NEW-FULL-NAME     TO  SYS-1010-NAME
           END-IF
      *
           IF    ( MUREQ-NEW-KANA-NAME NOT =   SPACE )
               MOVE    MUREQ-NEW-KANA-NAME     TO  SYS-1010-KANANAME
           END-IF
      *
           PERFORM VARYING IDXP    FROM    1   BY  1
                   UNTIL ( IDXP    >   CONST-MI-MAX )
      *
               IF    ( MUREQ-MI-NUMBER (IDXP)  NOT =   SPACE )
                   MOVE    MUREQ-MI-NUMBER (IDXP)
                                       TO  WRK-MI-NUMBER
                   PERFORM 210011-PRIVILEGE-CHK-SEC
                   IF    ( IDXA        >   ZERO )
                       MOVE    MENU-ITEM-GSRAUTH (IDXA)
                                   TO  IDXN
                       EVALUATE    MUREQ-MI-PRIVILEGE (IDXP)
                       WHEN    "1"
                           MOVE    "1" TO  SYS-1010-GSRAUTH (IDXN : 1)
                       WHEN    "0"
                           MOVE    "0" TO  SYS-1010-GSRAUTH (IDXN : 1)
                       END-EVALUATE
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    MUREQ-ADMIN-PRIVILEGE
           WHEN    "1"
               MOVE    "T"         TO  SYS-1010-KANRIAUTHFLG
           WHEN    "0"
               MOVE    "F"         TO  SYS-1010-KANRIAUTHFLG
           END-EVALUATE
      *
           IF    ( SYS-1010-KANRIAUTHFLG       =   "T" )
               MOVE    "1"         TO  SYS-1010-GSRAUTH
                                              (MENU-ITEM001-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM003-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM091-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM092-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM101-GSRAUTH : 1)
           ELSE
               MOVE    "0"         TO  SYS-1010-GSRAUTH
                                              (MENU-ITEM003-GSRAUTH : 1)
                                       SYS-1010-GSRAUTH
                                              (MENU-ITEM092-GSRAUTH : 1)
           END-IF
      *
           MOVE    SMCNDATE-YMD                TO  SYS-1010-UPYMD
           MOVE    SMCNDATE-HMS                TO  SYS-1010-UPHMS
      *
           MOVE    SYS-1010-REC                TO  API-UPD1010
      *
           PERFORM 2200-DELETE-SYSKANRI-SEC
      *
           PERFORM 2200-INSERT-SYSKANRI-SEC
      *
           PERFORM 2201-EDIT-RESPONSE-SEC
      *
           .
       20011-UPDATE-ACCOUNT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理チェック処理
      *****************************************************************
       200111-SYSKANRI-CHK-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-OUT
           MOVE    ZERO            TO  CNT-SYSKANRI
      *
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    MUTMP-USER-ID   TO  SYS-1010-USERID
           MOVE    "99999999"      TO  SYS-1010-STYUKYMD
           MOVE    "00000000"      TO  SYS-1010-EDYUKYMD
           MOVE    "%"             TO  SYS-1010-TBL (17 : )
           MOVE    SYS-1010-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY27-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDX1               >=  CONST-USER-MAX )
      *
               MOVE    SYSKANRI-REC TO SYS-1010-REC
                                       API-SYS1010
      *
               COMPUTE CNT-SYSKANRI =  CNT-SYSKANRI    +   1
      *
               IF    ( API-KJNYMD >=   SYS-1010-STYUKYMD )
                AND  ( API-KJNYMD <=   SYS-1010-EDYUKYMD )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-OUT
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY27-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( CNT-SYSKANRI        =   ZERO )
               MOVE    "0015"          TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( CNT-SYSKANRI        >   1 )
               MOVE    "0013"          TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( FLG-OUT         NOT =   ZERO )
               MOVE    "0014"          TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       200111-SYSKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  MURES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  MURES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD       =   SPACE )
               MOVE    "0000"      TO  WRK-ERRCD
           END-IF
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD       TO  MURES-API-RESULT
           MOVE    WRK-ERRMSG      TO  MURES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
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
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
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
      *
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-STRCHK
      *
           IF  ( WRK-KANACHK-INPUT NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-STRCHK
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-STRCHK
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角文字チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-STRCHK
      *
           IF  ( WRK-KANACHK-INPUT     NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-STRCHK
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-STRCHK
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カタカナチェック
      *****************************************************************
       800-KATAKANA-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-STRCHK
      *
           PERFORM VARYING IDXS    FROM    1   BY  2
                   UNTIL ( IDXS    >   512 )
                    OR   ( WRK-KANACHK-INPUT (IDXS : )
                                   =   SPACE )
                    OR   ( FLG-STRCHK
                               NOT =   ZERO  )
      *
               MOVE    ZERO            TO  FLG-KATAKANA
               PERFORM VARYING IDXK    FROM    1   BY  1
                       UNTIL ( IDXK    >   CONST-KATAKANA-MAX )
                        OR   ( FLG-KATAKANA
                                   NOT =   ZERO )
      *
                   IF    ( WRK-KANACHK-INPUT (IDXS : 2)
                                   =   CONST-KATAKANA (IDXK))
                       MOVE    1       TO  FLG-KATAKANA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-KATAKANA    =   ZERO )
                   MOVE    1           TO  FLG-STRCHK
               END-IF
      *
           END-PERFORM
      *
           .
       800-KATAKANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "0000"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "0001"
               MOVE    "基準日の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0002"
               MOVE    "リクエスト番号の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0003"
               MOVE    "ユーザＩＤが未入力です"
                                       TO  WRK-ERRMSG
           WHEN    "0004"
               MOVE    "ユーザＩＤに半角以外の文字が入力されています"
                                       TO  WRK-ERRMSG
           WHEN    "0005"
               MOVE    "パスワードが未入力です"
                                       TO  WRK-ERRMSG
           WHEN    "0006"
               MOVE    "パスワードに半角以外の文字が入力されています"
                                       TO  WRK-ERRMSG
           WHEN    "0007"
               MOVE    "職員区分の入力に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0008"
               MOVE    "氏名が未入力です"
                                       TO  WRK-ERRMSG
           WHEN    "0009"
               MOVE    "氏名に全角以外の文字が入力されています"
                                       TO  WRK-ERRMSG
           WHEN    "0010"
               MOVE "カナ氏名に全角カタカナ以外の文字が入力されています"
                                       TO  WRK-ERRMSG
           WHEN    "0011"
               MOVE    "既に同じユーザＩＤの登録があります"
                                       TO  WRK-ERRMSG
           WHEN    "0012"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    "0013"
               STRING  "システム管理に同じユーザＩＤの登録が複数"
                                           DELIMITED BY SIZE
                       "あります。更新できません"
                                           DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "0014"
               STRING  "システム管理の有効期間が切れています。"
                                           DELIMITED BY SIZE
                       "更新できません"
                                           DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "0015"
               STRING  "システム管理が未登録です"
                                           DELIMITED BY SIZE
                       "。更新できません"
                                           DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "0016"
               MOVE    "ユーザはオルカマスターです。削除できません"
                                       TO  WRK-ERRMSG
           WHEN    "0017"
               MOVE    "メニュー項目名称取得区分の入力に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0018"
               STRING  "ユーザＩＤに使用可能な文字は半角の英数字と"
                                           DELIMITED BY SIZE
                       "下線符号（アンダーバー）のみです"
                                           DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "0019"
               MOVE    "管理者権限の入力に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0020"
               MOVE    "権限の設定は管理者のみ可能です"
                                       TO  WRK-ERRMSG
           WHEN    "0021"
               MOVE    "該当する業務が存在しません"
                                       TO  WRK-ERRMSG
           WHEN    "0022"
               MOVE    "業務権限の入力に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0023"
               MOVE    "ユーザはオルカマスターです。変更できません"
                                       TO  WRK-ERRMSG
           WHEN    "4000"  THRU    "4010"
               MOVE    "ユーザ情報の更新に失敗しました"
                                       TO  WRK-ERRMSG
      *共通エラー
           WHEN    "8900"
               MOVE    "システム項目が設定できません"
                                       TO  WRK-ERRMSG
           WHEN    "8901"
               MOVE    "職員情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "8903"
               MOVE    "システム日付が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "8915"
               STRING  "グループ医療機関が不整合です。"
                                           DELIMITED  BY  SIZE
                       "処理を終了して下さい。"
                                           DELIMITED  BY  SIZE
               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "8097"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "8098"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "8099"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました"
                                       TO  WRK-ERRMSG
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( WAPI01-BODY      NOT =   LOW-VALUE )
               DISPLAY "xml_manageusersv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    WAPI01-BODY             TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "manageusersreq"        TO  APIREQ-RECORDNAME
           MOVE    "xml_manageusersv2req"  TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE    "8098"           TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE                TO  WAPI01-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  MUREQ-MANAGE-USERSREQ
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( WAPI01-BODY    NOT =   LOW-VALUE )
               DISPLAY "manageusersv2 object is not low_value"
           END-IF
      *
      *
      *    Response is SO LONG that you can't use XML-APIREQ.
           MOVE    1                       TO  MURES-MODE
           MOVE    "manageusersres"       TO   MURES-RECORDNAME
           MOVE    "xml_manageusersv2res" TO   MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-MURES
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    MURES-OBJECT        TO  WAPI01-BODY
               MOVE    "application/xml"   TO  WAPI01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       9001-XML-RESPONSE-SEC              SECTION.
      *
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-MURES
      *
           .
       9001-XML-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    システム基本検索処理
      *****************************************************************
       900-SYSBASE-ALL-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSBASE
      *
           INITIALIZE                      SYSBASE-REC
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
      *
           .
       900-SYSBASE-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム基本FETCH処理
      *****************************************************************
       900-SYSBASE-ALL-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSBASE
      *
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
      *
           .
       900-SYSBASE-ALL-FET-EXT.
           EXIT.
      *****************************************************************
      *    パスワード情報検索処理
      *****************************************************************
       900-PASSWD-ALL-USERS-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PASSWD
      *
           INITIALIZE                  PASSWD-REC
           MOVE    MUREQ-USER-ID       TO  PASSWD-USERID
           MOVE    PASSWD-REC          TO  MCPDATA-REC
           MOVE    "tbl_passwd"        TO  MCP-TABLE
           MOVE    "all_users"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PASSWD-REC
           ELSE
               INITIALIZE                  PASSWD-REC
               MOVE    1               TO  FLG-PASSWD
           END-IF
      *
           .
       900-PASSWD-ALL-USERS-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パスワード情報FETCH処理
      *****************************************************************
       900-PASSWD-ALL-USERS-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PASSWD
      *
           MOVE    "tbl_passwd"        TO  MCP-TABLE
           MOVE    "all_users"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PASSWD-REC
           ELSE
               INITIALIZE                  PASSWD-REC
               MOVE    1               TO  FLG-PASSWD
           END-IF
      *
           .
       900-PASSWD-ALL-USERS-FET-EXT.
           EXIT.
      *****************************************************************
      *    パスワード情報検索処理
      *****************************************************************
       900-PASSWD-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PASSWD
      *
           INITIALIZE                      PASSWD-REC
           MOVE    MUTMP-USER-ID       TO  PASSWD-USERID
           MOVE    PASSWD-REC          TO  MCPDATA-REC
           MOVE    "tbl_passwd"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PASSWD-REC
           ELSE
               INITIALIZE                  PASSWD-REC
               MOVE    1               TO  FLG-PASSWD
           END-IF
      *
           MOVE    "tbl_passwd"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PASSWD-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システムユーザ情報検索処理
      *****************************************************************
       900-SYSUSER-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSUSER
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    MUTMP-USER-ID       TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
      *
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSUSER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY27-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY27-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY27-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY28-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY28-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
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
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"       TO  MCP-FUNC
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
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "8099" )
               MOVE    404             TO  WAPI01-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       9901-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  MURES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  MURES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD       TO  MURES-API-RESULT
           MOVE    WRK-ERRMSG      TO  MURES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
