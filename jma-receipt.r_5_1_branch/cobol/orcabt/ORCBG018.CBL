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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBG018.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 経過措置薬剤更新
      *                    :（チェックマスタ・薬剤情報マスタ・患者禁忌薬剤）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/09/07    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者        日付      内容
      *  04.08.01    NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    経過措置薬剤情報
           SELECT  IN01-FILE       ASSIGN  BG0181
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN01-KEY
                                   FILE    STATUS  IS  STS-IN01.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    経過措置薬剤情報
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-KEY.
               05  IN01-MSTKBN         PIC X(01).
               05  IN01-PROCKBN        PIC X(01).
               05  IN01-KANANAME       PIC X(100).
               05  IN01-SRYCD-OLD      PIC X(09).
           03  IN01-SRYCD-NEW          PIC X(09).
           03  IN01-DELFLG             PIC 9(01).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    経過措置薬剤情報（追加分）
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0181//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-IN01            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-IN01            PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-CHKSND          PIC 9(01).
           03  FLG-CHKTRD          PIC 9(01).
           03  FLG-CHK005          PIC 9(01).
           03  FLG-YAKUJYO         PIC 9(01).
           03  FLG-PTKINKI         PIC 9(01).
           03  FLG-TCHKMST         PIC 9(01).
           03  FLG-TCHKSND         PIC 9(01).
           03  FLG-TCHKTRD         PIC 9(01).
           03  FLG-TCHK005         PIC 9(01).
           03  FLG-TYAKUJYO        PIC 9(01).
           03  FLG-TPTKINKI        PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(03).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-SEQ             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SRYYMD     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-SYMD            PIC X(08).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-YUKOEDYMD-N     PIC X(22).
           03  WRK-SAKYMD-N        PIC X(22).
           03  WRK-MSTKBN          PIC X(01).
           03  WRK-PROCKBN         PIC X(01).
           03  WRK-ZZZZ9-G.
               05  WRK-ZZZZ9       PIC ZZZZ9.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-MSTMSG          PIC X(20).
           03  WRK-NEXTDAY         PIC X(08).
      *
       01  KEY-AREA.
           03  KEY-MSTKBN                  PIC X(01).
           03  KEY-PROCKBN                 PIC X(01).
      *
       01  HEAD-AREA.
           03  HEAD-FL1                    PIC X(05).
           03  HEAD-NAME-OLD               PIC X(73).
           03  HEAD-NAME-NEW               PIC X(73).
      *
       01  MEISAI-AREA.
           03  MEISAI-NO                   PIC Z(04).
           03  MEISAI-FL1                  PIC X(01).
           03  MEISAI-SRYCD-OLD            PIC X(09).
           03  MEISAI-FL2                  PIC X(01).
           03  MEISAI-NAME-OLD             PIC X(62).
           03  MEISAI-FL3                  PIC X(01).
           03  MEISAI-SRYCD-NEW            PIC X(09).
           03  MEISAI-FL4                  PIC X(01).
           03  MEISAI-NAME-NEW             PIC X(62).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LINE-MAX              PIC 9(05)   VALUE 55.
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  TCHK-REC.
           COPY    "CPCHK.INC"     REPLACING   //CHK-//
                                   BY          //TCHK-//.
      *
       01  TCHKSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING   //CHKSND-//
                                   BY          //TCHKSND-//.
      *
       01  TCHKTRD-REC.
           COPY    "CPCHKTRD.INC"  REPLACING   //CHKTRD-//
                                   BY          //TCHKTRD-//.
      *
       01  TCHK005-REC.
           COPY    "CPCHK005.INC"  REPLACING   //CHK005-//
                                   BY          //TCHK005-//.
      *
           COPY    "CPYAKUJYO.INC" REPLACING   //YAKUJYO-//
                                   BY          //TYAKUJYO-//.
      *
       01  TPTKINKI-REC.
           COPY    "CPPTKINKI.INC" REPLACING   //PTKINKI-//
                                   BY          //TPTKINKI-//.
      *
           COPY    "CPTTNS.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *     チェックマスタ
        01  CHK-REC.
            COPY    "CPCHK.INC".
      *
        01  CHKSND-REC.
            COPY    "CPCHKSND.INC".
      *
        01  CHKTRD-REC.
            COPY    "CPCHKTRD.INC".
      *
        01  CHK005-REC.
            COPY    "CPCHK005.INC".
      *
      *     薬剤情報マスタ
            COPY    "CPYAKUJYO.INC".
      *
      *    患者禁忌薬剤
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *
      *    診療行為コード置き換えマスタ
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ****************************************************************
       PROCEDURE               DIVISION
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP   RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TTNS-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SRYYMD
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBG018"      TO  JOB-PGID
           STRING  "経過措置薬剤更新（チェックマスタ"
                                       DELIMITED   BY  SIZE
                   "・薬剤情報マスタ・患者禁忌薬剤）"
                                       DELIMITED   BY  SIZE
           INTO    JOB-SHELLMSG
           END-STRING
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    3               TO  JOB-STOPFLG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "BG01801"       TO  BG0181-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0181-TERMID
           MOVE    SPA-HOSPNUM     TO  BG0181-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BG0181-BASENAME     TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BG0181-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           MOVE    "1"             TO  WRK-ZOGENPTN
           MOVE    1               TO  WRK-ZOGEN
           MOVE    WRK-PARA-SRYYMD TO  WRK-SYMD
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-NEXTDAY
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    有効期限
      *    MOVE    WRK-SRYYMD          TO  WRK-SYMD
      *    PERFORM 800-SEIWA-HEN-SEC
      *    MOVE    WRK-EDTYMD3         TO  WRK-YUKOEDYMD-N
      *
      *    作成日編集
      *    MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
      *    PERFORM 800-SEIWA-HEN-SEC
      *    MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD-N
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-INFILE-EDIT-SEC
      *
           OPEN    INPUT   IN01-FILE
      *
           PERFORM 900-IN01-READ-SEQ-SEC
      *
           PERFORM UNTIL ( FLG-IN01 NOT =   ZERO )
                     OR  ( FLG-END  NOT =   ZERO )
      *
               MOVE    IN01-MSTKBN     TO  KEY-MSTKBN
               MOVE    IN01-PROCKBN    TO  KEY-PROCKBN
               MOVE    ZERO            TO  CNT-SEQ
      *
               PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                        OR   ( FLG-END     NOT =   ZERO )
                        OR   ( KEY-MSTKBN  NOT =   IN01-MSTKBN )
                        OR   ( KEY-PROCKBN NOT =   IN01-PROCKBN )
      *
      *            ヘッダー編集処理
                   PERFORM 2001-HEAD-HEN-SEC
      *
      *            明細行編集処理
                   PERFORM 2001-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM 2001-PRINT-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           CLOSE    IN01-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ファイル編集処理
      *****************************************************************
       2001-INFILE-EDIT-SEC            SECTION.
      *
           OPEN    OUTPUT  IN01-FILE
           CLOSE   IN01-FILE
      *
           OPEN    I-O     IN01-FILE
      *
           MOVE    WRK-PARA-SRYYMD TO  WRK-SYMD
           PERFORM 900-TENSU-KEY26-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   CONTINUE
               ELSE
      *
                   PERFORM 20011-CHKMST-SRYCD-EDIT-SEC
      *
                   IF   ( FLG-END          =   ZERO )
                       PERFORM 20011-CHKMST-TEKCD-EDIT-SEC
                   END-IF
      *
                   IF   ( FLG-END          =   ZERO )
                       PERFORM 20011-CHKSND-SRYCD-EDIT-SEC
                   END-IF
      *
                   IF   ( FLG-END          =   ZERO )
                       PERFORM 20011-CHKSND-TEKCD-EDIT-SEC
                   END-IF
      *
                   IF   ( FLG-END          =   ZERO )
                       PERFORM 20011-CHKTRD-SRYCD-EDIT-SEC
                   END-IF
      *
                   IF   ( FLG-END          =   ZERO )
                       PERFORM 20011-CHKTRD-TEKCD-EDIT-SEC
                   END-IF
      *
                   IF   ( FLG-END          =   ZERO )
                       PERFORM 20011-CHK005-TEKCD-EDIT-SEC
                   END-IF
      *
                   IF    ( FLG-END         =   ZERO )
                       PERFORM 20011-YAKUJYO-EDIT-SEC
                   END-IF
      *
                   IF    ( FLG-END         =   ZERO )
                       PERFORM 20011-PTKINKI-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-TENSU-KEY26-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key26"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   IN01-FILE
      *
           .
       2001-INFILE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理（チェックマスタ）
      *****************************************************************
       20011-CHKMST-SRYCD-EDIT-SEC     SECTION.
      *
           PERFORM 900-CHKMST-KEY18-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG        =   ZERO )
                   PERFORM 900-CHKMST-KEY6-SEL-SEC
                   IF    ( FLG-TCHKMST     =   ZERO )
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   ELSE
                       PERFORM 200111-CHKMST-SRYCD-EDIT-SEC
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "0"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               ELSE
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
               END-IF
      *
               PERFORM 900-CHKMST-KEY18-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk_user"  TO  MCP-TABLE
           MOVE    "key18"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHKMST-SRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理（チェックマスタ）
      *****************************************************************
       200111-CHKMST-SRYCD-EDIT-SEC     SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  CHK-CREYMD
                                       CHK-UPYMD
           MOVE    SMCNDATE-HMS    TO  CHK-UPHMS
           MOVE    CHK-SRYCD       TO  CHK-UPSRYCD
           MOVE    CHK-YUKOSTYMD   TO  CHK-UPYUKOSTYMD
           MOVE    CHK-YUKOEDYMD   TO  CHK-UPYUKOEDYMD
           MOVE    CHG-RJNSRYCD    TO  CHK-SRYCD
           MOVE    "00000000"      TO  CHK-YUKOSTYMD
           MOVE    "99999999"      TO  CHK-YUKOEDYMD
           MOVE    CHK-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk_user"  TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHKMST-SRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ更新処理（適用コード）
      *****************************************************************
       20011-CHKMST-TEKCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHKMST-KEY19-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    CHK-SRYCD       TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   MOVE    TNS-SRYCD       TO  WRK-SRYCD
                   PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                   IF    ( FLG-SRYCDCHG        =   ZERO )
      *
                       PERFORM 900-CHKMST-KEY-SEL-SEC
                       IF    ( FLG-TCHKMST     =   ZERO )
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       ELSE
                           PERFORM 200111-CHKMST-TEKCD-EDIT-SEC
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "0"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       END-IF
                   ELSE
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-CHKMST-KEY19-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk_user"  TO  MCP-TABLE
           MOVE    "key19"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHKMST-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理（チェックマスタ）
      *****************************************************************
       200111-CHKMST-TEKCD-EDIT-SEC    SECTION.
      *
           PERFORM 900-CHKMST-KEY20-SEL-SEC
           MOVE    SMCNDATE-YMD    TO  CHK-CREYMD
                                       CHK-UPYMD
           MOVE    SMCNDATE-HMS    TO  CHK-UPHMS
           MOVE    CHG-RJNSRYCD    TO  CHK-CD
           COMPUTE CHK-RENNUM      =   TCHK-RENNUM +   1
           MOVE    CHK-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk_user"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHKMST-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２更新処理（診療行為コード）
      *****************************************************************
       20011-CHKSND-SRYCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHKSND-KEY19-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG        =   ZERO )
                   PERFORM 900-CHKSND-KEY30-SEL-SEC
                   IF    ( FLG-TCHKSND             =   ZERO )
                       PERFORM 900-CHKSND-KEY6-SEL-SEC
      *電話でのお問い合わせ４月１日−チェックマスタ経過措置薬剤更新
      *での不具合データ修復対応
                       IF    ( FLG-TCHKSND         =   ZERO )
                        AND  ( TCHKSND-UTAGAIKBN   =   SPACE )
                           PERFORM 200111-CHKSND-SRYCD-EDIT-SEC
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "0"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       ELSE
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       END-IF
                   ELSE
                       PERFORM 200111-CHKSND-SRYCD-EDIT-SEC
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "0"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               ELSE
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
               END-IF
      *
               PERFORM 900-CHKSND-KEY19-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key19"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHKSND-SRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２更新処理（診療行為コード）
      *****************************************************************
       200111-CHKSND-SRYCD-EDIT-SEC   SECTION.
      *
           IF    ( FLG-TCHKSND             =   ZERO )
            AND  ( TCHKSND-UTAGAIKBN       =   SPACE )
               MOVE    TCHKSND-REC     TO  MCPDATA-REC
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "del11"         TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  CHKSND-CREYMD
                                           CHKSND-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHKSND-UPHMS
           MOVE    CHKSND-CHKKBN       TO  CHKSND-UPCHKSNDKBN
           MOVE    CHKSND-SRYCD        TO  CHKSND-UPSRYCD
           MOVE    CHKSND-YUKOSTYMD    TO  CHKSND-UPYUKOSTYMD
           MOVE    CHKSND-YUKOEDYMD    TO  CHKSND-UPYUKOEDYMD
           MOVE    CHG-RJNSRYCD        TO  CHKSND-SRYCD
           MOVE    "00000000"          TO  CHKSND-YUKOSTYMD
           MOVE    "99999999"          TO  CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "ins1"              TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHKSND-SRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２更新処理（適用コード）
      *****************************************************************
       20011-CHKSND-TEKCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHKSND-KEY20-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    CHKSND-SRYCD    TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
      *
                   MOVE    TNS-SRYCD       TO  WRK-SRYCD
                   PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                   IF    ( FLG-SRYCDCHG        =   ZERO )
                       PERFORM 900-CHKSND-KEY22-SEL-SEC
                       IF    ( FLG-TCHKSND     =   ZERO )
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       ELSE
                           PERFORM 200111-CHKSND-TEKCD-EDIT-SEC
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "0"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       END-IF
                   ELSE
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "0"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-CHKSND-KEY20-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key20"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHKSND-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２更新処理（適用コード）
      *****************************************************************
       200111-CHKSND-TEKCD-EDIT-SEC    SECTION.
      *
           PERFORM 900-CHKSND-KEY21-SEL-SEC
           MOVE    SMCNDATE-YMD    TO  CHKSND-CREYMD
                                       CHKSND-UPYMD
           MOVE    SMCNDATE-HMS    TO  CHKSND-UPHMS
           MOVE    CHG-RJNSRYCD    TO  CHKSND-CD
           COMPUTE CHKSND-RENNUM   =   TCHKSND-RENNUM +   1
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHKSND-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（診療行為コード）
      *****************************************************************
       20011-CHKTRD-SRYCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHKTRD-KEY19-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG        =   ZERO )
                   PERFORM 900-CHKTRD-KEY6-SEL-SEC
                   IF    ( FLG-TCHKTRD     =   ZERO )
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   ELSE
                       PERFORM 200111-CHKTRD-SRYCD-EDIT-SEC
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "0"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               ELSE
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
               END-IF
      *
               PERFORM 900-CHKTRD-KEY19-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chktrd"    TO  MCP-TABLE
           MOVE    "key19"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHKTRD-SRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（診療行為コード）
      *****************************************************************
       200111-CHKTRD-SRYCD-EDIT-SEC   SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  CHKTRD-CREYMD
                                           CHKTRD-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHKTRD-UPHMS
           MOVE    CHKTRD-CHKKBN       TO  CHKTRD-UPCHKTRDKBN
           MOVE    CHKTRD-SRYCD        TO  CHKTRD-UPSRYCD
           MOVE    CHKTRD-YUKOSTYMD    TO  CHKTRD-UPYUKOSTYMD
           MOVE    CHKTRD-YUKOEDYMD    TO  CHKTRD-UPYUKOEDYMD
           MOVE    CHG-RJNSRYCD        TO  CHKTRD-SRYCD
           MOVE    "00000000"          TO  CHKTRD-YUKOSTYMD
           MOVE    "99999999"          TO  CHKTRD-YUKOEDYMD
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "ins1"              TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHKTRD-SRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（適用コード）
      *****************************************************************
       20011-CHKTRD-TEKCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHKTRD-KEY20-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    CHKTRD-SRYCD    TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
      *
                   MOVE    TNS-SRYCD       TO  WRK-SRYCD
                   PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                   IF    ( FLG-SRYCDCHG        =   ZERO )
                       PERFORM 900-CHKTRD-KEY22-SEL-SEC
                       IF    ( FLG-TCHKTRD     =   ZERO )
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       ELSE
                           PERFORM 200111-CHKTRD-TEKCD-EDIT-SEC
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "0"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                       END-IF
                   ELSE
                           MOVE    "1"     TO  WRK-MSTKBN
                           MOVE    "1"     TO  WRK-PROCKBN
                           PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-CHKTRD-KEY20-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chktrd"    TO  MCP-TABLE
           MOVE    "key20"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHKTRD-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（適用コード）
      *****************************************************************
       200111-CHKTRD-TEKCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHKTRD-KEY21-SEL-SEC
           MOVE    SMCNDATE-YMD    TO  CHKTRD-CREYMD
                                       CHKTRD-UPYMD
           MOVE    SMCNDATE-HMS    TO  CHKTRD-UPHMS
           MOVE    CHG-RJNSRYCD    TO  CHKTRD-CD
           COMPUTE CHKTRD-RENNUM   =   TCHKTRD-RENNUM +   1
           MOVE    CHKTRD-REC      TO  MCPDATA-REC
           MOVE    "tbl_chktrd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHKTRD-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５更新処理（適用コード）
      *****************************************************************
       20011-CHK005-TEKCD-EDIT-SEC   SECTION.
      *
           PERFORM 900-CHK005-KEY8-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG        =   ZERO )
                   PERFORM 900-CHK005-KEY9-SEL-SEC
                   IF    ( FLG-TCHK005     =   ZERO )
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   ELSE
                       PERFORM 200111-CHK005-TEKCD-EDIT-SEC
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "0"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               ELSE
                       MOVE    "1"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
               END-IF
      *
               PERFORM 900-CHK005-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk005"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-CHK005-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（適用コード）
      *****************************************************************
       200111-CHK005-TEKCD-EDIT-SEC    SECTION.
      *
           PERFORM 900-CHK005-KEY10-SEL-SEC
           MOVE    SMCNDATE-YMD    TO  CHK005-CREYMD
                                       CHK005-UPYMD
           MOVE    SMCNDATE-HMS    TO  CHK005-UPHMS
           MOVE    CHG-RJNSRYCD    TO  CHK005-SRYCD
           COMPUTE CHK005-RENNUM   =   TCHK005-RENNUM +   1
           MOVE    CHK005-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk005"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "チェックマスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-CHK005-TEKCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ更新処理
      *****************************************************************
       20011-YAKUJYO-EDIT-SEC      SECTION.
      *
           PERFORM 900-YAKUJYO-KEY-SEL-SEC
           IF    ( FLG-YAKUJYO     =   ZERO )
      *
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG        =   ZERO )
                   PERFORM 901-YAKUJYO-KEY-SEL-SEC
                   IF    ( FLG-TYAKUJYO    =   ZERO )
                       MOVE    "2"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   ELSE
                       PERFORM 200111-YAKUJYO-EDIT-SEC
                       MOVE    "2"     TO  WRK-MSTKBN
                       MOVE    "0"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               ELSE
                       MOVE    "2"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
               END-IF
           END-IF
      *
           .
       20011-YAKUJYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ更新処理
      *****************************************************************
       200111-YAKUJYO-EDIT-SEC     SECTION.
      *
           MOVE    CHG-RJNSRYCD   TO  YAKUJYO-SRYCD
           MOVE    SPACE           TO  YAKUJYO-NAME
                                       YAKUJYO-TERMID
                                       YAKUJYO-OPID
           MOVE    SMCNDATE-YMD    TO  YAKUJYO-CREYMD
                                       YAKUJYO-UPYMD
           MOVE    SMCNDATE-HMS    TO  YAKUJYO-UPHMS
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "薬剤情報マスタの更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-YAKUJYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤更新処理
      *****************************************************************
       20011-PTKINKI-EDIT-SEC      SECTION.
      *
           PERFORM 900-PTKINKI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTKINKI NOT =   ZERO )
      *
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
               IF    ( FLG-SRYCDCHG        =   ZERO )
                   PERFORM 901-PTKINKI-KEY-SEL-SEC
                   IF    ( FLG-TPTKINKI    =   ZERO )
                       MOVE    "3"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   ELSE
                       PERFORM 200111-PTKINKI-EDIT-SEC
                       MOVE    "3"     TO  WRK-MSTKBN
                       MOVE    "0"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
                   END-IF
               ELSE
                       MOVE    "3"     TO  WRK-MSTKBN
                       MOVE    "1"     TO  WRK-PROCKBN
                       PERFORM 2001-IN01-EDIT-SEC
               END-IF
      *
               PERFORM 900-PTKINKI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-PTKINKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤更新処理
      *****************************************************************
       200111-PTKINKI-EDIT-SEC     SECTION.
      *
           MOVE    CHG-RJNSRYCD    TO  PTKINKI-SRYCD
           MOVE    SPACE           TO  PTKINKI-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  PTKINKI-OPID
           MOVE    SMCNDATE-YMD    TO  PTKINKI-CREYMD
                                       PTKINKI-UPYMD
           MOVE    SMCNDATE-HMS    TO  PTKINKI-UPHMS
           MOVE    PTKINKI-REC     TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "患者禁忌薬剤の更新に失敗しました"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       200111-PTKINKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル編集処理
      *****************************************************************
       2001-IN01-EDIT-SEC              SECTION.
      *
           MOVE    WRK-MSTKBN      TO  IN01-MSTKBN
           MOVE    ZERO            TO  IN01-PROCKBN
           MOVE    TNS-KANANAME    TO  IN01-KANANAME
           MOVE    WRK-SRYCD       TO  IN01-SRYCD-OLD
           PERFORM 900-IN01-READ-RND-SEC
           IF    ( FLG-IN01        =   ZERO )
               CONTINUE
           ELSE
               MOVE    WRK-MSTKBN  TO  IN01-MSTKBN
               MOVE    "1"         TO  IN01-PROCKBN
               MOVE    TNS-KANANAME TO IN01-KANANAME
               MOVE    WRK-SRYCD   TO  IN01-SRYCD-OLD
               PERFORM 900-IN01-READ-RND-SEC
               IF    ( FLG-IN01        =   ZERO )
                   IF    ( WRK-PROCKBN =   ZERO )
                       INITIALIZE                  IN01-REC
                       MOVE    WRK-MSTKBN      TO  IN01-MSTKBN
                       MOVE    "1"             TO  IN01-PROCKBN
                       MOVE    TNS-KANANAME    TO  IN01-KANANAME
                       MOVE    WRK-SRYCD       TO  IN01-SRYCD-OLD
                       MOVE    CHG-RJNSRYCD    TO  IN01-SRYCD-NEW
                       MOVE    1               TO  IN01-DELFLG
                       REWRITE IN01-REC
      *
                       INITIALIZE                  IN01-REC
                       MOVE    WRK-MSTKBN      TO  IN01-MSTKBN
                       MOVE    WRK-PROCKBN     TO  IN01-PROCKBN
                       MOVE    TNS-KANANAME    TO  IN01-KANANAME
                       MOVE    WRK-SRYCD       TO  IN01-SRYCD-OLD
                       MOVE    CHG-RJNSRYCD    TO  IN01-SRYCD-NEW
                       WRITE   IN01-REC
                   END-IF
               ELSE
                   INITIALIZE                  IN01-REC
                   MOVE    WRK-MSTKBN      TO  IN01-MSTKBN
                   MOVE    WRK-PROCKBN     TO  IN01-PROCKBN
                   MOVE    TNS-KANANAME    TO  IN01-KANANAME
                   MOVE    WRK-SRYCD       TO  IN01-SRYCD-OLD
                   MOVE    CHG-RJNSRYCD    TO  IN01-SRYCD-NEW
                   WRITE   IN01-REC
               END-IF
           END-IF
      *
           .
       2001-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       2001-HEAD-HEN-SEC               SECTION.
      *
           COMPUTE CNT-PAGE            =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  WRK-ZZZZ9
      *
           INITIALIZE                  HCMSL55
      *
           MOVE    SPACE               TO   WRK-MSTMSG
           EVALUATE    IN01-MSTKBN
           WHEN    "1"
               MOVE    "チェックマスタ" TO  WRK-MSTMSG
           WHEN    "2"
               MOVE    "薬剤情報マスタ" TO  WRK-MSTMSG
           WHEN    "3"
               MOVE    "患者禁忌薬剤"   TO  WRK-MSTMSG
           END-EVALUATE
      *
           STRING
               "＊＊　"                            DELIMITED   BY  SIZE
               WRK-MSTMSG                          DELIMITED   BY  SPACE
                "経過措置薬剤更新リスト"           DELIMITED   BY  SIZE
               "　＊＊　　"                        DELIMITED   BY  SIZE
               "有効期限："                        DELIMITED   BY  SIZE
               WRK-PARA-SRYYMD (1:4)               DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               WRK-PARA-SRYYMD (5:2)               DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               WRK-PARA-SRYYMD (7:2)               DELIMITED   BY  SIZE
               "　　　　　　　　　　　　　　　　"  DELIMITED   BY  SIZE
               "（"                                DELIMITED   BY  SIZE
               "作成日："                          DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(1:4)            DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(5:2)            DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(7:2)            DELIMITED   BY  SIZE
               "）"                                DELIMITED   BY  SIZE
               "PAGE "                             DELIMITED   BY  SIZE
               WRK-ZZZZ9-G                         DELIMITED   BY  SIZE
               INTO    HCMSL55-MOJI (1)
           END-STRING
      *
           IF    ( IN01-PROCKBN    =   ZERO )
               STRING
                   "経過措置薬剤より"              DELIMITED   BY  SIZE
                   WRK-MSTMSG                      DELIMITED   BY  SPACE
                   "を複写しました。"              DELIMITED   BY  SIZE
               INTO    HCMSL55-MOJI (4)
               END-STRING
           ELSE
               STRING
                   "既に"                          DELIMITED BY SIZE
                   WRK-MSTMSG                      DELIMITED   BY  SPACE
                   "に登録済か、"                  DELIMITED BY SIZE
                   "複写先となる薬剤が存在しません。"
                                                   DELIMITED BY SIZE
               INTO    HCMSL55-MOJI (4)
               END-STRING
           END-IF
      *
           INITIALIZE                      HEAD-AREA
           STRING  "経過措置薬剤"      DELIMITED   BY  SIZE
           INTO    HEAD-NAME-OLD
           END-STRING
      *
           STRING  "複写先薬剤"        DELIMITED   BY  SIZE
           INTO    HEAD-NAME-NEW
           END-STRING
           MOVE    HEAD-AREA           TO  HCMSL55-MOJI (5)
           MOVE    ALL "-"             TO  HCMSL55-MOJI (6)
      *
           MOVE    6                   TO  CNT-LINE
      *
           .
      *
       2001-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       2001-BODY-HEN-SEC               SECTION.
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
                    OR   ( KEY-MSTKBN  NOT =   IN01-MSTKBN )
                    OR   ( KEY-PROCKBN NOT =   IN01-PROCKBN )
                    OR   ( CNT-LINE       >=   CONST-LINE-MAX )
      *
               PERFORM 20011-BODY-NAIYO-HEN-SEC
      *
               PERFORM 900-IN01-READ-SEQ-SEC
      *
           END-PERFORM
      *
           .
       2001-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20011-BODY-NAIYO-HEN-SEC        SECTION.
      *
           COMPUTE CNT-SEQ     =   CNT-SEQ     +   1
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           INITIALIZE                      MEISAI-AREA
      *
           MOVE    CNT-SEQ             TO  MEISAI-NO
      *
           MOVE    IN01-SRYCD-OLD      TO  MEISAI-SRYCD-OLD
           MOVE    IN01-SRYCD-OLD      TO  WRK-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-NAME            TO  MEISAI-NAME-OLD
      *
           IF    ( IN01-SRYCD-NEW      =   SPACE )
               MOVE    "なし"
                                           TO  MEISAI-SRYCD-NEW
           ELSE
               MOVE    IN01-SRYCD-NEW      TO  MEISAI-SRYCD-NEW
               MOVE    IN01-SRYCD-NEW      TO  WRK-SRYCD
               MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-NAME        TO  MEISAI-NAME-NEW
               ELSE
                   PERFORM 900-TENSU-KEY24-SEL-SEC
                   MOVE    TNS-NAME        TO  MEISAI-NAME-NEW
               END-IF
           END-IF
      *
           INSPECT MEISAI-NAME-OLD REPLACING  ALL "  "  BY  "　"
           INSPECT MEISAI-NAME-NEW REPLACING  ALL "  "  BY  "　"
      *
           MOVE    MEISAI-AREA         TO  HCMSL55-MOJI (CNT-LINE)
      *
           .
       20011-BODY-NAIYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    SPACE           TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"   TO  SPRT-PRTID
           MOVE    "経過措置薬剤更新"
                                   TO  SPRT-TITLE
           MOVE    HCMSL55         TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       2001-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
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
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BG0181          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           END-IF
      *
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    経過措置薬剤情報読込み処理
      *****************************************************************
       900-IN01-READ-RND-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE
           INVALID
               MOVE    1           TO  FLG-IN01
           END-READ
      *
           .
       900-IN01-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    経過措置薬剤情報読込み処理
      *****************************************************************
       900-IN01-READ-SEQ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    IN01-FILE           NEXT
               AT  END
                   MOVE    1       TO  FLG-IN01
               NOT AT END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( IN01-DELFLG     =   1 )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               END-READ
           END-PERFORM
      *
           .
       900-IN01-READ-SEQ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY24-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM TO TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY24-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード置換マスタ検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYCDCHG
      *
           INITIALIZE                  SRYCDCHG-REC
           MOVE    WRK-PARA-HOSPNUM TO CHG-HOSPNUM
           MOVE    WRK-SRYCD        TO  CHG-IPNSRYCD
      *
           MOVE    SRYCDCHG-REC    TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYCDCHG-REC
           ELSE
               INITIALIZE              SRYCDCHG-REC
               MOVE    1           TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM TO TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SYMD        TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理 存在チェックのみ（結果を取得しない）
      *****************************************************************
       901-TENSU-KEY-SEL-SEC      SECTION.
      *
           MOVE    TNS-TENSU-REC   TO  TTNS-DAT (1)
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM TO TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SYMD        TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    TTNS-DAT (1)    TO  TNS-TENSU-REC
      *
           .
       901-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY26-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM TO TNS-HOSPNUM
           MOVE    WRK-SYMD        TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key26"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-TENSU-KEY26-FET-SEC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタFETCH処理
      *****************************************************************
       900-TENSU-KEY26-FET-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key26"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( TNS-SRYCD (1:1) NOT =   "6" )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                    INITIALIZE              TNS-TENSU-REC
                    MOVE    1           TO  FLG-TENSU
               END-IF
           END-PERFORM
      *
           .
       900-TENSU-KEY26-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY18-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    TNS-SRYCD   TO  CHK-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY18-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY18-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY18-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    CHK-REC             TO  TCHK-REC
           MOVE    CHG-RJNSRYCD        TO  TCHK-SRYCD
      *
           MOVE    TCHK-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHK-REC
           ELSE
               MOVE    1           TO  FLG-TCHKMST
               INITIALIZE              TCHK-REC
           END-IF
      *
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKMST-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY19-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    TNS-SRYCD   TO  CHK-CD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKMST-KEY19-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY19-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chk_user"      TO  MCP-TABLE
               MOVE    "key19"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHK-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHK-CDKBN       =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHKMST
                   INITIALIZE                  CHK-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHKMST-KEY19-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    CHK-REC             TO  TCHK-REC
           MOVE    CHG-RJNSRYCD        TO  TCHK-CD
      *
           MOVE    TCHK-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHK-REC
           ELSE
               MOVE    1           TO  FLG-TCHKMST
               INITIALIZE              TCHK-REC
           END-IF
      *
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKMST-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    CHK-REC             TO  TCHK-REC
      *
           MOVE    TCHK-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHK-REC
           ELSE
               MOVE    1           TO  FLG-TCHKMST
               INITIALIZE              TCHK-REC
           END-IF
      *
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKMST-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY19-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    TNS-SRYCD   TO  CHKSND-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY19-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY19-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY30-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    CHKSND-REC          TO  TCHKSND-REC
           MOVE    CHG-RJNSRYCD        TO  TCHKSND-SRYCD
           MOVE    "0"                 TO  TCHKSND-CDKBN
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKSND-REC
           ELSE
               MOVE    1           TO  FLG-TCHKSND
               INITIALIZE              TCHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY30-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKSND-REC
           ELSE
               MOVE    1           TO  FLG-TCHKSND
               INITIALIZE              TCHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    TNS-SRYCD   TO  CHKSND-CD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKSND-KEY20-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY20-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chksnd"        TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHKSND-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHKSND-CDKBN    =   "3" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHKSND
                   INITIALIZE                  CHKSND-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHKSND-KEY20-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY22-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    CHKSND-REC          TO  TCHKSND-REC
           MOVE    CHG-RJNSRYCD        TO  TCHKSND-CD
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKSND-REC
           ELSE
               MOVE    1           TO  FLG-TCHKSND
               INITIALIZE              TCHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY22-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY21-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    CHKSND-REC          TO  TCHKSND-REC
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKSND-REC
           ELSE
               MOVE    1           TO  FLG-TCHKSND
               INITIALIZE              TCHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY19-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    TNS-SRYCD           TO  CHKTRD-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY19-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY19-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    CHKTRD-REC          TO  TCHKTRD-REC
           MOVE    CHG-RJNSRYCD        TO  TCHKTRD-SRYCD
      *
           MOVE    TCHKTRD-REC         TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKTRD-REC
           ELSE
               MOVE    1           TO  FLG-TCHKTRD
               INITIALIZE              TCHKTRD-REC
           END-IF
      *
           MOVE    "tbl_chktrd"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKTRD-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    TNS-SRYCD   TO  CHKTRD-CD
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKTRD-KEY20-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY20-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chktrd"        TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHKTRD-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHKTRD-CDKBN    =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHKTRD
                   INITIALIZE                  CHKTRD-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHKTRD-KEY20-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY22-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    CHKTRD-REC          TO  TCHKTRD-REC
           MOVE    CHG-RJNSRYCD        TO  TCHKTRD-CD
      *
           MOVE    TCHKTRD-REC         TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKTRD-REC
           ELSE
               MOVE    1           TO  FLG-TCHKTRD
               INITIALIZE              TCHKTRD-REC
           END-IF
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKTRD-KEY22-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY21-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    CHKTRD-REC          TO  TCHKTRD-REC
      *
           MOVE    TCHKTRD-REC         TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHKTRD-REC
           ELSE
               MOVE    1           TO  FLG-TCHKTRD
               INITIALIZE              TCHKTRD-REC
           END-IF
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKTRD-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY8-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
           INITIALIZE                      CHK005-REC
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    TNS-SRYCD           TO  CHK005-SRYCD
           MOVE    WRK-PARA-SRYYMD     TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHK005-KEY8-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           .
       900-CHK005-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY8-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chk005"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHK005-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHK005-CDKBN    =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHK005
                   INITIALIZE                  CHK005-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHK005-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY9-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHK005
      *
           MOVE    CHK005-REC          TO  TCHK005-REC
           MOVE    CHG-RJNSRYCD        TO  TCHK005-SRYCD
      *
           MOVE    TCHK005-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHK005-REC
           ELSE
               MOVE    1           TO  FLG-TCHK005
               INITIALIZE              TCHK005-REC
           END-IF
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK005-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY10-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHK005
      *
           MOVE    CHK005-REC          TO  TCHK005-REC
      *
           MOVE    TCHK005-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TCHK005-REC
           ELSE
               MOVE    1           TO  FLG-TCHK005
               INITIALIZE              TCHK005-REC
           END-IF
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK005-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       900-YAKUJYO-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUJYO
      *
           INITIALIZE                      YAKUJYO-REC
           MOVE    SPA-HOSPNUM         TO  YAKUJYO-HOSPNUM
           MOVE    TNS-SRYCD           TO  YAKUJYO-SRYCD
           MOVE    1                   TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  YAKUJYO-REC
           ELSE
               MOVE    1           TO  FLG-YAKUJYO
               INITIALIZE              YAKUJYO-REC
           END-IF
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-YAKUJYO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       901-YAKUJYO-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TYAKUJYO
      *
           MOVE    YAKUJYO-REC         TO  TYAKUJYO-REC
           MOVE    CHG-RJNSRYCD        TO  TYAKUJYO-SRYCD
           MOVE    TYAKUJYO-REC        TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TYAKUJYO-REC
           ELSE
               MOVE    1           TO  FLG-TYAKUJYO
               INITIALIZE              TYAKUJYO-REC
           END-IF
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-YAKUJYO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤検索処理
      *****************************************************************
       900-PTKINKI-KEY4-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKINKI
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    TNS-SRYCD           TO  PTKINKI-SRYCD
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  PTKINKI-REC
           ELSE
               MOVE    1               TO  FLG-PTKINKI
               INITIALIZE                  PTKINKI-REC
           END-IF
      *
           .
       900-PTKINKI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤FETCH処理
      *****************************************************************
       900-PTKINKI-KEY4-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKINKI
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  PTKINKI-REC
           ELSE
               MOVE    1               TO  FLG-PTKINKI
               INITIALIZE                  PTKINKI-REC
           END-IF
      *
           .
       900-PTKINKI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤検索処理
      *****************************************************************
       901-PTKINKI-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TPTKINKI
      *
           MOVE    PTKINKI-REC         TO  TPTKINKI-REC
           MOVE    CHG-RJNSRYCD        TO  TPTKINKI-SRYCD
           MOVE    TPTKINKI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  TPTKINKI-REC
           ELSE
               MOVE    1           TO  FLG-TPTKINKI
               INITIALIZE              TPTKINKI-REC
           END-IF
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-PTKINKI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC            SECTION.
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
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
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA

           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
