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
       PROGRAM-ID.                 ORCGXG03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 統計ＣＳＶ出力
      *  コンポーネント名  : 電子お薬手帳送信画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/11/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/11/21  ＱＲコードver.2.1対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    CSVデータファイル
           SELECT  CSVDATA-FILE    ASSIGN   ASS-CSVDATA
                                   ORGANIZATION    IS  LINE SEQUENTIAL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  CSVDATA-FILE.
       01  CSVDATA-REC         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //XG03ERR//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    再印刷処理共通パラメタ
           COPY    "XG01COMMON-SPA".
      *
       01  SPA-XG03.
           03  SPA-XG03-AREA.
               05  SPA-GMN-PAGE            PIC 9(05).
               05  SPA-GMN-MAX             PIC 9(03).
               05  SPA-GMN-CUR             PIC 9(03).
               05  SPA-NAI-TODAY           PIC 9(08).
               05  SPA-GMN-TODAY           PIC X(09).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-MLIST-NEXT          PIC 9(01).
                   07  SPA-GMN-MLIST-AREA.
                       09  SPA-GMN-MLIST-OCC       OCCURS  200.
                           11  SPA-GMN-TNO         PIC 9(05).
                           11  SPA-GMN-TPTNUM      PIC X(20).
                           11  SPA-GMN-TPTNAME     PIC X(50).
                           11  SPA-GMN-TTITLE      PIC X(50).
                           11  SPA-GMN-TDATA-VERSION
                                                   PIC X(10).
                           11  SPA-GMN-TSRYYMD     PIC X(09).
                           11  SPA-GMN-TLDNAME     PIC X(20).
                           11  SPA-GMN-TDRNAME     PIC X(50).
                           11  SPA-GMN-TCREYMD     PIC X(9).
                           11  SPA-GMN-TCREHMS     PIC X(8).
                           11  SPA-GMN-TSTATUS     PIC X(6).
      *
                   07  SPA-GMN-BODY-AREA.
                       09  SPA-GMN-BODY1.
                           11  SPA-GMN-SELNUM      PIC 9(03).
                           11  SPA-GMN-CREYMD      PIC X(10).
                           11  SPA-GMN-PTNUM       PIC X(20).
                           11  SPA-GMN-NAME        PIC X(50).
                       09  SPA-GMN-BODY2.
                           11  SPA-GMN-IPTNUM      PIC X(20).
                           11  SPA-GMN-IPTNAME     PIC X(50).
                           11  SPA-GMN-ITITLE      PIC X(50).
                           11  SPA-GMN-IDATA-VERSION
                                                   PIC X(10).
                           11  SPA-GMN-ISRYYMD     PIC X(09).
                           11  SPA-GMN-ILDNAME     PIC X(20).
                           11  SPA-GMN-IDRNAME     PIC X(50).
                           11  SPA-GMN-ICREYMD     PIC X(9).
                           11  SPA-GMN-ICREHMS     PIC X(8).
                           11  SPA-GMN-IOPID       PIC X(16).
                           11  SPA-GMN-ISTATUS     PIC X(6).
                           11  SPA-GMN-CONTEXT     PIC X(50000).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-ICCARDRW            PIC X(01).
                   07  SPA-NAI-CLIENT-SAVE         PIC X(01).
                   07  SPA-NAI-TBL-KEY             PIC X(08).
      *
                   07  SPA-NAI-MLIST-AREA.
                       09  SPA-NAI-MLIST-OCC       OCCURS  200.
                           11  SPA-NAI-TTBL-KEY    PIC X(08).
                           11  SPA-NAI-TTBL-GROUP  PIC X(14).
                           11  SPA-NAI-TTBL-UUID   PIC X(36).
                           11  SPA-NAI-TRENNUM     PIC 9(04).
                           11  SPA-NAI-TSHORI-RENNUM PIC 9(04).
                           11  SPA-NAI-TPTID       PIC 9(10).
                           11  SPA-NAI-TSRYYMD     PIC X(08).
                           11  SPA-NAI-TOPID       PIC X(16).
                           11  SPA-NAI-TCNT        PIC 9(05).
                           11  SPA-NAI-TSTATUS     PIC X(1).
      *
                   07  SPA-NAI-BODY-AREA.
                       09  SPA-NAI-BODY1.
                           11  SPA-NAI-COLLECT     PIC X(01).
                           11  SPA-NAI-SELNUM      PIC 9(03).
                           11  SPA-NAI-CREYMD      PIC X(08).
                           11  SPA-NAI-PTID        PIC 9(10).
                       09  SPA-NAI-BODY2.
                           11  SPA-NAI-IPTID       PIC 9(10).
                           11  SPA-NAI-ITBL-KEY    PIC X(08).
                           11  SPA-NAI-ITBL-GROUP  PIC X(14).
                           11  SPA-NAI-ITBL-UUID   PIC X(36).
                           11  SPA-NAI-IRENNUM     PIC 9(04).
                           11  SPA-NAI-ISHORI-RENNUM PIC 9(04).
                           11  SPA-NAI-ICNT        PIC 9(05).
                           11  SPA-NAI-ISTATUS     PIC X(1).
                           11  SPA-NAI-OVER-FLOW   PIC X(1).
      *
      *
           COPY    "ENUM-VALUE".
           COPY    "CMJOBNAME.INC".
           COPY    "CPLOCKCONST.INC".
      *
           COPY    "CPASGNPARA.INC"        REPLACING   //ASGNPARA//
                                           BY          //ASS-CSVDATA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-CSVDATA         PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MNOTE-H        PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-MNOTE-H         PIC 9(01).
           03  FLG-MNOTE-B         PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-NEXT            PIC 9(01).
           03  FLG-SENDDATA        PIC 9(01).
           03  FLG-SENT            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RESET           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDXL                PIC 9(05).
           03  IDXN                PIC 9(05).
           03  IDXP                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TBL-GROUP       PIC X(14).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-XGIDMSG         PIC X(70).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-CNT             PIC ZZZZ9.
           03  WRK-SELNUM          PIC S9(05).
      *
           03  WRK-ZZ              PIC ZZZZ9.
           03  WRK-YMD             PIC X(10).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
      *
           03  WRK-START           PIC 9(05).
      *  
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-YMD1.
               05  WRK-YY1         PIC 9(04).
               05  WRK-MM1         PIC 9(02).
               05  WRK-DD1         PIC 9(02).
      *
           03  WRK-YMD2.
               05  WRK-YY2         PIC 9(04).
               05  WRK-MM2         PIC 9(02).
               05  WRK-DD2         PIC 9(02).
           03  WRK-AGE-G.
               05  WRK-AGE         PIC 9(03).
           03  WRK-PTID            PIC 9(10).
           03  WRK-XG03-ROW        PIC 9(05).
           03  WRK-NAME            PIC X(50).
           03  WRK-HOST-G.
               05  WRK-HOST        PIC X(40).
               05  WRK-PORT        PIC X(05).
           03  WRK-AGE-X           PIC X(03).
           03  WRK-OBJECTDATA      PIC X(36).
           03  WRK-DL-FILENAME     PIC X(1024).
           03  WRK-DEL-FILENAME    PIC X(1024).
           03  WRK-MAX-TEXTSIZE    PIC 9(8).
           03  WRK-MAX-DATASIZE    PIC 9(8).
           03  WRK-SIZE            PIC 9(8).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-ROW.
           05  WRK-LIST-ROW        PIC S9(9)   BINARY.
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  CONST-AREA.
           03  CONST-TBL-MAX    PIC 9(03)   VALUE   200.
           03  CONST-TBL-KEY    PIC X(08)   VALUE   "MNOTE".
           03  CONST-TBL-KEY2   PIC X(08)   VALUE   "JMAMR".
      *
       01  TMNOTE-B-REC.
           COPY    "CPMNOTE-B.INC"      REPLACING    //MNOTE-//
                                        BY           //TMNOTE-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
           COPY    "CPSK1050.INC".
      *
      *    統計ＣＳＶ格納管理
       01  MNOTE-H-REC.
           COPY    "CPMNOTE-H.INC".
      *
      *    統計ＣＳＶ格納明細
       01  MNOTE-B-REC.
           COPY    "CPMNOTE-B.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
       01  UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
           COPY    "ORCA-BLOB".
      *
      *    文字コード変換
           COPY    "CPORCSFILEENCODE.INC".
      *
      *    クライアント保存可否情報取得サブ
           COPY    "CPORCSCLIENTSV.INC".
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
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA". 
      *
       01  SCRAREA.
           COPY    "ORCA07SCRAREA.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XG03
           MOVE    SPA-WORK        TO  SPA-XG01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-XG01KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XG03        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG         =   "XGERR" )
               MOVE    SPACE          TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END        =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "XG03"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF    ( SPA-MOTOPG          =   "XGID1" )
               CONTINUE
           ELSE
               IF    ( LINKAREA        NOT =   SPACE )
                   MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
                   MOVE    SPA-MOTOPG  TO  WRK-MOTOPG
                   MOVE    SPA-MOTOPG  TO  SPA-MOTOPG2
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "XGID1"
               PERFORM 330-XGID1-SET-SEC
           WHEN    OTHER
      *
               PERFORM 310-SPASET-SEC
      *
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-XG03
                                       SPA-XG01KYOUTU
      *
      *    更新日取得
           PERFORM 800-ORCSMCNDATE-SEC
      *
           MOVE    CONST-TBL-KEY   TO  SPA-NAI-TBL-KEY
      *
           MOVE    SMCNDATE-YMD    TO  SPA-NAI-TODAY
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD 
           PERFORM 800-HIZUKE-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-TODAY
      *
          MOVE    SPA-NAI-TODAY    TO  SPA-NAI-CREYMD
          MOVE    SPA-GMN-TODAY    TO  SPA-GMN-CREYMD
      *
          MOVE    1                TO  SPA-GMN-PAGE
      *
      *    クライアント保存可否情報取得サブ
           INITIALIZE              ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"  USING
                                   ORCSCLIENTSVAREA
                                   SPA-AREA
           MOVE    SCLIENTSV-CLIENT-SAVE
                               TO  SPA-NAI-CLIENT-SAVE
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1050"          TO  SYS-KANRICD
           MOVE    SPA-STAFFCD     TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1050-REC
           MOVE    SYS-1050-ICCARDRW
                                   TO  SPA-NAI-ICCARDRW
      *
           MOVE    1               TO  FLG-LIST-ROW
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           PERFORM 3101-MLIST-EDIT-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コラムリスト編集処理
      *****************************************************************
       3101-MLIST-EDIT-SEC         SECTION.
      *
           INITIALIZE                      SPA-GMN-MLIST-AREA
                                           SPA-NAI-MLIST-AREA
      *
           PERFORM 900-MNOTE-H-KEY8-SEL-SEC
      *
           COMPUTE WRK-START       = ( SPA-GMN-PAGE  -  1 )
                                   *   CONST-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDXL
                                           IDXN
                                           SPA-GMN-MAX
      *
           PERFORM UNTIL ( IDXL           >=   CONST-TBL-MAX )
                   OR    ( FLG-MNOTE-H =   1   )
      *
               COMPUTE IDXN    =   IDXN    +   1
      *
               IF    ( IDXN   >=  WRK-START )
      *
                   COMPUTE IDXL    =   IDXL    +   1
      *
                   IF    ( IDXL   <=  CONST-TBL-MAX )
                       PERFORM 31011-MLIST-EDIT-SEC
                   END-IF
      *
               END-IF
      *
               PERFORM 900-MNOTE-H-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_mnote_h"   TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXL               >=   CONST-TBL-MAX )
            AND  ( FLG-MNOTE-H     =   ZERO )
               MOVE    1               TO  SPA-GMN-MLIST-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-MLIST-NEXT
           END-IF
      *
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       3101-MLIST-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    コラムリスト編集処理
      *****************************************************************
       31011-MLIST-EDIT-SEC       SECTION.
      *
           MOVE    MNOTE-H-PTID    TO  WRK-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1010"              TO  SYS-KANRICD
           MOVE    MNOTE-H-DRCD        TO  SYS-KBNCD
           MOVE    MNOTE-H-SRYYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1010-REC
      *
           MOVE    IDXN                TO  SPA-GMN-TNO       (IDXL)
           MOVE    PTNUM-PTNUM         TO  SPA-GMN-TPTNUM    (IDXL)
           MOVE    PTINF-NAME          TO  SPA-GMN-TPTNAME   (IDXL)
           MOVE    MNOTE-H-TITLE       TO  SPA-GMN-TTITLE    (IDXL)
           EVALUATE    MNOTE-H-DATA-VERSION
           WHEN    "1"
               MOVE    "1.0"           TO  SPA-GMN-TDATA-VERSION
                                                             (IDXL)
           WHEN    "2"
               MOVE    "2.1"           TO  SPA-GMN-TDATA-VERSION
                                                             (IDXL)
           END-EVALUATE
      *
           MOVE    MNOTE-H-SRYYMD      TO  SPA-NAI-TSRYYMD   (IDXL)
                                           WRK-SYMD
           PERFORM 800-HIZUKE-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TSRYYMD   (IDXL)
      *
           EVALUATE    MNOTE-H-LOCATION-CD
           WHEN    "01"
               MOVE    "診療行為"
                                       TO  SPA-GMN-TLDNAME (IDXL)
           WHEN    "02"
               MOVE    "診療行為（中途終了）"
                                       TO  SPA-GMN-TLDNAME (IDXL)
           WHEN    "03"
               MOVE    "診療行為（前回処方）"
                                       TO  SPA-GMN-TLDNAME (IDXL)
           END-EVALUATE
      *
           MOVE    SYS-1010-NAME       TO  SPA-GMN-TDRNAME  (IDXL)
           MOVE    MNOTE-H-TBL-GROUP (1:8)
                                       TO  WRK-SYMD
           PERFORM 800-HIZUKE-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TCREYMD  (IDXL)
      *
           MOVE    SPACE               TO  SPA-GMN-TCREHMS  (IDXL)
           STRING  MNOTE-H-TBL-GROUP (9:2)     DELIMITED BY SIZE
                   ":"                         DELIMITED BY SIZE
                   MNOTE-H-TBL-GROUP (11:2)    DELIMITED BY SIZE
                   ":"                         DELIMITED BY SIZE
                   MNOTE-H-TBL-GROUP (13:2)    DELIMITED BY SIZE
           INTO    SPA-GMN-TCREHMS  (IDXL)
           END-STRING
      *
           IF    ( MNOTE-H-PUTFLG     =    "1" )
               MOVE    "送信済"        TO  SPA-GMN-TSTATUS  (IDXL)
           ELSE
               MOVE    "未送信"        TO  SPA-GMN-TSTATUS  (IDXL)
           END-IF
      *
           MOVE    MNOTE-H-TBL-KEY     TO  SPA-NAI-TTBL-KEY  (IDXL)
           MOVE    MNOTE-H-TBL-GROUP   TO  SPA-NAI-TTBL-GROUP(IDXL)
           MOVE    MNOTE-H-TBL-UUID    TO  SPA-NAI-TTBL-UUID (IDXL)
           MOVE    MNOTE-H-RENNUM      TO  SPA-NAI-TRENNUM (IDXL)
           MOVE    MNOTE-H-SHORI-RENNUM
                                       TO  SPA-NAI-TSHORI-RENNUM (IDXL)
           MOVE    MNOTE-H-PTID        TO  SPA-NAI-TPTID     (IDXL)
           MOVE    MNOTE-H-OPID        TO  SPA-NAI-TOPID     (IDXL)
           MOVE    MNOTE-H-CNT         TO  SPA-NAI-TCNT      (IDXL)
           MOVE    MNOTE-H-PUTFLG      TO  SPA-NAI-TSTATUS   (IDXL)
      *
           MOVE    IDXL                TO  SPA-GMN-MAX
      *
           MOVE    1                   TO  FLG-LIST-ROW
      *
           .
       31011-MLIST-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ情報初期化処理
      *****************************************************************
       31011-MNOTE-INFO-CLEAR-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-BODY2
                                           SPA-NAI-BODY2
      *
           MOVE    ZERO            TO      SPA-GMN-SELNUM
                                           SPA-NAI-SELNUM
      *
           IF    ( SPA-NAI-COLLECT =   SPACE )
      *
               INITIALIZE                  SPA-NAI-PTID
                                           SPA-GMN-PTNUM
                                           SPA-GMN-NAME
           END-IF
      *
           .
       31011-MNOTE-INFO-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-XGID1-SET-SEC            SECTION.
      *
           IF    ( SPA-XGID1-FLG    =   "OK" )
               EVALUATE    SPA-XGIDCD
               WHEN    "3002"
               WHEN    "3003"
                   PERFORM 430-DELETE-SEC
               WHEN    "3004"
                   MOVE     1      TO  FLG-SENT
                   PERFORM 4501-CHANGE-STATUS-SEC
               WHEN    "3005"
                   MOVE     0      TO  FLG-SENT
                   PERFORM 4501-CHANGE-STATUS-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-XGID1-FLG
                                       SPA-XG99-FLG
           MOVE    SPACE           TO  SPA-XGIDCD
           .
       330-XGID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    終了
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-DELETE-GID-SEC
      *    絞込
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 440-COLLECT-MNOTE-SEC
      *    データ切り替え
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 440-DATA-CHANGE-SEC
      *    前日
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 440-BEFORE-SEC
      *    次日
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 450-NEXT-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06S"
               PERFORM 440-BACK-PAGE-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07S"
               PERFORM 450-NEXT-PAGE-SEC
      *    クライアント保存
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 460-SAVE-SEC
      *    状態変更
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM 450-CHANGE-STATUS-SEC
      *    リセット
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 460-RESET-SEC
      *    一覧更新
           WHEN    "CLICKED"       ALSO    "B11S"
               PERFORM 470-REFRESH-SEC
      *    送信
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 460-SEND-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "PTNUM"
               PERFORM 4100-PTNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "CREYMD"
               PERFORM 4100-CREYMD-CHK-SEC
      *    行選択
           WHEN    ANY             ALSO    "LIST"
               PERFORM 203-SELECT-SEC
           WHEN    OTHER
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
            IF    ( SPA-ERRCD      =   SPACE )
                PERFORM 4101-CREYMD-CHK-SEC
            END-IF
      *
            IF    ( SPA-ERRCD      =   SPACE )
                PERFORM 4101-PTNUM-CHK-SEC
            END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 31011-MNOTE-INFO-CLEAR-SEC
      *
           MOVE    XG03-SELNUM     TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )
               CONTINUE
           ELSE
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL        >   SPA-GMN-MAX )
                        OR   ( SPA-NAI-SELNUM
                                           >   ZERO )
      *
                   IF    ( SPA-GMN-TNO (IDXL)
                                   =   SPA-GMN-SELNUM )
                       MOVE    IDXL
                                   TO  SPA-NAI-SELNUM
                       PERFORM 4101-ITEM-EDIT-SEC
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SPA-NAI-SELNUM  =    ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択項目編集処理
      *****************************************************************
       4101-ITEM-EDIT-SEC              SECTION.
      *
           MOVE    SPA-GMN-TNO (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-SELNUM
           MOVE    SPA-NAI-TPTID (SPA-NAI-SELNUM)
                                       TO  SPA-NAI-PTID
           MOVE    SPA-GMN-TPTNUM (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-PTNUM
           MOVE    SPA-GMN-TPTNAME (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-NAME
           PERFORM 41001-MNOTE-INFO-EDIT-SEC
           MOVE    SPA-NAI-SELNUM      TO  WRK-XG03-ROW
      *
           .
       4101-ITEM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳管理情報編集処理
      *****************************************************************
       41001-MNOTE-INFO-EDIT-SEC       SECTION.
      *
           MOVE    SPA-NAI-SELNUM      TO  IDXL
      *
           MOVE    SPA-GMN-TCREYMD (IDXL)
                                       TO  SPA-GMN-ICREYMD
           MOVE    SPA-GMN-TPTNUM  (IDXL)
                                       TO  SPA-GMN-IPTNUM
           MOVE    SPA-GMN-TPTNAME (IDXL)
                                       TO  SPA-GMN-IPTNAME
           MOVE    SPA-GMN-TTITLE  (IDXL)
                                       TO  SPA-GMN-ITITLE
           MOVE    SPA-GMN-TDATA-VERSION (IDXL)
                                       TO  SPA-GMN-IDATA-VERSION
           MOVE    SPA-GMN-TSRYYMD (IDXL)
                                       TO  SPA-GMN-ISRYYMD
           MOVE    SPA-GMN-TLDNAME (IDXL)
                                       TO  SPA-GMN-ILDNAME
           MOVE    SPA-GMN-TDRNAME (IDXL)
                                       TO  SPA-GMN-IDRNAME
           MOVE    SPA-GMN-TCREYMD (IDXL)
                                       TO  SPA-GMN-ICREYMD
           MOVE    SPA-GMN-TCREHMS (IDXL)
                                       TO  SPA-GMN-ICREHMS
           MOVE    SPA-NAI-TOPID   (IDXL)
                                       TO  SPA-GMN-IOPID
           MOVE    SPA-GMN-TSTATUS (IDXL)
                                       TO  SPA-GMN-ISTATUS
           MOVE    SPA-NAI-TPTID   (IDXL)
                                       TO  SPA-NAI-IPTID
           MOVE    SPA-NAI-TTBL-KEY   (IDXL)
                                       TO  SPA-NAI-ITBL-KEY
           MOVE    SPA-NAI-TTBL-GROUP (IDXL)
                                       TO  SPA-NAI-ITBL-GROUP
           MOVE    SPA-NAI-TTBL-UUID  (IDXL)
                                       TO  SPA-NAI-ITBL-UUID
           MOVE    SPA-NAI-TRENNUM    (IDXL)
                                       TO  SPA-NAI-IRENNUM
           MOVE    SPA-NAI-TSHORI-RENNUM (IDXL)
                                       TO  SPA-NAI-ISHORI-RENNUM
           MOVE    SPA-NAI-TCNT (IDXL)
                                       TO  SPA-NAI-ICNT
           MOVE    SPA-NAI-TSTATUS (IDXL)
                                       TO  SPA-NAI-ISTATUS
      *
           PERFORM 900-MNOTE-H-KEY-SEL-SEC
           PERFORM 900-MNOTE-B-KEY2-SEL-SEC
      *
           INSPECT SPA-GMN-CONTEXT
                           TALLYING WRK-MAX-TEXTSIZE FOR ALL " "
      *
           INITIALIZE                  TMNOTE-B-REC
           INSPECT TMNOTE-B-CSVDATA
                           TALLYING WRK-MAX-DATASIZE FOR ALL " "
      *
           MOVE    1                   TO  IDXP
      *
           PERFORM UNTIL ( FLG-MNOTE-B NOT =   ZERO )
                    OR   ( SPA-NAI-OVER-FLOW   =   "1" )
      *
               PERFORM VARYING WRK-SIZE FROM WRK-MAX-DATASIZE BY -1
                       UNTIL ( WRK-SIZE <=  0 )
                        OR   ( MNOTE-B-CSVDATA (WRK-SIZE : 1 )
                                     NOT =   SPACE )
                   CONTINUE
               END-PERFORM
      *
               IF    ( WRK-MAX-TEXTSIZE    <
                                  IDXP + WRK-SIZE )
                   MOVE    "1"         TO  SPA-NAI-OVER-FLOW
               ELSE
                   IF    ( MNOTE-B-SEQ-NO  >   1 )
                       STRING    X"0A"         DELIMITED BY SIZE
                       INTO      SPA-GMN-CONTEXT
                       WITH POINTER IDXP
                       END-STRING
                   END-IF
      *
                   IF    ( WRK-SIZE    >   ZERO )
                       STRING    MNOTE-B-CSVDATA (1: WRK-SIZE)
                                               DELIMITED BY SIZE
                       INTO      SPA-GMN-CONTEXT
                       WITH POINTER IDXP
                       END-STRING
                   END-IF
               END-IF
      *
               PERFORM 900-MNOTE-B-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_mnote_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41001-MNOTE-INFO-EDIT-EXT.
           EXIT.
           .
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       4100-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( XG03-PTNUM      =   SPA-GMN-PTNUM )
               CONTINUE
           ELSE
      *
               PERFORM 31011-MNOTE-INFO-CLEAR-SEC
      *
               MOVE    XG03-PTNUM  TO  SPA-GMN-PTNUM
      *
               PERFORM 4101-PTNUM-CHK-SEC
      *
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM VARYING IDXL    FROM    1   BY  1
                           UNTIL ( IDXL        >   CONST-TBL-MAX )
                       IF    ( SPA-NAI-TPTID (IDXL)
                                           =   SPA-NAI-PTID )
                           MOVE    IDXL    TO  SPA-NAI-SELNUM
                           PERFORM 4101-ITEM-EDIT-SEC
                           MOVE    CONST-TBL-MAX
                                           TO  IDXL
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-NAI-SELNUM  >   ZERO )
                   MOVE    212         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4100-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       4101-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-NAME
           MOVE    ZERO        TO  SPA-NAI-PTID
      *
           IF    ( SPA-GMN-PTNUM   NOT =   SPACE )
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    SPA-GMN-PTNUM
                                       TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC    NOT =   ZERO )
                   MOVE    "0008"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-CUR
               ELSE
                   MOVE    SPTID-PTNUM TO  SPA-GMN-PTNUM
                   MOVE    SPTID-PTID  TO  SPA-NAI-PTID
      *
      *            患者マスター存在チェック
                   MOVE    SPA-NAI-PTID
                                       TO  WRK-PTID
                   PERFORM 900-PTINF-KEY-SEL-SEC
                   IF    ( FLG-PTINF   NOT =  ZERO )
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    7       TO  SPA-GMN-CUR
                   ELSE
                       MOVE    PTINF-NAME
                                       TO  SPA-GMN-NAME
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    作成日入力処理
      *****************************************************************
       4100-CREYMD-CHK-SEC              SECTION.
      *
           IF    ( XG03-CREYMD      =   SPA-GMN-CREYMD )
               CONTINUE
           ELSE
               PERFORM 31011-MNOTE-INFO-CLEAR-SEC
               MOVE    XG03-CREYMD  TO  SPA-GMN-CREYMD
               PERFORM 4101-CREYMD-CHK-SEC
      *
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 3101-MLIST-EDIT-SEC
               END-IF
           END-IF
      *
           .
       4100-CREYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成日入力処理
      *****************************************************************
       4101-CREYMD-CHK-SEC             SECTION.
      *
           MOVE    SPACE        TO  SPA-NAI-CREYMD
      *
           IF    ( SPA-GMN-CREYMD   =   SPACE )
               MOVE    SPA-NAI-TODAY
                                    TO  SPA-NAI-CREYMD
               MOVE    SPA-GMN-TODAY
                                    TO  SPA-GMN-CREYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE               ORCSGDAYAREA
               MOVE    SPA-GMN-CREYMD
                                    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"   USING
                                    ORCSGDAYAREA
               IF    ( SGDAY-RC     =   ZERO )
                   MOVE    SGDAY-OTDAY  TO  SPA-GMN-CREYMD
                   MOVE    SGDAY-SDAY   TO  SPA-NAI-CREYMD
               ELSE
                   MOVE    "0010"   TO  SPA-ERRCD
                   MOVE    8        TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-CREYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       203-SELECT-SEC              SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-MAX )
               IF    ( XG03-SELECT (IDXL)   =   "T" )
                   PERFORM 31011-MNOTE-INFO-CLEAR-SEC
                   MOVE    SPA-GMN-TNO (IDXL)
                                   TO  XG03-SELNUM
                   PERFORM 4100-SELNUM-CHK-SEC
                   MOVE    ZERO    TO  WRK-XG03-ROW
                   MOVE    212     TO  SPA-GMN-CUR
                   MOVE    SPA-GMN-MAX
                                   TO  IDXL
               END-IF
           END-PERFORM
      *
           .
       203-SELECT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *
           IF    ( SPA-NAI-ITBL-GROUP  =   SPACE )
               MOVE    "0101"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 210-BACK-SYORI-SEC
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "XG03"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                SECTION.
      *
           IF    ( SPA-NAI-COLLECT  =    "1" )
               IF    ( SPA-NAI-SELNUM   =    ZERO )
                   PERFORM 440-COLLECT-MNOTE-SEC
               ELSE
                   PERFORM 31011-MNOTE-INFO-CLEAR-SEC
               END-IF
           ELSE
      *
               IF    ( SPA-NAI-SELNUM   =    ZERO )
                   PERFORM 310-SPASET-SEC
               ELSE
                   PERFORM 31011-MNOTE-INFO-CLEAR-SEC
               END-IF
           END-IF
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-GID-SEC                SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    更新前
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-NAI-ISTATUS  =   SPACE )
                   MOVE    "3003"  TO  SPA-XGIDCD
               ELSE
                   MOVE    "3002"  TO  SPA-XGIDCD
               END-IF
           END-IF
      *
           .
       430-DELETE-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC                SECTION.
      *
           INITIALIZE                      MNOTE-H-REC
           MOVE    SPA-NAI-ITBL-KEY    TO  MNOTE-H-TBL-KEY
           MOVE    SPA-NAI-ITBL-GROUP  TO  MNOTE-H-TBL-GROUP
           MOVE    SPA-NAI-ITBL-UUID   TO  MNOTE-H-TBL-UUID 
           MOVE    SPA-NAI-ISHORI-RENNUM
                                       TO  MNOTE-H-SHORI-RENNUM
           MOVE    SPA-NAI-IRENNUM     TO  MNOTE-H-RENNUM
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC         NOT  =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  MNOTE-B-REC
               MOVE    SPA-NAI-ITBL-KEY    TO  MNOTE-B-TBL-KEY
               MOVE    SPA-NAI-ITBL-GROUP  TO  MNOTE-B-TBL-GROUP
               MOVE    SPA-NAI-ITBL-UUID   TO  MNOTE-B-TBL-UUID
               MOVE    SPA-NAI-ISHORI-RENNUM
                                           TO  MNOTE-B-SHORI-RENNUM
               MOVE    SPA-NAI-IRENNUM     TO  MNOTE-B-RENNUM
               MOVE    SPA-HOSPNUM         TO  MNOTE-B-HOSPNUM
               MOVE    MNOTE-B-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_mnote_b"       TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC         NOT  =   ZERO )
                   MOVE    "1002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               PERFORM 470-REFRESH-SEC
           END-IF
      *
           .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報絞込処理
      *****************************************************************
       440-COLLECT-MNOTE-SEC           SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               IF    ( SPA-NAI-COLLECT =   SPACE )
      *
                   IF    ( SPA-NAI-ITBL-GROUP  =   SPACE )
                    AND  ( SPA-NAI-PTID        =   ZERO )
                       MOVE    "0102"      TO  SPA-ERRCD
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       MOVE    "1"         TO  SPA-NAI-COLLECT
                   END-IF
               ELSE
                   MOVE    SPACE       TO  SPA-NAI-COLLECT
               END-IF
      *
               PERFORM 31011-MNOTE-INFO-CLEAR-SEC
               PERFORM 3101-MLIST-EDIT-SEC
      *
           END-IF
      *
           .
       440-COLLECT-MNOTE-EXT.
           EXIT.
      *****************************************************************
      *    データ切り替え処理
      *****************************************************************
       440-DATA-CHANGE-SEC             SECTION.
      *
           IF      SPA-NAI-TBL-KEY     =   CONST-TBL-KEY
               MOVE    CONST-TBL-KEY2      TO  SPA-NAI-TBL-KEY
           ELSE
               MOVE    CONST-TBL-KEY       TO  SPA-NAI-TBL-KEY
           END-IF
           PERFORM 470-REFRESH-SEC
           .
       440-DATA-CHANGE-EXT.
           EXIT.
      *****************************************************************
      *    前日処理
      *****************************************************************
       440-BEFORE-SEC                  SECTION.
      *
           MOVE    ZERO            TO  FLG-NEXT
           PERFORM 4401-CHANGE-DATE-SEC
      *
           .
       440-BEFORE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日処理
      *****************************************************************
       450-NEXT-SEC                    SECTION.
      *
           MOVE    1               TO  FLG-NEXT
           PERFORM 4401-CHANGE-DATE-SEC
      *
           .
       450-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    日付変更処理
      *****************************************************************
       4401-CHANGE-DATE-SEC            SECTION.
      *
           PERFORM 31011-MNOTE-INFO-CLEAR-SEC
      *
           IF    ( SPA-NAI-COLLECT     =   "1" )
               MOVE    SPACE       TO  SPA-NAI-COLLECT
               MOVE    SPACE       TO  SPA-GMN-NAME
               MOVE    SPACE       TO  SPA-GMN-PTNUM
               MOVE    ZERO        TO  SPA-NAI-PTID
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           MOVE    SPA-NAI-CREYMD      TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
      *
           IF    ( FLG-NEXT            =   1 )
               MOVE    1               TO  WRK-ZOGEN
           ELSE
               MOVE    -1              TO  WRK-ZOGEN
           END-IF
      *
           PERFORM 800-CALENDAR-SEC
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-CREYMD
      *
           MOVE    SPA-NAI-CREYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-CREYMD
      *
      *    コラムリストの編集
           PERFORM 3101-MLIST-EDIT-SEC
      *
           .
       4401-CHANGE-DATE-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       440-BACK-PAGE-SEC               SECTION.
      *
           IF    ( SPA-GMN-PAGE    >   1 )
               MOVE    ZERO        TO  FLG-NEXT
               PERFORM 4401-CHANGE-PAGE-SEC
           END-IF
      *
           .
       440-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       450-NEXT-PAGE-SEC           SECTION.
      *
           IF    ( SPA-GMN-MLIST-NEXT    =   1 )
               MOVE    1           TO  FLG-NEXT
               PERFORM 4401-CHANGE-PAGE-SEC
           END-IF
      *
           .
       450-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁変更処理
      *****************************************************************
       4401-CHANGE-PAGE-SEC        SECTION.
      *
           PERFORM 31011-MNOTE-INFO-CLEAR-SEC
      *
           IF    ( FLG-NEXT            =   1 )
               COMPUTE     SPA-GMN-PAGE
                   =       SPA-GMN-PAGE
                   +       1
           ELSE
               COMPUTE     SPA-GMN-PAGE
                   =       SPA-GMN-PAGE
                   -       1
           END-IF
      *
      *    コラムリストの編集
           PERFORM 3101-MLIST-EDIT-SEC
      *
           .
       4401-CHANGE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    状態変更処理
      *****************************************************************
       450-CHANGE-STATUS-SEC       SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    更新前
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           IF    ( SPA-NAI-SELNUM  >   ZERO )
               IF    ( SPA-NAI-TSTATUS (SPA-NAI-SELNUM)
                                   =  SPACE )
                   MOVE    "3004"      TO SPA-XGIDCD
               ELSE
                   MOVE    "3005"      TO SPA-XGIDCD
               END-IF
           END-IF
      *
           .
       450-CHANGE-STATUS-EXT.
           EXIT.
      *****************************************************************
      *    状態変更処理
      *****************************************************************
       4501-CHANGE-STATUS-SEC      SECTION.
      *
           PERFORM 800-ORCSMCNDATE-SEC
      *
           INITIALIZE                      MNOTE-H-REC
           MOVE    SPA-NAI-ITBL-KEY    TO  MNOTE-H-TBL-KEY
           MOVE    SPA-NAI-ITBL-GROUP  TO  MNOTE-H-TBL-GROUP
           MOVE    SPA-NAI-ITBL-UUID   TO  MNOTE-H-TBL-UUID 
           MOVE    SPA-NAI-ISHORI-RENNUM
                                       TO  MNOTE-H-SHORI-RENNUM
           MOVE    SPA-NAI-IRENNUM     TO  MNOTE-H-RENNUM
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           IF    ( FLG-SENT            =   1 )
               MOVE    "1"             TO  MNOTE-H-PUTFLG
           ELSE
               MOVE    SPACE           TO  MNOTE-H-PUTFLG
           END-IF
           MOVE    SMCNDATE-YMD        TO  MNOTE-H-UPYMD
           MOVE    SMCNDATE-HMS        TO  MNOTE-H-UPHMS
           MOVE    MNOTE-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"   TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC         NOT  =   ZERO )
               MOVE    "1003"          TO  SPA-ERRCD
           END-IF
      *
           PERFORM 470-REFRESH-SEC
      *
           .
       4501-CHANGE-STATUS-EXT.
           EXIT.
      *****************************************************************
      *    リフレッシュ処理
      *****************************************************************
       470-REFRESH-SEC                 SECTION.
      *
           PERFORM 31011-MNOTE-INFO-CLEAR-SEC
           PERFORM 3101-MLIST-EDIT-SEC
      *
           .
       470-REFRESH-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント保存
      *****************************************************************
       460-SAVE-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-ITBL-GROUP  =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4601-SAVE-SEC
           END-IF
      *
           .
       460-SAVE-EXT.
           EXIT.
      *****************************************************************
      *    クライアント保存
      *****************************************************************
       4601-SAVE-SEC                   SECTION.
      *
      *    帳票管理ＤＢのチェック
           PERFORM 900-MNOTE-H-KEY-SEL-SEC
           IF    ( FLG-MNOTE-H     =   ZERO )
      *
               MOVE    SPACE           TO  ASS-CSVDATA
      *
               PERFORM 800-ORCSMCNDATE-SEC
      *
               PERFORM 800-ORCUIDSET-SEC
      *
               IF      SPA-NAI-TBL-KEY     =   CONST-TBL-KEY2
                   STRING  "/tmp/"         DELIMITED BY SIZE
                           "jmamr-"        DELIMITED BY SIZE
                           SMCNDATE-YMD    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           SMCNDATE-HMS    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           UIDIO-UID       DELIMITED BY SPACE
                           ".csv"          DELIMITED BY SIZE
                   INTO    ASS-CSVDATA
                   END-STRING
               ELSE
                   STRING  "/tmp/"         DELIMITED BY SIZE
                           "/okusuri_techo-"
                                           DELIMITED BY SIZE
                           SMCNDATE-YMD    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           SMCNDATE-HMS    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           UIDIO-UID       DELIMITED BY SPACE
                           ".csv"          DELIMITED BY SIZE
                   INTO    ASS-CSVDATA
                   END-STRING
               END-IF
      *
               INITIALIZE                  SGETTEMP-AREA
               MOVE    ASS-CSVDATA     TO  SGETTEMP-BASENAME
               CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAME
                                       TO  ASS-CSVDATA
      *
               OPEN    OUTPUT          CSVDATA-FILE
      *
               PERFORM 900-MNOTE-B-KEY2-SEL-SEC
      *
               PERFORM UNTIL ( FLG-MNOTE-B NOT =   ZERO )
      *
                   MOVE    MNOTE-B-CSVDATA
                                       TO  CSVDATA-REC
                   WRITE   CSVDATA-REC
      *
                   PERFORM 900-MNOTE-B-KEY2-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_mnote_b"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               CLOSE   CSVDATA-FILE
      *
               INITIALIZE                      ORCSFILEENCODEAREA
               MOVE    ASS-CSVDATA         TO  FLEN-INFILENAME
               STRING  ASS-CSVDATA             DELIMITED BY SPACE
                       ".sjis"                 DELIMITED BY SIZE
               INTO    FLEN-OUTFILENAME
               END-STRING
               MOVE    "3"                 TO  FLEN-OPTION
               CALL    "orcfileencode"     USING
                                           ORCSFILEENCODEAREA
      *
               INITIALIZE                      ORCA-BLOB
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "blob"              TO  MCP-TABLE
               MOVE    FLEN-OUTFILENAME    TO  ORCA-BLOB-FILE
               PERFORM 910-BLOBIMPORT-SEC
               MOVE    ORCA-BLOB-OBJECT    TO  WRK-OBJECTDATA
               MOVE    MNOTE-H-TO-DATA     TO  WRK-DL-FILENAME
      *
               MOVE    FLEN-INFILENAME     TO  WRK-DEL-FILENAME
               PERFORM 800-ORCFILEDEL-SEC
      *
               MOVE    FLEN-OUTFILENAME    TO  WRK-DEL-FILENAME
               PERFORM 800-ORCFILEDEL-SEC
      *
           END-IF
      *
           .
       4601-SAVE-EXT.
           EXIT.
      *****************************************************************
      *    リセット処理
      *****************************************************************
       460-RESET-SEC                   SECTION.
      *
           MOVE    SPACE           TO  ASS-CSVDATA
      *
           PERFORM 800-ORCUIDSET-SEC
      *
           IF      SPA-NAI-TBL-KEY     =   CONST-TBL-KEY2
               STRING  "/tmp/"         DELIMITED BY SIZE
                       "jmamr-reset-"
                       UIDIO-UID       DELIMITED BY SPACE
                       ".ksr"          DELIMITED BY SIZE
               INTO    ASS-CSVDATA
               END-STRING
           ELSE
               STRING  "/tmp/"         DELIMITED BY SIZE
                       "/okusuri_techo-reset-"
                       UIDIO-UID       DELIMITED BY SPACE
                       ".ksr"          DELIMITED BY SIZE
               INTO    ASS-CSVDATA
               END-STRING
           END-IF
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    ASS-CSVDATA     TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  ASS-CSVDATA
      *
           OPEN    OUTPUT          CSVDATA-FILE
           MOVE    SPACE           TO  CSVDATA-REC
           WRITE   CSVDATA-REC
      *
           CLOSE   CSVDATA-FILE
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    ASS-CSVDATA         TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBIMPORT-SEC
           MOVE    ORCA-BLOB-OBJECT    TO  WRK-OBJECTDATA
           IF      SPA-NAI-TBL-KEY     =   CONST-TBL-KEY2
               MOVE    "jmamr-reset.ksr"   TO  WRK-DL-FILENAME
           ELSE
               MOVE    "okusuri_techo-reset.ksr"
                                       TO  WRK-DL-FILENAME
           END-IF
      *
           MOVE    1                   TO  FLG-RESET
      *
           MOVE    ASS-CSVDATA         TO  WRK-DEL-FILENAME
           PERFORM 800-ORCFILEDEL-SEC
      *
           .
       460-RESET-EXT.
           EXIT.
      *****************************************************************
      *    送信処理
      *****************************************************************
       460-SEND-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-ITBL-GROUP  =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4601-SEND-SEC
           END-IF
      *
           .
       460-SEND-EXT.
           EXIT.
      *****************************************************************
      *    送信処理
      *****************************************************************
       4601-SEND-SEC                   SECTION.
      *
           MOVE    SPA-NAI-IPTID       TO  WRK-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( SPA-NAI-TODAY       <   PTINF-BIRTHDAY )
               MOVE    ZERO            TO  WRK-AGE
           ELSE
               MOVE    PTINF-BIRTHDAY  TO  WRK-YMD1
               MOVE    SPA-NAI-TODAY   TO  WRK-YMD2
               COMPUTE WRK-AGE     =   WRK-YY2
                                   -   WRK-YY1
               IF    ( WRK-YMD1(5:4)  >   WRK-YMD2(5:4) )
                   COMPUTE WRK-AGE    =   WRK-AGE
                                      -   1
               END-IF
           END-IF
      *
      *    帳票管理ＤＢのチェック
           PERFORM 900-MNOTE-H-KEY-SEL-SEC
           IF    ( FLG-MNOTE-H     =   ZERO )
      *
               MOVE    SPACE           TO  ASS-CSVDATA
      *
               PERFORM 800-ORCSMCNDATE-SEC
      *
               PERFORM 800-ORCUIDSET-SEC
      *
               IF      SPA-NAI-TBL-KEY     =   CONST-TBL-KEY2
                   STRING  "/tmp/"         DELIMITED BY SIZE
                           "jmamr-"        DELIMITED BY SIZE
                           SMCNDATE-YMD    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           SMCNDATE-HMS    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           UIDIO-UID       DELIMITED BY SPACE
                           ".ksr"          DELIMITED BY SIZE
                   INTO    ASS-CSVDATA
                   END-STRING
               ELSE
                   STRING  "/tmp/"         DELIMITED BY SIZE
                           "/okusuri_techo-"
                                           DELIMITED BY SIZE
                           SMCNDATE-YMD    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           SMCNDATE-HMS    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           UIDIO-UID       DELIMITED BY SPACE
                           ".ksr"          DELIMITED BY SIZE
                   INTO    ASS-CSVDATA
                   END-STRING
               END-IF
      *
               INITIALIZE                  SGETTEMP-AREA
               MOVE    ASS-CSVDATA     TO  SGETTEMP-BASENAME
               CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAME
                                       TO  ASS-CSVDATA
      *
               OPEN    OUTPUT          CSVDATA-FILE
      *
               PERFORM 46011-HEADER-EDIT-SEC
      *
               MOVE    "{{{"           TO  CSVDATA-REC
               WRITE   CSVDATA-REC
      *
               PERFORM 900-MNOTE-B-KEY2-SEL-SEC
      *
               PERFORM UNTIL ( FLG-MNOTE-B NOT =   ZERO )
      *
                   MOVE    MNOTE-B-CSVDATA
                                       TO  CSVDATA-REC
                   WRITE   CSVDATA-REC
      *
                   PERFORM 900-MNOTE-B-KEY2-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_mnote_b"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               MOVE    "}}}"           TO  CSVDATA-REC
               WRITE   CSVDATA-REC
      *
               CLOSE   CSVDATA-FILE
      *
               INITIALIZE                      ORCSFILEENCODEAREA
               MOVE    ASS-CSVDATA         TO  FLEN-INFILENAME
               STRING  ASS-CSVDATA             DELIMITED BY SPACE
                       ".sjis"                 DELIMITED BY SIZE
               INTO    FLEN-OUTFILENAME
               END-STRING
               MOVE    "3"                 TO  FLEN-OPTION
               CALL    "orcfileencode"     USING
                                           ORCSFILEENCODEAREA
      *
               INITIALIZE                      ORCA-BLOB
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "blob"              TO  MCP-TABLE
               MOVE    FLEN-OUTFILENAME    TO  ORCA-BLOB-FILE
               PERFORM 910-BLOBIMPORT-SEC
               MOVE    ORCA-BLOB-OBJECT    TO  WRK-OBJECTDATA
               IF      SPA-NAI-TBL-KEY     =   CONST-TBL-KEY2
                   MOVE    "jmamr.ksr"         TO  WRK-DL-FILENAME
               ELSE
                   MOVE    "okusuri_techo.ksr"
                                           TO  WRK-DL-FILENAME
               END-IF
      *
               MOVE    SPA-GMN-NAME    TO  WRK-NAME
      *
               MOVE    1               TO   FLG-SENT
               PERFORM 4501-CHANGE-STATUS-SEC
      *
               MOVE    1               TO  FLG-SENDDATA
      *
               MOVE    FLEN-INFILENAME TO  WRK-DEL-FILENAME
               PERFORM 800-ORCFILEDEL-SEC
      *
               MOVE    FLEN-OUTFILENAME TO  WRK-DEL-FILENAME
               PERFORM 800-ORCFILEDEL-SEC
      *
           END-IF
      *
           .
       4601-SEND-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー編集処理
      *****************************************************************
       46011-HEADER-EDIT-SEC           SECTION.
      *
           MOVE    SPACE               TO  CSVDATA-REC
           STRING  "id="               DELIMITED BY SIZE
                   SPA-GMN-IPTNUM      DELIMITED BY SPACE
           INTO    CSVDATA-REC
           END-STRING
           WRITE   CSVDATA-REC
      *
           MOVE    SPACE               TO  CSVDATA-REC
           STRING  "name="             DELIMITED BY SIZE
                   SPA-GMN-IPTNAME     DELIMITED BY SPACE
           INTO    CSVDATA-REC
           END-STRING
           WRITE   CSVDATA-REC
      *
           MOVE    WRK-AGE-G           TO  WRK-AGE-X
           EVALUATE    TRUE
           WHEN   ( WRK-AGE             <  10 )
               MOVE    WRK-AGE-G (3:)  TO  WRK-AGE-X
           WHEN   ( WRK-AGE             <  100 )
               MOVE    WRK-AGE-G (2:)  TO  WRK-AGE-X
           END-EVALUATE
           MOVE    SPACE               TO  CSVDATA-REC
           STRING  "age="              DELIMITED BY SIZE
                   WRK-AGE-X           DELIMITED BY SPACE
           INTO    CSVDATA-REC
           END-STRING
           WRITE   CSVDATA-REC
      *
           IF    ( PTINF-SEX           =   "1" )
               MOVE    "gender=male"   TO  CSVDATA-REC
               WRITE   CSVDATA-REC
           ELSE
               MOVE    "gender=female" TO  CSVDATA-REC
               WRITE   CSVDATA-REC
           END-IF
      *
           MOVE    "mime=application/x-medication-note"
                                       TO  CSVDATA-REC
           WRITE   CSVDATA-REC
      *
           MOVE    "charset=Shift-JIS"
                                       TO  CSVDATA-REC
           WRITE   CSVDATA-REC
      *
           .
       46011-HEADER-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-XGIDCD      NOT =   SPACE )
               PERFORM 520-TIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XG03    "          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF    ( WRK-XG03-ROW        >   ZERO )
               MOVE    WRK-XG03-ROW    TO  WRK-LIST-ROW
               MOVE    2               TO  FLG-LIST-ROW
           ELSE
               MOVE    XG03-ROW        TO  WRK-LIST-ROW
           END-IF
      *
           MOVE    SPACE               TO  XG03
           INITIALIZE                      XG03
      *
           EVALUATE    FLG-LIST-ROW
           WHEN    1
               MOVE    ZERO            TO  XG03-ROW
               MOVE    ZERO            TO  XG03-ROWATTR
           WHEN    2
               MOVE    WRK-LIST-ROW    TO  XG03-ROW
               MOVE    2               TO  XG03-ROWATTR
           WHEN    OTHER
               MOVE    WRK-LIST-ROW    TO  XG03-ROW
               MOVE    ZERO            TO  XG03-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-MAX         TO  XG03-COUNT
           PERFORM VARYING     IDXL    FROM    1   BY  1
                   UNTIL     ( IDXL        >   SPA-GMN-MAX )
      *
               MOVE    SPA-GMN-TNO (IDXL)
                                       TO  WRK-ZZ
               MOVE    WRK-ZZ          TO  XG03-TNO    (IDXL)
               MOVE    SPA-GMN-TPTNUM (IDXL)
                                       TO  XG03-TPTNUM (IDXL)
               MOVE    SPA-GMN-TPTNAME (IDXL)
                                       TO  XG03-TPTNAME (IDXL)
               MOVE    SPA-GMN-TTITLE (IDXL)
                                       TO  XG03-TTITLE (IDXL)
               MOVE    SPA-GMN-TDATA-VERSION (IDXL)
                                       TO  XG03-TDATA-VERSION (IDXL)
               MOVE    SPA-GMN-TSRYYMD (IDXL)
                                       TO  XG03-TSRYYMD (IDXL)
               MOVE    SPA-GMN-TDRNAME (IDXL)
                                       TO  XG03-TDRNAME (IDXL)
               MOVE    SPA-GMN-TLDNAME (IDXL)
                                       TO  XG03-TLDNAME (IDXL)
               MOVE    SPA-GMN-TCREYMD (IDXL)
                                       TO  XG03-TCREYMD (IDXL)
               MOVE    SPA-GMN-TCREHMS (IDXL)
                                       TO  XG03-TCREHMS (IDXL)
               MOVE    SPA-GMN-TSTATUS (IDXL)
                                       TO  XG03-TSTATUS (IDXL)
      *
               IF    ( IDXL                =   SPA-NAI-SELNUM )
                   MOVE    "T"             TO  XG03-SELECT (IDXL)
               ELSE
                   MOVE    "F"             TO  XG03-SELECT (IDXL)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SELNUM      TO  XG03-SELNUM
           MOVE    SPA-GMN-PTNUM       TO  XG03-PTNUM 
           MOVE    "blue"              TO  XG03-PTNAME-STYLE
           MOVE    SPA-GMN-NAME        TO  XG03-PTNAME-VALUE
      *
           MOVE    SPA-GMN-CREYMD      TO  XG03-CREYMD
      *
           MOVE    SPA-GMN-IPTNUM      TO  XG03-IPTNUM
           MOVE    SPA-GMN-IPTNAME     TO  XG03-IPTNAME
           MOVE    SPA-GMN-ITITLE      TO  XG03-ITITLE
           MOVE    SPA-GMN-IDATA-VERSION
                                       TO  XG03-IDATA-VERSION
           MOVE    SPA-GMN-IDRNAME     TO  XG03-IDRNAME
           MOVE    SPA-GMN-ISRYYMD     TO  XG03-ISRYYMD
           MOVE    SPA-GMN-ILDNAME     TO  XG03-ILDNAME
           MOVE    SPA-GMN-ICREYMD     TO  XG03-ICREYMD
           MOVE    SPA-GMN-ICREHMS     TO  XG03-ICREHMS
           MOVE    SPA-GMN-IOPID       TO  XG03-IOPID
           MOVE    SPA-GMN-ISTATUS     TO  XG03-ISTATUS
           MOVE    SPA-GMN-CONTEXT     TO  XG03-CONTEXT
      *
           MOVE    WIDGET-NORMAL       TO  XG03-B06S-STATE
                                           XG03-B07S-STATE
                                           XG03-B09-STATE
                                           XG03-B11-STATE
                                           XG03-B12-STATE
                                           XG03-PTNUM-STATE
                                           XG03-CREYMD-STATE
      *
           IF    ( SPA-NAI-COLLECT     =   "1" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XG03-PTNUM-STATE
                                           XG03-CREYMD-STATE
               MOVE    SPACE           TO  XG03-CREYMD
           END-IF
      *
           IF    ( SPA-NAI-SELNUM      >   ZERO )
               IF    ( SPA-NAI-TSTATUS(SPA-NAI-SELNUM)
                                       =   SPACE )
                   MOVE    "済に変更"  TO  XG03-B10-LABEL
               ELSE
                   MOVE    "未に変更"  TO  XG03-B10-LABEL
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-COLLECT =   SPACE )
               IF    ( SPA-NAI-SELNUM      >   ZERO )
                OR   ( SPA-NAI-PTID    NOT =   ZERO )
                   MOVE    "名寄せ"    TO  XG03-B04-LABEL
               END-IF
           ELSE
                   MOVE    "名寄せ解除" TO XG03-B04-LABEL
           END-IF
      *
           IF    ( SPA-GMN-PAGE        =   1 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XG03-B06S-STATE
           END-IF
      *
           IF    ( SPA-GMN-MLIST-NEXT    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XG03-B07S-STATE
           END-IF
      *
           IF    ( SPA-NAI-CLIENT-SAVE NOT =   "1" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XG03-B09-STATE
           END-IF
      *
           IF    ( SPA-NAI-ICCARDRW    NOT =   "1" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XG03-B11-STATE
                                           XG03-B12-STATE
           END-IF
      *
           MOVE    "blue"              TO  XG03-LBLMSG01-STYLE
           EVALUATE    TRUE
           WHEN  ( FLG-SENDDATA        =   1 )
               STRING  WRK-NAME        DELIMITED   BY  SPACE
                       "様のお薬手帳データを送信しました。"
                                       DELIMITED   BY  SIZE
               INTO    XG03-LBLMSG01
               END-STRING
           WHEN  ( FLG-RESET           =   1 )
               MOVE    "送信情報をリセットしました。"
                                       TO  XG03-LBLMSG01
           END-EVALUATE
      *
           MOVE    "red"               TO  XG03-LBLMSG02-STYLE
           IF    ( SPA-NAI-OVER-FLOW   =   "1" )
               MOVE    "サイズが大きいため、途中まで表示しています。"
                                       TO  XG03-LBLMSG02
           END-IF
      *
           MOVE    WRK-OBJECTDATA      TO  XG03-OBJECTDATA
           MOVE    WRK-DL-FILENAME     TO  XG03-FILENAME
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           IF    ( SPA-GMN-CUR     =    212 )
               IF    ( XG03-B12-STATE  =   WIDGET-INSENSITIVE )
                   IF    ( XG03-B09-STATE  =   WIDGET-INSENSITIVE )
                       MOVE    1       TO  SPA-GMN-CUR
                   ELSE
                       MOVE    209     TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE    "SELNUM"   TO  MCP-WIDGET
           WHEN    7
               MOVE    "PTNUM"    TO  MCP-WIDGET
           WHEN    8
               MOVE    "CREYMD"   TO  MCP-WIDGET
           WHEN    209
               MOVE    "B09"      TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"      TO  MCP-WIDGET
           END-EVALUATE
          .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    8          TO  SPA-GMN-CUR
           WHEN    "CREYMD"
               MOVE    7          TO  SPA-GMN-CUR
           WHEN    "PTNUM"
               MOVE    8          TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "正しい選択番号を入力してください" 
                                               TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "該当の電子版お薬情報が存在しません" 
                                               TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "該当患者の電子版お薬情報はありません"
                                               TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "診療年月日入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "番号を選択してください"
                                               TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "番号を選択するか患者番号を入力してください"
                                               TO  SPA-ERRMSG
           WHEN    "1001"  THRU  "1003"
               MOVE    "更新処理に失敗しました"
                                               TO  SPA-ERRMSG
           WHEN    "8888"
                   MOVE
                   "　　開　　発　　中!!(しばらくお待ち下さい)"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XGERR
           MOVE    SPA-ERRCD            TO  XGERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XGERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "XG03"                TO  SPA-MOTOPG
           MOVE    "XGERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "XGERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-XGIDMSG
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           EVALUATE    SPA-XGIDCD
           WHEN    "3002"
              MOVE    "電子版お薬情報を削除します"
                                       TO  WRK-XGIDMSG
               MOVE    "B01"           TO  MCP-WIDGET
           WHEN    "3003"
               MOVE    "未送信のデータですが削除しますか"
                                       TO  WRK-XGIDMSG
               MOVE    "B01"           TO  MCP-WIDGET
           WHEN    "3004"
               MOVE    "送信済に変更します。よろしいですか"
                                       TO  WRK-XGIDMSG
           WHEN    "3005"
               MOVE    "未送信に変更します。よろしいですか"
                                       TO  WRK-XGIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XGID1
           INITIALIZE                       XGID1
           MOVE    SPA-XGIDCD           TO  XGID1-ID1CODE
           MOVE    WRK-XGIDMSG          TO  XGID1-ID1MSG
      *
           MOVE    "XG03"               TO  SPA-MOTOPG
           MOVE    "XGID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XGID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *    MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得
      *****************************************************************
       800-ORCSMCNDATE-SEC                SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       800-ORCSMCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＵＵＩＤ取得
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset" USING
                               UIDIO-ST
                               UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ファイル削除処理
      *****************************************************************
       800-ORCFILEDEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    WRK-DEL-FILENAME TO IF-FILENAME
           CALL    "orcfiledel"    USING   ORCSFDELAREA
      *
           .
       800-ORCFILEDEL-EXT.
           EXIT.
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       800-HIZUKE-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           IF    ( WRK-SYMD            =   ALL "9"  OR   SPACE )
               MOVE    SPACE           TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)   TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)   TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)   TO  WRK-HDD
               MOVE    WRK-HENYMD      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-SYMD        TO  SGDAY-INDAY
               CALL    "ORCSGDAY"      USING
                                               ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ格納管理検索処理
      *****************************************************************
       900-MNOTE-H-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           INITIALIZE                      MNOTE-H-REC
      *
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
      *     MOVE    CONST-TBL-KEY       TO  MNOTE-H-TBL-KEY
           MOVE    SPA-NAI-TBL-KEY     TO  MNOTE-H-TBL-KEY
           MOVE    SPA-NAI-IRENNUM     TO  MNOTE-H-RENNUM
           MOVE    SPA-NAI-ITBL-GROUP  TO  MNOTE-H-TBL-GROUP
           MOVE    SPA-NAI-ITBL-UUID   TO  MNOTE-H-TBL-UUID
           MOVE    SPA-NAI-ISHORI-RENNUM
                                       TO  MNOTE-H-SHORI-RENNUM
           MOVE    MNOTE-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MNOTE-H-REC
           ELSE
               MOVE    1               TO  FLG-MNOTE-H
               INITIALIZE                  MNOTE-H-REC
           END-IF
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-MNOTE-H-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ格納管理検索処理
      *****************************************************************
       900-MNOTE-B-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-MNOTE-B
      *
           INITIALIZE                  MNOTE-B-REC
           MOVE    SPA-HOSPNUM     TO  MNOTE-B-HOSPNUM
           MOVE    MNOTE-H-TBL-KEY
                                   TO  MNOTE-B-TBL-KEY
           MOVE    MNOTE-H-RENNUM
                                   TO  MNOTE-B-RENNUM
           MOVE    MNOTE-H-TBL-GROUP
                                   TO  MNOTE-B-TBL-GROUP
           MOVE    MNOTE-H-TBL-UUID
                                   TO  MNOTE-B-TBL-UUID
           MOVE    MNOTE-H-SHORI-RENNUM
                                   TO  MNOTE-B-SHORI-RENNUM
           MOVE    "tbl_mnote_b"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  MNOTE-B-REC
           ELSE
               MOVE    1           TO  FLG-MNOTE-B
               INITIALIZE              MNOTE-B-REC
           END-IF
      *
           .
      *
       900-MNOTE-B-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ格納明細FETCH処理
      *****************************************************************
       900-MNOTE-B-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-B
      *
           MOVE    "tbl_mnote_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MNOTE-B-REC
           ELSE
               MOVE    1               TO  FLG-MNOTE-B
               INITIALIZE                  MNOTE-B-REC
           END-IF
      *
           .
      *
       900-MNOTE-B-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ格納管理検索処理
      *****************************************************************
       900-MNOTE-H-KEY8-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           INITIALIZE                      MNOTE-H-REC
      *
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
      *     MOVE    CONST-TBL-KEY       TO  MNOTE-H-TBL-KEY
           MOVE    SPA-NAI-TBL-KEY     TO  MNOTE-H-TBL-KEY
           IF    ( SPA-NAI-COLLECT     =   SPACE )
               MOVE    SPA-NAI-CREYMD  TO  MNOTE-H-CREYMD
           ELSE
               MOVE    SPA-NAI-PTID    TO  MNOTE-H-PTID
           END-IF
           MOVE    MNOTE-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"   TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-MNOTE-H-KEY8-FET-SEC
           ELSE
               MOVE    1               TO  FLG-MNOTE-H
               INITIALIZE                  MNOTE-H-REC
           END-IF
      *
           .
      *
       900-MNOTE-H-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ格納管理FETCH処理
      *****************************************************************
       900-MNOTE-H-KEY8-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
                                           FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-MNOTE-H NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_mnote_h"   TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  MNOTE-H-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
      *             IF    ( MNOTE-H-TBL-KEY =   CONST-TBL-KEY )
                   IF    ( MNOTE-H-TBL-KEY =   SPA-NAI-TBL-KEY )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-MNOTE-H
                   INITIALIZE                  MNOTE-H-REC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-MNOTE-H-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ格納明細検索処理
      *****************************************************************
       900-MNOTE-B-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-B
      *
           INITIALIZE                      MNOTE-B-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-B-HOSPNUM
           MOVE    MNOTE-H-RENNUM  TO  MNOTE-B-RENNUM
           MOVE    MNOTE-H-SHORI-RENNUM
                                       TO  MNOTE-B-SHORI-RENNUM
           MOVE    MNOTE-H-TBL-GROUP
                                       TO  MNOTE-B-TBL-GROUP
           MOVE    MNOTE-H-TBL-UUID
                                       TO  MNOTE-B-TBL-UUID
           MOVE    MNOTE-H-TBL-KEY     TO  MNOTE-B-TBL-KEY
           MOVE    1                   TO  MNOTE-B-SEQ-NO
           MOVE    MNOTE-B-REC     TO  MCPDATA-REC
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MNOTE-B-REC
           ELSE
               MOVE    1               TO  FLG-MNOTE-B
           END-IF
      *
           MOVE    "tbl_mnote_b"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-MNOTE-B-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
               MOVE    ZERO        TO  FLG-PTINF
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
               MOVE    ZERO        TO  FLG-PTNUM
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
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
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢＳＥＬＥＣＴ処理(FETCHはしない)
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＬＯＳＥＣＵＲＳＯＲ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＢＬＯＢＩＭＰＯＲＴ処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *
           .
      *
       910-BLOBIMPORT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

