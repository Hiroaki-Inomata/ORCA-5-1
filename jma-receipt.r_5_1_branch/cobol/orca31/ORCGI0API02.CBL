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
       PROGRAM-ID.         ORCGI0API02.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 出産育児一時金
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/04/09    NACL−太田　新規作成
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
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-KYUSEIRRK           PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-KYUSEI-ALERT        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX8                    PIC 9(05).
           03  IDX9                    PIC 9(05).
           03  IDXE                    PIC 9(05).
           03  IDX-ITM                 PIC 9(05).
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
           03  WRK-NUM2Z               PIC Z9.
           03  WRK-NUM3Z               PIC ZZ9.
           03  WRK-NUM7Z               PIC ZZZZZZ9.
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-KYUSEI-G.
               05  WRK-KYUSEI-NAME     PIC X(100).
               05  WRK-KYUSEI-KANANAME PIC X(100).
      *
       01  REG-AREA.
           03  REG-ORDERID             PIC X(04).
           03  REG-SAVEREQ             PIC X(01).
           03  REG-REQNUM              PIC X(02).
           03  REG-NYUINYMD            PIC X(08).
           03  REG-SRYYMD              PIC X(08).
           03  REG-ERR-MAX             PIC 9(03).
           03  REG-ERR-OCC             OCCURS  10.
               05  REG-ERRCD           PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-H240401           PIC X(08)   VALUE "20120401".
           03  CONST-ERR-MAX           PIC 9(03)   VALUE 10.
           03  CONST-ODRITEM-MAX       PIC 9(05)   VALUE 30.
           03  CONST-ALERT-MAX         PIC 9(05)   VALUE 5.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *    オーダエラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
      *    オーダ項目ＩＤ
           COPY    "CPNCLAIMID.INC".
      *
      *    オーダＩＤ
           COPY    "CPNORDERID.INC".
      *
      *    患者入院履歴退避
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *    分娩管理情報退避
       01  TBBN-REC.
           COPY    "CPBUNBEN.INC"      REPLACING  //BUNBEN-//
                                       BY         //TBBN-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    算定可能入院料取得
           COPY    "CPORCSNYUINRYO.INC".
      *
      *    入院オーダーエラーメッセージ取得
           COPY    "CPORCSODRS03.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *
      *    ＵＩＤ取得用エリア
       01  SUID-AREA.
           03  SUID-RET                PIC X(2).
           03  SUID-UID                PIC X(36).
       01  SUID-ST                     PIC 9(2).
      *
      *    API XML読み込み用定義体
           COPY    "CPBDV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPBDV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY  "CPKYUSEIRRK.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY  "CPHKNJAINF.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    分娩管理情報
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA31SCRAREA.INC".
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
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  REG-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TRRK-REC
           INITIALIZE                  TBBN-REC
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( REG-ERR-MAX         =   ZERO )
               PERFORM 200-MAIN-SEC
           ELSE
               IF    ( REG-SAVEREQ     =   "1" )
                   PERFORM 290-ORDER-INSERT-SEC
               END-IF
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
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
           INITIALIZE                  XML-BDREQ
                                       XML-BDRES
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
               PERFORM 990-ERROR-EDIT-SEC
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
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    NORDERID-BUNBENID   TO  REG-ORDERID
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           EVALUATE    BDREQ-REQNUM
           WHEN    "01"
               PERFORM 1001-SHOKAI-SEC
      *
           WHEN    "02"
               PERFORM 1001-TOROKU-SEC
      *
           WHEN    OTHER
               MOVE    "8003"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC         SECTION.
      *
           MOVE    BDREQ-SAVEREQ   TO  REG-SAVEREQ
      *
           MOVE    BDREQ-REQNUM    TO  REG-REQNUM
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           PERFORM 10011-NYUINYMD-CHECK-SEC
      *
           IF    ( WRK-ERRCD   =   SPACE )
               PERFORM 10011-BUNBEN-CHECK-SEC
           END-IF
      *
           IF    ( BDREQ-REQNUM    NOT =   "02" )
               MOVE    ZERO        TO  REG-SAVEREQ
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( BDREQ-PTNUM    =   SPACE )
               MOVE    "0004"           TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    BDREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "0004"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "0004"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           .
       10011-PTNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       10011-NYUINYMD-CHECK-SEC       SECTION.
      *
           IF    ( BDREQ-NYUINYMD =   SPACE )
               MOVE    "8001"           TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               MOVE    BDREQ-NYUINYMD
                                      TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1    =   SPACE )
                   MOVE    "8001"     TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD   TO  REG-NYUINYMD
               END-IF
      *
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
      *
               PERFORM 900-PTNYUINRRK-KEY39-SEL-SEC
               IF    ( FLG-PTNYUINRRK      =   ZERO )
      *
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  WRK-YMD
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    1               TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD   TO  WRK-SRYYMD
      *
                   MOVE    PTNYUINRRK-REC  TO  TRRK-REC
      *
                   IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                    AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
                    AND  ( PTNYUINRRK-NYUINYMD =   REG-NYUINYMD )
                       CONTINUE
                   ELSE
                       IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                        AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                           PERFORM 900-PTNYUINRRK-KEY20-SEL-SEC
                           IF    ( FLG-PTNYUINRRK      =   ZERO )
                            AND  ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                            AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
                            AND  ( PTNYUINRRK-NYUINYMD =   WRK-SRYYMD )
                               MOVE    PTNYUINRRK-REC  TO  TRRK-REC
                           ELSE
                               MOVE    "8005"  TO  WRK-ERRCD
                               PERFORM 990-ERROR-EDIT-SEC
                           END-IF
                       ELSE
                           MOVE    "8005"  TO  WRK-ERRCD
                           PERFORM 990-ERROR-EDIT-SEC
                       END-IF
                   END-IF
               ELSE
                       MOVE    "8005"      TO  WRK-ERRCD
                       PERFORM 990-ERROR-EDIT-SEC
               END-IF
           END-IF
      *
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    TRRK-NYUINYMD       TO  REG-SRYYMD
               IF    ( TRRK-TAIINYMD   NOT =   "99999999" )
                   MOVE    TRRK-TAIINYMD   TO  REG-SRYYMD
               END-IF
           END-IF
      *
           .
       10011-NYUINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       10011-BUNBEN-CHECK-SEC          SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN          =   ZERO )
               IF    ( BDREQ-REQNUM    =   "01" )
                AND  ( BUNBEN-UPDKBN   =   ZERO )
                   MOVE    "8916"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               END-IF
           ELSE
               MOVE    "8916"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           MOVE    BUNBEN-REC          TO  TBBN-REC
      *
           .
       10011-BUNBEN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（照会時）
      *****************************************************************
       1001-SHOKAI-SEC                 SECTION.
      *
      *
           .
       1001-SHOKAI-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（登録時）
      *****************************************************************
       1001-TOROKU-SEC             SECTION.
      *
      *
           .
       1001-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
      *
           EVALUATE    BDREQ-REQNUM
           WHEN    "01"
               PERFORM 2001-KAKUNIN-SEC
           WHEN    "02"
               PERFORM 2001-TOROKU-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認処理
      *****************************************************************
       2001-KAKUNIN-SEC                SECTION.
      *
           PERFORM 2001-RESPONSE-EDIT-SEC
      *
           .
       2001-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    確認処理
      *****************************************************************
       2001-TOROKU-SEC                 SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           CALL   "ORCSODR11S01"   USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           XML-BDREQ
                                           MCPAREA
                                           SPA-AREA
      *
           CALL   "ORCSODR11"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-SODR-COMMON-SET-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SODR-HOSPNUM
           MOVE    SUID-UID            TO  SODR-KARTE-KEY
           MOVE    1                   TO  SODR-ORDERNUM
           MOVE    "1"                 TO  SODR-NYUGAIKBN
           MOVE    SPA-PTNUM           TO  SODR-PTNUM
           MOVE    SPA-PTID            TO  SODR-PTID
           MOVE    SMCNDATE-YMD        TO  SODR-UKEYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UKEHMS
           IF    ( REG-SAVEREQ         =   "1" )
               MOVE    "0"             TO  SODR-PROCKBN
           ELSE
               MOVE    "2"             TO  SODR-PROCKBN
           END-IF
           MOVE    2                   TO  SODR-KEYGENERATE
           MOVE    99                  TO  SODR-INF-DIALOG
           MOVE    "1"                 TO  SODR-SRCKBN
      *
           MOVE    "api_"              TO  SODR-OPID
           MOVE    SPA-OPID            TO  SODR-OPID(5:)
           MOVE    SMCNDATE-YMD        TO  SODR-UPYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UPHMS
      *
           MOVE    REG-SRYYMD          TO  SODR-SRYYMD
           MOVE    REG-ORDERID         TO  SODR-ORDERID
      *
           .
       2300-SODR-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-ODRITEM-COMMON-SET-SEC       SECTION.
      *
           COMPUTE    IDX-ITM  =   IDX-ITM +   1
      *
           MOVE    SPA-HOSPNUM     TO  LNK-ODRITEM-HOSPNUM  (IDX-ITM)
           MOVE    SUID-UID        TO  LNK-ODRITEM-KARTE-KEY(IDX-ITM)
           MOVE    1               TO  LNK-ODRITEM-ORDERNUM(IDX-ITM)
           MOVE    IDX-ITM         TO  LNK-ODRITEM-ITEMNUM (IDX-ITM)
      *
           .
       2300-ODRITEM-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーエラー編集処理
      *****************************************************************
       2400-ORDER-ERROR-EDIT-SEC         SECTION.
      *
           IF    ( SODR-ERRCD   NOT =  SPACE )
               MOVE    SODR-ERRCD       TO WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           PERFORM VARYING IDX9    FROM    1   BY  1
                   UNTIL ( IDX9    >   LNK-ODRITEM-MAX )
      *
               MOVE    LNK-ODRITEM-OCC (IDX9)  TO  ODRITEM-REC
               IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                   MOVE    ODRITEM-ERRCD
                                        TO WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
                END-IF
      *
           END-PERFORM
      *
           .
       2400-ORDER-ERROR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2001-RESPONSE-EDIT-SEC          SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPACE           TO  WRK-KYUSEI-G
           PERFORM 900-KYUSEIRRK-KEY3-SEL-SEC
           IF    ( FLG-KYUSEIRRK   =   ZERO )
               MOVE    KYUSEI-KANANAME
                                   TO  WRK-KYUSEI-KANANAME
               MOVE    KYUSEI-NAME
                                   TO  WRK-KYUSEI-NAME
           END-IF
      *
           PERFORM 900-HKNJAINF-KEY-SEL-SEC
      *
           MOVE    SPA-PTNUM       TO  BDRES-PTNUM
           IF    ( WRK-KYUSEI-KANANAME =   SPACE )
               MOVE    PTINF-NAME      TO  BDRES-NAME
               MOVE    PTINF-KANANAME  TO  BDRES-KANANAME
           ELSE
               MOVE    WRK-KYUSEI-NAME     TO  BDRES-NAME
               MOVE    WRK-KYUSEI-KANANAME TO  BDRES-KANANAME
               IF    ( BUNBEN-NAME     NOT =   WRK-KYUSEI-KANANAME )
                   MOVE    1       TO  FLG-KYUSEI-ALERT
               END-IF
           END-IF
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  BDRES-BIRTHDAY
           MOVE    PTINF-SEX       TO  BDRES-SEX
      *
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  BDRES-RRKNUM
      *
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  BDRES-NYUINYMD
      *
           MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  BDRES-TAIINYMD
      *
           IF    ( BUNBEN-RIYOUKBN NOT =   SPACE )
               MOVE    "直接支払制度"
                                   TO  BDRES-RIYOUKBN-LBL
               MOVE    BUNBEN-RIYOUKBN
                                   TO  BDRES-RIYOUKBN-DATA
               EVALUATE    BUNBEN-RIYOUKBN
               WHEN    "0"
                   MOVE    "利用しない"
                                   TO  BDRES-RIYOUKBN-NAME
               WHEN    "1"
                   MOVE    "利用する"
                                   TO  BDRES-RIYOUKBN-NAME
               END-EVALUATE
           END-IF
      *
           IF    ( BUNBEN-SKYYM    NOT =   SPACE )
            AND  ( BUNBEN-SKYYM    NOT =   ZERO )
      *
               MOVE    "請求年月"      TO  BDRES-SKYYM-LBL
      *
               MOVE    BUNBEN-SKYYM    TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE (1:7)  TO  BDRES-SKYYM-DATA
           END-IF
      *
           IF    ( BUNBEN-TEISYUTUBIKBN NOT =   SPACE )
      *
               MOVE    "提出日"        TO  BDRES-TEISYUTUBIKBN-LBL
               MOVE    BUNBEN-TEISYUTUBIKBN
                                       TO  BDRES-TEISYUTUBIKBN-DATA
      *
               EVALUATE    BUNBEN-TEISYUTUBIKBN
               WHEN    "1"
                   MOVE    "１０日請求" TO BDRES-TEISYUTUBIKBN-NAME
               WHEN    "2"
                   MOVE    "２５日請求" TO BDRES-TEISYUTUBIKBN-NAME
               END-EVALUATE
           END-IF
      *
           IF    ( BUNBEN-SKYKBN       NOT =   SPACE )
      *
               MOVE    "請求区分"      TO  BDRES-SKYKBN-LBL
               MOVE    BUNBEN-SKYKBN   TO  BDRES-SKYKBN-DATA
      *
               EVALUATE    BUNBEN-SKYKBN
               WHEN    "0"
                   MOVE    "請求しない" TO BDRES-SKYKBN-NAME
               WHEN    "1"
                   MOVE    "未請求"    TO  BDRES-SKYKBN-NAME
               WHEN    "2"
                   MOVE    "請求済"    TO  BDRES-SKYKBN-NAME
               END-EVALUATE
      *
           END-IF
      *
           IF    ( BUNBEN-BUNBENKBN    NOT =   SPACE )
      *
               MOVE    "分娩区分"      TO  BDRES-BUNBENKBN-LBL
               MOVE    BUNBEN-BUNBENKBN TO BDRES-BUNBENKBN-DATA
      *
               EVALUATE    BUNBEN-BUNBENKBN
               WHEN    "1"
                   MOVE    "正常分娩"  TO  BDRES-BUNBENKBN-NAME
               WHEN    "2"
                   MOVE    "異常分娩"  TO  BDRES-BUNBENKBN-NAME
               END-EVALUATE
           END-IF
      *
           IF    ( BUNBEN-TEISYUTUSAKI NOT =   SPACE )
      *
               MOVE    "提出先"        TO  BDRES-TEISYUTUSAKI-LBL
               MOVE    BUNBEN-TEISYUTUSAKI
                                       TO  BDRES-TEISYUTUSAKI-DATA
      *
               EVALUATE    BUNBEN-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "支払基金"
                                       TO  BDRES-TEISYUTUSAKI-NAME
               WHEN    "2"
                   MOVE    "国保連合会"
                                       TO  BDRES-TEISYUTUSAKI-NAME
               END-EVALUATE
      *
           END-IF
      *
           IF    ( BUNBEN-ZAIWEEK NOT  =   ZERO )
      *
               MOVE    "在胎週数"      TO  BDRES-ZAIWEEK-LBL
               MOVE    BUNBEN-ZAIWEEK  TO  WRK-NUM2Z
               MOVE    WRK-NUM2Z       TO  BDRES-ZAIWEEK-DATA
               MOVE    "週"            TO  BDRES-ZAIWEEK-NAME
      *
           END-IF
      *
           IF    ( BUNBEN-BIRTHDAY NOT =   SPACE )
               MOVE    "出産年月日"    TO  BDRES-BIRTHDAY-LBL
               MOVE    BUNBEN-BIRTHDAY TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  BDRES-BIRTHDAY-DATA
           END-IF
      *
           IF    ( BUNBEN-SIZANKBN NOT =   SPACE )
      *
               MOVE    "死産有無"      TO  BDRES-SIZANKBN-LBL
               MOVE    BUNBEN-SIZANKBN TO  BDRES-SIZANKBN-DATA
      *
               EVALUATE    BUNBEN-SIZANKBN
               WHEN    "1"
                   MOVE    "死産"      TO  BDRES-SIZANKBN-NAME
               WHEN    "2"
                   MOVE    "死産でない" TO  BDRES-SIZANKBN-NAME
               WHEN    "3"
                   MOVE    "混在"      TO  BDRES-SIZANKBN-NAME
               END-EVALUATE
      *
           END-IF
      *
           IF    ( BUNBEN-BIRTHCNT NOT =   ZERO )
               MOVE    "出産数"        TO  BDRES-BIRTHCNT-LBL
               MOVE    BUNBEN-BIRTHCNT TO  WRK-NUM2Z
               MOVE    WRK-NUM2Z       TO  BDRES-BIRTHCNT-DATA
               MOVE    "人"            TO  BDRES-BIRTHCNT-NAME
           END-IF
      *
           IF    ( BUNBEN-SANKAKBN NOT =   SPACE )
      *
               MOVE    "産科医療補償制度"  TO  BDRES-SANKAKBN-LBL
               MOVE    BUNBEN-SANKAKBN     TO  BDRES-SANKAKBN-DATA
      *
               EVALUATE    BUNBEN-SANKAKBN
               WHEN    "1"
                   MOVE    "対象分娩"
                                   TO  BDRES-SANKAKBN-NAME
               WHEN    "2"
                   MOVE    "対象分娩でない"
                                   TO  BDRES-SANKAKBN-NAME
               WHEN    "3"
                   MOVE    "混在"  TO  BDRES-SANKAKBN-NAME
               END-EVALUATE
      *
           END-IF
      *
           IF    ( BUNBEN-UPDKBN       NOT =   "3" )
               MOVE    "入院日数"      TO  BDRES-NYUIN-DAY-LBL
               MOVE    BUNBEN-NYUIN-DAY TO WRK-NUM3Z
               MOVE    WRK-NUM3Z       TO  BDRES-NYUIN-DAY-DATA
               MOVE    "日"            TO  BDRES-NYUIN-DAY-NAME
      *
               MOVE    "入院料"            TO  BDRES-NYU-MONEY-LBL
               MOVE    BUNBEN-NYU-MONEY    TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-NYU-MONEY-DATA
               MOVE    "円"                TO  BDRES-NYU-MONEY-NAME
      *
               MOVE    "室料差額"          TO  BDRES-RMSGK-MONEY-LBL
               MOVE    BUNBEN-RMSGK-MONEY  TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-RMSGK-MONEY-DATA
               MOVE    "円"                TO  BDRES-RMSGK-MONEY-NAME
      *
               MOVE    "分娩介助料"        TO  BDRES-KAIJO-MONEY-LBL
               MOVE    BUNBEN-KAIJO-MONEY  TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-KAIJO-MONEY-DATA
               MOVE    "円"                TO  BDRES-KAIJO-MONEY-NAME
      *
               MOVE    "分娩料"            TO  BDRES-BUNBEN-MONEY-LBL
               MOVE    BUNBEN-BUNBEN-MONEY TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-BUNBEN-MONEY-DATA
               MOVE    "円"                TO  BDRES-BUNBEN-MONEY-NAME
      *
               MOVE    "新生児管理保育料"  TO  BDRES-SHIN-MONEY-LBL
               MOVE    BUNBEN-SHIN-MONEY   TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-SHIN-MONEY-DATA
               MOVE    "円"                TO  BDRES-SHIN-MONEY-NAME
      *
               MOVE    "検査・薬剤料"      TO  BDRES-KNS-MONEY-LBL
               MOVE    BUNBEN-KNS-MONEY    TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-KNS-MONEY-DATA
               MOVE    "円"                TO  BDRES-KNS-MONEY-NAME
      *
               MOVE    "処置・手当料"      TO  BDRES-SYC-MONEY-LBL
               MOVE    BUNBEN-SYC-MONEY    TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-SYC-MONEY-DATA
               MOVE    "円"                TO  BDRES-SYC-MONEY-NAME
      *
               MOVE    "産科医療補償制度掛金"
                                           TO  BDRES-SNK-MONEY-LBL
               MOVE    BUNBEN-SNK-MONEY    TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-SNK-MONEY-DATA
               MOVE    "円"                TO  BDRES-SNK-MONEY-NAME
      *
               MOVE    "その他"            TO  BDRES-ETC-MONEY-LBL
               MOVE    BUNBEN-ETC-MONEY    TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-ETC-MONEY-DATA
               MOVE    "円"                TO  BDRES-ETC-MONEY-NAME
      *
               MOVE    "一部負担金等"      TO  BDRES-ICBFTN-MONEY-LBL
               MOVE    BUNBEN-ICBFTN-MONEY TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-ICBFTN-MONEY-DATA
               MOVE    "円"                TO  BDRES-ICBFTN-MONEY-NAME
      *
               MOVE    "妊婦合計負担額"    TO  BDRES-NINPUFTN-MONEY-LBL
               MOVE    BUNBEN-NINPUFTN-MONEY TO WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-NINPUFTN-MONEY-DATA
               MOVE    "円"                TO  BDRES-NINPUFTN-MONEY-NAME
      *
               MOVE    "代理受取額"        TO  BDRES-DAIRI-MONEY-LBL
               MOVE    BUNBEN-DAIRI-MONEY  TO  WRK-NUM7Z
               MOVE    WRK-NUM7Z           TO  BDRES-DAIRI-MONEY-DATA
               MOVE    "円"                TO  BDRES-DAIRI-MONEY-NAME
      *
           END-IF
      *
           IF    ( BUNBEN-BIKOU        NOT =   SPACE )
               MOVE    "備考欄"        TO  BDRES-BIKOU-LBL
               MOVE    BUNBEN-BIKOU    TO  BDRES-BIKOU-DATA
           END-IF
      *
           MOVE    BUNBEN-HKNJANUM     TO  BDRES-HKNJANUM
           MOVE    HKNJA-HKNJANAME     TO  BDRES-HKNJANAME
           MOVE    BUNBEN-KIGO         TO  BDRES-KIGO
           MOVE    BUNBEN-NUM          TO  BDRES-NUM
           MOVE    BUNBEN-HONKZKKBN    TO  BDRES-HONKZKKBN
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       290-ORDER-INSERT-SEC          SECTION.
      *
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SUID-UID            TO  ORDER-KARTE-KEY
           MOVE    1                   TO  ORDER-ORDERNUM
           MOVE    "1"                 TO  ORDER-NYUGAIKBN
           MOVE    SPA-PTNUM           TO  ORDER-PTNUM
           MOVE    SPA-PTID            TO  ORDER-PTID
           MOVE    REG-NYUINYMD        TO  ORDER-SRYYMD
           MOVE    SMCNDATE-YMD        TO  ORDER-UKEYMD
           MOVE    SMCNDATE-HMS        TO  ORDER-UKEHMS
           MOVE    REG-ORDERID         TO  ORDER-ORDERID
      *
           MOVE    REG-ERRCD (1)       TO  ORDER-ERRCD1
      *    MOVE    REG-ERRCD (2)       TO  ORDER-ERRCD2
      *
           MOVE    1                   TO  ORDER-STATUS
      *
           MOVE    2                   TO  ORDER-KEYGENERATE
           MOVE    SPACE               TO  ORDER-TERMID
           MOVE    "api_"              TO  ORDER-OPID
           MOVE    SPA-OPID            TO  ORDER-OPID(5:)
           MOVE    SMCNDATE-YMD        TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    SMCNDATE-YMD        TO  ORDER-UPHMS
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       290-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  BDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  BDRES-INFORMATION-TIME
      *
           IF    ( REG-ERR-MAX     =   ZERO )
               MOVE    1           TO  REG-ERR-MAX
               MOVE    "0000"      TO  REG-ERRCD (1)
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   REG-ERR-MAX )
      *
               MOVE    REG-ERRCD  (IDX1)
                                       TO  WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD
                               TO  BDRES-API-RESULT     (IDX1)
               MOVE    WRK-ERRMSG
                               TO  BDRES-API-RESULT-MSG (IDX1)
      *
           END-PERFORM
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           INITIALIZE                      SODRS03-AREA
           MOVE    WRK-ERRCD           TO  SODRS03-ERRCD
           CALL    "ORCSODRS03"            USING
                                           SODRS03-AREA
           MOVE    SODRS03-ERRCD       TO  WRK-ERRCD
           MOVE    SODRS03-ERRMSG      TO  WRK-ERRMSG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( I0API02-BODY     NOT =   LOW-VALUE )
               DISPLAY "birthdeliveryv2r object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_birthdeliveryv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    I0API02-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "private_objects"   TO  APIREQ-RECORDNAME
           MOVE    "XMLREAD"       TO  MCP-FUNC
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "8098"           TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  I0API02-BODY
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
                                       TO  BDREQ-PRIVATE-OBJECTS
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    BDRES-INFORMATION-DATE
                               TO  BDRES-INFORMATION-DATE
      *
           MOVE    BDRES-INFORMATION-TIME
                               TO  BDRES-INFORMATION-TIME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   REG-ERR-MAX )
      *
               MOVE    BDRES-API-RESULT     (IDX1)
                               TO  BDRES-API-RESULT (IDX1)
               MOVE    BDRES-API-RESULT-MSG (IDX1)
                               TO  BDRES-API-RESULT-MSG (IDX1)
      *
           END-PERFORM
      *
           IF    ( BDREQ-REQNUM    NOT =   SPACE )
               MOVE    "リクエスト番号"
                               TO  BDRES-REQNUM-LBL
      *
               MOVE    BDREQ-REQNUM
                               TO  BDRES-REQNUM-DATA
           END-IF
      *
           EVALUATE    BDREQ-REQNUM
           WHEN    "01"
               MOVE    "照会"  TO  BDRES-REQNUM-NAME
           WHEN    "02"
               MOVE    "登録"  TO  BDRES-REQNUM-NAME
           END-EVALUATE
      *
           PERFORM 9001-XML-WRITE-SEC
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       9001-XML-WRITE-SEC             SECTION.
      *
           IF    ( I0API02-BODY    NOT =   LOW-VALUE )
               DISPLAY "birthdeliveryv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_birthdeliveryv2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    BDRES-PRIVATE-OBJECTS
                                           TO  APIREQ-PATIENTINFOREQ
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  I0API02-BODY
               MOVE    "application/xml"   TO  I0API02-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       9001-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC           SECTION.
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
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY39-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    REG-NYUINYMD        TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY39-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
      *
           INITIALIZE                  BUNBEN-REC
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    TRRK-PTID       TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    TRRK-RRKNUM     TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴検索処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-PTID            TO  KYUSEI-PTID
           MOVE    REG-SRYYMD          TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-KYUSEIRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴FETCH処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           .
       900-KYUSEIRRK-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタ検索処理
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    BUNBEN-HKNJANUM     TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           ELSE
               MOVE    1               TO  FLG-HKNJAINF
               INITIALIZE                  HKNJAINF-REC
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
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
      *    エラー編集処理
      *****************************************************************
       990-ERROR-EDIT-SEC              SECTION.
      *
           IF    ( REG-ERR-MAX     <   CONST-ERR-MAX )
      *
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   CONST-ERR-MAX )
                        OR   ( REG-ERRCD  (IDX8)
                                       =   WRK-ERRCD )
      *
                   IF    ( REG-ERRCD  (IDX8)   =   SPACE )
                       MOVE    WRK-ERRCD       TO  REG-ERRCD  (IDX8)
                       MOVE    IDX8            TO  REG-ERR-MAX
                       MOVE    CONST-ERR-MAX   TO  IDX8
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           .
       990-ERROR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "8099" )
               MOVE    404             TO  I0API02-HTTPSTATUS
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
           MOVE    WRK-DATE        TO  BDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  BDRES-INFORMATION-TIME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   REG-ERR-MAX )
      *
               MOVE    REG-ERRCD  (IDX1)
                                       TO  WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD
                               TO  BDRES-API-RESULT     (IDX1)
               MOVE    WRK-ERRMSG
                               TO  BDRES-API-RESULT-MSG (IDX1)
      *
           END-PERFORM
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
