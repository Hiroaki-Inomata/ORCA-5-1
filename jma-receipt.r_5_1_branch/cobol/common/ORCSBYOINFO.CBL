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
       PROGRAM-ID.             ORCSBYOINFO.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 病名一覧返却サブ
      *  管理者            : 
      *  11/08/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    16/02/29  合併症に係わる情報追加対応
      *                                     原疾患区分、合併症区分
      *  04.08.02    NACL-藤原    16/09/08  全ての患者病名情報の取得対応
      *  04.08.03    NACL-藤原    17/05/15  補足コメントコード返却対応
      *
      *  05.00.01    NACL-藤原    19/05/14  退院証明書返却対応
      *  05.00.02    NACL-藤原    19/06/17  第三者行為返却対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-YM              PIC 9(06).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03 WRK-INDEX            PIC 9(04).
      *
           03  WRK-LENGTH          PIC 9(02).
      *
       01  KEY-AREA.
           03 KEY-N-SRYYM          PIC X(06).
           03 KEY-O-SRYYM          PIC X(06).
      *
      *    ワーク患者病名
       01  WRK-PTBYOMEI-TABLE.
         02  WRK-PTBYOMEI-T        OCCURS  200.
           COPY    "CPPTBYOMEI.INC"    REPLACING   //PTBYO-//
                                       BY          //WRK-PTBYO-//.
         02  WRK-PTBYOMEI-MAX      PIC 9(03).
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
       01  WRK-CON-AREA.
           03  WRK-CONS-MAX        PIC 9(03)   VALUE   200.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡領域
      ****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSBYOINFO.INC".
           COPY    "COMMON-SPA".
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSBYOINFOAREA
                                   SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           DISPLAY "SBYOINFO-RC=" SBYOINFO-RC
            .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "SRYYM =" SBYOINFO-SRYYM
           DISPLAY "PTNUM =" SBYOINFO-PTNUM
           DISPLAY "SELECT-MODE=" SBYOINFO-SELECT-MODE
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
                                       KEY-AREA
                                       WRK-PTBYOMEI-TABLE
      *
           IF      SBYOINFO-PTID   =   ZERO
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    SBYOINFO-PTNUM  TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"     USING   ORCSPTNUMAREA
                                               SPA-AREA
               MOVE    SPTNUM-PTID     TO  SBYOINFO-PTID
           END-IF
           .
       100-INIT-EXT.
      *
           EXIT
           .
      *     
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      * 
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    SBYOINFO-PTID   TO  PTBYO-PTID
           MOVE    SBYOINFO-SRYYM  TO  PTBYO-SRYYMD (1:6)
           MOVE    "31"            TO  PTBYO-SRYYMD (7:2)
           MOVE    "%%"            TO  PTBYO-SRYKA
           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
           IF      SBYOINFO-SELECT-MODE
                                   =   "All"
               MOVE    "key50"         TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key37"         TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL       FLG-PTBYOMEI    =   1
                           OR          SBYOINFO-RC     >   ZERO
               PERFORM 2001-PTBYOMEI-HENSYU-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      **     IF      FLG-PTBYOMEI        =   ZERO
      **         MOVE    9                   TO  SBYOINFO-RC
      **     END-IF
           .
       200-MAIN-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    患者病名編集処理
      *****************************************************************
       2001-PTBYOMEI-HENSYU-SEC        SECTION.
      *
           INITIALIZE                      WRK-PTBYOMEI-TABLE
      *
      *    診療年月毎に処理
           MOVE    KEY-N-SRYYM         TO  KEY-O-SRYYM
           PERFORM         UNTIL   FLG-PTBYOMEI    =   1
                           OR      KEY-N-SRYYM NOT =   KEY-O-SRYYM
               MOVE    ZERO                TO  FLG-CHK
      *
      *        対象の病名かチェック
               IF      SBYOINFO-SELECT-MODE
                                           =   "All"
                   CONTINUE
               ELSE
                   IF      PTBYO-SRYYMD (1:6)  =   SBYOINFO-SRYYM
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIKBN       =   SPACE
                           CONTINUE
                       ELSE
                           MOVE    PTBYO-TENKIYMD(1:6)
                                                   TO  WRK-YM
                           IF      WRK-YM          >=  SBYOINFO-SRYYM
                               CONTINUE
                           ELSE
                               MOVE    1               TO  FLG-CHK
                           END-IF
                      END-IF
                   END-IF
               END-IF    
      *
      *!レセプト表示は判定しない
      *    レセプト表示フラグ
      **** IF      PTBYO-REZEFLG       =   "1"
      *        MOVE    1                   TO  FLG-CHK
      *    END-IF
      *
      *    レセプト表示月数
      *    IF      PTBYO-REZEMM        >   ZERO
      *        INITIALIZE                      STS-AREA-DAY
      *        INITIALIZE                      LNK-DAY6-AREA
      *        MOVE    "61"                TO  LNK-DAY6-IRAI
      *        MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
      *        MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
      *        COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
      *        CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                            LNK-DAY6-AREA
      *        MOVE    LNK-DAY6-KEISAN (1:6)
      *                                    TO  WRK-YM
      *        IF      WRK-YM              >=  SBYOINFO-SRYYM
      *            CONTINUE
      *        ELSE
      *            MOVE    1                   TO  FLG-CHK
      *        END-IF
      *****END-IF
      *!
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1                   TO  WRK-PTBYOMEI-MAX
                   IF      WRK-PTBYOMEI-MAX    >   WRK-CONS-MAX
                       MOVE    9                   TO  SBYOINFO-RC
                       GO  TO  2001-PTBYOMEI-HENSYU-EXT
                   ELSE
                       MOVE    PTBYOMEI-REC        TO  WRK-PTBYOMEI-T 
                                                   (WRK-PTBYOMEI-MAX)
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           COMPUTE WRK-INDEX   =   IDX +   WRK-PTBYOMEI-MAX
           IF      WRK-INDEX           >   WRK-CONS-MAX
                   MOVE    9                   TO  SBYOINFO-RC
                   GO  TO  2001-PTBYOMEI-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-PTBYOMEI-MAX
               ADD     1                   TO  IDX
      *
               MOVE    WRK-PTBYO-SRYKA          (IDXX)
                                           TO  SBYOINFO-SRYKA      (IDX)
               MOVE    WRK-PTBYO-SRYYMD        (IDXX)
                                           TO  SBYOINFO-SRYYMD     (IDX)
               MOVE    WRK-PTBYO-BYOMEIINPUTCD-G (IDXX)
                                           TO  SBYOINFO-BYOMEIINPUTCD-G
                                                                   (IDX)
               MOVE    WRK-PTBYO-BYOMEICD-G    (IDXX)
                                           TO  SBYOINFO-BYOMEICD-G (IDX)
               MOVE    WRK-PTBYO-BYOMEI        (IDXX)
                                           TO  SBYOINFO-BYOMEI     (IDX)
               MOVE    WRK-PTBYO-CHARTBYOMEI   (IDXX)
                                           TO  SBYOINFO-CHARTBYOMEI(IDX)
               MOVE    WRK-PTBYO-HOSOKU-COMMENT(IDXX) 
                                           TO  SBYOINFO-HOSOKU-COMMENT
                                                                   (IDX)
               MOVE    WRK-PTBYO-HOSOKUCD-G    (IDXX)
                                           TO  SBYOINFO-HOSOKUCD-G (IDX)
               PERFORM 20011-HOSOKUCD-COMMENT-HEN-SEC
               MOVE    WRK-PTBYO-TENKIYMD      (IDXX)
                                           TO  SBYOINFO-TENKIYMD   (IDX)
               MOVE    WRK-PTBYO-TENKIKBN      (IDXX)
                                           TO  SBYOINFO-TENKIKBN   (IDX)
               MOVE    WRK-PTBYO-UTAGAIFLG     (IDXX)
                                           TO  SBYOINFO-UTAGAIFLG  (IDX)
               MOVE    WRK-PTBYO-SYUBYOFLG     (IDXX)
                                           TO  SBYOINFO-SYUBYOFLG  (IDX)
               MOVE    WRK-PTBYO-MANSEIKBN     (IDXX)
                                           TO  SBYOINFO-MANSEIKBN  (IDX)
               MOVE    WRK-PTBYO-NYUGAIKBN     (IDXX)
                                           TO  SBYOINFO-NYUGAIKBN  (IDX)
               MOVE    WRK-PTBYO-HKNCOMBI      (IDXX)
                                           TO  SBYOINFO-HKNCOMBI   (IDX)
               MOVE    WRK-PTBYO-REZEFLG       (IDXX)
                                           TO  SBYOINFO-REZEFLG    (IDX)
               MOVE    WRK-PTBYO-REZEMM        (IDXX)
                                           TO  SBYOINFO-REZEMM     (IDX)
               MOVE    WRK-PTBYO-HKNBYOMEIFLG  (IDXX)
                                           TO  SBYOINFO-HKNBYOMEIFLG
                                                                   (IDX)
               MOVE    WRK-PTBYO-BYOMEIHENFLG  (IDXX)
                                           TO  SBYOINFO-BYOMEIHENFLG
                                                                   (IDX)
               MOVE    WRK-PTBYO-CLASS1        (IDXX)
                                           TO  SBYOINFO-CLASS1     (IDX)
               MOVE    WRK-PTBYO-CLASS2        (IDXX)
                                           TO  SBYOINFO-CLASS2     (IDX)
               MOVE    WRK-PTBYO-DISCHARGEFLG  (IDXX)
                                           TO  SBYOINFO-DISCHARGEFLG
                                                                   (IDX)
               MOVE    WRK-PTBYO-KOHID   (IDXX)
                                           TO  SBYOINFO-KOHID      (IDX)
      *??
               DISPLAY "IDX=" IDX " SRYKA=" SBYOINFO-SRYKA      (IDX)
                       " SRYYMD=" SBYOINFO-SRYYMD     (IDX)
                       " BYOMEI=" SBYOINFO-BYOMEI     (IDX)
      *??
      *
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       21
                       OR      SBYOINFO-BYOMEICD (IDX IDY)
                                       =   SPACE
                   INITIALIZE                      BYOMEI-REC
                   MOVE    SBYOINFO-BYOMEICD (IDX IDY)
                                               TO  BYO-BYOMEICD
                   IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
                   AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                         ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *                予約病名コード検索
                       SET     TBL-BYOMEICD-IDX  TO  1
                       SEARCH  TBL-BYOMEICD-OCC
                                           VARYING TBL-BYOMEICD-IDX
                           AT  END
                               MOVE    "1"             TO     
                                               SBYOINFO-FLG (IDX IDY)
                           WHEN    BYO-BYOMEICD    =
                                               TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                               MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                                   TO
                                           SBYOINFO-BYOMEI-T (IDX IDY)
                               MOVE    ZERO        TO  FLG-BYOMEI
                       END-SEARCH
                   ELSE
      *                病名マスタ検索
                       IF      BYO-BYOMEICD    =   "0000999"
                           MOVE    SPACE           TO
                                           SBYOINFO-BYOMEI-T (IDX IDY)
                       ELSE 
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                           SBYOINFO-BYOMEI-T (IDX IDY)
                               IF      BYO-HAISIYMD    =   "99999999"
                                                       OR  SPACE
                                   CONTINUE
                               ELSE
                                   MOVE    "2"             TO  
                                               SBYOINFO-FLG (IDX IDY)
                               END-IF
                           ELSE
                               MOVE    "1"             TO     
                                               SBYOINFO-FLG (IDX IDY)
                           END-IF
                       END-IF
                   END-IF
      *??
               DISPLAY "IDY=" IDY
                  " BYOMEICD=" SBYOINFO-BYOMEICD (IDX IDY)
                  " FLG="          SBYOINFO-FLG (IDX IDY)
                  " BYOMEI="   SBYOINFO-BYOMEI-T (IDX IDY)
      *??
               END-PERFORM
           END-PERFORM
      * 
           .
       2001-PTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコードからコメント内容を編集
      *****************************************************************
       20011-HOSOKUCD-COMMENT-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       3
                   OR      SBYOINFO-HOSOKUCD (IDX IDX1)
                                   =       SPACE
               MOVE    ZERO                TO  WRK-LENGTH
               PERFORM VARYING IDX2    FROM    10  BY  -1
                       UNTIL   IDX2    <       1
                   IF      SBYOINFO-HOSOKUCD (IDX IDX1) (IDX2:1)
                                       NOT =   SPACE
                       MOVE    IDX2            TO  WRK-LENGTH
                       MOVE    1               TO  IDX2
                   END-IF
               END-PERFORM
      *
               IF      WRK-LENGTH          =   7
                   MOVE    ZERO                  TO  FLG-BYOMEI
      *
                   INITIALIZE                  BYOMEI-REC
                   MOVE    SBYOINFO-HOSOKUCD (IDX IDX1)
                                               TO  BYO-BYOMEICD
      *
                   IF  (   BYO-BYOMEICD (1:3)  =   "ZZZ"    )
                   AND ( ( BYO-BYOMEICD (4:1)  <   "1" ) OR 
                         ( BYO-BYOMEICD (4:1)  =   "C" )    )
      *                予約病名コード検索
                       SET     TBL-BYOMEICD-IDX    TO  1
                       SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                           AT  END
                               MOVE    1           TO  FLG-BYOMEI
                           WHEN    BYO-BYOMEICD    =
                                   TBL-YYK-BYOMEICD (TBL-BYOMEICD-IDX)
                               MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                                       TO
                                   SBYOINFO-HOSOKUCD-COMMENT (IDX IDX1)
                       END-SEARCH
                   ELSE
      *                病名マスタ読込
                       IF      BYO-BYOMEICD    =   "0000999"
                           CONTINUE
                       ELSE
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                   SBYOINFO-HOSOKUCD-COMMENT (IDX IDX1)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-BYOMEI          =   1 )
               OR    ( WRK-LENGTH      NOT =   7 )
      *            自院病名読込
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    SBYOINFO-HOSOKUCD (IDX IDX1)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USERBYOMEI-READ-SEC
                   IF      FLG-USERBYOMEI      =   ZERO
                       MOVE    USBYO-BYOMEI        TO
                                   SBYOINFO-HOSOKUCD-COMMENT (IDX IDX1)
                   END-IF
               END-IF
           END-PERFORM
           .
       20011-HOSOKUCD-COMMENT-HEN-XT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ読込
      *****************************************************************
       900-PTBYO-READ-SEC              SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    PTBYO-SRYYMD (1:6)
                                       TO  KEY-N-SRYYM
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    HIGH-VALUE      TO  KEY-N-SRYYM
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスタ読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BYOMEI
                   MOVE    MCPDATA-REC     TO  BYOMEI-REC
               ELSE
                   MOVE    1               TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名読込
      *****************************************************************
       900-USERBYOMEI-READ-SEC         SECTION.
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  USERBYOMEI-REC
                   MOVE    ZERO                TO  FLG-USERBYOMEI
               ELSE
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    1                   TO  FLG-USERBYOMEI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                      USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USERBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
