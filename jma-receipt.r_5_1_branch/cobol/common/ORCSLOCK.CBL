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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSLOCK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 排他管理
      *  コンポーネント名  : 排他制御ＤＢ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/08/08    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.00    NACL-多々納  07/01/23  患者登録、診療行為、会計照会の排他修正
      *  03.04.00    NACL-太田    07/02/23  データチェックの排他を追加
      *  03.05.00    NACL-太田    07/05/09  グループ診療対応
      *
      *  04.05.01    NACL-竹田    10/02/27  監視ログ出力対応
      *  04.08.00    NACL-多々納  17/04/XX  受付の排他追加
      *****************************************************************
001200*
001300 ENVIRONMENT             DIVISION.
001400 CONFIGURATION           SECTION.
001500 INPUT-OUTPUT            SECTION.
001600 FILE-CONTROL.
007000 DATA                    DIVISION.
010200 WORKING-STORAGE         SECTION.
      *
      *    フラグ領域
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-LOCK                                PIC 9(01).
           03  FLG-MAP                                 PIC 9(01).
           03  WRK-LOCK-MATCH-FLG                      PIC S9(09).
           03  FLG-ALLCHK                              PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC S9(03). 
           03  IDY                                     PIC S9(03). 
           03  IDZ                                     PIC S9(03). 
           03  IDZZ                                    PIC S9(03). 
      *
      *
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
      *シェルＩＤ
           03  WRK-LDNAME              PIC X(40).
           03  WRK-LD                  PIC X(20).
           03  WRK-WINDOW              PIC X(10).
           03  WRK-TERMID              PIC X(64).
           03  WRK-OPID                PIC X(16).
           03  WRK-IPV4-TABLE.
             05 WRK-IPV4-TBL           PIC 9(03)  OCCURS 4.
      *H27.2  API用
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
           COPY    "CPLOCKCONST.INC".
      *
024200 01  WRK-MSG-AREA.
      **     03 FILLER                    PIC X(04)  VALUE " T:[".
      **     03 WRK-MSG-T1                PIC ZZZ.
      **     03 FILLER                    PIC X(01)  VALUE ".".
      **     03 WRK-MSG-T2                PIC ZZZ.
      **     03 FILLER                    PIC X(01)  VALUE ".".
      **     03 WRK-MSG-T3                PIC ZZZ.
      **     03 FILLER                    PIC X(01)  VALUE ".".
      **     03 WRK-MSG-T4                PIC ZZZ.
      **     03 FILLER                    PIC X(01)  VALUE "]".
           03 FILLER                    PIC X(07)  VALUE " OPID:[".
           03 WRK-MSG-O1                PIC X(16).
           03 FILLER                    PIC X(01)  VALUE "]".
      *
024200 01  CONSTANT-AREA.
           03 CNST-IPV4-2.
             05  CNST-IPV4-2-0          PIC X(01)  VALUE "0".
             05  CNST-IPV4-2-0-N        PIC 9(03)  VALUE   0.
             05  CNST-IPV4-2-1          PIC X(01)  VALUE "1".
             05  CNST-IPV4-2-1-N        PIC 9(03)  VALUE  16.
             05  CNST-IPV4-2-2          PIC X(01)  VALUE "2".
             05  CNST-IPV4-2-2-N        PIC 9(03)  VALUE  32.
             05  CNST-IPV4-2-3          PIC X(01)  VALUE "3".
             05  CNST-IPV4-2-3-N        PIC 9(03)  VALUE  48.
             05  CNST-IPV4-2-4          PIC X(01)  VALUE "4".
             05  CNST-IPV4-2-4-N        PIC 9(03)  VALUE  64.
             05  CNST-IPV4-2-5          PIC X(01)  VALUE "5".
             05  CNST-IPV4-2-5-N        PIC 9(03)  VALUE  80.
             05  CNST-IPV4-2-6          PIC X(01)  VALUE "6".
             05  CNST-IPV4-2-6-N        PIC 9(03)  VALUE  96.
             05  CNST-IPV4-2-7          PIC X(01)  VALUE "7".
             05  CNST-IPV4-2-7-N        PIC 9(03)  VALUE 112.
             05  CNST-IPV4-2-8          PIC X(01)  VALUE "8".
             05  CNST-IPV4-2-8-N        PIC 9(03)  VALUE 128.
             05  CNST-IPV4-2-9          PIC X(01)  VALUE "9".
             05  CNST-IPV4-2-9-N        PIC 9(03)  VALUE 144.
             05  CNST-IPV4-2-A          PIC X(01)  VALUE "A".
             05  CNST-IPV4-2-A-N        PIC 9(03)  VALUE 160.
             05  CNST-IPV4-2-B          PIC X(01)  VALUE "B".
             05  CNST-IPV4-2-B-N        PIC 9(03)  VALUE 176.
             05  CNST-IPV4-2-C          PIC X(01)  VALUE "C".
             05  CNST-IPV4-2-C-N        PIC 9(03)  VALUE 192.
             05  CNST-IPV4-2-D          PIC X(01)  VALUE "D".
             05  CNST-IPV4-2-D-N        PIC 9(03)  VALUE 208.
             05  CNST-IPV4-2-E          PIC X(01)  VALUE "E".
             05  CNST-IPV4-2-E-N        PIC 9(03)  VALUE 224.
             05  CNST-IPV4-2-F          PIC X(01)  VALUE "F".
             05  CNST-IPV4-2-F-N        PIC 9(03)  VALUE 240.
           03 CNST-IPV4-2-R    REDEFINES    CNST-IPV4-2.
             05  CNST-IPV4-2-T              OCCURS     16.
               07  CNST-IPV4-2-X        PIC X(01).
               07  CNST-IPV4-2-N        PIC 9(03).
      *
           03 CNST-IPV4-1.
             05  CNST-IPV4-1-0          PIC X(01)  VALUE "0".
             05  CNST-IPV4-1-0-N        PIC 9(03)  VALUE   0.
             05  CNST-IPV4-1-1          PIC X(01)  VALUE "1".
             05  CNST-IPV4-1-1-N        PIC 9(03)  VALUE   1.
             05  CNST-IPV4-1-2          PIC X(01)  VALUE "2".
             05  CNST-IPV4-1-2-N        PIC 9(03)  VALUE   2.
             05  CNST-IPV4-1-3          PIC X(01)  VALUE "3".
             05  CNST-IPV4-1-3-N        PIC 9(03)  VALUE   3.
             05  CNST-IPV4-1-4          PIC X(01)  VALUE "4".
             05  CNST-IPV4-1-4-N        PIC 9(03)  VALUE   4.
             05  CNST-IPV4-1-5          PIC X(01)  VALUE "5".
             05  CNST-IPV4-1-5-N        PIC 9(03)  VALUE   5.
             05  CNST-IPV4-1-6          PIC X(01)  VALUE "6".
             05  CNST-IPV4-1-6-N        PIC 9(03)  VALUE   6.
             05  CNST-IPV4-1-7          PIC X(01)  VALUE "7".
             05  CNST-IPV4-1-7-N        PIC 9(03)  VALUE   7.
             05  CNST-IPV4-1-8          PIC X(01)  VALUE "8".
             05  CNST-IPV4-1-8-N        PIC 9(03)  VALUE   8.
             05  CNST-IPV4-1-9          PIC X(01)  VALUE "9".
             05  CNST-IPV4-1-9-N        PIC 9(03)  VALUE   9.
             05  CNST-IPV4-1-A          PIC X(01)  VALUE "A".
             05  CNST-IPV4-1-A-N        PIC 9(03)  VALUE  10.
             05  CNST-IPV4-1-B          PIC X(01)  VALUE "B".
             05  CNST-IPV4-1-B-N        PIC 9(03)  VALUE  11.
             05  CNST-IPV4-1-C          PIC X(01)  VALUE "C".
             05  CNST-IPV4-1-C-N        PIC 9(03)  VALUE  12.
             05  CNST-IPV4-1-D          PIC X(01)  VALUE "D".
             05  CNST-IPV4-1-D-N        PIC 9(03)  VALUE  13.
             05  CNST-IPV4-1-E          PIC X(01)  VALUE "E".
             05  CNST-IPV4-1-E-N        PIC 9(03)  VALUE 14.
             05  CNST-IPV4-1-F          PIC X(01)  VALUE "F".
             05  CNST-IPV4-1-F-N        PIC 9(03)  VALUE  15.
           03 CNST-IPV4-1-R    REDEFINES    CNST-IPV4-1.
             05  CNST-IPV4-1-T              OCCURS     16.
               07  CNST-IPV4-1-X        PIC X(01).
               07  CNST-IPV4-1-N        PIC 9(03).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
           COPY    "CPTBLLOCK.INC".
           COPY    "CPLDMAP.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      ***  COPY    "ORCA-DBPATH".
      **** COPY    "CPORCMCP.INC".
      *     
      **************************************************************************
      *
       LINKAGE                 SECTION.
      *   排他制御ＤＢ制御サブ
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
           COPY    "CPORCSLOCK.INC".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               MCPAREA
               SPA-AREA
               ORCSLOCKAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *???
      *???     
           INITIALIZE                  CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       INDEX-AREA
      *
           MOVE        SPACE           TO  SLOCK-INSERT
      *
           MOVE        ZERO            TO  SLOCK-RETURN  
      *
           MOVE    ZERO            TO  SPA-LOCK
      *H27.2  API対応
           MOVE    MCP-TERM        TO  WRK-MCP-TERM
      *
      *    処理モードのチェック
           EVALUATE    SLOCK-MODE
      *    チェック＆ＬＯＣＫ
               WHEN    1
      *            画面ＩＤより業務ＬＤを検索する
                   PERFORM 1101-LD-GET-SEC
      *            業務により排他フラグを無効にする
                   IF      FLG-ALLCHK      =   ZERO
                       IF  SPA-HAITAFLG    =      ZERO
                           MOVE     ZERO   TO     SLOCK-RETURN
                       ELSE
                           PERFORM 110-CHK-SEC
                       END-IF
                   ELSE
                       PERFORM 110-CHK-SEC
                   END-IF
      *    ＬＯＣＫ解除
               WHEN    2
                       PERFORM 120-RESET-SEC
      *    ＬＯＣＫリセット（端末ＩＤ毎）
               WHEN    3
                       PERFORM 130-CLEAR-SEC
      *    パラメータエラー
               WHEN    OTHER
                       MOVE  99            TO  SLOCK-RETURN  
                       MOVE  1             TO  FLG-END
           END-EVALUATE
      *???
      *    DISPLAY "ORCSLOCK ED RETRN =" SLOCK-RETURN "##"
      *    DISPLAY "ORCSLOCK ED SPA-HAITAFLG =" SPA-HAITAFLG "##"
      *    DISPLAY "ORCSLOCK ED MCP-WINDOW = [" MCP-WINDOW "]"
      *???     
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    排他制御チェック処理
      *****************************************************************
       110-CHK-SEC                 SECTION.
      *
      *    画面ＩＤより業務ＬＤを検索する
      *    (当面、氏名検索以外は、画面ＩＤの上１桁で取得）
      *!!  PERFORM 1101-LD-GET-SEC
           IF      WRK-LD          =   SPACE
                   MOVE  99        TO  SLOCK-RETURN
                   GO    TO        110-CHK-EXT
           END-IF
      *    他端末レコード存在チェック
      *    MOVE    MCP-TERM(5:)            TO  LOCK-TERMID
      ***  MOVE    MCP-TERM                TO  LOCK-TERMID
           MOVE    WRK-MCP-TERM            TO  LOCK-TERMID
grpsys     MOVE    SPA-HOSPNUM             TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           PERFORM 910-LOCK-INV2-SEC
           IF       FLG-LOCK       NOT  =  ZERO
               CONTINUE
      *        MOVE  MCP-RC            TO  SLOCK-RETURN
           ELSE
               PERFORM VARYING      IDX FROM  1   BY   1
                       UNTIL      (IDX       >   50)  OR
                                  (FLG-LOCK  =   1)
                 PERFORM  1101-LOCK-CHEK1-SEC
                 IF  FLG-MAP          =     1
                     MOVE     99            TO     IDX
                 ELSE
                     PERFORM  910-LOCK-READ2-SEC
                 END-IF
               END-PERFORM
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      * 排他
           IF  FLG-MAP          =     1
      *        PERFORM  1102-TERMID-BUNKAI-SEC
               MOVE     WRK-TERMID    TO     SLOCK-TERMID
               MOVE     WRK-OPID      TO     SLOCK-OPID
                                             WRK-MSG-O1
               MOVE     10            TO     SLOCK-RETURN
               MOVE     WRK-MSG-AREA  TO     SLOCK-MSG
      *        表示のみの設定
               IF       FLG-ALLCHK    =   1
                   MOVE    1              TO  SPA-LOCK
               END-IF
           ELSE
      * チェックＯＫならば排他テーブル作成
               PERFORM  1101-LOCK-MAKE-SEC
      *    監視ログ出力
               CALL    "ORCSAUDIT"     USING
                                      "1"
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       ORCSLOCKAREA
                                       JOBKANRI-REC
           END-IF
      *
           .
       110-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    端末ＩＤ分解　処理
      *****************************************************************
       1102-TERMID-BUNKAI-SEC        SECTION.
      *
      *    DISPLAY "WRK-TERMID = " WRK-TERMID
      **     IF  WRK-TERMID(9:1)       =  SPACE
      *  For IPV4
      **         MOVE    1             TO IDZ
      **         MOVE    2             TO IDZZ
      **         PERFORM VARYING      IDX FROM  1   BY   1
      **                 UNTIL       (IDX       >   4)
      **           PERFORM  VARYING   IDY FROM  1   BY   1
      **                 UNTIL       (IDY       >   16)
      **             IF  WRK-TERMID(IDZ:1)      =   CNST-IPV4-2-X(IDY)
      **                 MOVE  CNST-IPV4-2-N(IDY)
      **                                        TO  WRK-IPV4-TBL(IDX)
      **             END-IF
      **           END-PERFORM
      **           PERFORM  VARYING   IDY FROM  1   BY   1
      **                 UNTIL       (IDY       >   16)
      **             IF  WRK-TERMID(IDZZ:1)    =   CNST-IPV4-1-X(IDY)
      **                 COMPUTE  WRK-IPV4-TBL(IDX) =
      **                          CNST-IPV4-1-N(IDY)     +
      **                          WRK-IPV4-TBL(IDX)
      **             END-IF
      **           END-PERFORM
      **           COMPUTE  IDZ        =  IDZ   +    2
      **           COMPUTE  IDZZ       =  IDZZ  +    2
      **         END-PERFORM
      *
      **       MOVE  WRK-IPV4-TBL(1) TO WRK-MSG-T4
      **         MOVE  WRK-IPV4-TBL(2) TO WRK-MSG-T3
      **         MOVE  WRK-IPV4-TBL(3) TO WRK-MSG-T2
      **         MOVE  WRK-IPV4-TBL(4) TO WRK-MSG-T1
      **     END-IF
           .
       1102-TERMID-BUNKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務LD取得　処理
      *****************************************************************
       1101-LD-GET-SEC               SECTION.
      *
      *    IF      MCP-WINDOW      =   "P97"
      *            MOVE  SPA-MOTOPG(1:1)   TO  WRK-WINDOW
      *    ELSE
      *            MOVE  MCP-WINDOW(1:1)   TO  WRK-WINDOW
      *    END-IF
           IF      MCP-WINDOW      =   "P97"
             IF    SPA-MOTOPG(1:1) =   "I"
                   MOVE  SPA-MOTOPG(1:2)   TO  WRK-WINDOW
             ELSE
                   MOVE  SPA-MOTOPG(1:1)   TO  WRK-WINDOW
             END-IF
           ELSE
             IF    MCP-WINDOW(1:1) =   "I"
                   MOVE  MCP-WINDOW(1:2)   TO  WRK-WINDOW
             ELSE
                   MOVE  MCP-WINDOW(1:1)   TO  WRK-WINDOW
             END-IF
           END-IF
      *
           SET     TBLLOCK-IDX             TO  1
           SEARCH  ORC-LOCKCONST-TX        VARYING   TBLLOCK-IDX
               AT   END
                   MOVE    ZERO               TO  FLG-ARI
                   MOVE    SPACE              TO  WRK-LD
                   MOVE    SPACE              TO  WRK-LDNAME
               WHEN   WRK-WINDOW          =   
                      ORC-LOCKCONST-WINDOW (TBLLOCK-IDX)
                   MOVE    1                  TO  FLG-ARI
                   MOVE    ORC-LOCKCONST-LD  (TBLLOCK-IDX)
                                              TO  WRK-LD
                   MOVE    ORC-LOCKCONST-NAME (TBLLOCK-IDX)
                                              TO  WRK-LDNAME
           END-SEARCH
      *
      *    業務が患者登録・診療行為・会計照会はすべて排他処理を行う
      *    データチェック業務もすべて排他処理を行う
           IF      WRK-LD              =   "ORCA12"
                                       OR  "ORCA21"
                                       OR  "ORCA24"
                                       OR  "ORCA31"
                                       OR  "ORCA32"
                                       OR  "ORCA41"
      *H29.4     受付も対象とする
                                       OR  "ORCA11"
               MOVE    1               TO  FLG-ALLCHK
           ELSE
               MOVE    ZERO            TO  FLG-ALLCHK
           END-IF
      *
           .
       1101-LD-GET-EXT.
           EXIT.
      *****************************************************************
      *    排他LD変換　処理
      *****************************************************************
       1101-LOCK-CHEK1-SEC           SECTION.
           MOVE    ZERO                TO  FLG-MAP
           PERFORM VARYING      IDY FROM  1   BY   1
                       UNTIL      (IDY       >   50)
             IF   (LOCK-FLG (IDY)   =     1)            AND
                  (LOCK-PTID (IDY)  =     SLOCK-PTID)
                   MOVE     WRK-LD        TO     LDMAP-LDNAME
                   MOVE     LOCK-LDNAME (IDY)
                                          TO     LDMAP-CHK-LDNAME (1)
grpsys             MOVE     SPA-HOSPNUM   TO  LDMAP-HOSPNUM
                   MOVE     TBL-LDMAP     TO  MCPDATA-REC
                   PERFORM  910-MAP-INV1-SEC
               IF  FLG-MAP          =     1
                   MOVE     LOCK-TERMID
                                          TO     WRK-TERMID
                   MOVE     LOCK-OPID
                                          TO     WRK-OPID
                   MOVE     99            TO     IDY
               END-IF
             END-IF
           END-PERFORM
      *
           .
       1101-LOCK-CHEK1-EXT.
          EXIT.
      *****************************************************************
      *    排他制御テーブル作成　　処理
      *****************************************************************
       1101-LOCK-MAKE-SEC            SECTION.
      *    自端末レコード存在チェック
      *    MOVE    MCP-TERM(5:)            TO  LOCK-TERMID
      **   MOVE    MCP-TERM                TO  LOCK-TERMID
           MOVE    WRK-MCP-TERM            TO  LOCK-TERMID
      *    DISPLAY "MAKE --> MCP-TERM = [" LOCK-TERMID "]"
grpsys     MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           PERFORM 910-LOCK-INV-SEC
           IF      FLG-LOCK       NOT  =  ZERO
      *    レコード追加処理
              INITIALIZE   TBL-LOCK
      *        MOVE  MCP-TERM(5:)          TO  LOCK-TERMID
      *        MOVE  MCP-TERM              TO  LOCK-TERMID
               MOVE  WRK-MCP-TERM          TO  LOCK-TERMID
             IF  SPA-OPID              NOT =   SPACE
               MOVE  SPA-OPID          TO  LOCK-OPID
             ELSE
               MOVE  MCP-USER          TO  LOCK-OPID
             END-IF
               MOVE  WRK-LD            TO  LOCK-LDNAME(01)
               MOVE  SLOCK-PTID        TO  LOCK-PTID(01)
               MOVE  "1"               TO  LOCK-FLG(01)
               ACCEPT LOCK-STARTDATE(01)   FROM  DATE
               ACCEPT LOCK-STARTTIME(01)   FROM  TIME
grpsys         MOVE  SPA-HOSPNUM       TO  LOCK-HOSPNUM
               MOVE  SLOCK-KARTE-UID   TO  LOCK-KARTE-UID
               MOVE  TBL-LOCK          TO  MCPDATA-REC
               PERFORM 910-LOCK-INS-SEC
      *
               MOVE  "1"               TO  SLOCK-INSERT
      *
           ELSE
      *    レコード更新処理
      *    (更新位置の取得)
               MOVE  ZERO              TO  IDZ
               PERFORM VARYING      IDX FROM  1   BY   1
                       UNTIL       (IDX     >     50)
                 IF  LOCK-LDNAME(IDX)       =     SPACE
                   IF  IDZ                  =     ZERO
                       MOVE  IDX            TO    IDZ
                   END-IF
                 END-IF
      *
                 IF  WRK-LD                 =     LOCK-LDNAME(IDX)
                     MOVE  IDX              TO    IDZ
                     MOVE  99               TO    IDX
                 END-IF
               END-PERFORM
               MOVE  1                      TO    WRK-LOCK-MATCH-FLG
               MOVE  WRK-LD                 TO    LOCK-LDNAME(IDZ)
             IF  SPA-OPID              NOT =   SPACE
               MOVE  SPA-OPID          TO  LOCK-OPID
             ELSE
               MOVE  MCP-USER          TO  LOCK-OPID
             END-IF
               MOVE  SLOCK-PTID             TO    LOCK-PTID(IDZ)
               ACCEPT LOCK-STARTDATE(IDZ)   FROM  DATE
               ACCEPT LOCK-STARTTIME(IDZ)   FROM  TIME
               MOVE  "1"                    TO    LOCK-FLG(IDZ)
grpsys         MOVE  SPA-HOSPNUM            TO  LOCK-HOSPNUM
               MOVE  SLOCK-KARTE-UID        TO  LOCK-KARTE-UID
               MOVE  TBL-LOCK               TO    MCPDATA-REC
               PERFORM 910-LOCK-UPD-SEC
           END-IF
      *
      *  データ作成処理（りんプロ）
      *    MOVE  "ORCA21"          TO  LDMAP-LDNAME
      *    MOVE  "ORCA12"          TO  LDMAP-CHK-LDNAME(01)
      *    MOVE  "ORCA21"          TO  LDMAP-CHK-LDNAME(02)
      *    MOVE  "ORCA22"          TO  LDMAP-CHK-LDNAME(03)
      *    MOVE  TBL-LDMAP         TO  MCPDATA-REC
      *    MOVE    "DBINSERT"          TO  MCP-FUNC
      *    MOVE    TBL-LDMAP            TO  MCPDATA-REC
      *    MOVE    PATH-TBL-LDMAP-KEY
      *                                TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC      
      *    IF      MCP-RC     =   ZERO
      *        CONTINUE
      *    ELSE
      *        DISPLAY "INSERT ERR KEY = [" MCP-RC "]"   
      *        MOVE  MCP-RC            TO  SLOCK-RETURN
      *    END-IF
      *
      *
           .
       1101-LOCK-MAKE-EXT.
          EXIT.
      *****************************************************************
      *    排他チェック　　処理２
      *****************************************************************
       11011-LOCK-CHEK2-SEC          SECTION.
      *
           .
       11011-LOCK-CHEK2-EXT.
          EXIT.
      *****************************************************************
      *    排他テーブル追加　処理
      *****************************************************************
       910-LOCK-INS-SEC              SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "INSERT ERR KEY = [" MCP-RC "]"   
               MOVE  MCP-RC            TO  SLOCK-RETURN
           END-IF
      *
           .
       910-LOCK-INS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    排他テーブル更新　処理
      *****************************************************************
       910-LOCK-UPD-SEC              SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC "]"   
               MOVE  MCP-RC            TO  SLOCK-RETURN
           END-IF
      *
           .
       910-LOCK-UPD-EXT.
           EXIT.
      * 
      *****************************************************************
      *    排他テーブル削除　処理
      *****************************************************************
       920-LOCK-DEL-SEC              SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC "]"   
               MOVE  MCP-RC            TO  SLOCK-RETURN
           END-IF
      *
           .
       920-LOCK-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    排他開始セット　処理
      *****************************************************************
       120-RESET-SEC                 SECTION.
      *
      *    業務LD取得　処理
           PERFORM 1101-LD-GET-SEC
      *    自端末レコード存在チェック
      *    MOVE    MCP-TERM(5:)            TO  LOCK-TERMID
      *    DISPLAY  "RESET MCP-TERM   = " MCP-TERM                 
      **** MOVE    MCP-TERM                TO  LOCK-TERMID
           MOVE    WRK-MCP-TERM            TO  LOCK-TERMID
grpsys     MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           PERFORM 910-LOCK-INV-SEC
           IF      FLG-LOCK       NOT  =  ZERO
      *    レコードが存在しない場合は、何もしない
               MOVE  ZERO              TO  SLOCK-RETURN
               GO    TO                120-RESET-EXT
           ELSE
      *    レコードが存在した場合は、該当のＬＤをクリア
      *        DISPLAY  "WRK-LD = " WRK-LD
               MOVE    ZERO             TO  IDZ
               PERFORM VARYING      IDX FROM  1   BY   1
                       UNTIL       (IDX     >     50)
      *        DISPLAY  "LOCK-LDNAME (" IDX ") = " LOCK-LDNAME(IDX)
                 IF  WRK-LD                 =     LOCK-LDNAME(IDX)
                     MOVE  IDX              TO    IDZ
                     MOVE  99               TO    IDX
                 END-IF
               END-PERFORM
      *        該当のＬＤがない時
               IF    IDZ                    =   ZERO
                   MOVE  ZERO              TO  SLOCK-RETURN
                   GO    TO                120-RESET-EXT
               END-IF
      *
               MOVE  SPACE                  TO  LOCK-LDNAME(IDZ)
               MOVE  SPACE                  TO  LOCK-PTID(IDZ)
               MOVE  SPACE                  TO  LOCK-STARTDATE(IDZ)
               MOVE  SPACE                  TO  LOCK-SRYYM(IDZ)
               MOVE  SPACE                  TO  LOCK-ENDDATE(IDZ)
               MOVE  SPACE                  TO  LOCK-STARTTIME(IDZ)
               MOVE  SPACE                  TO  LOCK-ENDTIME(IDZ)
               MOVE  SPACE                  TO  LOCK-FLG(IDZ)
      *    項目（日付、時間）
      *        DISPLAY  "RESET IDZ =  [" IDZ "]"
grpsys         MOVE  SPA-HOSPNUM       TO  LOCK-HOSPNUM
               MOVE  SLOCK-KARTE-UID   TO  LOCK-KARTE-UID
               MOVE  TBL-LOCK          TO  MCPDATA-REC
               PERFORM 910-LOCK-UPD-SEC
               IF      MCP-RC     =   ZERO
      *            DISPLAY "UPD OK = [" MCP-RC "]"   
                   CONTINUE
               ELSE
                   DISPLAY "INSERT ERR KEY = [" MCP-RC "]"   
                   MOVE  MCP-RC            TO  SLOCK-RETURN
               END-IF
           END-IF
      *
      *    監視ログ出力
               CALL    "ORCSAUDIT"     USING
                                      "1"
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       ORCSLOCKAREA
                                       JOBKANRI-REC
      *
           .
       120-RESET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    排他終了セット　処理
      *****************************************************************
       130-CLEAR-SEC                 SECTION.
      *
      *    自端末レコード存在チェック
      *    MOVE    MCP-TERM(5:)            TO  LOCK-TERMID
      **   MOVE    MCP-TERM                TO  LOCK-TERMID
           MOVE    WRK-MCP-TERM            TO  LOCK-TERMID
grpsys     MOVE    SPA-HOSPNUM             TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           PERFORM 910-LOCK-INV-SEC
           IF      FLG-LOCK       NOT  =  ZERO
      *    レコードが存在しない場合は、何もしない
               MOVE  ZERO              TO  SLOCK-RETURN
               GO    TO                130-CLEAR-EXT
           ELSE
      *    レコードが存在した場合は、該当のＬＤテーブルをクリア
      *   （除く、明細書出力処理＝＞フラグオフのときのみ）
      *        MOVE  SPACE             TO  LOCKTABLE
               PERFORM VARYING      IDX FROM  1   BY   1
                       UNTIL       (IDX     >     50)
                 IF  LOCK-LDNAME(IDX)       =     "ORCA42"   AND
                     LOCK-FLG(IDX)          =     "1"
                     CONTINUE
                 ELSE
                     MOVE  SPACE             TO  LOCK-LDNAME(IDX)
                     MOVE  SPACE             TO  LOCK-PTID(IDX)
                     MOVE  SPACE             TO  LOCK-STARTDATE(IDX)
                     MOVE  SPACE             TO  LOCK-SRYYM(IDX)
                     MOVE  SPACE             TO  LOCK-ENDDATE(IDX)
                     MOVE  SPACE             TO  LOCK-STARTTIME(IDX)
                     MOVE  SPACE             TO  LOCK-ENDTIME(IDX)
                     MOVE  SPACE             TO  LOCK-FLG(IDX)
                 END-IF
               END-PERFORM
grpsys         MOVE  SPA-HOSPNUM       TO  LOCK-HOSPNUM
               MOVE  TBL-LOCK          TO  MCPDATA-REC
               PERFORM 920-LOCK-DEL-SEC
               IF      MCP-RC     =   ZERO
                   CONTINUE
               ELSE
                   DISPLAY "INSERT ERR KEY = [" MCP-RC "]"   
                   MOVE  MCP-RC            TO  SLOCK-RETURN
               END-IF
           END-IF
      *
           .
       130-CLEAR-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    ステップ終了セット　処理
      *****************************************************************
       160-STE-SEC                 SECTION.
      *
      *    レコード存在チェック
grpsys     MOVE    SPA-HOSPNUM     TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK        TO  MCPDATA-REC
           PERFORM 910-LOCK-INV2-SEC
           IF       FLG-LOCK       NOT  =  ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
               GO    TO                160-STE-EXT
           END-IF
      *
      *    レコード更新
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC "]"   
               MOVE  MCP-RC            TO  SLOCK-RETURN
           END-IF
      *
           .
       160-STE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理
      *****************************************************************
       140-DEL-SEC                 SECTION.
      *
      *    レコード存在チェック
grpsys     MOVE    SPA-HOSPNUM     TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK        TO  MCPDATA-REC
           PERFORM 910-LOCK-INV2-SEC
           IF       FLG-LOCK       NOT  =  ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
               GO    TO                140-DEL-EXT
           END-IF
      *
      *    レコード更新
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC "]"   
               MOVE  MCP-RC            TO  SLOCK-RETURN
           END-IF
      *
           .
       140-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理（対象のシェルＩＤを全て）
      *****************************************************************
       170-DELALL-SEC                 SECTION.
      *
      *    レコード存在チェック
grpsys     MOVE SPA-HOSPNUM        TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK        TO  MCPDATA-REC
           PERFORM 910-LOCK-INV-SEC
           IF       FLG-LOCK       NOT  =  ZERO
               MOVE  ZERO              TO  SLOCK-RETURN
               GO    TO                170-DELALL-EXT
           END-IF
      *
      *    レコード更新
grpsys     MOVE    SPA-HOSPNUM     TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SLOCK-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC "]"   
               MOVE  MCP-RC            TO  SLOCK-RETURN
           END-IF
      *
           .
       170-DELALL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    排他制御マスタ読込
      *****************************************************************
       910-LOCK-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
grpsys                                SPA-AREA
      *????
      *        DISPLAY "FETCH=" MCP-RC    
      *???? 
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TBL-LOCK
                   MOVE    ZERO                TO  FLG-LOCK
               ELSE
                   MOVE    1                   TO  FLG-LOCK
               END-IF
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
       910-LOCK-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御マスタ読込（ＫＥＹ２）
      *****************************************************************
       910-LOCK-INV2-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
grpsys                                SPA-AREA
      *????
      *        DISPLAY "FETCH=" MCP-RC    
      *???? 
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TBL-LOCK
                   MOVE    ZERO                TO  FLG-LOCK
               ELSE
                   MOVE    1                   TO  FLG-LOCK
               END-IF
           ELSE
      *        DISPLAY "SELECT=" MCP-RC    
               MOVE    1                   TO  FLG-LOCK
           END-IF
      *
           .
       910-LOCK-INV2-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御マスタ読込（ＫＥＹ２）
      *****************************************************************
       910-MAP-INV1-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
grpsys                                SPA-AREA
      *????
      *        DISPLAY "LDMAP FETCH=" MCP-RC    
      *???? 
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TBL-LDMAP
                   MOVE    1                   TO  FLG-MAP
               ELSE
                   MOVE    ZERO                TO  FLG-MAP
               END-IF
           ELSE
      *        DISPLAY "LDMAP SELECT=" MCP-RC    
               MOVE    9                       TO  FLG-MAP
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
       910-MAP-INV1-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御マスタ読込（ＫＥＹ２）
      *****************************************************************
       910-LOCK-READ2-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *????
      *        DISPLAY "FETCH=" MCP-RC    
      *???? 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TBL-LOCK
               MOVE    ZERO                TO  FLG-LOCK
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
      *
           .
       910-LOCK-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
