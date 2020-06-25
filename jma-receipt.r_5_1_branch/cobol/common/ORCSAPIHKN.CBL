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
       PROGRAM-ID.                 ORCSAPIHKN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通（ＡＰＩ）
      *  コンポーネント名  : 保険組合決定・編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/10/25    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  12/07/24  労災区分追加対応
      * 04.07.00     NACL-多々納  13/11/01  保険組合せ２０件以上対応
      * 04.07.00     NACL-多々納  14/03/XX  前期高齢者２割対応
      * 04.07.00     NACL-多々納  14/11/XX  保険確認日付対応
      * 04.08.00     NACL-多々納  15/10/XX  補助区分名称サブ対応
      * 05.00.00     NACL-多々納  17/10/XX  保険組合せ番号対象対応
      * 05.01.00     NACL-多々納  18/11/02  診療日付追加対応
      * 05.00.00     NACL-多々納  19/06/XX  公費確認日付対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK1             PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-OK3             PIC 9(01).
           03  FLG-IN-OK           PIC 9(01).
           03  FLG-KOHARI          PIC 9(01).
           03  FLG-CHK-OK          PIC 9(01).
           03  FLG-COMBIOK         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDH-MAX             PIC 9(04).
           03  IDK-MAX             PIC 9(04).
           03  IDX-KOH             PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDXH                PIC 9(04).
      *
           03  HZN-IDK             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-HYOJIKBN            PIC X(01).
      *    表示用保険
           03  WRK-IDXH                PIC 9(04).
           03  WRK-IDXH2               PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKNID               PIC 9(10).
           03  WRK-FTNJANUM            PIC X(08).
           03  WRK-JKSNUM              PIC X(20).
           03  WRK-PROVIDER            PIC X(20).
           03  WRK-KOHNUM              PIC X(03).
      *
           03  WRK-KOH-AREA.
               05  WRK-KOHID           PIC 9(10)   OCCURS  4.
      *
           03  WRK-KEY-KOHNUM          PIC X(03).
      *
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-SYSYMD              PIC X(08).
      *
      *    公費並替え用ワーク
       01  WRK-KOHNUM-AREA.
           03  WRK-KOH-IN-AREA.
      *    並び替え前
               05  WRK-KOH-I-TBL       OCCURS  4.
                   07  WRK-KOHID-I         PIC  9(10).
                   07  WRK-KOHHKNNUM-I     PIC  X(03).
                   07  WRK-KOHNUMMEI-I     PIC  X(20).
                   07  WRK-GAIJGNKBN-I     PIC  X(01).
      *    並び替え後
               05  WRK-KOH-O-TBL       OCCURS  4.
                   07  WRK-KOHID-O         PIC  9(10).
                   07  WRK-KOHHKNNUM-O     PIC  X(03).
                   07  WRK-KOHNUMMEI-O     PIC  X(20).
                   07  WRK-GAIJGNKBN-O     PIC  X(01).
      *
      *    公費レセ順
       01  WRK-KOHSRT-AREA.
           03  WRK-KOHSRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "038".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "025".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
           03  WRK-KOHSRT-TABLE-R      REDEFINES   WRK-KOHSRT-TABLE.
               05  WRK-SRT-KOHHKNNUM       PIC X(03)
                                           OCCURS  28.
           03  WRK-SRT-KOHHKNNUM-MAX       PIC 9(02)   VALUE   28.
      *
      *    保険組み合わせ
       01  TBL-HKNCOMBI-AREA.
           02  TBL-HKNCOMBI-REC        OCCURS   200.
           COPY    "CPHKNCOMBI.INC"    REPLACING
                                       //COMB-// BY //TBL-COMB-//.
      *
      *    患者保険情報
       01  TBL-PTHKNINF-AREA.
         02  TBL-PTHKNINF-REC           OCCURS  90.
           COPY    "CPPTHKNINF.INC"     REPLACING
                                        //PTHKN-// BY //TBL-PTHKN-//.
      *
      *    公費マスタ（テーブル）
       01  TBL-PTKOHINF-AREA.
         02  TBL-PTKOHINF-REC           OCCURS  90.
           COPY    "CPPTKOHINF.INC"     REPLACING
                                        //PTKOH-// BY //TBL-PTKOH-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    チェック機能制御
           COPY  "CPSK1008.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    年齢・負担割合サブ
      ***  COPY    "CPORCSAGECHK.INC".
      *H27.10
      *    補助区分名称編集パラメタ
           COPY    "CPORAPI012SUB1.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *****COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSAPIHKNAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSAPIHKN-HKNCOMBI-AREA
           INITIALIZE                  ORCSAPIHKN-ERRCD
      *
      *H30.11
      *    診療日付で検索
           IF      ORCSAPIHKN-SRYYMD   =   SPACE
                                       OR  ZERO
               MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           ELSE
               MOVE    ORCSAPIHKN-SRYYMD   TO  WRK-SYSYMD
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 100-HKNCOMBI-HENSYU-SEC
      *
      *
      *H29.9
           IF      ORCSAPIHKN-KBN      =   "3"
      *        保険組合せ番号のみ対応
               PERFORM 250-HKNCOMBI-SELECT-SEC
               GO      TO  000-PROC-EXT
           END-IF
      *
      *    保険組合せ決定
           PERFORM 200-HKNCOMBI-SET-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       100-HKNCOMBI-HENSYU-SEC            SECTION.
      *
      *    保険組合マスター編集
           INITIALIZE                  TBL-HKNCOMBI-AREA
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    ZERO                TO  IDX-MAX
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1   )  OR
                           (IDX-MAX        >=  200 )
               MOVE    ZERO                TO  FLG-CHK-OK
               IF     (COMB-DELKBN         =   SPACE     ) AND
                      (COMB-HYOJIKBN   NOT =   "1"       )
                   EVALUATE    ORCSAPIHKN-KBN2
                   WHEN    "1"
      *                新規登録時すべて
                       MOVE    1                   TO  FLG-CHK-OK
                   WHEN    "2"
      *                年月で有効であること
                       IF     (COMB-TEKSTYMD(1:6)
                                               <=  WRK-SYSYMD(1:6)) AND
                              (COMB-TEKEDYMD(1:6)
                                               >=  WRK-SYSYMD(1:6))
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   WHEN    OTHER
      *                当日有効であること
                       IF     (COMB-TEKSTYMD   <=  WRK-SYSYMD )  AND
                              (COMB-TEKEDYMD   >=  WRK-SYSYMD )
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   END-EVALUATE
               END-IF
               IF      FLG-CHK-OK            =   1
                   ADD     1                 TO  IDX-MAX
                   MOVE    HKNCOMBI-REC      TO  TBL-HKNCOMBI-REC
                                                         (IDX-MAX)
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険情報編集
           INITIALIZE                  TBL-PTHKNINF-AREA
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  IDH-MAX 
           PERFORM     UNTIL   ( FLG-PTHKNINF  =   1 )  OR
                               ( IDH-MAX       >=  90)
               MOVE    ZERO                TO  FLG-CHK-OK
               IF     (PTHKN-SAKJOKBN      =   SPACE )
                   EVALUATE    ORCSAPIHKN-KBN2
                   WHEN    "1"
      *                新規登録時すべて
                       MOVE    1                   TO  FLG-CHK-OK
                   WHEN    "2"
      *                年月で有効であること
                       IF     (PTHKN-TEKSTYMD(1:6)
                                               <=  WRK-SYSYMD(1:6)) AND
                              (PTHKN-TEKEDYMD(1:6)
                                               >=  WRK-SYSYMD(1:6))
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   WHEN    OTHER
      *                当日有効であること
                       IF     (PTHKN-TEKSTYMD    <=  WRK-SYSYMD )  AND
                              (PTHKN-TEKEDYMD    >=  WRK-SYSYMD )
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   END-EVALUATE
               END-IF
               IF      FLG-CHK-OK            =   1
                   ADD     1                   TO  IDH-MAX 
                   MOVE    PTHKNINF-REC        TO  TBL-PTHKNINF-REC
                                                            (IDH-MAX)
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費編集
           INITIALIZE                  TBL-PTKOHINF-AREA
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDK-MAX 
           PERFORM     UNTIL   ( FLG-PTKOHINF  =   1 )  OR
                               ( IDK-MAX       >=  90)
               MOVE    ZERO                TO  FLG-CHK-OK
               IF     (PTKOH-SAKJOKBN      =   SPACE )
                   EVALUATE    ORCSAPIHKN-KBN2
                   WHEN    "1"
      *                新規登録時すべて
                       MOVE    1                   TO  FLG-CHK-OK
                   WHEN    "2"
      *                年月で有効であること
                       IF     (PTKOH-TEKSTYMD(1:6)
                                               <=  WRK-SYSYMD(1:6)) AND
                              (PTKOH-TEKEDYMD(1:6)
                                               >=  WRK-SYSYMD(1:6))
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   WHEN    OTHER
      *                当日有効であること
                       IF     (PTKOH-TEKSTYMD    <=  WRK-SYSYMD )  AND
                              (PTKOH-TEKEDYMD    >=  WRK-SYSYMD )
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   END-EVALUATE
               END-IF
               IF      FLG-CHK-OK            =   1
                   ADD     1                   TO  IDK-MAX 
                   MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                            (IDK-MAX)
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ決定処理
      *****************************************************************
       200-HKNCOMBI-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-IN-OK
      *    設定情報より決定
           IF     (ORCSAPIHKN-IN-HKNNUM    NOT =   SPACE )
             OR   (ORCSAPIHKN-IN-HKNNUM-NAME
                                           NOT =   SPACE )
               MOVE    1                   TO  FLG-IN-OK
           END-IF
           IF     (ORCSAPIHKN-IN-HKNJANUM  NOT =   SPACE )
               MOVE    1                   TO  FLG-IN-OK
           END-IF
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                        OR    (FLG-IN-OK   =   1   )
               IF     (ORCSAPIHKN-IN-KOHNUM (IDX)  NOT =   SPACE )
                  OR  (ORCSAPIHKN-IN-KOHNUM-NAME(IDX)
                                                   NOT =   SPACE )
                  OR  (ORCSAPIHKN-IN-FTNJANUM(IDX) NOT =   SPACE )
                  OR  (ORCSAPIHKN-IN-JKYSNUM (IDX) NOT =   SPACE )
                   MOVE    1               TO  FLG-IN-OK
               END-IF
           END-PERFORM
      *
           IF      ORCSAPIHKN-IN-HKNCOMBI  =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBINUM
               IF      FLG-IN-OK           =   ZERO
                   IF      ORCSAPIHKN-KBN      =   "1"
      *                前回保険組合せで決定
                       PERFORM 2001-HKNCOMBI-SRYKA-SEC
                   END-IF
               ELSE
      *            設定内容より決定
                   PERFORM 2002-HKNCOMBI-API-SEC
                   IF     (ORCSAPIHKN-ERRCD    =   ZERO   )  AND
                          (WRK-HKNCOMBINUM     =   SPACE  )
                       MOVE    3               TO  ORCSAPIHKN-ERRCD
                   END-IF
               END-IF
               MOVE    WRK-HKNCOMBINUM     TO  ORCSAPIHKN-OT-HKNCOMBI
           ELSE
               MOVE    ORCSAPIHKN-IN-HKNCOMBI
                                           TO  ORCSAPIHKN-OT-HKNCOMBI
           END-IF
      *
      *    出力編集
           IF     (ORCSAPIHKN-KBN      =   "2" )
      *        該当保険組合せ編集
               PERFORM 291-HKNCOMBI-ONLYHEN-SEC
           ELSE
      *        全保険組合せ出力編集
               PERFORM 290-HKNCOMBI-OUTHENSYU-SEC
           END-IF
      *
      *    新規登録時、保険組合せ対象外の公費
      *H30.11 (要望があるまで未対応)
      *    IF      ORCSAPIHKN-KBN2     =   "1"
      *        PERFORM 299-PTKOHINF-OUTHEN-SEC
      **   END-IF
      *
      *    最大件数 
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   200 )
               IF      ORCSAPIHKN-THKNCOMBI (IDX) NOT  =  SPACE
                   MOVE    IDX             TO  ORCSAPIHKN-OT-MAX
               END-IF
           END-PERFORM
      *
           .
       200-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回保険組合せで決定
      *****************************************************************
       2001-HKNCOMBI-SRYKA-SEC         SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    ORCSAPIHKN-SRYKA    TO  JYURRK-SRYKA
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "1"                 TO  JYURRK-NYUGAIKBN
           ELSE
               MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *!!  入外区分を反映するなら key7
      **** MOVE    "key7"              TO  MCP-PATHNAME
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ***      MOVE    "key7"              TO  MCP-PATHNAME
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-HKNCOMBINUM      TO  WRK-HKNCOMBINUM
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      ***  MOVE    "key7"              TO  MCP-PATHNAME
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-HKNCOMBINUM     NOT =   SPACE
      *        有効期間判定
               PERFORM 20010-HKNCOMBI-CHEK-SEC
           END-IF
      *
           IF      WRK-HKNCOMBINUM     =   SPACE
      *        省略時の保険組合せ決定
               PERFORM 20011-HKNCOMBI-SET-SEC
           END-IF
           .
       2001-HKNCOMBI-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ有効期間判定処理
      *****************************************************************
       20010-HKNCOMBI-CHEK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-MAX )
                           OR (FLG-OK  =   1       )
               IF      TBL-COMB-HKNCOMBINUM (IDX)
                                       =   WRK-HKNCOMBINUM
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPACE               TO  WRK-HKNCOMBINUM
           END-IF
           .
       20010-HKNCOMBI-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    省略時保険組合せ決定処理
      *****************************************************************
       20011-HKNCOMBI-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
           MOVE    ZERO                TO  WRK-IDXH2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
      *      自費は対象外とする
             IF     (TBL-COMB-HKNNUM (IDX)(1:2)   =   "98" )
                 CONTINUE
             ELSE
               MOVE    ZERO                TO  WRK-CNT-HKN2
               IF      TBL-COMB-HKNNUM (IDX)   NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      TBL-COMB-KOH1HKNNUM (IDX)   NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      TBL-COMB-KOH2HKNNUM (IDX)   NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      TBL-COMB-KOH3HKNNUM (IDX)   NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      TBL-COMB-KOH4HKNNUM (IDX)   NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
      *        治験・自費は他の保険がない時に有効とする
               IF     (TBL-COMB-HKNNUM (IDX)(1:2)  =   "98"
                                                   OR  "90"
                                                   OR  "91" )
                   IF      WRK-IDXH2           =   ZERO
                       MOVE    IDX             TO  WRK-IDXH2
                   END-IF
               ELSE
                   IF      WRK-CNT-HKN         <   WRK-CNT-HKN2
                       MOVE    IDX                 TO  WRK-IDXH
                       MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
                   END-IF
               END-IF
      *        労災・自賠責があれば対象とする
               IF      TBL-COMB-HKNNUM (IDX)   =   "971"
                                               OR  "973"
                   MOVE    IDX                 TO  WRK-IDXH
                   MOVE    IDX-MAX             TO  IDX
               END-IF
             END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    WRK-IDXH2       TO  WRK-IDXH
           END-IF
           IF      WRK-IDXH            =   ZERO
               MOVE    1               TO  WRK-IDXH
           END-IF
      *
           MOVE    TBL-COMB-HKNCOMBINUM (WRK-IDXH)
                                           TO  WRK-HKNCOMBINUM
           MOVE    TBL-COMB-HYOJIKBN (WRK-IDXH)
                                           TO  WRK-HYOJIKBN
           .
       20011-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    設定内容より決定
      *****************************************************************
       2002-HKNCOMBI-API-SEC             SECTION.
      *
      *    保険種類決定
           MOVE    ORCSAPIHKN-IN-HKNNUM    TO  WRK-HKNNUM
      *    短縮名称より判定
           IF      ORCSAPIHKN-IN-HKNNUM-NAME   NOT =   SPACE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   IDX-MAX  )
                             OR   (WRK-HKNNUM  NOT =   SPACE )
                   IF      TBL-COMB-SYU-TANSEIDONAME (IDX)
                                           =   ORCSAPIHKN-IN-HKNNUM-NAME
                       MOVE    TBL-COMB-HKNNUM (IDX)
                                               TO  WRK-HKNNUM
                   END-IF
               END-PERFORM
           END-IF
      *    保険者番号より決定
           MOVE    ZERO                TO  WRK-HKNID
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   IDH-MAX  )
                         OR   (WRK-HKNID   NOT =   ZERO  )
               IF     (WRK-HKNNUM                  =   SPACE )
                 AND  (ORCSAPIHKN-IN-HKNJANUM  NOT =   SPACE )
                   IF      TBL-PTHKN-HKNJANUM (IDX)
                                           =   ORCSAPIHKN-IN-HKNJANUM
                       MOVE    TBL-PTHKN-HKNNUM (IDX)
                                               TO  WRK-HKNNUM
                       MOVE    TBL-PTHKN-HKNID  (IDX)
                                               TO  WRK-HKNID
                   END-IF
               END-IF
      *
               IF     (WRK-HKNNUM              NOT =   SPACE )
                 AND  (ORCSAPIHKN-IN-HKNJANUM  NOT =   SPACE )
                   IF     (TBL-PTHKN-HKNNUM   (IDX)
                                           =   WRK-HKNNUM  )
                   AND    (TBL-PTHKN-HKNJANUM (IDX)
                                           =   ORCSAPIHKN-IN-HKNJANUM)
                       MOVE    TBL-PTHKN-HKNID  (IDX)
                                               TO  WRK-HKNID
                   END-IF
               END-IF
      *
               IF     (WRK-HKNNUM              NOT =   SPACE )
                 AND  (ORCSAPIHKN-IN-HKNJANUM      =   SPACE )
                   IF     (TBL-PTHKN-HKNNUM   (IDX)
                                           =   WRK-HKNNUM  )
                       MOVE    TBL-PTHKN-HKNID  (IDX)
                                               TO  WRK-HKNID
                   END-IF
               END-IF
           END-PERFORM
           IF     (ORCSAPIHKN-IN-HKNNUM    NOT =   SPACE )
             OR   (ORCSAPIHKN-IN-HKNJANUM  NOT =   SPACE )
             OR   (ORCSAPIHKN-IN-HKNNUM-NAME   NOT =   SPACE )
               IF      WRK-HKNID           =   ZERO
      *            保険情報一致なし
                   MOVE    1               TO  ORCSAPIHKN-ERRCD
                   GO      TO      2002-HKNCOMBI-API-EXT
               END-IF
           END-IF
      *
      *    公費情報決定
           INITIALIZE                  WRK-KOH-AREA
      *    公費負担名称がある場合、公費をテーブルへ
           IF    ( WRK-HKNNUM(1:1)     =   "9"   )
               MOVE    ZERO                TO  FLG-KOHARI
           ELSE
               MOVE    ZERO                TO  FLG-KOHARI
               PERFORM 200111-KOH-HENSYU-SEC
           END-IF
           IF      ORCSAPIHKN-ERRCD    NOT =   ZERO
               GO      TO      2002-HKNCOMBI-API-EXT
           END-IF
      *
      *    保険組合せ決定
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   IDX-MAX
               IF     (TBL-COMB-HYOJIKBN (IDH) =   "1"  )
      *            非表示対象外（入・外非表示は対象）
                   CONTINUE
               ELSE
                   IF     (TBL-COMB-HKNID (IDH)    =   WRK-HKNID  )
                      AND (TBL-COMB-KOH1ID(IDH)    =   WRK-KOHID (1))
                      AND (TBL-COMB-KOH2ID(IDH)    =   WRK-KOHID (2))
                      AND (TBL-COMB-KOH3ID(IDH)    =   WRK-KOHID (3))
                      AND (TBL-COMB-KOH4ID(IDH)    =   WRK-KOHID (4))
                       MOVE    TBL-COMB-HKNCOMBINUM(IDH)
                                               TO  WRK-HKNCOMBINUM
                       MOVE    IDX-MAX             TO  IDH
                   END-IF
               END-IF
           END-PERFORM
           .
       2002-HKNCOMBI-API-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       200111-KOH-HENSYU-SEC                SECTION.
      *
      *    対象の公費ＩＤを決定する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費の有無
               IF     (ORCSAPIHKN-IN-KOHNUM  (IDX) =   SPACE ) AND
                      (ORCSAPIHKN-IN-FTNJANUM(IDX) =   SPACE ) AND
                      (ORCSAPIHKN-IN-JKYSNUM (IDX) =   SPACE ) AND
                      (ORCSAPIHKN-IN-KOHNUM-NAME (IDX) =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-KOHARI
                   MOVE    ORCSAPIHKN-IN-KOHNUM  (IDX) TO  WRK-KOHNUM
                   MOVE    ORCSAPIHKN-IN-FTNJANUM(IDX) TO  WRK-FTNJANUM
                   MOVE    ORCSAPIHKN-IN-JKYSNUM (IDX) TO  WRK-JKSNUM
                   MOVE    ORCSAPIHKN-IN-KOHNUM-NAME (IDX)
                                                       TO  WRK-PROVIDER
                   PERFORM 2001112-KOH-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       200111-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費のチェック処理
      *****************************************************************
       2001112-KOH-SET-SEC                SECTION.
      *
           IF     (WRK-PROVIDER    NOT =   SPACE )
             AND  (WRK-KOHNUM          =   SPACE )
      *        公費の種類を名称より決定
               PERFORM 20011121-KOH-SET02-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  HZN-IDK
           PERFORM VARYING      IDK    FROM    1   BY  1
                   UNTIL       (IDK    >   IDK-MAX )  OR
                               (FLG-CHK    =   1   )
               MOVE    ZERO                TO  FLG-OK1
               MOVE    ZERO                TO  FLG-OK2
               MOVE    ZERO                TO  FLG-OK3
      *        種類
               IF      WRK-KOHNUM      NOT =   SPACE 
                   IF      WRK-KOHNUM      =   TBL-PTKOH-KOHNUM (IDK)
                       MOVE    1               TO  FLG-OK1
                   ELSE
                       MOVE    9               TO  FLG-OK1
                   END-IF
               END-IF
      *        負担者番号
               IF      (WRK-FTNJANUM   NOT =   SPACE   )
                   IF      WRK-FTNJANUM    =   TBL-PTKOH-FTNJANUM (IDK)
                       MOVE    1               TO  FLG-OK2
                   ELSE
                       MOVE    9               TO  FLG-OK2
                   END-IF
               END-IF
      *        受給者番号
               IF      (WRK-JKSNUM     NOT =   SPACE   )
                   IF      WRK-JKSNUM      =   TBL-PTKOH-JKYSNUM (IDK)
                       MOVE    1               TO  FLG-OK3
                   ELSE
                       MOVE    9               TO  FLG-OK3
                   END-IF
               END-IF
      *
               IF     (FLG-OK1             =   9     )  OR
                      (FLG-OK2             =   9     )  OR
                      (FLG-OK3             =   9     )
                   CONTINUE
               ELSE
      *            公費の種類・負担者番号・受給者番号の不一致なし
                   IF     (FLG-OK1             =   1     )  OR
                          (FLG-OK2             =   1     )  OR
                          (FLG-OK3             =   1     )
                       MOVE    IDK             TO  HZN-IDK
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
           END-PERFORM
           IF      HZN-IDK             >   ZERO
               MOVE     TBL-PTKOH-KOHID (HZN-IDK)  TO  WRK-KOHID(IDX)
           END-IF
      *    公費情報一致なし
           IF     (WRK-KOHNUM      NOT =   SPACE )
             OR   (WRK-PROVIDER    NOT =   SPACE )
             OR   (WRK-FTNJANUM    NOT =   SPACE )
             OR   (WRK-JKSNUM      NOT =   SPACE )
               IF      HZN-IDK             =   ZERO
                   MOVE    2               TO  ORCSAPIHKN-ERRCD
               END-IF
           END-IF
      *
           .
       2001112-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費のチェック処理
      *****************************************************************
       20011121-KOH-SET02-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHNUM
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   IDX-MAX )
                           OR (WRK-KOHNUM  NOT =   SPACE )
      *        短縮名称から公費種別
               IF     (WRK-PROVIDER        =   TBL-COMB-KOH-TANSEIDONAME
                                                         (IDH 1))
                   MOVE    TBL-COMB-KOHHKNNUM (IDH 1)
                                               TO  WRK-KOHNUM
               END-IF
               IF     (WRK-PROVIDER        =   TBL-COMB-KOH-TANSEIDONAME
                                                         (IDH 2))
                   MOVE    TBL-COMB-KOHHKNNUM (IDH 2)
                                               TO  WRK-KOHNUM
               END-IF
               IF     (WRK-PROVIDER        =   TBL-COMB-KOH-TANSEIDONAME
                                                         (IDH 3))
                   MOVE    TBL-COMB-KOHHKNNUM (IDH 3)
                                               TO  WRK-KOHNUM
               END-IF
               IF     (WRK-PROVIDER        =   TBL-COMB-KOH-TANSEIDONAME
                                                         (IDH 4))
                   MOVE    TBL-COMB-KOHHKNNUM (IDH 4)
                                               TO  WRK-KOHNUM
               END-IF
           END-PERFORM
      *
           .
       20011121-KOH-SET02-EXT.
           EXIT.
      *****************************************************************
      *    出力保険情報編集処理
      *****************************************************************
       290-HKNCOMBI-OUTHENSYU-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  FLG-COMBIOK
           IF     (IDX-MAX             >   20 )
             AND  (ORCSAPIHKN-KBN3 NOT =   "1" )
      *        ２０件以上は、最後から編集（２０件までの時）
               MOVE    20                  TO  IDXH
               PERFORM VARYING     IDX     FROM    IDX-MAX BY  -1
                       UNTIL      (IDX     <   1       )
                              OR  (IDXH    <   1       )
                   PERFORM 2901-HKNCOMBI-HEN-SEC
                   IF      TBL-COMB-HKNCOMBINUM (IDX) 
                                           =   ORCSAPIHKN-OT-HKNCOMBI
                       MOVE    1               TO  FLG-COMBIOK
                   END-IF
                   COMPUTE IDXH            =   IDXH    -   1
               END-PERFORM
               MOVE    20                  TO  IDX-TBL
               IF      ORCSAPIHKN-ERRCD    =   ZERO
                   MOVE    9                   TO  ORCSAPIHKN-ERRCD
               END-IF
           ELSE
               MOVE    1                   TO  IDXH
               PERFORM VARYING     IDX      FROM    1   BY  1
                       UNTIL      (IDX     >   IDX-MAX )
      *H29.1                  OR  (IDXH    >   20      )
                              OR  (IDXH    >   200     )
                   IF      TBL-COMB-HKNCOMBINUM (IDX) NOT =   SPACE
                       PERFORM 2901-HKNCOMBI-HEN-SEC
                       IF      TBL-COMB-HKNCOMBINUM (IDX) 
                                           =   ORCSAPIHKN-OT-HKNCOMBI
                           MOVE    1               TO  FLG-COMBIOK
                       END-IF
                       MOVE    IDXH                TO  IDX-TBL
                       ADD     1                   TO  IDXH
                   END-IF
               END-PERFORM
           END-IF
      *
           IF     (ORCSAPIHKN-OT-HKNCOMBI  NOT =   SPACE )
             AND  (FLG-COMBIOK             =   ZERO )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   IDX-MAX )
                              OR  (FLG-COMBIOK =   1   )
                   IF      TBL-COMB-HKNCOMBINUM (IDX)
                                           =   ORCSAPIHKN-OT-HKNCOMBI
                       MOVE    1               TO  IDXH
                       PERFORM 2901-HKNCOMBI-HEN-SEC
                       MOVE    1               TO  FLG-COMBIOK
                   END-IF
               END-PERFORM
      *        対象なし
               IF      FLG-COMBIOK         =   ZERO
                   MOVE    4                   TO  ORCSAPIHKN-ERRCD
               END-IF
           END-IF
           .
       290-HKNCOMBI-OUTHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    該当保険組合せ情報編集処理
      *****************************************************************
       291-HKNCOMBI-ONLYHEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  FLG-COMBIOK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-MAX )
                          OR  (FLG-COMBIOK =   1   )
               IF      TBL-COMB-HKNCOMBINUM (IDX)
                                           =   ORCSAPIHKN-OT-HKNCOMBI
                   MOVE    1               TO  IDXH
                   PERFORM 2901-HKNCOMBI-HEN-SEC
                   MOVE    1               TO  FLG-COMBIOK
               END-IF
           END-PERFORM
      *    対象なし
           IF      FLG-COMBIOK         =   ZERO
               MOVE    4                   TO  ORCSAPIHKN-ERRCD
           END-IF
           .
       291-HKNCOMBI-ONLYHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       2901-HKNCOMBI-HEN-SEC             SECTION.
      *
           MOVE    TBL-COMB-HKNCOMBINUM (IDX)
                                       TO  ORCSAPIHKN-THKNCOMBI
                                                               (IDXH)
           MOVE    TBL-COMB-HKNNUM (IDX)
                                       TO  ORCSAPIHKN-HKNNUM (IDXH)
      *    保険ＩＤ
           MOVE    TBL-COMB-HKNID  (IDX)
                                       TO  ORCSAPIHKN-NAI-HKNID (IDXH)
      *    主保険−短縮制度名
           MOVE    TBL-COMB-SYU-TANSEIDONAME (IDX)
                                       TO  ORCSAPIHKN-HKNNUM-NAME
                                                                 (IDXH)
      *    本人家族区分
           MOVE    TBL-COMB-HONKZKKBN (IDX)
                                       TO  ORCSAPIHKN-HONKZKKBN(IDXH)
      *    補助区分
           MOVE    TBL-COMB-HOJOKBN (IDX)
                                       TO  ORCSAPIHKN-HOJOKBN (IDXH)
           PERFORM 29013-HOJOKBN-NAME-HEN-SEC
      *
      *    継続区分
           MOVE    TBL-COMB-CONTKBN (IDX)
                                       TO  ORCSAPIHKN-CONTKBN (IDXH)
      *H29.1
      *    保険組合せ負担割合
           MOVE    TBL-COMB-GAI-KFTNRATE (IDX)
                                       TO  ORCSAPIHKN-COMB-RATEG(IDXH)
           MOVE    TBL-COMB-NYU-KFTNRATE(IDX)
                                       TO  ORCSAPIHKN-COMB-RATEN(IDXH)
      *    有効期間
           MOVE    TBL-COMB-TEKSTYMD (IDX) TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE       TO  ORCSAPIHKN-COMB-TEKSTYMD(IDXH)
           MOVE    TBL-COMB-TEKEDYMD (IDX) TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE       TO  ORCSAPIHKN-COMB-TEKEDYMD(IDXH)
      *
           MOVE    TBL-COMB-TEKSTYMD (IDX)
                                      TO  ORCSAPIHKN-COMBN-TEKSTYMD
                                                                (IDXH)
           MOVE    TBL-COMB-TEKEDYMD (IDX)
                                      TO  ORCSAPIHKN-COMBN-TEKEDYMD
                                                                (IDXH)
      *    非表示区分
           MOVE    TBL-COMB-HYOJIKBN (IDX)
                                       TO  ORCSAPIHKN-HYOJIKBN (IDXH)
      *
      *    保険情報
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH         >   IDH-MAX  )
               IF      TBL-COMB-HKNID (IDX)    =   TBL-PTHKN-HKNID (IDH)
                   MOVE    TBL-PTHKN-HKNJANUM(IDH)
                                           TO  ORCSAPIHKN-HKNJANUM(IDXH)
                   MOVE    TBL-PTHKN-KIGO (IDH)
                                           TO  ORCSAPIHKN-KIGO (IDXH)
                   MOVE    TBL-PTHKN-NUM  (IDH)
                                           TO  ORCSAPIHKN-NUM  (IDXH)
      *            枝番
                   MOVE    TBL-PTHKN-EDABAN(IDH)
                                           TO  ORCSAPIHKN-EDABAN(IDXH)
                   MOVE    TBL-PTHKN-HIHKNJANAME(IDH)
                                           TO  ORCSAPIHKN-HIHKNJANAME
                                                                (IDXH)
      *            保険有効期間
                   MOVE    TBL-PTHKN-TEKSTYMD (IDH) TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-TEKSTYMD(IDXH)
                   MOVE    TBL-PTHKN-TEKEDYMD (IDH) TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-TEKEDYMD(IDXH)
      *
                   MOVE    TBL-PTHKN-TEKSTYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-TEKSTYMD
                                                               (IDXH)
                   MOVE    TBL-PTHKN-TEKEDYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-TEKEDYMD
                                                               (IDXH)
      *            資格取得日
                   IF      TBL-PTHKN-SKKGETYMD (IDH) NOT =   SPACE
                       MOVE    TBL-PTHKN-SKKGETYMD (IDH) TO  WRK-SYMD
                       PERFORM 800-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-SKKGETYMD
                                                                (IDXH)
                   END-IF
                   MOVE    TBL-PTHKN-SKKGETYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-SKKGETYMD
                                                                (IDXH)
      *H26.11
      *            確認日付
                   IF      TBL-PTHKN-KAKUNINYMD(IDH) NOT =   SPACE
                       MOVE    TBL-PTHKN-KAKUNINYMD(IDH) TO  WRK-SYMD
                       PERFORM 800-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KAKUNINYMD
                                                                (IDXH)
                   END-IF
                   MOVE    TBL-PTHKN-KAKUNINYMD (IDH)
                                           TO  ORCSAPIHKN-NAI-KAKUNINYMD
                                                                (IDXH)
      *
                   MOVE    IDH-MAX         TO  IDH
               END-IF
           END-PERFORM
      *    労災・自賠責区分
           IF      TBL-COMB-HKNNUM (IDX)   =   "971"  OR  "973"
               PERFORM 29013-PTRSIINF-HEN-SEC
           END-IF
      *
      *    公費情報
           IF      TBL-COMB-KOHHKNNUM (IDX 1)  NOT =   SPACE
      *         公費ＯＲＴ
                PERFORM 29011-KOHID-HENSYU-SEC
                PERFORM VARYING     IDY     FROM    1   BY  1
                        UNTIL      (IDY         >   4   )
                               OR  (WRK-KOHID-O (IDY)   =   ZERO )
                   PERFORM 29012-KOHHENSYU-SEC
               END-PERFORM
           END-IF
      *
           .
       2901-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分名称編集
      *****************************************************************
       29013-HOJOKBN-NAME-HEN-SEC             SECTION.
      *
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    TBL-COMB-HKNNUM (IDX)   TO  API012SUB1-HKNNUM
           MOVE    TBL-COMB-HOJOKBN (IDX)  TO  API012SUB1-HOJOKBN
           MOVE    WRK-SYSYMD              TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
           MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  ORCSAPIHKN-HOJOKBN-NAME(IDXH)
      *
           .
       29013-HOJOKBN-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災・自賠責区分処理
      *****************************************************************
       29013-PTRSIINF-HEN-SEC             SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    TBL-COMB-HKNID (IDX)    TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTRSIINF        =   ZERO
      *        労災区分
               MOVE    PTRSI-HKNKBN        TO  ORCSAPIHKN-RSI-HKNKBN
                                                             (IDXH)
           END-IF
           .
       29013-PTRSIINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費優先順ＳＯＲＴ処理
      *****************************************************************
       29011-KOHID-HENSYU-SEC             SECTION.
      *
           INITIALIZE                      WRK-KOHNUM-AREA
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
               MOVE    TBL-COMB-KOHID (IDX IDZ)
                                           TO  WRK-KOHID-I (IDZ)
               MOVE    TBL-COMB-KOHHKNNUM (IDX IDZ)
                                           TO  WRK-KOHHKNNUM-I (IDZ)
      *        公費名称
               MOVE    TBL-COMB-KOH-TANSEIDONAME  (IDX IDZ)
                                           TO  WRK-KOHNUMMEI-I (IDZ)
      *H26.11
      *        並び替え廃止
               MOVE    WRK-KOH-I-TBL (IDZ) TO  WRK-KOH-O-TBL (IDZ)
           END-PERFORM
      *
      *
      *
      *H26.11
      *    並び替え廃止（以下なし）
           GO      TO      29011-KOHID-HENSYU-EXT
      *
      *
      *    優先順位に並べ替え
           MOVE    ZERO                TO  IDX-KOH
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ     >   WRK-SRT-KOHHKNNUM-MAX )
                   OR         (IDX-KOH     =   4  )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >       4
                   IF      WRK-SRT-KOHHKNNUM (IDZ)
                                           =   WRK-KOHHKNNUM-I (IDY)
                       ADD     1               TO  IDX-KOH
                       MOVE    WRK-KOH-I-TBL (IDY)
                                               TO  WRK-KOH-O-TBL
                                                          (IDX-KOH)
                       INITIALIZE              WRK-KOH-I-TBL (IDY)
                       MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時
           IF      IDX-KOH                 =   4
               GO  TO  29011-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え（昇順）
           PERFORM VARYING     IDZ     FROM    1    BY  1
                   UNTIL      (IDZ         >   4    )
                          OR  (IDX-KOH     >   4    )
               MOVE    ZERO                TO  IDX1
               MOVE    SPACE               TO  WRK-KEY-KOHNUM
               PERFORM VARYING     IDY     FROM    1    BY  1
                       UNTIL       IDY     >       4
                   IF     (WRK-KOHHKNNUM-I (IDY)   NOT =   SPACE)
                   AND   ((WRK-KOHHKNNUM-I (IDY)   <   WRK-KEY-KOHNUM)
                      OR  (WRK-KEY-KOHNUM          =   SPACE  ))
                       MOVE    WRK-KOHHKNNUM-I (IDY)
                                                   TO  WRK-KEY-KOHNUM
                       MOVE    IDY                 TO  IDX1
                   END-IF
               END-PERFORM
               IF      IDX1                >   ZERO
                   ADD     1               TO  IDX-KOH
                   MOVE    WRK-KOH-I-TBL (IDX1)
                                           TO  WRK-KOH-O-TBL
                                                          (IDX-KOH)
                   INITIALIZE              WRK-KOH-I-TBL (IDX1)
               END-IF
           END-PERFORM
      *
           .
       29011-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       29012-KOHHENSYU-SEC             SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   IDK-MAX )
      *        公費情報
               IF      WRK-KOHID-O (IDY)   =   TBL-PTKOH-KOHID (IDH)
      *            公費ＩＤ
                   MOVE    TBL-PTKOH-KOHID (IDH)
                                           TO  ORCSAPIHKN-NAI-KOHID
                                                            (IDXH IDY)
      *            公費の種類
                   MOVE    TBL-PTKOH-KOHNUM (IDH)
                                           TO  ORCSAPIHKN-KOHNUM
                                                            (IDXH IDY)
      *            公費の名称
                   MOVE    WRK-KOHNUMMEI-O (IDY)
                                           TO  ORCSAPIHKN-KOHNUM-NAME
                                                            (IDXH IDY)
      *            負担者番号
                   MOVE    TBL-PTKOH-FTNJANUM (IDH)
                                           TO  ORCSAPIHKN-FTNJANUM
                                                            (IDXH IDY)
      *            受給者番号
                   MOVE    TBL-PTKOH-JKYSNUM (IDH)
                                           TO  ORCSAPIHKN-JKYSNUM
                                                            (IDXH IDY)
      *            有効開始日
                   MOVE    TBL-PTKOH-TEKSTYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKSTYMD
                                                            (IDXH IDY)
      *            有効終了日
                   MOVE    TBL-PTKOH-TEKEDYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKEDYMD
                                                            (IDXH IDY)
      *            確認日付
                   IF      TBL-PTKOH-KAKUNINYMD (IDH)  NOT =   SPACE
                       MOVE    TBL-PTKOH-KAKUNINYMD (IDH)  TO  WRK-SYMD
                       PERFORM 800-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  ORCSAPIHKN-KOH-KAKUNINYMD
                                                            (IDXH IDY)
                   END-IF
      *
      *            負担率・負担額編集
                   PERFORM 400-KOHFTN-HENSYU-SEC
      *
                   MOVE    IDK-MAX             TO  IDH
               END-IF
           END-PERFORM
           .
       29012-KOHHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    負担率・負担額編集処理
      *****************************************************************
       400-KOHFTN-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *    保険番号（公費）
           MOVE    TBL-PTKOH-KOHNUM (IDH)  TO  HKN-HKNNUM
      *    支払区分
           MOVE    TBL-PTKOH-PAYKBN (IDH)  TO  HKN-PAYKBN
      *    有効期間
           MOVE    TBL-PTKOH-TEKSTYMD(IDH) TO  HKN-TEKSTYMD
           MOVE    TBL-PTKOH-TEKSTYMD(IDH) TO  HKN-TEKEDYMD
      *    MOVE    WRK-SYSYMD          TO  HKN-TEKSTYMD
      *    MOVE    WRK-SYSYMD          TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    本人−外来−回−負担割合
           COMPUTE WRK-FTNRATE         =   HKN-HON-GAIKFTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-RATE-OUTPATIENT
                                                            (IDXH IDY)
      *    外来上限額
           IF      HKN-HON-GAIKKOTGAK  =   ZERO
      *        本人−外来−日−上限額
               MOVE    HKN-HON-GAIDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        本人−外来−回−上限額
               MOVE    HKN-HON-GAIKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  ORCSAPIHKN-MONEY-OUTPATIENT
                                                            (IDXH IDY)
      *
      *    本人−入院−回−負担割合
           COMPUTE WRK-FTNRATE         =   HKN-HON-NYUKFTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  ORCSAPIHKN-RATE-ADMISSION
                                                            (IDXH IDY)
      *    外来上限額
           IF      HKN-HON-NYUKKOTGAK  =   ZERO
      *        本人−入院−日−上限額
               MOVE    HKN-HON-NYUDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        本人−入院−回−上限額
               MOVE    HKN-HON-NYUKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  ORCSAPIHKN-MONEY-ADMISSION
                                                            (IDXH IDY)
           .
       400-KOHFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ対象外の公費編集処理
      *****************************************************************
       299-PTKOHINF-OUTHEN-SEC                SECTION.
      *
           MOVE    IDX-TBL             TO  IDX
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   IDK-MAX )
                          OR  (IDX     >=  20      )
      *        特定公費（減免〜後期非該当）
               IF      (TBL-PTKOH-KOHNUM (IDH) =   "956"
                                               OR  "957"
                                               OR  "958"
                                               OR  "959"
                                               OR  "977"
                                               OR  "978"
                                               OR  "979"
                                               OR  "976"  )
               OR     (TBL-PTKOH-KOHNUM(IDH) (1:2)  =   "96"  )
      *H30.8   高齢者現役など
               OR     (TBL-PTKOH-KOHNUM(IDH)(1:2)   =   "94"
                                                    OR  "95"  )
      *        当日登録分のみ
                 IF     (TBL-PTKOH-CREYMD(IDH)   =   WRK-SYSYMD )
                   ADD     1               TO  IDX
      *            保険組合せ作成なし
                   MOVE    ALL "*"         TO  ORCSAPIHKN-THKNCOMBI(IDX)
                   MOVE    ALL "*"         TO  ORCSAPIHKN-HKNNUM (IDX)
                   MOVE    ALL "＊"        TO  ORCSAPIHKN-HKNNUM-NAME
                                                                 (IDX)
                   MOVE    1               TO  IDY
      *            公費ＩＤ
                   MOVE    TBL-PTKOH-KOHID (IDH)
                                           TO  ORCSAPIHKN-NAI-KOHID
                                                            (IDX IDY)
      *            公費の種類
                   MOVE    TBL-PTKOH-KOHNUM (IDH)
                                           TO  ORCSAPIHKN-KOHNUM
                                                               (IDX IDY)
      *            負担者番号
                   MOVE    TBL-PTKOH-FTNJANUM (IDH)
                                           TO  ORCSAPIHKN-FTNJANUM
                                                               (IDX IDY)
      *            受給者番号
                   MOVE    TBL-PTKOH-JKYSNUM (IDH)
                                           TO  ORCSAPIHKN-JKYSNUM
                                                               (IDX IDY)
      *            有効開始日
                   MOVE    TBL-PTKOH-TEKSTYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKSTYMD
                                                               (IDX IDY)
      *            有効終了日
                   MOVE    TBL-PTKOH-TEKEDYMD (IDH)    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKEDYMD
                                                               (IDX IDY)
      *            負担率・負担額編集
      ***********  MOVE    TBL-PTKOH-TEKSTYMD (IDH)    TO  WRK-SYSYMD
                   MOVE    IDX             TO  IDXH
                   PERFORM 400-KOHFTN-HENSYU-SEC
      *            公費の名称
                   MOVE    HKN-TANSEIDONAME
                                           TO  ORCSAPIHKN-KOHNUM-NAME
                                                               (IDX IDY)
                 END-IF
      *
               END-IF
           END-PERFORM
           .
       299-PTKOHINF-OUTHEN-EXT.
           EXIT.
      *
      *H29.9
      *****************************************************************
      *    保険組合せ番号編集処理
      *****************************************************************
       250-HKNCOMBI-SELECT-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-COMBIOK
           MOVE    1                   TO  IDXH
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-MAX )
                          OR  (FLG-COMBIOK =   1   )
               IF      TBL-COMB-HKNCOMBINUM (IDX)
                                           =   ORCSAPIHKN-IN-HKNCOMBI
                   MOVE    ORCSAPIHKN-IN-HKNCOMBI
                                           TO  ORCSAPIHKN-OT-HKNCOMBI
                   PERFORM 2901-HKNCOMBI-HEN-SEC
                   MOVE    1               TO  FLG-COMBIOK
               END-IF
           END-PERFORM
           IF      FLG-COMBIOK         =   1
               GO      TO      250-HKNCOMBI-SELECT-EXT
           END-IF
      *
      *    保険組合せ当月なし
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    ORCSAPIHKN-IN-HKNCOMBI
                                       TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNCOMBI        =   1
      *        対象なし
               MOVE    4                   TO  ORCSAPIHKN-ERRCD
               GO      TO      250-HKNCOMBI-SELECT-EXT
           END-IF
      *
           MOVE    ORCSAPIHKN-IN-HKNCOMBI
                                       TO  ORCSAPIHKN-OT-HKNCOMBI
      *
           IF      COMB-DELKBN         =   "1"
      *        削除分
               MOVE    8                   TO  ORCSAPIHKN-ERRCD
           ELSE
      *        期間外
               MOVE    ZERO                TO  FLG-CHK-OK
               EVALUATE    ORCSAPIHKN-KBN2
                   WHEN    "1"
                       MOVE    1                   TO  FLG-CHK-OK
                   WHEN    "2"
      *                年月で有効であること
                       IF     (COMB-TEKSTYMD(1:6)
                                               <=  WRK-SYSYMD(1:6)) AND
                              (COMB-TEKEDYMD(1:6)
                                               >=  WRK-SYSYMD(1:6))
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
                   WHEN    OTHER
      *                当日有効であること
                       IF     (COMB-TEKSTYMD   <=  WRK-SYSYMD )  AND
                              (COMB-TEKEDYMD   >=  WRK-SYSYMD )
                           MOVE    1               TO  FLG-CHK-OK
                       END-IF
               END-EVALUATE
      *        期間外
               IF      FLG-CHK-OK          =   ZERO
                   MOVE    7                   TO  ORCSAPIHKN-ERRCD
               END-IF
           END-IF
      *
      *    編集
           MOVE    COMB-HKNCOMBINUM    TO  ORCSAPIHKN-THKNCOMBI
                                                             (IDXH)
           MOVE    COMB-HKNNUM         TO  ORCSAPIHKN-HKNNUM (IDXH)
      *    保険ＩＤ
           MOVE    COMB-HKNID          TO  ORCSAPIHKN-NAI-HKNID (IDXH)
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  ORCSAPIHKN-HKNNUM-NAME
                                                                 (IDXH)
      *    本人家族区分
           MOVE    COMB-HONKZKKBN      TO  ORCSAPIHKN-HONKZKKBN(IDXH)
      *    補助区分
           MOVE    COMB-HOJOKBN        TO  ORCSAPIHKN-HOJOKBN (IDXH)
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    COMB-HKNNUM         TO  API012SUB1-HKNNUM
           MOVE    COMB-HOJOKBN        TO  API012SUB1-HOJOKBN
           MOVE    COMB-TEKSTYMD       TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
           MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  ORCSAPIHKN-HOJOKBN-NAME(IDXH)
      *
      *    継続区分
           MOVE    COMB-CONTKBN        TO  ORCSAPIHKN-CONTKBN (IDXH)
      *
      *    保険組合せ負担割合
           MOVE    COMB-GAI-KFTNRATE   TO  ORCSAPIHKN-COMB-RATEG(IDXH)
           MOVE    COMB-NYU-KFTNRATE   TO  ORCSAPIHKN-COMB-RATEN(IDXH)
      *    有効期間
           MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ORCSAPIHKN-COMB-TEKSTYMD
                                                             (IDXH)
           MOVE    COMB-TEKEDYMD       TO  WRK-SYMD
           PERFORM 800-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ORCSAPIHKN-COMB-TEKEDYMD
                                                             (IDXH)
      *
           MOVE    COMB-TEKSTYMD       TO  ORCSAPIHKN-COMBN-TEKSTYMD
                                                                (IDXH)
           MOVE    COMB-TEKEDYMD       TO  ORCSAPIHKN-COMBN-TEKEDYMD
                                                                (IDXH)
      *    非表示区分
           MOVE    COMB-HYOJIKBN       TO  ORCSAPIHKN-HYOJIKBN (IDXH)
      *
      *    保険情報
           IF      COMB-HKNID      NOT =   ZERO
               PERFORM 901-PTHKNINF-READ-SEC
               MOVE    PTHKN-HKNJANUM      TO  ORCSAPIHKN-HKNJANUM(IDXH)
               MOVE    PTHKN-KIGO          TO  ORCSAPIHKN-KIGO (IDXH)
               MOVE    PTHKN-NUM           TO  ORCSAPIHKN-NUM  (IDXH)
      *        枝番
               MOVE    PTHKN-EDABAN        TO  ORCSAPIHKN-EDABAN(IDXH)
               MOVE    PTHKN-HIHKNJANAME   TO  ORCSAPIHKN-HIHKNJANAME
                                                                (IDXH)
      *        保険有効期間
               MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  ORCSAPIHKN-TEKSTYMD(IDXH)
               MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  ORCSAPIHKN-TEKEDYMD(IDXH)
      *
               MOVE    PTHKN-TEKSTYMD      TO  ORCSAPIHKN-NAI-TEKSTYMD
                                                               (IDXH)
               MOVE    PTHKN-TEKEDYMD      TO  ORCSAPIHKN-NAI-TEKEDYMD
                                                               (IDXH)
      *        資格取得日
               IF      PTHKN-SKKGETYMD     NOT =   SPACE
                   MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-SKKGETYMD
                                                                (IDXH)
               END-IF
               MOVE    PTHKN-SKKGETYMD     TO  ORCSAPIHKN-NAI-SKKGETYMD
                                                                (IDXH)
      *        確認日付
               IF      PTHKN-KAKUNINYMD    NOT =   SPACE
                   MOVE    PTHKN-KAKUNINYMD    TO  WRK-SYMD
                   PERFORM 800-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KAKUNINYMD
                                                                (IDXH)
               END-IF
               MOVE    PTHKN-KAKUNINYMD    TO  ORCSAPIHKN-NAI-KAKUNINYMD
                                                                (IDXH)
           END-IF
      *    労災・自賠責区分
           IF      COMB-HKNNUM         =   "971"  OR  "973"
               PERFORM 29013-PTRSIINF-HEN-SEC
           END-IF
      *    公費情報
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   4   )
                           OR (COMB-KOHHKNNUM (IDY)   =   SPACE)
      *        公費の種類
               MOVE    COMB-KOHHKNNUM (IDY)
                                       TO  ORCSAPIHKN-KOHNUM
                                                            (IDXH IDY)
      *        公費の種類名称
               MOVE    COMB-KOH-TANSEIDONAME(IDY)
                                       TO  ORCSAPIHKN-KOHNUM-NAME
                                                            (IDXH IDY)
               MOVE    COMB-KOHID (IDY)
                                       TO  ORCSAPIHKN-NAI-KOHID
                                                            (IDXH IDY)
      *        負担者番号、受給者番号
               PERFORM 901-PTKOHINF-READ-SEC
               MOVE    PTKOH-FTNJANUM  TO  ORCSAPIHKN-FTNJANUM
                                                            (IDXH IDY)
               MOVE    PTKOH-JKYSNUM   TO  ORCSAPIHKN-JKYSNUM
                                                            (IDXH IDY)
      *        有効開始日
               MOVE    PTKOH-TEKSTYMD  TO  WRK-SYMD
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKSTYMD
                                                            (IDXH IDY)
      *        有効終了日
               MOVE    PTKOH-TEKEDYMD  TO  WRK-SYMD
               PERFORM 800-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  ORCSAPIHKN-KOH-TEKEDYMD
                                                            (IDXH IDY)
      *        負担率・負担額編集
               MOVE    1               TO  IDH
               MOVE    PTKOHINF-REC    TO  TBL-PTKOHINF-REC(IDH)
      *****    MOVE    PTKOH-TEKSTYMD  TO  WRK-SYSYMD
               PERFORM 400-KOHFTN-HENSYU-SEC
           END-PERFORM
           .
       250-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
           .
       800-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       980-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       980-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報検索
      *****************************************************************
       901-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       901-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索
      *****************************************************************
       901-PTKOHINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    COMB-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    COMB-PTID           TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDY)    TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       901-PTHKNINF-READ-EXT.
           EXIT.
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
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
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
      *****************************************************************
